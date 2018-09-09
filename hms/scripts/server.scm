;;; Guile HMS --- HMS command-line interface.
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;;
;;; This file is part of Guile HMS.
;;;
;;; Guile HMS is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation; either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; Guile HMS is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile HMS.  If not, see <http://www.gnu.org/licenses/>.

(define-module (hms scripts server)
  #:use-module (hms ui)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (web client)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:export (hms-server))


;;;
;;; Entry point.
;;;

(define (hms-server . args)
  (define (serialize-field field)
    (if (string-prefix? "@" field)
        (string-drop field (string-length "@"))
        field))
  (define (serialize-boolean value)
    (if value "true" "false"))
  (define (serialize-quota value)
    (let ((size (number->string (/ (/ (/ value 1024.0) 1024.0) 1024.0))))
      (match (string-split size #\.)
        ((natural decimal)
         (string-append natural "."
                        (if (> (string-length decimal) 2)
                            (string-take decimal 2)
                            decimal))))))
  (for-each (lambda (server)
              (let-values (((response body)
                            (http-get (string-append "https://api.majordomo.ru/rc-staff/server")
                                      #:headers `((content-type . (application/json))
                                                  (Authorization . ,(format #f "Bearer ~a" (auth))))
                                      #:keep-alive? #t)))
                (for-each (lambda (server)
                            (pretty-print server)
                            (format #t "id: ~a~%" (assoc-ref server "id"))
                            (format #t "name: ~a~%" (assoc-ref server "name"))
                            (format #t "switched_on: ~a~%" (assoc-ref server "switchedOn"))
                            (for-each (lambda (service)
                                        (format #t "id: ~a~%" (assoc-ref service "id"))
                                        (format #t "name: ~a~%" (assoc-ref service "name"))
                                        (for-each (lambda (socket)
                                                    (format #t "id: ~a~%" (assoc-ref socket "id"))
                                                    (format #t "name: ~a~%" (assoc-ref socket "name"))
                                                    (format #t "address: ~a~%" (assoc-ref socket "address"))
                                                    (format #t "port: ~a~%" (assoc-ref socket "port"))
                                                    (format #t "switched_on: ~a~%" (assoc-ref socket "switchedOn"))
                                                    (newline))
                                                  (assoc-ref service "serviceSockets"))
                                        (newline))
                                      (assoc-ref server "services"))
                            (newline))
                          #;(map hash-table->alist (json-string->scm (utf8->string body)))
                          (list (hash-table->alist (list-ref (json-string->scm (utf8->string body)) 23))))))
            args))

