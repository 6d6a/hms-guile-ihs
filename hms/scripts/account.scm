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

(define-module (hms scripts account)
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
  #:export (hms-account))


;;;
;;; Entry point.
;;;

(define (hms-account . args)
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
  (for-each (lambda (account)
              (let-values (((response body)
                            (http-get (string-append "https://api.majordomo.ru/" account "/account")
                                      #:headers `((content-type . (application/json))
                                                  (Authorization . ,(format #f "Bearer ~a" (auth))))
                                      #:keep-alive? #t)))
                (let ((json (hash-table->alist (json-string->scm (utf8->string body)))))
                  (format #t "name: ~a~%" (assoc-ref json "name"))
                  (format #t "active: ~a~%" (serialize-boolean (assoc-ref json "active")))
                  (format #t "automatic_billing_sending: ~a~%" (serialize-boolean (assoc-ref json "autoBillSending")))
                  (format #t "notify_days: ~a~%" (serialize-boolean (assoc-ref json "notifyDays")))
                  (format #t "credit: ~a~%" (serialize-boolean (assoc-ref json "credit")))
                  #;(let ((services (assoc-ref json "services")))
                    (pretty-print services)
                    (format #t "name: ~a~%" (assoc-ref services "name"))
                    (format #t "cost: ~a rub~%" (assoc-ref services "cost"))
                    (format #t "enabled: ~a~%" (serialize-boolean (assoc-ref services "enabled")))
                    (format #t "last_billed: ~a~%" (assoc-ref services "lastBilled")))
                  (newline))))
            args))

