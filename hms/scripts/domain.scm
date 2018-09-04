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

(define-module (hms scripts domain)
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
  #:export (hms-domain))


;;;
;;; Entry point.
;;;

(define (hms-domain . args)
  (define (get-domains account)
    (let-values (((response body)
                  (http-get (string-append "https://api.majordomo.ru/" account "/website")
                            #:headers `((content-type . (application/json))
                                        (Authorization . ,(format #f "Bearer ~a" (auth))))
                            #:keep-alive? #t)))
      (unzip1 (map (lambda (json)
                     (map (lambda (domain)
                            (assoc-ref domain "name"))
                          (assoc-ref (hash-table->alist json) "domains")))
                   (json-string->scm (utf8->string body))))))
  (for-each (lambda (account)
              (format #t "account: ~a
uri: https://hms-billing.majordomo.ru/account/~a/resources
domains: ~a~%~%"
                      account account (string-join (get-domains account))))
            args))

