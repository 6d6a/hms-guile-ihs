;;; Guile GMS --- GMS command-line interface.
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;;
;;; This file is part of Guile GMS.
;;;
;;; Guile GMS is free software; you can redistribute it and/or modify it under
;;; the terms of the GNU General Public License as published by the Free
;;; Software Foundation; either version 3 of the License, or (at your option)
;;; any later version.
;;;
;;; Guile GMS is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;;; more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with Guile GMS.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gms scripts search)
  #:use-module ((guix scripts) #:select (parse-command-line))
  #:use-module ((guix ui) #:select (G_ leave))
  #:use-module (gms scripts)
  #:use-module (gms scripts account)
  #:use-module (gms ui)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (web client)
  #:export (gms-search
            search-domain
            search-account
            search-owner))

(define (show-help)
  (display (G_ "Usage: gms server [OPTION ...] ACTION [ARG ...] [FILE]
Fetch data about server.\n"))
  (newline)
  (display (G_ "The valid values for ACTION are:\n"))
  (newline)
  (display (G_ "\
   service           search for existing service types\n"))
  (display (G_ "\
   storage           search for existing storage types\n"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  ;; TODO: show-bug-report-information
  #;(show-bug-report-information))

(define %options
  ;; Specifications of the command-line options.
  (list (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))))

(define %default-options '())


;;;
;;;
;;;

(define (search-domain domain)
  (let-values (((response body)
                (http-get (string-append "https://api.majordomo.ru/domain/filter?nameContains="
                                         domain)
                          #:headers `((content-type
                                       . (application/json))
                                      (Authorization
                                       . ,(format #f "Bearer ~a" (auth))))
                          #:keep-alive? #t)))
    (map hash-table->alist (json-string->scm (utf8->string body)))))

(define (search-account account)
  (let-values (((response body)
                (http-get (string-append "https://api.majordomo.ru/pm/accounts?accountId="
                                         account)
                          #:headers `((content-type
                                       . (application/json))
                                      (Authorization
                                       . ,(format #f "Bearer ~a" (auth))))
                          #:keep-alive? #t)))
    (hash-table->alist (json-string->scm (utf8->string body)))))

(define (search-owner owner)
  (let-values (((response body)
                (http-get (string-append "https://api.majordomo.ru/pm/account-owner/search?search="
                                         owner)
                          #:headers `((content-type
                                       . (application/json))
                                      (Authorization
                                       . ,(format #f "Bearer ~a" (auth))))
                          #:keep-alive? #t)))
    (hash-table->alist (json-string->scm (utf8->string body)))))

(define (gms-search . args)
  (define (serialize-account-search account)
    (for-each (lambda (account)
                (format #t "created: ~a~%" (assoc-ref account "created"))
                (format #t "id: ~a~%" (assoc-ref account "clientId"))
                (newline))
              (assoc-ref (search-account (serialize-account account)) "content")))

  (define (serialize-owner-search owner)
    (let ((var (map (lambda (owner)
                      (let ((account (assoc-ref owner "personalAccountId")))
                        (format #t "id: ~a~%" (assoc-ref owner "id"))
                        (format #t "account: ~a~%" account)
                        (format #t "name: ~a~%" (assoc-ref owner "name"))
                        (format #t "type: ~a~%" (assoc-ref owner "type"))
                        account))
                    (assoc-ref (search-owner owner) "content"))))
      ((@@ (ice-9 pretty-print) pretty-print) var)
      var))

  (define (serialize-domain-search domain)
    (for-each (lambda (domain)
                (format #t "name: ~a~%" (assoc-ref domain "name"))
                (format #t "id: ~a~%" (assoc-ref domain "id"))
                (format #t "account: ~a~%" (assoc-ref domain "accountId")))
              (search-domain domain)))

  ;; TODO: with-error-handling
  (match (let ((var args))
           ((@@ (ice-9 pretty-print) pretty-print) var)
           var)
    ((detail? args ...)
     (for-each (lambda (arg)
                 (cond ((string-prefix? "ac_" (string-downcase arg))
                        (serialize-account-search arg))
                       ((string-contains arg "@")
                        (let ((account (serialize-owner-search arg)))
                          (if detail?
                              (match account
                                ((account)
                                 (for-each (lambda (account)
                                             (format #t "created: ~a~%" (assoc-ref account "created"))
                                             (newline))
                                           (assoc-ref (search-account (serialize-account account)) "content"))))
                              account)))
                       (else (serialize-domain-search arg))))
               args))))
