;;; Guile GMS --- GMS command-line interface.
;;; Copyright © 2018, 2019, 2022 Oleg Pykhalov <go.wigust@gmail.com>
;;;
;;; This file is part of Guile GMS.
;;;
;;; Guile GMS is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation; either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; Guile GMS is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile GMS.  If not, see <http://www.gnu.org/licenses/>.

(define-module (ihs scripts web search)
  #:use-module ((guix scripts) #:select (parse-command-line))
  #:use-module ((guix ui) #:select (G_ leave))
  #:use-module (guix import utils)
  #:use-module (ihs hms)
  #:use-module (ihs scripts)
  #:use-module (ihs scripts web)
  #:use-module (ihs ui)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (web client)
  #:export (ihs-web-search))

(define (show-help)
  (display (G_ "Usage: ihs server [OPTION ...] ACTION [ARG ...] [FILE]
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
  (show-bug-report-information))

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

(define (domain->scm domain)
  (json-string->scm (search-domain domain)))

(define (account->scm account)
  (json-string->scm (search-account account)))

(define (owner->scm owner)
  (json-string->scm (search-owner owner)))

(define (serialize-owner owner)
  (let ((account (assoc-ref owner "personalAccountId")))
    (format #t "id: ~a~%" (assoc-ref owner "id"))
    (format #t "account: ~a~%" account)
    (format #t "name: ~a~%" (assoc-ref owner "name"))
    (format #t "type: ~a~%" (assoc-ref owner "type"))
    account))

(define (serialize-domain domain)
  (format #t "name: ~a~%" (assoc-ref domain "name"))
  (format #t "id: ~a~%" (assoc-ref domain "id"))
  (format #t "account: ~a~%" (assoc-ref domain "accountId"))
  (format #t "active: ~a~%" (colorize (serialize-boolean (assoc-ref domain "switchedOn")))))

(define (serialize-account-number account)
  (format #t "created: ~a~%" (assoc-ref account "created"))
  (format #t "id: ~a~%" (assoc-ref account "clientId")))

(define (ihs-web-search . args)
  ;; TODO: with-error-handling
  (for-each (lambda (arg)
              (cond ((string-prefix? "ac_" (string-downcase arg))
                     (for-each (lambda (account)
                                 (serialize-account-number account)
                                 (newline))
                               (array->list
                                (assoc-ref (account->scm (serialize-account arg))
                                           "content"))))
                    ((string-contains arg "@")
                     (map (lambda (owner)
                            (serialize-owner owner)
                            (newline))
                          (array->list
                           (assoc-ref (owner->scm arg) "content"))))
                    (else (for-each (lambda (domain)
                                      (serialize-domain domain)
                                      (newline))
                                    (array->list (domain->scm arg))))))
            args))
