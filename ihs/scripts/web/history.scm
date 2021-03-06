;;; Guile IHS --- IHS command-line interface.
;;; Copyright © 2018, 2019, 2022 Oleg Pykhalov <go.wigust@gmail.com>
;;;
;;; This file is part of Guile IHS.
;;;
;;; Guile IHS is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation; either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; Guile IHS is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile IHS.  If not, see <http://www.gnu.org/licenses/>.

(define-module (ihs scripts web history)
  #:use-module ((guix scripts) #:select (parse-command-line))
  #:use-module ((guix ui) #:select (G_ leave))
  #:use-module (guix import utils)
  #:use-module (ihs hms)
  #:use-module (ihs scripts)
  #:use-module (ihs ui)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (web client)
  #:use-module (ihs scripts web)
  #:export (ihs-web-history))

(define (show-help)
  (display (G_ "Usage: ihs history [OPTION ...] ACTION [ARG ...]
Fetch history about a user.\n"))
  (newline)
  (display (G_ "The valid values for ACTION are:\n"))
  (newline)
  ;; TODO: version
  #;(display (G_ "
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
;;; Entry point.
;;;

(define* (fetch-history #:key account page)
  (let-values (((response body)
                ;; https://api.majordomo.ru/pm/208112/account-history?page=0&created=2017-01-01+00:00:00&created=2018-09-11+23:59:59&sort=created,desc
                (http-get (string-append "https://api.majordomo.ru/pm/" account
                                         "/account-history"
                                         "?page=" (number->string page)
                                         "&sort=created,desc")
                          #:headers `((content-type
                                       . (application/json))
                                      (Authorization
                                       . ,(format #f "Bearer ~a" (auth))))
                          #:keep-alive? #t)))
    (utf8->string body)))

(define* (history->scm #:key account (page 0))
  (array->list (json-string->scm (fetch-history #:account account
                                                      #:page page))))

(define (ihs-web-history . args)
  ;; TODO: with-error-handling
  (let* ((history (history->scm #:account (car args)))
         (pages (assoc-ref history "totalPages")))
    (letrec ((fetch-page (lambda (page)
                           (if (> page pages)
                               '()
                               (begin
                                 (for-each (lambda (record)
                                             (let ((operator (assoc-ref record "operator")))
                                               (when (not (string=? operator "service"))
                                                 (format #t "id: ~a~%" (assoc-ref record "id"))
                                                 (format #t "created: ~a~%" (assoc-ref record "created"))
                                                 (format #t "operator: ~a~%" operator)
                                                 (format #t "message: ~a~%" (assoc-ref record "message"))
                                                 (newline))))
                                           (assoc-ref (history->scm #:account (car args)
                                                                    #:page page)
                                                      "content"))
                                 (cons page (fetch-page (1+ page))))))))
      (fetch-page 5))))
