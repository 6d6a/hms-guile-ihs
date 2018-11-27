;;; Guile IHS --- IHS command-line interface.
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (ihs scripts web panel)
  #:use-module ((guix scripts) #:select (parse-command-line))
  #:use-module ((guix ui) #:select (G_ leave))
  #:use-module (guix build utils)
  #:use-module (guix import utils)
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
  #:export (ihs-web-panel))

(define (show-help)
  (display (G_ "Usage: ihs account panel [OPTION ...] ACTION [ARG ...] [FILE]
Panel a browser to configure account.\n"))
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

(define (ihs-web-panel . args)
  ;; TODO: with-error-handling
  (for-each (lambda (account)
              (let-values (((response body)
                            (http-post (string-append "https://api.majordomo.ru/si/web-access-accounts/"
                                                      account
                                                      "/create_token")
                                       #:headers `((content-type . (application/json))
                                                   (Authorization . ,(format #f "Bearer ~a" (auth))))
                                       #:body "{}"
                                       #:keep-alive? #t)))
                (let ((json (hash-table->alist (json-string->scm (utf8->string body)))))
                  ;; TODO: Open browser for all accounts in parallel
                  (for-each (match-lambda
                              (("token" records ...)
                               (let ((account-profile (string-append "/tmp/" account)))
                                 (mkdir-p account-profile)
                                 (system* "firefox" "--new-instance"
                                          "--profile" account-profile
                                          "--private-window"
                                          (string-append "https://hms.majordomo.ru/login"
                                                         "?bearer=" (assoc-ref records "access_token")
                                                         "&refresh=" (assoc-ref records "refresh_token")))
                                 (delete-file-recursively account-profile)))
                              (_ #f))
                            (assoc-ref json "params")))))
            args))
