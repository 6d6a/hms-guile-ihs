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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile HMS.  If not, see <http://www.gnu.org/licenses/>.

(define-module (hms scripts account)
  #:use-module ((guix scripts) #:select (parse-command-line))
  #:use-module ((guix ui) #:select (G_ leave))
  #:use-module (hms scripts)
  #:use-module (hms ui)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (web client)
  #:export (account->scm
            hms-account))

(define (show-help)
  (display (G_ "Usage: hms account [OPTION ...] ACTION [ARG ...]
Fetch data about user."))
  (newline)
  (display (G_ "The valid values for ACTION are:\n"))
  (newline)
  (display (G_ "\
   service               search for existing service types\n"))
  (display (G_ "\
   show                  show user\n"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  ;; TODO: version
  #;(display (G_ "
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
;;; Entry point.
;;;

(define (parse-sub-command arg result)
  ;; Parse sub-command ARG and augment RESULT accordingly.
  (if (assoc-ref result 'action)
      (alist-cons 'argument arg result)
      (let ((action (string->symbol arg)))
        (case action
          ((show service)
           (alist-cons 'action action result))
          (else (leave (G_ "~a: unknown action~%") action))))))

(define (fetch-account account)
  (let-values (((response body)
                (http-get (string-append "https://api.majordomo.ru/" account "/account")
                          #:headers `((content-type . (application/json))
                                      (Authorization . ,(format #f "Bearer ~a" (auth))))
                          #:keep-alive? #t)))
    (utf8->string body)))

(define (account->scm account)
  (hash-table->alist (json-string->scm (fetch-account account))))

(define (process-command command args opts)
  "Process COMMAND, one of the 'hms server' sub-commands.  ARGS is its
argument list and OPTS is the option alist."
  (define (serialize-args procedure)
    (for-each (lambda (account)
                (procedure (account->scm account)))
              args))

  (case command
    ((show)
     (serialize-args
      (lambda (user)
        (format #t "name: ~a~%"
                (assoc-ref user "name"))
        (format #t "active: ~a~%"
                (serialize-boolean
                 (assoc-ref user "active")))
        (format #t "automatic_billing_sending: ~a~%"
                (serialize-boolean
                 (assoc-ref user "autoBillSending")))
        (format #t "notify_days: ~a~%"
                (serialize-boolean
                 (assoc-ref user "notifyDays")))
        (format #t "credit: ~a~%"
                (serialize-boolean
                 (assoc-ref user "credit")))
        (newline))))

    ((service)
     (serialize-args
      (lambda (user)
        (for-each (lambda (service)
                    (format #t "name: ~a~%"
                            (assoc-ref service "name"))
                    (format #t "cost: ~a rub~%"
                            (assoc-ref service "cost"))
                    (format #t "enabled: ~a~%"
                            (serialize-boolean
                             (assoc-ref service "enabled")))
                    (format #t "last_billed: ~a~%"
                            (assoc-ref service "lastBilled")))
                  (assoc-ref user "services")))))))

(define (hms-account . args)
  ;; TODO: with-error-handling
  (let* ((opts (parse-command-line args %options
                                   (list %default-options)
                                   #:argument-handler
                                   parse-sub-command))
         (args (option-arguments opts))
         (command (assoc-ref opts 'action)))
    (process-command command args opts)))
