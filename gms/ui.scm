;;; Guile GMS --- GMS command-line interface.
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile GMS.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gms ui)
  #:autoload   (ice-9 ftw)  (scandir)
  #:use-module (ice-9 match)
  #:use-module (guix import utils)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (web client)
  #:use-module (web response)
  #:export (auth
            gms-main))

(define gms-user
  (getenv "GMS_USER"))

(define gms-password
  (getenv "GMS_PASS"))

(define* (auth #:key (user gms-user) (pass gms-password))
  (letrec-syntax ((option (syntax-rules ()
                            ((_ key value)
                             (if value
                                 (list (string-append key "=" value))
                                 '()))))
                  (key/value (syntax-rules ()
                               ((_ (key value) rest ...)
                                (append (option key value)
                                        (key/value rest ...)))
                               ((_)
                                '()))))
    (assoc-ref (let-values
                   (((response body)
                     (http-post "https://api.majordomo.ru/oauth/token"
                                #:headers `((content-type . (application/x-www-form-urlencoded)))
                                #:keep-alive? #t
                                #:body (string-join (key/value ("grant_type" "password")
                                                               ("client_id" "frontend_app")
                                                               ("client_secret" "frontend_app_secret")
                                                               ("username" user)
                                                               ("password" pass))
                                                    "&"))))
                 (hash-table->alist (json-string->scm (utf8->string body))))
               "access_token")))


;;;
;;; ui
;;;

(define (show-gms-usage)
  (format (current-error-port)
          "Try `gms --help' for more information.~%")
  (exit 1))

(define (command-files)
  "Return the list of source files that define GMS sub-commands."
  (define directory
    (and=> (search-path %load-path "gms.scm")
           (compose (cut string-append <> "/gms/scripts")
                    dirname)))

  (define dot-scm?
    (cut string-suffix? ".scm" <>))

  (if directory
      (scandir directory dot-scm?)
      '()))

(define (commands)
  "Return the list of GMS command names."
  (map (compose (cut string-drop-right <> 4)
                basename)
       (command-files)))

(define (show-gms-help)
  (format
    #t
    "Usage: gms COMMAND ARGS...\nRun COMMAND with ARGS.\n")
  (newline)
  (format
    #t
    "COMMAND must be one of the sub-commands listed below:\n")
  (newline)
  (format
    #t
    "~{   ~a~%~}"
    (sort (commands) string<?)))

(define program-name
  ;; Name of the command-line program currently executing, or #f.
  (make-parameter #f))

(define (run-gms-command command . args)
  "Run COMMAND with the given ARGS.  Report an error when COMMAND is not
found."
  (define module
    (catch 'misc-error
      (lambda ()
        (resolve-interface `(gms scripts ,command)))
      (lambda -
        (format (current-error-port)
                "gms: ~a: command not found~%" command)
        (show-gms-usage))))

  (let ((command-main (module-ref module
                                  (symbol-append 'gms- command))))
    (parameterize ((program-name command))
      ;; Disable canonicalization so we don't don't stat unreasonably.
      (with-fluids ((%file-port-name-canonicalization #f))
        (dynamic-wind
          (const #f)
          (lambda ()
            (apply command-main args))
          (lambda ()
            ;; Abuse 'exit-hook' (which is normally meant to be used by the
            ;; REPL) to run things like profiling hooks upon completion.
            (run-hook exit-hook)))))))

(define (run-gms . args)
  "Run the 'gms' command defined by command line ARGS."
  ;; The default %LOAD-EXTENSIONS includes the empty string, which doubles the
  ;; number of 'stat' calls per entry in %LOAD-PATH.  Shamelessly remove it.
  (set! %load-extensions '(".scm"))

  (match args
    (()
     (format (current-error-port) "gms: missing command name~%")
     (show-gms-usage))
    ((or ("-h") ("--help"))
     (show-gms-help))
    ((command args ...)
     (apply run-gms-command (string->symbol command) args))))

(define (gms-main arg0 . args)
  (apply run-gms args))


;;;
;;; main
;;;

(define* (main #:optional (args (command-line)))
  (exit (apply gms-main args)))
