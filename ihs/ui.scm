;;; Guile IHS --- IHS command-line interface.
;;; Copyright © 2018, 2019 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (ihs ui)
  #:autoload   (ice-9 ftw)  (scandir)
  #:use-module ((guix ui)  #:select (G_))
  #:use-module (ihs config)
  #:use-module (guix import utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (web client)
  #:use-module (web response)
  #:export (show-bug-report-information
            ihs-main))


;;;
;;; UI
;;;

(define (show-bug-report-information)
  (format #t (G_ "
Report bugs to: ~a.") %ihs-bug-report-address)
  (format #t (G_ "
~a home page: <~a>") %ihs-package-name %ihs-home-page-url)
  (newline))

(define (show-ihs-usage)
  (format (current-error-port)
          "Try `ihs --help' for more information.~%")
  (exit 1))

(define (command-files)
  "Return the list of source files that define ihs sub-commands."
  (define directory
    (and=> (search-path %load-path "ihs.scm")
           (compose (cut string-append <> "/ihs/scripts")
                    dirname)))

  (define dot-scm?
    (cut string-suffix? ".scm" <>))

  (if directory
      (scandir directory dot-scm?)
      '()))

(define (commands)
  "Return the list of ihs command names."
  (map (compose (cut string-drop-right <> 4)
                basename)
       (command-files)))

(define (show-ihs-help)
  (format
    #t
    "Usage: ihs COMMAND ARGS...\nRun COMMAND with ARGS.\n")
  (newline)
  (format
    #t
    "COMMAND must be one of the sub-commands listed below:\n")
  (newline)
  (format
    #t
    "~{   ~a~%~}"
    (sort (commands) string<?))
  (show-bug-report-information))

(define program-name
  ;; Name of the command-line program currently executing, or #f.
  (make-parameter #f))

(define (run-ihs-command command . args)
  "Run COMMAND with the given ARGS.  Report an error when COMMAND is not
found."
  (define module
    (catch 'misc-error
      (lambda ()
        (resolve-interface `(ihs scripts ,command)))
      (lambda -
        (format (current-error-port)
                "ihs: ~a: command not found~%" command)
        (show-ihs-usage))))

  (let ((command-main (module-ref module
                                  (symbol-append 'ihs- command))))
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

(define (run-ihs . args)
  "Run the 'ihs' command defined by command line ARGS."
  ;; The default %LOAD-EXTENSIONS includes the empty string, which doubles the
  ;; number of 'stat' calls per entry in %LOAD-PATH.  Shamelessly remove it.
  (set! %load-extensions '(".scm"))

  (match args
    (()
     (format (current-error-port) "ihs: missing command name~%")
     (show-ihs-usage))
    ((or ("-h") ("--help"))
     (show-ihs-help))
    ((command args ...)
     (apply run-ihs-command (string->symbol command) args))))

(define (ihs-main arg0 . args)
  (apply run-ihs args))


;;;
;;; Main
;;;

(define* (main #:optional (args (command-line)))
  (exit (apply ihs-main args)))
