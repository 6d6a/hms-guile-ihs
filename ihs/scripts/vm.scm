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

(define-module (ihs scripts vm)
  #:use-module ((guix scripts) #:select (parse-command-line))
  #:use-module ((guix ui) #:select (G_ leave))
  #:use-module (guix import utils)
  #:use-module (ihs config)
  #:use-module (ihs scripts)
  #:use-module (ihs ui)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (web client)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (vm->scm
            fetch-vm
            ihs-vm))

(define (show-help)
  (display (G_ "Usage: ihs vm [OPTION ...] ACTION [ARG ...]
Fetch data about user.\n"))
  (newline)
  (display (G_ "The valid values for ACTION are:\n"))
  (newline)
  (display (G_ "\
   dump                  dump all available information\n"))
  (display (G_ "\
   show                  show user\n"))
  (display (G_ "\
   ip                    show ip address\n"))
  (display (G_ "\
   plan                  show account plan\n"))
  (display (G_ "\
   server                show server\n"))
  (display (G_ "\
   template              show operating system template\n"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
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
                   (exit 0)))
        (option '(#\n "Don't convert addresses (i.e., host addresses, port
numbers, etc.) to names.") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'do-not-resolve? #t result)))))

(define %default-options '())

(define (fetch-vm account)
  (let* ((port   (apply open-pipe* OPEN_READ %cvm (list account)))
         (output (read-string port)))
    (close-port port)
    (string-trim-right output #\newline)))

(define (vm->scm account)
  (assoc-ref (hash-table->alist (json-string->scm (fetch-vm account)))
             "vds_account"))

(define (parse-sub-command arg result)
  ;; Parse sub-command ARG and augment RESULT accordingly.
  (if (assoc-ref result 'action)
      (alist-cons 'argument arg result)
      (let ((action (string->symbol arg)))
        (case action
          ((dump ip plan server show template)
           (alist-cons 'action action result))
          (else (leave (G_ "~a: unknown action~%") action))))))

(define (process-command command args opts)
  (define (format-primary-ip-address primary-ip-address)
    (format #t "ip_address: ~a~%" (assoc-ref primary-ip-address "address"))
    (format #t "isp_license: ~a~%"
            (serialize-boolean (assoc-ref primary-ip-address "isp_license"))))

  (define (format-plan plan)
    (format #t "name: ~a~%" (assoc-ref plan "name"))
    (format #t "admin: ~a~%" (serialize-boolean (assoc-ref plan "adm")))
    (format #t "created: ~a~%" (assoc-ref plan "created")))

  (define (format-server server)
    (format #t "name: ~a~%" (assoc-ref server "name")))

  (define (format-account account)
    (format #t "client_id: ~a~%" (assoc-ref account "client_id"))
    (format #t "vnc_port: ~a~%" (assoc-ref account "vnc_port"))
    (format #t "state: ~a~%" (assoc-ref account "state"))
    (format #t "status: ~a~%" (assoc-ref account "status"))
    (format #t "changed: ~a~%" (assoc-ref account "changed"))
    (format #t "created: ~a~%" (assoc-ref account "created"))
    (format #t "template_id: ~a~%" (assoc-ref account "vds_template_id")))

  (define (format-template template)
    (format #t "name: ~a~%" (assoc-ref template "name"))
    (format #t "uri: ~a~%" (assoc-ref template "path")))

  (for-each (lambda (arg)
              (let ((vds-account (vm->scm arg)))
                (case command
                  ((dump)
                   (format-account vds-account)
                   (format-template (assoc-ref vds-account "template"))
                   (format-plan (assoc-ref vds-account "plan"))
                   (format-primary-ip-address
                    (assoc-ref vds-account "primary_ip_address"))
                   (format-server (assoc-ref (assoc-ref vds-account "host")
                                             "server"))
                   (newline))
                  ((ip)
                   (format-primary-ip-address
                    (assoc-ref vds-account "primary_ip_address"))
                   (newline))
                  ((plan)
                   (format-plan (assoc-ref vds-account "plan"))
                   (newline))
                  ((server)
                   (format-server (assoc-ref (assoc-ref vds-account "host")
                                             "server"))
                   (newline))
                  ((show)
                   (format-account vds-account)
                   (newline))
                  ((template)
                   (format-template (assoc-ref vds-account "template"))
                   (newline)))))
            args))

(define (option-arguments opts)
  ;; Extract the plain arguments from OPTS.
  (let* ((args   (reverse (filter-map (match-pair 'argument) opts)))
         (count  (length args))
         (action (assoc-ref opts 'action))
         (expr   (assoc-ref opts 'expression)))
    (define (fail)
      (leave (G_ "wrong number of arguments for action '~a'~%")
             action))

    (unless action
      (format (current-error-port)
              (G_ "ihs vm: missing command name~%"))
      (format (current-error-port)
              (G_ "Try 'ihs vm --help' for more information.~%"))
      (exit 1))

    args))

(define (ihs-vm . args)
  ;; TODO: with-error-handling
  (let* ((opts (parse-command-line args %options
                                   (list %default-options)
                                   #:argument-handler
                                   parse-sub-command))
         (args (option-arguments opts))
         (command (assoc-ref opts 'action)))
    (process-command command args opts)))
