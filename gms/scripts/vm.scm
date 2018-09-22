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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile GMS.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gms scripts vm)
  #:use-module ((guix scripts) #:select (parse-command-line))
  #:use-module ((guix ui) #:select (G_ leave))
  #:use-module (guix import utils)
  #:use-module (gms scripts)
  #:use-module (gms ui)
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
            gms-vm))

(define (show-help)
  (display (G_ "Usage: gms vm [OPTION ...] ACTION [ARG ...]
Fetch data about user.\n"))
  (newline)
  (display (G_ "The valid values for ACTION are:\n"))
  (newline)
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
  ;; TODO: show-bug-report-information
  #;(show-bug-report-information))

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
  (let* ((port   (apply open-pipe* OPEN_READ "jord-cvm" (list account)))
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
          ((ip plan server show template)
           (alist-cons 'action action result))
          (else (leave (G_ "~a: unknown action~%") action))))))

(define (process-command command args opts)
  (define (format-primary-ip-address primary-ip-address)
    (format #t "ip_address: ~a~%" (assoc-ref primary-ip-address "address"))
    (format #t "isp_license: ~a~%" (assoc-ref primary-ip-address "isp_license")))

  (define (format-plan plan)
    (format #t "created: ~a~%" (assoc-ref plan "created"))
    (format #t "name: ~a~%" (assoc-ref plan "name"))
    (format #t "admin: ~a~%" (assoc-ref plan "adm")))

  (define (format-server server)
    (format #t "name: ~a~%" (assoc-ref server "name")))

  (define (format-account account)
    (format #t "created: ~a~%" (assoc-ref account "created"))
    (format #t "changed: ~a~%" (assoc-ref account "changed"))
    (format #t "status: ~a~%" (assoc-ref account "status"))
    (format #t "state: ~a~%" (assoc-ref account "state"))
    (format #t "template_id: ~a~%" (assoc-ref account "vds_template_id"))
    (format #t "client_id: ~a~%" (assoc-ref account "client_id"))
    (format #t "vnc_port: ~a~%" (assoc-ref account "vnc_port")))

  (define (format-template template)
    (format #t "name: ~a~%" (assoc-ref template "name"))
    (format #t "URI: ~a~%" (assoc-ref template "path")))

  (for-each (lambda (arg)
              (let ((vds-account (vm->scm arg)))
                (case command
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

(define (gms-vm . args)
  ;; TODO: with-error-handling
  (let* ((opts (parse-command-line args %options
                                   (list %default-options)
                                   #:argument-handler
                                   parse-sub-command))
         (args (option-arguments opts))
         (command (assoc-ref opts 'action)))
    (process-command command args opts)))
