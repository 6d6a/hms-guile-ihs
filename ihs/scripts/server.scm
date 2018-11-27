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

(define-module (ihs scripts server)
  #:use-module ((guix build utils) #:select (mkdir-p))
  #:use-module ((guix scripts) #:select (parse-command-line))
  #:use-module ((guix ui)  #:select (leave G_))
  #:use-module (ihs scripts)
  #:use-module (ihs ui)
  #:use-module (ihs utils)
  #:use-module (guix import utils)
  #:use-module (ice-9 pretty-print)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (web client)
  #:export (ihs-server
            update-cache
            serialize-server-args))

(define %cache-file
  (string-append (cache-directory) "/servers.scm"))


;;;
;;; Command-line
;;;

(define (show-help)
  (display (G_ "Usage: ihs server [OPTION ...] ACTION [ARG ...] [FILE]
Fetch data about server.\n"))
  (newline)
  (display (G_ "The valid values for ACTION are:\n"))
  (newline)
  (display (G_ "\
   service           search for existing service types\n"))
  (display (G_ "\
   show              show server\n"))
  (display (G_ "\
   sockets           display sockets\n"))
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
;;; Entry point.
;;;

(define (fetch-server)
  (let-values (((response body)
                (http-get (string-append "https://api.majordomo.ru/rc-staff\
/server")
                          #:headers `((content-type . (application/json))
                                      (Authorization . ,(format #f "Bearer ~a"
                                                                (auth))))
                          #:keep-alive? #t)))
    (utf8->string body)))

(define (server->scm)
  (map hash-table->alist (json-string->scm (fetch-server))))

(define (update-cache)
  (with-output-to-file %cache-file
    (lambda ()
      (pretty-print (server->scm)))))

(define (parse-sub-command arg result)
  ;; Parse sub-command ARG and augment RESULT accordingly.
  (if (assoc-ref result 'action)
      (alist-cons 'argument arg result)
      (let ((action (string->symbol arg)))
        (case action
          ((service show socket storage)
           (alist-cons 'action action result))
          (else (leave (G_ "~a: unknown action~%") action))))))

(define (serialize-server-args procedure args)
  (for-each (lambda (arg)
              (for-each (lambda (server)
                          (if (or (string=? (assoc-ref server "name") arg)
                                  (string=? (assoc-ref server "id") arg))
                              (procedure server)
                              '()))
                        (with-input-from-file %cache-file read)))
            args))

(define (process-command command args opts)
  "Process COMMAND, one of the 'ihs server' sub-commands.  ARGS is its
argument list and OPTS is the option alist."
  (case command
    ((service)
     (serialize-server-args
      (lambda (server)
        (for-each (lambda (service)
                    (format #t "id: ~a~%"
                            (assoc-ref service "id"))
                    (format #t "name: ~a~%"
                            (assoc-ref service "name"))
                    (newline))
                  (assoc-ref server "services")))
      args))
    ((storage)
     (serialize-server-args
      (lambda (server)
        (for-each (lambda (storage)
                    (format #t "id: ~a~%"
                            (assoc-ref storage "id"))
                    (format #t "name: ~a~%"
                            (assoc-ref storage "name"))
                    (format #t "online: ~a~%"
                            (serialize-boolean (assoc-ref storage "switchedOn")))
                    ;; TODO: Check capacity size.
                    (format #t "capacity: ~a/~a GB~%"
                            (serialize-quota (assoc-ref storage "capacityUsed"))
                            (serialize-quota (assoc-ref storage "capacity")))
                    (newline))
                  (assoc-ref server "storages")))
      args))
    ((socket)
     (serialize-server-args
      (lambda (server)
        (for-each (lambda (service)
                    (for-each (lambda (socket)
                                (format #t "id: ~a~%"
                                        (assoc-ref socket "id"))
                                (format #t "name: ~a~%"
                                        (assoc-ref socket "name"))
                                (format #t "address: ~a~%"
                                        (assoc-ref socket "address"))
                                (format #t "port: ~a~%"
                                        (assoc-ref socket "port"))
                                (format #t "online: ~a~%"
                                        (serialize-boolean
                                         (assoc-ref socket "switchedOn")))
                                (newline))
                              (assoc-ref service "serviceSockets")))
                  (assoc-ref server "services")))
      args))
    ((show)
     (serialize-server-args
      (lambda (server)
        (format #t "id: ~a~%"
                (assoc-ref server "id"))
        (format #t "name: ~a~%"
                (assoc-ref server "name"))
        (format #t "online: ~a~%"
                (serialize-boolean (assoc-ref server "switchedOn")))
        (newline))
      args))))

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
              (G_ "ihs server: missing command name~%"))
      (format (current-error-port)
              (G_ "Try 'ihs server --help' for more information.~%"))
      (exit 1))

    args))

(define (ihs-server . args)
  ;; TODO: with-error-handling
  (let* ((opts (parse-command-line args %options
                                   (list %default-options)
                                   #:argument-handler
                                   parse-sub-command))
         (args (option-arguments opts))
         (command (assoc-ref opts 'action)))
    (process-command command args opts)))
