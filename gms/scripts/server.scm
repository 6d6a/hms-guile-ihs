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

(define-module (gms scripts server)
  #:use-module ((guix scripts) #:select (parse-command-line))
  #:use-module ((guix ui)  #:select (leave G_))
  #:use-module (guix import utils)
  #:use-module (gms scripts)
  #:use-module (gms ui)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:use-module (web client)
  #:export (gms-server))

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
;;; Entry point.
;;;

(define (tester)
  (let-values (((response body)
                   (http-get (string-append "https://api.majordomo.ru/rc-staff/server")
                             #:headers `((content-type . (application/json))
                                         (Authorization . ,(format #f "Bearer ~a" (pk (auth)))))
                             #:keep-alive? #t)))
       (with-output-to-file "/tmp/gms-servers.scm"
         (lambda ()
           (write (map hash-table->alist (json-string->scm (utf8->string body))))))))

(define (parse-sub-command arg result)
  ;; Parse sub-command ARG and augment RESULT accordingly.
  (if (assoc-ref result 'action)
      (alist-cons 'argument arg result)
      (let ((action (string->symbol arg)))
        (case action
          ((service show socket storage)
           (alist-cons 'action action result))
          (else (leave (G_ "~a: unknown action~%") action))))))

;; TODO: Use json.
#;(map hash-table->alist (json-string->scm (utf8->string body)))
#;(list (hash-table->alist (list-ref (json-string->scm (utf8->string body)) 23)))

(define (process-command command args opts)
  "Process COMMAND, one of the 'gms server' sub-commands.  ARGS is its
argument list and OPTS is the option alist."
  (define (serialize-args procedure)
    (for-each (lambda (arg)
                (for-each (lambda (server)
                            (if (string=? (assoc-ref server "name") arg)
                                (procedure server)
                                '()))
                          (with-input-from-file "/tmp/gms-servers.scm" read)))
              args))

  (case command
    ((service)
     (serialize-args (lambda (server)
                       (for-each (lambda (service)
                                   (format #t "id: ~a~%"
                                           (assoc-ref service "id"))
                                   (format #t "name: ~a~%"
                                           (assoc-ref service "name"))
                                   (newline))
                                 (assoc-ref server "services")))))
    ((storage)
     (serialize-args (lambda (server)
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
                                 (assoc-ref server "storages")))))
    ((socket)
     (serialize-args
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
                  (assoc-ref server "services")))))
    ((show)
     (serialize-args
      (lambda (server)
        (format #t "id: ~a~%"
                (assoc-ref server "id"))
        (format #t "name: ~a~%"
                (assoc-ref server "name"))
        (format #t "online: ~a~%"
                (serialize-boolean (assoc-ref server "switchedOn")))
        (newline))))))

(define (gms-server . args)
  ;; TODO: with-error-handling
  (let* ((opts (parse-command-line args %options
                                   (list %default-options)
                                   #:argument-handler
                                   parse-sub-command))
         (args (option-arguments opts))
         (command (assoc-ref opts 'action)))
    (process-command command args opts)))
