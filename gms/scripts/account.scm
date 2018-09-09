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

(define-module (gms scripts account)
  #:use-module ((guix scripts) #:select (parse-command-line))
  #:use-module ((guix ui) #:select (G_ leave))
  #:use-module (gms scripts)
  #:use-module (gms scripts search)
  #:use-module (gms ui)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (web client)
  #:export (account->scm
            gms-account
            serialize-account
            account-websites))

(define (show-help)
  (display (G_ "Usage: gms account [OPTION ...] ACTION [ARG ...]
Fetch data about user.\n"))
  (newline)
  (display (G_ "The valid values for ACTION are:\n"))
  (newline)
  (display (G_ "\
   domain                show domains on account\n"))
  (display (G_ "\
   service               search for existing service types\n"))
  (display (G_ "\
   show                  show user\n"))
  (display (G_ "\
   unix                  show unix account\n"))
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
          ((domain search service show unix website)
           (alist-cons 'action action result))
          (else (leave (G_ "~a: unknown action~%") action))))))

(define (fetch-account account)
  (let-values (((response body)
                (http-get (string-append "https://api.majordomo.ru/" account
                                         "/account")
                          #:headers `((content-type
                                       . (application/json))
                                      (Authorization
                                       . ,(format #f "Bearer ~a" (auth))))
                          #:keep-alive? #t)))
    (utf8->string body)))

(define (account->scm account)
  (hash-table->alist (json-string->scm (fetch-account account))))

(define (serialize-account account)
    (if (string-prefix? "ac" account)
        (string-take-right account (- (string-length account)
                                      (string-length "ac_")))
        account))

(define (account-websites account)
  (let-values (((response body)
                (http-get (string-append "https://api.majordomo.ru/" account "/website")
                          #:headers `((content-type . (application/json))
                                      (Authorization . ,(format #f "Bearer ~a" (auth))))
                          #:keep-alive? #t)))
    (map hash-table->alist (json-string->scm (utf8->string body)))))

(define (process-command command args opts)
  "Process COMMAND, one of the 'gms server' sub-commands.  ARGS is its
argument list and OPTS is the option alist."
  (define (serialize-args procedure)
    (for-each (lambda (account)
                (procedure (account->scm account)))
              args))

  (define (serialize-website-args procedure)
    (for-each (lambda (account)
                (procedure (first (account-websites account))))
              args))

  (define (serialize-websites-args procedure)
    (for-each (lambda (account)
                (let-values (((response body)
                              (http-get (string-append "https://api.majordomo.ru/" account "/website")
                                        #:headers `((content-type . (application/json))
                                                    (Authorization . ,(format #f "Bearer ~a" (auth))))
                                        #:keep-alive? #t)))
                  (for-each procedure
                            (map hash-table->alist (json-string->scm (utf8->string body))))))
              args))

  (define (serialize-search-user-args procedure)
    (for-each (lambda (account)
                (for-each procedure
                          (assoc-ref (search-account account) "content")))
              args))

  (case command
    ((show)
     (serialize-args
      (lambda (user)
        (format #t "name: ~a~%"
                (assoc-ref user "name"))
        (format #t "active: ~a~%"
                (serialize-boolean (assoc-ref user "active")))
        (format #t "automatic_billing_sending: ~a~%"
                (serialize-boolean (assoc-ref user "autoBillSending")))
        (format #t "notify_days: ~a~%"
                (serialize-boolean (assoc-ref user "notifyDays")))
        (format #t "credit: ~a~%"
                (serialize-boolean (assoc-ref user "credit")))
        (newline))))

    ((search)
     (serialize-search-user-args
      (lambda (account)
        (format #t "created: ~a~%" (assoc-ref account "created"))
        (format #t "id: ~a~%" (assoc-ref account "clientId"))
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
                            (serialize-boolean (assoc-ref service "enabled")))
                    (format #t "last_billed: ~a~%"
                            (assoc-ref service "lastBilled")))
                  (assoc-ref user "services")))))

    ((website)
     (serialize-websites-args
      (lambda (user)
        (format #t "name: ~a~%"
                (assoc-ref user "name"))
        (format #t "document_root: ~a~%"
                (assoc-ref user "documentRoot"))
        (format #t "auto_sub_domain: ~a~%"
                (serialize-boolean (assoc-ref user "autoSubDomain")))
        (format #t "index_file_list: ~a~%"
                (string-join (sort (assoc-ref user "indexFileList")
                                   string<)))
        (format #t "static_file_extensions: ~a~%"
                (string-join (sort (assoc-ref user "staticFileExtensions")
                                   string<)))
        (format #t "cgi_enabled: ~a~%"
                (serialize-boolean (assoc-ref user "cgiEnabled")))
        (format #t "cgi_file_extensions: ~a~%"
                (string-join (assoc-ref user "cgiFileExtensions")))
        (format #t "infected: ~a~%"
                (serialize-boolean (assoc-ref user "infected")))
        (format #t "writable: ~a~%"
                (serialize-boolean (assoc-ref user "writable")))
        (format #t "sendmail_allowed: ~a~%"
                (serialize-boolean (assoc-ref user "sendmailAllowed")))
        (format #t "ddos_protection: ~a~%"
                (serialize-boolean (assoc-ref user "ddosProtection")))
        (newline))))

    ((unix)
     (serialize-website-args
      (lambda (user)
        (let ((unix-account (assoc-ref user "unixAccount")))
          (format #t "quota: ~a/~a GB~%"
                  (serialize-quota (assoc-ref unix-account "quotaUsed"))
                  (serialize-quota (assoc-ref unix-account "quota")))
          (format #t "server_id: ~a~%" (assoc-ref unix-account "serverId"))
          (format #t "home_dir: ~a~%" (assoc-ref unix-account "homeDir"))
          (newline)))))

    ((domain)
     (serialize-websites-args
      (lambda (user)
        (for-each (lambda (domain)
                    (format #t "name: ~a~%" (assoc-ref domain "name"))
                    (match (assoc-ref domain "dnsResourceRecords")
                      ((record records ...)
                       (format #t "records: ~a ~a ~a ~a ~a\n"
                               (assoc-ref record "name")
                               (assoc-ref record "ttl")
                               (assoc-ref record "rrClass")
                               (assoc-ref record "rrType")
                               (assoc-ref record "data"))
                       (for-each (lambda (record)
                                   (format #t "+ ~a ~a ~a ~a ~a\n"
                                           (assoc-ref record "name")
                                           (assoc-ref record "ttl")
                                           (assoc-ref record "rrClass")
                                           (assoc-ref record "rrType")
                                           (assoc-ref record "data")))
                                 records))
                      (_ '()))
                    (newline))
                  (assoc-ref user "domains")))))))

(define (gms-account . args)
  ;; TODO: with-error-handling
  (let* ((opts (parse-command-line args %options
                                   (list %default-options)
                                   #:argument-handler
                                   parse-sub-command))
         (args (map (compose serialize-account string-downcase)
                    (option-arguments opts)))
         (command (assoc-ref opts 'action)))
    (process-command command args opts)))
