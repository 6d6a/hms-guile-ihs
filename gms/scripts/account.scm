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

(define-module (gms scripts account)
  #:use-module ((guix scripts) #:select (parse-command-line))
  #:use-module ((guix ui) #:select (G_ leave))
  #:use-module (guix build utils)
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
  #:use-module (gms scripts server)
  #:export (account->scm
            gms-account
            serialize-account

            account-websites
            account-websites->scm))

(define (show-help)
  (display (G_ "Usage: gms account [OPTION ...] ACTION [ARG ...]
Fetch data about user.\n"))
  (newline)
  (display (G_ "The valid values for ACTION are:\n"))
  (newline)
  (display (G_ "\
   database              show database on account\n"))
  (display (G_ "\
   database-user         show database users on account\n"))
  (display (G_ "\
   domain                show domains on account\n"))
  (display (G_ "\
   dump                  dump all available information\n"))
  (display (G_ "\
   mailbox               show mailboxes on account\n"))
  (display (G_ "\
   service               search for existing service types\n"))
  (display (G_ "\
   open                  open billing\n"))
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


;;;
;;; Entry point.
;;;

(define (domain->ip domain)
  (let ((radix 2))
    (map (cut string->number <> radix)
         (letrec ((split-octet
                   (lambda* (#:key str (pos 0) #:allow-other-keys)
                     (if (<= 32 pos)
                         '()
                         (let ((next-pos (+ pos 8)))
                           (cons (string-copy str pos next-pos)
                                 (split-octet #:str str #:pos next-pos)))))))
           (split-octet #:str (number->string (vector-ref (addrinfo:addr (first domain))
                                                          1)
                                              radix))))))

(define (resolve-domain domain)
  (define (resolved? domain)
    (catch 'getaddrinfo-error
      (lambda ()
        (getaddrinfo domain))
      (lambda (key errcode)
        (cond ((= errcode EAI_SERVICE)
               (format #t "resolve: doesn't know about ~a~%" domain)
               #f)
              ((= errcode EAI_NONAME)
               (format #t "resolve: ~a not found~%" domain)
               #f)
              (else
               (format #t "resolve: something is wrong ~a"
          	       (gai-strerror errcode))
               #f)))))
  (let ((domain (resolved? domain)))
    (when domain
      (format #t "resolve: ~a~%"
              (string-join (map number->string (domain->ip domain))
                           ".")))))

(define (parse-sub-command arg result)
  ;; Parse sub-command ARG and augment RESULT accordingly.
  (if (assoc-ref result 'action)
      (alist-cons 'argument arg result)
      (let ((action (string->symbol arg)))
        (case action
          ((database database-user domain dump history mailbox search service
            show open unix website)
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
    (utf8->string body)))

(define (account-websites->scm account)
  (map hash-table->alist (json-string->scm (account-websites account))))

(define (account-mailbox account)
  (let-values (((response body)
                (http-get (string-append "https://api.majordomo.ru/" account "/mailbox")
                          #:headers `((content-type . (application/json))
                                      (Authorization . ,(format #f "Bearer ~a" (auth))))
                          #:keep-alive? #t)))
    (utf8->string body)))

(define (account-mailbox->scm account)
  (map hash-table->alist (json-string->scm (account-mailbox account))))

(define (account-database account)
  (let-values (((response body)
                (http-get (string-append "https://api.majordomo.ru/" account "/database")
                          #:headers `((content-type . (application/json))
                                      (Authorization . ,(format #f "Bearer ~a" (auth))))
                          #:keep-alive? #t)))
    (utf8->string body)))

(define (account-database->scm account)
  (map hash-table->alist (json-string->scm (account-database account))))

(define (resolve-subcommand name)
  (let ((module (resolve-interface
                 `(gms scripts account ,(string->symbol name))))
        (proc (string->symbol (string-append "gms-account-" name))))
    (module-ref module proc)))

(define (process-command command args opts)
  "Process COMMAND, one of the 'gms server' sub-commands.  ARGS is its
argument list and OPTS is the option alist."
  (let ((resolve? (assoc-ref opts 'do-not-resolve?)))
    (define (serialize-args procedure)
      (for-each (lambda (account)
                  (procedure (account->scm account)))
                args))

    (define (serialize-website-args procedure)
      (for-each (lambda (account)
                  (procedure (first (account-websites->scm account))))
                args))

    (define (serialize-websites-args procedure)
      (for-each (lambda (account)
                  (for-each procedure (account-websites->scm account)))
                args))

    (define (format-user user)
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
      (newline))

    (define (format-service service)
      (format #t "name: ~a~%"
              (assoc-ref service "name"))
      (format #t "cost: ~a rub~%"
              (assoc-ref service "cost"))
      (format #t "enabled: ~a~%"
              (serialize-boolean (assoc-ref service "enabled")))
      (format #t "last_billed: ~a~%"
              (assoc-ref service "lastBilled"))
      (newline))

    (define (format-website website)
      (format #t "name: ~a~%"
              (assoc-ref website "name"))
      (format #t "document_root: ~a~%"
              (assoc-ref website "documentRoot"))
      (format #t "auto_sub_domain: ~a~%"
              (serialize-boolean (assoc-ref website "autoSubDomain")))
      (format #t "index_file_list: ~a~%"
              (string-join (sort (assoc-ref website "indexFileList")
                                 string<)))
      (format #t "static_file_extensions: ~a~%"
              (string-join (sort (assoc-ref website "staticFileExtensions")
                                 string<)))
      (format #t "cgi_enabled: ~a~%"
              (serialize-boolean (assoc-ref website "cgiEnabled")))
      (format #t "cgi_file_extensions: ~a~%"
              (string-join (assoc-ref website "cgiFileExtensions")))
      (format #t "infected: ~a~%"
              (serialize-boolean (assoc-ref website "infected")))
      (format #t "writable: ~a~%"
              (serialize-boolean (assoc-ref website "writable")))
      (format #t "sendmail_allowed: ~a~%"
              (serialize-boolean (assoc-ref website "sendmailAllowed")))
      (format #t "ddos_protection: ~a~%"
              (serialize-boolean (assoc-ref website "ddosProtection")))
      (newline))

    (define (format-unix account)
      (let ((unix-account (assoc-ref account "unixAccount")))
        (format #t "quota: ~a/~a GB~%"
                (serialize-quota (assoc-ref unix-account "quotaUsed"))
                (serialize-quota (assoc-ref unix-account "quota")))
        (let ((server-id (assoc-ref unix-account "serverId")))
          (format #t "server_id: ~a~%" (assoc-ref unix-account "serverId"))
          (serialize-server-args
           (lambda (server)
             (format #t "server_name: ~a~%" (assoc-ref server "name")))
           (list server-id)))
        (format #t "name: ~a~%" (assoc-ref unix-account "name"))
        (format #t "home_dir: ~a~%" (assoc-ref unix-account "homeDir"))
        (newline)))

    (define (format-domain domain)
      (define (format-record record)
        (format #t "+ ~a ~a ~a ~a ~a\n"
                (assoc-ref record "name")
                (assoc-ref record "ttl")
                (assoc-ref record "rrClass")
                (assoc-ref record "rrType")
                (assoc-ref record "data")))

      (let ((name (assoc-ref domain "name")))
        (format #t "name: ~a~%" name)
        (unless resolve? (resolve-domain name)))

      (match (assoc-ref domain "dnsResourceRecords")
        ((record records ...)
         (format #t "records: ~a ~a ~a ~a ~a\n"
                 (assoc-ref record "name")
                 (assoc-ref record "ttl")
                 (assoc-ref record "rrClass")
                 (assoc-ref record "rrType")
                 (assoc-ref record "data"))
         (for-each format-record records))
        (_ '()))

      (newline))

    (case command
      ((database)
       (for-each (lambda (account)
                   (for-each (lambda (database)
                               (format #t "quota_used: ~a~%"
                                       (assoc-ref database "quotaUsed"))
                               (format #t "will_be_deleted: ~a~%"
                                       (serialize-boolean
                                        (assoc-ref database "willBeDeleted")))
                               (format #t "id: ~a~%"
                                       (assoc-ref database "id"))
                               (format #t "quota: ~a~%"
                                       (assoc-ref database "quota"))
                               (format #t "name: ~a~%"
                                       (assoc-ref database "name"))
                               (format #t "account_id: ~a~%"
                                       (assoc-ref database "accountId"))
                               (format #t "switched_on: ~a~%"
                                       (serialize-boolean
                                        (assoc-ref database "switchedOn")))
                               (format #t "type: ~a~%"
                                       (assoc-ref database "type"))
                               (format #t "type: ~a~%"
                                       (assoc-ref database "@type"))
                               (format #t "service_id: ~a~%"
                                       (assoc-ref database "serviceId"))
                               (format #t "writable: ~a~%"
                                       (serialize-boolean
                                        (assoc-ref database "writable")))
                               (format #t "locked: ~a~%"
                                       (serialize-boolean
                                        (assoc-ref database "locked")))
                               (newline))
                             (account-database->scm account)))
            args))

      ((database-user)
       (for-each (lambda (account)
                   (for-each (lambda (database)
                               (for-each (lambda (user)
                                           (format #t "will_be_deleted: ~a~%"
                                                   (serialize-boolean
                                                    (assoc-ref user "willBeDeleted")))
                                           (format #t "id: ~a~%"
                                                   (assoc-ref user "id"))
                                           (format #t "max_cpu_time_per_second: ~a~%"
                                                   (assoc-ref user "maxCpuTimePerSecond"))
                                           (format #t "name: ~a~%"
                                                   (assoc-ref user "name"))
                                           (format #t "account_id: ~a~%"
                                                   (assoc-ref user "accountId"))
                                           (format #t "switched_on: ~a~%"
                                                   (serialize-boolean
                                                    (assoc-ref user "switchedOn")))
                                           (format #t "type: ~a~%"
                                                   (assoc-ref user "type"))
                                           (format #t "type: ~a~%"
                                                   (assoc-ref user "@type"))
                                           (format #t "serviceId: ~a~%"
                                                   (assoc-ref user "serviceId"))
                                           (format #t "allowed_ip_addresses: ~a~%"
                                                   (assoc-ref user "allowedIPAddresses"))
                                           (format #t "password_hash: ~a~%"
                                                   (assoc-ref user "passwordHash"))
                                           (format #t "locked: ~a~%"
                                                   (serialize-boolean
                                                    (assoc-ref user "locked")))
                                           (newline))
                                         (assoc-ref database "databaseUsers")))
                             (account-database->scm account)))
                 args))

      ((dump)
       (serialize-args format-user)
       (serialize-args
        (lambda (user)
          (for-each format-service (assoc-ref user "services"))))
       (serialize-websites-args format-website)
       (serialize-website-args format-unix)
       (serialize-websites-args
        (lambda (user)
          (for-each format-domain (assoc-ref user "domains")))))

      ((history)
       (apply (resolve-subcommand "history") args))

      ((mailbox)
       (for-each (lambda (account)
                   (for-each (lambda (mailbox)
                               (format #t "quota_used: ~a~%"
                                       (assoc-ref mailbox "quotaUsed"))
                               (format #t "server_id: ~a~%"
                                       (assoc-ref mailbox "serverId"))
                               (format #t "will_be_deleted: ~a~%"
                                       (serialize-boolean
                                        (assoc-ref mailbox "willBeDeleted")))
                               (format #t "id: ~a~%"
                                       (assoc-ref mailbox "id"))
                               (format #t "mail_spool: ~a~%"
                                       (assoc-ref mailbox "mailSpool"))
                               (format #t "white_list: ~a~%"
                                       (assoc-ref mailbox "whiteList"))
                               (format #t "quota: ~a~%"
                                       (assoc-ref mailbox "quota"))
                               (format #t "name: ~a~%"
                                       (assoc-ref mailbox "name"))
                               (format #t "account_id: ~a~%"
                                       (assoc-ref mailbox "accountId"))
                               (format #t "switched_on: ~a~%"
                                       (serialize-boolean
                                        (assoc-ref mailbox "switchedOn")))
                               (format #t "is_aggregator: ~a~%"
                                       (assoc-ref mailbox "isAggregator"))
                               (format #t "type: ~a~%"
                                       (assoc-ref mailbox "@type"))
                               (format #t "spam_filter_mood: ~a~%"
                                       (assoc-ref mailbox "spamFilterMood"))
                               (format #t "writable: ~a~%"
                                       (serialize-boolean
                                        (assoc-ref mailbox "writable")))
                               (format #t "black_list: ~a~%"
                                       (assoc-ref mailbox "blackList"))
                               (format #t "domain_id: ~a~%"
                                       (assoc-ref mailbox "domainId"))
                               (format #t "password_hash: ~a~%"
                                       (assoc-ref mailbox "passwordHash"))
                               (format #t "comment: ~a~%"
                                       (assoc-ref mailbox "comment"))
                               (format #t "mail_from_allowed: ~a~%"
                                       (serialize-boolean
                                        (assoc-ref mailbox "mailFromAllowed")))
                               (format #t "redirect_addresses: ~a~%"
                                       (assoc-ref mailbox "redirectAddresses"))
                               (format #t "anti_spam_enabled: ~a~%"
                                       (serialize-boolean
                                        (assoc-ref mailbox "antiSpamEnabled")))
                               (format #t "spam_filter_action: ~a~%"
                                       (assoc-ref mailbox "spamFilterAction"))
                               (format #t "locked: ~a~%"
                                       (serialize-boolean
                                        (assoc-ref mailbox "locked")))
                               (format #t "uid: ~a~%"
                                       (assoc-ref mailbox "uid"))
                               (newline))
                             (account-mailbox->scm account)))
                 args))

      ((show)
       (serialize-args format-user))

      ((search)
       (apply (resolve-subcommand "search") args))

      ((service)
       (serialize-args
        (lambda (user)
          (for-each format-service (assoc-ref user "services")))))

      ((website)
       (serialize-websites-args format-website))

      ((unix)
       (serialize-website-args format-unix))

      ((open)
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
                                      (format #t "Open account: ~a~%" account)
                                      (mkdir-p account-profile)
                                      (system* "firefox" "--new-instance"
                                               "--profile" account-profile
                                               "--private-window"
                                               (string-append "https://hms.majordomo.ru/login"
                                                              "?bearer=" (assoc-ref records "access_token")
                                                              "&refresh=" (assoc-ref records "refresh_token")))))
                                   (_ #f))
                                 (assoc-ref json "params")))))
                 args))

      ((domain)
       (serialize-websites-args
        (lambda (user)
          (for-each format-domain (assoc-ref user "domains"))))))))

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
              (G_ "gms account: missing command name~%"))
      (format (current-error-port)
              (G_ "Try 'gms account --help' for more information.~%"))
      (exit 1))

    args))

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
