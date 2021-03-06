;;; Guile IHS --- IHS command-line interface.
;;; Copyright © 2018, 2019, 2020, 2022 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (ihs scripts web)
  #:use-module ((guix scripts) #:select (parse-command-line))
  #:use-module ((guix ui) #:select (G_ leave))
  #:use-module (guix colors)
  #:use-module (guix build utils)
  #:use-module (guix import utils)
  #:use-module (guix records)
  #:use-module (ihs hms)
  #:use-module (ihs scripts)
  #:use-module (ihs ui)
  #:use-module (ihs utils)
  #:use-module (ice-9 pretty-print)
  #:use-module (json)
  #:use-module (guix memoization)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (web client)
  #:export (account->scm
            ihs-web
            serialize-account

            account-websites
            account-websites->scm

            update-cache

            colorize))

(define (quote-string str)
  (string-append "\"" str "\""))

(define-record-type* <website>
  website make-website
  website?
  (name                   website-name                   ;string
                          (default ""))
  (document-root          website-document-root          ;string
                          (default ""))
  (auto-sub-domain?       website-auto-sub-domain        ;boolean
                          (default ""))
  (index-file-list        website-index-file-list        ;string
                          (default ""))
  (static-file-extensions website-static-file-extensions ;string
                          (default ""))
  (cgi-enabled?           website-cgi-enabled            ;boolean
                          (default ""))
  (cgi-file-extensions    website-cgi-file-extensions    ;string
                          (default ""))
  (infected?              website-infected               ;infected?
                          (default ""))
  (ddos-protection?       website-ddos-protection        ;boolean
                          (default ""))
  (quota                  website-quota                  ;string
                          (default "")))

(define (show-help)
  (display (G_ "Usage: ihs account [OPTION ...] ACTION [ARG ...]
Fetch data about user.\n"))
  (newline)
  (display (G_ "The valid values for ACTION are:\n"))
  (newline)
  (display (G_ "
  -f, --format=FORMAT    display information in the given FORMAT"))
  (display (G_ "\
   billing               open web billing to edit account settings\n"))
  (display (G_ "\
   block-ip              block IP-address via NGINX
                         ihs web block-ip web33:95.55.190.61\n"))
  (display (G_ "\
   block                 block all websites DDoS protection on ACCOUNTs\n"))
  (display (G_ "\
   unblock               unblock all websites DDoS protection on ACCOUNTs\n"))
  (display (G_ "\
   database              show database on account\n"))
  (display (G_ "\
   database-user         show database users on account\n"))
  (display (G_ "\
   domain                show domains on account\n"))
  (display (G_ "\
   dump                  dump all available information\n"))
  (display (G_ "\
   ftp                   show ftp users on account\n"))
  (display (G_ "\
   mailbox               show mailboxes on account\n"))
  (display (G_ "\
   owner                 show account owner\n"))
  (display (G_ "\
   pull                  pull information about servers\n"))
  (display (G_ "\
   server-show           show server\n"))
  (display (G_ "\
   server-socket         show server's socket\n"))
  (display (G_ "\
   server-storage        show server's storages\n"))
  (display (G_ "\
   server-service        show server's services\n"))
  (display (G_ "\
   open                  open billing\n"))
  (display (G_ "\
   show                  show user\n"))
  (display (G_ "\
   toggle                toggle account state\n"))
  (display (G_ "\
   unix                  show unix account\n"))
  (display (G_ "\
   website               show websites on account\n"))
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
  (list (option '(#\f "format") #t #f
                (lambda (opt name arg result)
                  (unless (member arg '("json" "recutils"))
                    (leave (G_ "~a: unsupported output format~%") arg))
                  (alist-cons 'format (string->symbol arg) result)))
        (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\n "no-resolve") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'do-not-resolve? #t result)))))

(define %default-options
  ;; Alist of default option values.
  '((format . recutils)))

(define isatty?*
  (mlambdaq (port)
    (isatty? port)))

(define (color-output? port)
  "Return true if we should write colored output to PORT."
  (and (not (getenv "INSIDE_EMACS"))
       (not (getenv "NO_COLOR"))
       (isatty?* port)))

(define* (colorize value
                   #:optional (port (current-output-port))
                   #:key (colorize? (color-output? port)) (good? #t))
  (define good
    (if colorize?
        (cut colorize-string <> (color GREEN BOLD))
        identity))

  (define failure
    (if colorize?
        (cut colorize-string <> (color RED BOLD))
        identity))

  (if (string=? value "true")
      (good value)
      (failure value)))



;;;
;;; Entry point.
;;;

(define-record-type* <account-action>
  account-action
  make-account-action
  account-action?
  ;; (account-id   account-action-account-id   ;string
  ;;               (default #f))
  (action-id    account-action-action-id
                (default #f))
  (operation-id account-action-operation-id ;string
                (default #f))
  ;; (object-ref   account-action-object-ref
  ;;               (default #f))
  (parameters   account-action-parameters   ;list of <account-action-params>
                (default '()))
  )

(define-record-type* <account-action-parameter>
  account-action-parameter
  make-account-action-parameter
  account-action-parameter?
  (ddos-protection account-action-parameter-ddos-protection ;boolean
                   (default #f))
  (resource-id     account-action-parameter-resource-id     ;string
                   (default #f))
  (success         account-action-parameter-success         ;boolean
                   (default #f)))

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
           (split-octet #:str (let* ((ip (number->string (vector-ref (addrinfo:addr (first domain))
                                                                     1)
                                                         radix))
                                     (length (string-length ip)))
                                (if (< length 32)
                                    (string-append (list->string (make-list (- 32 length) #\0))
                                                   ip)
                                    ip)))))))

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
          ((billing block-ip database database-user domain dump ftp history
            mailbox owner search service show open toggle pull unix website
            block unblock server-show server-socket server-storage
            server-service)
           (alist-cons 'action action result))
          (else (leave (G_ "~a: unknown action~%") action))))))

(define (fetch-account account)
  (api account "/account"))

(define (toggle-account account)
  (http-post (string-append "https://api.majordomo.ru/"
                            account "/account/toggle_state")
             #:headers `((content-type . (application/json))
                         (Authorization . ,(format #f "Bearer ~a" (auth))))
             #:body "{}"
             #:keep-alive? #t))

(define (account->scm account)
  (array->list (json-string->scm (fetch-account account))))

(define* (website-ddos account website #:key block?)
  "Block WEBSITE by id in ACCOUNT."
  (let* ((port
          (open-pipe (string-join
                      (list
                       "curl" "-s"
                       (quote-string (string-append "https://api.majordomo.ru/pm/"
                                                    account"/website/" website))
                       "-X" "PATCH"
                       "-H" (quote-string (format #f "Accept: ~a" "application/json"))
                       "-H" (quote-string (format #f "content-type: ~a" "application/json"))
                       "-H" (quote-string (format #f "authorization: ~a"
                                                  (format #f "Bearer ~a" (auth))))
                       "-H" (quote-string (format #f "Connection: ~a" "keep-alive"))
                       "--data"
                       "'{\"operationIdentity\":null,\"params\":{\"ddosProtection\":"
                       (if block? "true" "false")
                       "}}'"))
                     OPEN_READ))
         (output (read-string port))
         (action-record
          (alist->record (array->list
                          (json-string->scm (string-trim-right output
                                                               #\newline)))
                         make-account-action
                         '("actionIdentity" "operationIdentity" "params")))
         (action
          (account-action
           (action-id (account-action-action-id action-record))
           (operation-id (account-action-operation-id action-record))
           (parameters (alist->record (account-action-parameters action-record)
                                      make-account-action-parameter
                                      '("ddosProtection" "resourceId" "success"))))))
    (close-port port)
    (match action
      (($ <account-action> action-id operation-id parameters)
       (format #t "action_id: ~a~%" action-id)
       (format #t "operation_id: ~a~%" operation-id)
       (match parameters
         (($ <account-action-parameter> ddos-protection resource-id success)
          (format #t "ddos_protection: ~a~%" (colorize
                                              (serialize-boolean ddos-protection)
                                              #:good? #f))
          (format #t "resource_id: ~a~%" resource-id)
          (format #t "success: ~a~%" (colorize (serialize-boolean success)))))
       (newline)))))

(define (account->scm account)
  (fetch-account account))

(define (serialize-account account)
  (if (string-prefix? "ac" account)
      (string-take-right account (- (string-length account)
                                    (string-length "ac_")))
      account))

(define account-websites
  (cut api <> "/website"))

(define (account-websites->scm account)
  (account-websites account))

(define account-ftp
  (cut api <> "/ftp-user"))

(define (account-ftp->scm account)
  (array->list (json-string->scm (account-ftp account))))

(define account-mailbox
  (cut api <> "/mailbox"))

(define (account-mailbox->scm account)
  (array->list (json-string->scm (account-mailbox account))))

(define account-database
  (cut api <> "/database"))

(define (account-database->scm account)
  (array->list (json-string->scm (account-database account))))

(define account-owner
  (cut api <> "/owner"))

(define (account-owner->scm account)
  (account-owner account))

(define (resolve-subcommand name)
  (let ((module (resolve-interface
                 `(ihs scripts web ,(string->symbol name))))
        (proc (string->symbol (string-append "ihs-web-" name))))
    (module-ref module proc)))

(define %cache-file
  (string-append (cache-directory) "/servers.scm"))

(define (server->scm)
  (array->list (json-string->scm (fetch-server))))

(define (update-cache)
  (with-output-to-file %cache-file
    (lambda ()
      (pretty-print (server->scm)))))

(define (find-server server)
  (find (lambda (entry)
          (or (string=? (assoc-ref entry "name") server)
              (string=? (assoc-ref entry "id") server)))
        (with-input-from-file %cache-file read)))

(define (process-command command args opts fmt)
  "Process COMMAND, one of the 'ihs server' sub-commands.  ARGS is its
argument list and OPTS is the option alist."
  (let ((resolve? (assoc-ref opts 'do-not-resolve?)))
    (define (serialize-args procedure)
      (for-each (lambda (account)
                  (let ((json (account->scm account)))
                    (if (eq? fmt 'json)
                        (display json)
                        (procedure
                         (json-string->scm json)))))
                args))

    (define (serialize-website-args procedure)
      (for-each (lambda (account)
                  (let ((json (account-websites->scm account)))
                    (if (eq? fmt 'json)
                        (display json)
                        (procedure
                         (first
                          (array->list
                               (json-string->scm json)))))))
                args))

    (define (serialize-websites-args procedure)
      (for-each (lambda (account)
                  (let ((json (account-websites->scm account)))
                    (if (eq? fmt 'json)
                        (display json)
                        (for-each procedure
                                  (array->list
                                       (json-string->scm json))))))
                args))

    (define (format-user user)
      (format #t "name: ~a~%"
              (assoc-ref user "name"))
      (format #t "active: ~a~%"
              (colorize (serialize-boolean (assoc-ref user "active"))))
      (format #t "automatic_billing_sending: ~a~%"
              (colorize (serialize-boolean (assoc-ref user "autoBillSending"))))
      (format #t "notify_days: ~a~%"
              (colorize (serialize-boolean (assoc-ref user "notifyDays"))))
      (format #t "credit: ~a~%"
              (colorize (serialize-boolean (assoc-ref user "credit"))))
      (newline))

    (define (format-service service)
      (format #t "name: ~a~%"
              (assoc-ref service "name"))
      (format #t "cost: ~a rub~%"
              (assoc-ref service "cost"))
      (format #t "enabled: ~a~%"
              (colorize (serialize-boolean (assoc-ref service "enabled"))))
      (format #t "last_billed: ~a~%"
              (assoc-ref service "lastBilled"))
      (newline))

    (define (format-website website)
      (format #t "name: ~a~%"
              (assoc-ref website "name"))
      (format #t "id: ~a~%"
              (assoc-ref website "id"))
      (format #t "document_root: ~a~%"
              (assoc-ref website "documentRoot"))
      (format #t "auto_sub_domain: ~a~%"
              (colorize (serialize-boolean (assoc-ref website "autoSubDomain"))))
      (format #t "index_file_list: ~a~%"
              (string-join (sort (array->list (assoc-ref website "indexFileList"))
                                 string<)))
      (format #t "static_file_extensions: ~a~%"
              (string-join (sort (array->list (assoc-ref website "staticFileExtensions"))
                                 string<)))
      (format #t "cgi_enabled: ~a~%"
              (colorize (serialize-boolean (assoc-ref website "cgiEnabled"))
                        #:good? #f))
      (format #t "cgi_file_extensions: ~a~%"
              (string-join (array->list (assoc-ref website "cgiFileExtensions"))))
      (format #t "infected: ~a~%"
              (colorize (serialize-boolean (assoc-ref website "infected"))))
      (format #t "ddos_protection: ~a~%"
              (colorize (serialize-boolean (assoc-ref website "ddosProtection"))))
      (newline))

    (define (format-unix account)
      (let ((unix-account (assoc-ref account "unixAccount")))
        (format #t "quota: ~a/~a GB~%"
                (serialize-quota (assoc-ref unix-account "quotaUsed"))
                (serialize-quota (assoc-ref unix-account "quota")))
        (serialize-server unix-account)
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

      (match (array->list (assoc-ref domain "dnsResourceRecords"))
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

    (define (serialize-server server)
      (let ((server-id (assoc-ref server "serverId")))
        (format #t "server_id: ~a~%" server-id)
        (format #t "server_name: ~a~%"
                (assoc-ref (find-server server-id) "name"))))

    (case command
      ((billing)
       (for-each (lambda (account)
                   (format #t "Open account billing: ~a~%" account)
                   (system* "firefox"
                            (string-append "https://hms-billing.majordomo.ru/account/"
                                           (let ((prefix "AC_"))
                                             (if (string-prefix? prefix account)
                                                 (string-drop account (string-length prefix))
                                                 account)))))
                 args))
      ((database)
       (for-each (lambda (account)
                   (for-each (lambda (database)
                               (format #t "quota_used: ~a~%"
                                       (assoc-ref database "quotaUsed"))
                               (format #t "will_be_deleted: ~a~%"
                                       (colorize
                                        (serialize-boolean
                                         (assoc-ref database "willBeDeleted"))))
                               (format #t "id: ~a~%"
                                       (assoc-ref database "id"))
                               (format #t "quota: ~a~%"
                                       (assoc-ref database "quota"))
                               (format #t "name: ~a~%"
                                       (assoc-ref database "name"))
                               (format #t "account_id: ~a~%"
                                       (assoc-ref database "accountId"))
                               (format #t "switched_on: ~a~%"
                                       (colorize
                                        (serialize-boolean
                                         (assoc-ref database "switchedOn"))))
                               (format #t "type: ~a~%"
                                       (assoc-ref database "type"))
                               (format #t "type: ~a~%"
                                       (assoc-ref database "@type"))
                               (format #t "service_id: ~a~%"
                                       (assoc-ref database "serviceId"))
                               (format #t "writable: ~a~%"
                                       (colorize
                                        (serialize-boolean
                                         (assoc-ref database "writable"))))
                               (format #t "locked: ~a~%"
                                       (colorize
                                        (serialize-boolean
                                         (assoc-ref database "locked"))))
                               (newline))
                             (account-database->scm account)))
                 args))

      ((database-user)
       (for-each (lambda (account)
                   (for-each (lambda (database)
                               (for-each (lambda (user)
                                           (format #t "will_be_deleted: ~a~%"
                                                   (colorize
                                                    (serialize-boolean
                                                     (assoc-ref user "willBeDeleted"))))
                                           (format #t "id: ~a~%"
                                                   (assoc-ref user "id"))
                                           (format #t "max_cpu_time_per_second: ~a~%"
                                                   (assoc-ref user "maxCpuTimePerSecond"))
                                           (format #t "name: ~a~%"
                                                   (assoc-ref user "name"))
                                           (format #t "account_id: ~a~%"
                                                   (assoc-ref user "accountId"))
                                           (format #t "switched_on: ~a~%"
                                                   (colorize
                                                    (serialize-boolean
                                                     (assoc-ref user "switchedOn"))))
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
                                                   (colorize
                                                    (serialize-boolean
                                                     (assoc-ref user "locked"))))
                                           (newline))
                                         (assoc-ref database "databaseUsers")))
                             (account-database->scm account)))
                 args))

      ((dump)
       (serialize-args format-user)
       (serialize-args
        (lambda (user)
          (for-each format-service (array->list (assoc-ref user "services")))))
       (serialize-websites-args format-website)
       (serialize-website-args format-unix)
       (serialize-websites-args
        (lambda (user)
          (for-each format-domain (array->list (assoc-ref user "domains"))))))

      ((ftp)
       (for-each (lambda (account)
                   (for-each (lambda (user)
                               (format #t "will_be_deleted: ~a~%"
                                       (colorize
                                        (serialize-boolean
                                         (assoc-ref user "willBeDeleted"))))
                               (format #t "id: ~a~%"
                                       (assoc-ref user "id"))
                               (format #t "name: ~a~%"
                                       (assoc-ref user "name"))
                               (format #t "account_id: ~a~%"
                                       (assoc-ref user "accountId"))
                               (format #t "switched_on: ~a~%"
                                       (colorize
                                        (serialize-boolean
                                         (assoc-ref user "switchedOn"))))
                               (format #t "home_dir: ~a~%"
                                       (assoc-ref user "homeDir"))
                               (format #t "type: ~a~%"
                                       (serialize-boolean
                                        (assoc-ref user "type")))
                               (format #t "allow_web_ftp: ~a~%"
                                       (colorize
                                        (serialize-boolean
                                         (assoc-ref user "allowWebFtp"))))
                               (format #t "allowed_ip_addresses: ~a~%"
                                       (assoc-ref user "allowedIPAddresses"))
                               (format #t "password_hash: ~a~%"
                                       (assoc-ref user "passwordHash"))
                               (format #t "locked: ~a~%"
                                       (colorize
                                        (serialize-boolean
                                         (assoc-ref user "locked"))))
                               (newline))
                             (account-ftp->scm account)))
                 args))

      ((history)
       (apply (resolve-subcommand "history") args))

      ((mailbox)
       (for-each (lambda (account)
                   (for-each (lambda (mailbox)
                               (format #t "type: ~a~%"
                                       (assoc-ref mailbox "@type"))
                               (format #t "name: ~a~%"
                                       (assoc-ref mailbox "name"))
                               (format #t "id: ~a~%"
                                       (assoc-ref mailbox "id"))
                               (serialize-server mailbox)
                               (format #t "mail_spool: ~a~%"
                                       (assoc-ref mailbox "mailSpool"))
                               (format #t "switched_on: ~a~%"
                                       (colorize
                                        (serialize-boolean
                                         (assoc-ref mailbox "switchedOn"))))
                               (format #t "writable: ~a~%"
                                       (colorize
                                        (serialize-boolean
                                         (assoc-ref mailbox "writable"))))
                               (format #t "locked: ~a~%"
                                       (colorize
                                        (serialize-boolean
                                         (assoc-ref mailbox "locked"))))
                               (format #t "quota_used: ~a~%"
                                       (assoc-ref mailbox "quotaUsed"))
                               (format #t "account_id: ~a~%"
                                       (assoc-ref mailbox "accountId"))
                               (format #t "anti_spam_enabled: ~a~%"
                                       (colorize
                                        (serialize-boolean
                                         (assoc-ref mailbox "antiSpamEnabled"))))
                               (format #t "spam_filter_mood: ~a~%"
                                       (assoc-ref mailbox "spamFilterMood"))
                               (format #t "will_be_deleted: ~a~%"
                                       (colorize
                                        (serialize-boolean
                                         (assoc-ref mailbox "willBeDeleted"))))
                               (format #t "white_list: ~a~%"
                                       (assoc-ref mailbox "whiteList"))
                               (format #t "quota: ~a~%"
                                       (assoc-ref mailbox "quota"))
                               (format #t "is_aggregator: ~a~%"
                                       (assoc-ref mailbox "isAggregator"))
                               (format #t "black_list: ~a~%"
                                       (assoc-ref mailbox "blackList"))
                               (format #t "domain_id: ~a~%"
                                       (assoc-ref mailbox "domainId"))
                               (format #t "password_hash: ~a~%"
                                       (assoc-ref mailbox "passwordHash"))
                               (format #t "comment: ~a~%"
                                       (assoc-ref mailbox "comment"))
                               (format #t "mail_from_allowed: ~a~%"
                                       (colorize
                                        (serialize-boolean
                                         (assoc-ref mailbox "mailFromAllowed"))))
                               (format #t "redirect_addresses: ~a~%"
                                       (string-join
                                        (assoc-ref mailbox "redirectAddresses")))
                               (format #t "spam_filter_action: ~a~%"
                                       (assoc-ref mailbox "spamFilterAction"))
                               (format #t "uid: ~a~%"
                                       (assoc-ref mailbox "uid"))
                               (newline))
                             (account-mailbox->scm account)))
                 args))

      ((owner)
       (for-each (lambda (account)
                   (let ((json (account-owner->scm account)))
                     (if (eq? fmt 'json)
                         (display json)
                         (let ((owner (array->list (json-string->scm json))))
                           (format #t "account: ~a~%" (assoc-ref owner "personalAccountId"))
                           (match (assoc-ref owner "contactInfo")
                             ((_ _ _ _ _ _ (_ emails ...))
                              (format #t "emails: ~a~%" (string-join emails ", "))))))))
                 args))

      ((show)
       (serialize-args format-user))

      ((search)
       (apply (resolve-subcommand "search") args))

      ((service)
       (serialize-args
        (lambda (user)
          (for-each format-service (array->list (assoc-ref user "services"))))))

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
                     (let ((json (json-string->scm (utf8->string body))))
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

      ((toggle)
       (for-each (lambda (account)
                   (format #t "Toggle account state: ~a~%" account)
                   (toggle-account account))
                 args))

      ((block)
       (for-each (lambda (account)
                   (for-each (lambda (website)
                               (let ((website (assoc-ref website "id")))
                                 (website-ddos account website
                                               #:block? #t)))
                             (array->list (json-string->scm (account-websites->scm account)))))
                 args))

      ((unblock)
       (for-each (lambda (account)
                   (for-each (lambda (website)
                               (let ((website-id (assoc-ref website "id"))
                                     (website-name (assoc-ref website "name")))
                                 ;; (format #t "Unblock: ~a ~a~%" website-name website-id)
                                 (website-ddos account website-id
                                               #:block? #f)))
                             (array->list (json-string->scm (account-websites->scm account)))))
                 args))

      ((block-ip)
       (for-each (lambda (rule)
                   (match (string-split rule #\:)
                     ((server ip)
                      (let ((server (if (string-suffix? "s" server)
                                        (string-drop-right (string-length "1")
                                                           server)
                                        server)))
                        (format #t "Block ~a on ~a." ip server)
                        (system* "curl" "-i" "-XPUT"
                                 (string-append server "/ip-filter/" ip
                                                "?ttl=7200&action=setCookie"))))))
                 args))

      ((domain)
       (serialize-websites-args
        (lambda (user)
          (for-each format-domain (array->list (assoc-ref user "domains"))))))

      ((pull)
       (update-cache))

      ((server-service)
       (for-each (lambda (server)
                   (let ((server (find-server server)))
                     (match (assoc-ref server "services")
                       (()
                        (format #t "No services on ~s server.~%" (assoc-ref server "name")))
                       (services
                        (for-each (lambda (service)
                                    (format #t "id: ~a~%" (assoc-ref service "id"))
                                    (format #t "name: ~a~%" (assoc-ref service "name"))
                                    (newline))
                                  (array->list services))))))
                 args))
      ((server-storage)
       (for-each (lambda (server)
                   (let ((server (find-server server)))
                     (match (assoc-ref server "storages")
                       (()
                        (format #t "No storages on ~s server.~%" (assoc-ref server "name")))
                       (storages
                        (for-each (lambda (storage)
                                    (format #t "id: ~a~%"
                                            (assoc-ref storage "id"))
                                    (format #t "name: ~a~%"
                                            (assoc-ref storage "name"))
                                    (format #t "online: ~a~%"
                                            (colorize (serialize-boolean (assoc-ref storage "switchedOn"))))
                                    ;; TODO: Check capacity size.
                                    (format #t "capacity: ~a/~a GB~%"
                                            (serialize-quota (assoc-ref storage "capacityUsed"))
                                            (serialize-quota (assoc-ref storage "capacity")))
                                    (newline))
                                  (array->list storages))))))
                 args))
      ((server-socket)
       (for-each (lambda (server)
                   (let ((server (find-server server)))
                     (match (assoc-ref server "services")
                       (()
                        (format #t "No services on ~s server.~%" (assoc-ref server "name")))
                       (services
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
                                                        (colorize
                                                         (serialize-boolean
                                                          (assoc-ref socket "switchedOn"))))
                                                (newline))
                                              (array->list (assoc-ref service "serviceSockets"))))
                                  (array->list services))))))
                 args))
      ((server-show)
       (for-each (lambda (server)
                   (let ((server (find-server server)))
                     (format #t "id: ~a~%" (assoc-ref server "id"))
                     (format #t "name: ~a~%" (assoc-ref server "name"))
                     (format #t "online: ~a~%"
                             (colorize (serialize-boolean (assoc-ref server "switchedOn"))))
                     (newline)))
        args)))))

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
              (G_ "ihs web: missing command name~%"))
      (format (current-error-port)
              (G_ "Try 'ihs web --help' for more information.~%"))
      (exit 1))

    args))

(define (ihs-web . args)
  ;; TODO: with-error-handling
  (let* ((opts (parse-command-line args %options
                                   (list %default-options)
                                   #:argument-handler
                                   parse-sub-command))
         (args (map (compose serialize-account string-downcase)
                    (option-arguments opts)))
         (format  (assq-ref opts 'format))
         (command (assoc-ref opts 'action)))
    (process-command command args opts format)))
