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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile HMS.  If not, see <http://www.gnu.org/licenses/>.

(define-module (hms scripts website)
  #:use-module (hms ui)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (web client)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:export (hms-website))


;;;
;;; Entry point.
;;;

(define (hms-website . args)
  (define (serialize-field field)
    (if (string-prefix? "@" field)
        (string-drop field (string-length "@"))
        field))
  (define (serialize-boolean value)
    (if value "true" "false"))
  (define (serialize-quota value)
    (let ((size (number->string (/ (/ (/ value 1024.0) 1024.0) 1024.0))))
      (match (string-split size #\.)
        ((natural decimal)
         (string-append natural "."
                        (if (> (string-length decimal) 2)
                            (string-take decimal 2)
                            decimal))))))
  (for-each (lambda (account)
              (let-values (((response body)
                            (http-get (string-append "https://api.majordomo.ru/" account "/website")
                                      #:headers `((content-type . (application/json))
                                                  (Authorization . ,(format #f "Bearer ~a" (auth))))
                                      #:keep-alive? #t)))
                (for-each (lambda (json)
                            (format #t "name: ~a~%" (assoc-ref json "name"))
                            (format #t "document_root: ~a~%" (assoc-ref json "documentRoot"))
                            (format #t "auto_sub_domain: ~a~%" (serialize-boolean (assoc-ref json "autoSubDomain")))
                            (let ((unix-account (assoc-ref json "unixAccount")))
                              (format #t "quota: ~a/~a GB~%"
                                      (serialize-quota (assoc-ref unix-account "quotaUsed"))
                                      (serialize-quota (assoc-ref unix-account "quota")))
                              (format #t "server_id: ~a~%" (assoc-ref unix-account "serverId"))
                              (format #t "home_dir: ~a~%" (assoc-ref unix-account "homeDir")))
                            (format #t "index_file_list: ~a~%" (string-join (sort (assoc-ref json "indexFileList") string<)))
                            (format #t "static_file_extensions: ~a~%" (string-join (sort (assoc-ref json "staticFileExtensions") string<)))
                            (format #t "cgi_enabled: ~a~%" (serialize-boolean (assoc-ref json "cgiEnabled")))
                            (format #t "cgi_file_extensions: ~a~%" (string-join (assoc-ref json "cgiFileExtensions")))
                            (format #t "infected: ~a~%" (serialize-boolean (assoc-ref json "infected")))
                            (format #t "writable: ~a~%" (serialize-boolean (assoc-ref json "writable")))
                            (format #t "sendmail_allowed: ~a~%" (serialize-boolean (assoc-ref json "sendmailAllowed")))
                            (format #t "ddos_protection: ~a~%" (serialize-boolean (assoc-ref json "ddosProtection")))
                            (newline))
                          (map hash-table->alist (json-string->scm (utf8->string body))))))
            args))

