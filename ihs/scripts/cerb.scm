(define-module (ihs scripts cerb)
  #:use-module ((guix ui) #:select (G_ leave))
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (web client)
  #:use-module (web response)
  #:export (ihs-cerb))

(define info-source
  (or (getenv "CERB_HOST") "192.168.105.122"))

(define cookie
  (getenv "CERB_DEVBLOCKS"))

(define delay
  (or (getenv "CERB_DELAY") (number->string 2000)))

(define display
  (or (getenv "DISPLAY") ":1"))

(define (show-help)
  (display (G_ "Usage: ihs account [OPTION ...] ACTION [ARG ...]
Fetch data about user.\n"))
  (newline)
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

;; Origin <https://stackoverflow.com/a/25140048>.
(define (map-n n fn l . lists)
  (if (any (lambda(l)(< (length l) n)) (cons l lists))
      '()
      (cons (apply fn (append-map (lambda(l)(take l n)) (cons l lists)))
            (apply map-n n fn (map (lambda(l)(drop l n)) (cons l lists))))))

(define (split-by n l)
  (map-n n list l))

(define (fetch-info)
  (let* ((port (open-pipe (string-append (getenv "HOME")
                                         "/bin/cerb")
                          OPEN_READ))
         (output (read-string port)))
    (close-port port)
    output))



(define %cerb-uri "http://cerberus.intr")

(define %mail (getenv "CERB_MAIL"))

(define %password (getenv "CERB_PASS"))

(define (post uri devblocks)
  (http-post (string-append %cerb-uri uri)
             #:headers `((Cookie . ,(string-append "Devblocks=" devblocks))
                         (content-type . (application/x-www-form-urlencoded)))
             #:body (string-append "ext=password&email=" %mail
                                   "&password=" %password)
             #:keep-alive? #t))


;;;
;;; Authentication
;;;

(define (get-cookie)
  (let-values (((response body)
                (http-get (string-append %cerb-uri "/index.php/login")
                          #:keep-alive? #t
                          #:headers `((Connection . "keep-alive")))))
    (match (assq 'set-cookie (response-headers response))
      ((key . value)
       (match (string-split value #\;)
         ((devblocks _ ...)
          (match (string-split devblocks #\=)
            ((key value)
             value))))))))

(define (auth devblocks)
  (for-each (lambda (uri)
              (post uri devblocks))
            '("/index.php/login/authenticate"
              "/index.php/login/authenticated"))
  (let-values (((response body)
                (post "/index.php/profiles/worker/me" devblocks)))
    (let* ((head "_csrf_token\" content=\"")
           (tail "\">")
           (head-str (string-drop body (string-contains body head))))
      (string-drop (string-take head-str
                                (string-contains head-str tail))
                   (string-length head)))))


;;;
;;; Tickes
;;;

#;(define (ticket-id devblocks mask)
  "(ticket-id \"BY-25659-491\")"
  (let-values (((response body)
                (http-post (string-append %cerb-uri "/index.php/profiles/ticket/" mask)
                           #:headers `((Cookie . ,(string-append "Devblocks=" devblocks))))))
    (let ((head (string-drop body (string-contains body "ticket_id"))))
      (match (string-split (string-take head (string-contains head "'"))
                           #\=)
        ((_ id) id)))))

#;(define (get-ticket mask)
  (let-values (((response body)
                (http-post (string-append %cerb-uri
                                          "/ajax.php\
?c=display\
&a=showConversation\
&point=cerberusweb.profiles.ticket\
&ticket_id=" (ticket-id mask)
"&expand_all=1&_csrf_token=" auth)
                           #:headers `((Cookie . ,(string-append "Devblocks=" devblocks))))))))

(define (cookie-display args)
  (let ((c (cookie)))
    (auth c)
    (display c)))



;;;
;;; Entry point.
;;;

(define (ihs-cerb . args)
  (define* (format-meta #:key from time to box)
    (format #t "time: ~a~%" time)
    (format #t "from: ~a~%" from)
    (format #t "to: ~a~%" to)
    (format #t "box: ~a~%" box))
  (define* (format-title #:key title workers url)
    (format #t "title: ~a~%" title)
    (format #t "workers: ~a~%" workers)
    (format #t "url: ~a~%" url))
  (for-each (match-lambda
              ((title meta)
               (match (string-split (string-trim-right title #\tab) #\tab)
                 ((workers title url)
                  (format-title #:title title
                                #:workers workers
                                #:url url))
                 ((workers _ title url)
                  (format-title #:title title
                                #:workers workers
                                #:url url)))
               (match (string-split (string-trim-right meta #\tab) #\tab)
                 ((from time to box)
                  (format-meta #:from from
                               #:time time
                               #:to to
                               #:box box))
                 ((from time to box quality)
                  (format-meta #:from from
                               #:time time
                               #:to to
                               #:box box)))
               (newline)))
            (split-by 2 (string-split (fetch-info) #\newline))))
