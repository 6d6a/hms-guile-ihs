(define-module (gms scripts cerb)
  #:use-module ((guix ui) #:select (G_ leave))
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:export (gms-cerb))

(define info-source
  (or (getenv "CERB_HOST") "192.168.105.122"))

(define cookie
  (getenv "CERB_DEVBLOCKS"))

(define delay
  (or (getenv "CERB_DELAY") (number->string 2000)))

(define display
  (or (getenv "DISPLAY") ":1"))

(define (show-help)
  (display (G_ "Usage: gms account [OPTION ...] ACTION [ARG ...]
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
  (let* ((port (open-pipe* OPEN_READ "ssh"
                           info-source "--"
                           (string-append (string-join (list "cd" "src/hello-nightmarejs"))
                                          ";"
                                          (string-join (list (string-append "CERB_DELAY=" delay)
                                                             (string-append "CERB_DEVBLOCKS=" cookie)
                                                             (string-append "DISPLAY=" display)
                                                             "node" "./cerb.js")))))
         (output (read-string port)))
    (close-port port)
    output))


;;;
;;; Entry point.
;;;

(define (gms-cerb . args)
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
