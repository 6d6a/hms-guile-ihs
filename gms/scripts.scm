(define-module (gms scripts)
  #:use-module (gms ui)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (web client)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (guix scripts)
  #:use-module ((guix ui)  #:select (leave G_))
  #:export (match-pair
            option-arguments

            serialize-boolean
            serialize-field
            serialize-quota))

(define (match-pair car)
  ;; Return a procedure that matches a pair with CAR.
  (match-lambda
    ((head . tail)
     (and (eq? car head) tail))
    (_ #f)))

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
              (G_ "guix system: missing command name~%"))
      (format (current-error-port)
              (G_ "Try 'guix system --help' for more information.~%"))
      (exit 1))

    args))

(define (serialize-boolean value)
  (if value "true" "false"))

(define (serialize-field field)
  (if (string-prefix? "@" field)
      (string-drop field (string-length "@"))
      field))

(define (serialize-quota value)
  (let ((size (number->string (/ (/ (/ value 1024.0) 1024.0) 1024.0))))
    (match (string-split size #\.)
      ((natural decimal)
       (string-append natural "."
                      (if (> (string-length decimal) 2)
                          (string-take decimal 2)
                          decimal))))))
