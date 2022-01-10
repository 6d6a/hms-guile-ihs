(define-module (ihs hms)
  #:use-module (json)
  #:use-module (web client)
  #:use-module (srfi srfi-11)
  #:use-module (rnrs bytevectors)
  #:export (api
            auth
            fetch-server
            search-account
            search-owner
            search-domain))

(define* (auth #:key (user (getenv "IHS_USER")) (pass (getenv "IHS_PASS")))
  (letrec-syntax ((option (syntax-rules ()
                            ((_ key value)
                             (if value
                                 (list (string-append key "=" value))
                                 '()))))
                  (key/value (syntax-rules ()
                               ((_ (key value) rest ...)
                                (append (option key value)
                                    (key/value rest ...)))
                               ((_)
                                '()))))
    (assoc-ref (let-values
                   (((response body)
                     (http-post "https://api.majordomo.ru/oauth/token"
                                #:headers `((content-type . (application/x-www-form-urlencoded)))
                                #:keep-alive? #t
                                #:body (string-join (key/value ("grant_type" "password")
                                                               ("client_id" "frontend_app")
                                                               ("client_secret" "frontend_app_secret")
                                                               ("username" user)
                                                               ("password" pass))
                                                    "&"))))
                 (json-string->scm (utf8->string body)))
               "access_token")))

(define (api account path)
  (let-values (((response body)
                (http-get (string-append "https://api.majordomo.ru/"
                                         account path)
                          #:headers `((content-type
                                       . (application/json))
                                      (Authorization
                                       . ,(format #f "Bearer ~a" (auth))))
                          #:keep-alive? #t)))
    (utf8->string body)))

(define (fetch-server)
  (let-values (((response body)
                (http-get (string-append "https://api.majordomo.ru/rc-staff\
/server")
                          #:headers `((content-type . (application/json))
                                      (Authorization . ,(format #f "Bearer ~a"
                                                                (auth))))
                          #:keep-alive? #t)))
    (utf8->string body)))

(define (search-account account)
  (let-values (((response body)
                (http-get (string-append "https://api.majordomo.ru/pm\
/accounts?accountId="
                                         account)
                          #:headers `((content-type
                                       . (application/json))
                                      (Authorization
                                       . ,(format #f "Bearer ~a" (auth))))
                          #:keep-alive? #t)))
    (utf8->string body)))

(define (search-owner owner)
  (let-values (((response body)
                (http-get (string-append "https://api.majordomo.ru/pm\
/account-owner/search?search="
                                         owner)
                          #:headers `((content-type
                                       . (application/json))
                                      (Authorization
                                       . ,(format #f "Bearer ~a" (auth))))
                          #:keep-alive? #t)))
    (utf8->string body)))

(define (search-domain domain)
  (let-values (((response body)
                (http-get (string-append "https://api.majordomo.ru/domain\
/filter?nameContains="
                                         domain)
                          #:headers `((content-type
                                       . (application/json))
                                      (Authorization
                                       . ,(format #f "Bearer ~a" (auth))))
                          #:keep-alive? #t)))
    (utf8->string body)))
