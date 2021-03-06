(define-module (ihs billing2)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ihs config)
  #:export (fetch-vm))

(define (fetch-vm account)
  (let* ((port   (apply open-pipe* OPEN_READ %cvm (list account)))
         (output (read-string port)))
    (close-port port)
    (delete "" (string-split output #\newline))))
