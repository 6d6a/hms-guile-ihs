;;; Guile IHS --- IHS command-line interface.
;;; Copyright © 2018, 2019 Oleg Pykhalov <go.wigust@gmail.com>
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile IHS.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains Guix package for development version of
;; Guile IHS.  To build or install, run:
;;
;;   guix build --file=guix.scm
;;   guix package --install-from-file=guix.scm

;;; Code:

(use-modules (gnu packages autotools)
             (gnu packages gnupg)
             (gnu packages guile)
             (gnu packages package-management)
             (gnu packages pkg-config)
             (gnu packages tls)
             (guix build utils)
             (guix build-system gnu)
             (guix gexp)
             (guix git-download)
             (guix packages)
             (ice-9 popen)
             (ice-9 rdelim)
             ((guix licenses) #:prefix license:))

(define %source-dir (dirname (current-filename)))

(define (git-output . args)
  "Execute 'git ARGS ...' command and return its output without trailing
newspace."
  (with-directory-excursion %source-dir
    (let* ((port   (apply open-pipe* OPEN_READ "git" args))
           (output (read-string port)))
      (close-port port)
      (string-trim-right output #\newline))))

(define (current-commit)
  (git-output "log" "-n" "1" "--pretty=format:%H"))

(define guile-ihs
  (let ((commit (current-commit)))
    (package
      (name "guile-ihs")
      (version (string-append "0.0.1" "-" (string-take commit 7)))
      (source (local-file %source-dir
                          #:recursive? #t
                          #:select? (git-predicate %source-dir)))
      (build-system gnu-build-system)
      (home-page "https://majordomo.ru/")
      (arguments
       `(#:configure-flags
         (list
          "--sysconfdir=/etc"
          (string-append "--with-bash-completion-dir="
                         (assoc-ref %outputs "out")
                         "/etc/bash_completion.d"))
         #:modules
         ((guix build gnu-build-system)
          (guix build utils)
          (srfi srfi-26)
          (ice-9 popen)
          (ice-9 rdelim))
         #:phases
         (modify-phases %standard-phases
           (add-after 'install 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               ;; Make sure the 'guix' command finds GnuTLS,
               ;; Guile-JSON, and Guile-Git automatically.
               (let* ((out    (assoc-ref outputs "out"))
                      (guile  (assoc-ref inputs "guile"))
                      (gcrypt (assoc-ref inputs "guile-gcrypt"))
                      (gnutls (assoc-ref inputs "gnutls"))
                      (guix   (assoc-ref inputs "guix"))
                      (json   (assoc-ref inputs "guile-json"))
                      (deps   (list gcrypt gnutls guix json out))
                      (effective
                       (read-line
                        (open-pipe* OPEN_READ
                                    (string-append guile "/bin/guile")
                                    "-c" "(display (effective-version))")))
                      (path   (string-join
                               (map (cut string-append <>
                                         "/share/guile/site/"
                                         effective)
                                    deps)
                               ":"))
                      (gopath (string-join
                               (map (cut string-append <>
                                         "/lib/guile/" effective
                                         "/site-ccache")
                                    deps)
                               ":")))

                 (wrap-program (string-append out "/bin/ihs")
                   `("GUILE_LOAD_PATH" ":" prefix (,path))
                   `("GUILE_LOAD_COMPILED_PATH" ":" prefix (,gopath)))

                 #t)
               (let* ((guile  (assoc-ref inputs "guile"))
                      (effective
                       (read-line
                        (open-pipe* OPEN_READ
                                    (string-append guile "/bin/guile")
                                    "-c" "(display (effective-version))")))
                      (path (cut string-append <>
                                 "/share/guile/site/"
                                 effective
                                 "/ihs")))
                 (with-directory-excursion "ihs"
                   (copy-file "config.scm.in" "config.scm")
                   (substitute* "config.scm"
                     (("@PACKAGE_NAME@") ,name)
                     (("@PACKAGE_VERSION@") ,version)
                     (("@PACKAGE_URL@") ,home-page))
                   (install-file "config.scm"
                                 (path (assoc-ref outputs "out"))))
                 #t)))
           (add-before 'check 'set-environment
             (lambda _
               (setenv "HOME" (getcwd)))))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("gnutls" ,gnutls)
         ("guile" ,guile-2.2)
         ("guile-gcrypt" ,guile-gcrypt)
         ("guile-json" ,guile-json)
         ("guix" ,guix)))
      (synopsis "Guile interface to Majordomo API")
      (description
       "This package provides a Guile interface to Majordomo API.")
      (license license:gpl3+))))

guile-ihs

;;; guix.scm ends here
