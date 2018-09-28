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

(define-module (gms utils)
  #:use-module ((guix build utils) #:select (mkdir-p))
  #:use-module (srfi srfi-26)
  #:export (config-directory
            cache-directory))

;; From (guix utils).
(define* (xdg-directory variable suffix #:key (ensure? #t))
  "Return the name of the XDG directory that matches VARIABLE and SUFFIX,
after making sure that it exists if ENSURE? is true.  VARIABLE is an
environment variable name like \"XDG_CONFIG_HOME\"; SUFFIX is a suffix like
\"/.config\".  Honor the XDG specs,
<http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html>."
  (let ((dir (and=> (or (getenv variable)
                        (and=> (or (getenv "HOME")
                                   (passwd:dir (getpwuid (getuid))))
                               (cut string-append <> suffix)))
                    (cut string-append <> "/gms"))))
    (when ensure?
      (mkdir-p dir))
    dir))

(define config-directory
  (cut xdg-directory "XDG_CONFIG_HOME" "/.config" <...>))

(define cache-directory
  (cut xdg-directory "XDG_CACHE_HOME" "/.cache" <...>))
