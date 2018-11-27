;;; Guile IHS --- IHS command-line interface.
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (ihs scripts)
  #:use-module (ihs ui)
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

            serialize-boolean
            serialize-field
            serialize-quota))

(define (match-pair car)
  ;; Return a procedure that matches a pair with CAR.
  (match-lambda
    ((head . tail)
     (and (eq? car head) tail))
    (_ #f)))

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
