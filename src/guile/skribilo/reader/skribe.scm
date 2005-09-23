;;; skribe.scm  --  A reader for the Skribe syntax.
;;;
;;; Copyright 2005  Ludovic Courtès <ludovic.courtes@laas.fr>
;;;
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
;;; USA.

(define-module (skribilo reader skribe)
  :use-module (skribilo reader)
  :use-module (ice-9 optargs)

   ;; the Scheme reader composition framework
  :use-module ((system reader) #:renamer (symbol-prefix-proc 'r:))

  :export (reader-specification
           make-skribe-reader))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; A reader for the Skribe syntax, i.e. roughly R5RS Scheme plus DSSSL-style
;;; keywords and sk-exps (expressions introduced using a square bracket).
;;;
;;; Code:


(define* (make-skribe-reader #:optional (version "1.2d"))
  "Return a Skribe reader (a procedure) suitable for version @var{version} of
the Skribe syntax."
  (if (string> version "1.2d")
      (error "make-skribe-reader: unsupported version" version)
      *skribe-reader*))


(define (%make-skribe-reader)
  (let* ((dsssl-keyword-reader  ;; keywords à la `#!key'
          (r:make-token-reader #\!
                                (r:token-reader-procedure
                                 (r:standard-token-reader 'keyword))))
         (sharp-reader (r:make-reader (cons dsssl-keyword-reader
                                            (map r:standard-token-reader
                                                 '(character srfi-4
                                                   number+radix
                                                   boolean))))))
    (r:make-reader (cons (r:make-token-reader #\# sharp-reader)
                         (map r:standard-token-reader
                              `(whitespace
                                sexp string number
                                symbol-lower-case
                                symbol-upper-case
                                symbol-misc-chars
                                quote-quasiquote-unquote
                                semicolon-comment
                                keyword  ;; keywords à la `:key'
                                skribe-exp))))))

;; We actually cache an instance here.
(define *skribe-reader* (%make-skribe-reader))



;;; The reader specification.

(define-reader skribe "1.2d" make-skribe-reader)

;;; skribe.scm ends here
