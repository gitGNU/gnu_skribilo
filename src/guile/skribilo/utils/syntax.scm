;;; syntax.scm  --  Syntactic candy for Skribilo modules.
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

(define-module (skribilo utils syntax)
  :use-module (skribilo reader)
  :use-module (system reader library)
  :use-module (system reader compat) ;; make sure `current-reader' exists
  :use-module (system reader confinement)
  :export (%skribe-reader %skribilo-module-reader)
  :export-syntax (unwind-protect unless when))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; A reader for the Skribe syntax, i.e. roughly R5RS Scheme plus DSSSL-style
;;; keywords and sk-exps (expressions introduced using a square bracket).
;;;
;;; Code:

(define %skribilo-module-reader
  ;; The syntax used to read Skribilo modules.
  (make-alternate-guile-reader '(colon-keywords
				 no-scsh-block-comments
				 srfi30-block-comments
				 srfi62-sexp-comments)
			       (lambda (chr port read)
				 (error "unexpected character in Skribilo module"
					chr))
			       'reader/record-positions))

(define %skribe-reader
  ;; The Skribe syntax reader.
  (make-reader 'skribe))


(define-macro (unwind-protect expr1 expr2)
  ;; This is no completely correct.
  `(dynamic-wind
       (lambda () #f)
       (lambda () ,expr1)
       (lambda () ,expr2)))

(define-macro (unless condition . exprs)
  `(if (not ,condition) (begin ,@exprs)))

(define-macro (when condition . exprs)
  `(if ,condition (begin ,@exprs)))

;;; arch-tag: 9a0e0638-64f0-480a-ab19-49e8bfcbcd9b

;;; syntax.scm ends here
