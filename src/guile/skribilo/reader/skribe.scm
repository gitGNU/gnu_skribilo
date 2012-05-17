;;; skribe.scm  --  A reader for the Skribe syntax.
;;;
;;; Copyright 2005, 2006 Ludovic Courtès <ludovic.courtes@laas.fr>
;;;
;;;
;;; This file is part of Skribilo.
;;;
;;; Skribilo is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Skribilo is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Skribilo.  If not, see <http://www.gnu.org/licenses/>.

(define-module (skribilo reader skribe)
  :use-module (skribilo reader)
  :use-module (ice-9 optargs)
  :use-module (srfi srfi-1)

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

;;; Note: We need guile-reader 0.2 at least.

(define* (make-skribe-reader #:optional (version "1.2d"))
  "Return a Skribe reader (a procedure) suitable for version @var{version} of
the Skribe syntax."
  (if (string> version "1.2d")
      (error "make-skribe-reader: unsupported version" version)
      %skribe-reader))

(define (make-colon-free-token-reader tr)
  ;; Stolen from `guile-reader' 0.3.
  "If token reader @var{tr} handles the @code{:} (colon) character, remove it
from its specification and return the new token reader."
  (let* ((spec (r:token-reader-specification tr))
	 (proc (r:token-reader-procedure tr)))
    (r:make-token-reader (filter (lambda (chr)
				   (not (char=? chr #\:)))
				 spec)
			 proc)))

(define &sharp-reader
  ;; The reader for what comes after a `#' character.
  (let* ((dsssl-keyword-reader  ;; keywords à la `#!key'
          (r:make-token-reader #\!
 			       (r:token-reader-procedure
 				(r:standard-token-reader 'keyword)))))
      (r:make-reader (cons dsssl-keyword-reader
			   (map r:standard-token-reader
				'(character srfi-4 vector
				  number+radix boolean
				  srfi30-block-comment
				  srfi62-sexp-comment)))
		     #f ;; use default fault handler
		     'reader/record-positions)))

(define (%make-skribe-reader)
  (let ((colon-keywords ;; keywords à la `:key' fashion
	 (r:make-token-reader #\:
			      (r:token-reader-procedure
			       (r:standard-token-reader 'keyword))))
	(symbol-misc-chars-tr
	 ;; Make sure `:' is handled only by the keyword token reader.
	 (make-colon-free-token-reader
	  (r:standard-token-reader 'r6rs-symbol-misc-chars))))


    ;; Note: we use the `r6rs-symbol-*' and `r6rs-number' token readers since
    ;; they consider square brackets as delimiters.
    (r:make-reader (cons* (r:make-token-reader #\# &sharp-reader)
			  colon-keywords
			  symbol-misc-chars-tr
			  (map r:standard-token-reader
			       `(whitespace
				 sexp string r6rs-number
				 r6rs-symbol-lower-case
				 r6rs-symbol-upper-case
				 quote-quasiquote-unquote
				 semicolon-comment
				 skribe-exp)))
		   #f ;; use the default fault handler
		   'reader/record-positions
		   )))

;; We actually cache an instance here.
(define %skribe-reader (%make-skribe-reader))



;;; The reader specification.

(define-reader skribe "1.2d" make-skribe-reader)

;;; skribe.scm ends here
