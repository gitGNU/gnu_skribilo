;;; syntax.scm  --  Syntactic candy for Skribilo modules.
;;;
;;; Copyright 2005, 2006, 2007, 2008  Ludovic Courtès <ludo@gnu.org>
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;;; USA.

(define-module (skribilo utils syntax)
  :use-module (skribilo reader)
  :use-module (system reader library)
  :use-module (system reader compat) ;; make sure `current-reader' exists
  :use-module (system reader confinement)
  :export (%skribe-reader %skribilo-module-reader
           _ N_)
  :export-syntax (unwind-protect unless when))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; This module provides syntactic candy for Skribilo modules, i.e., a syntax
;;; similar to Guile's default syntax with a few extensions, plus various
;;; convenience macros.
;;;
;;; Code:

(define %skribilo-module-reader
  ;; The syntax used to read Skribilo modules.
  (apply make-alternate-guile-reader
         '(colon-keywords no-scsh-block-comments
           srfi30-block-comments srfi62-sexp-comments)
         (lambda (chr port read)
	   (let ((file (port-filename port))
		 (line (port-line port))
		 (column (port-column port)))
	     (error (string-append
		     (if (string? file)
			 (format #f "~a:~a:~a: " file line column)
			 "")
		     (_ "unexpected character in Skribilo module"))
		    chr)))

         ;; By default, don't record positions: this yields a nice read
         ;; performance improvement.
         (if (memq 'debug (debug-options))
             (list 'reader/record-positions)
             '())))

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
  `(if (not ,condition)
       ,(if (null? (cdr exprs))
            (car exprs)
            `(begin ,@exprs))))

(define-macro (when condition . exprs)
  `(if ,condition
       ,(if (null? (cdr exprs))
            (car exprs)
            `(begin ,@exprs))))


;;;
;;; Gettext support.
;;;

(define %skribilo-text-domain "skribilo")

(textdomain %skribilo-text-domain)

(define (_ msg)
  (gettext msg %skribilo-text-domain))

(define (N_ msg msgplural n)
  (ngettext msg msgplural n %skribilo-text-domain))


;;; Local Variables:
;;; mode: scheme
;;; coding: latin-1
;;; End:

;;; syntax.scm ends here
