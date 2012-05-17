;;; syntax.scm  --  Syntactic candy for Skribilo modules. -*- coding: utf-8 -*-
;;;
;;; Copyright 2005, 2006, 2007, 2008, 2009, 2010, 2011,
;;;   2012  Ludovic Courtès <ludo@gnu.org>
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

(define-module (skribilo utils syntax)
  :use-module (system reader library)
  :use-module (system reader compat) ;; make sure `current-reader' exists
  :use-module (system reader confinement)
  :export (%skribilo-module-reader skribilo-module-syntax
                                   set-correct-file-encoding!
                                   default-to-utf-8
                                   _ N_
                                   unwind-protect))

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

         (cond-expand
          (guile-2 '(reader/record-positions))
          (else
           ;; On Guile 1.8, don't record positions by default: this yields a
           ;; nice read performance improvement.
           (if (memq 'positions (debug-options))
               (list 'reader/record-positions)
               '())))))

(define-macro (skribilo-module-syntax)
  "Install the syntax reader for Skribilo modules."
  (fluid-set! current-reader %skribilo-module-reader)
  #t)


(define-macro (unwind-protect expr1 expr2)
  ;; This is no completely correct.
  `(dynamic-wind
     (lambda () #f)
     (lambda () ,expr1)
     (lambda () ,expr2)))

(cond-expand
 ((not guile-2)
  ;; In Guile 2.x these macros are defined in the core.
  (begin
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

    (export when unless)))
 (else (begin)))

(cond-expand
 (guile-2
  (define-syntax set-correct-file-encoding!
    (syntax-rules ()
      ((_)
       (set-correct-file-encoding! (current-input-port)))
      ((_ port)
       ;; Use the encoding specified by the `coding:' comment.
       (let ((e (false-if-exception (file-encoding port))))
         (and (string? e)
              (set-port-encoding! port e))))))

  (define-syntax default-to-utf-8
    (syntax-rules ()
      ((_ body ...)
       (with-fluids ((%default-port-encoding "UTF-8"))
         body ...)))))

 (else
  (define-macro (set-correct-file-encoding! . p)
    #f)

  (define-macro (default-to-utf-8 . body)
    `(begin ,@body))))


;;;
;;; Gettext support.
;;;

(define %skribilo-text-domain "skribilo")

(textdomain %skribilo-text-domain)

(define (_ msg)
  (gettext msg %skribilo-text-domain))

(define (N_ msg msgplural n)
  (ngettext msg msgplural n %skribilo-text-domain))

;;; syntax.scm ends here
