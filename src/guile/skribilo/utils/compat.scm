;;; compat.scm  --  Skribe compatibility module.
;;;
;;; Copyright 2005, 2006  Ludovic Courtès  <ludovic.courtes@laas.fr>
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


(define-module (skribilo utils compat)
  :use-module (skribilo utils syntax)
  :use-module (skribilo utils files)
  :use-module (skribilo parameters)
  :use-module (skribilo evaluator)
  :use-module (srfi srfi-1)
  :autoload   (srfi srfi-13) (string-rindex)
  :use-module (srfi srfi-34)
  :use-module (srfi srfi-35)
  :use-module (ice-9 optargs)
  :autoload   (skribilo ast) (ast?)
  :autoload   (skribilo condition) (file-search-error? &file-search-error)
  :re-export (file-size)
  :replace (gensym))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; This module defines symbols for compatibility with Skribe 1.2.
;;;
;;; Code:

(fluid-set! current-reader %skribilo-module-reader)


;;;
;;; gensym
;;;

(define %gensym-orig (module-ref the-root-module 'gensym))

(define gensym
  ;; In Skribe, `gensym' accepts a symbol.  Guile's `gensym' accepts only
  ;; strings (or no argument).
  (lambda obj
    (apply %gensym-orig
	   (cond ((null? obj) '())
		 ((symbol? (car obj)) (list (symbol->string (car obj))))
		 ((string? (car obj)) (list (car obj)))
		 (else (skribe-error 'gensym "invalid argument" obj))))))


;;;
;;; Global variables that have been replaced by parameter objects
;;; in `(skribilo parameters)'.
;;;
;;; FIXME: There's not much we can do about these variables (as opposed to
;;; the _accessors_ below).  Perhaps we should just not define them?
;;;

;;; Switches
(define-public *skribe-verbose*	0)
(define-public *skribe-warning*	5)
(define-public *load-rc*		#t)


;;; Path variables
(define-public *skribe-path*		#f)
(define-public *skribe-bib-path*	'("."))
(define-public *skribe-source-path*	'("."))
(define-public *skribe-image-path*	'("."))


(define-public *skribe-rc-directory*
  (string-append (getenv "HOME") "/" ".skribilo"))


;;; In and out ports
(define-public *skribe-src*		'())
(define-public *skribe-dest*		#f)

;;; Engine
(define-public *skribe-engine*  	'html)	;; Use HTML by default

;;; Misc
(define-public *skribe-chapter-split*	'())
(define-public *skribe-ref-base*	#f)
(define-public *skribe-convert-image*  #f)	;; i.e. use the Skribe standard converter
(define-public *skribe-variants*	'())



;;;
;;; Accessors mapped to parameter objects.
;;;

(define-public skribe-path        *document-path*)
(define-public skribe-image-path  *image-path*)
(define-public skribe-source-path *source-path*)
(define-public skribe-bib-path    *bib-path*)

(define-public (skribe-path-set! path)        (*document-path* path))
(define-public (skribe-image-path-set! path)  (*image-path* path))
(define-public (skribe-source-path-set! path) (*source-path* path))
(define-public (skribe-bib-path-set! path)    (*bib-path* path))



;;;
;;; Evaluator.
;;;

(define %skribe-known-files
  ;; Like of Skribe package files and their equivalent Skribilo module.
  '(("web-book.skr"     . (skribilo package web-book))
    ("web-article.skr"  . (skribilo package web-article))
    ("slide.skr"        . (skribilo package slide))
    ("sigplan.skr"      . (skribilo package sigplan))
    ("scribe.skr"       . (skribilo package scribe))
    ("lncs.skr"         . (skribilo package lncs))
    ("letter.skr"       . (skribilo package letter))
    ("jfp.skr"          . (skribilo package jfp))
    ("french.skr"       . (skribilo package french))
    ("acmproc.skr"      . (skribilo package acmproc))))

(define*-public (skribe-load file :rest args)
  (call/cc
   (lambda (return)
     (guard (c ((file-search-error? c)
		;; Regular file loading failed.  Try built-ins.
		(let ((mod-name (assoc-ref %skribe-known-files file)))
		  (if mod-name
		      (begin
			(if (> (*verbose*) 1)
			    (format (current-error-port)
				    "  skribe-load: `~a' -> `~a'~%"
				    file mod-name))
			(let ((mod (false-if-exception
				    (resolve-module mod-name))))
			  (if (not mod)
			      (raise c)
			      (begin
				(set-module-uses!
				 (current-module)
				 (cons mod (module-uses (current-module))))
				(return #t)))))
		      (raise c)))))

	    ;; Try a regular `load-document'.
	    (apply load-document file args)))))


(define-public skribe-include      include-document)
(define-public skribe-load-options *load-options*)

(define-public skribe-eval         evaluate-document)
(define-public skribe-eval-port    evaluate-document-from-port)



;;;
;;; Compatibility with Bigloo.
;;;

(define-public (substring=? s1 s2 len)
  (let ((l1 (string-length s1))
	(l2 (string-length s2)))
    (let Loop ((i 0))
      (cond
	((= i len) #t)
	((= i l1)  #f)
	((= i l2)  #f)
	((char=? (string-ref s1 i) (string-ref s2 i)) (Loop (+ i 1)))
	(else #f)))))

(define-public (directory->list str)
  (map basename (glob (string-append str "/*") (string-append "/.*"))))

(define-macro (printf . args)   `(format #t ,@args))
(export-syntax printf)
(define-public fprintf			format)

(define-public (fprint port . args)
  (if port
      (with-output-to-port port
	(lambda ()
	  (for-each display args)
	  (display "\n")))))



(define-public prefix			file-prefix)
(define-public suffix			file-suffix)
(define-public system->string		system)  ;; FIXME
(define-public any?			any)
(define-public every?			every)
(define-public (find-file/path file path)
  (search-path path file))

(define-public process-input-port	#f) ;process-input)
(define-public process-output-port	#f) ;process-output)
(define-public process-error-port	#f) ;process-error)

;;; hash tables
(define-public make-hashtable		make-hash-table)
(define-public hashtable?		hash-table?)
(define-public hashtable-get		(lambda (h k) (hash-ref h k #f)))
(define-public hashtable-put!		hash-set!)
(define-public (hashtable-update! table key update-proc init-value)
  ;; This is a Bigloo-specific API.
  (let ((handle (hash-get-handle table key)))
    (if (not handle)
	(hash-set! table key init-value)
	(set-cdr! handle (update-proc (cdr handle))))))

(define-public hashtable->list	(lambda (h)
                          (map cdr (hash-map->list cons h))))

(define-public (find-runtime-type obj)
  (cond ((string? obj)  "string")
	((ast? obj)     "ast")
	((list? obj)    "list")
	((pair? obj)    "pair")
	((number? obj)  "number")
	((char? obj)    "character")
	((keyword? obj) "keyword")
	(else           (with-output-to-string
			  (lambda () (write obj))))))



;;;
;;; Miscellaneous.
;;;

(use-modules ((srfi srfi-19) #:renamer (symbol-prefix-proc 's19:)))

(define (date)
  (s19:date->string (s19:current-date) "~c"))



;;; compat.scm ends here
