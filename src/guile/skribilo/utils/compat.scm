;;; compat.scm  --  Skribe compatibility module.
;;;
;;; Copyright 2005, 2006, 2007  Ludovic Courtès  <ludovic.courtes@laas.fr>
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


(define-module (skribilo utils compat)
  :use-module (skribilo utils syntax)
  :use-module (skribilo utils files)
  :use-module (skribilo parameters)
  :use-module (skribilo evaluator)
  :use-module (skribilo color)
  :use-module (skribilo lib)
  :use-module (srfi srfi-1)
  :autoload   (srfi srfi-13)       (string-rindex)
  :use-module (srfi srfi-34)
  :use-module (ice-9 optargs)
  :autoload   (skribilo ast)       (ast? document? document-lookup-node)
  :autoload   (skribilo condition) (file-search-error? &file-search-error)
  :autoload   (skribilo reader)    (make-reader)
  :autoload   (skribilo resolve)   (*document-being-resolved*)
  :autoload   (skribilo output)    (*document-being-output*)
  :autoload   (skribilo biblio)    (*bib-table* open-bib-file)
  :use-module (skribilo debug)

  :re-export (file-size)  ;; re-exported from `(skribilo utils files)'
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
				 (resolve-interface mod-name))))
		       (if (not mod)
			   (raise c)
			   (begin
			     (module-use-interfaces! (current-module)
                                                     (list mod))
			     #t))))
		   (raise c)))))

	 ;; Try a regular `load-document'.
	 (apply load-document file args)))


(define-public skribe-include      include-document)
(define-public skribe-load-options *load-options*)

(define-public skribe-eval         evaluate-document)
(define-public skribe-eval-port    evaluate-document-from-port)

(set! %skribe-reader #f)
(define*-public (skribe-read #:optional (port (current-input-port)))
  (if (not %skribe-reader)
      (set! %skribe-reader (make-reader 'skribe)))
  (%skribe-reader port))


;;;
;;; Location.
;;;

(define-public (location-pos loc)  0)

(define-public skribe-eval-location invocation-location)


;;;
;;; Node lookup (formerly provided by `ast.scm').
;;;

(define-public (bind-markup! node)
  (let ((doc (or (*document-being-resolved*)
		 (*document-being-output*))))
    (if (document? doc)
	(document-bind-node! doc node)
	(error "Sorry, unable to achieve `bind-markup!'.  Use `document-bind-node!' instead."
	       node))))

(define-public (find-markups ident)
  (let ((doc (or (*document-being-resolved*)
		 (*document-being-output*))))
    (if (document? doc)
	(let ((result (document-lookup-node doc ident)))
	  (if result
	      (list result)
	      #f))
	(error "Sorry, unable to achieve `find-markups'.  Use `document-lookup-node' instead."
	       ident))))

(define-public (find-markup-ident ident)
  (or (find-markups ident) '()))



;;;
;;; Colors.
;;;

(define-public skribe-color->rgb color->rgb)
(define-public (skribe-use-color! c) c)
(define-public (skribe-get-used-colors)
  (let ((doc (or (*document-being-output*)
                 (*document-being-resolved*))))
    (if doc
        (document-used-colors doc)
        '())))



;;;
;;; Bibliography.
;;;

(define-public (default-bib-table)
  (*bib-table*))

(define-public (skribe-open-bib-file file command)
  (open-bib-file file command))



;;;
;;; Debugging facilities.
;;;

(define-public (set-skribe-debug! val)
  (*debug* val))

(define-public (no-debug-color)
  (*debug-use-colors?* #f))

(define-public skribe-debug *debug*)

(define-public (add-skribe-debug-symbol s)
  (*watched-symbols* (cons s *watched-symbols*)))



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
  ;; FIXME: Guile doesn't provide wrappers for `glob(3)'.
  ;;(map basename (glob (string-append str "/*") "/.*"))
  '())

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

(define-public (hashtable->list h)
  (hash-map->list (lambda (key val) val) h))

(define-public (find-runtime-type obj)
  (type-name obj))


;;;
;;; Miscellaneous.
;;;

(use-modules ((srfi srfi-19) #:renamer (symbol-prefix-proc 's19:)))

(define-public (date)
  (s19:date->string (s19:current-date) "~c"))

(define-public (correct-arity? proc argcount)
  (let ((a (procedure-property proc 'arity)))
    (and (pair? a)
         (let ((compulsory (car a))
               (optional   (cadr a))
               (rest?      (caddr a)))
           (or rest?
               (>= (+ compulsory optional) argcount))))))


;;; compat.scm ends here
