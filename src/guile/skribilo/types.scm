;;;
;;; types.stk	-- Definition of Skribe classes
;;;
;;; Copyright © 2003-2004 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
;;;
;;;           Author: Erick Gallesio [eg@essi.fr]
;;;    Creation date: 12-Aug-2003 22:18 (eg)
;;; Last file update: 28-Oct-2004 16:18 (eg)
;;;

(read-set! keywords 'prefix)
(define-module (skribilo types)  ;; FIXME:  Why should it be a separate module?
   :export (<ast> ast? ast-loc ast-loc-set!
	    <command> command? command-fmt command-body
	    <unresolved> unresolved? unresolved-proc
	    <handle> handle? handle-ast
	    <node> node? node-options node-loc
	    <engine> engine? engine-ident engine-format engine-customs
		     engine-filter engine-symbol-table
	    <writer> writer? write-object writer-options writer-ident
	             writer-before writer-action writer-after
	    <processor> processor? processor-combinator processor-engine
	    <markup> markup? bind-markup! markup-options is-markup?
		     markup-markup markup-body markup-ident markup-class
		     find-markups write-object
	    <container> container? container-options
			container-ident container-body
	    <document> document? document-ident document-body
		       document-options document-end
	    <language> language?
	    <location> location? ast-location

	    *node-table*)
   :use-module (oop goops))

(define *node-table* (make-hash-table))
					; Used to stores the nodes of  an AST.
					; It permits to retrieve a node from its
					; identifier.


;;; ======================================================================
;;;
;;;				<AST>
;;;
;;; ======================================================================
;;FIXME: set! location in <ast>
(define-class <ast> ()
  (parent :accessor ast-parent :init-keyword :parent :init-value 'unspecified)
  (loc    :init-value #f))

(define (ast? obj)		(is-a? obj <ast>))
(define (ast-loc obj)		(slot-ref obj 'loc))
(define (ast-loc-set! obj v)	(slot-set! obj 'loc v))

;;; ======================================================================
;;;
;;;				<COMMAND>
;;;
;;; ======================================================================
(define-class <command> (<ast>)
  (fmt    :init-keyword :fmt)
  (body   :init-keyword :body))

(define (command? obj)     (is-a? obj <command>))
(define (command-fmt obj)  (slot-ref obj 'fmt))
(define (command-body obj) (slot-ref obj 'body))

;;; ======================================================================
;;;
;;;				<UNRESOLVED>
;;;
;;; ======================================================================
(define-class <unresolved> (<ast>)
  (proc :init-keyword :proc))

(define (unresolved? obj)     (is-a? obj <unresolved>))
(define (unresolved-proc obj) (slot-ref obj 'proc))

;;; ======================================================================
;;;
;;;				<HANDLE>
;;;
;;; ======================================================================
(define-class <handle> (<ast>)
  (ast :init-keyword :ast :init-value #f :getter handle-ast))

(define (handle? obj)     (is-a? obj <handle>))
(define (handle-ast obj)  (slot-ref obj 'ast))

;;; ======================================================================
;;;
;;;				<ENGINE>
;;;
;;; ======================================================================
(define-class <engine> ()
  (ident		:init-keyword :ident		:init-value '???)
  (format		:init-keyword :format		:init-value "raw")
  (info		:init-keyword :info		:init-value '())
  (version		:init-keyword :version		:init-value 'unspecified)
  (delegate		:init-keyword :delegate		:init-value #f)
  (writers		:init-keyword :writers		:init-value '())
  (filter		:init-keyword :filter		:init-value #f)
  (customs		:init-keyword :custom		:init-value '())
  (symbol-table	:init-keyword :symbol-table	:init-value '()))




(define (engine? obj)
  (is-a? obj <engine>))

(define (engine-ident obj)	;; Define it here since the doc searches it
  (slot-ref obj 'ident))

(define (engine-format obj)	;; Define it here since the doc searches it
  (slot-ref obj 'format))

(define (engine-customs obj)	;; Define it here since the doc searches it
  (slot-ref obj 'customs))

(define (engine-filter obj)	;; Define it here since the doc searches it
  (slot-ref obj 'filter))

(define (engine-symbol-table obj)	;; Define it here since the doc searches it
  (slot-ref obj 'symbol-table))

;;; ======================================================================
;;;
;;;				<WRITER>
;;;
;;; ======================================================================
(define-class <writer> ()
  (ident	:init-keyword :ident	 :init-value '??? :getter writer-ident)
  (class	:init-keyword :class	 :init-value 'unspecified
		:getter writer-class)
  (pred	:init-keyword :pred	 :init-value 'unspecified)
  (upred	:init-keyword :upred	 :init-value 'unspecified)
  (options	:init-keyword :options	 :init-value '()  :getter writer-options)
  (verified?	:init-keyword :verified? :init-value #f)
  (validate	:init-keyword :validate  :init-value #f)
  (before	:init-keyword :before	 :init-value #f   :getter writer-before)
  (action	:init-keyword :action	 :init-value #f   :getter writer-action)
  (after	:init-keyword :after	 :init-value #f   :getter writer-after))

(define (writer? obj)
  (is-a? obj <writer>))

(define-method (write-object (obj <writer>) port)
  (format port "#[~A (~A) ~A]"
	  (class-name (class-of obj))
	  (slot-ref obj 'ident)
	  (address-of obj)))

;;; ======================================================================
;;;
;;;				<NODE>
;;;
;;; ======================================================================
(define-class <node> (<ast>)
  (required-options :init-keyword :required-options :init-value '())
  (options	     :init-keyword :options	     :init-value '())
  (body	     :init-keyword :body	     :init-value #f
	     :getter	   node-body))

(define (node? obj)        (is-a? obj <node>))
(define (node-options obj) (slot-ref obj 'options))
(define node-loc	   ast-loc)


;;; ======================================================================
;;;
;;;				<PROCESSOR>
;;;
;;; ======================================================================
(define-class <processor> (<node>)
  (combinator :init-keyword :combinator :init-value (lambda (e1 e2) e1))
  (engine     :init-keyword :engine	 :init-value 'unspecified)
  (procedure  :init-keyword :procedure	 :init-value (lambda (n e) n)))

(define (processor? obj)           (is-a? obj <processor>))
(define (processor-combinator obj) (slot-ref obj 'combinator))
(define (processor-engine obj)     (slot-ref obj 'engine))

;;; ======================================================================
;;;
;;;				<MARKUP>
;;;
;;; ======================================================================
(define-class <markup> (<node>)
  (ident  :init-keyword :ident  :getter markup-ident :init-value #f)
  (class  :init-keyword :class  :getter markup-class :init-value #f)
  (markup :init-keyword :markup :getter markup-markup))


(define (bind-markup! node)
  (hash-set! *node-table*
	     (markup-ident node)
	     ;(lambda (cur) (cons node cur))
	     (list node)))


(define-method (initialize (self <markup>) initargs)
  (next-method)
  (bind-markup! self))


(define (markup? obj)		(is-a? obj <markup>))
(define (markup-options obj)	(slot-ref obj 'options))
(define markup-body    node-body)


(define (is-markup? obj markup)
  (and (is-a? obj <markup>)
       (eq? (slot-ref obj 'markup) markup)))



(define (find-markups ident)
  (hash-ref *node-table* ident #f))


(define-method (write-object (obj <markup>) port)
  (format port "#[~A (~A/~A) ~A]"
	  (class-name (class-of obj))
	  (slot-ref obj 'markup)
	  (slot-ref obj 'ident)
	  (address-of obj)))

;;; ======================================================================
;;;
;;;				<CONTAINER>
;;;
;;; ======================================================================
(define-class <container> (<markup>)
  (env :init-keyword :env :init-value '()))

(define (container? obj)    (is-a? obj <container>))
(define (container-env obj) (slot-ref obj 'env))
(define container-options   markup-options)
(define container-ident     markup-ident)
(define container-body      node-body)



;;; ======================================================================
;;;
;;;				<DOCUMENT>
;;;
;;; ======================================================================
(define-class <document> (<container>))

(define (document? obj)      (is-a? obj <document>))
(define (document-ident obj) (slot-ref obj 'ident))
(define (document-body obj)  (slot-ref obj 'body))
(define document-options     markup-options)
(define document-env         container-env)



;;; ======================================================================
;;;
;;;				<LANGUAGE>
;;;
;;; ======================================================================
(define-class <language> ()
  (name	:init-keyword :name	 :init-value #f :getter langage-name)
  (fontifier	:init-keyword :fontifier :init-value #f :getter langage-fontifier)
  (extractor	:init-keyword :extractor :init-value #f :getter langage-extractor))

(define (language? obj)
  (is-a? obj <language>))


;;; ======================================================================
;;;
;;;				<LOCATION>
;;;
;;; ======================================================================
(define-class <location> ()
  (file :init-keyword :file :getter location-file)
  (pos  :init-keyword :pos  :getter location-pos)
  (line :init-keyword :line :getter location-line))

(define (location? obj)
  (is-a? obj <location>))

(define (ast-location obj)
  (let ((loc (slot-ref obj 'loc)))
    (if (location? loc)
	(let* ((fname (location-file loc))
	       (line  (location-line loc))
	       (pwd   (getcwd))
	       (len   (string-length pwd))
	       (lenf  (string-length fname))
	       (file  (if (and (substring=? pwd fname len)
			       (> lenf len))
			  (substring fname len (+ 1 (string-length fname)))
			  fname)))
	  (format "~a, line ~a" file line))
	"no source location")))
