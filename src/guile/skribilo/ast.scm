;;; ast.scm  --  Skribilo abstract syntax trees.
;;;
;;; Copyright 2003, 2004  Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
;;; Copyright 2003, 2004  Manuel Serrano
;;; Copyright 2005, 2006  Ludovic Courtès <ludovic.courtes@laas.fr>
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

(define-module (skribilo ast)
  :use-module (oop goops)
  :autoload (skribilo location) (location?)
  :autoload (skribilo lib) (skribe-type-error skribe-error)
  :autoload (srfi srfi-1)  (fold)
  :use-module (skribilo utils syntax)
  :export (<ast> ast? ast-loc ast-loc-set!
		 ast-parent ast->string ast->file-location
		 ast-resolved?

	   <command> command? command-fmt command-body
	   <unresolved> unresolved? unresolved-proc
	   <handle> handle? handle-ast handle-body
	   <node> node? node-options node-loc node-body
	   <processor> processor? processor-combinator processor-engine

	   <markup> markup? markup-options is-markup?
		    markup-markup markup-body markup-body-set!
                    markup-ident markup-class
		    markup-option markup-option-set!
		    markup-option-add! markup-output
		    markup-parent markup-document markup-chapter

	   <container> container? container-options
		       container-ident container-body
		       container-env-get

	   <document> document? document-ident document-body
		      document-options document-end
		      document-lookup-node document-bind-node!
		      document-bind-nodes!

           ;; traversal
	   ast-fold
           container-search-down search-down find-down find1-down
           find-up find1-up
           ast-document ast-chapter ast-section))

;;; Author:  Erick Gallesio, Manuel Serrano, Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; The abstract syntax tree (AST) and its sub-types.  These class form the
;;; core of a document: each part of a document is an instance of `<ast>' or
;;; one of its sub-classes.
;;;
;;; Code:

(fluid-set! current-reader %skribilo-module-reader)




;;;
;;; Abstract syntax tree (AST).
;;;

;;FIXME: set! location in <ast>
(define-class <ast> ()
  ;; Parent of this guy.
  (parent  :accessor ast-parent :init-keyword :parent :init-value 'unspecified)

  ;; Its source location.
  (loc     :init-value #f)

  ;; This slot is used as an optimization when resolving an AST: sub-parts of
  ;; the tree are marked as resolved as soon as they are and don't need to be
  ;; traversed again.
  (resolved? :accessor ast-resolved? :init-value #f))


(define (ast? obj)		(is-a? obj <ast>))
(define (ast-loc obj)		(slot-ref obj 'loc))
(define (ast-loc-set! obj v)	(slot-set! obj 'loc v))
(define (ast-parent n)
  (slot-ref n 'parent))


(define (ast->file-location ast)
   (let ((l (ast-loc ast)))
     (if (location? l)
	 (format #f "~a:~a:" (location-file l) (location-line l))
	 "")))

(define-generic ast->string)

(define-method (ast->string (ast <top>))     "")
(define-method (ast->string (ast <string>))  ast)
(define-method (ast->string (ast <number>))  (number->string ast))

(define-method (ast->string (ast <pair>))
  (let ((out (open-output-string)))
    (let Loop ((lst ast))
      (cond
	((null? lst)
	   (get-output-string out))
	(else
	   (display (ast->string (car lst)) out)
	   (unless (null? (cdr lst))
	     (display #\space out))
	   (Loop (cdr lst)))))))



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
(define (handle-body h)   (slot-ref h 'body))

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

(define-method (ast->string (ast <node>))
  (ast->string (slot-ref ast 'body)))


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



;;;
;;; Markup.
;;;

(define-class <markup> (<node>)
  (ident  :init-keyword :ident  :getter markup-ident :init-value #f)
  (class  :init-keyword :class  :getter markup-class :init-value #f)
  (markup :init-keyword :markup :getter markup-markup))


(define (markup? obj)		(is-a? obj <markup>))
(define (markup-options obj)	(slot-ref obj 'options))
(define markup-body    node-body)
(define (markup-body-set! m body)
  (slot-set! m 'resolved? #f)
  (slot-set! m 'body      body))

(define (markup-option m opt)
  (if (markup? m)
      (let ((c (assq opt (slot-ref m 'options))))
	(and (pair? c) (pair? (cdr c))
	     (cadr c)))
      (skribe-type-error 'markup-option "Illegal markup: " m "markup")))

(define (markup-option-set! m opt val)
  (if (markup? m)
      (let ((c (assq opt (slot-ref m 'options))))
	(if (and (pair? c) (pair? (cdr c)))
	    (set-cdr! c (list val))
	    (skribe-error 'markup-option-set! "unknown option: "
			  m)))
      (skribe-type-error 'markup-option-set! "Illegal markup: " m "markup")))

(define (markup-option-add! m opt val)
  (if (markup? m)
      (slot-set! m 'options (cons (list opt val)
				  (slot-ref m 'options)))
      (skribe-type-error 'markup-option "Illegal markup: " m "markup")))


(define (is-markup? obj markup)
  (and (is-a? obj <markup>)
       (eq? (slot-ref obj 'markup) markup)))


(define (markup-parent m)
  (let ((p (slot-ref m 'parent)))
    (if (eq? p 'unspecified)
	(skribe-error 'markup-parent "Unresolved parent reference" m)
	p)))

(define (markup-document m)
  (let Loop ((p m)
	     (l #f))
    (cond
      ((is-markup? p 'document)           p)
      ((or (eq? p 'unspecified) (not p))  l)
      (else			          (Loop (slot-ref p 'parent) p)))))

(define (markup-chapter m)
  (let loop ((p m)
	     (l #f))
    (cond
      ((is-markup? p 'chapter)           p)
      ((or (eq? p 'unspecified) (not p)) l)
      (else				 (loop (slot-ref p 'parent) p)))))




(define-method (write (obj <markup>) port)
  (format port "#<~A (~A/~A) ~A>"
	  (class-name (class-of obj))
	  (slot-ref obj 'markup)
	  (slot-ref obj 'ident)
	  (object-address obj)))

(define-method (write (node <unresolved>) port)
  (let ((proc (slot-ref node 'proc)))
    (format port "#<<unresolved> (~A~A) ~A>"
	    proc
	    (let* ((name (or (procedure-name proc) ""))
		   (source (procedure-source proc))
		   (file (and source (source-property source 'filename)))
		   (line (and source (source-property source 'line))))
	      ;;(format (current-error-port) "src=~a~%" source)
	      (string-append name
			     (if file
				 (string-append " " file
						(if line
						    (number->string line)
						    ""))
				 "")))
	    (object-address node))))



;;; XXX: This was already commented out in the original Skribe source.
;;;
;; (define (markup-output markup
;;		       :optional (engine    #f)
;;		       :key	 (predicate #f)
;;				 (options  '())
;;				 (before    #f)
;;				 (action    #f)
;;				 (after     #f))
;;   (let ((e (or engine (use-engine))))
;;     (cond
;;       ((not (is-a? e <engine>))
;;           (skribe-error 'markup-writer "illegal engine" e))
;;       ((and (not before)
;;	    (not action)
;;	    (not after))
;;           (%find-markup-output e markup))
;;       (else
;;	  (let ((mp (if (procedure? predicate)
;;			(lambda (n e) (and (is-markup? n markup) (predicate n e)))
;;			(lambda (n e) (is-markup? n markup)))))
;;	    (engine-output e markup mp options
;;			   (or before (slot-ref e 'default-before))
;;			   (or action (slot-ref e 'default-action))
;;			   (or after  (slot-ref e 'default-after))))))))



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

(define (container-env-get m key)
  (let ((c (assq key (slot-ref m 'env))))
    (and (pair? c) (cadr c))))



;;;
;;; Document.
;;;

(define-class <document> (<container>)
  (node-table   :init-thunk make-hash-table :getter document-node-table)
  (nodes-bound? :init-value #f :getter document-nodes-bound?))


(define (document? obj)      (is-a? obj <document>))
(define (document-ident obj) (slot-ref obj 'ident))
(define (document-body obj)  (slot-ref obj 'body))
(define document-options     markup-options)
(define document-env         container-env)

(define (document-lookup-node doc ident)
  ;; Lookup the node with identifier IDENT (a string) in document DOC.
  (hash-ref (document-node-table doc) ident))

(define (document-bind-node! doc node . ident)
  ;; Bind NODE (a markup object) to DOC (a document object).
  (let ((ident (if (null? ident) (markup-ident node) (car ident))))
    (if ident
	(let ((handle (hash-get-handle (document-node-table doc) ident)))
	  ;;(format (current-error-port) "binding `~a' in `~a'~%" ident node)
	  (if (and (pair? handle) (not (eq? (cdr handle) node)))
	      (error "node identifier already bound"
		     (cdr handle)) ;; FIXME: use `raise'
	      (hash-set! (document-node-table doc) ident node))))))

(define (document-bind-nodes! doc)
  ;; Bind all the nodes contained in DOC if they are not already bound.
  ;; Once, this is done, `document-lookup-node' can be used to search a node
  ;; by its identifier.

  ;; We assume that unresolved nodes do not introduce any new identifier,
  ;; hence this optimization.
  (if (document-nodes-bound? doc)
      #t
      (begin
	(ast-fold (lambda (node result)
		    (if (markup? node) (document-bind-node! doc node))
		    #t)
		  #t ;; unused
		  doc)
	(slot-set! doc 'nodes-bound? #t))))


;;;
;;; AST traversal utilities.
;;;

(define (ast-fold proc init ast)
  ;; Apply PROC to each node in AST (per `node?'), in a way similar to `fold'
  ;; (in SRFI-1).
  (let loop ((ast ast)
	     (result init))
    (cond ((pair? ast)
	   (fold loop result ast))
	  ((node? ast)
	   (loop (node-body ast) (proc ast result)))
	  (else result))))


;; The procedures below are almost unchanged compared to Skribe 1.2d's
;; `lib.scm' file found in the `common' directory, written by Manuel Serrano
;; (I removed uses of `with-debug' et al., though).


(define (container-search-down pred obj)
  (let loop ((obj (markup-body obj)))
    (cond
     ((pair? obj)
      (apply append (map (lambda (o) (loop o)) obj)))
     ((container? obj)
      (let ((rest (loop (markup-body obj))))
        (if (pred obj)
            (cons obj rest)
            rest)))
     ((pred obj)
      (list obj))
     (else
      '()))))

(define (search-down pred obj)
  (let loop ((obj (markup-body obj)))
    (cond
     ((pair? obj)
      (apply append (map (lambda (o) (loop o)) obj)))
     ((markup? obj)
      (let ((rest (loop (markup-body obj))))
        (if (pred obj)
            (cons obj rest)
            rest)))
     ((pred obj)
      (list obj))
     (else
      '()))))

(define (find-down pred obj)
  (let loop ((obj obj))
    (cond
     ((pair? obj)
      (apply append (map (lambda (o) (loop o)) obj)))
     ((markup? obj)
      (if (pred obj)
          (list (cons obj (loop (markup-body obj))))
          '()))
     (else
      (if (pred obj)
          (list obj)
          '())))))

(define (find1-down pred obj)
  (let loop ((obj obj)
             (stack '()))
    (cond
     ((memq obj stack)
      (skribe-error 'find1-down "Illegal cyclic object" obj))
     ((pair? obj)
      (let liip ((obj obj))
        (cond
         ((null? obj)
          #f)
         (else
          (or (loop (car obj) (cons obj stack))
              (liip (cdr obj)))))))
     ((pred obj)
      obj)
     ((markup? obj)
      (loop (markup-body obj) (cons obj stack)))
     (else
      #f))))

(define (find-up pred obj)
  (let loop ((obj obj)
             (res '()))
    (cond
     ((not (ast? obj))
      res)
     ((pred obj)
      (loop (ast-parent obj) (cons obj res)))
     (else
      (loop (ast-parent obj) (cons obj res))))))

(define (find1-up pred obj)
  (let loop ((obj obj))
    (cond
     ((not (ast? obj))
      #f)
     ((pred obj)
      obj)
     (else
      (loop (ast-parent obj))))))

(define (ast-document m)
  (find1-up document? m))

(define (ast-chapter m)
  (find1-up (lambda (n) (is-markup? n 'chapter)) m))

(define (ast-section m)
  (find1-up (lambda (n) (is-markup? n 'section)) m))


;;; arch-tag: e2489bd6-1b6d-4b03-bdfb-83cffd2f7ce7

;;; ast.scm ends here
