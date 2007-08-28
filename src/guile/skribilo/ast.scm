;;; ast.scm  --  Skribilo abstract syntax trees.
;;;
;;; Copyright 2003, 2004  Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
;;; Copyright 2003, 2004  Manuel Serrano
;;; Copyright 2005, 2006, 2007  Ludovic Courtès <ludovic.courtes@laas.fr>
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

  :use-module (srfi srfi-13)
  :use-module (srfi srfi-34)
  :use-module (srfi srfi-35)
  :use-module (skribilo condition)
  :use-module (skribilo utils syntax)

  :autoload (skribilo location) (location?)
  :autoload (srfi srfi-1)  (fold concatenate)

  :use-module (ice-9 optargs)

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
		    markup-option-add!
		    markup-parent markup-document markup-chapter

	   <container> container? container-options
		       container-ident container-body
		       container-env-get

	   <document> document? document-ident document-body
		      document-options document-env
		      document-lookup-node document-bind-node!
		      document-bind-nodes!

           ;; traversal
	   ast-fold
           container-search-down search-down find-down find1-down
           find-up find1-up
           ast-document ast-chapter ast-section

           ;; numbering
           markup-number-string

	   ;; error conditions
	   &ast-error &ast-orphan-error &ast-cycle-error
	   &markup-unknown-option-error &markup-already-bound-error
	   ast-orphan-error? ast-orphan-error:ast
	   ast-cycle-error? ast-cycle-error:object
	   markup-unknown-option-error?
	   markup-unknown-option-error:markup
	   markup-unknown-option-error:option
	   markup-already-bound-error?
	   markup-already-bound-error:markup
	   markup-already-bound-error:ident))

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
;;; Error conditions.
;;;

(define-condition-type &ast-error &skribilo-error
  ast-error?)

(define-condition-type &ast-orphan-error &ast-error
  ast-orphan-error?
  (ast ast-orphan-error:ast))

(define-condition-type &ast-cycle-error &ast-error
  ast-cycle-error?
  (object ast-cycle-error:object))

(define-condition-type &markup-unknown-option-error &ast-error
  markup-unknown-option-error?
  (markup markup-unknown-option-error:markup)
  (option markup-unknown-option-error:option))

(define-condition-type &markup-already-bound-error &ast-error
  markup-already-bound-error?
  (markup markup-already-bound-error:markup)
  (ident  markup-already-bound-error:ident))


(define (handle-ast-error c)
  ;; Issue a user-friendly error message for error condition C.
  (define (show-location obj)
    (let ((location (and (ast? obj) (ast-loc obj))))
      (if (location? location)
          (format (current-error-port) "~a:~a:~a: "
                  (location-file location)
                  (location-line location)
                  (location-column location)))))

  (cond ((ast-orphan-error? c)
	 (let ((node (ast-orphan-error:ast c)))
           (show-location node)
	   (format (current-error-port) (_ "orphan node: ~a~%")
		   node)))

	((ast-cycle-error? c)
	 (let ((object (ast-cycle-error:object c)))
           (show-location object)
	   (format (current-error-port)
		   (_ "cycle found in AST: ~a~%") object)))

	((markup-unknown-option-error? c)
	 (let ((markup (markup-unknown-option-error:markup c))
	       (option (markup-unknown-option-error:option c)))
           (show-location markup)
	   (format (current-error-port)
		   (_ "~a: unknown markup option for `~a'~%")
		   option markup)))

	((markup-already-bound-error? c)
	 (let ((markup (markup-already-bound-error:markup c))
	       (ident  (markup-already-bound-error:ident  c)))
           (show-location markup)
	   (format (current-error-port)
		   (_ "`~a' (~a): markup identifier already bound~%")
		   ident
		   (if (markup? markup)
		       (markup-markup markup)
		       markup))))

	(else
	 (format (current-error-port)
                 (_ "undefined AST error: ~a~%")
		 c))))

(register-error-condition-handler! ast-error? handle-ast-error)



;;;
;;; Abstract syntax tree (AST).
;;;

(define-class <ast> ()
  ;; Parent of this guy.
  (parent  :accessor ast-parent :init-keyword :parent :init-value 'unspecified)

  ;; Its source location.
  (loc     :init-value #f :init-keyword :loc
           :getter ast-loc :setter ast-loc-set!)

  ;; This slot is used as an optimization when resolving an AST: sub-parts of
  ;; the tree are marked as resolved as soon as they are and don't need to be
  ;; traversed again.
  (resolved? :accessor ast-resolved? :init-value #f))


(define (ast? obj)		(is-a? obj <ast>))
(define-method (equal? (a <ast>) (b <ast>))
  (eq? (ast-parent a) (ast-parent b)))


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
  (fmt    :init-keyword :fmt  :getter command-fmt)
  (body   :init-keyword :body :getter command-body))

(define (command? obj)     (is-a? obj <command>))


;;; ======================================================================
;;;
;;;				<UNRESOLVED>
;;;
;;; ======================================================================
(define-class <unresolved> (<ast>)
  (proc :init-keyword :proc :getter unresolved-proc))

(define (unresolved? obj)     (is-a? obj <unresolved>))

;;; ======================================================================
;;;
;;;				<HANDLE>
;;;
;;; ======================================================================
(define-class <handle> (<ast>)
  (ast :init-keyword :ast :init-value #f :getter handle-ast))

(define (handle? obj)     (is-a? obj <handle>))
(define (handle-body h)   (slot-ref h 'body)) ;; FIXME: Looks suspicious!

;;; ======================================================================
;;;
;;;				<NODE>
;;;
;;; ======================================================================
(define-class <node> (<ast>)
  (required-options :init-keyword :required-options :init-value '())
  (options          :init-keyword :options	    :init-value '()
                    :getter node-options)
  (body	     :init-keyword :body	     :init-value #f
	     :getter	   node-body))

(define (node? obj)        (is-a? obj <node>))
(define-method (equal? (a <node>) (b <node>))
  (and (next-method)
       (equal? (slot-ref a 'required-options)
               (slot-ref b 'required-options))
       (equal? (node-body a)
               (node-body b))))

(define node-loc	   ast-loc)

(define-method (ast->string (ast <node>))
  (ast->string (slot-ref ast 'body)))


;;; ======================================================================
;;;
;;;				<PROCESSOR>
;;;
;;; ======================================================================
(define-class <processor> (<node>)
  (combinator :init-keyword :combinator  :init-value (lambda (e1 e2) e1)
              :getter processor-combinator)
  (engine     :init-keyword :engine	 :init-value 'unspecified
              :getter processor-engine)
  (procedure  :init-keyword :procedure	 :init-value (lambda (n e) n)))

(define (processor? obj)           (is-a? obj <processor>))



;;;
;;; Markup.
;;;

(define-class <markup> (<node>)
  (ident  :init-keyword :ident  :getter markup-ident :init-value #f)
  (class  :init-keyword :class  :getter markup-class :init-value #f)
  (markup :init-keyword :markup :getter markup-markup))


(define (markup? obj)		(is-a? obj <markup>))
(define-method (equal? (a <markup>) (b <markup>))
  (and (next-method)
       ;; We don't check for equality of `ident' because most markups return
       ;; a fresh identifier every time they are called (using `gensym'),
       ;; unless an identifier is explicitly specified.
       (equal? (markup-class a)  (markup-class b))
       (eq?    (markup-markup a) (markup-markup b))))

(define markup-options	        node-options)
(define markup-body             node-body)
(define (markup-body-set! m body)
  (slot-set! m 'resolved? #f)
  (slot-set! m 'body      body))

(define (markup-option m opt)
  (if (markup? m)
      (let ((c (assq opt (slot-ref m 'options))))
	(and (pair? c) (pair? (cdr c))
	     (cadr c)))
      (raise (condition (&invalid-argument-error
			 (proc-name "markup-option")
			 (argument  m))))))

(define (markup-option-set! m opt val)
  (if (markup? m)
      (let ((c (assq opt (slot-ref m 'options))))
	(if (and (pair? c) (pair? (cdr c)))
	    (set-cdr! c (list val))
	    (raise (condition (&markup-unknown-option-error
			       (markup m)
			       (option opt))))))
      (raise (condition (&invalid-argument-error
			 (proc-name "markup-option-set!")
			 (argument  m))))))

(define (markup-option-add! m opt val)
  (if (markup? m)
      (slot-set! m 'options (cons (list opt val)
				  (slot-ref m 'options)))
      (raise (condition (&invalid-argument-error
			 (proc-name "markup-option-add!")
			 (argument  m))))))


(define (is-markup? obj markup)
  (and (is-a? obj <markup>)
       (eq? (slot-ref obj 'markup) markup)))


(define (markup-parent m)
  (let ((p (slot-ref m 'parent)))
    (if (eq? p 'unspecified)
	(raise (condition (&ast-orphan-error (ast m))))
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
  (env :init-keyword :env :init-value '() :getter container-env))

(define (container? obj)    (is-a? obj <container>))
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
(define document-ident       container-ident)
(define document-body        container-body)
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
	      (raise (condition (&markup-already-bound-error
				 (ident  ident)
				 (markup node))))
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
      (concatenate (map loop obj)))
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
      (concatenate (map loop obj)))
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
      (concatenate (map loop obj)))
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
      (raise (condition (&ast-cycle-error (object obj)))))
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
      (loop (ast-parent obj) res)))))

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


;;;
;;; Section numbering.
;;;

(define* (markup-number-string markup :optional (sep "."))
  ;; Return a structure number string such as "1.2".
  (cond ((is-markup? markup 'figure)
         ;; Figure numbering is assumed to be document-wide.
         (let ((num (markup-option markup :number)))
           (if (number? num)
               (number->string num)
               num)))
        (else
         ;; Use a hierarchical numbering scheme.
         (let loop ((markup markup)
                    (result '()))
           (if (document? markup)
               (string-join result sep)
               (let ((num (markup-option markup :number)))
                 (loop (ast-parent markup)
                       (cond ((number? num)
                              (cons (number->string num)
                                    result))
                             ((ast? num)
                              (cons (ast->string num)
                                    result))
                             ((string? num)
                              (cons num result))
                             (else
                              result)))))))))


;;; Local Variables:
;;; coding: latin-1
;;; End:

;;; arch-tag: e2489bd6-1b6d-4b03-bdfb-83cffd2f7ce7

;;; ast.scm ends here
