;*=====================================================================*/
;*    serrano/prgm/project/skribe/src/bigloo/resolve.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jul 25 09:31:18 2003                          */
;*    Last change :  Sun Jul 11 09:17:52 2004 (serrano)                */
;*    Copyright   :  2003-04 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Skribe resolve stage                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module skribe_resolve
   
   (include "debug.sch")
   
   (import  skribe_types
	    skribe_lib
	    skribe_bib
	    skribe_eval)
   
   (import  skribe_index)
   
   (export  (resolve! ::obj ::%engine ::pair-nil)
	    (resolve-children ::obj)
	    (resolve-children* ::obj)
	    (resolve-parent ::%ast ::pair-nil)
	    (resolve-search-parent ::%ast ::pair-nil ::procedure)
	    (resolve-counter ::%ast ::pair-nil ::symbol ::obj . o)
	    (resolve-ident ::bstring ::obj ::%ast ::obj)))

;*---------------------------------------------------------------------*/
;*    *unresolved* ...                                                 */
;*---------------------------------------------------------------------*/
(define *unresolved* #f)

;*---------------------------------------------------------------------*/
;*    resolve! ...                                                     */
;*    -------------------------------------------------------------    */
;*    This function iterates over an ast until all unresolved          */
;*    references are resolved.                                         */
;*---------------------------------------------------------------------*/
(define (resolve! ast engine env)
   (with-debug 3 'resolve
      (debug-item "ast=" ast)
      (let ((old *unresolved*))
	 (let loop ((ast ast))
	    (set! *unresolved* #f)
	    (let ((ast (do-resolve! ast engine env)))
	       (if *unresolved*
		   (loop ast)
		   (begin
		      (set! *unresolved* old)
		      ast)))))))

;*---------------------------------------------------------------------*/
;*    do-resolve!  ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (do-resolve! ast engine env)
   (if (pair? ast)
       (do-resolve*! ast engine env)
       ast))

;*---------------------------------------------------------------------*/
;*    do-resolve! ::%node ...                                          */
;*---------------------------------------------------------------------*/
(define-method (do-resolve! node::%node engine env)
   (with-access::%node node (body options parent)
      (with-debug 5 'do-resolve::body 
	 (debug-item "node=" (if (markup? node)
				 (markup-markup node)
				 (find-runtime-type node)))
	 (debug-item "body=" (find-runtime-type body))
	 (if (not (eq? parent #unspecified))
	     node
	     (let ((p (assq 'parent env)))
		(set! parent (and (pair? p) (pair? (cdr p)) (cadr p)))
		(if (pair? options)
		    (begin
		       (debug-item "unresolved options=" options)
		       (for-each (lambda (o)
				    (set-car! (cdr o)
					      (do-resolve! (cadr o) engine env)))
				 options)
		       (debug-item "resolved options=" options)))))
	 (set! body (do-resolve! body engine env))
	 node)))

;*---------------------------------------------------------------------*/
;*    do-resolve! ::%container ...                                     */
;*---------------------------------------------------------------------*/
(define-method (do-resolve! node::%container engine env0)
   (with-access::%container node (body options env parent)
      (with-debug 5 'do-resolve::%container
	 (debug-item "markup=" (markup-markup node))
	 (debug-item "body=" (find-runtime-type body))
	 (debug-item "env0=" env0)
	 (debug-item "env=" env)
	 (if (not (eq? parent #unspecified))
	     node
	     (let ((p (assq 'parent env0)))
		(set! parent (and (pair? p) (pair? (cdr p)) (cadr p)))
		(if (pair? options)
		    (let ((e (append `((parent ,node)) env0)))
		       (debug-item "unresolved options=" options)
		       (for-each (lambda (o)
				    (set-car! (cdr o)
					      (do-resolve! (cadr o) engine e)))
				 options)
		       (debug-item "resolved options=" options)))
		(let ((e `((parent ,node) ,@env ,@env0)))
		   (set! body (do-resolve! body engine e))
		   node))))
      ;; return the container
      node))

;*---------------------------------------------------------------------*/
;*    do-resolve! ::%document ...                                      */
;*---------------------------------------------------------------------*/
(define-method (do-resolve! node::%document engine env0)
   (with-access::%document node (env)
      (call-next-method)
      ;; resolve the engine custom
      (let ((env (append `((parent ,node)) env0)))
	 (for-each (lambda (c)
		      (let ((i (car c))
			    (a (cadr c)))
			 (debug-item "custom=" i " " a)
			 (set-car! (cdr c) (do-resolve! a engine env))))
		   (%engine-customs engine)))
      ;; return the container
      node))

;*---------------------------------------------------------------------*/
;*    do-resolve! ::%unresolved ...                                    */
;*---------------------------------------------------------------------*/
(define-method (do-resolve! node::%unresolved engine env)
   (with-debug 5 'do-resolve::%unresolved
      (debug-item "node=" node)
      (with-access::%unresolved node (proc parent loc)
	 (let ((p (assq 'parent env)))
	    (set! parent (and (pair? p) (pair? (cdr p)) (cadr p))))
	 (let ((res (resolve! (proc node engine env) engine env)))
	    (if (ast? res) (%ast-loc-set! res loc))
	    (debug-item "res=" res)
	    (set! *unresolved* #t)
	    res))))

;*---------------------------------------------------------------------*/
;*    do-resolve! ::handle ...                                         */
;*---------------------------------------------------------------------*/
(define-method (do-resolve! node::%handle engine env)
   node)

;*---------------------------------------------------------------------*/
;*    do-resolve*! ...                                                 */
;*---------------------------------------------------------------------*/
(define (do-resolve*! n+ engine env)
   (let loop ((n* n+))
      (cond
	 ((pair? n*)
	  (set-car! n* (do-resolve! (car n*) engine env))
	  (loop (cdr n*)))
	 ((not (null? n*))
	  (skribe-error 'do-resolve "Illegal argument" n*))
	 (else
	  n+))))

;*---------------------------------------------------------------------*/
;*    resolve-children ...                                             */
;*---------------------------------------------------------------------*/
(define (resolve-children n)
   (if (pair? n)
       n
       (list n)))

;*---------------------------------------------------------------------*/
;*    resolve-children* ...                                            */
;*---------------------------------------------------------------------*/
(define (resolve-children* n)
   (cond
      ((pair? n)
       (map resolve-children* n))
      ((%container? n)
       (cons n (resolve-children* (%container-body n))))
      (else
       (list n))))

;*---------------------------------------------------------------------*/
;*    resolve-parent ...                                               */
;*---------------------------------------------------------------------*/
(define (resolve-parent n e)
   (with-debug 5 'resolve-parent
      (debug-item "n=" n)
      (cond
	 ((not (%ast? n))
	  (let ((c (assq 'parent e)))
	     (if (pair? c)
		 (cadr c)
		 n)))
	 ((eq? (%ast-parent n) #unspecified)
	  (skribe-error 'resolve-parent "Orphan node" n))
	 (else
	  (%ast-parent n)))))

;*---------------------------------------------------------------------*/
;*    resolve-search-parent ...                                        */
;*---------------------------------------------------------------------*/
(define (resolve-search-parent n e pred)
   (with-debug 5 'resolve-search-parent
      (debug-item "node=" (find-runtime-type n))
      (debug-item "searching=" pred)
      (let ((p (resolve-parent n e)))
	 (debug-item "parent=" (find-runtime-type p) " "
		     (if (markup? p) (markup-markup p) "???"))
	 (cond
	    ((pred p)
	     p)
	    ((%unresolved? p)
	     p)
	    ((not p)
	     #f)
	    (else
	     (resolve-search-parent p e pred))))))

;*---------------------------------------------------------------------*/
;*    resolve-counter ...                                              */
;*---------------------------------------------------------------------*/
(define (resolve-counter n e cnt val . opt)
   (let ((c (assq (symbol-append cnt '-counter) e)))
      (if (not (pair? c))
	  (if (or (null? opt) (not (car opt)) (null? e))
	      (skribe-error cnt "Orphan node" n)
	      (begin
		 (set-cdr! (last-pair e)
			   (list (list (symbol-append cnt '-counter) 0)
				 (list (symbol-append cnt '-env) '())))
		 (resolve-counter n e cnt val)))
	  (let* ((num (cadr c))
		 (nval (if (integer? val)
			   val
			   (+ 1 num))))
	     (let ((c2 (assq (symbol-append cnt '-env) e)))
		(set-car! (cdr c2) (cons (resolve-parent n e) (cadr c2))))
	     (cond
		((integer? val)
		 (set-car! (cdr c) val)
		 (car val))
		((not val)
		 val)
		(else
		 (set-car! (cdr c) (+ 1 num))
		 (+ 1 num)))))))

;*---------------------------------------------------------------------*/
;*    resolve-ident ...                                                */
;*---------------------------------------------------------------------*/
(define (resolve-ident ident markup n e)
   (with-debug 4 'resolve-ident
      (debug-item "ident=" ident)
      (debug-item "markup=" markup)
      (debug-item "n=" (if (markup? n) (markup-markup n) n))
      (if (not (string? ident))
	  (skribe-type-error 'resolve-ident
			     "Illegal ident"
			     ident
			     "string")
	  (let ((mks (find-markups ident)))
	     (and mks
		  (if (not markup)
		      (car mks)
		      (let loop ((mks mks))
			 (cond
			    ((null? mks)
			     #f)
			    ((is-markup? (car mks) markup)
			     (car mks))
			    (else
			     (loop (cdr mks)))))))))))
