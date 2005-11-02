;*=====================================================================*/
;*    serrano/prgm/project/skribe/src/bigloo/verify.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jul 25 09:54:55 2003                          */
;*    Last change :  Thu Sep 23 19:58:01 2004 (serrano)                */
;*    Copyright   :  2003-04 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Skribe verification stage                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module skribe_verify

   (include "debug.sch")
   
   (import  skribe_types
	    skribe_lib
	    skribe_engine
	    skribe_writer
	    skribe_eval)
   
   (export  (generic verify ::obj ::%engine)))

;*---------------------------------------------------------------------*/
;*    check-required-options ...                                       */
;*---------------------------------------------------------------------*/
(define (check-required-options n::%markup w::%writer e::%engine)
   (with-access::%markup n (required-options)
      (with-access::%writer w (ident options verified?)
	 (or verified?
	     (eq? options 'all)
	     (begin
		(for-each (lambda (o)
			     (if (not (memq o options))
				 (skribe-error (%engine-ident e)
					       (format "Option unsupported: ~a, supported options: ~a" o options)
					       n)))
			  required-options)
		(set! verified? #t))))))

;*---------------------------------------------------------------------*/
;*    check-options ...                                                */
;*    -------------------------------------------------------------    */
;*    Only keywords are checked, symbols are voluntary left unchecked. */
;*---------------------------------------------------------------------*/
(define (check-options eo*::pair-nil m::%markup e::%engine)
   (with-debug 6 'check-options
      (debug-item "markup=" (%markup-markup m))
      (debug-item "options=" (%markup-options m))
      (debug-item "eo*=" eo*)
      (for-each (lambda (o2)
		   (for-each (lambda (o)
				(if (and (keyword? o)
					 (not (eq? o :&skribe-eval-location))
					 (not (memq o eo*)))
				    (skribe-warning/ast
				     3
				     m
				     'verify
				     (format "Engine `~a' does not support markup `~a' option `~a' -- ~a"
					     (%engine-ident e)
					     (%markup-markup m)
					     o
					     (markup-option m o)))))
			     o2))
		(%markup-options m))))

;*---------------------------------------------------------------------*/
;*    verify :: ...                                                    */
;*---------------------------------------------------------------------*/
(define-generic (verify node e)
   (if (pair? node)
       (for-each (lambda (n) (verify n e)) node))
   node)

;*---------------------------------------------------------------------*/
;*    verify ::%processor ...                                          */
;*---------------------------------------------------------------------*/
(define-method (verify n::%processor e)
   (with-access::%processor n (combinator engine body)
      (verify body (processor-get-engine combinator engine e))
      n))

;*---------------------------------------------------------------------*/
;*    verify ::%node ...                                               */
;*---------------------------------------------------------------------*/
(define-method (verify node::%node e)
   (with-access::%node node (body options)
      (verify body e)
      (for-each (lambda (o) (verify (cadr o) e)) options)
      node))

;*---------------------------------------------------------------------*/
;*    verify ::%markup ...                                             */
;*---------------------------------------------------------------------*/
(define-method (verify node::%markup e)
   (with-debug 5 'verify::%markup
      (debug-item "node=" (%markup-markup node))
      (debug-item "options=" (%markup-options node))
      (debug-item "e=" (%engine-ident e))
      (call-next-method)
      (let ((w (lookup-markup-writer node e)))
	 (if (%writer? w)
	     (begin
		(check-required-options node w e)
		(if (pair? (%writer-options w))
		    (check-options (%writer-options w) node e))
		(let ((validate (%writer-validate w)))
		   (when (procedure? validate)
		      (unless (validate node e)
			 (skribe-warning
			  1
			  node
			  (format "Node `~a' forbidden here by ~a engine"
				  (markup-markup node)
				  (engine-ident e))
			  node)))))))
      ;; return the node
      node))

;*---------------------------------------------------------------------*/
;*    verify ::%document ...                                           */
;*---------------------------------------------------------------------*/
(define-method (verify node::%document e)
   (call-next-method)
   ;; verify the engine custom
   (for-each (lambda (c)
		(let ((i (car c))
		      (a (cadr c)))
		   (set-car! (cdr c) (verify a e))))
	     (%engine-customs e))
   ;; return the node
   node)

;*---------------------------------------------------------------------*/
;*    verify ::%handle ...                                             */
;*---------------------------------------------------------------------*/
(define-method (verify node::%handle e)
   node)

