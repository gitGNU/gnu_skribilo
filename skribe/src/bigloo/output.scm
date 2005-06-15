;*=====================================================================*/
;*    serrano/prgm/project/skribe/src/bigloo/output.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 23 12:48:11 2003                          */
;*    Last change :  Wed Feb  4 10:33:19 2004 (serrano)                */
;*    Copyright   :  2003-04 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Skribe engine                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module skribe_output
   
   (include "debug.sch")
   
   (import  skribe_types
	    skribe_lib
	    skribe_engine
	    skribe_writer
	    skribe_eval)

   (export  (output ::obj ::%engine . w)))

;*---------------------------------------------------------------------*/
;*    output ...                                                       */
;*---------------------------------------------------------------------*/
(define (output node e . writer)
   (with-debug 3 'output
      (debug-item "node=" node " " (if (markup? node) (markup-markup node) ""))
      (debug-item "writer=" writer)
      (if (pair? writer)
	  (cond
	     ((%writer? (car writer))
	      (out/writer node e (car writer)))
	     ((not (car writer))
	      (skribe-error 'output 
			    (format "Illegal `~a' user writer" (%engine-ident e))
			    (if (markup? node) (%markup-markup node) node)))
	     (else
	      (skribe-error 'output "Illegal user writer" (car writer))))
	  (out node e))))
       
;*---------------------------------------------------------------------*/
;*    out/writer ...                                                   */
;*---------------------------------------------------------------------*/
(define (out/writer n e w)
   (with-debug 5 'out/writer
      (debug-item "n=" (find-runtime-type n)
		  " " (if (markup? n) (markup-markup n) ""))
      (debug-item "e=" (%engine-ident e))
      (debug-item "w=" (%writer-ident w))
      (if (%writer? w)
	  (with-access::%writer w (before action after)
	     (invoke before n e)
	     (invoke action n e)
	     (invoke after n e)))))
   
;*---------------------------------------------------------------------*/
;*    out ...                                                          */
;*---------------------------------------------------------------------*/
(define-generic (out node e::%engine)
   (cond
      ((pair? node)
       (out* node e))
      ((string? node)
       (let ((f (%engine-filter e)))
	  (if (procedure? f)
	      (display (f node))
	      (display node))))
      ((number? node)
       (display node))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    out ::%processor ...                                             */
;*---------------------------------------------------------------------*/
(define-method (out n::%processor e::%engine)
   (with-access::%processor n (combinator engine body procedure)
      (let ((newe (processor-get-engine combinator engine e)))
	 (out (procedure body newe) newe))))

;*---------------------------------------------------------------------*/
;*    out ::%command ...                                               */
;*---------------------------------------------------------------------*/
(define-method (out node::%command e::%engine)
   (with-access::%command node (fmt body)
      (let ((lb (length body))
	    (lf (string-length fmt)))
	 (define (loops i n)
	    (if (= i lf)
		(begin
		   (if (> n 0)
		       (if (<= n lb)
			   (output (list-ref body (- n 1)) e)
			   (skribe-error '!
					 "Too few arguments provided"
					 node)))
		   lf)
		(let ((c (string-ref fmt i)))
		   (cond
		      ((char=? c #\$)
		       (display "$")
		       (+ 1 i))
		      ((not (char-numeric? c))
		       (cond
			  ((= n 0)
			   i)
			  ((<= n lb)
			   (output (list-ref body (- n 1)) e)
			   i)
			  (else
			   (skribe-error '!
					 "Too few arguments provided"
					 node))))
		      (else
		       (loops (+ i 1)
			      (+ (- (char->integer c)
				    (char->integer #\0))
				 (* 10 n))))))))
	 (let loop ((i 0))
	    (cond
	       ((= i lf)
		#f)
	       ((not (char=? (string-ref fmt i) #\$))
		(display (string-ref fmt i))
		(loop (+ i 1)))
	       (else
		(loop (loops (+ i 1) 0))))))))

;*---------------------------------------------------------------------*/
;*    out ::%handle ...                                                */
;*---------------------------------------------------------------------*/
(define-method (out node::%handle e::%engine)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    out ::%unresolved ...                                            */
;*---------------------------------------------------------------------*/
(define-method (out node::%unresolved e::%engine)
   (error 'output "Orphan unresolved" node))

;*---------------------------------------------------------------------*/
;*    out ::%markup ...                                                */
;*---------------------------------------------------------------------*/
(define-method (out node::%markup e::%engine)
   (let ((w (lookup-markup-writer node e)))
      (if (writer? w)
	  (out/writer node e w)
	  (output (%markup-body node) e))))

;*---------------------------------------------------------------------*/
;*    out* ...                                                         */
;*---------------------------------------------------------------------*/
(define (out* n+ e)
   (let loop ((n* n+))
      (cond
	 ((pair? n*)
	  (out (car n*) e)
	  (loop (cdr n*)))
	 ((not (null? n*))
	  (error 'output "Illegal argument" n*)))))

       
