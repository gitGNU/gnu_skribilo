;*=====================================================================*/
;*    serrano/prgm/project/skribe/src/bigloo/api.sch                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jul 21 18:15:25 2003                          */
;*    Last change :  Wed Oct 27 12:43:23 2004 (eg)                     */
;*    Copyright   :  2003-04 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Bigloo macros for the API implementation                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    define-pervasive-macro ...                                       */
;*---------------------------------------------------------------------*/
(define-macro (define-pervasive-macro proto . body)
   `(begin
       (eval '(define-macro ,proto ,@body))
       (define-macro ,proto ,@body)))
 
;*---------------------------------------------------------------------*/
;*    define-markup ...                                                */
;*---------------------------------------------------------------------*/
(define-pervasive-macro (define-markup proto . body)
   (define (s2k symbol)
      (string->keyword (string-append ":" (symbol->string symbol))))
   (if (not (pair? proto))
       (error 'define-markup "Illegal markup definition" proto)
       (let* ((id (car proto))
	      (args (cdr proto))
	      (dargs (dsssl-formals->scheme-formals args error)))
	  `(begin
	      ,(if (and (memq #!key args)
			(memq '&skribe-eval-location args))
		   `(define-expander ,id
		       (lambda (x e)
			  (append 
			   (cons ',id (map (lambda (x) (e x e)) (cdr x)))
			   (list :&skribe-eval-location
				 '(skribe-eval-location)))))
		   #unspecified)
	      (define ,(cons id dargs)
		 ,(make-dsssl-function-prelude proto
					       args `(begin ,@body)
					       error s2k))))))

;*---------------------------------------------------------------------*/
;*    define-simple-markup ...                                         */
;*---------------------------------------------------------------------*/
(define-pervasive-macro (define-simple-markup markup)
   `(define-markup (,markup #!rest opts #!key ident class loc)
       (new markup
	  (markup ',markup)
	  (ident (or ident (symbol->string (gensym ',markup))))
	  (loc loc)
	  (class class)
	  (required-options '())
	  (options (the-options opts :ident :class :loc))
	  (body (the-body opts)))))
		  
;*---------------------------------------------------------------------*/
;*    define-simple-container ...                                      */
;*---------------------------------------------------------------------*/
(define-pervasive-macro (define-simple-container markup)
   `(define-markup (,markup #!rest opts #!key ident class loc)
       (new container
	  (markup ',markup)
	  (ident (or ident (symbol->string (gensym ',markup))))
	  (loc loc)
	  (class class)
	  (required-options '())
	  (options (the-options opts :ident :class :loc))
	  (body (the-body opts)))))
		  
;*---------------------------------------------------------------------*/
;*    define-processor-markup ...                                      */
;*---------------------------------------------------------------------*/
(define-pervasive-macro (define-processor-markup proc)
   `(define-markup (,proc #!rest opts)
       (new processor
	  (engine (find-engine ',proc))
	  (body (the-body opts))
	  (options (the-options opts)))))

;*---------------------------------------------------------------------*/
;*    new (at runtime)                                                 */
;*---------------------------------------------------------------------*/
(eval '(define-macro (new id . inits)
	  (cons (symbol-append 'new- id)
		(map (lambda (i)
			(list 'list (list 'quote (car i)) (cadr i)))
		     inits))))
