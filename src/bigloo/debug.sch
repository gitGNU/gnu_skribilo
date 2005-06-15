;*=====================================================================*/
;*    serrano/prgm/project/skribe/src/bigloo/debug.sch                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May 29 06:46:33 2003                          */
;*    Last change :  Tue Nov  2 14:31:45 2004 (serrano)                */
;*    Copyright   :  2003-04 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Simple debug facilities                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    directives                                                       */
;*---------------------------------------------------------------------*/
(directives
   (import skribe_debug))

;*---------------------------------------------------------------------*/
;*    when-debug ...                                                   */
;*---------------------------------------------------------------------*/
(define-macro (when-debug level . exp)
   (if (and (number? *compiler-debug*) (> *compiler-debug* 0))
       `(if (>= *skribe-debug* ,level) (begin ,@exp))
       #unspecified))

;*---------------------------------------------------------------------*/
;*    with-debug ...                                                   */
;*---------------------------------------------------------------------*/
(define-macro (with-debug level lbl . arg*)
   (if (and (number? *compiler-debug*) (> *compiler-debug* 0))
       `(%with-debug ,level ,lbl (lambda () (begin ,@arg*)))
       `(begin ,@arg*)))

;*---------------------------------------------------------------------*/
;*    with-push-trace ...                                              */
;*---------------------------------------------------------------------*/
(define-macro (with-push-trace lbl . arg*)
   (if (and (number? *compiler-debug*) (> *compiler-debug* 0))
       (let ((r (gensym)))
	  `(let ()
	      (c-push-trace ,lbl)
	      (let ((,r ,@arg*))
		 (c-pop-trace)
		 ,r)))
       `(begin ,@arg*)))

;*---------------------------------------------------------------------*/
;*    debug-item ...                                                   */
;*---------------------------------------------------------------------*/
(define-expander debug-item
   (lambda (x e)
      (if (and (number? *compiler-debug*) (> *compiler-debug* 0))
	  `(debug-item ,@(map (lambda (x) (e x e)) (cdr x)))
	  #unspecified)))
