;*=====================================================================*/
;*    serrano/prgm/project/skribe/src/common/lib.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 10 11:57:54 2003                          */
;*    Last change :  Wed Oct 27 12:16:40 2004 (eg)                     */
;*    Copyright   :  2003-04 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Scheme independent lib part.                                 */
;*    -------------------------------------------------------------    */
;*    Implementation: @label lib@                                      */
;*    bigloo: @path ../bigloo/lib.bgl@                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    engine-custom-add! ...                                           */
;*---------------------------------------------------------------------*/
(define (engine-custom-add! e id val)
   (let ((old (engine-custom e id)))
      (if (unspecified? old)
	  (engine-custom-set! e id (list val))
	  (engine-custom-set! e id (cons val old)))))

;*---------------------------------------------------------------------*/
;*    find-markup-ident ...                                            */
;*---------------------------------------------------------------------*/
(define (find-markup-ident ident)
   (let ((r (find-markups ident)))
      (if (or (pair? r) (null? r))
	  r
	  '())))

;*---------------------------------------------------------------------*/
;*    container-search-down ...                                        */
;*---------------------------------------------------------------------*/
(define (container-search-down pred obj)
   (with-debug 4 'container-search-down
      (debug-item "obj=" (find-runtime-type obj))
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
	     '())))))
       
;*---------------------------------------------------------------------*/
;*    search-down ...                                                  */
;*---------------------------------------------------------------------*/
(define (search-down pred obj)
   (with-debug 4 'search-down
      (debug-item "obj=" (find-runtime-type obj))
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
	     '())))))
       
;*---------------------------------------------------------------------*/
;*    find-down ...                                                    */
;*---------------------------------------------------------------------*/
(define (find-down pred obj)
   (with-debug 4 'find-down
      (debug-item "obj=" (find-runtime-type obj))
      (let loop ((obj obj))
	 (cond
	    ((pair? obj)
	     (apply append (map (lambda (o) (loop o)) obj)))
	    ((markup? obj)
	     (debug-item "loop=" (find-runtime-type obj)
			 " " (markup-ident obj))
	     (if (pred obj)
		 (list (cons obj (loop (markup-body obj))))
		 '()))
	    (else
	     (if (pred obj)
		 (list obj)
		 '()))))))
       
;*---------------------------------------------------------------------*/
;*    find1-down ...                                                   */
;*---------------------------------------------------------------------*/
(define (find1-down pred obj)
   (with-debug 4 'find1-down
      (let loop ((obj obj)
		 (stack '()))
	 (debug-item "obj=" (find-runtime-type obj)
		     " " (if (markup? obj) (markup-markup obj) "???")
		     " " (if (markup? obj) (markup-ident obj) ""))
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
	     #f)))))

;*---------------------------------------------------------------------*/
;*    find-up ...                                                      */
;*---------------------------------------------------------------------*/
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

;*---------------------------------------------------------------------*/
;*    find1-up ...                                                     */
;*---------------------------------------------------------------------*/
(define (find1-up pred obj)
   (let loop ((obj obj))
      (cond
	 ((not (ast? obj))
	  #f)
	 ((pred obj)
	  obj)
	 (else
	  (loop (ast-parent obj))))))

;*---------------------------------------------------------------------*/
;*    ast-document ...                                                 */
;*---------------------------------------------------------------------*/
(define (ast-document m)
   (find1-up document? m))

;*---------------------------------------------------------------------*/
;*    ast-chapter ...                                                  */
;*---------------------------------------------------------------------*/
(define (ast-chapter m)
   (find1-up (lambda (n) (is-markup? n 'chapter)) m))

;*---------------------------------------------------------------------*/
;*    ast-section ...                                                  */
;*---------------------------------------------------------------------*/
(define (ast-section m)
   (find1-up (lambda (n) (is-markup? n 'section)) m))

;*---------------------------------------------------------------------*/
;*    the-body ...                                                     */
;*    -------------------------------------------------------------    */
;*    Filter out the options                                           */
;*---------------------------------------------------------------------*/
(define (the-body opt+)
   (let loop ((opt* opt+)
	      (res '()))
      (cond
	 ((null? opt*)
	  (reverse! res))
	 ((not (pair? opt*))
	  (skribe-error 'the-body "Illegal body" opt*))
	 ((keyword? (car opt*))
	  (if (null? (cdr opt*))
	      (skribe-error 'the-body "Illegal option" (car opt*))
	      (loop (cddr opt*) res)))
	 (else
	  (loop (cdr opt*) (cons (car opt*) res))))))

;*---------------------------------------------------------------------*/
;*    the-options ...                                                  */
;*    -------------------------------------------------------------    */
;*    Returns an list made of options. The OUT argument contains       */
;*    keywords that are filtered out.                                  */
;*---------------------------------------------------------------------*/
(define (the-options opt+ . out)
   (let loop ((opt* opt+)
	      (res '()))
      (cond
	 ((null? opt*)
	  (reverse! res))
	 ((not (pair? opt*))
	  (skribe-error 'the-options "Illegal options" opt*))
	 ((keyword? (car opt*))
	  (cond
	     ((null? (cdr opt*))
	      (skribe-error 'the-options "Illegal option" (car opt*)))
	     ((memq (car opt*) out)
	      (loop (cdr opt*) res))
	     (else
	      (loop (cdr opt*)
		    (cons (list (car opt*) (cadr opt*)) res)))))
	 (else
	  (loop (cdr opt*) res)))))

;*---------------------------------------------------------------------*/
;*    list-split ...                                                   */
;*---------------------------------------------------------------------*/
(define (list-split l num . fill)
   (let loop ((l l)
	      (i 0)
	      (acc '())
	      (res '()))
      (cond
	 ((null? l)
	  (reverse! (cons (if (or (null? fill) (= i num))
			      (reverse! acc)
			      (append! (reverse! acc)
				       (make-list (- num i) (car fill))))
			  res)))
	 ((= i num)
	  (loop l
		0
		'()
		(cons (reverse! acc) res)))
	 (else
	  (loop (cdr l)
		(+ i 1)
		(cons (car l) acc)
		res)))))

