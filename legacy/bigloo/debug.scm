;*=====================================================================*/
;*    serrano/prgm/project/skribe/src/bigloo/debug.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jun 11 10:01:47 2003                          */
;*    Last change :  Thu Oct 28 21:33:00 2004 (eg)                     */
;*    Copyright   :  2003 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Simple debug facilities                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module skribe_debug

   (export *skribe-debug*
	   *skribe-debug-symbols*
	   *skribe-debug-color*

	   (skribe-debug::int)
	   (debug-port::output-port . ::obj)
	   (debug-margin::bstring)
	   (debug-color::bstring ::int . ::obj)
	   (debug-bold::bstring . ::obj)
	   (debug-string ::obj)
	   (debug-item . ::obj)
	   
	   (%with-debug ::obj ::obj ::procedure)))

;*---------------------------------------------------------------------*/
;*    *skribe-debug* ...                                               */
;*---------------------------------------------------------------------*/
(define *skribe-debug* 0)

;*---------------------------------------------------------------------*/
;*    *skribe-debug-symbols* ...                                       */
;*---------------------------------------------------------------------*/
(define *skribe-debug-symbols* '())

;*---------------------------------------------------------------------*/
;*    *skribe-debug-color* ...                                         */
;*---------------------------------------------------------------------*/
(define *skribe-debug-color* #t)

;*---------------------------------------------------------------------*/
;*    *skribe-debug-item* ...                                          */
;*---------------------------------------------------------------------*/
(define *skribe-debug-item* #f)

;*---------------------------------------------------------------------*/
;*    *debug-port* ...                                                 */
;*---------------------------------------------------------------------*/
(define *debug-port* (current-error-port))

;*---------------------------------------------------------------------*/
;*    *debug-depth* ...                                                */
;*---------------------------------------------------------------------*/
(define *debug-depth* 0)

;*---------------------------------------------------------------------*/
;*    *debug-margin* ...                                               */
;*---------------------------------------------------------------------*/
(define *debug-margin* "")

;*---------------------------------------------------------------------*/
;*    *skribe-margin-debug-level* ...                                  */
;*---------------------------------------------------------------------*/
(define *skribe-margin-debug-level* 0)

;*---------------------------------------------------------------------*/
;*    skribe-debug ...                                                 */
;*---------------------------------------------------------------------*/
(define (skribe-debug)
  *skribe-debug*)

;*---------------------------------------------------------------------*/
;*    debug-port ...                                                   */
;*---------------------------------------------------------------------*/
(define (debug-port . o)
   (cond
      ((null? o)
       *debug-port*)
      ((output-port? (car o))
       (set! *debug-port* o)
       o)
      (else
       (error 'debug-port "Illegal debug port" (car o)))))

;*---------------------------------------------------------------------*/
;*    debug-margin ...                                                 */
;*---------------------------------------------------------------------*/
(define (debug-margin)
   *debug-margin*)

;*---------------------------------------------------------------------*/
;*    debug-color ...                                                  */
;*---------------------------------------------------------------------*/
(define (debug-color col::int . o)
   (with-output-to-string
      (if *skribe-debug-color*
	  (lambda ()
	     (display* "[0m[1;" (+ 31 col) "m")
	     (apply display* o)
	     (display "[0m"))
	  (lambda ()
	     (apply display* o)))))

;*---------------------------------------------------------------------*/
;*    debug-bold ...                                                   */
;*---------------------------------------------------------------------*/
(define (debug-bold . o)
   (apply debug-color -30 o))

;*---------------------------------------------------------------------*/
;*    debug-item ...                                                   */
;*---------------------------------------------------------------------*/
(define (debug-item . args)
   (if (or (>= *skribe-debug* *skribe-margin-debug-level*)
	   *skribe-debug-item*)
       (begin
	  (display (debug-margin) *debug-port*)
	  (display (debug-color (-fx *debug-depth* 1) "- "))
	  (for-each (lambda (a) (display a *debug-port*)) args)
	  (newline *debug-port*))))

;*---------------------------------------------------------------------*/
;*    %with-debug-margin ...                                           */
;*---------------------------------------------------------------------*/
(define (%with-debug-margin margin thunk)
   (let ((om *debug-margin*))
      (set! *debug-depth* (+fx *debug-depth* 1))
      (set! *debug-margin* (string-append om margin))
      (let ((res (thunk)))
	 (set! *debug-depth* (-fx *debug-depth* 1))
	 (set! *debug-margin* om)
	 res)))
      
;*---------------------------------------------------------------------*/
;*    %with-debug ...                                                  */
;*---------------------------------------------------------------------*/
(define (%with-debug lvl lbl thunk)
   (let ((ol *skribe-margin-debug-level*)
	 (oi *skribe-debug-item*))
      (set! *skribe-margin-debug-level* lvl)
      (let ((r (if (or (and (number? lvl) (>= *skribe-debug* lvl))
		       (and (symbol? lbl)
			    (memq lbl *skribe-debug-symbols*)
			    (set! *skribe-debug-item* #t)))
		   (with-output-to-port *debug-port*
		      (lambda ()
			 (display (debug-margin))
			 (display (if (= *debug-depth* 0)
				      (debug-color *debug-depth* "+ " lbl)
				      (debug-color *debug-depth* "--+ " lbl)))
			 (newline)
			 (%with-debug-margin (debug-color *debug-depth* "  |")
					     thunk)))
		   (thunk))))
	 (set! *skribe-debug-item* oi)
	 (set! *skribe-margin-debug-level* ol)
	 r)))

;*---------------------------------------------------------------------*/
;*    debug-string ...                                                 */
;*---------------------------------------------------------------------*/
(define (debug-string o)
   (with-output-to-string
      (lambda ()
	 (write o))))

;*---------------------------------------------------------------------*/
;*    example                                                          */
;*---------------------------------------------------------------------*/
;; (%with-debug 0 'foo1.1
;; 	     (lambda ()
;; 		(debug-item 'foo2.1)
;; 		(debug-item 'foo2.2)
;; 		(%with-debug 0 'foo2.3
;; 			     (lambda ()
;; 				(debug-item 'foo3.1)
;; 				(%with-debug 0 'foo3.2
;; 					     (lambda ()
;; 						(debug-item 'foo4.1)
;; 						(debug-item 'foo4.2)))
;; 				(debug-item 'foo3.3)))
;; 		(debug-item 'foo2.4)))
		
