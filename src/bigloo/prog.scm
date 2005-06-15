;*=====================================================================*/
;*    serrano/prgm/project/skribe/src/bigloo/prog.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 27 09:14:28 2003                          */
;*    Last change :  Tue Oct  7 15:07:48 2003 (serrano)                */
;*    Copyright   :  2003 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The Skribe prog bigloo implementation                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module skribe_prog
    
   (include "new.sch")
   
   (import  skribe_types
	    skribe_lib
	    skribe_resolve
	    skribe_eval
	    skribe_api)
   
   (export (make-prog-body ::obj ::obj ::obj ::obj)
	   (resolve-line ::bstring)))

;*---------------------------------------------------------------------*/
;*    *lines* ...                                                      */
;*---------------------------------------------------------------------*/
(define *lines* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    make-line-mark ...                                               */
;*---------------------------------------------------------------------*/
(define (make-line-mark m lnum b)
   (let* ((ls (integer->string lnum))
	  (n (list (mark ls) b)))
      (hashtable-put! *lines* m n)
      n))

;*---------------------------------------------------------------------*/
;*    resolve-line ...                                                 */
;*---------------------------------------------------------------------*/
(define (resolve-line id)
   (hashtable-get *lines* id))

;*---------------------------------------------------------------------*/
;*    extract-string-mark ...                                          */
;*---------------------------------------------------------------------*/
(define (extract-string-mark line mark regexp)
   (let ((m (pregexp-match regexp line)))
      (if (pair? m)
	  (values (substring (car m)
			     (string-length mark)
			     (string-length (car m)))
		  (pregexp-replace regexp line ""))
	  (values #f line))))
   
;*---------------------------------------------------------------------*/
;*    extract-mark ...                                                 */
;*    -------------------------------------------------------------    */
;*    Extract the prog mark from a line.                               */
;*---------------------------------------------------------------------*/
(define (extract-mark line mark regexp)
   (cond
      ((not regexp)
       (values #f line))
      ((string? line)
       (extract-string-mark line mark regexp))
      ((pair? line)
       (let loop ((ls line)
		  (res '()))
	  (if (null? ls)
	      (values #f line)
	      (multiple-value-bind (m l)
		 (extract-mark (car ls) mark regexp)
		 (if (not m)
		     (loop (cdr ls) (cons l res))
		     (values m (append (reverse! res) (cons l (cdr ls)))))))))
      ((%node? line)
       (multiple-value-bind (m l)
	  (extract-mark (%node-body line) mark regexp)
	  (if (not m)
	      (values #f line)
	      (begin
		 (%node-body-set! line l)
		 (values m line)))))
      (else
       (values #f line))))

;*---------------------------------------------------------------------*/
;*    split-line ...                                                   */
;*---------------------------------------------------------------------*/
(define (split-line line)
   (cond
      ((string? line)
       (let ((l (string-length line)))
	  (let loop ((r1 0)
		     (r2 0)
		     (res '()))
	     (cond
		((=fx r2 l)
		 (if (=fx r1 r2)
		     (reverse! res)
		     (reverse! (cons (substring line r1 r2) res))))
		((char=? (string-ref line r2) #\Newline)
		 (loop (+fx r2 1)
		       (+fx r2 1)
		       (if (=fx r1 r2)
			   (cons 'eol res)
			   (cons* 'eol (substring line r1 r2) res))))
		(else
		 (loop r1
		       (+fx r2 1)
		       res))))))
      ((pair? line)
       (let loop ((ls line)
		  (res '()))
	  (if (null? ls)
	      res
	      (loop (cdr ls) (append res (split-line (car ls)))))))
      (else
       (list line))))

;*---------------------------------------------------------------------*/
;*    flat-lines ...                                                   */
;*---------------------------------------------------------------------*/
(define (flat-lines lines)
   (apply append (map split-line lines)))

;*---------------------------------------------------------------------*/
;*    collect-lines ...                                                */
;*---------------------------------------------------------------------*/
(define (collect-lines lines)
   (let loop ((lines (flat-lines lines))
	      (res '())
	      (tmp '()))
      (cond
	 ((null? lines)
	  (reverse! (cons (reverse! tmp) res)))
	 ((eq? (car lines) 'eol)
	  (cond
	     ((null? (cdr lines))
	      (reverse! (cons (reverse! tmp) res)))
	     ((and (null? res) (null? tmp))
	      (loop (cdr lines)
		    res
		    '()))
	     (else
	      (loop (cdr lines)
		    (cons (reverse! tmp) res)
		    '()))))
	 (else
	  (loop (cdr lines)
		res
		(cons (car lines) tmp))))))
      
;*---------------------------------------------------------------------*/
;*    make-prog-body ...                                               */
;*---------------------------------------------------------------------*/
(define (make-prog-body src lnum-init ldigit mark)
   (define (int->str i rl)
      (let* ((s (integer->string i))
	     (l (string-length s)))
	 (if (= l rl)
	     s
	     (string-append (make-string (- rl l) #\space) s))))
   (let* ((regexp (and mark
		       (format "~a[-a-zA-Z_][-0-9a-zA-Z_]+"
			       (pregexp-quote mark))))
	  (src (cond
		  ((not (pair? src)) (list src))
		  ((and (pair? (car src)) (null? (cdr src))) (car src))
		  (else src)))
	  (lines (collect-lines src))
	  (lnum (if (integer? lnum-init) lnum-init 1))
	  (s (integer->string (+fx (if (integer? ldigit)
				       (max lnum (expt 10 (-fx ldigit 1)))
				       lnum)
				   (length lines))))
	  (cs (string-length s)))
      (let loop ((lines lines)
		 (lnum lnum)
		 (res '()))
	 (if (null? lines)
	     (reverse! res)
	     (multiple-value-bind (m l)
		(extract-mark (car lines) mark regexp)
		(let ((n (new markup
			    (markup '&prog-line)
			    (ident (and lnum-init (int->str lnum cs)))
			    (body (if m (make-line-mark m lnum l) l)))))
		   (loop (cdr lines)
			 (+ lnum 1)
			 (cons n res))))))))
