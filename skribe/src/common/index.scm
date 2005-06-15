;*=====================================================================*/
;*    serrano/prgm/project/skribe/src/common/index.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Aug 24 08:01:45 2003                          */
;*    Last change :  Wed Feb  4 14:58:05 2004 (serrano)                */
;*    Copyright   :  2003-04 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Skribe indexes                                                   */
;*    -------------------------------------------------------------    */
;*    Implementation: @label index@                                    */
;*    bigloo: @path ../bigloo/index.bgl@                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    index? ...                                                       */
;*---------------------------------------------------------------------*/
(define (index? obj)
   (hashtable? obj))

;*---------------------------------------------------------------------*/
;*    *index-table* ...                                                */
;*---------------------------------------------------------------------*/
(define *index-table* #f)

;*---------------------------------------------------------------------*/
;*    make-index-table ...                                             */
;*---------------------------------------------------------------------*/
(define (make-index-table ident)
   (make-hashtable))

;*---------------------------------------------------------------------*/
;*    default-index ...                                                */
;*---------------------------------------------------------------------*/
(define (default-index)
   (if (not *index-table*)
       (set! *index-table* (make-index-table "default-index")))
   *index-table*)

;*---------------------------------------------------------------------*/
;*    resolve-the-index ...                                            */
;*---------------------------------------------------------------------*/
(define (resolve-the-index loc i c indexes split char-offset header-limit col)
   ;; fetch the descriminating index name letter
   (define (index-ref n)
      (let ((name (markup-option n 'name)))
	 (if (>= char-offset (string-length name))
	     (skribe-error 'the-index "char-offset out of bound" char-offset)
	     (string-ref name char-offset))))
   ;; sort a bucket of entries (the entries in a bucket share there name)
   (define (sort-entries-bucket ie)
      (sort ie 
	    (lambda (i1 i2)
	       (or (not (markup-option i1 :note))
		   (markup-option i2 :note)))))
   ;; accumulate all the entries starting with the same letter
   (define (letter-references refs)
      (let ((letter (index-ref (car (car refs)))))
	 (let loop ((refs refs)
		    (acc '()))
	    (if (or (null? refs)
		    (not (char-ci=? letter (index-ref (car (car refs))))))
		(values (char-upcase letter) acc refs)
		(loop (cdr refs) (cons (car refs) acc))))))
   ;; merge the buckets that comes from different index tables
   (define (merge-buckets buckets)
      (if (null? buckets)
	  '()
	  (let loop ((buckets buckets)
		     (res '()))
	     (cond
		((null? (cdr buckets))
		 (reverse! (cons (car buckets) res)))
		((string=? (markup-option (car (car buckets)) 'name)
			   (markup-option (car (cadr buckets)) 'name))
		 ;; we merge
		 (loop (cons (append (car buckets) (cadr buckets))
			     (cddr buckets))
		       res))
		(else
		 (loop (cdr buckets)
		       (cons (car buckets) res)))))))
   (let* ((entries (apply append (map hashtable->list indexes)))
	  (sorted (map sort-entries-bucket
		       (merge-buckets
			(sort entries
			      (lambda (e1 e2)
				 (string-ci<?
				  (markup-option (car e1) 'name)
				  (markup-option (car e2) 'name))))))))
      (if (and (not split) (< (apply + (map length sorted)) header-limit))
	  (new markup
	     (markup '&the-index)
	     (loc loc)
	     (ident i)
	     (class c)
	     (options `((:column ,col)))
	     (body sorted))
	  (let loop ((refs sorted)
		     (lrefs '())
		     (body '()))
	     (if (null? refs)
		 (new markup
		    (markup '&the-index)
		    (loc loc)
		    (ident i)
		    (class c)
		    (options `((:column ,col)
			       (header ,(new markup
					   (markup '&the-index-header)
					   (loc loc)
					   (body (reverse! lrefs))))))
		    (body (reverse! body)))
		 (call-with-values
		    (lambda () (letter-references refs))
		    (lambda (l lr next-refs)
		       (let* ((s (string l))
			      (m (mark (symbol->string (gensym s)) :text s))
			      (h (new handle (loc loc) (ast m)))
			      (r (ref :handle h :text s)))
			  (ast-loc-set! m loc)
			  (ast-loc-set! r loc)
			  (loop next-refs
				(cons r lrefs)
				(append lr (cons m body)))))))))))

