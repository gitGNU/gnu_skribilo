;*=====================================================================*/
;*    serrano/prgm/project/scribe/scribetext/table.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Nov  5 06:41:44 2001                          */
;*    Last change :  Fri Nov 23 11:04:56 2001 (serrano)                */
;*    Copyright   :  2001 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Table handling                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __scribetext_table

   (library scribeapi)

   (import __scribetext_justify)
   
   (export (table->ascii ::%table ::procedure)))

;*---------------------------------------------------------------------*/
;*    table->ascii ...                                                 */
;*---------------------------------------------------------------------*/
(define (table->ascii obj scribe->ascii)
   (with-access::%table obj (width nbcols rows)
      (let* ((nc::long nbcols)
	     (awidth (-fx (cond
			     ((fixnum? width)
			      width)
			     ((flonum? width)
			      (inexact->exact (* width (justification-width))))
			     (else
			      (justification-width)))
			  ;; remove one characters per columns separation
			  (-fx nc 1)))
	     (colswidth::vector (rows-requested-sizes obj nc awidth))
	     (lcolswidth::pair-nil (vector->list colswidth))
	     (nb-def-cols (length (filter (lambda (s) (= s 0)) lcolswidth)))
	     (defcolwidth (if (= nb-def-cols 0)
			      0
			      (inexact->exact
			       (/ (- awidth (apply + lcolswidth))
				  nb-def-cols)))))
	 ;; we have computed the requested col sizes and the global size
	 ;; of the table, we now compute the exact col sizes
	 (let loop ((i 0)
		    (sum 0))
	    (if (=fx i nc)
		;; we adjust (because of modulo approx) the last column
		(if (not (=fx sum awidth))
		    (vector-set! colswidth
				 (-fx i 1)
				 (+fx (vector-ref colswidth (-fx i 1))
				      (-fx awidth sum))))
		(if (= (vector-ref colswidth i) 0)
		    (begin
		       (vector-set! colswidth i defcolwidth)
		       (loop (+fx i 1) (+fx sum defcolwidth)))
		    (loop (+fx i 1) (+fx sum (vector-ref colswidth i))))))
	 ;; at that point colswidth contains the width of each colums.
	 (let ((lcolswidth (vector->list colswidth))
	       (hrows (map (lambda (r)
			      (table-row-format r nc colswidth scribe->ascii))
			   rows)))
	    (let ((vrows (map (lambda (r)
				 (let ((lines (apply max
						     (map (lambda (c)
							     (length (cdr c)))
							  r))))
				    (smallest-map (lambda (c cw)
						     (table-cell-vformat
						      c lines cw))
						  r lcolswidth)))
			      hrows)))
	       ;; we are done, all cells are formated, we can display the
	       ;; whole table now
	       (for-each (lambda (r)
			    (apply for-each
				   (lambda l
				      (for-each output-token l)
				      (output-newline))
				   r))
			 vrows))))))

;*---------------------------------------------------------------------*/
;*    table-row-format ...                                             */
;*---------------------------------------------------------------------*/
(define (table-row-format row::%table-row nbcols::int colswidth scribe->ascii)
   (define (cell-width cnum cspan)
      (let ((v (do ((i cnum (+fx i 1))
		    (w 0 (+fx w (vector-ref colswidth i))))
		   ((=fx i (+fx cnum cspan)) w))))
	 (+fx v (-fx cspan 1))))
   (with-access::%table-row row (cells)
      (let loop ((cells cells)
		 (cnum 0)
		 (res '()))
	 (cond
	    ((pair? cells)
	     (let* ((cell (car cells))
		    (cspan (%table-cell-colspan cell))
		    (cell-width (cell-width cnum cspan)))
		(loop (cdr cells)
		      (+fx cnum cspan)
		      (cons (table-cell-format cell
					       cell-width
					       scribe->ascii)
			    res))))
	    ((=fx cnum nbcols)
	     (reverse! res))
	    (else
	     (let ((eline (make-string (vector-ref colswidth cnum) #\space)))
		(loop cells
		      (+fx cnum 1)
		      (cons (cons (instantiate::%table-data
				     (body ""))
				  (list eline)) res))))))))

;*---------------------------------------------------------------------*/
;*    table-cell-format ::table-cell ...                               */
;*    -------------------------------------------------------------    */
;*    Output a table cell according to its WIDTH. At that time we have */
;*    to ignore the width that is specified in the cell because we     */
;*    have processed a global computation of column width. For the     */
;*    reason, we ignore the cell COLSPAN.                              */
;*    -------------------------------------------------------------    */
;*    This is a bit tricky because we can't display the cell as        */
;*    we process it. We have to store in data structures the lines     */
;*    representing the cell and in a second time, when all the table   */
;*    is processed we display the cells. For that reason, we can't     */
;*    use WITH-JUSTIFICATION form.                                     */
;*---------------------------------------------------------------------*/
(define (table-cell-format obj::%table-cell width scribe->ascii)
   (with-access::%table-cell obj (align body)
      (cons obj
	    (let ((fmt (with-justification/noflush
			(make-justifier width
					(case align
					   ((left)
					    'left)
					   ((right)
					    'right)
					   ((center)
					    'center)
					   (else
					    (if (not align)
						'left
						(error "table-cell-format"
						       "Illegal horizontal alignment"
						       align)))))
			(lambda () (scribe->ascii body)))))
	       (if (null? fmt)
		   '("")
		   fmt)))))

;*---------------------------------------------------------------------*/
;*    table-cell-vformat ...                                           */
;*    -------------------------------------------------------------    */
;*    We have formated the table cell horizontally. Now we have to     */
;*    consider the VALIGN fields. That is, we complete the cell        */
;*    formating with empty blank line around the cell.                 */
;*---------------------------------------------------------------------*/
(define (table-cell-vformat::pair p::pair nblines::int cwidth)
   (define (make-filler-lines num)
      (vector->list (make-vector num (make-string cwidth #\Space))))
   (let* ((cell (car p))
	  (fmt (cdr p))
	  (lfmt (length fmt)))
      (if (=fx lfmt nblines)
	  fmt
	  (let* ((new (- nblines lfmt))
		 (new/2 (inexact->exact (/ new 2))))
	     (case (%table-cell-valign cell)
		((top)
		 (append fmt (make-filler-lines new)))
		((bottom)
		 (append (make-filler-lines new) fmt))
		(else
		 (append (make-filler-lines new/2)
			 fmt
			 (make-filler-lines (- nblines lfmt new/2)))))))))
      
;*---------------------------------------------------------------------*/
;*    rows-requested-sizes ...                                         */
;*    -------------------------------------------------------------    */
;*    This function scans all the rows of TABLE. It check each         */
;*    cells of the rows in order to find out if there is a             */
;*    specified width for the column the cell belongs to.              */
;*---------------------------------------------------------------------*/
(define (rows-requested-sizes table nbcols twidth)
   (let ((rsizes (make-vector nbcols 0)))
      (for-each (lambda (row)
		   (let loop ((cells (%table-row-cells row))
			      (col 0))
		      (if (pair? cells)
			  (let* ((cell (car cells))
				 (cspan (%table-cell-colspan cell))
				 (swidth (%table-cell-width cell)))
			     (if (number? swidth)
				 (let* ((swidth (if (fixnum? swidth)
						    swidth
						    (inexact->exact
						     (* swidth twidth))))
					(cswidth (/ swidth cspan)))
				    (do ((j 0 (+fx j 1)))
					((=fx j cspan)
					 (loop (cdr cells)
					       (+fx col cspan)))
					(if (< (vector-ref rsizes (+ col j))
					       cswidth)
					    (vector-set! rsizes
							 (+ col j)
							 cswidth))))
				 (loop (cdr cells) (+fx col cspan)))))))
		(%table-rows table))
      rsizes))
				       
;*---------------------------------------------------------------------*/
;*    smallest-map ...                                                 */
;*---------------------------------------------------------------------*/
(define (smallest-map f l1 l2)
   (if (or (null? l1) (null? l2))
       '()
       (cons (f (car l1) (car l2)) (smallest-map f (cdr l1) (cdr l2)))))
