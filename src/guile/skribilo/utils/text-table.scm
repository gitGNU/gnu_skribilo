;;; text-table.scm  --  Producing ASCII tables.
;;;
;;; Copyright 2008  Ludovic Courtès <ludo@gnu.org>
;;; Copyright 2001  Manuel Serrano
;;;
;;;
;;; This file is part of Skribilo.
;;;
;;; Skribilo is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Skribilo is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Skribilo.  If not, see <http://www.gnu.org/licenses/>.

(define-module (skribilo utils text-table)
  :use-module (skribilo ast)
  :use-module (skribilo table)
  :use-module (skribilo utils justify)
  :use-module (skribilo utils syntax)

  :export (table->ascii))

;;; Author: Manuel Serrano, Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; Provide a procedure, `table->ascii', that produces a representation of
;;; the given table using ASCII symbols.
;;;
;;; Code:

(skribilo-module-syntax)


;*---------------------------------------------------------------------*/
;*    table->ascii ...                                                 */
;*---------------------------------------------------------------------*/
(define (table->ascii obj scribe->ascii)
   (let ((width  (markup-option obj :width))
         (nbcols (table-column-count obj))
         (rows   (markup-body obj)))
      (let* ((nc nbcols)
	     (awidth (- (cond
			     ((integer? width)
			      width)
			     ((and (number? width) (inexact? width))
			      (inexact->exact (* width (justification-width))))
			     (else
			      (justification-width)))
			  ;; remove one characters per columns separation
			  (- nc 1)))
	     (colswidth (rows-requested-sizes obj nc awidth))
	     (lcolswidth (map round-to-exact (vector->list colswidth)))
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
	    (if (= i nc)
		;; we adjust (because of modulo approx) the last column
		(if (not (= sum awidth))
		    (vector-set! colswidth
				 (- i 1)
				 (+ (vector-ref colswidth (- i 1))
				      (- awidth sum))))
		(if (= (vector-ref colswidth i) 0)
		    (begin
		       (vector-set! colswidth i defcolwidth)
		       (loop (+ i 1) (+ sum defcolwidth)))
		    (loop (+ i 1) (+ sum (vector-ref colswidth i))))))
	 ;; at that point colswidth contains the width of each colums.
	 (let ((lcolswidth (map round-to-exact (vector->list colswidth)))
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
(define (table-row-format row nbcols colswidth scribe->ascii)
   (define (cell-width cnum cspan)
      (let ((v (do ((i cnum (+ i 1))
		    (w 0 (+ w (round-to-exact (vector-ref colswidth i)))))
		   ((= i (+ cnum cspan)) w))))
	 (+ v (- cspan 1))))

   (let ((cells (markup-body row)))
      (let loop ((cells cells)
		 (cnum 0)
		 (res '()))
	 (cond
	    ((pair? cells)
	     (let* ((cell (car cells))
		    (cspan (markup-option cell :colspan))
		    (cell-width (cell-width cnum cspan)))
		(loop (cdr cells)
		      (+ cnum cspan)
		      (cons (table-cell-format cell
					       cell-width
					       scribe->ascii)
			    res))))
	    ((= cnum nbcols)
	     (reverse! res))
	    (else
	     (let ((eline (make-string (vector-ref colswidth cnum) #\space)))
		(loop cells
		      (+ cnum 1)
		      (cons (cons #f ;; XXX: It's unclear why we need it
				  (list eline)) res))))))))

;*---------------------------------------------------------------------*/
;*    table-cell-format table-cell ...                                 */
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
(define (table-cell-format obj width scribe->ascii)
   (let ((align (markup-option obj :align))
         (body  (markup-body obj)))
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
(define (table-cell-vformat p nblines cwidth)
   (define (make-filler-lines num)
      (vector->list (make-vector num (make-string cwidth #\Space))))
   (let* ((cell (car p))
	  (fmt (cdr p))
	  (lfmt (length fmt)))
      (if (= lfmt nblines)
	  fmt
	  (let* ((new (- nblines lfmt))
		 (new/2 (inexact->exact (round (/ new 2)))))
	     (case (markup-option cell :valign)
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
		   (let loop ((cells (markup-body row))
			      (col 0))
		      (if (pair? cells)
			  (let* ((cell (car cells))
				 (cspan (markup-option cell :colspan))
				 (swidth (markup-option cell :width)))
			     (if (number? swidth)
				 (let* ((swidth (if (integer? swidth)
						    swidth
						    (inexact->exact
						     (* swidth twidth))))
					(cswidth (/ swidth cspan)))
				    (do ((j 0 (+ j 1)))
					((= j cspan)
					 (loop (cdr cells)
					       (+ col cspan)))
					(if (< (vector-ref rsizes (+ col j))
					       cswidth)
					    (vector-set! rsizes
							 (+ col j)
							 cswidth))))
				 (loop (cdr cells) (+ col cspan)))))))
		(markup-body table))
      rsizes))


;;;
;;; Convenience functions.
;;;

(define (smallest-map f l1 l2)
   (if (or (null? l1) (null? l2))
       '()
       (cons (f (car l1) (car l2)) (smallest-map f (cdr l1) (cdr l2)))))

(define (round-to-exact w)
  (if (inexact? w)
      (inexact->exact (round w))
      (round w)))

;;; text-table.scm ends here
