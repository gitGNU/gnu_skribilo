;;; prog.scm  --  All the stuff for the prog markup
;;;
;;; Copyright 2003 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
;;; Copyright 2006 Ludovic Courtès  <ludovic.courtes@laas.fr>
;;;
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
;;; USA.

(define-module (skribilo prog)
  :use-module (ice-9 regex)
  :autoload   (ice-9 receive) (receive)
  :use-module (skribilo lib)  ;; `new'
  :autoload   (skribilo ast) (node?)
  :export (make-prog-body resolve-line))

;;; ======================================================================
;;;
;;; COMPATIBILITY
;;;
;;; ======================================================================
(define pregexp-match 	string-match)
(define pregexp-replace (lambda (rx str what)
			  (regexp-substitute/global #f rx str
						    'pre what 'post)))
(define pregexp-quote   regexp-quote)


(define (node-body-set! b v)
  (slot-set! b 'body v))

;;;
;;; FIXME: Tout le module peut se factoriser
;;;        définir en bigloo  node-body-set


;*---------------------------------------------------------------------*/
;*    *lines* ...                                                      */
;*---------------------------------------------------------------------*/
(define *lines* (make-hash-table))

;*---------------------------------------------------------------------*/
;*    make-line-mark ...                                               */
;*---------------------------------------------------------------------*/
(define (make-line-mark m lnum b)
   (let* ((ls (number->string lnum))
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
	      (receive (m l)
		 (extract-mark (car ls) mark regexp)
		 (if (not m)
		     (loop (cdr ls) (cons l res))
		     (values m (append (reverse! res) (cons l (cdr ls)))))))))
      ((node? line)
       (receive (m l)
	  (extract-mark (node-body line) mark regexp)
	  (if (not m)
	      (values #f line)
	      (begin
		 (node-body-set! line l)
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
		((= r2 l)
		 (if (= r1 r2)
		     (reverse! res)
		     (reverse! (cons (substring line r1 r2) res))))
		((char=? (string-ref line r2) #\Newline)
		 (loop (+ r2 1)
		       (+ r2 1)
		       (if (= r1 r2)
			   (cons 'eol res)
			   (cons* 'eol (substring line r1 r2) res))))
		(else
		 (loop r1
		       (+ r2 1)
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
      (let* ((s (number->string i))
	     (l (string-length s)))
	 (if (= l rl)
	     s
	     (string-append (make-string (- rl l) #\space) s))))
 
   (let* ((regexp (and mark
		       (format #f "~a[-a-zA-Z_][-0-9a-zA-Z_]+"
			       (pregexp-quote mark))))
	  (src (cond
		  ((not (pair? src)) (list src))
		  ((and (pair? (car src)) (null? (cdr src))) (car src))
		  (else src)))
	  (lines (collect-lines src))
	  (lnum (if (integer? lnum-init) lnum-init 1))
	  (s (number->string (+ (if (integer? ldigit)
				    (max lnum (expt 10 (- ldigit 1)))
				    lnum)
				(length lines))))
	  (cs (string-length s)))
     (let loop ((lines lines)
		 (lnum lnum)
		 (res '()))
	 (if (null? lines)
	     (reverse! res)
	     (receive (m l)
		      (extract-mark (car lines) mark regexp)
		(let ((n (new markup
 			    (markup '&prog-line)
 			    (ident (and lnum-init (int->str lnum cs)))
 			    (body (if m (make-line-mark m lnum l) l)))))
 		   (loop (cdr lines)
 			 (+ lnum 1)
 			 (cons n res))))))))

