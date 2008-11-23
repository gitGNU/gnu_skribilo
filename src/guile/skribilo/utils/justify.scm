;;; justify.scm  --  Producing justified text.
;;;
;;; Copyright 2008  Ludovic Courtès <ludo@gnu.org>
;;; Copyright 2001  Manuel Serrano
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;;; USA.

(define-module (skribilo utils justify)
  :use-module (srfi srfi-13)
  :export (make-justifier output-flush

	   *text-column-width*
	   *text-justification*
	   *margin*

           output-justified output-token output-center
           output-newline justification-width
	   with-justification with-justification/noflush))

;;; Author: Manuel Serrano, Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; Provide a set of tools that makes it easy to produce justified text,
;;; i.e., text that automatically wraps after, say, 80 columns, indented
;;; text, etc.
;;;
;;; Code:


;*---------------------------------------------------------------------*/
;*    *text-column-width* ...                                          */
;*---------------------------------------------------------------------*/
(define *text-column-width* 79)
(define *text-justification* 'left)

;*---------------------------------------------------------------------*/
;*    text-string ...                                                  */
;*---------------------------------------------------------------------*/
(define (text-string str)
   (let ((len (string-length str)))
      (let loop ((r 0))
	 (cond
	    ((= r len)
	     str)
	    ((char=? (string-ref str r) #\bs)
	     (string-set! str r #\Space)
	     (loop (+ r 1)))
	    (else
	     (loop (+ r 1)))))))

;*---------------------------------------------------------------------*/
;*    string-replace ...                                               */
;*---------------------------------------------------------------------*/
(define (string-replace-char str1 c1 c2)
   (let* ((len (string-length str1))
	  (str2 (make-string len)))
      (let loop ((r 0))
	 (if (= r len)
	     str2
	     (let ((c (string-ref str1 r)))
		(if (char=? c c1)
		    (string-set! str2 r c2)
		    (string-set! str2 r c))
		(loop (+ r 1)))))))

;*---------------------------------------------------------------------*/
;*    output-center ...                                                */
;*---------------------------------------------------------------------*/
(define (output-center str)
   (let ((justifier (make-justifier (justification-width) 'center)))
      (with-justification justifier
			  (lambda ()
			     (output str)))))

(define *margin* 0)

;*---------------------------------------------------------------------*/
;*    output ...                                                       */
;*---------------------------------------------------------------------*/
(define (output-justified str)
   ((car *justifiers*) 'output str))

;*---------------------------------------------------------------------*/
;*    output-token ...                                                 */
;*    -------------------------------------------------------------    */
;*    Display one string as if it is one token. No matter if it        */
;*    contains #\spaces.                                               */
;*---------------------------------------------------------------------*/
(define (output-token str)
   ((car *justifiers*) 'output (string-replace-char str #\space #\bs)))

;*---------------------------------------------------------------------*/
;*    output-newline ...                                               */
;*---------------------------------------------------------------------*/
(define (output-newline)
   ((car *justifiers*) 'newline))

;*---------------------------------------------------------------------*/
;*    output-flush ...                                                 */
;*---------------------------------------------------------------------*/
(define (output-flush margin)
   (for-each (if (> margin 0)
		 (let ((m (make-string margin #\space)))
		    (lambda (x)
                      (display m)
                      (display (text-string x))
                      (newline)))
		 (lambda (x)
                   (display (text-string x))
                   (newline)))
	     ((car *justifiers*) 'flush)))

;*---------------------------------------------------------------------*/
;*    justification-width ...                                          */
;*---------------------------------------------------------------------*/
(define (justification-width)
   ((car *justifiers*) 'width))

;*---------------------------------------------------------------------*/
;*    with-justification ...                                           */
;*---------------------------------------------------------------------*/
(define (with-justification justifier thunk . margin)
   (output-flush *margin*)
   (let ((old-margin *margin*))
      (if (pair? margin) (set! *margin* (+ *margin* (car margin))))
      (set! *justifiers* (cons justifier *justifiers*))
      (thunk)
      (output-flush *margin*)
      (set! *justifiers* (cdr *justifiers*))
      (set! *margin* old-margin)))

;*---------------------------------------------------------------------*/
;*    with-justification/noflush ...                                   */
;*---------------------------------------------------------------------*/
(define (with-justification/noflush justifier thunk . margin)
   (let ((old-margin *margin*))
      (if (pair? margin) (set! *margin* (+ *margin* (car margin))))
      (set! *justifiers* (cons justifier *justifiers*))
      (thunk)
      (let ((res ((car *justifiers*) 'flush)))
	 (set! *justifiers* (cdr *justifiers*))
	 (set! *margin* old-margin)
	 res)))

;*---------------------------------------------------------------------*/
;*    *spaces* ...                                                     */
;*---------------------------------------------------------------------*/
(define *spaces* '(#\Space #\Tab #\Newline))

;*---------------------------------------------------------------------*/
;*    kotrts ...                                                       */
;*---------------------------------------------------------------------*/
(define (kotrts str delims)
   (let ((stop (string-length str)))
      (let loop ((cur  0)
                 (mark #f)
                 (acc  '()))
         (cond
            ((= cur stop)
             (if (number? mark)
		 (cons (substring str mark cur) acc)
                 acc))
            ((memq (string-ref str cur) delims)
             (loop (+ cur 1)
                   #f
                   (if (number? mark)
                       (cons (substring str mark cur)
                             acc)
                       acc)))
            (else
             (loop (+ cur 1)
                   (if (number? mark) mark cur)
                   acc))))))

;*---------------------------------------------------------------------*/
;*    string-insert! ...                                               */
;*---------------------------------------------------------------------*/
(define (string-insert! str-to str-from offset)
   (let ((len1 (string-length str-to))
         (len2 (string-length str-from)))
      (if (> (+ len2 offset) len1)
          (error "string-insert!" "String too long" str-from)
          (let loop ((i 0))
             (if (= i len2)
                 str-to
                 (begin
                    (string-set! str-to
                                 (+ i offset)
                                 (string-ref str-from i))
                    (loop (+ i 1))))))))

;*---------------------------------------------------------------------*/
;*    make-justified-line ...                                          */
;*---------------------------------------------------------------------*/
(define (make-justified-line tokens width)
   (let ((result (make-string width #\space)))
      (cond
         ((null? tokens)
          result)
         ((null? (cdr tokens))
          (string-insert! result (car tokens) 0))
         (else
          (let* ((nb-tokens  (length tokens))
                 (nb-chars   (apply + (map string-length
                                           tokens)))
                 (all-spaces (- width nb-chars))
                 (one-spaces (/ all-spaces
                                (- nb-tokens 1)))
                 (cursor     (string-length (car tokens))))
             (string-insert! result (car tokens) 0)
             (let loop ((tokens (cdr tokens))
                        (cursor cursor))
                (if (null? (cdr tokens))
                    (let* ((len (string-length
                                 (car tokens)))
                           (cursor (- width len)))
                       (string-insert! result
                                       (car tokens)
                                       cursor)
                       result)
                    (let* ((token    (car tokens))
                           (token-ln (string-length token))
                           (n-cursor (+ cursor
                                        token-ln
                                        one-spaces))
                           (offset   (inexact->exact
                                      (round
                                       (+ cursor
                                          one-spaces)))))
                       (string-insert! result token offset)
                       (loop (cdr tokens) n-cursor)))))))))

;*---------------------------------------------------------------------*/
;*    make-formated-line ...                                           */
;*---------------------------------------------------------------------*/
(define (make-formated-line tokens width cursor)
   (let ((result (make-string width #\space)))
      (if (null? tokens)
          result
          (let loop ((toks tokens)
                     (cur cursor))
             (if (null? toks)
                 result
                 (begin
                    (string-insert! result (car toks) cur)
                    (loop (cdr toks)
                          (+ 1
                             cur
                             (string-length
			      (car toks))))))))))

;*---------------------------------------------------------------------*/
;*    make-centered-line ...                                           */
;*---------------------------------------------------------------------*/
(define (make-centered-line tokens width)
   (make-formated-line tokens
		       width
		       (quotient (- width
				    (+ (apply + (map string-length tokens))
				       (- (length tokens) 1)))
				 2)))

;*---------------------------------------------------------------------*/
;*    make-flushleft-line ...                                          */
;*---------------------------------------------------------------------*/
(define (make-flushleft-line tokens width)
   (make-formated-line tokens width 0))

;*---------------------------------------------------------------------*/
;*    make-flushright-line ...                                         */
;*---------------------------------------------------------------------*/
(define (make-flushright-line tokens width)
   (make-formated-line tokens
		       width
		       (- width
			  (+ (apply + (map string-length tokens))
			     (- (length tokens) 1)))))

;*---------------------------------------------------------------------*/
;*    tokens-justify ...                                               */
;*---------------------------------------------------------------------*/
(define (tokens-justify justifier tokens width)
   (let loop ((tokens    tokens)
              (line-len  0)
              (line     '())
              (acc      '()))
      (if (null? tokens)
          (reverse! (cons (justifier (reverse line) width) acc))
          (let ((tok (car tokens)))
	     (cond
		((eq? tok 'NEWLINE)
		 (loop (cdr tokens)
		       0
		       '()
		       (cons (justifier (reverse line) width) acc)))
		(else
		 (let ((toklen (string-length tok)))
		    (cond
		       ((>= toklen width)
			(let ((jl (justifier (list (substring tok 0 width))
					     width))
			      (ll (if (pair? line)
				      (cons (justifier (reverse line) width)
					    acc)
				      acc)))
			   (loop (cdr tokens)
				 0
				 '()
				 (cons jl ll))))
		       ((>= (+ toklen line-len) width)
			(loop tokens
			      0
			      '()
			      (cons (justifier (reverse line) width) acc)))
		       (else
			(loop (cdr tokens)
			      (+ line-len toklen 1)
			      (cons tok line)
			      acc))))))))))
 
;*---------------------------------------------------------------------*/
;*    make-justifier ...                                               */
;*---------------------------------------------------------------------*/
(define (make-justifier width policy)
   (let ((tokens '()))
      (if (eq? policy 'verbatim)
	  (lambda (cmd . vals)
	     (case cmd
		((output)
		 (set! tokens (append (reverse vals) tokens)))
		((newline)
		 (set! tokens (cons "\n" tokens)))
		((flush)
		 (let ((str (string-concatenate (reverse! tokens))))
		    (set! tokens '())
		    (list str)))
		((width)
		 width)))
	  (let ((justifier (case policy
			      ((center)
			       make-centered-line)
			      ((flushleft left)
			       make-flushleft-line)
			      ((flushright right)
			       make-flushright-line)
			      ((justify)
			       make-justified-line)
			      (else
			       make-justified-line)))
		(last ""))
	     (lambda (cmd . vals)
		(case cmd
		   ((newline)
		    (set! tokens (cons 'NEWLINE
				       (append (kotrts last *spaces*) tokens)))
		    (set! last ""))
		   ((output)
		    (if (pair? vals)
			(let* ((val0 (string-append last (car vals)))
			       (vals (cons val0 (cdr vals))))
			   (let loop ((vals vals)
				      (toks tokens))
			      (cond
				 ((null? vals)
				  (set! last "")
				  (set! tokens toks))
				 ((and (null? (cdr vals))
				       (string? (car vals)))
				  (set! last (car vals))
				  (set! tokens toks))
				 (else
				  (loop (cdr vals)
					(append (kotrts (car vals) *spaces*)
						toks))))))))
		   ((flush)
		    (let ((ntokens (append (kotrts last *spaces*) tokens)))
		       (set! last "")
		       (if (pair? ntokens)
			   (let ((toks (reverse! ntokens)))
			      (set! tokens '())
			      (tokens-justify justifier toks width))
			   '())))
		   ((width)
		    width)
		   (else
		    (error "justifier" "Illegal command" cmd))))))))

;*---------------------------------------------------------------------*/
;*    *justifiers* ...                                                 */
;*---------------------------------------------------------------------*/
(define *justifiers* (list (make-justifier *text-column-width*
					   *text-justification*)))


;;; justify.scm ends here
