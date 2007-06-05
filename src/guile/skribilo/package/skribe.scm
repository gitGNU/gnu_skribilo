;;; skribe.scm  --  The standard Skribe style (always loaded).
;;;
;;; Copyright 2003, 2004  Manuel Serrano
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

;;; FIXME: This must be moved to the base package!

;*---------------------------------------------------------------------*/
;*    p ...                                                            */
;*---------------------------------------------------------------------*/
(define-markup (p :rest opt :key ident (class #f) &skribe-eval-location)
   (paragraph :ident ident :class class :loc &skribe-eval-location
      (the-body opt)))

;*---------------------------------------------------------------------*/
;*    fg ...                                                           */
;*---------------------------------------------------------------------*/
(define (fg c . body)
   (color :fg c body))

;*---------------------------------------------------------------------*/
;*    bg ...                                                           */
;*---------------------------------------------------------------------*/
(define (bg c . body)
   (color :bg c body))

;*---------------------------------------------------------------------*/
;*    counter ...                                                      */
;*    -------------------------------------------------------------    */
;*    This produces a kind of "local enumeration" that is:             */
;*       (counting "toto," "tutu," "titi.")                            */
;*    produces:                                                        */
;*       i) toto, ii) tutu, iii) titi.                                 */
;*---------------------------------------------------------------------*/
(define-markup (counter :rest opts :key (numbering 'roman))
   (define items (if (eq? (car opts) :numbering) (cddr opts) opts))
   (define vroman '#(- "i" "ii" "iii" "iv" "v" "vi" "vii" "viii" "ix" "x"))
   (define (the-roman-number num)
      (if (< num (vector-length vroman))
	  (list (list "(" (it (vector-ref vroman num)) ") "))
	  (skribe-error 'counter
			"too many items for roman numbering"
			(length items))))
   (define (the-arabic-number num)
      (list (list "(" (it (integer->string num)) ") ")))
   (define (the-alpha-number num)
      (list (list "(" (it (+ (integer->char #\a) num -1)) ") ")))
   (let ((the-number (case numbering
			((roman) the-roman-number)
			((arabic) the-arabic-number)
			((alpha) the-alpha-number)
			(else (skribe-error 'counter
					    "Illegal numbering"
					    numbering)))))
      (let loop ((num 1)
		 (items items)
		 (res '()))
	   (if (null? items)
	       (reverse! res)
	       (loop (+ num 1)
		     (cdr items)
		     (cons (list (the-number num) (car items)) res))))))

;*---------------------------------------------------------------------*/
;*    q                                                                */
;*---------------------------------------------------------------------*/
(define-markup (q :rest opt)
   (new markup
      (markup 'q)
      (options (the-options opt))
      (body (the-body opt))))

