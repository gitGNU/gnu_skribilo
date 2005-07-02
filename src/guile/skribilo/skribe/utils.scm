;;; utils.scm
;;;
;;; Copyright 2003, 2004  Manuel Serrano
;;; Copyright 2005  Ludovic Courtès  <ludovic.courtes@laas.fr>
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

(define-skribe-module (skribilo skribe utils))

;;; Author:  Manuel Serrano
;;; Commentary:
;;;
;;; A library of various utilities, including AST traversal helper functions.
;;;
;;; Code:


;;; The contents of the file below are unchanged compared to Skribe 1.2d's
;;; `lib.scm' file found in the `common' directory.

;*---------------------------------------------------------------------*/
;*    engine-custom-add! ...                                           */
;*---------------------------------------------------------------------*/
(define-public (engine-custom-add! e id val)
   (let ((old (engine-custom e id)))
      (if (unspecified? old)
	  (engine-custom-set! e id (list val))
	  (engine-custom-set! e id (cons val old)))))

;*---------------------------------------------------------------------*/
;*    find-markup-ident ...                                            */
;*---------------------------------------------------------------------*/
(define-public (find-markup-ident ident)
   (let ((r (find-markups ident)))
      (if (or (pair? r) (null? r))
	  r
	  '())))

;*---------------------------------------------------------------------*/
;*    container-search-down ...                                        */
;*---------------------------------------------------------------------*/
(define-public (container-search-down pred obj)
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
(define-public (search-down pred obj)
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
(define-public (find-down pred obj)
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
(define-public (find1-down pred obj)
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
(define-public (find-up pred obj)
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
(define-public (find1-up pred obj)
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
(define-public (ast-document m)
   (find1-up document? m))

;*---------------------------------------------------------------------*/
;*    ast-chapter ...                                                  */
;*---------------------------------------------------------------------*/
(define-public (ast-chapter m)
   (find1-up (lambda (n) (is-markup? n 'chapter)) m))

;*---------------------------------------------------------------------*/
;*    ast-section ...                                                  */
;*---------------------------------------------------------------------*/
(define-public (ast-section m)
   (find1-up (lambda (n) (is-markup? n 'section)) m))

;*---------------------------------------------------------------------*/
;*    the-body ...                                                     */
;*    -------------------------------------------------------------    */
;*    Filter out the options                                           */
;*---------------------------------------------------------------------*/
(define-public (the-body opt+)
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
(define-public (the-options opt+ . out)
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
(define-public (list-split l num . fill)
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

;;; utils.scm ends here
