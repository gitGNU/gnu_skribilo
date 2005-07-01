;;; lib.scm
;;;
;;; Copyright 2001, 2002, 2003, 2004  Manuel Serrano
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

(define-skribe-module (skribilo skribe bib))

;;; Author:  Manuel Serrano
;;; Commentary:
;;;
;;; A library of bibliography-related functions.
;;;
;;; Code:


;;; The contents of the file below are unchanged compared to Skribe 1.2d's
;;; `bib.scm' file found in the `common' directory.


;*---------------------------------------------------------------------*/
;*    bib-load! ...                                                    */
;*---------------------------------------------------------------------*/
(define (bib-load! table filename command)
   (if (not (bib-table? table))
       (skribe-error 'bib-load "Illegal bibliography table" table)
       ;; read the file
       (let ((p (skribe-open-bib-file filename command)))
	  (if (not (input-port? p))
	      (skribe-error 'bib-load "Can't open data base" filename)
	      (unwind-protect
		 (parse-bib table p)
		 (close-input-port p))))))

;*---------------------------------------------------------------------*/
;*    resolve-bib ...                                                  */
;*---------------------------------------------------------------------*/
(define (resolve-bib table ident)
   (if (not (bib-table? table))
       (skribe-error 'resolve-bib "Illegal bibliography table" table)
       (let* ((i (cond
		    ((string? ident) ident)
		    ((symbol? ident) (symbol->string ident))
		    (else (skribe-error 'resolve-bib "Illegal ident" ident))))
	      (en (hashtable-get table i)))
	  (if (is-markup? en '&bib-entry)
	      en
	      #f))))

;*---------------------------------------------------------------------*/
;*    make-bib-entry ...                                               */
;*---------------------------------------------------------------------*/
(define (make-bib-entry kind ident fields from)
   (let* ((m (new markup
		(markup '&bib-entry)
		(ident ident)
		(options `((kind ,kind) (from ,from)))))
	  (h (new handle
		(ast m))))
      (for-each (lambda (f)
		   (if (and (pair? f)
			    (pair? (cdr f))
			    (null? (cddr f))
			    (symbol? (car f)))
		       (markup-option-add! m 
					   (car f)
					   (new markup
					      (markup (symbol-append
						       '&bib-entry-
						       (car f)))
					      (parent h)
					      (body (cadr f))))
		       (bib-parse-error f)))
		fields)
      m))

;*---------------------------------------------------------------------*/
;*    bib-sort/authors ...                                             */
;*---------------------------------------------------------------------*/
(define (bib-sort/authors l)
   (define (cmp i1 i2 def)
      (cond
	 ((and (markup? i1) (markup? i2))
	  (cmp (markup-body i1) (markup-body i2) def))
	 ((markup? i1)
	  (cmp (markup-body i1) i2 def))
	 ((markup? i2)
	  (cmp i1 (markup-body i2) def))
	 ((and (string? i1) (string? i2))
	  (if (string=? i1 i2)
	      (def)
	      (string<? i1 i2)))
	 ((string? i1)
	  #f)
	 ((string? i2)
	  #t)
	 (else
	  (def))))
   (sort l (lambda (e1 e2)
	      (cmp (markup-option e1 'author)
		   (markup-option e2 'author)
		   (lambda ()
		      (cmp (markup-option e1 'year)
			   (markup-option e2 'year)
			   (lambda ()
			      (cmp (markup-option e1 'title)
				   (markup-option e2 'title)
				   (lambda ()
				      (cmp (markup-ident e1)
					   (markup-ident e2)
					   (lambda ()
					      #t)))))))))))

;*---------------------------------------------------------------------*/
;*    bib-sort/idents ...                                              */
;*---------------------------------------------------------------------*/
(define (bib-sort/idents l)
   (sort l (lambda (e f) (string<? (markup-ident e) (markup-ident f)))))

;*---------------------------------------------------------------------*/
;*    bib-sort/dates ...                                               */
;*---------------------------------------------------------------------*/
(define (bib-sort/dates l)
   (sort l (lambda (p1 p2)
	      (define (month-num m)
		 (let ((body (markup-body m)))
		    (if (not (string? body))
			13
			(let* ((s (if (> (string-length body) 3)
				      (substring body 0 3)
				      body))
			       (sy (string->symbol (string-downcase body)))
			       (c (assq sy '((jan . 1)
					     (feb . 2)
					     (mar . 3)
					     (apr . 4)
					     (may . 5)
					     (jun . 6)
					     (jul . 7)
					     (aug . 8)
					     (sep . 9)
					     (oct . 10)
					     (nov . 11)
					     (dec . 12)))))
			   (if (pair? c) (cdr c) 13)))))
	      (let ((d1 (markup-option p1 'year))
		    (d2 (markup-option p2 'year)))
		 (cond
		    ((not (markup? d1)) #f)
		    ((not (markup? d2)) #t)
		    (else
		     (let ((y1 (markup-body d1))
			   (y2 (markup-body d2)))
			(cond
			   ((string>? y1 y2) #t)
			   ((string<? y1 y2) #f)
			   (else
			    (let ((d1 (markup-option p1 'month))
				  (d2 (markup-option p2 'month)))
			       (cond
				  ((not (markup? d1)) #f)
				  ((not (markup? d2)) #t)
				  (else
				   (let ((m1 (month-num d1))
					 (m2 (month-num d2)))
				      (> m1 m2))))))))))))))

;*---------------------------------------------------------------------*/
;*    resolve-the-bib ...                                              */
;*---------------------------------------------------------------------*/
(define (resolve-the-bib table n sort pred count opts)
   (define (count! entries)
      (let loop ((es entries)
		 (i 1))
	 (if (pair? es)
	     (begin
		(markup-option-add! (car es)
				    :title
				    (new markup
				       (markup '&bib-entry-ident)
				       (parent (car es))
				       (options `((number ,i)))
				       (body (new handle
						(ast (car es))))))
		(loop (cdr es) (+ i 1))))))
   (if (not (bib-table? table))
       (skribe-error 'resolve-the-bib "Illegal bibliography table" table)
       (let* ((es (sort (hashtable->list table)))
	      (fes (filter (if (procedure? pred)
			       (lambda (m) (pred m n))
			       (lambda (m) (pair? (markup-option m 'used))))
			   es)))
	  (count! (if (eq? count 'full) es fes))
	  (new markup
	     (markup '&the-bibliography)
	     (options opts)
	     (body fes)))))


;;; bib.scm ends here
