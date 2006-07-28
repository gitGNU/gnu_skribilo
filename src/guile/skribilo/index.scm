;;; index.scm
;;;
;;; Copyright 2003, 2004  Manuel Serrano
;;; Copyright 2005, 2006  Ludovic Courtès  <ludovic.courtes@laas.fr>
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

(define-module (skribilo index)
  :use-syntax (skribilo utils syntax)
  :use-syntax (skribilo lib)

  :use-module (skribilo lib)
  :use-module (skribilo ast)
  :use-module (srfi srfi-39)

  ;; XXX: The use of `mark' here introduces a cross-dependency between
  ;; `index' and `package base'.  Thus, we require that each of these two
  ;; modules autoloads the other one.
  :autoload   (skribilo package base) (mark)

  :export (index? make-index-table *index-table*
           default-index resolve-the-index))


(fluid-set! current-reader %skribilo-module-reader)

;;; Author:  Manuel Serrano
;;; Commentary:
;;;
;;; A library of functions dealing with the creation of indices in
;;; documents.
;;;
;;; Code:


;;; The contents of the file below are (almost) unchanged compared to Skribe
;;; 1.2d's `index.scm' file found in the `common' directory.


;*---------------------------------------------------------------------*/
;*    index? ...                                                       */
;*---------------------------------------------------------------------*/
(define (index? obj)
   (hash-table? obj))

;*---------------------------------------------------------------------*/
;*    *index-table* ...                                                */
;*---------------------------------------------------------------------*/
(define *index-table* (make-parameter #f))

;*---------------------------------------------------------------------*/
;*    make-index-table ...                                             */
;*---------------------------------------------------------------------*/
(define (make-index-table ident)
   (make-hash-table))

;*---------------------------------------------------------------------*/
;*    default-index ...                                                */
;*---------------------------------------------------------------------*/
(define (default-index)
   (if (not (*index-table*))
       (*index-table* (make-index-table "default-index")))
   (*index-table*))

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
   (let* ((entries (apply append (map (lambda (t)
                                        (hash-map->list
                                         (lambda (key val) val) t))
                                      indexes)))
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


;;; index.scm ends here
