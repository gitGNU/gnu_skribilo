;;; sui.scm
;;;
;;; Copyright 2003, 2004  Manuel Serrano
;;; Copyright 2005, 2006, 2007  Ludovic Courtès  <ludovic.courtes@laas.fr>
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

(define-module (skribilo sui)
  :use-module (skribilo utils syntax)
  :use-module (skribilo lib)
  :use-module (ice-9 match)
  :use-module (srfi srfi-1)
  :autoload   (skribilo parameters) (*verbose*)
  :autoload   (skribilo reader)     (make-reader)

  :export (load-sui sui-ref->url sui-title sui-file sui-key
           sui-find-ref sui-search-ref sui-filter))

(fluid-set! current-reader %skribilo-module-reader)

;;; Author:  Manuel Serrano
;;; Commentary:
;;;
;;; Library dealing with Skribe URL Indexes (SUI).
;;;
;;; Code:


;;; The contents of the file below are (almost) unchanged compared to Skribe
;;; 1.2d's `sui.scm' file found in the `common' directory.


;*---------------------------------------------------------------------*/
;*    *sui-table* ...                                                  */
;*---------------------------------------------------------------------*/
(define *sui-table* (make-hash-table))

;*---------------------------------------------------------------------*/
;*    load-sui ...                                                     */
;*    -------------------------------------------------------------    */
;*    Returns a SUI sexp if already loaded. Load it otherwise.         */
;*    Raise an error if the file cannot be open.                       */
;*---------------------------------------------------------------------*/
(define (load-sui path)
   (let ((sexp (hash-ref *sui-table* path)))
      (or sexp
	  (begin
	     (when (> (*verbose*) 0)
		(format (current-error-port) "  [loading sui: ~a]\n" path))
	     (let ((p (open-input-file path))
                   (read (make-reader 'skribe)))
		(if (not (input-port? p))
		    (skribe-error 'load-sui
				  "Can't find `Skribe Url Index' file"
				  path)
		    (unwind-protect
		       (let ((sexp (read p)))
			  (match sexp
			     (('sui (? string?) . _)
			      (hash-set! *sui-table* path sexp))
			     (else
			      (skribe-error 'load-sui
					    "Illegal `Skribe Url Index' file"
					    path)))
			  sexp)
		       (close-input-port p))))))))

;*---------------------------------------------------------------------*/
;*    sui-ref->url ...                                                 */
;*---------------------------------------------------------------------*/
(define (sui-ref->url dir sui ident opts)
   (let ((refs (sui-find-ref sui ident opts)))
      (and (pair? refs)
	   (let ((base (sui-file sui))
		 (file (car (car refs)))
		 (mark (cdr (car refs))))
	      (format #f "~a/~a#~a" dir (or file base) mark)))))

;*---------------------------------------------------------------------*/
;*    sui-title ...                                                    */
;*---------------------------------------------------------------------*/
(define (sui-title sexp)
   (match sexp
      (('sui (and title (? string?)) . _)
       title)
      (else
       (skribe-error 'sui-title "Illegal `sui' format" sexp))))

;*---------------------------------------------------------------------*/
;*    sui-file ...                                                     */
;*---------------------------------------------------------------------*/
(define (sui-file sexp)
   (sui-key sexp :file))

;*---------------------------------------------------------------------*/
;*    sui-key ...                                                      */
;*---------------------------------------------------------------------*/
(define (sui-key sexp key)
   (match sexp
      (('sui _ . rest)
       (let loop ((rest rest))
	  (and (pair? rest)
	       (if (eq? (car rest) key)
		   (and (pair? (cdr rest))
			(cadr rest))
		   (loop (cdr rest))))))
      (else
       (skribe-error 'sui-key "Illegal `sui' format" sexp))))

;*---------------------------------------------------------------------*/
;*    sui-find-ref ...                                                 */
;*---------------------------------------------------------------------*/
(define (sui-find-ref sui ident opts)
   (let ((ident (assq :ident opts))
	 (mark (assq :mark opts))
	 (class (let ((c (assq :class opts)))
		   (and (pair? c) (cadr c))))
	 (chapter (assq :chapter opts))
	 (section (assq :section opts))
	 (subsection (assq :subsection opts))
	 (subsubsection (assq :subsubsection opts)))
      (match sui
	 (('sui (? string?) . refs)
	  (cond
	     (mark (sui-search-ref 'marks refs (cadr mark) class))
	     (chapter (sui-search-ref 'chapters refs (cadr chapter) class))
	     (section (sui-search-ref 'sections refs (cadr section) class))
	     (subsection (sui-search-ref 'subsections refs (cadr subsection) class))
	     (subsubsection (sui-search-ref 'subsubsections refs (cadr subsubsection) class))
	     (ident (sui-search-all-refs sui ident class))
	     (else '())))
	 (else
	  (skribe-error 'sui-find-ref "Illegal `sui' format" sui)))))

;*---------------------------------------------------------------------*/
;*    sui-search-all-refs ...                                          */
;*---------------------------------------------------------------------*/
(define (sui-search-all-refs sui id refs)
   '())

;*---------------------------------------------------------------------*/
;*    sui-search-ref ...                                               */
;*---------------------------------------------------------------------*/
(define (sui-search-ref kind refs val class)
   (define (find-ref refs val class)
      (map (lambda (r)
	      (let ((f (memq :file r))
		    (c (memq :mark r)))
		 (cons (and (pair? f) (cadr f)) (and (pair? c) (cadr c)))))
	   (filter (if class
		       (lambda (m)
			  (and (pair? m)
			       (string? (car m))
			       (string=? (car m) val)
			       (let ((c (memq :class m)))
				  (and (pair? c)
				       (eq? (cadr c) class)))))
		       (lambda (m)
			  (and (pair? m)
			       (string? (car m))
			       (string=? (car m) val))))
		   refs)))
   (let loop ((refs refs))
      (if (pair? refs)
	  (if (and (pair? (car refs)) (eq? (caar refs) kind))
	      (find-ref (cdar refs) val class)
	      (loop (cdr refs)))
	  '())))

;*---------------------------------------------------------------------*/
;*    sui-filter ...                                                   */
;*---------------------------------------------------------------------*/
(define (sui-filter sui pred1 pred2)
   (match sui
      (('sui (? string?) . refs)
       (let loop ((refs refs)
		  (res '()))
	  (if (pair? refs)
	      (if (pred1 (car refs))
		  (loop (cdr refs)
			(cons (filter pred2 (cdar refs)) res))
		  (loop (cdr refs) res))
	      (reverse! res))))
      (else
       (skribe-error 'sui-filter "Illegal `sui' format" sui))))
