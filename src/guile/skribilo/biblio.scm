;;; biblio.scm  --  Bibliography functions.
;;;
;;; Copyright 2003, 2004  Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
;;; Copyright 2005, 2006  Ludovic Courtès <ludovic.courtes@laas.fr>
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
;;; USA.main.st



(define-module (skribilo biblio)
  :use-module (skribilo utils strings)
  :use-module (skribilo utils syntax) ;; `when', `unless'

  :autoload   (srfi srfi-34)         (raise)
  :use-module (srfi srfi-35)
  :use-module (srfi srfi-1)
  :autoload   (skribilo condition)   (&file-search-error)

  :autoload   (skribilo reader)      (%default-reader)
  :autoload   (skribilo parameters)  (*bib-path*)
  :autoload   (skribilo ast)         (<markup> <handle> is-markup?)

  :use-module (ice-9 optargs)
  :use-module (oop goops)

  :export (bib-table? make-bib-table default-bib-table
	   bib-add! bib-duplicate bib-for-each bib-map
	   skribe-open-bib-file parse-bib

           bib-load! resolve-bib resolve-the-bib make-bib-entry

           ;; sorting entries
           bib-sort/authors bib-sort/idents bib-sort/dates))

;;; Author: Erick Gallesio, Manuel Serrano, Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; Provides the bibliography data type and basic bibliography handling,
;;; including simple procedures to sort bibliography entries.
;;;
;;; FIXME: This module need cleanup!
;;;
;;; Code:

(fluid-set! current-reader %skribilo-module-reader)


;; FIXME: Should be a fluid?
(define *bib-table*	     #f)

;; Forward declarations
(define skribe-open-bib-file #f)
(define parse-bib	     #f)



;;; ======================================================================
;;;
;;;				Utilities
;;;
;;; ======================================================================

(define (make-bib-table ident)
   (make-hash-table))

(define (bib-table? obj)
  (hash-table? obj))

(define (default-bib-table)
  (unless *bib-table*
    (set! *bib-table* (make-bib-table "default-bib-table")))
  *bib-table*)

(define (%bib-error who entry)
  (let ((msg "bibliography syntax error on entry"))
    (if (%epair? entry)
	(skribe-line-error (%epair-file entry) (%epair-line entry) who msg entry)
	(skribe-error who msg entry))))

(define* (bib-for-each proc :optional (table (default-bib-table)))
  (hash-for-each (lambda (ident entry)
		   (proc ident entry))
		 table))

(define* (bib-map proc :optional (table (default-bib-table)))
  (hash-map->list (lambda (ident entry)
		    (proc ident entry))
		  table))


;;; ======================================================================
;;;
;;;				BIB-DUPLICATE
;;;
;;; ======================================================================
(define (bib-duplicate ident from old)
  (let ((ofrom (markup-option old 'from)))
    (skribe-warning 2
		    'bib
		    (format #f "duplicated bibliographic entry ~a'.\n" ident)
		    (if ofrom
			(format #f " using version of `~a'.\n" ofrom)
			"")
		    (if from
			(format #f " ignoring version of `~a'." from)
			" ignoring redefinition."))))


;;; ======================================================================
;;;
;;;				PARSE-BIB
;;;
;;; ======================================================================
(define (parse-bib table port)
  (let ((read %default-reader)) ;; FIXME: We should use a fluid
    (if (not (bib-table? table))
	(skribe-error 'parse-bib "Illegal bibliography table" table)
	(let ((from (port-filename port)))
	  (let Loop ((entry (read port)))
	    (unless (eof-object? entry)
	      (cond
	       ((and (list? entry) (> (length entry) 2))
		(let* ((kind   (car entry))
		       (key    (format #f "~A" (cadr entry)))
		       (fields (cddr entry))
		       (old    (hash-ref table key)))
		  (if old
		      (bib-duplicate ident from old)
		      (hash-set! table key
				 (make-bib-entry kind key fields from)))
		  (Loop (read port))))
	       (else
		(%bib-error 'bib-parse entry)))))))))


;;; ======================================================================
;;;
;;;				   BIB-ADD!
;;;
;;; ======================================================================
(define (bib-add! table . entries)
  (if (not (bib-table? table))
      (skribe-error 'bib-add! "Illegal bibliography table" table)
      (for-each (lambda (entry)
		  (cond
		    ((and (list? entry) (> (length entry) 2))
		     (let* ((kind   (car entry))
			    (key    (format #f "~A" (cadr entry)))
			    (fields (cddr entry))
			    (old    (hash-ref table key)))
		       (if old
			   (bib-duplicate key #f old)
			   (hash-set! table key
				      (make-bib-entry kind key fields #f)))))
		    (else
		     (%bib-error 'bib-add! entry))))
		entries)))


;;; ======================================================================
;;;
;;;				SKRIBE-OPEN-BIB-FILE
;;;
;;; ======================================================================
;; FIXME: Factoriser
(define (skribe-open-bib-file file command)
 (let ((path (search-path (*bib-path*) file)))
   (if (string? path)
       (begin
	 (when (> (*verbose*) 0)
	   (format (current-error-port) "  [loading bibliography: ~S]\n" path))
	 (open-input-file (if (string? command)
			      (string-append "| "
					     (format #f command path))
			      path)))
       (raise (condition (&file-search-error (file-name file)
					     (path (*bib-path*))))))))


;;;
;;; High-level API.
;;;
;;; The contents of the file below are unchanged compared to Skribe 1.2d's
;;; `bib.scm' file found in the `common' directory.  The copyright notice for
;;; this file was:
;;;
;;;  Copyright 2001, 2002, 2003, 2004  Manuel Serrano
;;;


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
	      (en (hash-ref table i)))
	  (if (is-markup? en '&bib-entry)
	      en
	      #f))))

;*---------------------------------------------------------------------*/
;*    make-bib-entry ...                                               */
;*---------------------------------------------------------------------*/
(define (make-bib-entry kind ident fields from)
   (let* ((m (make <markup>
		:markup '&bib-entry
		:ident ident
		:options `((kind ,kind) (from ,from))))
	  (h (make <handle> :ast m)))
      (for-each (lambda (f)
		   (if (and (pair? f)
			    (pair? (cdr f))
			    (null? (cddr f))
			    (symbol? (car f)))
		       (markup-option-add! m 
					   (car f)
					   (make <markup>
					      :markup (symbol-append
						       '&bib-entry-
						       (car f))
					      :parent h
					      :body (cadr f)))
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
				    (make <markup>
				       :markup '&bib-entry-ident
				       :parent (car es)
				       :options `((number ,i))
				       :body (make <handle> :ast (car es))))
		(loop (cdr es) (+ i 1))))))
   (if (not (bib-table? table))
       (skribe-error 'resolve-the-bib "Illegal bibliography table" table)
       (let* ((es (sort (hash-map->list (lambda (key val) val) table)))
	      (fes (filter (if (procedure? pred)
			       (lambda (m) (pred m n))
			       (lambda (m) (pair? (markup-option m 'used))))
			   es)))
	  (count! (if (eq? count 'full) es fes))
	  (make <markup>
	     :markup '&the-bibliography
	     :options opts
	     :body fes))))


;;; biblio.scm ends here
