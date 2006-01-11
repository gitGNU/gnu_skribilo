;;; biblio.scm  --  Bibliography functions.
;;;
;;; Copyright 2003-2004  Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
;;; Copyright 2005  Ludovic Courtès <ludovic.courtes@laas.fr>
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
;;; USA.main.st



(define-module (skribilo biblio)
  :use-module (skribilo runtime)
  :use-module (skribilo utils syntax) ;; `when', `unless'
  :use-module (skribilo module)
  :use-module (skribilo skribe bib) ;; `make-bib-entry'
  :autoload   (skribilo reader)      (%default-reader)
  :autoload   (skribilo parameters)  (*bib-path*)
  :autoload   (ice-9 format)         (format)
  :export (bib-table? make-bib-table default-bib-table
	   bib-add! bib-duplicate
	   skribe-open-bib-file parse-bib))

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

;;
;; Utilities
;;
(define (%bib-error who entry)
  (let ((msg "bibliography syntax error on entry"))
    (if (%epair? entry)
	(skribe-line-error (%epair-file entry) (%epair-line entry) who msg entry)
	(skribe-error who msg entry))))

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
       (begin
	 (skribe-warning 1
			 'bibliography
			 "Can't find bibliography -- " file)
	 #f))))
