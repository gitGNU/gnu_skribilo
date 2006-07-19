;;; author.scm  --  Handling author names.
;;;
;;; Copyright 2006  Ludovic Courtès <ludovic.courtes@laas.fr>
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

(define-module (skribilo biblio author)
  :use-module (srfi srfi-13)
  :use-module (srfi srfi-14)
  :use-module (skribilo biblio abbrev)
  :autoload   (skribilo ast)     (markup-option markup-body markup-ident)
  :autoload   (skribilo lib)     (skribe-error)
  :autoload   (skribilo runtime) (make-string-replace)
  :export (comma-separated->author-list
	   comma-separated->and-separated-authors

	   extract-first-author-name
	   abbreviate-author-first-names
	   abbreviate-first-names
	   first-author-last-name

	   bib-sort/first-author-last-name))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; Heuristics to manipulate author names as strings.
;;;
;;; Code:

(define (comma-separated->author-list authors)
  ;; Return a list of strings where each individual string is an author
  ;; name.  AUTHORS is a string representing a list of author names separated
  ;; by a comma.

  ;; XXX: I should use SRFI-13 instead.
  (string-split authors #\,))

(define (comma-separated->and-separated-authors authors)
  ;; Take AUTHORS, a string containing comma-separated author names, and
  ;; return a string where author names are separated by " and " (suitable
  ;; for BibTeX).
  (string-join (comma-separated->author-list authors)
	       " and " 'infix))


(define (extract-first-author-name names)
   ;; Extract the name of the first author from string
   ;; NAMES that is a comma-separated list of authors.
   (let ((author-name-end (or (string-index names #\,)
			      (string-length names))))
      (substring names 0 author-name-end)))

(define (abbreviate-author-first-names name)
   ;; Abbreviate author first names
   (let* ((components (string-split name #\space))
	  (component-number (length components)))
      (apply string-append
	     (append
	      (map (lambda (c)
		      (string-append (abbreviate-word c) " "))
		   (list-head components
			      (- component-number 1)))
	      (list-tail components (- component-number 1))))))

(define (abbreviate-first-names names)
   ;; Abbreviate first names in NAMES.  NAMES is supposed to be
   ;; something like "Ludovic Courtès, Marc-Olivier Killijian".
   (let loop ((names ((make-string-replace '((#\newline " ")
					     (#\tab     " ")))
		      names))
	      (result ""))
      (if (string=? names "")
	  result
	  (let* ((len (string-length names))
		 (first-author-names-end (or (string-index names #\,)
					     len))
		 (first-author-names (substring names 0
						first-author-names-end))
		 (next (substring names
				  (min (+ 1 first-author-names-end) len)
				  len)))
	     (loop next
		   (string-append result
				  (if (string=? "" result) "" ", ")
				  (abbreviate-author-first-names
				   first-author-names)))))))


(define (first-author-last-name authors)
  ;; Return a string containing exactly the last name of the first author.
  ;; Author names in AUTHORS are assumed to be comma separated.
  (let loop ((first-author (extract-first-author-name authors)))
    (let ((space (string-index first-author #\space)))
      (if (not space)
	  first-author
	  (loop (substring first-author (+ space 1)
			   (string-length first-author)))))))

(define (bib-sort/first-author-last-name entries)
   ;; May be passed as the `:sort' argument of `the-bibliography'.
   (let ((check-author (lambda (e)
			  (if (not (markup-option e 'author))
			      (skribe-error 'web
					    "No author for this bib entry"
					    (markup-ident e))
			      #t))))
      (sort entries
	    (lambda (e1 e2)
	    (let* ((x1 (check-author e1))
		   (x2 (check-author e2))
		   (a1 (first-author-last-name
			(markup-body (markup-option e1 'author))))
		   (a2 (first-author-last-name
			(markup-body (markup-option e2 'author)))))
	       (string-ci<=? a1 a2))))))


;;; arch-tag: c9a1ef10-a2cd-4a06-bd35-fbdee1abf09a

;;; author.scm ends here
