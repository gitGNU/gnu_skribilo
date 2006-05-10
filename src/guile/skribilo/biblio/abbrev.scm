;;; abbrev.scm  --  Determining abbreviations.
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

(define-module (skribilo biblio abbrev)
  :use-module (srfi srfi-13)
  :autoload   (ice-9 regex) (regexp-substitute/global)
  :export (is-abbreviation? is-acronym? abbreviate-word))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; Heuristics to identify or generate abbreviations.
;;;
;;; Code:

(define (is-abbreviation? str)
  ;; Return #t if STR denotes an abbreviation or name initial.
  (and (>= (string-length str) 2)
       (char=? (string-ref str 1) #\.)))

(define (is-acronym? str)
  (string=? str (string-upcase str)))

(define (abbreviate-word word)
   (if (or (string=? "" word)
	   (and (>= (string-length word) 3)
		(string=? "and" (substring word 0 3)))
	   (is-acronym? word))
       word
       (let ((dash (string-index word #\-))
	     (abbr (string (string-ref word 0) #\.)))
	  (if (not dash)
	      abbr
	      (string-append (string (string-ref word 0)) "-"
			     (abbreviate-word
			      (substring word (+ 1 dash)
					 (string-length word))))))))

(define (abbreviate-string subst title)
  ;; Abbreviate common conference names within TITLE based on the SUBST list
  ;; of regexp-substitution pairs.  This function also removes the
  ;; abbreviation if it appears in parentheses right after the substitution
  ;; regexp.  Example:
  ;;
  ;;   "Symposium on Operating Systems Principles (SOSP 2004)"
  ;;
  ;; yields
  ;;
  ;;   "SOSP"
  ;;
  (let loop ((title title)
	     (subst subst))
    (if (null? subst)
	title
	(let* ((abbr (cdar subst))
	       (abbr-rexp (string-append "( \\(" abbr "[^\\)]*\\))?"))
	       (to-replace (string-append (caar subst) abbr-rexp)))
	  (loop (regexp-substitute/global #f to-replace title
					  'pre abbr 'post)
		(cdr subst))))))


;;; arch-tag: 34e0c5bb-592f-467b-b59a-d6f7d130ae4e

;;; abbrev.scm ends here
