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
  :autoload   (skribilo ast)           (markup? markup-body-set!)
  :autoload   (skribilo utils strings) (make-string-replace)
  :autoload   (ice-9 regex)      (regexp-substitute/global)
  :export (is-abbreviation? is-acronym? abbreviate-word
           abbreviate-string abbreviate-markup

           %cs-conference-abbreviations
           %ordinal-number-abbreviations
           %common-booktitle-abbreviations))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; Heuristics to identify or generate abbreviations.  This module
;;; particularly targets booktitle abbreviations (in bibliography entries).
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
  ;; of regexp-substitution pairs (see examples below).  This function also
  ;; removes the abbreviation if it appears in parentheses right after the
  ;; substitution regexp.  Example:
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

(define (abbreviate-markup subst markup)
  ;; A version of `abbreviate-string' generalized to arbitrary markup
  ;; objects.
  (let loop ((markup markup))
    (cond ((string? markup)
           (let ((purify (make-string-replace '((#\newline " ")
                                                (#\tab     " ")))))
             (abbreviate-string subst (purify markup))))
          ((list? markup)
           (map loop markup))
          ((markup? markup)
           (markup-body-set! markup (loop (markup-body title)))
           markup)
          (else markup))))


;;;
;;; Common English abbreviations.
;;;

;; The following abbreviation alists may be passed to `abbreviate-string'
;; and `abbreviate-markup'.

(define %cs-conference-abbreviations
  ;; Common computer science conferences and their acronym.
  '(("(Symposium [oO]n )?Operating Systems? Design and [iI]mplementation"
     . "OSDI")
    ("(Symposium [oO]n )?Operating Systems? Principles"
     . "SOSP")
    ("([wW]orkshop [oO]n )?Hot Topics [iI]n Operating Systems"
     . "HotOS")
    ("([cC]onference [oO]n )?[fF]ile [aA]nd [sS]torage [tT]echnologies"
     . "FAST")
    ("([tT]he )?([iI]nternational )?[cC]onference [oO]n [aA]rchitectural Support [fF]or Programming Languages [aA]nd Operating Systems"
     . "ASPLOS")
    ("([tT]he )?([iI]nternational )?[cC]onference [oO]n Peer-[tT]o-[pP]eer Computing"
     . "P2P")
    ("([iI]nternational )?[cC]onference [oO]n [dD]ata [eE]ngineering"
     . "ICDE")
    ("([cC]onference [oOn]) [mM]ass [sS]torage [sS]ystems( [aA]nd [tT]echnologies)?"
     . "MSS")))


(define %ordinal-number-abbreviations
  ;; The poor man's abbreviation system.
  ;; FIXME:  This doesn't work with things like "twenty-first"!
  '(("[Ff]irst"    . "1st")
    ("[sS]econd"   . "2nd")
    ("[Tt]hird"    . "3rd")
    ("[Ff]ourth"   . "4th")
    ("[Ff]ifth"    . "5th")
    ("[Ss]ixth"    . "6th")
    ("[Ss]eventh"  . "7th")
    ("[eE]ighth"   . "8th")
    ("[Nn]inth"    . "9th")
    ("[Tt]enth"    . "10th")
    ("[Ee]leventh" . "11th")
    ("[Tt]welfth"  . "12th")))

(define %common-booktitle-abbreviations
  ;; Common book title abbreviations.  This is used by
  ;; `abbreviate-booktitle'.
  '(("[pP]roceedings?"  . "Proc.")
    ("[iI]nternational" . "Int.")
    ("[sS]ymposium"     . "Symp.")
    ("[cC]onference"    . "Conf.")))


;;; arch-tag: 34e0c5bb-592f-467b-b59a-d6f7d130ae4e

;;; abbrev.scm ends here
