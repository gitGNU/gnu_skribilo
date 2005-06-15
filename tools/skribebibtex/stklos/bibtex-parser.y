;;;;							-*- Scheme -*-
;;;; bibtex-parser.y	-- SILex input for BibTeX
;;;; 
;;;; Copyright © 2004 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
;;;; 
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, 
;;;; USA.
;;;; 
;;;;           Author: Erick Gallesio [eg@essi.fr]
;;;;    Creation date: 21-Oct-2004 17:47 (eg)
;;;; Last file update: 22-Oct-2004 18:14 (eg)
;;;;

(load "lalr")

(define (main args)
  ;; Build the parser
  (lalr-parser
     ;; Options
     (output: parser "bibtex-parser.stk")

     ;; Terminal symbols
     (CHAR BLANK IDENT STRING COMMA LBRACKET RBRACKET NUMBER EQUAL
      BIBSTRING BIBITEM)

     ;; Rules 
     (S
        ()
	(S string-def)
	(S blank*)
	(S bibtex-entry))


     (blank*
        ()
	(blank* BLANK))

     
     (string-def
        (BIBSTRING LBRACKET  blank* IDENT blank* EQUAL blank* entry-value
		   blank* RBRACKET)
	: (bibtex-string-def! (car $4) (car $8)))


     (bibtex-entry
        (BIBITEM LBRACKET blank* IDENT blank* COMMA blank* entry-item* RBRACKET)
	: (make-bibentry $1 $4 $8))

     
     (entry-item*
        (blank*)
	: '()
	(entry-item)
	: (list $1)
	(entry-item COMMA entry-item*)
	: (cons $1 $3))
      
     
     (entry-item
        (blank* IDENT blank* EQUAL blank* entry-value blank*)
	: (cons (car $2) $6))
     
     
     (entry-value
        (NUMBER)
	: (list (car $1))
	(STRING)
	: $1
	(IDENT)
	: (bibtex-string-ref (car $1))
	(LBRACKET entry-value-block* RBRACKET)
	: (list (apply string-append $2)))
	  

      (entry-value-block*
         ()
	 : '()
	 (entry-value-block* entry-value-block)
	 : (append $1 $2))

      
      (entry-value-block
         (LBRACKET entry-value-block* RBRACKET)
	 : $2
	 (COMMA)
	 : (list ",")
	 (IDENT)
	 : $1
	 (BLANK)
	 : (list " ")
	 (EQUAL)
	 : (list "=")
	 (CHAR)
	 : $1
	 (NUMBER)
	 : $1
	 (STRING)
	 : $1)
     )
  ;; Terminate
  0)
  

  