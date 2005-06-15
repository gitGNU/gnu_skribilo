;;;;
;;;; xml.stk			-- XML Fontification stuff
;;;; 
;;;; Copyright © 2003 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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
;;;;    Creation date: 16-Oct-2003 22:33 (eg)
;;;; Last file update: 28-Dec-2003 17:33 (eg)
;;;;


;(require "lex-rt")		;; to avoid module problems


(define-module (skribe xml)
   :export (xml))

(use-modules (skribe source))

(include "xml-lex.stk")		;; SILex generated

(define (xml-fontifier s)
  (let ((lex (xml-lex (open-input-string s))))
    (let Loop ((token (lexer-next-token lex))
	       (res   '()))
      (if (eq? token 'eof)
	  (reverse! res)
	  (Loop (lexer-next-token lex)
		(cons token res))))))


(define xml
  (new language
       (name "xml")
       (fontifier xml-fontifier)
       (extractor #f)))

