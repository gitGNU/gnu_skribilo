;;;;
;;;; c-lex.l			-- C fontifier for Skribe
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
;;;;    Creation date:  6-Mar-2004 15:35 (eg)
;;;; Last file update:  7-Mar-2004 00:10 (eg)
;;;;

space	 [ \n\9]
letter   [_a-zA-Z]
alphanum [_a-zA-Z0-9]

%%

;; Strings
\"[^\"]*\"		(new markup
			     (markup '&source-string)
			     (body yytext))
;;Comments
/\*.*\*/		(new markup
			     (markup '&source-line-comment)
			     (body   yytext))
//.*			(new markup
			     (markup '&source-line-comment)
			     (body   yytext))

;; Identifiers (only letters since we are interested in keywords only)
[_a-zA-Z]+		(let* ((ident (string->symbol yytext))
			       (tmp   (memq  ident *the-keys*)))
			  (if tmp
			      (new markup
				   (markup '&source-module)
				   (body yytext))
			      yytext))

;; Regular text
[^\"a-zA-Z]+		(begin yytext)



<<EOF>>			'eof
<<ERROR>>		(skribe-error 'lisp-fontifier "Parse error" yytext)

				   
			    
			      
			      

			  