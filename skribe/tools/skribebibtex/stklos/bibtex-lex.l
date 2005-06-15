;;;;							-*- Scheme -*-
;;;; bibtex-lex.l	-- SILex input for BibTeX
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
;;;; Last file update: 25-Oct-2004 20:16 (eg)
;;;;


space	[ \n\9]
alpha   [-+a-zA-ZàâäéèêëîïôöûüùÀÂÄÉÈÊËÎÏÔÖÛÜÙ./:()?!'&_~]

%%

;; Spaces
{space}+			(list 'BLANK)
;; Comment
\%.*$				(yycontinue)
;; equal sign
=				(list 'EQUAL)
;; Open Bracket
\{				(list 'LBRACKET)
;; Close Bracket
\}				(list 'RBRACKET)
;; Comma
, 				(list 'COMMA)
;; Strings
\"[^\"]*\"			(list 'STRING yytext)
;; Commands
@{alpha}+			(let* ((str (string-downcase
					     (substring yytext 1
							(string-length yytext))))
				       (sym (string->symbol str)))
				  (case sym
				    ((string) (list 'BIBSTRING))
				    (else     (list 'BIBITEM sym))))
;; Ident
{alpha}({alpha}|[0-9])*		(list 'IDENT yytext)
;; Number
[0-9]+				(list 'NUMBER yytext)
;; Diacritic 
\\['`^\"][aeiouAEIOU] 		(lex-char (string-ref yytext 1)
					  (string-ref yytext 2))
\{\\['`^\"][aeiouAEIOU]\} 	(lex-char (string-ref yytext 2)
					  (string-ref yytext 3))

;; Unrecognized character
.				(begin
				  (format (current-error-port)
					  "Skipping character ~S\n" yytext)
				  (yycontinue))

;;;; ======================================================================
<<EOF>>			'*eoi*
<<ERROR>>		(error 'bibtex-lexer "Parse error" yytext)


