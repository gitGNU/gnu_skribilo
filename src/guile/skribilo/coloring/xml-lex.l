;;;;							-*- Scheme -*-
;;;;
;;;; xml-lex.l			-- SILex input for the XML languages
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
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, 
;;;; USA.
;;;; 
;;;;           Author: Erick Gallesio [eg@essi.fr]
;;;;    Creation date: 21-Dec-2003 17:19 (eg)
;;;; Last file update: 21-Dec-2003 22:38 (eg)
;;;;

space	[ \n\9]

%%

;; Strings
\"[^\"]*\"		(new markup
			     (markup '&source-string)
			     (body yytext))
'[^']*'			(new markup
			     (markup '&source-string)
			     (body yytext))

;;Comment
<!--(.|\n)*-->		(new markup
			     (markup '&source-comment)
			     (body   yytext))

;; Markup
<[^>\n ]+|>		(new markup
			     (markup '&source-module)
			     (body yytext))

;; Regular text
[^<>\"']+			(begin yytext)


<<EOF>>			'eof
<<ERROR>>		(skribe-error 'xml-fontifier "Parse error" yytext)






			    
				
					