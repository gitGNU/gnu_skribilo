;;; lisp-lex.l -- SILex input for the Lisp Languages
;;;
;;; Copyright 2003, 2004  Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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


space	[ \n\9]
letter  [#?!_:a-zA-Z\-]
digit   [0-9]


%%
;; Strings
\"[^\"]*\"		(new markup
			     (markup '&source-string)
			     (body yytext))

;;Comment
\;.*			(new markup
			     (markup '&source-line-comment)
			     (body   yytext))

;; Skribe text (i.e. [....])
\[|\]		        (if (*bracket-highlight*)
			    (new markup
				 (markup '&source-bracket)
				 (body   yytext))
			    yytext)
;; Spaces & parenthesis
[ \n\9\(\)]+		(begin
			  yytext)

;; Identifier (real syntax is slightly more complicated but we are
;; interested here in the identifiers that we will fontify)
[^\;\"\[\] \n\9\(\)]+	(let ((c (string-ref yytext 0)))
			  (cond
			    ((or (char=? c #\:)
				 (char=? (string-ref yytext
						     (- (string-length yytext) 1))
					 #\:))
			     ;; Scheme keyword
			     (new markup
				  (markup '&source-type)
				  (body yytext)))
			    ((char=? c #\<)
			       ;; STklos class
			       (let* ((len (string-length yytext))
				      (c   (string-ref yytext (- len 1))))
				 (if (char=? c #\>)
				     (if (*class-highlight*)
					 (new markup
					      (markup '&source-module)
					      (body yytext))
					 yytext)		; no
				     yytext)))			; no
			    (else
			       (let ((tmp (assoc (string->symbol yytext)
						 (*the-keys*))))
				 (if tmp
				     (new markup
					  (markup (cdr tmp))
					  (body yytext))
				     yytext)))))


<<EOF>>			'eof
<<ERROR>>		(skribe-error 'lisp-fontifier "Parse error" yytext)


; LocalWords:  fontify
