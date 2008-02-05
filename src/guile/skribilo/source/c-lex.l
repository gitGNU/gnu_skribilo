;;; c-lex.l  --  C fontifier for Skribilo.
;;;
;;; Copyright 2007, 2008  Ludovic Courtès <ludo@gnu.org>
;;; Copyright 2004  Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;;; USA.

;; Comment regexp stolen from http://ostermiller.org/findcomment.html .
comment  /\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/

space	 [ \n\9]
letter   [_a-zA-Z]
alphanum [_a-zA-Z0-9]

%%

;; Strings
\"[^\"]*\"		(new markup
			     (markup '&source-string)
			     (body yytext))
;; Comments
{comment}               (let* ((not-line (char-set-complement (char-set #\newline)))
                               (lines    (string-tokenize yytext not-line)))
                          (reverse!
                           (pair-fold (lambda (line* result)
                                        (let* ((line  (car line*))
                                               (last? (null? (cdr line*)))
                                               (markup
                                                (new markup
                                                   (markup '&source-line-comment)
                                                   (body   line))))
                                          (if last?
                                              (cons markup result)
                                              (cons* (string #\newline)
                                                     markup result))))
                                      '()
                                      lines)))

//.*			(new markup
			     (markup '&source-line-comment)
			     (body   yytext))

;; Identifiers (only letters since we are interested in keywords only)
[_a-zA-Z]+		(let* ((ident (string->symbol yytext))
			       (tmp   (memq ident (*the-keys*))))
			  (if tmp
			      (new markup
				   (markup '&source-module)
				   (body yytext))
			      yytext))

;; Regular text (excluding `/' and `*')
[^\"a-zA-Z/*]+		(begin yytext)

;; `/' and `*' alone.
/[^\*]                  (begin yytext)
\*[^/]                  (begin yytext)


<<EOF>>			'eof
<<ERROR>>		(skribe-error 'c-fontifier "Parse error" yytext)
