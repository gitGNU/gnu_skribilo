;;; c.scm -- C fontifier.
;;;
;;; Copyright 2004  Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
;;; Copyright 2007  Ludovic Courtès <ludo@chbouib.org>
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;;; USA.

(define-module (skribilo source c)
  :use-module (skribilo lib)
  :use-module (skribilo utils syntax)
  :use-module (skribilo source c-lex)		;; SILex generated
  :use-module (skribilo source parameters)
  :use-module (srfi srfi-39)
  :export (c c-language java))

(skribilo-module-syntax)


;;;
;;; Generic fontifier.
;;;

(define (fontifier s)
  (lexer-init 'port (open-input-string s))
  (let loop ((token (lexer))
             (res   '()))
    (if (eq? token 'eof)
        (reverse! res)
        (loop (lexer)
              (cons token res)))))


;;;
;;; C.
;;;

(define %c-keys
  '(for while return break continue void do if else typedef struct union
    goto switch case static extern default))

(define (c-fontifier s)
  (parameterize ((*the-keys* %c-keys))
    (fontifier s)))

(define c
  (new language
       (name "C")
       (fontifier c-fontifier)
       (extractor #f)))

(define c-language
  ;; This alias is defined for the user's convenience.
  c)


;;;
;;; Java.
;;;

(define %java-keys
  (append %c-keys
          '(public final class throw catch)))

(define (java-fontifier s)
  (parameterize ((*the-keys* %java-keys))
    (fontifier s)))

(define java
  (new language
       (name "java")
       (fontifier java-fontifier)
       (extractor #f)))
