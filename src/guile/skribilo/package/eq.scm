;;; eq.scm  --  An equation formatting package.
;;;
;;; Copyright 2005, 2006  Ludovic Courtès <ludovic.courtes@laas.fr>
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

(define-module (skribilo package eq)
  :autoload   (skribilo ast)    (markup?)
  :autoload   (skribilo output) (output)
  :use-module (skribilo writer)
  :use-module (skribilo engine)
  :use-module (skribilo lib)
  :use-module (skribilo utils syntax)
  :use-module (skribilo module)
  :use-module (skribilo skribe utils) ;; `the-options', etc.
  :use-module (ice-9 optargs))

;;; Author: Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; This package defines a set of markups for formatting equations.  The user
;;; may either use the standard Scheme prefix notation to represent
;;; equations, or directly use the specific markups (which looks more
;;; verbose).
;;;
;;; FIXME: This is incomplete.
;;;
;;; Code:

(fluid-set! current-reader %skribilo-module-reader)



;;;
;;; Utilities.
;;;

(define %operators
  '(/ * + - = != ~= < > <= >= sqrt expt sum product script in notin))

(define %symbols
  ;; A set of symbols that are automatically recognized within an `eq' quoted
  ;; list.
  '(;; lower-case Greek
    alpha beta gamma delta epsilon zeta eta theta iota kappa
    lambda mu nu xi omicron pi rho sigma tau upsilon phi chi omega

    ;; upper-case Greek
    Alpha Beta Gamma Delta Epsilon Zeta Eta Theta Iota Kappa
    Lambda Mu Nu Xi Omicron Pi Rho Sigma Tau Upsilon Phi Chi Omega

    ;; Hebrew
    alef

    ;; mathematics
    ellipsis weierp image real forall partial exists
    emptyset infinity in notin nabla nipropto angle and or cap cup
    sim cong approx neq equiv le ge subset supset subseteq supseteq
    oplus otimes perp mid lceil rceil lfloor rfloor langle rangle))

(define %rebindings
  (map (lambda (sym)
	 (list sym (symbol-append 'eq: sym)))
       %operators))

(define (make-fast-member-predicate lst)
  (let ((h (make-hash-table)))
    ;; initialize a hash table equivalent to LST
    (for-each (lambda (s) (hashq-set! h s #t)) lst)

    ;; the run-time, fast, definition
    (lambda (sym)
      (hashq-ref h sym #f))))

(define-public known-operator? (make-fast-member-predicate %operators))
(define-public known-symbol? (make-fast-member-predicate %symbols))

(define-public (equation-markup? m)
  "Return true if @var{m} is an instance of one of the equation sub-markups."
  (define eq-sym?
    (make-fast-member-predicate (map (lambda (s)
				       (symbol-append 'eq: s))
				     %operators)))
  (and (markup? m)
       (eq-sym? (markup-markup m))))


(define (eq:symbols->strings equation)
  "Turn symbols located in non-@code{car} positions into strings."
  (cond ((list? equation)
	 (if (or (null? equation) (null? (cdr equation)))
	     equation
	     (cons (car equation) ;; XXX: not tail-recursive
		   (map eq:symbols->strings (cdr equation)))))
	((symbol? equation)
	 (if (known-symbol? equation)
	     `(symbol ,(symbol->string equation))
	     (symbol->string equation)))
	(else equation)))

(define-public (eq-evaluate equation)
  "Evaluate @var{equation}, an sexp (list) representing an equation, e.g.
@code{'(+ a (/ b 3))}."
  (eval `(let ,%rebindings ,(eq:symbols->strings equation))
	(current-module)))


;;;
;;; Markup.
;;;

(define-markup (eq :rest opts :key (ident #f) (class "eq"))
  (new markup
       (markup 'eq)
       (ident (or ident (symbol->string (gensym "eq"))))
       (options (the-options opts))
       (body (let loop ((body (the-body opts))
			(result '()))
	       (if (null? body)
		   result
		   (loop (cdr body)
			 (if (markup? (car body))
			     (car body)  ;; the `eq:*' markups were used
					 ;; directly
			     (eq-evaluate (car body))) ;; a quoted list was
						       ;; passed
			     ))))))

(define-simple-markup eq:/)
(define-simple-markup eq:*)
(define-simple-markup eq:+)
(define-simple-markup eq:-)

(define-simple-markup eq:=)
(define-simple-markup eq:!=)
(define-simple-markup eq:~=)
(define-simple-markup eq:<)
(define-simple-markup eq:>)
(define-simple-markup eq:>=)
(define-simple-markup eq:<=)

(define-simple-markup eq:sqrt)
(define-simple-markup eq:expt)

(define-markup (eq:sum :rest opts :key (ident #f) (class "eq:sum")
		                       (from #f) (to #f))
  (new markup
       (markup 'eq:sum)
       (ident (or ident (symbol->string (gensym "eq:sum"))))
       (options (the-options opts))
       (body (the-body opts))))

(define-markup (eq:product :rest opts :key (ident #f) (class "eq:product")
			                   (from #f) (to #f))
  (new markup
       (markup 'eq:product)
       (ident (or ident (symbol->string (gensym "eq:product"))))
       (options (the-options opts))
       (body (the-body opts))))

(define-markup (eq:script :rest opts :key (ident #f) (class "eq:script")
			                  (sub #f) (sup #f))
  (new markup
       (markup 'eq:script)
       (ident (or ident (symbol->string (gensym "eq:script"))))
       (options (the-options opts))
       (body (the-body opts))))

(define-simple-markup eq:in)
(define-simple-markup eq:notin)


;;;
;;; Text-only implementation.
;;;

(markup-writer 'eq (find-engine 'base)
   :action (lambda (node engine)
	      (output (apply it (markup-body node)) engine)))

(markup-writer 'eq:/ (find-engine 'base)
   :action (lambda (node engine)
	      (let loop ((operands (markup-body node)))
	       (if (null? operands)
		   #t
		   (begin
		     (display " ")
		     (output (car operands) engine)
		     (display " ")
		     (if (pair? (cdr operands))
			 (display " / "))
		     (loop (cdr operands)))))))


;;;
;;; Initialization.
;;;

(when-engine-is-loaded 'lout
  (lambda ()
    (resolve-module '(skribilo package eq lout))))


;;; arch-tag: 58764650-2684-47a6-8cc7-6288f2b474da

;;; eq.scm ends here
