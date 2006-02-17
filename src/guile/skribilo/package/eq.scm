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
  '(/ * + - = != ~= < > <= >= sqrt expt sum product script))

(define %rebindings
  (map (lambda (sym)
	 (list sym (symbol-append 'eq: sym)))
       %operators))


(define (eq:symbols->strings equation)
  "Turn symbols located in non-@code{car} positions into strings."
  (cond ((list? equation)
	 (if (or (null? equation) (null? (cdr equation)))
	     equation
	     (cons (car equation) ;; XXX: not tail-recursive
		   (map eq:symbols->strings (cdr equation)))))
	((symbol? equation)
	 (symbol->string equation))
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



;;;
;;; Lout implementation
;;;

(let ((lout (find-engine 'lout)))
  (if (not lout)
      (skribe-error 'eq "Lout engine not found" lout)
      (let ((includes (engine-custom lout 'includes)))
	;; Append the `eq' include file
	(engine-custom-set! lout 'includes
			    (string-append includes "\n"
					   "@SysInclude { eq }\n")))))

;; FIXME:  Reimplement the `symbol' writer so that `@Sym' is not used within
;; equations (e.g. output `alpha' instead of `{ @Sym alpha }').

(markup-writer 'eq (find-engine 'lout)
   :before "\n@Eq { "
   :action (lambda (node engine)
	      (let ((eq (markup-body node)))
		 ;(fprint (current-error-port) "eq=" eq)
		 (output eq engine)))
   :after  " }\n")


;;
;; `+' and `-' have lower precedence than `*', `/', `=', etc., so their
;; operands do not need to be enclosed in braces.
;;

(markup-writer 'eq:+ (find-engine 'lout)
   :action (lambda (node engine)
	      (let loop ((operands (markup-body node)))
		 (if (null? operands)
		     #t
		     (begin
		       ;; no braces
		       (output (car operands) engine)
		       (if (pair? (cdr operands))
			   (display " + "))
		       (loop (cdr operands)))))))

(markup-writer 'eq:- (find-engine 'lout)
   :action (lambda (node engine)
	      (let loop ((operands (markup-body node)))
		 (if (null? operands)
		     #t
		     (begin
		       ;; no braces
		       (output (car operands) engine)
		       (if (pair? (cdr operands))
			   (display " - "))
		       (loop (cdr operands)))))))

(define-macro (simple-lout-markup-writer sym . lout-name)
  `(markup-writer ',(symbol-append 'eq: sym)
		  (find-engine 'lout)
      :action (lambda (node engine)
		(let loop ((operands (markup-body node)))
		  (if (null? operands)
		      #t
		      (begin
			(display " { ")
			(output (car operands) engine)
			(display " }")
			(if (pair? (cdr operands))
			    (display ,(string-append " "
						     (if (null? lout-name)
							 (symbol->string sym)
							 (car lout-name))
						     " ")))
			(loop (cdr operands))))))))

(simple-lout-markup-writer * "times")
(simple-lout-markup-writer / "over")
(simple-lout-markup-writer =)
(simple-lout-markup-writer <)
(simple-lout-markup-writer >)
(simple-lout-markup-writer <=)
(simple-lout-markup-writer >=)

(markup-writer 'eq:expt (find-engine 'lout)
   :action (lambda (node engine)
	     (let ((body (markup-body node)))
	       (if (= (length body) 2)
		   (let ((base (car body))
			 (expt (cadr body)))
		     (display " { { ")
		     (if (markup? base) (display "("))
		     (output base engine)
		     (if (markup? base) (display ")"))
		     (display " } sup { ")
		     (output expt engine)
		     (display " } } "))
		   (skribe-error 'eq:expt "wrong number of arguments"
				 body)))))


;;;
;;; Sums, products, integrals, etc.
;;;

(define-macro (range-lout-markup-writer sym lout-name)
  `(markup-writer ',(symbol-append 'eq: sym) (find-engine 'lout)
      :action (lambda (node engine)
		(let ((from (markup-option node :from))
		      (to (markup-option node :to))
		      (body (markup-body node)))
		  (display ,(string-append " { big " lout-name
					   " from { "))
		  (output from engine)
		  (display " } to { ")
		  (output to engine)
		  (display " } { ")
		  (output body engine)
		  (display " } } ")))))

(range-lout-markup-writer sum "sum")
(range-lout-markup-writer product "prod")

(markup-writer 'eq:script (find-engine 'lout)
   :action (lambda (node engine)
	     (let ((body (markup-body node))
		   (sup (markup-option node :sup))
		   (sub (markup-option node :sub)))
	       (display " { { ")
	       (output body engine)
	       (display " } ")
	       (if sup
		   (begin
		     (display (if sub " supp { " " sup { "))
		     (output sup engine)
		     (display " } ")))
	       (if sub
		   (begin
		     (display " on { ")
		     (output sub engine)
		     (display " } ")))
	       (display " } "))))


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

;;; arch-tag: 58764650-2684-47a6-8cc7-6288f2b474da

;;; eq.scm ends here
