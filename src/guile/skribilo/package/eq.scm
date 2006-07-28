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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;;; USA.

(define-module (skribilo package eq)
  :autoload   (skribilo ast)    (markup?)
  :autoload   (skribilo output) (output)
  :use-module (skribilo writer)
  :use-module (skribilo engine)
  :use-module (skribilo lib)
  :use-module (skribilo utils syntax)
  :use-module (skribilo module)
  :use-module (skribilo utils keywords) ;; `the-options', etc.
  :autoload   (skribilo package base) (it symbol sub sup)
  :autoload   (skribilo engine lout) (lout-illustration)
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
  '(/ * + - = != ~= < > <= >= sqrt expt sum product script
    in notin apply))

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


(define (make-fast-member-predicate lst)
  (let ((h (make-hash-table)))
    ;; initialize a hash table equivalent to LST
    (for-each (lambda (s) (hashq-set! h s #t)) lst)

    ;; the run-time, fast, definition
    (lambda (sym)
      (hashq-ref h sym #f))))

(define-public known-operator? (make-fast-member-predicate %operators))
(define-public known-symbol? (make-fast-member-predicate %symbols))

(define-public equation-markup-name?
  (make-fast-member-predicate (map (lambda (s)
				     (symbol-append 'eq: s))
				   %operators)))

(define-public (equation-markup? m)
  "Return true if @var{m} is an instance of one of the equation sub-markups."
  (and (markup? m)
       (equation-markup-name? (markup-markup m))))

(define-public (equation-markup-name->operator m)
  "Given symbol @var{m} (an equation markup name, e.g., @code{eq:+}), return
a symbol representing the mathematical operator denoted by @var{m} (e.g.,
@code{+})."
  (if (equation-markup-name? m)
      (string->symbol (let ((str (symbol->string m)))
			(substring str
				   (+ 1 (string-index str #\:))
				   (string-length str))))
      #f))


;;;
;;; Operator precedence.
;;;

(define %operator-precedence
  ;; FIXME: This needs to be augmented.
  '((+ . 1)
    (- . 1)
    (* . 2)
    (/ . 2)
    (sum . 3)
    (product . 3)
    (= . 0)
    (< . 0)
    (> . 0)
    (<= . 0)
    (>= . 0)))

(define-public (operator-precedence op)
  (let ((p (assq op %operator-precedence)))
    (if (pair? p) (cdr p) 0)))



;;;
;;; Turning an S-exp into an `eq' markup.
;;;

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

(define-markup (eq :rest opts :key (ident #f) (inline? #f)
		                   (renderer #f) (class "eq"))
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

(define-markup (eq:apply :rest opts :key (ident #f) (class "eq:apply"))
  ;; This markup may receive either a list of arguments or arguments
  ;; compatible with the real `apply'.  Note: the real `apply' can take N
  ;; non-list arguments but the last one has to be a list.
  (new markup
       (markup 'eq:apply)
       (ident (or ident (symbol->string (gensym "eq:apply"))))
       (options (the-options opts))
       (body (let loop ((body (the-body opts))
			(result '()))
	       (if (null? body)
		   (reverse! result)
		   (let ((first (car body)))
		     (if (list? first)
			 (if (null? (cdr body))
			     (append (reverse! result) first)
			     (skribe-error 'eq:apply
					   "wrong argument type"
					   body))
			 (loop (cdr body) (cons first result)))))))))



;;;
;;; Text-based rendering.
;;;


(markup-writer 'eq (find-engine 'base)
   :action (lambda (node engine)
	     ;; The `:renderer' option should be a symbol (naming an engine
	     ;; class) or an engine or engine class.  This allows the use of
	     ;; another engine to render equations.  For instance, equations
	     ;; may be rendered using the Lout engine within an HTML
	     ;; document.
	     (let ((renderer (markup-option node :renderer)))
	       (cond ((not renderer) ;; default: use the current engine
		      (output (it (markup-body node)) engine))
		     ((symbol? renderer)
		      (case renderer
			;; FIXME: We should have an `embed' slot for each
			;; engine class similar to `lout-illustration'.
			((lout)
			 (let ((lout-code
				(with-output-to-string
				  (lambda ()
				    (output node (find-engine 'lout))))))
			   (output (lout-illustration
				    :ident (markup-ident node)
				    lout-code)
				   engine)))
			(else
			 (skribe-error 'eq "invalid renderer" renderer))))
		     ;; FIXME: `engine?' and `engine-class?'
		     (else
		      (skribe-error 'eq "`:renderer' -- wrong argument type"
				    renderer))))))

(define-macro (simple-markup-writer op . obj)
  ;; Note: The text-only rendering is less ambiguous if we parenthesize
  ;; without taking operator precedence into account.
  (let ((precedence (operator-precedence op)))
    `(markup-writer ',(symbol-append 'eq: op) (find-engine 'base)
       :action (lambda (node engine)
		  (let loop ((operands (markup-body node)))
		   (if (null? operands)
		       #t
		       (let* ((o (car operands))
			      (nested-eq? (equation-markup? o))
			      (need-paren?
			       (and nested-eq?
; 				    (< (operator-precedence
; 					(equation-markup-name->operator
; 					 (markup-markup o)))
; 				       ,precedence)
				    )
			       ))

			 (display (if need-paren? "(" ""))
			 (output o engine)
			 (display (if need-paren? ")" ""))
			 (if (pair? (cdr operands))
			     (begin
			       (display " ")
			       (output ,(if (null? obj)
					    (symbol->string op)
					    (car obj))
				       engine)
			       (display " ")))
			 (loop (cdr operands)))))))))

(simple-markup-writer +)
(simple-markup-writer -)
(simple-markup-writer /)
(simple-markup-writer * (symbol "times"))

(simple-markup-writer =)
(simple-markup-writer != (symbol "neq"))
(simple-markup-writer ~= (symbol "approx"))
(simple-markup-writer <)
(simple-markup-writer >)
(simple-markup-writer >= (symbol "ge"))
(simple-markup-writer <= (symbol "le"))

(markup-writer 'eq:sqrt (find-engine 'base)
   :action (lambda (node engine)
	     (display "sqrt(")
	     (output (markup-body node) engine)
	     (display ")")))

(define-macro (simple-binary-markup-writer op obj)
  `(markup-writer ',(symbol-append 'eq: op) (find-engine 'base)
     :action (lambda (node engine)
	       (let ((body (markup-body node)))
		 (if (= (length body) 2)
		     (let ((first (car body))
			   (second (cadr body)))
		       (display (if (equation-markup? first) "(" " "))
		       (output first engine)
		       (display (if (equation-markup? first) ")" " "))
		       (output ,obj engine)
		       (display (if (equation-markup? second) "(" ""))
		       (output second engine)
		       (display (if (equation-markup? second) ")" "")))
		     (skribe-error ',(symbol-append 'eq: op)
				   "wrong argument type"
				   body))))))

(markup-writer 'eq:expt (find-engine 'base)
   :action (lambda (node engine)
	     (let ((body (markup-body node)))
		 (if (= (length body) 2)
		     (let ((first (car body))
			   (second (cadr body)))
		       (display (if (equation-markup? first) "(" ""))
		       (output first engine)
		       (display (if (equation-markup? first) ")" ""))
		       (output (sup second) engine))))))

(simple-binary-markup-writer in    (symbol "in"))
(simple-binary-markup-writer notin (symbol "notin"))

(markup-writer 'eq:apply (find-engine 'base)
   :action (lambda (node engine)
	     (let ((func (car (markup-body node))))
	       (output func engine)
	       (display "(")
	       (let loop ((operands (cdr (markup-body node))))
		 (if (null? operands)
		     #t
		     (begin
		       (output (car operands) engine)
		       (if (not (null? (cdr operands)))
			   (display ", "))
		       (loop (cdr operands)))))
	       (display ")"))))

(markup-writer 'eq:sum (find-engine 'base)
   :action (lambda (node engine)
	     (let ((from (markup-option node :from))
		   (to (markup-option node :to)))
	       (output (symbol "Sigma") engine)
	       (display "(")
	       (output from engine)
	       (display ", ")
	       (output to engine)
	       (display ", ")
	       (output (markup-body node) engine)
	       (display ")"))))

(markup-writer 'eq:prod (find-engine 'base)
   :action (lambda (node engine)
	     (let ((from (markup-option node :from))
		   (to (markup-option node :to)))
	       (output (symbol "Pi") engine)
	       (display "(")
	       (output from engine)
	       (display ", ")
	       (output to engine)
	       (display ", ")
	       (output (markup-body node) engine)
	       (display ")"))))

(markup-writer 'eq:script (find-engine 'base)
   :action (lambda (node engine)
	     (let ((body (markup-body node))
		   (sup* (markup-option node :sup))
		   (sub* (markup-option node :sub)))
	       (output body engine)
	       (output (sup sup*) engine)
	       (output (sub sub*) engine))))




;;;
;;; Initialization.
;;;

(when-engine-is-loaded 'lout
  (lambda ()
    (resolve-module '(skribilo package eq lout))))


;;; arch-tag: 58764650-2684-47a6-8cc7-6288f2b474da

;;; eq.scm ends here
