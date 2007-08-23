;;; eq.scm  --  An equation formatting package.
;;;
;;; Copyright 2005, 2006, 2007  Ludovic Courtès <ludovic.courtes@laas.fr>
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
  :autoload   (skribilo ast)    (markup? find1-up)
  :autoload   (skribilo output) (output)
  :use-module (skribilo writer)
  :use-module (skribilo engine)
  :use-module (skribilo lib)
  :use-module (skribilo utils syntax)
  :use-module (skribilo utils keywords) ;; `the-options', etc.
  :autoload   (skribilo package base) (it symbol sub sup)
  :autoload   (skribilo engine lout)  (lout-illustration)
  :autoload   (skribilo resolve)      (resolve-counter)

  :use-module (srfi srfi-1)
  :use-module (srfi srfi-39)
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

(define-public *embedded-renderer*
  ;; Tells whether an engine is invoked as an embedded renderer or as the
  ;; native engine.
  (make-parameter #f))

(define-public *use-lout-math?*
  ;; Whether the use the Lout's `math' package (new in Lout 3.36) instead
  ;; of `eq'.
  (make-parameter #t))


(define %operators
  '(/ * + - modulo = != ~= < > <= >= sqrt expt sum product script
    in notin apply limit combinations set))

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

(define-public (inline-equation? m)
  "Return @code{#t} if @var{m} is an equation that is to be displayed inline."
  (and (is-markup? m 'eq)
       (let ((i (markup-option m :inline?)))
         (case i
           ((auto)
            (not (find1-up (lambda (n)
                             (is-markup? n 'eq-display))
                           m)))
           ((#t) #t)
           (else #f)))))

(define-public (direct-equation-child? m)
  "Return @code{#t} if @var{m} is a direct child of an @code{eq} markup."
  (let ((parent (ast-parent m)))
    (and (is-markup? parent 'eq)
         (let ((body (markup-body parent)))
           (or (eq? body m)
               (and (list? body)
                    (memq m body)))))))



;;;
;;; Operator precedence.
;;;

(define %operator-precedence
  ;; Taken from http://en.wikipedia.org/wiki/Order_of_operations .
  '((expt . 2)
    (sqrt . 2)

    (* . 3)
    (/ . 3)
    (product . 3)

    (+ . 4)
    (- . 4)
    (sum . 4)

    (< . 6)
    (> . 6)
    (<= . 6)
    (>= . 6)

    (= . 7)
    (!= . 7)
    (~= . 7)))


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


(define-public (equation-number-string equation)
  "Return an appropriate document-wide number for @var{equation}."
  (and (is-markup? equation 'eq)
       (not (inline-equation? equation))
       (let ((num (markup-option equation :number)))
         (and (number? num)
              (let ((chapter (ast-chapter equation)))
                (and (markup? chapter)
                     (string-append
                      (number->string (markup-option chapter :number)) "."
                      (number->string num))))))))


;;;
;;; Markup.
;;;

(define-markup (eq-display :rest opts :key (ident #f) (class "eq-display"))
  (new container
       (markup 'eq-display)
       (ident (or ident (symbol->string (gensym "eq-display"))))
       (class class)
       (loc   &invocation-location)
       (options (the-options opts :ident :class))
       (body (the-body opts))))

(define-markup (eq :rest opts :key (ident #f) (class "eq")
                                   (inline? 'auto) (align-with #f)
		                   (renderer #f) (div-style 'over)
                                   (mul-style 'space)
                                   (number #t))
  (new container
       (markup 'eq)
       (ident (or ident (symbol->string (gensym "eq"))))
       (class class)
       (loc   &invocation-location)
       (options `((:div-style ,div-style) (:align-with ,align-with)
                  (:mul-style ,mul-style) (:inline? ,inline?)
                  (:number ,(cond ((not number)     #f)
                                  ((string? number) number)
                                  (else
                                   (new unresolved
                                      (proc (lambda (n e env)
                                              (let* ((p? (assq 'parent env))
                                                     (p  (and (pair? p?)
                                                              (cadr p?))))
                                                (and (is-markup? p 'eq)
                                                     (not (inline-equation? p))
                                                     (resolve-counter n env
                                                                      'equation
                                                                      number)))))))))
                  ,@(the-options opts
                                 :ident :class :inline?
                                 :div-style :mul-style :align-with
                                 :number)))
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


(define-markup (eq:/ :rest opts :key (ident #f) (div-style #f))
  ;; If no `:div-style' is specified here, obey the top-level one.
  (new markup
       (markup 'eq:/)
       (loc &invocation-location)
       (ident (or ident (symbol->string (gensym "eq:/"))))
       (class #f)
       (options `((:div-style ,div-style)
                  ,@(the-options opts :ident :div-style)))
       (body (the-body opts))))

(define-markup (eq:* :rest opts :key (ident #f) (mul-style #f))
  ;; If no `:mul-style' is specified here, obey the top-level one.
  (new markup
       (markup 'eq:*)
       (loc &invocation-location)
       (ident (or ident (symbol->string (gensym "eq:*"))))
       (class #f)
       (options `((:mul-style ,mul-style)
                  ,@(the-options opts :ident :mul-style)))
       (body (the-body opts))))

(define-simple-markup eq:+)
(define-simple-markup eq:-)
(define-simple-markup eq:modulo)

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
       (loc &invocation-location)
       (ident (or ident (symbol->string (gensym "eq:sum"))))
       (options (the-options opts))
       (body (the-body opts))))

(define-markup (eq:product :rest opts :key (ident #f) (class "eq:product")
			                   (from #f) (to #f))
  (new markup
       (markup 'eq:product)
       (loc &invocation-location)
       (ident (or ident (symbol->string (gensym "eq:product"))))
       (options (the-options opts))
       (body (the-body opts))))

(define-markup (eq:script :rest opts :key (ident #f) (class "eq:script")
			                  (sub #f) (sup #f))
  (new markup
       (markup 'eq:script)
       (loc &invocation-location)
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
       (loc &invocation-location)
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


(define-markup (eq:limit var lim :rest body :key (ident #f))
  (new markup
       (markup 'eq:limit)
       (loc &invocation-location)
       (ident (or ident (symbol->string (gensym "eq:limit"))))
       (options `((:var ,var) (:limit ,lim)
                  ,@(the-options body :ident)))
       (body (the-body body))))

(define-markup (eq:combinations x y :rest opts :key (ident #f))
  (new markup
       (markup 'eq:combinations)
       (loc &invocation-location)
       (ident (or ident (symbol->string (gensym "eq:combinations"))))
       (options `((:of ,x) (:among ,y)
                  ,@(the-options opts :ident)))
       (body (the-body opts))))

(define-markup (eq:set :rest opts :key (ident #f))
  (new markup
       (markup 'eq:set)
       (loc &invocation-location)
       (ident (or ident (symbol->string (gensym "eq:set"))))
       (options '())
       (body (the-body opts))))


;;;
;;; Text-based rendering.
;;;


(markup-writer 'eq-display (find-engine 'base)
   :action (lambda (node engine)
             (for-each (lambda (node)
                         (let ((eq? (is-markup? node 'eq)))
                           (if eq? (output (linebreak) engine))
                           (output node engine)
                           (if eq? (output (linebreak) engine))))
                       (markup-body node))))

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
                      (parameterize ((*embedded-renderer* #t))
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
                           (skribe-error 'eq "invalid renderer" renderer)))))
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
 				    (>= (operator-precedence
                                         (equation-markup-name->operator
                                          (markup-markup o)))
                                        ,precedence)
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

(markup-writer 'eq:limit (find-engine 'base)
   :action (lambda (node engine)
             (let ((body  (markup-body node))
                   (var   (markup-option node :var))
                   (limit (markup-option node :limit)))
               (display "lim (")
               (output var engine)
               (output (symbol "->") engine)
               (output limit engine)
               (display ", ")
               (output body engine)
               (display ")"))))

(markup-writer 'eq:combinations (find-engine 'base)
   :action (lambda (node engine)
             (let ((of    (markup-option node :of))
                   (among (markup-option node :among)))
               (display "combinations(")
               (output of engine)
               (display ", ")
               (output among engine)
               (display ")"))))

(markup-writer 'eq:set (find-engine 'base)
   ;; Take the elements of the set and enclose them into braces.
   :action (lambda (node engine)
             (define (printable elem)
               (if (eq? elem '...)
                   (symbol "ellipsis")
                   elem))

             (output "{ " engine)
             (pair-for-each (lambda (pair)
                              (let ((elem (printable (car pair))))
                                (output elem engine)
                                (if (not (null? (cdr pair)))
                                    (output ", " engine))))
                            (markup-body node))
             (output " }" engine)))



;;;
;;; Initialization.
;;;

(when-engine-is-loaded 'lout
  (lambda ()
    (resolve-module '(skribilo package eq lout))))


;;; arch-tag: 58764650-2684-47a6-8cc7-6288f2b474da

;;; eq.scm ends here
