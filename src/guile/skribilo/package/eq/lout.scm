;;; lout.scm  --  Lout implementation of the `eq' package.
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

(define-module (skribilo package eq lout)
  :use-module (skribilo package eq)
  :use-module (skribilo ast)
  :autoload   (skribilo output) (output)
  :use-module (skribilo writer)
  :use-module (skribilo engine)
  :use-module (skribilo lib)
  :use-module (skribilo utils syntax)
  :use-module (skribilo utils keywords) ;; `the-options', etc.
  :use-module (srfi srfi-1)
  :use-module (ice-9 optargs))

(fluid-set! current-reader %skribilo-module-reader)



;;;
;;; Initialization.
;;;

(let ((lout (find-engine 'lout)))
  (if (not lout)
      (skribe-error 'eq "Lout engine not found" lout)
      (let ((includes (engine-custom lout 'includes)))
	;; Append the `eq' include file
	(engine-custom-set! lout 'includes
			    (string-append includes "\n"
					   "@SysInclude { eq }\n")))))


;;;
;;; Simple markup writers.
;;;

(markup-writer 'eq-display (find-engine 'lout)
   :before "\n@BeginAlignedDisplays\n"
   :after  "\n@EndAlignedDisplays\n")

(markup-writer 'eq (find-engine 'lout)
   :options '(:inline? :align-with :div-style :mul-style)
   :before (lambda (node engine)
             (let* ((parent (ast-parent node))
                    (displayed? (is-markup? parent 'eq-display)))
               (format #t "~a{ "
                       (if (and displayed? (not (*embedded-renderer*)))
                           "\n@IAD " ""))))
   :action (lambda (node engine)
	     (display (if (inline-equation? node)
			  "@OneRow @OneCol @E { "
			  "@Eq { "))
	     (let ((eq (markup-body node)))
	       ;;(fprint (current-error-port) "eq=" eq)
	       (output eq engine)))
   :after  " } }")


;; Scaled parenthesis.  We could use `pmatrix' here but it precludes
;; line-breaking within equations.
(define %left-paren  "{ Base @Font @VScale \"(\" }")
(define %right-paren "{ Base @Font @VScale \")\" }")

(define (div-style->lout style)
  (case style
    ((over)     "over")
    ((fraction) "frac")
    ((div)      "div")
    ((slash)    "slash")
    (else
     (error "unsupported div style" style))))

(define (mul-style->lout style)
  (case style
    ((space)    "")
    ((cross)    "times")
    ((asterisk) "*")
    ((dot)      "cdot")
    (else
     (error "unsupported mul style" style))))


(define-macro (simple-lout-markup-writer sym . args)
  (let* ((lout-name (if (null? args)
			(symbol->string sym)
			(car args)))
	 (parentheses? (if (or (null? args) (null? (cdr args)))
			   #t
			   (cadr args)))
	 (precedence (operator-precedence sym))

	 (open-par (if parentheses?
                       `(if need-paren? %left-paren "")
                       ""))
	 (close-par (if parentheses?
                        `(if need-paren? %right-paren "")
                        "")))

    `(markup-writer ',(symbol-append 'eq: sym) (find-engine 'lout)
        :action (lambda (node engine)
                  (let* ((lout-name ,(if (string? lout-name)
                                         lout-name
                                         `(,lout-name node
                                                      engine)))
                         (eq        (ast-parent node))
                         (eq-parent (ast-parent eq)))

                    (let loop ((operands (markup-body node))
                               (first? #t))
                      (if (null? operands)
                          #t
                          (let* ((align?
                                  (and first?
                                       (is-markup? eq-parent 'eq-display)
                                       (eq? ',sym
                                            (markup-option eq :align-with))))
                                 (op (car operands))
                                 (eq-op? (equation-markup? op))
                                 (need-paren?
                                  (and eq-op?
                                       (>= (operator-precedence
                                            (equation-markup-name->operator
                                             (markup-markup op)))
                                           ,precedence)))
                                 (column (port-column (current-output-port))))

                            ;; Work around Lout's limitations...
                            (if (> column 1000) (display "\n"))

                            (display (string-append " { " ,open-par))
                            (output op engine)
                            (display (string-append ,close-par " }"))
                            (if (pair? (cdr operands))
                                (display (string-append " "
                                                        (if align? "^" "")
                                                        lout-name
                                                        " ")))
                            (loop (cdr operands) #f)))))))))


;; `+' and `*' have higher precedence than `-', `/', `=', etc., so their
;; operands do not need to be enclosed in parentheses.  OTOH, since we use a
;; horizontal bar of `/', we don't need to parenthesize its arguments.


(simple-lout-markup-writer +)
(simple-lout-markup-writer - "-")

(simple-lout-markup-writer *
                           (lambda (n e)
                             ;; Obey either the per-node `:mul-style' or the
                             ;; top-level one.
                             (mul-style->lout
                              (or (markup-option n :mul-style)
                                  (let ((eq (ast-parent n)))
                                    (markup-option eq :mul-style))))))

(simple-lout-markup-writer /
                           (lambda (n e)
                             ;; Obey either the per-node `:div-style' or the
                             ;; top-level one.
                             (div-style->lout
                              (or (markup-option n :div-style)
                                  (let ((eq (ast-parent n)))
                                    (markup-option eq :div-style)))))
                           #f)
(simple-lout-markup-writer =)
(simple-lout-markup-writer <)
(simple-lout-markup-writer >)
(simple-lout-markup-writer <=)
(simple-lout-markup-writer >=)

(define-macro (binary-lout-markup-writer sym lout-name)
  `(markup-writer ',(symbol-append 'eq: sym) (find-engine 'lout)
     :action (lambda (node engine)
	       (let ((body (markup-body node)))
		 (if (= (length body) 2)
		     (let* ((first (car body))
			    (second (cadr body))
			    (parentheses? (equation-markup? first)))
		       (display " { { ")
		       (if parentheses? (display %left-paren))
		       (output first engine)
		       (if parentheses? (display %right-paren))
		       (display ,(string-append " } " lout-name " { "))
		       (output second engine)
		       (display " } } "))
		     (skribe-error ,(symbol-append 'eq: sym)
				   "wrong number of arguments"
				   body))))))

(binary-lout-markup-writer expt  "sup")
(binary-lout-markup-writer in    "element")
(binary-lout-markup-writer notin "notelement")

(markup-writer 'eq:apply (find-engine 'lout)
   :action (lambda (node engine)
	     (let ((func (car (markup-body node))))
	       (output func engine)
	       (display %left-paren)
	       (let loop ((operands (cdr (markup-body node))))
		 (if (null? operands)
		     #t
		     (begin
		       (output (car operands) engine)
		       (if (not (null? (cdr operands)))
			   (display ", "))
		       (loop (cdr operands)))))
	       (display %right-paren))))


(markup-writer 'eq:limit (find-engine 'lout)
   :action (lambda (node engine)
             (let ((body  (markup-body node))
                   (var   (markup-option node :var))
                   (limit (markup-option node :limit)))
               (display "{ lim from { ")
               (output var engine)
               (display " --> ")
               (output limit engine)
               (display (string-append " } } @VContract { " %left-paren))
               (output body engine)
               (display (string-append %right-paren " } ")))))

(markup-writer 'eq:combinations (find-engine 'lout)
   :action (lambda (node engine)
             (let ((of    (markup-option node :of))
                   (among (markup-option node :among)))
               (display " ` { matrix atleft { lpar } atright { rpar } { ")
               (display "row col { ")
               (output of engine)
               (display " } row col { ")
               (output among engine)
               (display " } } } `\n"))))


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
		     (display (if sup " on { " " sub { "))
		     (output sub engine)
		     (display " } ")))
	       (display " } "))))


;;;
;;; Sets.
;;;

(markup-writer 'eq:set (find-engine 'lout)
   ;; Take the elements of the set and enclose them into braces.
   :action (lambda (node engine)
             (define (printable elem)
               (if (eq? elem '...)
                   (symbol "ellipsis")
                   elem))

             (display " brmatrix { ")
             (pair-for-each (lambda (pair)
                              (let ((elem (printable (car pair))))
                                (output elem engine)
                                (if (not (null? (cdr pair)))
                                    (output ", " engine))))
                            (markup-body node))
             (display " } ")))


;;; arch-tag: 2a1410e5-977e-4600-b781-3d57f4409b35
