;;; lout.scm  --  Lout implementation of the `eq' package.
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

(define-module (skribilo package eq lout)
  :use-module (skribilo package eq)
  :use-module (skribilo ast)
  :autoload   (skribilo output) (output)
  :use-module (skribilo writer)
  :use-module (skribilo engine)
  :use-module (skribilo lib)
  :use-module (skribilo utils syntax)
  :use-module (skribilo skribe utils) ;; `the-options', etc.
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


(markup-writer 'eq (find-engine 'lout)
   :before "{ @Eq { "
   :action (lambda (node engine)
	      (let ((eq (markup-body node)))
		 ;(fprint (current-error-port) "eq=" eq)
		 (output eq engine)))
   :after  " } }")


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

(define-macro (binary-lout-markup-writer sym lout-name)
  `(markup-writer ',(symbol-append 'eq: sym) (find-engine 'lout)
     :action (lambda (node engine)
	       (let ((body (markup-body node)))
		 (if (= (length body) 2)
		     (let* ((first (car body))
			    (second (cadr body))
			    (parentheses? (equation-markup? first)))
		       (display " { { ")
		       (if parentheses? (display "("))
		       (output first engine)
		       (if parentheses? (display ")"))
		       (display ,(string-append " } " lout-name " { "))
		       (output second engine)
		       (display " } } "))
		     (skribe-error ,(symbol-append 'eq: sym)
				   "wrong number of arguments"
				   body))))))

(binary-lout-markup-writer expt "sup")
(binary-lout-markup-writer in "element")
(binary-lout-markup-writer notin "notelement")

(markup-writer 'eq:apply (find-engine 'lout)
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


;;; arch-tag: 2a1410e5-977e-4600-b781-3d57f4409b35
