;;; verify.scm  --  Skribe AST verification.
;;;
;;; Copyright 2003-2004  Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
;;; Copyright 2005  Ludovic Courtès <ludovic.courtes@laas.fr>
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

(define-module (skribilo verify)
  :autoload (skribilo engine) (engine-ident processor-get-engine)
  :autoload (skribilo writer) (writer? writer-options lookup-markup-writer)
  :autoload (skribilo lib)    (skribe-warning/ast skribe-warning
			       skribe-error)
  :export (verify))

(use-modules (skribilo debug)
	     (skribilo ast)
	     (skribilo utils syntax)
	     (oop goops))

(fluid-set! current-reader %skribilo-module-reader)



(define-generic verify)

;;;
;;; CHECK-REQUIRED-OPTIONS
;;;
(define (check-required-options markup writer engine)
  (let ((required-options (slot-ref markup 'required-options))
	(options	  (slot-ref writer 'options))
	(verified?	  (slot-ref writer 'verified?)))
    (or verified?
	(eq? options 'all)
	(begin
	  (for-each (lambda (o)
		      (if (not (memq o options))
			  (skribe-error (engine-ident engine)
					(format #f "option unsupported: ~a, supported options: ~a" o options)
					markup)))
		    required-options)
	  (slot-set! writer 'verified? #t)))))

;;;
;;; CHECK-OPTIONS
;;;
(define (check-options lopts markup engine)

  ;;  Only keywords are checked, symbols are voluntary left unchecked. */
  (with-debug 6 'check-options
      (debug-item "markup="  (markup-markup markup))
      (debug-item "options=" (slot-ref markup 'options))
      (debug-item "lopts="   lopts)
      (for-each
	  (lambda (o2)
	    (for-each
		(lambda (o)
		  (if (and (keyword? o)
			   (not (eq? o :&skribe-eval-location))
			   (not (memq o lopts)))
		      (skribe-warning/ast
		       3
		       markup
		       'verify
		       (format #f "engine ~a does not support markup ~a option `~a' -- ~a"
			       (engine-ident engine)
			       (markup-markup markup)
			       o
			       (markup-option markup o)))))
		o2))
	  (slot-ref markup 'options))))


;;; ======================================================================
;;;
;;;				V E R I F Y
;;;
;;; ======================================================================

;;; TOP
(define-method (verify (obj <top>) e)
  obj)

;;; PAIR
(define-method (verify (obj <pair>) e)
  (for-each (lambda (x) (verify x e)) obj)
  obj)

;;; PROCESSOR
(define-method (verify (obj <processor>) e)
  (let ((combinator (slot-ref obj 'combinator))
	(engine     (slot-ref obj 'engine))
	(body       (slot-ref obj 'body)))
    (verify body (processor-get-engine combinator engine e))
    obj))

;;; NODE
(define-method (verify (node <node>) e)
  ;; Verify body
  (verify (slot-ref node 'body) e)
  ;; Verify options
  (for-each (lambda (o) (verify (cadr o) e))
	    (slot-ref node 'options))
  node)

;;; MARKUP
(define-method (verify (node <markup>) e)
  (with-debug 5 'verify::<markup>
     (debug-item "node="    (markup-markup node))
     (debug-item "options=" (slot-ref node 'options))
     (debug-item "e="	    (engine-ident e))

     (next-method)

     (let ((w (lookup-markup-writer node e)))
       (when (writer? w)
	 (check-required-options node w e)
	 (when (pair? (writer-options w))
	   (check-options (slot-ref w 'options) node e))
	 (let ((validate (slot-ref w 'validate)))
	   (when (procedure? validate)
	     (unless (validate node e)
	       (skribe-warning
		     1
		     node
		     (format #f "node `~a' forbidden here by ~a engine"
			     (markup-markup node)
			     (engine-ident e))))))))
     node))


;;; DOCUMENT
(define-method (verify (node <document>) e)
  (next-method)

  ;; verify the engine customs
  (for-each (lambda (c)
	      (let ((a (cadr c)))
		(set-car! (cdr c) (verify a e))))
	    (slot-ref e 'customs))

   node)

;;; verify.scm ends here
