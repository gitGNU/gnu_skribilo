;;;
;;; lib.scm	-- Utilities
;;;
;;; Copyright © 2003-2004 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
;;; Copyright © 2005 Ludovic Courtès <ludovic.courtes@laas.fr>
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

(define-module (skribilo lib)
  :use-module (skribilo utils syntax)
  :export (skribe-eval-location skribe-ast-error skribe-error
           skribe-type-error
           skribe-warning skribe-warning/ast
           skribe-message

	   %procedure-arity)

  :export-syntax (new define-markup define-simple-markup
                  define-simple-container define-processor-markup)

  :use-module (skribilo config)
  :use-module (skribilo ast)

  ;; useful for `new' to work well with <language>
  :autoload   (skribilo source)   (<language>)

  :use-module (skribilo reader)
  :use-module (skribilo parameters)
  :use-module (skribilo location)

  :use-module (srfi srfi-1)
  :use-module (oop goops)
  :use-module (ice-9 optargs))


(fluid-set! current-reader %skribilo-module-reader)


;;;
;;; NEW
;;;

(define %types-module (current-module))

(define-macro (new class . parameters)
  ;; Thanks to the trick below, modules don't need to import `(oop goops)'
  ;; and `(skribilo ast)' in order to make use of `new'.
  (let* ((class-name (symbol-append '< class '>))
	 (actual-class (module-ref %types-module class-name)))
    `(let ((make ,make)
	   (,class-name ,actual-class))
       (make ,class-name
	 ,@(apply append (map (lambda (x)
				`(,(symbol->keyword (car x)) ,(cadr x)))
			      parameters))))))

;;;
;;; DEFINE-MARKUP
;;;
(define-macro (define-markup bindings . body)
  ;; This is just an `(ice-9 optargs)' kind of `lambda*', with DSSSL
  ;; keyword-style conversion enabled.  However, using `(ice-9 optargs)', the
  ;; `#:rest' argument can only appear last, which is not what Skribe/DSSSL
  ;; expect, hence `fix-rest-arg'.
  (define (fix-rest-arg args)
    (let loop ((args args)
	       (result '())
	       (rest-arg #f))
      (cond ((null? args)
	     (if rest-arg
		 (append (reverse result) rest-arg)
		 (reverse result)))

	    ((list? args)
	     (let ((is-rest-arg? (eq? (car args) #:rest)))
	       (loop (if is-rest-arg? (cddr args) (cdr args))
		     (if is-rest-arg? result (cons (car args) result))
		     (if is-rest-arg?
			 (list (car args) (cadr args))
			 rest-arg))))

	    ((pair? args)
	     (loop '()
		   (cons (car args) result)
		   (list #:rest (cdr args)))))))

  (let ((name (car bindings))
	(opts (cdr bindings)))
    `(define*-public ,(cons name (fix-rest-arg opts)) ,@body)))


;;;
;;; DEFINE-SIMPLE-MARKUP
;;;
(define-macro (define-simple-markup markup)
  `(define-markup (,markup :rest opts :key ident class loc)
     (new markup
	  (markup ',markup)
	  (ident (or ident (symbol->string
			    (gensym ',(symbol->string markup)))))
	  (loc loc)
	  (class class)
	  (required-options '())
	  (options (the-options opts :ident :class :loc))
	  (body (the-body opts)))))


;;;
;;; DEFINE-SIMPLE-CONTAINER
;;;
(define-macro (define-simple-container markup)
   `(define-markup (,markup :rest opts :key ident class loc)
       (new container
	  (markup ',markup)
	  (ident (or ident (symbol->string
			    (gensym ',(symbol->string markup)))))
	  (loc loc)
	  (class class)
	  (required-options '())
	  (options (the-options opts :ident :class :loc))
	  (body (the-body opts)))))


;;;
;;; DEFINE-PROCESSOR-MARKUP
;;;
(define-macro (define-processor-markup proc)
  `(define-markup (,proc #:rest opts)
     (new processor
	  (engine  (find-engine ',proc))
	  (body    (the-body opts))
	  (options (the-options opts)))))


;;;
;;; SKRIBE-EVAL-LOCATION ...
;;;
(define (skribe-eval-location)
  (format (current-error-port)
	  "FIXME: ...... SKRIBE-EVAL-LOCATION (should not appear)\n")
  #f)

;;;
;;; SKRIBE-ERROR
;;;
(define (skribe-ast-error proc msg obj)
  (let ((l     (ast-loc obj))
	(shape (if (markup? obj) (markup-markup obj) obj)))
    (if (location? l)
	(error (format #f "~a:~a: ~a: ~a ~s" (location-file l)
		       (location-line l) proc msg shape))
	(error (format #f "~a: ~a ~s " proc msg shape)))))

(define (skribe-error proc msg obj)
  (if (ast? obj)
      (skribe-ast-error proc msg obj)
      (error (format #f "~a: ~a ~s" proc msg obj))))


;;;
;;; SKRIBE-TYPE-ERROR
;;;
(define (skribe-type-error proc msg obj etype)
  (skribe-error proc (format "~a ~s (~a expected)" msg obj etype) #f))


;;;
;;; SKRIBE-WARNING  &  SKRIBE-WARNING/AST
;;;
(define (%skribe-warn level file line lst)
  (let ((port (current-error-port)))
    (if (or (not file) (not line))
	(begin
	  ;; XXX:  This is a bit hackish, but it proves to be quite useful.
	  (set! file (port-filename (current-input-port)))
	  (set! line (port-line (current-input-port)))))
    (when (and file line)
      (format port "~a:~a: " file line))
    (format port "warning: ")
    (for-each (lambda (x) (format port "~a " x)) lst)
    (newline port)))


(define (skribe-warning level . obj)
  (if (>= (*warning*) level)
      (%skribe-warn level #f #f obj)))


(define (skribe-warning/ast level ast . obj)
  (if (>= (*warning*) level)
      (let ((l (ast-loc ast)))
	(if (location? l)
	    (%skribe-warn level (location-file l) (location-line l) obj)
	    (%skribe-warn level #f #f obj)))))

;;;
;;; SKRIBE-MESSAGE
;;;
(define (skribe-message fmt . obj)
  (when (> (*verbose*) 0)
    (apply format (current-error-port) fmt obj)))


;;;
;;; %PROCEDURE-ARITY
;;;
(define (%procedure-arity proc)
  (car (procedure-property proc 'arity)))

;;; lib.scm ends here
