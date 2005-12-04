;;; resolve.scm  --  Skribilo reference resolution.
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
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
;;; USA.

(define-module (skribilo resolve)
  :use-module (skribilo debug)
  :use-module (skribilo runtime)
  :use-module (skribilo ast)
  :use-module (skribilo utils syntax)

  :use-module (oop goops)
  :use-module (srfi srfi-39)

  :export (resolve! resolve-search-parent resolve-children resolve-children*
	   find1 resolve-counter resolve-parent resolve-ident))

(set-current-reader %skribilo-module-reader)


(define *unresolved* (make-parameter #f))
(define-generic do-resolve!)


;;;; ======================================================================
;;;;
;;;; RESOLVE!
;;;;
;;;; This function iterates over an ast until all unresolved  references
;;;; are resolved.
;;;;
;;;; ======================================================================
(define (resolve! ast engine env)
  (with-debug 3 'resolve
     (debug-item "ast=" ast)
     (parameterize ((*unresolved* #f))
       (let Loop ((ast ast))
	 (*unresolved* #f)
	 (let ((ast (do-resolve! ast engine env)))
	   (if (*unresolved*)
	       (begin
		 (debug-item "iterating over ast " ast)
		 (Loop ast))
	       ast))))))

;;;; ======================================================================
;;;;
;;;;				D O - R E S O L V E !
;;;;
;;;; ======================================================================

(define-method (do-resolve! ast engine env)
  ast)


(define-method (do-resolve! (ast <pair>) engine env)
  (let Loop ((n* ast))
    (cond
      ((pair? n*)
       (set-car! n* (do-resolve! (car n*) engine env))
       (Loop (cdr n*)))
      ((not (null? n*))
       (error 'do-resolve "illegal argument" n*))
      (else
       ast))))


(define-method (do-resolve! (node <node>) engine env)
  (let ((body    (slot-ref node 'body))
	(options (slot-ref node 'options))
	(parent  (slot-ref node 'parent)))
    (with-debug 5 'do-resolve<body>
       (debug-item "body=" body)
       (when (eq? parent 'unspecified)
	 (let ((p (assq 'parent env)))
	   (slot-set! node 'parent (and (pair? p) (pair? (cdr p)) (cadr p)))
	   (when (pair? options)
	     (debug-item "unresolved options=" options)
	     (for-each (lambda (o)
			 (set-car! (cdr o)
				   (do-resolve! (cadr o) engine env)))
		       options)
	     (debug-item "resolved options=" options))))
       (slot-set! node 'body (do-resolve! body engine env))
       node)))



(define-method (do-resolve! (node <container>) engine env0)
  (let ((body     (slot-ref node 'body))
	(options  (slot-ref node 'options))
	(env      (slot-ref node 'env))
	(parent   (slot-ref node 'parent)))
    (with-debug 5 'do-resolve<container>
       (debug-item "markup=" (markup-markup node))
       (debug-item "body=" body)
       (debug-item "env0=" env0)
       (debug-item "env=" env)
       (when (eq? parent 'unspecified)
	 (let ((p (assq 'parent env0)))
	   (slot-set! node 'parent (and (pair? p) (pair? (cdr p)) (cadr p)))
	   (when (pair? options)
	     (let ((e (append `((parent ,node)) env0)))
	       (debug-item "unresolved options=" options)
	       (for-each (lambda (o)
			   (set-car! (cdr o)
				     (do-resolve! (cadr o) engine e)))
			 options)
	       (debug-item "resolved options=" options)))))
       (let ((e `((parent ,node) ,@env ,@env0)))
	 (slot-set! node 'body (do-resolve! body engine e)))
       node)))


(define-method (do-resolve! (node <document>) engine env0)
  (next-method)
  ;; resolve the engine custom
  (let ((env (append `((parent ,node)) env0)))
    (for-each (lambda (c)
		(let ((i (car c))
		      (a (cadr c)))
		  (debug-item "custom=" i " " a)
		  (set-car! (cdr c) (do-resolve! a engine env))))
	      (slot-ref engine 'customs)))
  node)


(define-method (do-resolve! (node <unresolved>) engine env)
  (with-debug 5 'do-resolve<unresolved>
     (debug-item "node=" node)
     (let ((p (assq 'parent env)))
       (slot-set! node 'parent (and (pair? p) (pair? (cdr p)) (cadr p))))

     (let* ((proc (slot-ref node 'proc))
	    (res  (proc node engine env))
	    (loc  (ast-loc node)))
       (when (ast? res)
	 (ast-loc-set! res loc))
       (debug-item "res=" res)
       (*unresolved* #t)
       res)))


(define-method (do-resolve! (node <handle>) engine env)
  node)


;;;; ======================================================================
;;;;
;;;; RESOLVE-PARENT
;;;;
;;;; ======================================================================
(define (resolve-parent n e)
  (with-debug 5 'resolve-parent
     (debug-item "n=" n)
     (cond
       ((not (is-a? n <ast>))
	(let ((c (assq 'parent e)))
	  (if (pair? c)
	      (cadr c)
	      n)))
       ((eq? (slot-ref n 'parent) 'unspecified)
	(skribe-error 'resolve-parent "orphan node" n))
       (else
	(slot-ref n 'parent)))))


;;;; ======================================================================
;;;;
;;;; RESOLVE-SEARCH-PARENT
;;;;
;;;; ======================================================================
(define (resolve-search-parent n e pred)
  (with-debug 5 'resolve-search-parent
     (debug-item "node=" n)
     (debug-item "searching=" pred)
     (let ((p (resolve-parent n e)))
       (debug-item "parent=" p " "
		   (if (is-a? p 'markup) (slot-ref p 'markup) "???"))
       (cond
	 ((pred p)		 p)
	 ((is-a? p <unresolved>) p)
	 ((not p)		 #f)
	 (else			 (resolve-search-parent p e pred))))))

;;;; ======================================================================
;;;;
;;;; RESOLVE-COUNTER
;;;;
;;;; ======================================================================
;;FIXME: factoriser
(define (resolve-counter n e cnt val . opt)
  (let ((c (assq (symbol-append cnt '-counter) e)))
    (if (not (pair? c))
	(if (or (null? opt) (not (car opt)) (null? e))
	    (skribe-error cnt "orphan node" n)
	    (begin
	      (set-cdr! (last-pair e)
			(list (list (symbol-append cnt '-counter) 0)
			      (list (symbol-append cnt '-env) '())))
	      (resolve-counter n e cnt val)))
	(let* ((num (cadr c))
	       (nval (if (integer? val)
			 val
			 (+ 1 num))))
	  (let ((c2 (assq (symbol-append cnt '-env) e)))
	    (set-car! (cdr c2) (cons (resolve-parent n e) (cadr c2))))
	  (cond
	    ((integer? val)
	     (set-car! (cdr c) val)
	     (car val))
	    ((not val)
	     val)
	    (else
	     (set-car! (cdr c) (+ 1 num))
	     (+ 1 num)))))))

;;;; ======================================================================
;;;;
;;;; RESOLVE-IDENT
;;;;
;;;; ======================================================================
(define (resolve-ident ident markup n e)
  (with-debug 4 'resolve-ident
     (debug-item "ident=" ident)
     (debug-item "markup=" markup)
     (debug-item "n=" (if (markup? n) (markup-markup n) n))
     (if (not (string? ident))
	 (skribe-type-error 'resolve-ident
			    "Illegal ident"
			    ident
			    "string")
	 (let ((mks (find-markups ident)))
	   (and mks
		(if (not markup)
		    (car mks)
		    (let loop ((mks mks))
		      (cond
			((null? mks)
			 #f)
			((is-markup? (car mks) markup)
			 (car mks))
			(else
			 (loop (cdr mks)))))))))))
