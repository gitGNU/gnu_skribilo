;;;;
;;;; eval.stk		-- Skribe Evaluator
;;;; 
;;;; Copyright © 2003-2004 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
;;;; 
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, 
;;;; USA.
;;;; 
;;;;           Author: Erick Gallesio [eg@essi.fr]
;;;;    Creation date: 27-Jul-2003 09:15 (eg)
;;;; Last file update: 28-Oct-2004 15:05 (eg)
;;;;


;; FIXME; On peut implémenter maintenant skribe-warning/node


(define-module (skribe eval)
  :export (skribe-eval skribe-eval-port skribe-load skribe-load-options
	   skribe-include))

(use-modules (skribe debug)
	     (skribe engine)
	     (skribe verify)
	     (skribe resolve)
	     (skribe output)
	     (ice-9 optargs))


(define *skribe-loaded* '())  		;; List of already loaded files
(define *skribe-load-options* '())

(define (%evaluate expr)
  (with-handler
      (lambda (c)
	(flush-output-port (current-error-port))
	(raise c))
      (eval expr (find-module 'STklos))))

;;;
;;; SKRIBE-EVAL
;;;
(define* (skribe-eval a e #:key (env '()))
  (with-debug 2 'skribe-eval
     (debug-item "a=" a " e=" (engine-ident e))
     (let ((a2 (resolve! a e env)))
       (debug-item "resolved a=" a)
       (let ((a3 (verify a2 e)))
	 (debug-item "verified a=" a3)
	 (output a3 e)))))

;;;
;;; SKRIBE-EVAL-PORT
;;;
(define* (skribe-eval-port port engine #:key (env '()))
  (with-debug 2 'skribe-eval-port
     (debug-item "engine=" engine)
     (let ((e (if (symbol? engine) (find-engine engine) engine)))
       (debug-item "e=" e)
       (if (not (is-a? e <engine>))
	   (skribe-error 'skribe-eval-port "Cannot find engine" engine)
	   (let loop ((exp (read port)))
	     (with-debug 10 'skribe-eval-port
		(debug-item "exp=" exp))
	     (unless (eof-object? exp)
	       (skribe-eval (%evaluate exp) e :env env)
	       (loop (read port))))))))

;;;
;;; SKRIBE-LOAD
;;;
(define *skribe-load-options* '())

(define (skribe-load-options)
  *skribe-load-options*)

(define* (skribe-load file #:key (engine #f) (path #f) #:rest opt)
  (with-debug 4 'skribe-load
     (debug-item "  engine=" engine)
     (debug-item "  path=" path)
     (debug-item "  opt" opt)

     (let* ((ei  (cond
		  ((not engine) *skribe-engine*)
		  ((engine? engine) engine)
		  ((not (symbol? engine)) (skribe-error 'skribe-load
							"Illegal engine" engine))
		  (else engine)))
	    (path (cond
		    ((not path) (skribe-path))
		    ((string? path) (list path))
		    ((not (and (list? path) (every? string? path)))
		        (skribe-error 'skribe-load "Illegal path" path))
		    (else path)))
	     (filep (find-path file path)))

       (set! *skribe-load-options* opt)

       (unless (and (string? filep) (file-exists? filep))
	 (skribe-error 'skribe-load
		       (format "Cannot find ~S in path" file)
		       *skribe-path*))
       
       ;; Load this file if not already done
       (unless (member filep *skribe-loaded*)
	 (cond
	   ((> *skribe-verbose* 1)
	    (format (current-error-port) "  [loading file: ~S ~S]\n" filep opt))
	   ((> *skribe-verbose* 0)
	    (format (current-error-port) "  [loading file: ~S]\n" filep)))
	 ;; Load it
	 (with-input-from-file filep
	   (lambda ()
	     (skribe-eval-port (current-input-port) ei)))
	 (set! *skribe-loaded* (cons filep *skribe-loaded*))))))

;;;
;;; SKRIBE-INCLUDE
;;;
(define* (skribe-include file #:optional (path (skribe-path)))
  (unless (every string? path)
    (skribe-error 'skribe-include "Illegal path" path))

  (let ((path (find-path file path)))
    (unless (and (string? path) (file-exists? path))
      (skribe-error 'skribe-load
		    (format "Cannot find ~S in path" file)
		    path))
    (when (> *skribe-verbose* 0)
      (format (current-error-port) "  [including file: ~S]\n" path))
    (with-input-from-file path
      (lambda ()
	(let Loop ((exp (read (current-input-port)))
		   (res '()))
	  (if (eof-object? exp)
	      (if (and (pair? res) (null? (cdr res)))
		  (car res)
		  (reverse! res))
	      (Loop (read (current-input-port))
		    (cons (%evaluate exp) res))))))))
