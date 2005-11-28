;;; eval.scm  --  Skribilo evaluator.
;;;
;;; Copyright 2003-2004  Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
;;; Copyright 2005  Ludovic Courtès  <ludovic.courtes@laas.fr>
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


(define-module (skribilo evaluator)
  :export (skribe-eval skribe-eval-port skribe-load skribe-load-options
	   skribe-include)
  :autoload (skribilo parameters) (*verbose* *document-path*)
  :autoload (skribilo location)   (<location>)
  :autoload (skribilo ast)        (ast? markup?)
  :autoload (skribilo engine)     (engine? find-engine engine-ident)
  :autoload (skribilo reader)     (%default-reader)

  :autoload (skribilo verify)     (verify)
  :autoload (skribilo resolve)    (resolve!))


(use-modules (skribilo utils syntax)
	     (skribilo debug)
	     (skribilo output)
             (skribilo lib)

	     (ice-9 optargs)
	     (oop goops)
	     (srfi srfi-13)
	     (srfi srfi-1))


(set-current-reader %skribilo-module-reader)


(define *skribe-loaded* '())		;; List of already loaded files
(define *skribe-load-options* '())

;;;
;;; %EVALUATE
;;;
(define (%evaluate expr)
  (let ((result (eval expr (current-module))))
    (if (or (ast? result) (markup? result))
	(let ((file (source-property expr 'filename))
	      (line (source-property expr 'line))
	      (column (source-property expr 'column)))
	  (format #t "~%~%*** source props for `~a': ~a~%~%"
		  result (source-properties expr))
	  (slot-set! result 'loc
		     (make <location>
		       :file file :line line :pos column))))
    result))




;;;
;;; SKRIBE-EVAL
;;;
(define* (skribe-eval a e :key (env '()))
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
(define* (skribe-eval-port port engine :key (env '())
			                    (reader %default-reader))
  (with-debug 2 'skribe-eval-port
     (debug-item "engine=" engine)
     (debug-item "reader=" reader)

     (let ((e (if (symbol? engine) (find-engine engine) engine)))
       (debug-item "e=" e)
       (if (not (engine? e))
	   (begin
	     (format #t "engine: ~a~%" e)
	     (skribe-error 'skribe-eval-port "cannot find engine" engine))
	   (let loop ((exp (reader port)))
	     (with-debug 10 'skribe-eval-port
		(debug-item "exp=" exp))
	     (unless (eof-object? exp)
	       (skribe-eval (%evaluate exp) e :env env)
	       (loop (reader port))))))))

;;;
;;; SKRIBE-LOAD
;;;

;;; FIXME: Use a fluid for that.
(define *skribe-load-options* '())

(define (skribe-load-options)
  *skribe-load-options*)

(define* (skribe-load file :key (engine #f) (path #f) :rest opt)
  (with-debug 4 'skribe-load
     (debug-item "  engine=" engine)
     (debug-item "  path=" path)
     (debug-item "  opt=" opt)

     (let* ((ei  (*current-engine*))
	    (path (append (cond
			   ((not path) (*document-path*))
			   ((string? path) (list path))
			   ((not (and (list? path) (every? string? path)))
			    (skribe-error 'skribe-load "illegal path" path))
			   (else path))
			  %load-path))
            (filep (or (search-path path file)
		       (search-path (append path %load-path) file)
		       (search-path (append path %load-path)
				    (let ((dot (string-rindex file #\.)))
				      (if dot
					  (string-append
					   (string-take file dot)
					   ".scm")
					  file))))))

       (set! *skribe-load-options* opt)

       (unless (and (string? filep) (file-exists? filep))
	 (skribe-error 'skribe-load
		       (string-append "cannot find `" file "' in path")
		       path))

       ;; Load this file if not already done
       (unless (member filep *skribe-loaded*)
	 (cond
	   ((> (*verbose*) 1)
	    (format (current-error-port) "  [loading file: ~S ~S]\n" filep opt))
	   ((> (*verbose*) 0)
	    (format (current-error-port) "  [loading file: ~S]\n" filep)))
	 ;; Load it
	 (with-input-from-file filep
	   (lambda ()
	     (skribe-eval-port (current-input-port) ei)))
	 (set! *skribe-loaded* (cons filep *skribe-loaded*))))))

;;;
;;; SKRIBE-INCLUDE
;;;
(define* (skribe-include file :optional (path (*document-path*)))
  (unless (every string? path)
    (skribe-error 'skribe-include "illegal path" path))

  (let ((path (search-path path file)))
    (unless (and (string? path) (file-exists? path))
      (skribe-error 'skribe-load
		    (format #t "cannot find ~S in path" file)
		    path))
    (when (> (*verbose*) 0)
      (format (current-error-port) "  [including file: ~S]\n" path))

    (with-input-from-file path
      (lambda ()
	(let Loop ((exp (%default-reader (current-input-port)))
		   (res '()))
	  (if (eof-object? exp)
	      (if (and (pair? res) (null? (cdr res)))
		    (car res)
		    (reverse! res))
	      (Loop (%default-reader (current-input-port))
		    (cons (%evaluate exp) res))))))))
