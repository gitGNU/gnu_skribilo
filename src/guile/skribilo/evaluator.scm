;;; eval.scm  --  Skribilo evaluator.
;;;
;;; Copyright 2003-2004  Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
;;; Copyright 2005,2006  Ludovic Courtès  <ludovic.courtes@laas.fr>
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


(define-module (skribilo evaluator)
  :export (evaluate-document evaluate-document-from-port
	   load-document include-document *load-options*)
  :autoload (skribilo parameters) (*verbose* *document-path*)
  :autoload (skribilo location)   (<location>)
  :autoload (skribilo ast)        (ast? markup?)
  :autoload (skribilo engine)     (*current-engine*
				   engine? find-engine engine-ident)
  :autoload (skribilo reader)     (*document-reader*)

  :autoload (skribilo verify)     (verify)
  :autoload (skribilo resolve)    (resolve!))


(use-modules (skribilo utils syntax)
	     (skribilo condition)
	     (skribilo debug)
	     (skribilo output)
             (skribilo lib)

	     (ice-9 optargs)
	     (oop goops)
	     (srfi srfi-1)
	     (srfi srfi-13)
	     (srfi srfi-34)
	     (srfi srfi-35)
	     (srfi srfi-39))


(fluid-set! current-reader %skribilo-module-reader)


;;;
;;; %EVALUATE
;;;
(define (%evaluate expr)
  ;; Evaluate EXPR, an arbitrary S-expression that may contain calls to the
  ;; markup functions defined in a markup package such as
  ;; `(skribilo package base)', e.g., `(bold "hello")'.
  (let ((result (eval expr (current-module))))

    (if (ast? result)
	(let ((file (source-property expr 'filename))
	      (line (source-property expr 'line))
	      (column (source-property expr 'column)))
	  (slot-set! result 'loc
		     (make <location>
		       :file file :line line :pos column))))

    result))



;;;
;;; EVALUATE-DOCUMENT
;;;
(define* (evaluate-document a e :key (env '()))
  ;; Argument A must denote an AST of something like that, not just an
  ;; S-exp.
  (with-debug 2 'evaluate-document
     (debug-item "a=" a " e=" (engine-ident e))
     (let ((a2 (resolve! a e env)))
       (debug-item "resolved a=" a)
       (let ((a3 (verify a2 e)))
	 (debug-item "verified a=" a3)
	 (output a3 e)))))

;;;
;;; EVALUATE-DOCUMENT-FROM-PORT
;;;
(define* (evaluate-document-from-port port engine
				      :key (env '())
				           (reader (*document-reader*)))
  (with-debug 2 'evaluate-document-from-port
     (debug-item "engine=" engine)
     (debug-item "reader=" reader)

     (let ((e (if (symbol? engine) (find-engine engine) engine)))
       (debug-item "e=" e)
       (if (not (engine? e))
	   (skribe-error 'evaluate-document-from-port "cannot find engine" engine)
	   (let loop ((exp (reader port)))
	     (with-debug 10 'evaluate-document-from-port
		(debug-item "exp=" exp))
	     (unless (eof-object? exp)
	       (evaluate-document (%evaluate exp) e :env env)
	       (loop (reader port))))))))


;;;
;;; LOAD-DOCUMENT
;;;

;; Options that may make sense to a specific back-end or package.
(define-public *load-options* (make-parameter '()))

;; List of the names of files already loaded.
(define *loaded-files* (make-parameter '()))

(define* (load-document file :key (engine #f) (path #f) :allow-other-keys
			:rest opt)
  (with-debug 4 'skribe-load
     (debug-item "  engine=" engine)
     (debug-item "  path=" path)
     (debug-item "  opt=" opt)

     (let* ((ei  (*current-engine*))
	    (path (append (cond
			   ((not path) (*document-path*))
			   ((string? path) (list path))
			   ((not (and (list? path) (every? string? path)))
			    (raise (condition (&invalid-argument-error
					       (proc-name 'load-document)
					       (argument  path)))))
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

       (unless (and (string? filep) (file-exists? filep))
	 (raise (condition (&file-search-error
			    (file-name file)
			    (path path)))))

       ;; Pass the additional options to the back-end and/or packages being
       ;; used.
       (parameterize ((*load-options* opt))

	 ;; Load this file if not already done
	 ;; FIXME: Shouldn't we remove this logic?  -- Ludo'.
	 (unless (member filep (*loaded-files*))
	   (cond
	    ((> (*verbose*) 1)
	     (format (current-error-port) "  [loading file: ~S ~S]\n" filep opt))
	    ((> (*verbose*) 0)
	     (format (current-error-port) "  [loading file: ~S]\n" filep)))

	   ;; Load it
	   (with-input-from-file filep
	     (lambda ()
	       (evaluate-document-from-port (current-input-port) ei)))

	   (*loaded-files* (cons filep (*loaded-files*))))))))

;;;
;;; INCLUDE-DOCUMENT
;;;
(define* (include-document file :key (path (*document-path*))
			             (reader (*document-reader*)))
  (unless (every string? path)
    (raise (condition (&invalid-argument-error (proc-name 'include-document)
					       (argument  path)))))

  (let ((full-path (search-path path file)))
    (unless (and (string? full-path) (file-exists? full-path))
      (raise (condition (&file-search-error
			 (file-name file)
			 (path path)))))

    (when (> (*verbose*) 0)
      (format (current-error-port) "  [including file: ~S]\n" full-path))

    (with-input-from-file full-path
      (lambda ()
	(let Loop ((exp (reader (current-input-port)))
		   (res '()))
	  (if (eof-object? exp)
	      (if (and (pair? res) (null? (cdr res)))
		    (car res)
		    (reverse! res))
	      (Loop (reader (current-input-port))
		    (cons (%evaluate exp) res))))))))
