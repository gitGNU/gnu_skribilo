;;; eval.scm  --  Skribilo evaluator.
;;; -*- coding: iso-8859-1 -*-
;;;
;;; Copyright 2005, 2006, 2009  Ludovic Courtès  <ludo@gnu.org>
;;; Copyright 2003, 2004  Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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
  :export (evaluate-ast-from-port
           evaluate-document evaluate-document-from-port
	   load-document include-document *load-options*)
  :autoload (skribilo parameters) (*verbose* *document-path*)
  :autoload (skribilo engine)     (*current-engine*
				   engine? find-engine engine-ident)
  :autoload (skribilo reader)     (*document-reader*)

  :autoload (skribilo verify)     (verify)
  :autoload (skribilo resolve)    (resolve!)

  :autoload (skribilo module)     (*skribilo-user-module*))


(use-modules (skribilo utils syntax)
	     (skribilo condition)
	     (skribilo debug)
	     (skribilo output)
             (skribilo lib)

	     (ice-9 optargs)
	     (srfi srfi-1)
	     (srfi srfi-13)
	     (srfi srfi-34)
	     (srfi srfi-35)
	     (srfi srfi-39))


(skribilo-module-syntax)


;;;
;;; %EVALUATE
;;;
(define (%evaluate expr module)
  ;; Evaluate EXPR in the current module.  EXPR is an arbitrary S-expression
  ;; that may contain calls to the markup functions defined in a markup
  ;; package such as `(skribilo package base)', e.g., `(bold "hello")'.
  (let ((opts (debug-options)))
    (dynamic-wind
        (lambda ()
          ;; Force use of the debugging evaluator so that we can track source
          ;; location.
          (debug-enable 'debug)
          (debug-enable 'backtrace))
        (lambda ()
          (eval expr module))
        (lambda ()
          ;; Restore previous evaluator options.
          (debug-options opts)))))


;;;
;;; EVALUATE-AST-FROM-PORT
;;;
(define* (evaluate-ast-from-port port :key (reader (*document-reader*))
                                           (module (*skribilo-user-module*)))
  ;; Evaluate code from PORT in MODULE, reading it with READER, and return an
  ;; AST (resulting from the last form evaluated).  The returned AST is
  ;; unresolved and unverified.
  (save-module-excursion
   (lambda ()
     (with-debug 10 'evaluate-ast-from-port

       (set-current-module module)

       (let loop ((exp (reader port))
                  (result #f))
         (debug-item "exp=" exp)
         (if (eof-object? exp)
             result
             (loop (reader port)
                   (%evaluate exp (current-module)))))))))


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
				           (reader (*document-reader*))
                                           (module (*skribilo-user-module*)))
  (with-debug 2 'evaluate-document-from-port
     (debug-item "engine=" engine)
     (debug-item "reader=" reader)

     (let ((e (if (symbol? engine) (lookup-engine engine) engine)))
       (debug-item "e=" e)
       (if (not (engine? e))
           (raise (condition (&invalid-argument-error
                              (proc-name 'evaluate-document-from-port)
                              (argument  e))))
           (let ((ast (evaluate-ast-from-port port :reader reader
                                              :module module)))
             (evaluate-document ast engine :env env))))))



;;;
;;; LOAD-DOCUMENT
;;;

;; Options that may make sense to a specific back-end or package.
(define-public *load-options* (make-parameter '()))

;; List of the names of files already loaded.
(define *loaded-files* (make-parameter '()))


(define* (load-document file
                        :key engine path
                             (module (*skribilo-user-module*))
                             (reader (*document-reader*))
                        :allow-other-keys
			:rest opt)
  (with-debug 4 'load-document
     (debug-item "  engine=" engine)
     (debug-item "  path=" path)
     (debug-item "  opt=" opt)

     (let* ((ei  (*current-engine*))
	    (path (append (cond
			   ((not path) (*document-path*))
			   ((string? path) (list path))
			   ((not (and (list? path) (every string? path)))
			    (raise (condition (&invalid-argument-error
					       (proc-name 'load-document)
					       (argument  path)))))
			   (else path))
			  %load-path))
            (filep  (search-path path file)))

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
               (cond-expand (guile-2
                             ;; Use the encoding specified by the `coding:'
                             ;; comment.
                             (let ((p (current-input-port)))
                               (set-port-encoding! p
                                                   (file-encoding p)))))
	       (evaluate-document-from-port (current-input-port) ei
                                            :module module
                                            :reader reader)))

	   (*loaded-files* (cons filep (*loaded-files*))))))))

;;;
;;; INCLUDE-DOCUMENT
;;;
(define* (include-document file :key (path (*document-path*))
			             (reader (*document-reader*))
                                     (module (current-module)))
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
        (cond-expand (guile-2
                      ;; Use the encoding specified by the `coding:' comment.
                      (let ((p (current-input-port)))
                        (set-port-encoding! p (file-encoding p)))))
        (save-module-excursion
          (lambda ()
            (set-current-module module)

            (let Loop ((exp (reader (current-input-port)))
                       (res '()))
              (if (eof-object? exp)
                  (if (and (pair? res) (null? (cdr res)))
                      (car res)
                      (reverse! res))
                  (Loop (reader (current-input-port))
                        (cons (%evaluate exp module) res))))))))))
