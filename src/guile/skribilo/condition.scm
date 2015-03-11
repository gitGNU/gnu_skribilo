;;; condition.scm  --  Skribilo SRFI-35 error condition hierarchy.
;;; -*- coding: iso-8859-1 -*-
;;;
;;; Copyright 2006, 2007, 2008, 2015 Ludovic Courtès <ludo@gnu.org>
;;;
;;;
;;; This file is part of Skribilo.
;;;
;;; Skribilo is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Skribilo is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Skribilo.  If not, see <http://www.gnu.org/licenses/>.

(define-module (skribilo condition)
  :autoload   (srfi srfi-1)  (find)
  :use-module (srfi srfi-34)
  :use-module (srfi srfi-35)
  :use-module (srfi srfi-39)
  :use-module (ice-9 optargs)
  :autoload   (skribilo parameters)   (*destination-file*)
  :autoload   (skribilo utils syntax) (_ N_)
  :export     (&skribilo-error skribilo-error?

               invalid-argument-error
	       &invalid-argument-error invalid-argument-error?
	       &too-few-arguments-error too-few-arguments-error?

	       &file-error file-error?
	       &file-search-error file-search-error?
	       &file-open-error file-open-error?
	       &file-write-error file-write-error?

	       register-error-condition-handler!
	       lookup-error-condition-handler

	       %call-with-skribilo-error-catch
	       call-with-skribilo-error-catch
               call-with-skribilo-error-catch/exit))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; Top-level of Skribilo's SRFI-35 error conditions.
;;;
;;; Code:


;;;
;;; Standard error conditions.
;;;

(define-condition-type &skribilo-error &error
  skribilo-error?)


;;;
;;; Generic errors.
;;;

(define-condition-type &invalid-argument-error &skribilo-error
  invalid-argument-error?
  (proc-name invalid-argument-error:proc-name)
  (argument  invalid-argument-error:argument)
  (name      invalid-argument-error:name))     ;#f | symbol, for kw arguments

(define-condition-type &too-few-arguments-error &skribilo-error
  too-few-arguments-error?
  (proc-name too-few-arguments-error:proc-name)
  (arguments too-few-arguments-error:arguments))

(define* (invalid-argument-error proc argument #:optional name)
  "Raise an '&invalid-argument-error'."
  (raise (condition
          (&invalid-argument-error
           (proc-name 'proc)
           (argument argument)
           (name name)))))

;;;
;;; File errors.
;;;

(define-condition-type &file-error &skribilo-error
  file-error?
  (file-name file-error:file-name))

(define-condition-type &file-search-error &file-error
  file-search-error?
  (path file-search-error:path))

(define-condition-type &file-open-error &file-error
  file-open-error?)

(define-condition-type &file-write-error &file-error
  file-write-error?)



;;;
;;; Adding new error conditions from other modules.
;;;

(define %external-error-condition-alist '())

(define (register-error-condition-handler! pred handler)
  (set! %external-error-condition-alist
	(cons (cons pred handler)
	      %external-error-condition-alist)))

(define (lookup-error-condition-handler c)
  (let ((pair (find (lambda (pair)
		      (let ((pred (car pair)))
			(pred c)))
		    %external-error-condition-alist)))
    (if (pair? pair)
	(cdr pair)
	#f)))



;;;
;;; Convenience functions.
;;;

(define (show-stack-trace)
  ;; Display a backtrace to stderr if possible.
  (let ((stack (make-stack #t)))
    (if stack
        (begin
          (format (current-error-port) "~%Call stack:~%")
          (display-backtrace stack (current-error-port)))
        (begin
          (format (current-error-port) (_ "Call stack trace not available.~%"))
          (format (current-error-port) (_ "Use 'GUILE=\"guile --debug\" skribilo ...' for a detailed stack trace.~%"))))))

(define (abort exit-val)
  ;; Abort the `skribilo' command-line program, returning EXIT-VAL.

  ;; XXX: Whether this works depends on whether `with-exception-handler' is
  ;; broken, see below.
  (and (string? (*destination-file*))
       (false-if-exception (delete-file (*destination-file*))))

  (show-stack-trace)
  (exit exit-val))

(define (%call-with-skribilo-error-catch thunk exit exit-val)
  (with-exception-handler
   (lambda (c)
     (cond  ((invalid-argument-error? c)
             (let ((name (invalid-argument-error:name c)))
               (if name
                   (format (current-error-port)
                           (_ "in '~a': invalid argument '~a': ~S~%")
                           (invalid-argument-error:proc-name c)
                           name
                           (invalid-argument-error:argument c))
                   (format (current-error-port)
                           (_ "in '~a': invalid argument: ~S~%")
                           (invalid-argument-error:proc-name c)
                           (invalid-argument-error:argument c))))
	     (abort exit-val))

	    ((too-few-arguments-error? c)
	     (format (current-error-port)
                     (_ "in '~a': too few arguments: ~S~%")
		     (too-few-arguments-error:proc-name c)
		     (too-few-arguments-error:arguments c))
             (abort exit-val))

	    ((file-search-error? c)
	     (format (current-error-port)
                     (_ "~a: not found in path '~S'~%")
		     (file-error:file-name c)
		     (file-search-error:path c))
	     (abort exit-val))

	    ((file-open-error? c)
	     (format (current-error-port)
                     (_ "~a: cannot open file~%")
		     (file-error:file-name c))
	     (abort exit-val))

	    ((file-write-error? c)
	     (format (current-error-port)
                     (_ "~a: cannot write to file~%")
		     (file-error:file-name c))
	     (abort exit-val))

	    ((file-error? c)
	     (format (current-error-port)
                     (_ "file error: ~a~%")
		     (file-error:file-name c))
	     (abort exit-val))

	    ((skribilo-error? c)
	     (let ((handler (lookup-error-condition-handler c)))
	       (if (procedure? handler)
		   (handler c)
		   (format (current-error-port)
			   (_ "undefined skribilo error: ~S~%")
			   c)))
	     (abort exit-val))

            ((message-condition? c)
             (format (current-error-port) (condition-message c))
             (abort exit-val))

            (else
             (format (current-error-port)
                     (_ "unexpected error condition: ~A~%") c)
             (abort exit-val))))

   thunk))

(define-macro (call-with-skribilo-error-catch thunk)
  `(call/cc (lambda (cont)
	      (%call-with-skribilo-error-catch ,thunk cont #f))))

(define (call-with-skribilo-error-catch/exit thunk)
  (%call-with-skribilo-error-catch thunk primitive-exit 1))


;;;
;;; SRFI-34 replacement.
;;;

(define (with-exception-handler handler thunk)
  ;; Work around a "bug" in `with-exception-handler' in Guile up to 1.8.5.
  ;; See http://thread.gmane.org/gmane.lisp.guile.user/6969 for details.
  ;; XXX: This code is useless for 1.8.6 and later.
  (with-throw-handler 'srfi-34
                      thunk
                      (lambda (key obj)
                        (handler obj))))

;;; conditions.scm ends here
