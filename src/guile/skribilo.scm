#!/bin/sh
# aside from this initial boilerplate, this is actually -*- scheme -*- code
main='(module-ref (resolve-module '\''(skribilo)) '\'main')'
exec ${GUILE-guile} -l $0 -c "(apply $main (cdr (command-line)))" "$@"
!#

;;;;
;;;; skribilo.scm
;;;; 
;;;; Copyright � 2003-2004 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
;;;; Copyright 2005  Ludovic Court�s <ludovic.courtes@laas.fr>
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
;;;;    Creation date: 24-Jul-2003 20:33 (eg)
;;;; Last file update:  6-Mar-2004 16:13 (eg)
;;;;

;;;; Commentary:
;;;;
;;;; Usage: skribilo [ARGS]
;;;;
;;;; Process a skribilo document.
;;;;
;;;; Code:

;; Allow for this `:style' of keywords.
(read-set! keywords 'prefix)

; (use-modules (skribe eval)
; 	     (skribe configure)
; 	     (skribe runtime)
; 	     (skribe engine)
; 	     (skribe writer)
; 	     (skribe verify)
; 	     (skribe output)
; 	     (skribe biblio)
; 	     (skribe prog)
; 	     (skribe resolve)
; 	     (skribe source)
; 	     (skribe lisp)
; 	     (skribe xml)
; 	     (skribe c)
; 	     (skribe debug)
; 	     (skribe color))

(use-modules (skribe runtime)
	     (skribe configure)
	     (skribe eval)
	     (skribe engine)

	     (ice-9 optargs))


(load "skribe/lib.scm")

(load "../common/configure.scm")
(load "../common/param.scm")

; (include "vars.stk")
; (include "reader.stk")
; (include "configure.stk")
; (include "types.stk")
; (include "debug.stk")
; (include "lib.stk")
(load "../common/lib.scm")
; (include "resolve.stk")
; (include "writer.stk")
; (include "verify.stk")
; (include "output.stk")
; (include "prog.stk")
; (include "eval.stk")
; (include "runtime.stk")
; (include "engine.stk")
; (include "biblio.stk")
; (include "source.stk")
; (include "lisp.stk")
; (include "xml.stk")
; (include "c.stk")
; (include "color.stk")
(load "../common/sui.scm")

(load "../common/index.scm")
(load "../common/api.scm")


;;; KLUDGE for allowing redefinition of Skribe INCLUDE
;(remove-expander! 'include)


;;;; ======================================================================
;;;;
;;;;				P A R S E - A R G S
;;;;
;;;; ======================================================================
(define (parse-args args)

  (define (version)
    (format #t "skribe v~A\n" (skribe-release)))

  (define (query)
    (version)
    (for-each (lambda (x)
		(let ((s (keyword->string (car x))))
		  (printf "  ~a: ~a\n" s (cadr x))))
	      (skribe-configure)))
  
  ;;
  ;; parse-args starts here
  ;;
  (let ((paths '())
	(engine #f))
    (parse-arguments args
      "Usage: skribe [options] [input]"
      "General options:"
	(("target" :alternate "t" :arg target
		   :help "sets the output format to <target>")
	   (set! engine (string->symbol target)))
	(("I" :arg path :help "adds <path> to Skribe path")
	   (set! paths (cons path paths)))
	(("B" :arg path :help "adds <path> to bibliography path")
	   (skribe-bib-path-set! (cons path (skribe-bib-path))))
	(("S" :arg path :help "adds <path> to source path")
	   (skribe-source-path-set! (cons path (skribe-source-path))))
	(("P" :arg path :help "adds <path> to image path")
	   (skribe-image-path-set! (cons path (skribe-image-path))))
	(("split-chapters" :alternate "C" :arg chapter
	  		   :help "emit chapter's sections in separate files")
	   (set! *skribe-chapter-split* (cons chapter *skribe-chapter-split*)))
	(("preload" :arg file :help "preload <file>")
	 (set! *skribe-preload* (cons file *skribe-preload*)))
	(("use-variant" :alternate "u" :arg variant
	  		:help "use <variant> output format")
	  (set! *skribe-variants* (cons variant *skribe-variants*)))
	(("base" :alternate "b" :arg base
	         :help "base prefix to remove from hyperlinks")
	   (set! *skribe-ref-base* base))
	(("rc-dir" :arg dir :alternate "d" :help "set the RC directory to <dir>")
	   (set! *skribe-rc-directory* dir))
	
      "File options:"
        (("no-init-file" :help "Dont load rc Skribe file")
	   (set! *load-rc* #f))
	(("output" :alternate "o" :arg file :help "set the output to <file>")
	   (set! *skribe-dest* file)
	   (let* ((s (file-suffix file))
		  (c (assoc s *skribe-auto-mode-alist*)))
	     (when (and (pair? c) (symbol? (cdr c)))
	       (set! *skribe-engine* (cdr c)))))

      "Misc:"
        (("help" :alternate "h" :help "provides help for the command")
	   (arg-usage (current-error-port))
	   (exit 0))
	(("options" :help "display the skribe options and exit")
	   (arg-usage (current-output-port) #t)
	   (exit 0))
	(("version" :alternate "V" :help "displays the version of Skribe")
	   (version)
	   (exit 0))
	(("query" :alternate "q"
	  	  :help "displays informations about Skribe conf.")
	   (query)
	   (exit 0))
	(("verbose" :alternate "v" :arg level
	  :help "sets the verbosity to <level>. Use -v0 for crystal silence")
	   (let ((val (string->number level)))
	     (when (integer? val)
	       (set! *skribe-verbose* val))))
	(("warning" :alternate "w" :arg level
	  :help "sets the verbosity to <level>. Use -w0 for crystal silence")
	   (let ((val (string->number level)))
	     (when (integer? val)
	       (set! *skribe-warning* val))))
	(("debug" :alternate "g" :arg level :help "sets the debug <level>")
	   (let ((val (string->number level)))
	     (if (integer? val)
		 (set-skribe-debug! val)
		 (begin
		   ;; Use the symbol for debug
		   (set-skribe-debug! 	    1)
		   (add-skribe-debug-symbol (string->symbol level))))))
	(("no-color" :help "disable coloring for output")
	 (no-debug-color))
	(("custom" :alternate "c" :arg key=val :help "Preset custom value")
	   (let ((args (string-split key=val "=")))
	     (if (and (list args) (= (length args) 2))
		 (let ((key (car args))
		       (val (cadr args)))
		   (set! *skribe-precustom* (cons (cons (string->symbol key) val)
						  *skribe-precustom*)))
		 (error 'parse-arguments "Bad custom ~S" key=val))))
	(("eval" :alternate "e" :arg expr :help "evaluate expression <expr>")
	   (with-input-from-string expr
	     (lambda () (eval (read)))))
	(else
	 (set! *skribe-src* other-arguments)))
    
    ;; we have to configure Skribe path according to the environment variable
    (skribe-path-set! (append (let ((path (getenv "SKRIBEPATH")))
				(if path 
				    (string-split path ":")
				    '()))
			      (reverse! paths)
			      (skribe-default-path)))
    ;; Final initializations
    (when engine
      (set! *skribe-engine* engine))))

;;;; ======================================================================
;;;;
;;;;				   L O A D - R C
;;;;
;;;; ======================================================================
(define (load-rc)
  (when *load-rc*
    (let ((file (make-path *skribe-rc-directory* *skribe-rc-file*)))
      (when (and file (file-exists? file))
	(load file)))))

      

;;;; ======================================================================
;;;;
;;;;				      S K R I B E
;;;;
;;;; ======================================================================
(define (doskribe)
   (let ((e (find-engine *skribe-engine*)))
     (if (and (engine? e) (pair? *skribe-precustom*))
	 (for-each (lambda (cv)
		     (engine-custom-set! e (car cv) (cdr cv)))
		   *skribe-precustom*))
     (if (pair? *skribe-src*)
	 (for-each (lambda (f) (skribe-load f :engine *skribe-engine*))
		   *skribe-src*)
	 (skribe-eval-port (current-input-port) *skribe-engine*))))


;;;; ======================================================================
;;;;
;;;;				      M A I N 
;;;;
;;;; ======================================================================
(define (skribilo . args)
  ;; Load the user rc file
  (load-rc)

  ;; Parse command line
  (parse-args args)

  ;; Load the base file to bootstrap the system as well as the files
  ;; that are in the *skribe-preload* variable
  (skribe-load "base.skr" :engine 'base)
  (for-each (lambda (f) (skribe-load f :engine *skribe-engine*)) *skribe-preload*)

  ;; Load the specified variants
  (for-each (lambda (x) (skribe-load (format "~a.skr" x) :engine *skribe-engine*))
	    (reverse! *skribe-variants*))

;;  (if (string? *skribe-dest*)
;;      (with-handler (lambda (kind loc msg)
;;		      (remove-file *skribe-dest*)
;;		      (error loc msg))
;;	 (with-output-to-file *skribe-dest* doskribe))
;;      (doskribe))
(if (string? *skribe-dest*)
    (with-output-to-file *skribe-dest* doskribe)
    (doskribe))


(define main skribilo)

;;; skribilo ends here.
