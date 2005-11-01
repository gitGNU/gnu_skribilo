;;;
;;; lib.scm	-- Utilities
;;;
;;; Copyright © 2003-2004 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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

(read-set! keywords 'prefix)

(define-module (skribilo lib)
  :export (skribe-eval-location skribe-ast-error skribe-error
           skribe-type-error
           skribe-warning skribe-warning/ast
           skribe-message

           ;; paths as lists of directories

           %skribilo-load-path
           %skribilo-image-path %skribilo-bib-path %skribilo-source-path

           ;; compatibility

           skribe-path skribe-path-set!
           skribe-image-path skribe-image-path-set!
           skribe-bib-path skribe-bib-path-set!
           skribe-source-path skribe-source-path-set!

           ;; various utilities for compatiblity

           substring=?
           file-suffix file-prefix prefix suffix
           directory->list find-file/path
           printf fprintf
           any? every?
           process-input-port process-output-port process-error-port
	   %procedure-arity

           make-hashtable hashtable?
           hashtable-get hashtable-put! hashtable-update!
           hashtable->list

	   skribe-read
           find-runtime-type

	   date)

  :export-syntax (new define-markup define-simple-markup
                  define-simple-container define-processor-markup

                  ;; for compatibility
                  unwind-protect unless when)

  :use-module (skribilo config)
  :use-module (skribilo types)
  :use-module (skribilo reader)
  :use-module (skribilo vars)

  :use-module (srfi srfi-1)
  :use-module ((srfi srfi-19) :renamer (symbol-prefix-proc 's19:)) ;; date
  :use-module (oop goops)
  :use-module (ice-9 optargs))




;;;
;;; NEW
;;;

(define %types-module (resolve-module '(skribilo types)))

(define-macro (new class . parameters)
  ;; Thanks to the trick below, modules don't need to import `(oop goops)'
  ;; and `(skribilo types)' in order to make use of `new'.
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
	  (ident (or ident (symbol->string (gensym ',markup))))
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
	  (ident (or ident (symbol->string (gensym ',markup))))
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
  (if (>= *skribe-warning* level)
      (%skribe-warn level #f #f obj)))


(define (skribe-warning/ast level ast . obj)
  (if (>= *skribe-warning* level)
      (let ((l (ast-loc ast)))
	(if (location? l)
	    (%skribe-warn level (location-file l) (location-line l) obj)
	    (%skribe-warn level #f #f obj)))))

;;;
;;; SKRIBE-MESSAGE
;;;
(define (skribe-message fmt . obj)
  (when (> *skribe-verbose* 0)
    (apply format (current-error-port) fmt obj)))

;;;
;;; FILE-PREFIX / FILE-SUFFIX
;;;
(define (file-prefix fn)
  (if fn
      (let ((match (regexp-match "(.*)\\.([^/]*$)" fn)))
	(if match
	    (cadr match)
	    fn))
      "./SKRIBE-OUTPUT"))

(define (file-suffix s)
  ;; Not completely correct, but sufficient here
  (let* ((basename (regexp-replace "^(.*)/(.*)$" s "\\2"))
	 (split    (string-split basename ".")))
    (if (> (length split) 1)
	(car (reverse! split))
	"")))


;;;
;;; KEY-GET
;;;
;;; We need to redefine the standard key-get to be more permissive. In
;;; STklos key-get accepts a list which is formed only of keywords. In
;;; Skribe, parameter lists are of the form
;;;      (:title "..." :option "...." body1 body2 body3)
;;; So is we find an element which is not a keyword, we skip it (unless it
;;; follows a keyword of course). Since the compiler of extended lambda
;;; uses the function key-get, it will now accept Skribe markups
(define* (key-get lst key #:optional (default #f) default?)
  (define (not-found)
    (if default?
	default
	(error 'key-get "value ~S not found in list ~S" key lst)))
  (let Loop ((l lst))
    (cond
      ((null? l)
       (not-found))
      ((not (pair? l))
       (error 'key-get "bad list ~S" lst))
      ((keyword? (car l))
       (if (null? (cdr l))
	   (error 'key-get "bad keyword list ~S" lst)
	   (if (eq? (car l) key)
	       (cadr l)
	       (Loop (cddr l)))))
       (else
	(Loop (cdr l))))))



;;; ======================================================================
;;;
;;;				A C C E S S O R S
;;;
;;; ======================================================================


(define %skribilo-load-path (list (skribilo-default-path) "."))
(define %skribilo-image-path '("."))
(define %skribilo-bib-path '("."))
(define %skribilo-source-path '("."))

(define-macro (define-compatibility-accessors var oldname)
  (let ((newname (symbol-append '%skribilo- var))
        (setter  (symbol-append oldname '-set!)))
    `(begin
       (define (,oldname) ,newname)
       (define (,setter path)
         (if (not (and (list? path) (every string? path)))
             (skribe-error ',setter "illegal path" path)
             (set! ,newname path))))))

(define-compatibility-accessors load-path   skribe-path)
(define-compatibility-accessors image-path  skribe-image-path)
(define-compatibility-accessors bib-path    skribe-bib-path)
(define-compatibility-accessors source-path skribe-source-path)



;;; ======================================================================
;;;
;;;				Compatibility with Bigloo
;;;
;;; ======================================================================

(define (substring=? s1 s2 len)
  (let ((l1 (string-length s1))
	(l2 (string-length s2)))
    (let Loop ((i 0))
      (cond
	((= i len) #t)
	((= i l1)  #f)
	((= i l2)  #f)
	((char=? (string-ref s1 i) (string-ref s2 i)) (Loop (+ i 1)))
	(else #f)))))

(define (directory->list str)
  (map basename (glob (string-append str "/*") (string-append "/.*"))))

(define-macro (printf . args)   `(format #t ,@args))
(define fprintf			format)


(define prefix			file-prefix)
(define suffix			file-suffix)
(define system->string		system)  ;; FIXME
(define any?			any)
(define every?			every)
(define find-file/path		(lambda (. args)
				  (format #t "find-file/path: ~a~%" args)
				  #f))
(define process-input-port	#f) ;process-input)
(define process-output-port	#f) ;process-output)
(define process-error-port	#f) ;process-error)


;;;
;;; h a s h   t a b l e s
;;;
(define make-hashtable		make-hash-table)
(define hashtable?		hash-table?)
(define hashtable-get		(lambda (h k) (hash-ref h k #f)))
(define hashtable-put!		hash-set!)
(define hashtable-update!	hash-set!)
(define hashtable->list	(lambda (h)
                          (map cdr (hash-map->list cons h))))

(define find-runtime-type	(lambda (obj) obj))


;;;
;;; Various things.
;;;

(define %skribe-reader (make-reader 'skribe))

(define* (skribe-read #:optional (port (current-input-port)))
  (%skribe-reader port))

(define (%procedure-arity proc)
    (car (procedure-property proc 'arity)))

(define-macro (unwind-protect expr1 expr2)
  ;; This is no completely correct.
  `(dynamic-wind
       (lambda () #f)
       (lambda () ,expr1)
       (lambda () ,expr2)))

(define-macro (unless condition . exprs)
  `(if (not ,condition) (begin ,@exprs)))

(define-macro (when condition . exprs)
  `(if ,condition (begin ,@exprs)))

(define (date)
  (s19:date->string (s19:current-date) "~c"))


;;; lib.scm ends here
