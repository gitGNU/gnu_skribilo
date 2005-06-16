;;;;
;;;; lib.stk	-- Utilities
;;;; 
;;;; Copyright © 2003-2004 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
;;;;    Creation date: 11-Aug-2003 20:29 (eg)
;;;; Last file update: 27-Oct-2004 12:41 (eg)
;;;;

(use-modules (srfi srfi-1))

;;;
;;; NEW
;;;
(define-macro (new class . parameters)
  `(make ,(string->symbol (format #f "<~a>" class))
     ,@(apply append (map (lambda (x)
			    `(,(symbol->keyword (car x)) ,(cadr x)))
			  parameters))))

;;;
;;; DEFINE-MARKUP
;;;
(define-macro (define-markup bindings . body)
  ;; This is just an `(ice-9 optargs)' kind of `lambda*', with DSSSL
  ;; keyword-style conversion enabled.  However, using `(ice-9 optargs)', the
  ;; `#:rest' argument can only appear last which not what Skribe/DSSSL
  ;; expect, hence `fix-rest-arg'.
  (define (fix-rest-arg args)
    (let loop ((args args)
	       (result '())
	       (rest-arg #f))
      (if (null? args)
	  (if rest-arg (append (reverse result) rest-arg) (reverse result))
	  (let ((is-rest-arg? (eq? (car args) #:rest)))
	    (loop (if is-rest-arg? (cddr args) (cdr args))
		  (if is-rest-arg? result (cons (car args) result))
		  (if is-rest-arg? (list (car args) (cadr args)) rest-arg))))))

  (let ((name (car bindings))
	(opts (cdr bindings)))
    `(define* ,(cons name (fix-rest-arg opts)) ,@body)))


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
	(error "~a:~a: ~a: ~a ~s" (location-file l) (location-pos l) proc msg shape)
	(error "~a: ~a ~s " proc msg shape))))

(define (skribe-error proc msg obj)
  (if (ast? obj)
      (skribe-ast-error proc msg obj)
      (error proc msg obj)))


;;;
;;; SKRIBE-TYPE-ERROR
;;;
(define (skribe-type-error proc msg obj etype)
  (skribe-error proc (format "~a ~s (~a expected)" msg obj etype) #f))



;;; FIXME: Peut-être virée maintenant
(define (skribe-line-error file line proc msg obj)
  (error (format "%a:%a:  ~a:~a ~S" file line proc msg obj)))


;;;
;;; SKRIBE-WARNING  &  SKRIBE-WARNING/AST
;;;
(define (%skribe-warn level file line lst)
  (let ((port (current-error-port)))
    (format port "**** WARNING:\n")
    (when (and file line) (format port "~a: ~a: " file line))
    (for-each (lambda (x) (format port "~a " x)) lst)
    (newline port)))


(define (skribe-warning level . obj)
  (if (>= *skribe-warning* level)
      (%skribe-warn level #f #f obj)))


(define (skribe-warning/ast level ast . obj)
  (if (>= *skribe-warning* level)
      (let ((l (ast-loc ast)))
	(if (location? l)
	    (%skribe-warn level (location-file l) (location-pos l) obj)
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


;;;
;;; UNSPECIFIED?
;;;
(define (unspecified? obj)
  (eq? obj 'unspecified))

;;;; ======================================================================
;;;;
;;;;   				A C C E S S O R S
;;;;
;;;; ======================================================================

;; 							  SKRIBE-PATH
(define (skribe-path) *skribe-path*)

(define (skribe-path-set! path)
  (if (not (and (list? path) (every string? path)))
      (skribe-error 'skribe-path-set! "Illegal path" path)
      (set! *skribe-path* path)))

;; 							  SKRIBE-IMAGE-PATH
(define (skribe-image-path) *skribe-image-path*)

(define (skribe-image-path-set! path)
  (if (not (and (list? path) (every string? path)))
      (skribe-error 'skribe-image-path-set! "Illegal path" path)
      (set! *skribe-image-path* path)))

;; 							  SKRIBE-BIB-PATH
(define (skribe-bib-path) *skribe-bib-path*)

(define (skribe-bib-path-set! path)
  (if (not (and (list? path) (every string? path)))
      (skribe-error 'skribe-bib-path-set! "Illegal path" path)
      (set! *skribe-bib-path* path)))

;; 							  SKRBE-SOURCE-PATH
(define (skribe-source-path) *skribe-source-path*)

(define (skribe-source-path-set! path)
  (if (not (and (list? path) (every string? path)))
      (skribe-error 'skribe-source-path-set! "Illegal path" path)
      (set! *skribe-source-path* path)))

;;;; ======================================================================
;;;;
;;;; 				Compatibility with Bigloo
;;;;
;;;; ======================================================================

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

(define (symbol-append . l)
  (string->symbol (apply string-append (map symbol->string l))))


(define (make-list n . fill)
  (let ((fill (if (null? fill) (void) (car fill))))
    (let Loop ((i n) (res '()))
      (if (zero? i)
	  res
	  (Loop (- i 1) (cons fill res))))))


(define string-capitalize 	string-titlecase)
(define prefix 			file-prefix)
(define suffix 			file-suffix)
(define system->string		system)
(define any?			any)
(define every?			every)
(define cons* 			list*)
(define find-file/path		(lambda (. args)
				  (format #t "find-file/path: ~a~%" args)
				  #f))
(define process-input-port	#f) ;process-input)
(define process-output-port	#f) ;process-output)
(define process-error-port	#f) ;process-error)

;;;
;;; h a s h   t a b l e s
;;;
(define make-hashtable		(lambda () (make-hash-table)))
(define hashtable? 		hash-table?)
(define hashtable-get		(lambda (h k) (hash-ref h k #f)))
(define hashtable-put!		hash-set!)
(define hashtable-update!	hash-set!)
(define hashtable->list 	(lambda (h)
				  (map cdr (hash-table->list h))))

(define find-runtime-type 	(lambda (obj) obj))

(define-macro (unwind-protect expr1 expr2)
  ;; This is no completely correct. 
  `(dynamic-wind
       (lambda () #f)
       (lambda () ,expr1)
       (lambda () ,expr2)))
