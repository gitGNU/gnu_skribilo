;;; debug.scm  --  Debugging facilities.
;;;
;;; Copyright 2003, 2004  Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
;;; Copyright 2005, 2006  Ludovic Courtès <ludovic.courtes@laas.fr>
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


(define-module (skribilo debug)
  :use-module (skribilo utils syntax)
  :use-module (srfi srfi-39)
  :export-syntax (debug-item debug-bold with-debug))

(fluid-set! current-reader %skribilo-module-reader)


;;;
;;; Parameters.
;;;

;; Current debugging level.
(define-public *debug*
  (make-parameter 0 (lambda (val)
		      (cond ((number? val) val)
			    ((string? val)
			     (string->number val))
			    (else
			     (error "*debug*: wrong argument type"
				    val))))))

;; Whether to use colors.
(define-public *debug-use-colors?* (make-parameter #t))

;; Where to spit debugging output.
(define-public *debug-port* (make-parameter (current-output-port)))

;; Whether to debug individual items.
(define-public *debug-item?* (make-parameter #f))

;; Watched (debugged) symbols (procedure names).
(define-public *watched-symbols* (make-parameter '()))



;;;
;;; Implementation.
;;;

(define *debug-depth*   (make-parameter 0))
(define *debug-margin*	(make-parameter ""))
(define *margin-level*  (make-parameter 0))



;;
;;   debug-port
;;
; (define (debug-port . o)
;    (cond
;       ((null? o)
;        *debug-port*)
;       ((output-port? (car o))
;        (set! *debug-port* o)
;        o)
;       (else
;        (error 'debug-port "Illegal debug port" (car o)))))
;

;;;
;;; debug-color
;;;
(define (debug-color col . o)
  (with-output-to-string
    (if (and (*debug-use-colors?*)
	     (equal? (getenv "TERM") "xterm"))
	(lambda ()
	  (format #t "[0m[1;~Am" (+ 31 col))
	  (for-each display o)
	  (display "[0m"))
	(lambda ()
	  (for-each display o)))))

;;;
;;; debug-bold
;;;
(define (debug-bold . o)
   (apply debug-color -30 o))

;;;
;;; debug-item
;;;
(define-macro (debug-item . args)
  `(if (*debug-item?*) (%do-debug-item ,@args)))

(define-public (%do-debug-item . args)
  (begin
    (display (*debug-margin*) (*debug-port*))
    (display (debug-color (- (*debug-depth*) 1) "- ") (*debug-port*))
    (for-each (lambda (a) (display a (*debug-port*))) args)
    (newline (*debug-port*))))

;;(define-macro (debug-item  . args)
;;  `())


;;;
;;; %with-debug-margin
;;;
(define (%with-debug-margin margin thunk)
  (parameterize ((*debug-depth*   (+ (*debug-depth*) 1))
		 (*debug-margin*  (string-append (*debug-margin*) margin)))
    (thunk)))

;;;
;;; %with-debug
;;;
(define-public (%do-with-debug lvl lbl thunk)
  (parameterize ((*margin-level* lvl)
                 (*debug-item?* #t))
    (display (*debug-margin*) (*debug-port*))
    (display (if (= (*debug-depth*) 0)
                 (debug-color (*debug-depth*) "+ " lbl)
                 (debug-color (*debug-depth*) "--+ " lbl))
             (*debug-port*))
    (newline (*debug-port*))
    (%with-debug-margin (debug-color (*debug-depth*) "  |")
                        thunk)))

(define-macro (with-debug level label . body)
  ;; We have this as a macro in order to avoid procedure calls in the
  ;; non-debugging case.  Unfortunately, the macro below duplicates BODY,
  ;; which has a negative impact on memory usage and startup time (XXX).
  (if (number? level)
      `(if (or (>= (*debug*) ,level)
               (memq ,label (*watched-symbols*)))
           (%do-with-debug ,level ,label (lambda () ,@body))
           (begin ,@body))
      (error "with-debug: syntax error")))


; Example:

; (with-debug 0 'foo1.1
;   (debug-item 'foo2.1)
;   (debug-item 'foo2.2)
;   (with-debug 0 'foo2.3
;      (debug-item 'foo3.1)
;      (with-debug 0 'foo3.2
;	(debug-item 'foo4.1)
;	(debug-item 'foo4.2))
;      (debug-item 'foo3.3))
;   (debug-item 'foo2.4))

;;; debug.scm ends here
