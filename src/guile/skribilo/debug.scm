;;;
;;; debug.scm	-- Debug Facilities (stolen to Manuel Serrano)
;;;
;;;
;;; Copyright © 2003-2004 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
;;;
;;;           Author: Erick Gallesio [eg@essi.fr]
;;;    Creation date: 10-Aug-2003 20:45 (eg)
;;; Last file update: 28-Oct-2004 13:16 (eg)
;;;


(define-module (skribilo debug)
   :export (with-debug %with-debug
	    debug-item skribe-debug set-skribe-debug! add-skribe-debug-symbol
	    no-debug-color)
   :use-module (srfi srfi-17))


(define *skribe-debug*			0)
(define *skribe-debug-symbols*		'())
(define *skribe-debug-color*		#t)
(define *skribe-debug-item*		#f)
(define *debug-port*			(current-error-port))
(define *debug-depth*			0)
(define *debug-margin*			"")
(define *skribe-margin-debug-level*	0)


(define (set-skribe-debug! val)
  (set! *skribe-debug* val))

(define (add-skribe-debug-symbol s)
  (set! *skribe-debug-symbols* (cons s *skribe-debug-symbols*)))


(define (no-debug-color)
  (set! *skribe-debug-color* #f))

(define-public skribe-debug
  (getter-with-setter (lambda () *skribe-debug*)
		      (lambda (val) (set! *skribe-debug* val))))

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
    (if (and *skribe-debug-color*
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
(define (debug-item . args)
  (if (or (>= *skribe-debug* *skribe-margin-debug-level*)
          *skribe-debug-item*)
      (begin
        (display *debug-margin* *debug-port*)
        (display (debug-color (- *debug-depth* 1) "- ") *debug-port*)
        (for-each (lambda (a) (display a *debug-port*)) args)
        (newline *debug-port*))))

;;(define-macro (debug-item  . args)
;;  `())

;;;
;;; %with-debug-margin
;;;
(define (%with-debug-margin margin thunk)
  (let ((om *debug-margin*))
    (set! *debug-depth* (+ *debug-depth* 1))
    (set! *debug-margin* (string-append om margin))
    (let ((res (thunk)))
      (set! *debug-depth* (- *debug-depth* 1))
      (set! *debug-margin* om)
      res)))

;;;
;;; %with-debug
;;
(define (%with-debug lvl lbl thunk)
  (let ((ol *skribe-margin-debug-level*)
	(oi *skribe-debug-item*))
    (set! *skribe-margin-debug-level* lvl)
    (let ((r (if (or (and (number? lvl) (>= *skribe-debug* lvl))
		     (and (symbol? lbl)
			  (memq lbl *skribe-debug-symbols*)
			  (set! *skribe-debug-item* #t)))
		 (begin
		   (display *debug-margin* *debug-port*)
		   (display (if (= *debug-depth* 0)
				(debug-color *debug-depth* "+ " lbl)
				(debug-color *debug-depth* "--+ " lbl))
			    *debug-port*)
		   (newline *debug-port*)
		   (%with-debug-margin (debug-color *debug-depth* "  |")
				       thunk))
		 (thunk))))
      (set! *skribe-debug-item* oi)
      (set! *skribe-margin-debug-level* ol)
      r)))

(define-macro (with-debug  level label . body)
  `(%with-debug ,level ,label (lambda () ,@body)))

;;(define-macro (with-debug  level label . body)
;;  `(begin ,@body))


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
