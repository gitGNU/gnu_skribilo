;;;;
;;;; configure.stk	-- Skribe configuration options
;;;; 
;;;; Copyright © 2004 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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
;;;;    Creation date: 10-Feb-2004 11:47 (eg)
;;;; Last file update: 17-Feb-2004 09:43 (eg)
;;;;

(define-module (skribe configure)
   :export (skribe-release skribe-scheme skribe-url
	    skribe-doc-dir skribe-ext-dir skribe-default-path

	    skribe-configure skribe-enforce-configure))

(define (skribe-release)
  "1.2d/skribilo")

(define (skribe-scheme)
  "Guile")

(define (skribe-url)
  "http://www.google.com")

;; FIXME:  The directory names should be defined at installation time.

(define (skribe-doc-dir)
  "/usr/share/doc/skribilo")

(define (skribe-ext-dir)
  "/usr/share/skribilo/ext")

(define (skribe-default-path)
  "/usr/share/skribe/")


(define %skribe-conf
  `((:release ,(skribe-release))
    (:scheme ,(skribe-scheme))
    (:url ,(skribe-url))
    (:doc-dir ,(skribe-doc-dir))
    (:ext-dir ,(skribe-ext-dir))
    (:default-path ,(skribe-default-path))))

;;;
;;; SKRIBE-CONFIGURE
;;;
(define (skribe-configure . opt)
  (let ((conf %skribe-conf))
    (cond
      ((null? opt)
       conf)
      ((null? (cdr opt))
       (let ((cell (assq (car opt) conf)))
	 (if (pair? cell)
	     (cadr cell)
	     'void)))
      (else
       (let loop ((opt opt))
	 (cond
	   ((null? opt)
	    #t)
	   ((not (keyword? (car opt)))
	    #f)
	   ((or (null? (cdr opt)) (keyword? (cadr opt)))
	    #f)
	   (else
	    (let ((cell (assq (car opt) conf)))
	      (if (and (pair? cell)
		       (if (procedure? (cadr opt))
			   ((cadr opt) (cadr cell))
			   (equal? (cadr opt) (cadr cell))))
		  (loop (cddr opt))
		  #f)))))))))
;;;
;;;    SKRIBE-ENFORCE-CONFIGURE ...
;;;
(define (skribe-enforce-configure . opt)
  (let loop ((o opt))
    (when (pair? o)
      (cond
	((or (not (keyword? (car o)))
	     (null? (cdr o)))
	 (skribe-error 'skribe-enforce-configure "Illegal enforcement" opt))
	((skribe-configure (car o) (cadr o))
	 (loop (cddr o)))
	(else
	 (skribe-error 'skribe-enforce-configure
		       (format "Configuration mismatch: ~a" (car o))
		       (if (procedure? (cadr o))
			   (format "provided `~a'"
				   (skribe-configure (car o)))
			   (format "provided `~a', required `~a'"
				   (skribe-configure (car o))
				   (cadr o)))))))))
