;;; xml.scm  --  Generic XML engine.
;;; -*- coding: iso-8859-1 -*-
;;;
;;; Copyright 2003, 2004  Manuel Serrano
;;; Copyright 2007  Ludovic Courtès <ludo@chbouib.org>
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

(define-module (skribilo engine xml)
  :use-module (skribilo ast)
  :use-module (skribilo engine)
  :use-module (skribilo writer)
  :use-module (skribilo utils strings)
  :use-module (skribilo utils syntax)
  :autoload   (skribilo output)         (output)
  :use-module (srfi srfi-1)
  :export (xml-engine))

(skribilo-module-syntax)



;*---------------------------------------------------------------------*/
;*    xml-engine ...                                                   */
;*---------------------------------------------------------------------*/
(define xml-engine
   ;; setup the xml engine
  (make-engine 'xml
               :version 1.0
               :format "html"
               :delegate (find-engine 'base)
               :filter (make-string-replace '((#\< "&lt;")
                                              (#\> "&gt;")
                                              (#\& "&amp;")
                                              (#\" "&quot;")
                                              (#\@ "&#x40;")))))

;*---------------------------------------------------------------------*/
;*    markup ...                                                       */
;*---------------------------------------------------------------------*/
(let ((xml-margin 0))
   (define (keyword->string kw)
     (symbol->string (keyword->symbol kw)))
   (define (make-margin)
      (make-string xml-margin #\space))
   (define (xml-attribute? val)
      (cond
	 ((or (string? val) (number? val) (boolean? val))
	  #t)
	 ((list? val)
	  (every xml-attribute? val))
	 (else
	  #f)))
   (define (xml-attribute att val)
      (let ((s (keyword->string att)))
	 (format #t " ~a=\"" s)
	 (let loop ((val val))
	    (cond
	       ((or (string? val) (number? val))
		(display val))
	       ((boolean? val)
		(display (if val "true" "false")))
	       ((pair? val)
		(for-each loop val))
	       (else
		#f)))
	 (display #\")))
   (define (xml-option opt val e)
      (let ((m (make-margin))
            (s (keyword->string opt)))
	 (format #t "~a<~a>\n" m s)
	 (output val e)
	 (format #t "~a</~a>\n" m s)))
   (define (xml-options n e)
      ;; display the true options
      (let ((opts (filter (lambda (o)
			     (and (keyword? (car o))
				  (not (xml-attribute? (cadr o)))))
			  (markup-options n))))
	 (if (pair? opts)
	     (let ((m (make-margin)))
		(display m)
		(display "<options>\n")
		(set! xml-margin (+ xml-margin 1))
		(for-each (lambda (o)
			     (xml-option (car o) (cadr o) e))
			  opts)
		(set! xml-margin (- xml-margin 1))
		(display m)
		(display "</options>\n")))))
   (markup-writer #t xml-engine
      :options 'all
      :before (lambda (n e)
		 (format #t "~a<~a" (make-margin) (markup-markup n))
		 ;; display the xml attributes
		 (for-each (lambda (o)
			      (if (and (keyword? (car o))
				       (xml-attribute? (cadr o)))
				  (xml-attribute (car o) (cadr o))))
			   (markup-options n))
		 (set! xml-margin (+ xml-margin 1))
		 (display ">\n"))
      :action (lambda (n e)
		 ;; options
		 (xml-options n e)
		 ;; body
		 (output (markup-body n) e))
      :after (lambda (n e)
		(format #t "~a</~a>\n" (make-margin) (markup-markup n))
		(set! xml-margin (- xml-margin 1)))))

