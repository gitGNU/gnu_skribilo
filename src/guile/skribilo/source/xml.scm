;;; xml.scm -- XML syntax highlighting.
;;;
;;; Copyright 2005, 2007  Ludovic Courtès <ludovic.courtes@laas.fr>
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

(define-module (skribilo source xml)
  :export (xml)
  :use-module (skribilo lib)
  :use-module (ice-9 regex))


(define %comment-rx (make-regexp "<!--(.|\\n)*-->" regexp/extended))

(define (xml-fontifier str)
  (let loop ((start 0)
	     (result '()))
    (if (>= start (string-length str))
	(reverse! result)
	(case (string-ref str start)
	  ((#\")
	   (let ((end (string-index str start #\")))
	     (if (not end)
		 (skribe-error 'xml-fontifier
			       "unterminated XML string"
			       (string-drop str start))
		 (loop end
		       (cons (new markup
				  (markup '&source-string)
				  (body (substring str start end)))
			     result)))))
	  ((#\<)
	   (let ((end (string-index str #\> start)))
	     (if (not end)
		 (skribe-error 'xml-fontifier
			       "unterminated XML tag"
			       (string-drop str start))
		 (let ((comment? (regexp-exec %comment-rx
					      (substring str start end))))
		   (loop end
			 (cons (if comment?
				   (new markup
					(markup '&source-comment)
					(body (substring str start end)))
				   (new markup
					(markup '&source-module)
					(body (substring str start end))))
			       result))))))

	  (else
	   (loop (+ 1 start)
		 (if (or (null? result)
			 (not (string? (car result))))
		     (cons (string (string-ref str start)) result)
		     (cons (string-append (car result)
					  (string (string-ref str start)))
			   (cdr result)))))))))


(define xml
  (new language
       (name "xml")
       (fontifier xml-fontifier)
       (extractor #f)))

;;; xml.scm ends here
