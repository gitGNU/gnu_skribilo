;;; images.scm  --  Images handling utilities.
;;;
;;; Copyright 2003, 2004  Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
;;; Copyright 2005, 2006  Ludovic Courtès <ludovic.courtes@laas.fr>
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

(define-module (skribilo utils images)
  :export (convert-image
	   *fig-convert-program* *bitmap-convert-program*)

  :autoload (skribilo utils files) (file-suffix file-prefix)
  :autoload (skribilo parameters)  (*image-path*)
  :autoload   (skribilo condition) (&file-search-error)
  :autoload   (srfi srfi-34) (raise)
  :use-module (srfi srfi-35)
  :use-module (srfi srfi-39))

;;; Author:  Erick Gallesio, Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; This module provides convenience functions to handle image files, notably
;;; for format conversion via ImageMagick's `convert'.
;;;
;;; Code:

(define *fig-convert-program*     (make-parameter "fig2dev -L"))
(define *generic-convert-program* (make-parameter "convert"))

(define (builtin-convert-image from fmt dir)
  (let* ((s  (file-suffix from))
	 (f  (string-append (file-prefix (basename from)) "." fmt))
	 (to (string-append dir "/" f)))   ;; FIXME:
    (cond
      ((string=? s fmt)
       to)
      ((file-exists? to)
       to)
      (else
       (let ((c (if (string=? s "fig")
		    (string-append (*fig-convert-program*) " "
				   fmt " " from " > " to)
		    (string-append (*generic-convert-program*) " "
				   from " " to))))
	 (cond
	   ((> (*verbose*) 1)
	    (format (current-error-port) "  [converting image: ~S (~S)]" from c))
	   ((> (*verbose*) 0)
	    (format (current-error-port) "  [converting image: ~S]" from)))
	 (and (zero? (system c))
	      to))))))

(define (convert-image file formats)
  (let ((path (search-path (*image-path*) file)))
    (if (not path)
	(raise (condition (&file-search-error (file-name file)
					      (path (*image-path*)))))
	(let ((suf (file-suffix file)))
	  (if (member suf formats)
	      (let* ((dir (if (string? (*destination-file*))
			      (dirname (*destination-file*))
			      #f)))
		(if dir
		    (let* ((dest (basename path))
			   (dest-path (string-append dir "/" dest)))
		      (if (not (string=? path dest-path))
			  (copy-file path dest-path))
		      dest)
		    path))
	      (let loop ((fmts formats))
		(if (null? fmts)
		    #f
		     (let* ((dir (if (string? (*destination-file*))
				     (dirname (*destination-file*))
				     "."))
			    (p (builtin-convert-image path (car fmts) dir)))
		       (if (string? p)
			   p
			   (loop (cdr fmts)))))))))))


;;; arch-tag: a1992fa8-6073-4cd7-a018-80e2cc8d537c

;;; images.scm ends here
