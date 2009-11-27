;;; parameters.scm  --  Skribilo settings as parameter objects.
;;; -*- coding: iso-8859-1 -*-
;;;
;;; Copyright 2005, 2008  Ludovic Courtès <ludo@gnu.org>
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

(define-module (skribilo parameters)
  :use-module (srfi srfi-39))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; This module defines parameter objects that may be used to specify
;;; run-time parameters of a Skribilo process.
;;;
;;; Code:


;;;
;;; Switches.
;;;

(define (make-expect pred pred-name parameter)
  (let ((msg (string-append parameter ": " pred-name " expected")))
    (lambda (val)
      (if (pred val)
	  val
	  (error msg val)))))

(define-macro (define-number-parameter name)
  `(define-public ,name
     (make-parameter 0
		     (make-expect number? "number" ,(symbol->string name)))))

(define-number-parameter *verbose*)
(define-number-parameter *warning*)

(define-public *load-rc-file?* (make-parameter #f))

;;;
;;; Paths.
;;;


(define-macro (define-path-parameter name)
  `(define-public ,name
     (make-parameter (list ".")
		     (make-expect list? "list" ,(symbol->string name)))))


(define-path-parameter *document-path*)
(define-path-parameter *bib-path*)
(define-path-parameter *source-path*)
(define-path-parameter *image-path*)
(define-path-parameter *sui-path*)


;;;
;;; Files.
;;;

(define-public *destination-file* (make-parameter "output.html"))
(define-public *source-file*      (make-parameter "default-input-file.skb"))

;; Base prefix to remove from hyperlinks.
(define-public *ref-base*         (make-parameter ""))

;;; TODO: Skribe used to have other parameters as global variables.  See
;;; which ones need to be kept.


;;; arch-tag: 3c0d2e18-b997-4615-8a3d-b6622ae28874

;;; parameters.scm ends here
