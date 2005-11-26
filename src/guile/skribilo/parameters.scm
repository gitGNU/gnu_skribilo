;;; parameters.scm  --  Skribilo settings as parameter objects.
;;;
;;; Copyright 2005  Ludovic Courtès <ludovic.courtes@laas.fr>
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

(define-public *verbose*       (make-parameter #f))
(define-public *warning*       (make-parameter 5))
(define-public *load-rc-file?* (make-parameter #f))

;;;
;;; Paths.
;;;

(define-public *document-path* (make-parameter (list ".")))
(define-public *bib-path*      (make-parameter (list ".")))
(define-public *source-path*   (make-parameter (list ".")))
(define-public *image-path*    (make-parameter (list ".")))

;;;
;;; Files.
;;;

(define-public *destination-file* (make-parameter "output.html"))
(define-public *source-file*      (make-parameter "default-input-file.skb"))


;;; TODO: Skribe used to have other parameters as global variables.  See
;;; which ones need to be kept.


;;; arch-tag: 3c0d2e18-b997-4615-8a3d-b6622ae28874

;;; parameters.scm ends here
