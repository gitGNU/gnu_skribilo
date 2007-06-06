;;; lisp.scm -- Lisp Family Fontification
;;;
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

(define-module (skribilo coloring parameters)
  :use-module (srfi srfi-39)
  :export (*bracket-highlight* *class-highlight* *the-keys*))

;;;
;;; Parameters used by the fontifiers.
;;;

(define *bracket-highlight* (make-parameter #t))
(define *class-highlight*   (make-parameter #t))
(define *the-keys*	    (make-parameter '()))

;;; arch-tag: 232c250e-0022-418f-9219-03b4446d0b55
