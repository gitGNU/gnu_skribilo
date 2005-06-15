;;;;
;;;; vars.stk	-- Skribe Globals
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
;;;;    Creation date: 11-Aug-2003 16:18 (eg)
;;;; Last file update: 26-Feb-2004 20:36 (eg)
;;;;


;;;
;;; Switches
;;;
(define *skribe-verbose* 	0)
(define *skribe-warning*	5)
(define *load-rc* 		#t)

;;;
;;; PATH variables
;;;
(define *skribe-path* 		#f)
(define *skribe-bib-path* 	'("."))
(define *skribe-source-path*	'("."))
(define *skribe-image-path*	'("."))


(define *skribe-rc-directory*
  (make-path (getenv "HOME") ".skribe"))


;;;
;;; In and out ports
;;; 
(define *skribe-src* 		'())
(define *skribe-dest* 		#f)

;;;
;;; Engine 
;;; 
(define *skribe-engine* 	'html)	;; Use HTML by default

;;;
;;; Misc
;;;
(define *skribe-chapter-split*	'())
(define *skribe-ref-base* 	#f)
(define *skribe-convert-image*  #f)	;; i.e. use the Skribe standard converter
(define *skribe-variants*	'())




;;; Forward definitions (to avoid warnings when compiling Skribe)
;;; This is a KLUDGE.
(define mark #f)
(define ref  #f)
;;(define invoke 3)
(define lookup-markup-writer #f)

; (define-module SKRIBE-ENGINE-MODULE
;   (define find-engine #f))

; (define-module SKRIBE-OUTPUT-MODULE)

; (define-module SKRIBE-RUNTIME-MODULE)
