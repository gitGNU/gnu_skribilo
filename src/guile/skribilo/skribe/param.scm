;;; param.scm
;;;
;;; Copyright 2003  Manuel Serrano
;;; Copyright 2005  Ludovic Courtès  <ludovic.courtes@laas.fr>
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

(define-skribe-module (skribilo skribe param))

;;; Author:  Manuel Serrano
;;; Commentary:
;;;
;;; Definition of various Skribe run-time parameters.
;;;
;;; Code:


;;; The contents of the file below are unchanged compared to Skribe 1.2d's
;;; `param.scm' file found in the `common' directory.


;*---------------------------------------------------------------------*/
;*    *skribe-rc-file* ...                                             */
;*    -------------------------------------------------------------    */
;*    The "runtime command" file.                                      */
;*---------------------------------------------------------------------*/
(define *skribe-rc-file* "skriberc")

;*---------------------------------------------------------------------*/
;*    *skribe-auto-mode-alist* ...                                     */
;*---------------------------------------------------------------------*/
(define *skribe-auto-mode-alist*
   '(("html" . html)
     ("sui" . sui)
     ("tex" . latex)
     ("ctex" . context)
     ("xml" . xml)
     ("info" . info)
     ("txt" . ascii)
     ("mgp" . mgp)
     ("man" . man)))

;*---------------------------------------------------------------------*/
;*    *skribe-auto-load-alist* ...                                     */
;*    -------------------------------------------------------------    */
;*    Autoload engines.                                                */
;*---------------------------------------------------------------------*/
(define *skribe-auto-load-alist*
   '((base . "base.skr")
     (html . "html.skr")
     (sui . "html.skr")
     (latex . "latex.skr")
     (context . "context.skr")
     (xml . "xml.skr")))

;*---------------------------------------------------------------------*/
;*    *skribe-preload* ...                                             */
;*    -------------------------------------------------------------    */
;*    The list of skribe files (e.g. styles) to be loaded at boot-time */
;*---------------------------------------------------------------------*/
(define *skribe-preload*
   '("skribe.skr"))

;*---------------------------------------------------------------------*/
;*    *skribe-precustom* ...                                           */
;*    -------------------------------------------------------------    */
;*    The list of pair <custom x value> to be assigned to the default  */
;*    engine.                                                          */
;*---------------------------------------------------------------------*/
(define *skribe-precustom*
   '())

;*---------------------------------------------------------------------*/
;*    *skribebib-auto-mode-alist* ...                                  */
;*---------------------------------------------------------------------*/
(define *skribebib-auto-mode-alist*
   '(("bib" . "skribebibtex")))

;;; param.scm ends here
