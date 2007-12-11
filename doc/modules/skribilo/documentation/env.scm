;;; env.scm  --  The environment variables for the documentation.
;;;
;;; Copyright 2005, 2006, 2007  Ludovic Courtès <ludo@gnu.org>
;;; Copyright 2003, 2004  Manuel Serrano
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

(define-module (skribilo documentation env)
  :use-module (skribilo config)
  :use-module (skribilo engine))

(define-public *serrano-url* "http://www.inria.fr/mimosa/Manuel.Serrano")
(define-public *serrano-mail* "Manuel.Serrano@sophia.inria.fr")
(define-public *courtes-mail* "ludo@gnu.org")
(define-public *html-url* "http://www.w3.org/TR/html4")
(define-public *html-form* "interact/forms.html")
(define-public *emacs-url* "http://www.gnu.org/software/emacs")
(define-public *xemacs-url* "http://www.xemacs.org")
(define-public *texinfo-url* "http://www.texinfo.org")
(define-public *r5rs-url* "http://www.inria.fr/mimosa/fp/Bigloo/doc/r5rs.html")
(define-public *bigloo-url* "http://www.inria.fr/mimosa/fp/Bigloo")

(define-public *skribe-user-doc-url*
  (string-append (skribilo-doc-directory) "/user.html"))
(define-public *skribe-dir-doc-url*
  (string-append (skribilo-doc-directory) "/dir.html"))

(define-public *prgm-width* 97.)
(define-public *prgm-skribe-color* "#ffffcc")
(define-public *prgm-default-color* "#ffffcc")
(define-public *prgm-xml-color* "#ffcccc")
(define-public *prgm-example-color* "#ccccff")
(define-public *disp-color* "#ccffcc")
(define-public *header-color* "#cccccc")

(define-public *api-engines* (map find-engine '(html latex xml)))
