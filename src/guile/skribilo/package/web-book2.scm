;;; web-book2.scm  --  Another web book style.
;;;
;;; Copyright 2008  Ludovic Courtès <ludo@gnu.org>
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

(define-module (skribilo package web-book2)
  :use-module (skribilo ast)
  :use-module (skribilo engine)
  :use-module (skribilo package base)

  :use-module (skribilo utils syntax)
  :use-module (skribilo utils keywords)

  :use-module (srfi srfi-1)

  :replace (chapter section subsection subsubsection))

(fluid-set! current-reader %skribilo-module-reader)

;;; Author: Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; Another variant of the web book publishing.  This module is purely
;;; functional, i.e., unlike `web-book', it doesn't modify the HTML engine
;;; customs or writers.  Instead, it replaces the `chapter' and
;;; `(sub)*section' markups with variants that produce a small table of
;;; contents at the beginning of new files.
;;;
;;; Code:


;;;
;;; Small table-of-contents.
;;;

(define %small-toc-class
  "small-toc")

(define (section? n)
  (and (container? n)
       (memq (markup-markup n)
             '(chapter section subsection subsubsection))))

(define (make-small-toc n e)
  ;; Return a small table of contents, for use at the beginning of chapter N.
  (define enclose
    (if (engine-format? "html" e)
        (lambda (toc)
          (! (format #f "\n<div class=\"~a\">\n$1\n</div>" %small-toc-class)
             toc))
        (lambda (toc)
          (p :class %small-toc-class toc))))

  (define (make-uplink)
    (let ((parent (ast-parent n)))
      (and parent
           (ref :handle (handle parent)
                :text (list (symbol "uparrow") " "
                            (markup-option parent :title))))))

  (let ((kids (filter section? (markup-body n))))
    (enclose
       (list
        (and (not (null? kids))
             (list "Contents"
                   (itemize
                    (map (lambda (section)
                           (item (ref :handle (handle section)
                                      :text (markup-option section :title))))
                         kids))))
        (make-uplink)))))


(define (in-file-of-its-own? n e)
  ;; Return true if node N is in an HTML file of its own.
  (and (engine-format? "html" e)
       (section? n)
       (or (markup-option n :file)
           (let ((custom (symbol-append (markup-markup n) '-file)))
             (engine-custom e custom)))))


;;;
;;; Overrides.
;;;

(define %base-package
  (resolve-interface '(skribilo package base)))

(define (make-overriding-markup markup)
  ;; Override the `chapter' markup from the `base' package to allow the
  ;; production of a small TOC at the beginning of each chapter.
  (let ((real-markup (module-ref %base-package markup)))
    (lambda args
      ;;(format (current-error-port) "in new `~a'~%" markup)
      (if (engine-format? "html")
          (apply real-markup
                 (append (concatenate (the-options args))
                         (cons (resolve (lambda (n e env)
                                          (let ((p (ast-parent n)))
                                            (and (in-file-of-its-own? p e)
                                                 (make-small-toc p e)))))
                               (the-body args))))
          (apply real-markup args)))))

(define chapter
  (make-overriding-markup 'chapter))

(define section
  (make-overriding-markup 'section))

(define subsection
  (make-overriding-markup 'subsection))

(define subsubsection
  (make-overriding-markup 'subsubsection))

;;; web-book2.scm ends here
