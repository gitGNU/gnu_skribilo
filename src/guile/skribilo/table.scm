;;; table.scm  --  Routines to operate on `table' markups.
;;; -*- coding: iso-8859-1 -*-
;;;
;;; Copyright 2008  Ludovic Courtès <ludo@gnu.org>
;;; Copyright 2001  Manuel Serrano
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

(define-module (skribilo table)
  :use-module (skribilo ast)
  :use-module (skribilo utils syntax)

  :export (table-column-count))

;;; Author: Manuel Serrano, Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; This module provides utility functions to operate on `table' ASTs.  It is
;;; partly based on code from Scribe 1.1a.
;;;
;;; Code:

(skribilo-module-syntax)


(define (table-column-count t)
  ;; Return the number of columns contained in table T.
  (define (row-columns row)
    (let loop ((cells (markup-body row))
               (nbcols 0))
      (if (null? cells)
          nbcols
          (loop (cdr cells)
                (+ nbcols (markup-option (car cells) :colspan))))))

  (let loop ((rows (markup-body t))
             (nbcols 0))
    (if (null? rows)
        nbcols
        (loop (cdr rows)
              (max (row-columns (car rows)) nbcols)))))

;;; table.scm ends here
