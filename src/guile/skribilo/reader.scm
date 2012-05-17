;;; reader.scm  --  Skribilo's front-end (aka. reader) interface.
;;; -*- coding: iso-8859-1 -*-
;;;
;;; Copyright 2005, 2009  Ludovic Courtès <ludo@gnu.org>
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

(define-module (skribilo reader)
  :use-module (srfi srfi-9)  ;; records
  :use-module (srfi srfi-17) ;; generalized `set!'
  :use-module (srfi srfi-39) ;; parameter objects
  :use-module (skribilo condition)
  :autoload   (srfi srfi-34) (raise)
  :use-module (srfi srfi-35)
  :export (%make-reader lookup-reader make-reader
	   *document-reader*

	   &reader-search-error reader-search-error?
	   reader-search-error:reader

           define-reader define-public-reader reader?))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; This module contains Skribilo's front-end (aka. ``reader'') interface.
;;; Skribilo's default reader is `(skribilo reader skribe)' which provides a
;;; reader for the Skribe syntax.
;;;
;;; Code:

(define-record-type <reader>
  (%make-reader name version make)
  reader?
  (name      reader:name      reader:set-name!)    ;; a symbol
  (version   reader:version   reader:set-version!) ;; a string
  (make      reader:make      reader:set-make!))   ;; a one-argument proc
                                                   ;; that returns a reader
                                                   ;; proc

(define-public reader:name
  (getter-with-setter reader:name reader:set-name!))

(define-public reader:version
  (getter-with-setter reader:version reader:set-version!))

(define-public reader:make
  (getter-with-setter reader:make reader:set-make!))

(define-macro (define-reader name version make-proc)
  `(define reader-specification
     (%make-reader (quote ,name) ,version ,make-proc)))

(define-macro (define-public-reader name version make-proc)
  `(define-reader ,name ,version ,make-proc))


;;; Error condition.

(define-condition-type &reader-search-error &skribilo-error
  reader-search-error?
  (reader reader-search-error:reader))



;;; The mechanism below is inspired by Guile-VM code written by K. Nishida.

(define (lookup-reader name)
  "Look for a reader named @var{name} (a symbol) in the @code{(skribilo
reader)} module hierarchy.  If no such reader was found, an error is
raised."
  (let ((m (false-if-exception
	    (resolve-module `(skribilo reader ,name)))))
    (if (and (module? m)
	     (module-bound? m 'reader-specification))
	(module-ref m 'reader-specification)
	(raise (condition (&reader-search-error (reader name)))))))

(define (make-reader name)
  "Look for reader @var{name} and instantiate it."
  (let* ((spec (lookup-reader name))
         (make (reader:make spec)))
    (make)))


;;; Current document reader.

(define *document-reader* (make-parameter #f))


;;; reader.scm ends here
