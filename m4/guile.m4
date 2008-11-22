dnl -* Autoconf -*-
dnl
dnl Copyright (C) 2008  Ludovic Courtès <ludo@gnu.org>
dnl
dnl This file is free software; you can redistribute it and/or
dnl modify it under the terms of the GNU Lesser General Public
dnl License as published by the Free Software Foundation; either
dnl version 2.1 of the License, or (at your option) any later version.
dnl 
dnl This file is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl Lesser General Public License for more details.
dnl 
dnl You should have received a copy of the GNU Lesser General Public
dnl License along with this file; if not, write to the Free Software
dnl Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

dnl Usage: SKR_GUILE_SRFI_35_WORKS
dnl
dnl Fails if the implementation of `(srfi srfi-35)' doesn't correctly
dnl handle condition type inheritance, which is the case with the
dnl implementation provided by Guile-Lib as of version 0.1.6.
AC_DEFUN([SKR_GUILE_SRFI_35_WORKS], [
  AC_MSG_CHECKING([whether `(srfi srfi-35)' correctly handles condition type inheritance])
  GUILE_CHECK([srfi_35_status],
    [(use-modules (srfi srfi-35))
     (define-condition-type &foo &error foo?)
     (define-condition-type &bar &foo bar?)
     (let ((c (condition (&bar))))
       (exit (and (condition? c)
                  (error? c)
		  (foo? c)
		  (bar? c))))])
  if test "$srfi_35_status" -eq 0; then
    AC_MSG_RESULT([yes])
  else
    AC_MSG_RESULT([no])
    AC_MSG_ERROR([Your version of `(srfi srfi-35)' (possibly from Guile-Lib) doesn't work correctly, please use the one from Guile 1.8.3+.])
  fi
])
