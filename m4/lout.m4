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


dnl Usage: LOUT_PROGRAM [ACTION-IF-FOUND] [ACTION-IF-NOT-FOUND]
dnl
dnl Look for Lout.  If found, define the `LOUT' environment variable
dnl containing its path, define Automake condition `HAVE_LOUT', and
dnl run ACTION-IF-FOUND.  If not found, run ACTION-IF-NOT-FOUND.
AC_DEFUN([LOUT_PROGRAM],
[
  AC_PATH_PROG([LOUT], [lout])
  AM_CONDITIONAL([HAVE_LOUT], [test "x$LOUT" != "x"])
  if test "x$LOUT" != "x"; then
    :
    $1
  else
    :
    $2
  fi
])

dnl Usage: LOUT_REQUIRED_PACKAGE PACKAGE
dnl
dnl Makes sure Lout has package PACKAGE.
AC_DEFUN([LOUT_REQUIRED_PACKAGE],
[
  AC_MSG_CHECKING([whether Lout has package `$1'])
  echo "@SysInclude { $1 }" | "$LOUT" 2> /dev/null
  if test "$?" -eq 0; then
    AC_MSG_RESULT([yes])
  else
    AC_MSG_RESULT([no])
    AC_MSG_ERROR([Your installation of Lout doesn't have package `$1', please upgrade.])
  fi
])

dnl Local Variables:
dnl mode: autoconf
dnl coding: utf-8
dnl End
