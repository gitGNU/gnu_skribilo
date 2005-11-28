#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(skribilo, 1.2, ludovic.courtes@laas.fr)
AM_INIT_AUTOMAKE(skribilo, 1.2)

AC_CONFIG_SRCDIR([src/guile/skribilo/reader.scm])

# Look for Guile.
GUILE_PROGS
GUILE_SITE_DIR

# Need guile-reader 0.2.
GUILE_MODULE_REQUIRED([system reader])

# Look for Lout.
AC_PATH_PROG([LOUT], [lout], [not-found])
AM_CONDITIONAL([HAVE_LOUT], [test "x$LOUT" != "xnot-found"])

AC_SUBST([SKRIBILO_DOC_DIR], ["$datadir/doc/skribilo"])
AC_SUBST([SKRIBILO_EXT_DIR], ["$datadir/skribilo/1.2/"])
AC_SUBST([SKRIBILO_SKR_PATH], ["$GUILE_SITE/"])

AC_OUTPUT([Makefile
	   src/skribilo
	   src/Makefile
	   src/guile/Makefile
	   src/guile/skribilo/Makefile
	   src/guile/skribilo/config.scm
	   src/guile/skribilo/engine/Makefile
	   src/guile/skribilo/reader/Makefile
	   src/guile/skribilo/package/Makefile
	   src/guile/skribilo/skribe/Makefile
	   src/guile/skribilo/coloring/Makefile
	   doc/Makefile
	   doc/user/Makefile])
