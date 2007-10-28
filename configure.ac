#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(skribilo, 1.2, skribilo-users@nongnu.org)
AC_CONFIG_AUX_DIR(build-aux)
AC_CONFIG_MACRO_DIR(m4)

AM_INIT_AUTOMAKE([gnu no-define check-news])

AC_CONFIG_SRCDIR([src/guile/skribilo/reader.scm])


# GNU Gettext.
AM_GNU_GETTEXT([external])

# Look for Guile.
GUILE_PROGS
GUILE_SITE_DIR

# Guile-Lint (http://www.geocities.com/user42_kevin/guile-lint/index.html)
AC_PATH_PROG([GUILE_LINT], [guile-lint])
AC_SUBST([GUILE_LINT])
AM_CONDITIONAL([HAVE_GUILE_LINT], test "x$GUILE_LINT" != "x")


# Need Guile-Reader.
GUILE_MODULE_REQUIRED([system reader])

# SRFI-34 is normally in Guile 1.8, but let's make sure.
GUILE_MODULE_REQUIRED([srfi srfi-34])

# Need SRFI-35, available in `guile-library' or Guile >= 1.8.3.
GUILE_MODULE_REQUIRED([srfi srfi-35])

# Look for Lout.
AC_PATH_PROG([LOUT], [lout])
AM_CONDITIONAL([HAVE_LOUT], [test "x$LOUT" != "x"])

# Look for Ploticus.
AC_PATH_PROGS([PLOTICUS], [ploticus pl])
AM_CONDITIONAL([HAVE_PLOTICUS], [test "x$PLOTICUS" != "x"])


AC_SUBST([SKRIBILO_DOC_DIR], ["$datadir/doc/skribilo"])
AC_SUBST([SKRIBILO_EXT_DIR], ["$datadir/skribilo/1.2/"])
AC_SUBST([SKRIBILO_SKR_PATH], ["$GUILE_SITE/"])

AC_CONFIG_COMMANDS([skribilo-executable], [chmod a+x src/skribilo])

AC_OUTPUT([Makefile
	   po/Makefile.in
	   src/skribilo
	   src/Makefile
	   src/guile/Makefile
	   src/guile/skribilo/Makefile
	   src/guile/skribilo/config.scm
	   src/guile/skribilo/utils/Makefile
	   src/guile/skribilo/engine/Makefile
	   src/guile/skribilo/reader/Makefile
	   src/guile/skribilo/package/Makefile
	   src/guile/skribilo/package/slide/Makefile
	   src/guile/skribilo/package/eq/Makefile
	   src/guile/skribilo/package/pie/Makefile
	   src/guile/skribilo/source/Makefile
	   src/guile/skribilo/biblio/Makefile
	   doc/Makefile
	   doc/user/Makefile
	   doc/user/src/Makefile
	   doc/img/Makefile
	   doc/dir/Makefile
	   doc/modules/Makefile
	   doc/modules/skribilo/Makefile
	   doc/modules/skribilo/documentation/Makefile])
