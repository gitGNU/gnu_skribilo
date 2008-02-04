#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(skribilo, 0.9.0, skribilo-users@nongnu.org)
AC_CONFIG_AUX_DIR(build-aux)
AC_CONFIG_MACRO_DIR(m4)

AM_INIT_AUTOMAKE([gnu no-define check-news readme-alpha])

AC_CONFIG_SRCDIR([src/guile/skribilo/reader.scm])


# GNU Gettext.
AM_GNU_GETTEXT([external])

# Specifying the Guile module directory.
AC_ARG_WITH([guilemoduledir],
  [use the specified installation path for Guile modules],
  [case "x$withval" in
     xyes|xno) guilemoduledir="";;
     *)        guilemoduledir="$withval";;
   esac],
  [guilemoduledir=""])


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

# Need SRFI-35, available in Guile >= 1.8.3.
GUILE_MODULE_REQUIRED([srfi srfi-35])

# Need SRFI-37, available in Guile >= 1.8.3.
GUILE_MODULE_REQUIRED([srfi srfi-37])


# Look for Lout.
AC_PATH_PROG([LOUT], [lout])
AM_CONDITIONAL([HAVE_LOUT], [test "x$LOUT" != "x"])

# Look for `ps2pdf' from GhostScript.
AC_PATH_PROG([PS2PDF], [ps2pdf])
AM_CONDITIONAL([HAVE_PS2PDF], [test "x$PS2PDF" != "x"])

# Look for Ploticus.
AC_PATH_PROGS([PLOTICUS], [ploticus pl])
AM_CONDITIONAL([HAVE_PLOTICUS], [test "x$PLOTICUS" != "x"])

# Emacs.
AM_PATH_LISPDIR


pkgdatadir="$datadir/$PACKAGE_NAME"
if test "x$guilemoduledir" = "x"; then
  guilemoduledir="$pkgdatadir/$PACKAGE_VERSION"
fi
AC_SUBST([guilemoduledir])

if test "$guilemoduledir" != "$GUILE_SITE"; then
   # Guile won't be able to locate the modules "out of the box", so
   # warn the user.  OTOH, `skribilo' will find them, no matter where
   # it is.
   AC_MSG_NOTICE([`guilemoduledir' ($guilemoduledir) is different from `GUILE_SITE' ($GUILE_SITE).])
   AC_MSG_NOTICE([There is nothing wrong with that, but to access Skribilo modules from Guile,])
   AC_MSG_NOTICE([you will need, e.g., to adjust the `GUILE_LOAD_PATH' environment variable accordingly.])
   AC_MSG_NOTICE([Alternatively, you can re-run `configure' with `--with-guilemoduledir=$GUILE_SITE'.])
fi


AC_OUTPUT([Makefile
	   po/Makefile.in
	   src/Makefile
	   src/guile/Makefile
	   src/guile/skribilo/Makefile
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
	   doc/img/Makefile
	   doc/dir/Makefile
	   doc/modules/Makefile
	   doc/modules/skribilo/Makefile
	   doc/modules/skribilo/documentation/Makefile
	   emacs/Makefile])
