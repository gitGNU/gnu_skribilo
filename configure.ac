#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(skribilo, 0.9.2, skribilo-users@nongnu.org)
AC_CONFIG_AUX_DIR(build-aux)
AC_CONFIG_MACRO_DIR(m4)

AM_INIT_AUTOMAKE([gnu no-define check-news readme-alpha])

AC_CONFIG_SRCDIR([src/guile/skribilo/reader.scm])

# GNU Gettext.
AM_GNU_GETTEXT([external])
AM_GNU_GETTEXT_VERSION([0.16.1])

# Specifying the Guile module directory.
AC_ARG_WITH([guilemoduledir],
  [use the specified installation path for Guile modules],
  [case "x$withval" in
     xyes|xno) guilemoduledir="";;
     *)        guilemoduledir="$withval";;
   esac],
  [guilemoduledir=""])

AC_PROG_LN_S
AC_PROG_MKDIR_P

# Look for Guile.
GUILE_PROGS
GUILE_SITE_DIR
AC_PATH_PROGS([GUILD], [guild guile-tools])

# GNU Guile 2.0.x and its compiler.
AM_CONDITIONAL([HAVE_GUILE2],
  [test "x$GUILD" != "x" && \
   "$GUILD" compile --help 2>&1 >/dev/null])

# Need Guile-Reader.
GUILE_MODULE_REQUIRED([system reader])

# SRFI-34 is normally in Guile 1.8, but let's make sure.
GUILE_MODULE_REQUIRED([srfi srfi-34])

# Need SRFI-35, available in Guile >= 1.8.3.
GUILE_MODULE_REQUIRED([srfi srfi-35])

# Need SRFI-37, available in Guile >= 1.8.3.
GUILE_MODULE_REQUIRED([srfi srfi-37])

# Make sure we're not using the broken SRFI-35 from Guile-Lib <= 0.1.6.
SKR_GUILE_SRFI_35_WORKS

# Check for SXML and HTMLPrag from Guile-Lib, needed for the `rss-2' reader.
GUILE_MODULE_AVAILABLE([have_sxml_simple], [(sxml simple)])
GUILE_MODULE_AVAILABLE([have_htmlprag],    [(htmlprag)])
if test "x$have_sxml_simple$have_htmlprag" != "xyesyes"; then
  AC_MSG_WARN([Guile-Lib modules needed by the `rss-2' reader are missing.])
fi

# Look for `convert', from ImageMagick.
AC_PATH_PROG([CONVERT], [convert])
if test "x$CONVERT" == "x"; then
  AC_MSG_ERROR([The `convert' program was not found.  Please install ImageMagick.])
fi

# Look for `fig2dev', from Xfig/Transfig.
AC_PATH_PROG([FIG2DEV], [fig2dev], [fig2dev])

# Look for a suitable version of Lout.
LOUT_PROGRAM([LOUT_REQUIRED_PACKAGE([math])])

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
           doc/Makefile
	   doc/user/Makefile
	   doc/modules/Makefile
	   doc/man/Makefile
	   tests/Makefile
	   emacs/Makefile])
