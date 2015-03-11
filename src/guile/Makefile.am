# Copyright 2005, 2006, 2007, 2008, 2009, 2010, 2012,
#   2015 Ludovic Courtès <ludo@gnu.org>
#
# This file is part of Skribilo.
#
# Skribilo is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# Skribilo is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
# General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

moduledir = $(guilemoduledir)

EXTRA_DIST =						\
  README						\
  skribilo/source/lisp-lex.l				\
  skribilo/source/xml-lex.l skribilo/source/c-lex.l	\
  skribilo/utils/images.in skribilo/config.in		\
  srfi/srfi-64.scm srfi/srfi-64.upstream.scm

SOURCES =								\
  skribilo.scm diff.scm							\
  skribilo/biblio/bibtex.scm skribilo/biblio/author.scm			\
  skribilo/biblio/abbrev.scm skribilo/biblio/template.scm		\
  skribilo/utils/syntax.scm skribilo/utils/compat.scm			\
  skribilo/utils/files.scm skribilo/utils/keywords.scm			\
  skribilo/utils/strings.scm						\
  skribilo/source/parameters.scm skribilo/source/c.scm			\
  skribilo/source/lisp.scm skribilo/source/xml.scm			\
  skribilo/biblio.scm skribilo/color.scm				\
  skribilo/debug.scm skribilo/engine.scm skribilo/evaluator.scm		\
  skribilo/index.scm skribilo/lib.scm skribilo/module.scm		\
  skribilo/output.scm skribilo/prog.scm					\
  skribilo/reader.scm skribilo/resolve.scm				\
  skribilo/source.scm skribilo/parameters.scm skribilo/verify.scm	\
  skribilo/writer.scm skribilo/ast.scm skribilo/location.scm		\
  skribilo/condition.scm skribilo/sui.scm				\
  skribilo/table.scm skribilo/utils/justify.scm				\
  skribilo/utils/text-table.scm

SOURCES += $(readers) $(packages) $(engines)

readers =							\
  skribilo/reader/skribe.scm skribilo/reader/outline.scm

if BUILD_RSS2_READER

readers += skribilo/reader/rss-2.scm

else !BUILD_RSS2_READER

EXTRA_DIST += skribilo/reader/rss-2.scm

endif !BUILD_RSS2_READER


engines =						\
  skribilo/engine/base.scm skribilo/engine/context.scm	\
  skribilo/engine/html.scm skribilo/engine/html4.scm	\
  skribilo/engine/latex.scm skribilo/engine/lout.scm	\
  skribilo/engine/xml.scm skribilo/engine/info.scm

packages =								\
  skribilo/package/acmproc.scm skribilo/package/french.scm		\
  skribilo/package/jfp.scm skribilo/package/letter.scm			\
  skribilo/package/lncs.scm skribilo/package/scribe.scm			\
  skribilo/package/sigplan.scm skribilo/package/slide.scm		\
  skribilo/package/web-article.scm skribilo/package/web-book.scm	\
  skribilo/package/eq.scm skribilo/package/pie.scm			\
  skribilo/package/base.scm skribilo/package/diff.scm			\
  skribilo/package/html-navtabs.scm skribilo/package/web-book2.scm	\
  skribilo/package/pie/lout.scm skribilo/package/eq/lout.scm		\
  skribilo/package/slide/base.scm skribilo/package/slide/latex.scm	\
  skribilo/package/slide/html.scm skribilo/package/slide/lout.scm

# Generated files part of the distribution.
SOURCES +=							\
  skribilo/source/lisp-lex.scm skribilo/source/xml-lex.scm	\
  skribilo/source/c-lex.scm

CLEANFILES = $(nobase_nodist_module_DATA)

include $(top_srcdir)/substitute.am
include $(top_srcdir)/guilec.am

# Generated files not part of the distribution.
nobase_nodist_module_DATA +=			\
  skribilo/config.scm				\
  skribilo/utils/images.scm

if HAVE_GUILE2

nobase_nodist_module_DATA +=			\
 skribilo/config.go				\
 skribilo/utils/images.go

endif HAVE_GUILE2


BUILT_SOURCES =					\
  skribilo/config.scm				\
  skribilo/utils/images.scm

.in.scm:
	$(AM_V_SUBSTITUTE)$(MKDIR_P) "`dirname $@`" ;	\
	$(substitute) "$^" > "$@.tmp" &&		\
	mv "$@.tmp" "$@"

#all-local:
#	if ! test -d "skribilo"; then mkdir "skribilo"; fi

skribilo:
	mkdir "skribilo" && mkdir -p "skribilo/utils" && \
	mkdir -p "skribilo/source"

clean-local:
	-if test "$(top_builddir)" != "$(top_srcdir)";	\
	 then rm -rf "skribilo" ; fi

# Building the lexers with SILex.  You must have SILex installed in
# `$(top_srcdir)/src/guile/silex'.  The Git repository already contains
# it.  Otherwise, you can fetch it from
# http://www.iro.umontreal.ca/~dube/silex-src.tar.gz .
#
# Note: Those files should normally be part of the distribution, making
# this rule useless to the user.
.l.scm:
	$(GUILE) -L $(top_srcdir)/src/guile/silex			\
	         -c '(load-from-path "lex.scm") (lex "$^" "$@")' &&	\
	mv "$@" "$@.tmp" &&						\
	echo '(define-module (skribilo source $(^:skribilo/source/%.l=%))' \
	     > "$@" &&	\
	echo '  :use-module (skribilo lib)' >> "$@" &&			\
	echo '  :use-module (skribilo source parameters)'		\
		>> "$@" &&						\
	echo '  :use-module (srfi srfi-1)' >> "$@" &&			\
	echo '  :export (lexer-init lexer' >> "$@" &&			\
	echo '           lexer-get-func-column' >> "$@" &&		\
	echo '           lexer-get-func-offset' >> "$@" &&		\
	echo '           lexer-get-line lexer-getc' >> "$@" &&		\
	echo '           lexer-ungetc))' >> "$@" &&			\
	cat "$@.tmp" >> "$@" &&						\
	rm "$@.tmp"
