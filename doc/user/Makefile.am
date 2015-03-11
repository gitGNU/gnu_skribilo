# Chapters and sections.
EXTRA_DIST = bib.skb char.skb colframe.skb document.skb emacs.skb	\
  engine.skb enumeration.skb eq.skb examples.skb figure.skb		\
  font.skb footnote.skb htmle.skb image.skb index.skb infoe.skb		\
  justify.skb latexe.skb loute.skb lib.skb line.skb links.skb		\
  markup.skb ornament.skb package.skb pie.skb prgm.skb sectioning.skb	\
  skribilo-config.skb compiler.skb skribeinfo.skb slide.skb spacing.skb	\
  start.skb syntax.skb table.skb toc.skb user.skb xmle.skb		\
  contexte.skb								\
  lout/book-style.lout lout/front-page.lout.in

# Examples.
EXTRA_DIST += src/api1.skb						\
  src/api10.skb src/api11.skb src/api12.skb src/api13.skb src/api14.skb	\
  src/api15.skb src/api16.skb src/api17.skb src/api18.skb src/api19.skb	\
  src/api2.skb src/api20-html.skb src/api20-lout.skb src/api3.skb	\
  src/api5.skb src/api6.skb src/api7.skb src/api8.skb src/api9.skb	\
  src/bib1.sbib src/bib2.skb src/bib3.skb src/bib4.skb src/bib5.skb	\
  src/bib6.skb								\
  src/eq1.skb src/eq2.skb src/eq3.skb src/html-navtabs.skb		\
  src/index1.skb src/index2.skb src/index3.skb src/links1.skb		\
  src/links2.skb src/pie1.skb src/pie2.skb src/prgm1.skb src/prgm2.skb	\
  src/prgm3.skb src/prgm4.skb src/slides.skb				\
  src/start1.skb src/start2.skb						\
  src/start3.skb src/start4.skb src/start5.skb				\
  src/lout-illustration.skb						\
  src/outline.txt src/scheme.scm

EXTRA_DIST += FONTS

skrinfodir = $(infodir)
skrinfo_DATA = skribilo.info

html_DATA = user.html user.sui
dist_html_DATA = skribilo.css			\
  lobster-1.4.otf junction.ttf

if HAVE_LOUT
BUILT_SOURCES = lout/front-page.lout
else !HAVE_LOUT
BUILT_SOURCES =
endif !HAVE_LOUT

CLEANFILES = $(BUILT_SOURCES) $(html_DATA) skribilo.info

# `user.sui' is produced by the HTML engine.
user.sui: user.html

skribilo = $(top_builddir)/src/pre-inst-skribilo
skrflags  = -I $(srcdir) -P $(top_srcdir)/doc/img		\
	    -S $(srcdir) -S $(top_srcdir)/src/guile		\
	    -B $(srcdir)					\
	    -e '(define %top-srcdir "$(top_srcdir)")'		\
	    -e '(define %top-builddir "$(top_builddir)")'

if HAVE_PLOTICUS
skrflags += -e "(define %have-ploticus? (= 1 1))"		\
	    -e "(define %ploticus-path \"$(PLOTICUS)\")"
else
skrflags += -e "(define %have-ploticus? (= 0 1))"	\
	    -e "(define %ploticus-path (= 0 1))"
endif

if HAVE_LOUT
skrflags += -e "(define %have-lout? (= 1 1))"
else
skrflags += -e "(define %have-lout? (= 0 1))"
endif

load_path = $(top_srcdir)/doc/modules
load_compiled_path = $(top_builddir)/doc/modules

# Silent rules.

AM_V_SKRINFO = $(AM_V_SKRINFO_$(V))
AM_V_SKRINFO_ = $(AM_V_SKRINFO_$(AM_DEFAULT_VERBOSITY))
AM_V_SKRINFO_0 = @echo "  SKRINFO" $@;

AM_V_SKRHTML = $(AM_V_SKRHTML_$(V))
AM_V_SKRHTML_ = $(AM_V_SKRHTML_$(AM_DEFAULT_VERBOSITY))
AM_V_SKRHTML_0 = @echo "  SKRHTML" $@;


skribilo.info: user.skb
	$(AM_V_SKRINFO)GUILE_LOAD_PATH=$(load_path):$$GUILE_LOAD_PATH	\
	GUILE_LOAD_COMPILED_PATH=$(load_compiled_path):$$GUILE_LOAD_COMPILED_PATH \
	GUILE_AUTO_COMPILE=0				\
	$(skribilo) $(skrflags) --target=info -o $@ $<

.skb.html:
	$(AM_V_SKRHTML)GUILE_LOAD_PATH=$(load_path):$$GUILE_LOAD_PATH	\
	GUILE_LOAD_COMPILED_PATH=$(load_compiled_path):$$GUILE_LOAD_COMPILED_PATH \
	GUILE_AUTO_COMPILE=0				\
	$(skribilo) $(skrflags) --target=html -o $@ $<

if HAVE_LOUT

AM_V_SKRLOUT = $(AM_V_SKRLOUT_$(V))
AM_V_SKRLOUT_ = $(AM_V_SKRLOUT_$(AM_DEFAULT_VERBOSITY))
AM_V_SKRLOUT_0 = @echo "  SKRLOUT" $@;

AM_V_LOUT = $(AM_V_LOUT_$(V))
AM_V_LOUT_ = $(AM_V_LOUT_$(AM_DEFAULT_VERBOSITY))
AM_V_LOUT_0 = @echo "  LOUT" $@;

CLEANFILES += user.lout user.lout.ld user.li user.lix

if HAVE_PS2PDF
pdf_DATA = user.pdf
CLEANFILES += $(pdf_DATA)
else !HAVE_PS2PDF
ps_DATA = user.ps
CLEANFILES += $(ps_DATA)
endif !HAVE_PS2PDF

.lout.ps:
	$(AM_V_LOUT)$(LOUT) -r4 -I$(srcdir)/lout -I$(builddir)/lout \
	  -c $(@:%.ps=%) -o $@ $<

# Variable substitution.
include $(top_srcdir)/substitute.am

lout/front-page.lout: lout/front-page.lout.in
	test -d "lout" || mkdir "lout"
	$(substitute) "$^" > "$@.tmp" &&	\
	mv "$@.tmp" "$@"

endif HAVE_LOUT

if HAVE_PS2PDF
.ps.pdf:
	$(PS2PDF) -sPAPERSIZE=a4 -dPDFSETTINGS=/prepress $(@:%.pdf=%.ps)
endif

.skb.lout:
	$(AM_V_SKRLOUT)GUILE_LOAD_PATH=$(load_path):$$GUILE_LOAD_PATH	\
	GUILE_LOAD_COMPILED_PATH=$(load_compiled_path):$$GUILE_LOAD_COMPILED_PATH \
	GUILE_AUTO_COMPILE=0				\
	$(skribilo) $(skrflags) --target=lout -o $@ $<

.skb.tex:
	GUILE_LOAD_PATH=$(load_path):$$GUILE_LOAD_PATH	\
	GUILE_LOAD_COMPILED_PATH=$(load_compiled_path):$$GUILE_LOAD_COMPILED_PATH \
	GUILE_AUTO_COMPILE=0				\
	$(skribilo) $(skrflags) --target=latex -o $@ $<

.skb.ctx:
	GUILE_LOAD_PATH=$(load_path):$$GUILE_LOAD_PATH		\
	GUILE_LOAD_COMPILED_PATH=$(load_compiled_path):$$GUILE_LOAD_COMPILED_PATH \
	GUILE_AUTO_COMPILE=0					\
	$(skribilo) $(skrflags) --target=context -o $@ $<


# Install/uninstall files not listed in `html_DATA'.
# Make the images available alongside the Info manual.
install-data-hook:
	$(INSTALL_DATA) user-*.html *.png "$(DESTDIR)$(htmldir)"
	$(MKDIR_P) "$(DESTDIR)$(infodir)"
	for i in "$(DESTDIR)$(htmldir)/"*.png; do	\
	  (cd "$(DESTDIR)$(infodir)";			\
	   rm -f "`basename $$i`"; $(LN_S) "$$i");	\
	done

uninstall-local:
	-rm -rf "$(DESTDIR)$(htmldir)"/user-*.html \
		"$(DESTDIR)$(htmldir)"/*.png


# Remove the HTML files and images created in the process and
# not listed in `CLEANFILES'.
clean-local:
	-rm -f user-*.html *.eps *.png
	-if test "$(top_builddir)" != "$(top_srcdir)";	\
	 then rm -rf "lout" ; else : ; fi
