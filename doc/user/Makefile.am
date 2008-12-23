# Chapters and sections.
EXTRA_DIST = bib.skb char.skb colframe.skb document.skb emacs.skb	\
  engine.skb enumeration.skb eq.skb examples.skb figure.skb		\
  font.skb footnote.skb htmle.skb image.skb index.skb			\
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
  src/outline.txt src/scheme.scm


skrinfodir = $(infodir)
skrinfo_DATA = skribilo.info

html_DATA = user.html user.sui
dist_html_DATA = skribilo.css
BUILT_SOURCES = lout/front-page.lout
CLEANFILES = $(BUILT_SOURCES) $(html_DATA) skribilo.info

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


load_path = $(top_srcdir)/doc/modules


skribilo.info: user.skb
	GUILE_LOAD_PATH=$(load_path):$$GUILE_LOAD_PATH \
	$(skribilo) $(skrflags) --target=info -o $@ $<

.skb.html:
	GUILE_LOAD_PATH=$(load_path):$$GUILE_LOAD_PATH \
	$(skribilo) $(skrflags) --target=html -o $@ $<

if HAVE_LOUT

CLEANFILES += user.lout user.lout.ld user.li user.lix

if HAVE_PS2PDF
pdf_DATA = user.pdf
CLEANFILES += $(pdf_DATA)
else !HAVE_PS2PDF
ps_DATA = user.ps
CLEANFILES += $(ps_DATA)
endif !HAVE_PS2PDF

.lout.ps:
	$(LOUT) -r4 -I$(srcdir)/lout -I$(builddir)/lout \
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
	GUILE_LOAD_PATH=$(load_path):$$GUILE_LOAD_PATH \
	$(skribilo) $(skrflags) --target=lout -o $@ $<

.skb.tex:
	GUILE_LOAD_PATH=$(load_path):$$GUILE_LOAD_PATH \
	$(skribilo) $(skrflags) --target=latex -o $@ $<

.skb.ctx:
	GUILE_LOAD_PATH=$(load_path):$$GUILE_LOAD_PATH \
	$(skribilo) $(skrflags) --target=context -o $@ $<


# Install/uninstall files not listed in `html_DATA'.
install-data-hook:
	$(INSTALL_DATA) user-*.html *.png "$(DESTDIR)$(htmldir)"

uninstall-local:
	-rm -rf "$(DESTDIR)$(htmldir)"/user-*.html \
		"$(DESTDIR)$(htmldir)"/*.png


# Remove the HTML files and images created in the process and
# not listed in `CLEANFILES'.
clean-local:
	-rm -f user-*.html *.eps *.png
	-if test "$(top_builddir)" != "$(top_srcdir)";	\
	 then rm -rf "lout" ; else : ; fi
