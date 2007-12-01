# Chapters and sections.
EXTRA_DIST = bib.skb char.skb colframe.skb document.skb emacs.skb	\
  engine.skb enumeration.skb eq.skb examples.skb figure.skb		\
  font.skb footnote.skb htmle.skb image.skb index.skb			\
  justify.skb latexe.skb lib.skb line.skb links.skb			\
  markup.skb ornament.skb package.skb pie.skb prgm.skb sectioning.skb	\
  skribe-config.skb skribec.skb skribeinfo.skb slide.skb start.skb	\
  syntax.skb table.skb toc.skb user.skb xmle.skb			\
  lout/book-style.lout

# Examples.
EXTRA_DIST += src/api1.skb						\
  src/api10.skb src/api11.skb src/api12.skb src/api13.skb src/api14.skb	\
  src/api15.skb src/api16.skb src/api17.skb src/api18.skb src/api19.skb	\
  src/api2.skb src/api20-html.skb src/api20-lout.skb src/api3.skb	\
  src/api5.skb src/api6.skb src/api7.skb src/api8.skb src/api9.skb	\
  src/bib2.skb src/bib3.skb src/bib4.skb src/bib5.skb src/bib6.skb	\
  src/eq1.skb src/eq2.skb src/eq3.skb src/html-navtabs.skb		\
  src/index1.skb src/index2.skb src/index3.skb src/links1.skb		\
  src/links2.skb src/pie1.skb src/pie2.skb src/prgm1.skb src/prgm2.skb	\
  src/prgm3.skb src/slides.skb src/start1.skb src/start2.skb		\
  src/start3.skb src/start4.skb src/start5.skb


BUILT_SOURCES = doc-config.scm
html_DATA = user.html
CLEANFILES = $(BUILT_SOURCES) $(html_DATA)

skribilo = $(top_builddir)/src/pre-inst-skribilo
skrflags = -I ../ -P ../img --compat=skribe
load_path = $(top_srcdir)/src/guile:$(top_srcdir)/src/guile/skribilo/package:$(top_builddir)/src/guile:$(top_srcdir)/doc/modules


.skb.html:
	GUILE_LOAD_PATH=$(load_path):$$GUILE_LOAD_PATH \
	$(skribilo) $(skrflags) --target html -o $@ $<

if HAVE_LOUT

CLEANFILES += user.lout user.lout.ld user.li user.lix

if HAVE_PS2PDF
pdf_DATA = user.pdf
CLEANFILES += $(pdf_DATA)
else
ps_DATA = user.ps
CLEANFILES += $(ps_DATA)
endif

.skb.lout:
	GUILE_LOAD_PATH=$(load_path):$$GUILE_LOAD_PATH \
	$(skribilo) $(skrflags) --target lout -o $@ $<

.lout.ps:
	$(LOUT) -r4 -I$(srcdir)/lout -c $(@:%.ps=%) -o $@ $<

endif

if HAVE_PS2PDF
.ps.pdf:
	$(PS2PDF) $(@:%.pdf=%.ps)
endif

if HAVE_PLOTICUS
doc-config.scm:
	echo "(define %have-ploticus? #t)" > $@
	echo "(define %ploticus-path \"$(PLOTICUS)\")" >> $@
else
doc-config.scm:
	echo "(define %have-ploticus? #f) (define %ploticus-path #f)" > $@
endif
