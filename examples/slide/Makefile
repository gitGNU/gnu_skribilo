#*=====================================================================*/
#*    serrano/prgm/project/skribe/examples/slide/Makefile              */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 11 10:19:46 2002                          */
#*    Last change :  Thu Dec 18 09:21:41 2003 (serrano)                */
#*    Copyright   :  2002-03 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the Slides example                         */
#*=====================================================================*/
include ../../etc/Makefile.config
include ../../etc/$(SYSTEM)/Makefile.skb

#*---------------------------------------------------------------------*/
#*    Compiler and tools                                               */
#*---------------------------------------------------------------------*/
BINDIR		= ../../bin
LIBDIR		= ../../lib

#*---------------------------------------------------------------------*/
#*    Compilers and Tools                                              */
#*---------------------------------------------------------------------*/
SFLAGS		= -I txt -I skr -I skb -I ../../skr
LATEX		= latex
DVIPS		= dvips -Ppdf -G0
TEXHOME         = $$HOME/tex
PS2PDF		= ps2pdf -dPDFSETTINGS=/prepress -sPAPERSIZE=a4
MODE		= advi

#*---------------------------------------------------------------------*/
#*    Skribe variables                                                 */
#*---------------------------------------------------------------------*/
SKRIBEVARS	= --eval "(define *mode* '$(MODE))"

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
MASTER		= skb/slides.skb

INPUTSNAME	= 
EXNAME		= skribe.skb syntax.scr
INPUTS		= $(INPUTSNAME:%=skb/%.skb) $(EXNAME:%=ex/%)

SOURCESNAME	= 
SOURCES		= $(SOURCESNAME:%=scm/%.scm)

STYLES		= local
LSTYLES		= $(STYLE:%=skr/%.skr)

FIGS_SOURCES	= 
FIGURES		= $(FIGS_SOURCES:%=fig/%.eps) $(FIGS_SOURCES:%=fig/%.png)

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .skb .skr .eps .fig .tex .ps .pdf .png .html .dvi

#*---------------------------------------------------------------------*/
#*    All                                                              */
#*---------------------------------------------------------------------*/
all: ps html

ps: slides.ps
slides.ps: slides.dvi
	$(DVIPS) -o slides.ps slides.dvi

pdf: slides.pdf
slides.pdf: slides.ps
	$(PS2PDF) slides.ps slides.pdf

dvi: slides.dvi
slides.dvi: slides.tex 
	$(LATEX) slides.tex

slides.tex: $(MASTER) $(INPUTS) $(LSTYLES) $(SOURCES) $(FIGURES)
	$(SKRIBE) $(SKRIBEVARS) $(SFLAGS) $(MASTER) -o slides.tex

html: slides.html
slides.html: $(MASTER) $(INPUTS) $(LSTYLES) $(SOURCES) $(FIGURES)
	$(SKRIBE) $(SKRIBEVARS) $(SFLAGS) $(MASTER) -o slides.html

text: slides.text
slides.text: $(MASTER) $(INPUTS) $(LSTYLES) $(SOURCES) $(FIGURES)
	$(SKRIBE) $(SKRIBEVARS) $(SFLAGS) $(MASTER) -t text -o slides.text

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*---------------------------------------------------------------------*/
.PHONY: pop

pop:
	@ echo examples/slide/Makefile \
               examples/slide/README \
               examples/slide/advi.sty \
               examples/slide/PPRskribe.sty \
               examples/slide/skr/local.skr
	@ echo $(MASTER:%=examples/slide/%)
	@ echo $(EXNAME:%=examples/slide/ex/%)

#*---------------------------------------------------------------------*/
#*    binary                                                           */
#*---------------------------------------------------------------------*/
getbinary:
	echo "slides"

#*---------------------------------------------------------------------*/
#*    re                                                               */
#*---------------------------------------------------------------------*/
.PHONY: re re.ps re.html

re: re.ps re.html

re.ps:
	touch -m -d 0 slides.tex 
	$(MAKE) ps

re.html:
	touch -m -d 0 slides.html
	$(MAKE) html

#*---------------------------------------------------------------------*/
#*    .eps.png                                                         */
#*---------------------------------------------------------------------*/
.eps.png:
	@ echo $*.png:
	@ convert $*.eps $*.png

#*---------------------------------------------------------------------*/
#*    .eps.fig                                                         */
#*---------------------------------------------------------------------*/
.fig.eps:
	@ echo $*.fig:
	@ fig2dev -L eps $*.fig > $*.eps

#*---------------------------------------------------------------------*/
#*    Clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	-/bin/rm -f slides.tex 2> /dev/null
	-/bin/rm -f slides.dvi 2> /dev/null
	-/bin/rm -f *.aux *.log 2> /dev/null
	-/bin/rm -f *~ 2> /dev/null
	-/bin/rm -f */*~ 2> /dev/null
	-/bin/rm -f */*/*~ 2> /dev/null
	-/bin/rm -f slides.ps 2> /dev/null
	-/bin/rm -f slides.pdf 2> /dev/null
	-/bin/rm -f slides*.html 2> /dev/null
	-/bin/rm -f slides.text 2> /dev/null
	-/bin/rm -f slides.out 2> /dev/null
	-/bin/rm -f $(FIGURES)

cleanall: clean
