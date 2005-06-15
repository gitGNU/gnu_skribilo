#*=====================================================================*/
#*    serrano/prgm/project/skribe/doc/Makefile                         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Sep  1 10:29:28 2003                          */
#*    Last change :  Wed Mar 10 11:16:48 2004 (serrano)                */
#*    Copyright   :  2003-04 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the Skribe documentation.                  */
#*=====================================================================*/
include ../etc/Makefile.config
include ../etc/$(SYSTEM)/Makefile.skb

#*---------------------------------------------------------------------*/
#*    Compiler and tools                                               */
#*---------------------------------------------------------------------*/
BINDIR		= ../bin
LIBDIR		= ../lib
LATEX		= latex
DVIPS		= dvips

SKRIBEVERBOSE	= -v1
SKRIBEWARNING	= -w1
SFLAGS		= $(SKRIBEVERBOSE) $(SKRIBEWARNING) \
                  -I ../skr \
                  -I skr \
                  -P img \
                  -S .. \
                  --custom emit-sui=yes \
                  --eval '(define *skribe-bin* "$(SKRIBE)")' \
                  --eval '(define *skribebibtex-bin* "$(SKRIBEBIBTEX)")'

#*---------------------------------------------------------------------*/
#*    Doc skr                                                          */
#*---------------------------------------------------------------------*/
_SKR		= manual.skr env.skr api.skr extension.skr
SKR		= $(_SKR:%=skr/%)

#*---------------------------------------------------------------------*/
#*    Images                                                           */
#*---------------------------------------------------------------------*/
_IMG		= bsd.gif lambda.gif linux.gif
IMG		= $(_IMG:%=img/%) 

#*---------------------------------------------------------------------*/
#*    User document                                                    */
#*---------------------------------------------------------------------*/
_USERMAIN	= user.skb
_USEROTHERS	= start.skb syntax.skb \
                  markup.skb document.skb \
                  sectioning.skb toc.skb ornament.skb line.skb font.skb \
		  justify.skb enumeration.skb \
                  examples.skb colframe.skb figure.skb image.skb table.skb \
                  footnote.skb char.skb \
                  links.skb index.skb bib.skb prgm.skb \
                  engine.skb htmle.skb latexe.skb xmle.skb \
                  emacs.skb skribec.skb skribe-config.skb \
                  lib.skb slide.skb package.skb
_USERSRC	= start1.skb start2.skb start3.skb start4.skb  start5.skb \
                  api1.skb api2.skb api3.skb api4.skb api5.skb \
                  api6.skb api7.skb api8.skb api9.skb api10.skb \
                  api11.skb api12.skb api13.skb api14.skb api15.skb \
                  api16.skb api17.skb api18.skb api19.skb api20.skb \
                  links1.skb links2.skb \
                  index1.skb index2.skb index3.skb \
                  bib1.sbib bib2.skb bib3.skb bib4.skb bib5.skb bib6.skb \
                  prgm1.skb prgm2.skb prgm3.skb slides.skb

USERMAIN	= $(_USERMAIN:%=user/%) 
USEROTHERS	= $(_USEROTHERS:%=user/%) 
USERSRC		= $(_USERSRC:%=user/src/%) 
USERSKB		= $(USERMAIN) $(USEROTHERS) $(USERSRC)

#*---------------------------------------------------------------------*/
#*    User document                                                    */
#*---------------------------------------------------------------------*/
_DIRMAIN	= dir.skb
_DIROTHERS	= 
_DIRSRC		= 

DIRMAIN		= $(_DIRMAIN:%=dir/%) 
DIROTHERS	= $(_DIROTHERS:%=dir/%) 
DIRSRC		= $(_DIRSRC:%=dir/src/%) 
DIRSKB		= $(DIRMAIN) $(DIROTHERS) $(DIRSRC)

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .skb .man .html .sui

#*---------------------------------------------------------------------*/
#*    All                                                              */
#*---------------------------------------------------------------------*/
.PHONY: user dir

all: user dir
re: re.html re.dir

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*---------------------------------------------------------------------*/
.PHONY: pop

pop:
	@ echo doc/Makefile doc/Makefile.dir
	@ echo $(USERSKB:%=doc/%)
	@ echo $(DIRSKB:%=doc/%)
	@ echo $(SKR:%=doc/%)
	@ echo $(IMG:%=doc/%)

#*---------------------------------------------------------------------*/
#*    user                                                             */
#*---------------------------------------------------------------------*/
.PHONY: user re.html user.html

user: user.html user.sui
user.html: html/user.html html/img/lambda.gif html/img/bsd.gif html/img/linux.gif
user.sui: html/user.sui

user.ps: tex/user.dvi
	(cd tex; $(DVIPS) user.dvi -o user.ps)

user.dvi: tex/user.dvi
tex/user.dvi: tex/user.tex
	(cd tex; $(LATEX) user.tex)

html/user.html html/user.sui: html $(USERSKB) $(SKR) 
	$(MAKE) re.html

tex/user.tex: tex $(USERSKB) $(SKR) tex/img/lambda.eps tex/img/bsd.eps tex/img/linux.eps
	$(MAKE) re.tex

# gif
html/img/lambda.gif: html/img img/lambda.gif
	cp img/lambda.gif html/img/lambda.gif

html/img/linux.gif: html/img img/linux.gif
	cp img/linux.gif html/img/linux.gif

html/img/bsd.gif: html/img img/bsd.gif
	cp img/bsd.gif html/img/bsd.gif

# eps image
tex/img/lambda.eps: tex/img img/lambda.gif
	convert img/lambda.gif tex/img/lambda.eps

tex/img/linux.eps: tex/img img/linux.gif
	convert img/linux.gif tex/img/linux.eps

tex/img/bsd.eps: tex/img img/bsd.gif
	convert img/bsd.gif tex/img/bsd.eps

re.html: 
	$(SKRIBE) $(SFLAGS) $(USERMAIN) \
                  --base html -I user -S user \
                  -o html/user.html

re.tex: 
	$(SKRIBE) $(SFLAGS) $(USERMAIN) \
                  --base tex -I user -S user \
                  -o tex/user.tex

#*---------------------------------------------------------------------*/
#*    dir                                                              */
#*---------------------------------------------------------------------*/
.PHONY: dir re.dir dir.html

dir: dir.html
dir.html: html/dir.html

html/dir.html: html $(DIRSKB) $(SKR) 
	$(MAKE) re.dir

re.dir: 
	$(MAKE) -f Makefile.dir SKRIBE="$(SKRIBE)" BASE=html

#*---------------------------------------------------------------------*/
#*    Misc                                                             */
#*---------------------------------------------------------------------*/
html:
	mkdir -p html

html/img:
	mkdir -p html/img

tex:
	mkdir -p tex

tex/img:
	mkdir -p tex/img

gethtml:
	@ echo "html/user.html"

#*---------------------------------------------------------------------*/
#*    install/uinstall                                                 */
#*---------------------------------------------------------------------*/
.PHONY: install uninstall

install: $(DESTDIR)$(INSTALL_DOCDIR) $(DESTDIR)$(INSTALL_SKRDIR)/doc/skr
	cp -r html/* $(DESTDIR)$(INSTALL_DOCDIR) \
            && chmod $(BMASK) $(DESTDIR)$(INSTALL_DOCDIR)/* \
            && chmod a+rx $(DESTDIR)$(INSTALL_DOCDIR)/img
	cp -r skr/* $(DESTDIR)$(INSTALL_SKRDIR)/doc/skr \
            && chmod a+rx $(DESTDIR)$(INSTALL_SKRDIR)/doc \
            && chmod a+rx $(DESTDIR)$(INSTALL_SKRDIR)/doc/skr \
            && chmod $(BMASK) $(DESTDIR)$(INSTALL_SKRDIR)/doc/skr/*
	cp Makefile.dir $(DESTDIR)$(INSTALL_DOCDIR) \
            && chmod $(BMASK) $(DESTDIR)$(INSTALL_DOCDIR)/Makefile.dir
	cp dir/dir.skb $(DESTDIR)$(INSTALL_DOCDIR) \
            && chmod $(BMASK) $(DESTDIR)$(INSTALL_DOCDIR)/dir.skb

uninstall:
	$(RM) -rf $(DESTDIR)$(INSTALL_DOCDIR)

$(DESTDIR)$(INSTALL_DOCDIR):
	mkdir -p $(DESTDIR)$(INSTALL_DOCDIR) && chmod a+rx $(DESTDIR)$(INSTALL_DOCDIR)


$(DESTDIR)$(INSTALL_SKRDIR)/doc/skr:
	mkdir -p $(DESTDIR)$(INSTALL_SKRDIR)/doc/skr \
            && chmod -R a+rx $(DESTDIR)$(INSTALL_SKRDIR)/doc

#*---------------------------------------------------------------------*/
#*    Clean                                                            */
#*---------------------------------------------------------------------*/
.PHONY: clean

clean:
	$(RM) -rf html
	$(RM) -rf tex
	$(RM) -f  img/bsd.eps img/linux.eps 
