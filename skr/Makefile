#*=====================================================================*/
#*    serrano/prgm/project/skribe/skr/Makefile                         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Oct 25 08:21:20 2003                          */
#*    Last change :  Wed May 18 15:34:21 2005 (serrano)                */
#*    Copyright   :  2003-05 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Skribe SKR Makefile                                          */
#*=====================================================================*/
include ../etc/Makefile.config
include ../etc/$(SYSTEM)/Makefile.skb

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
POPULATION= acmproc.skr sigplan.skr jfp.skr \
            slide.skr web-book.skr web-article.skr \
	    base.skr latex.skr scribe.skr xml.skr \
	    html.skr html4.skr lncs.skr skribe.skr \
	    letter.skr french.skr latex-simple.skr context.skr Makefile

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*---------------------------------------------------------------------*/
.PHONY: pop

pop:
	@ echo $(POPULATION:%=skr/%)

#*---------------------------------------------------------------------*/
#*    Install/Uinstall                                                 */
#*---------------------------------------------------------------------*/
.PHONY: install uninstall

install: $(DESTDIR)$(INSTALL_SKRDIR)
	cp *.skr $(DESTDIR)$(INSTALL_SKRDIR) && chmod $(BMASK) $(DESTDIR)$(INSTALL_SKRDIR)/*

uninstall:

$(DESTDIR)$(INSTALL_SKRDIR):
	mkdir -p $(DESTDIR)$(INSTALL_SKRDIR) && chmod a+rx $(DESTDIR)$(INSTALL_SKRDIR)

