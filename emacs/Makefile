#*=====================================================================*/
#*    serrano/prgm/project/skribe/emacs/Makefile                       */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Oct 25 08:20:06 2003                          */
#*    Last change :  Thu Jan  1 16:46:32 2004 (serrano)                */
#*    Copyright   :  2003-04 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    Skribe emacs Makefile                                            */
#*=====================================================================*/
include ../etc/Makefile.config
include ../etc/$(SYSTEM)/Makefile.skb

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*---------------------------------------------------------------------*/
.PHONY: pop

pop: 
	@ echo emacs/skribe.el.in emacs/Makefile

#*---------------------------------------------------------------------*/
#*    install/uninstall                                                */
#*---------------------------------------------------------------------*/
.PHONY: install uninstall

install:
	$(MAKE) install-$(SYSTEM)
uninstall:
	$(MAKE) uninstall-$(SYSTEM)

install-bigloo:
	if [ "$(EMACSDIR) " != " " ]; then \
          if [ -d $(EMACSDIR) ]; then \
	    cp skribe.el $(EMACSDIR) && chmod $(BMASK) $(EMACSDIR)/skribe.el; \
          fi \
        fi
uninstall-bigloo:
	if [ "$(EMACSDIR) " != " " ]; then \
          if [ -d $(EMACSDIR) ]; then \
	    $(RM) -f $(EMACSDIR)/skribe.el; \
          fi \
        fi

install-stklos:
uninstall-stklos:

#*---------------------------------------------------------------------*/
#*    clean/distclean                                                  */
#*---------------------------------------------------------------------*/
.PHONY: clean distclean

clean:
distclean: clean
	$(RM) -f skribe.el
