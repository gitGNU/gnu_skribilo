#*=====================================================================*/
#*    serrano/prgm/project/skribe/etc/Makefile                         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Oct 25 08:29:30 2003                          */
#*    Last change :  Sat Jan  3 06:40:19 2004 (serrano)                */
#*    Copyright   :  2003-04 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Skribe Meta etc Makefile                                     */
#*=====================================================================*/
include ../etc/Makefile.config
include ../etc/$(SYSTEM)/Makefile.skb

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*---------------------------------------------------------------------*/
.PHONY: pop

pop:
	@ echo etc/Makefile etc/skribe-config.in etc/ChangeLog
	@ (cd bigloo && $(MAKE) pop)
	@ (cd stklos && $(MAKE) pop)

#*---------------------------------------------------------------------*/
#*    Install/Uninstall                                                */
#*---------------------------------------------------------------------*/
.PHONY: install uninstall

install: $(DESTDIR)$(INSTALL_EXTDIR)
	cp skribe-config $(DESTDIR)$(INSTALL_BINDIR) && \
        chmod $(BMASK) $(DESTDIR)$(INSTALL_BINDIR)/skribe-config

uninstall:
	$(RM) -f $(DESTDIR)$(INSTALL_BINDIR)/skribe-config

$(DESTDIR)$(INSTALL_EXTDIR):
	mkdir -p $(DESTDIR)$(INSTALL_EXTDIR) && chmod a+rx $(DESTDIR)$(INSTALL_EXTDIR)


#*---------------------------------------------------------------------*/
#*    clean/distclean                                                  */
#*---------------------------------------------------------------------*/
.PHONY: clean distclean

clean:
	(cd $(SYSTEM) && $(MAKE) clean)

distclean: clean
	(cd $(SYSTEM) && $(MAKE) distclean)
	$(RM) -f skribe-config 	config
