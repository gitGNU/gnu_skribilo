#*=====================================================================*/
#*    serrano/prgm/project/skribe/tools/Makefile                       */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Jul 30 16:23:07 2003                          */
#*    Last change :  Tue Oct 26 19:36:26 2004 (eg)                */
#*    Copyright   :  2003-04 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Skribe Tools general makefile                                */
#*=====================================================================*/
include ../etc/Makefile.config

TOOLS= skribebibtex

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
.PHONY: all

all:
	for p in $(TOOLS); do \
	  (cd $$p/$(SYSTEM) && $(MAKE)) || exit -1; \
        done

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*---------------------------------------------------------------------*/
.PHONY: pop

pop: 
	@ for p in $(TOOLS); do \
	  (cd $$p/bigloo && $(MAKE) pop); \
	  (cd $$p/stklos && $(MAKE) pop); \
        done
	@ echo tools/Makefile

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*---------------------------------------------------------------------*/
.PHONY: install uninstall

install: 
	@ for p in $(TOOLS); do \
	  (cd $$p/$(SYSTEM) && $(MAKE) install) || exit -1; \
        done
uninstall: 
	@ for p in $(TOOLS); do \
	  (cd $$p/$(SYSTEM) && $(MAKE) uninstall) || exit -1; \
        done

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
.PHONY: clean

clean:
	for p in $(TOOLS); do \
	  (cd $$p/$(SYSTEM) && $(MAKE) clean); \
        done

