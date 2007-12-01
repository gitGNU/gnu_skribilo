SUBDIRS = guile

EXTRA_DIST = skribilo.in pre-inst-skribilo.in

bin_SCRIPTS = skribilo
noinst_SCRIPTS = pre-inst-skribilo

CLEANFILES  = $(bin_SCRIPTS) $(noinst_SCRIPTS)

substitute = sed -e 's,[@]guilemoduledir[@],$(guilemoduledir),g'	\
		 -e 's,[@]abs_top_srcdir[@],$(abs_top_srcdir),g'	\
		 -e 's,[@]abs_top_builddir[@],$(abs_top_builddir),g'	\
		 -e 's,[@]GUILE[@],$(GUILE),g'				\
		 -e 's,[@]PACKAGE_STRING[@],$(PACKAGE_STRING),g'	\
		 -e 's,[@]PACKAGE_NAME[@],$(PACKAGE_NAME),g'		\
		 -e 's,[@]PACKAGE_VERSION[@],$(PACKAGE_VERSION),g'

SUFFIXES = .in

.in:
	$(substitute) "$^" > "$@.tmp" &&	\
	chmod +x "$@.tmp" &&			\
	mv "$@.tmp" "$@"
