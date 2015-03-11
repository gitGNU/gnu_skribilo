SUBDIRS = guile

EXTRA_DIST = skribilo.in pre-inst-skribilo.in skribilo-config.in

bin_SCRIPTS = skribilo skribilo-config
noinst_SCRIPTS = pre-inst-skribilo

CLEANFILES  = $(bin_SCRIPTS) $(noinst_SCRIPTS)

include $(top_srcdir)/substitute.am

.in:
	$(AM_V_SUBSTITUTE)$(substitute) "$^" > "$@.tmp" &&	\
	chmod +x "$@.tmp" &&			\
	mv "$@.tmp" "$@"
