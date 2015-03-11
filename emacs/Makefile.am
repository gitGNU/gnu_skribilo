EXTRA_DIST     = skribe.el.in
lisp_DATA      = skribe.el

CLEANFILES     = $(lisp_DATA)

include $(top_srcdir)/substitute.am

.in:
	$(AM_V_SUBSTITUTE)$(substitute) "$^" > "$@.tmp" &&	\
	mv "$@.tmp" "$@"
