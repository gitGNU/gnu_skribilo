SUBDIRS = po src doc emacs tests

ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST = TODO THANKS README.Skribe ChangeLog.Skribe

gen-ChangeLog:
	if ! test -d "$(top_srcdir)/.git"; then				\
	  echo "\`.git' not found; \`ChangeLog' not generated" >&2;	\
	elif ! git --version > /dev/null 2>&1; then			\
	  echo "git not found; \`ChangeLog' not generated" >&2;		\
	else								\
	  "$(top_srcdir)/build-aux/gitlog-to-changelog"			\
	     > "$(distdir)/cl-t";					\
	  rm -f "$(distdir)/ChangeLog";					\
	  mv "$(distdir)/cl-t" "$(distdir)/ChangeLog";			\
	fi

dist-hook: gen-ChangeLog

.PHONY: gen-ChangeLog
