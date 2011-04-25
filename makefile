# Installation of public stuff.
#
# [created.  -- rgr, 9-Jan-99.]
#
# $Id$

install-dir = /usr/local/share/emacs/site-lisp

# this needs to track the contents of the rgr-hacks-public-files emacs-lisp
# variable.  -- rgr, 13-Feb-99.
public-source = matlab-hacks.el bmerc-hacks.el \
		rgr-html-hacks.el rgr-html-head.el rgr-html-nest.el
public-bins = ${public-source:%.el=%.elc}
# Install source, autoloads, and compiled files.
install-files = ${public-source} ${public-bins}

all:		elc-files
elc-files:	rgr-hacks-autoloads.el
	emacs --batch --load rgr-hacks-autoloads.el \
		--load compile-rgr-hacks.el -f rgr-hacks-compile-self

rgr-hacks-autoloads.el:		.
	@if [ ! -r $@ ]; then \
	    echo Creating new $@ file.; \
	    echo ';;; Autoloads for the rgr-hacks library.' > $@; \
	    echo >> $@; \
	    echo '' >> $@; \
	fi
	emacs --batch --load rgr-random-hacks.el \
		-f rgr-batch-update-autoloads $^ rgr-hacks-autoloads.el

clean:
	rm -f *.elc

install:
	install.pl -show -m 444 ${install-files} ${install-dir}
	cd ${install-dir}; emacs -batch -f batch-update-autoloads .

tags:
	etags *.el
