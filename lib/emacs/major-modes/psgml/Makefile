# Generated automatically from Makefile.in by configure.
# Makefile.in 
#
# Makefile for the GNU Emacs lisp library, PSGML

lispdir = $(prefix)/share/emacs/site-lisp

ifndef EMACS
  EMACS=emacs
endif

FLAGS = -batch -q -no-site-file -l ./psgml-maint.el
INSTALL = /usr/bin/install -c
INSTALL_DATA = ${INSTALL} -m 644
SHELL = /bin/sh

compile:
	$(EMACS) $(FLAGS) -f psgml-compile-files

all: 
	rm -f *.elc ; $(EMACS) $(FLAGS) -f psgml-compile-files


install: compile 
	$(SHELL) mkinstalldirs $(lispdir)
	for p in *.elc; do \
	  echo " $(INSTALL_DATA) $$p $(lispdir)/$$p"; \
	  $(INSTALL_DATA) $$p $(lispdir)/$$p; \
	done
	$(INSTALL_DATA) iso88591.map $(lispdir)

