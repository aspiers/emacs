# TODO: rewrite in Perl.  Gah.

srcdirs=init/common major-modes minor-modes utils
EMACSLIB=$(HOME)/lib/emacs
loadpaths=$(EMACSLIB)/minor-modes \
          $(EMACSLIB)/major-modes/mmm \
          $(EMACSLIB)/major-modes/tdtd \
          $(EMACSLIB)/major-modes
gnuinstalldir=$(EMACSLIB)/GNU_Emacs
xinstalldir=$(EMACSLIB)/XEmacs

ELs=$(shell find $(srcdirs) -name \*.el \
			    ! -name psgml\* \
			    ! -name tdtd\* \
			    ! -name sdf.el \
     )
ELCs=$(addsuffix c,$(ELs))

ifndef EMACS
  EMACS=emacs
endif

version=$(shell $(EMACS) --version)
version_number=$(shell echo "$(version)" | \
            perl -ne '/^\s*(GNU |X)Emacs (\d+\.\d+)/ && print "$$2"')

ifeq (, $(findstring XEmacs,$(version)))
  COMPILER=$(EMACS) $(addprefix -L ,$(loadpaths))
  installdir=$(gnuinstalldir)/$(version_number)
else
  # blegh!
  LOADPATHS=$(addsuffix \" ,$(addprefix \",$(loadpaths)))
  COMPILER=$(EMACS) -eval "(setq load-path (append '($(LOADPATHS)) load-path))"
  installdir=$(xinstalldir)/$(version_number)
endif

COMPILER+= -batch -f batch-byte-compile

.PHONY: all install

all: install

list:
	@for el in $(ELs); do echo $$el; done

install: $(ELCs) psgml uninstall
	@echo "Installing all .elcs in $(installdir)"
	@[ -d $(installdir) ] || mkdir -p $(installdir)
	@for elc in $(ELCs) major-modes/psgml/*.elc; do \
	  cp -i $$elc $(installdir); \
	done

%.elc: %.el
	@echo Compiling $< ...
	@$(COMPILER) $<

clean: uninstall
	@-find $(EMACSLIB) -name \*.elc | xargs rm -f

uninstall:
	@-rm -f $(wildcard $(installdir)/*.elc)

psgml:
	cd major-modes/psgml && $(MAKE)
