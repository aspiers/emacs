# TODO: rewrite in Perl.  Gah.

srcdirs=init/common major-modes minor-modes utils
loadpaths=$(HOME)/lib/emacs/minor-modes \
          $(HOME)/lib/emacs/major-modes/mmm \
          $(HOME)/lib/emacs/major-modes
gnuinstalldir=$(HOME)/lib/emacs/GNU_Emacs
xinstalldir=$(HOME)/lib/emacs/XEmacs

ELs=$(shell find $(srcdirs) -name \*.el ! -name psgml\* )
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
	@echo $(ELs)o

install: $(ELCs) psgml uninstall
	@echo "Installing in $(installdir):"
	@[ -d $(installdir) ] || mkdir -p $(installdir)
	@for elc in $(ELCs) major-modes/psgml/*.elc; do \
	  echo "    $$elc"; \
	  cp -i $$elc $(installdir); \
	done

%.elc: %.el
	@echo Compiling $< ...
	@$(COMPILER) $<

clean: uninstall
	@-rm -f $(ELCs)

uninstall:
	@-rm -f $(wildcard $(installdir)/*.elc)

psgml:
	cd major-modes/psgml && $(MAKE)
