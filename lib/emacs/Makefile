# TODO: rewrite in Perl.  Gah.

srcdirs=init/common major-modes minor-modes utils
loadpaths=$(HOME)/lib/emacs/minor-modes $(HOME)/lib/emacs/major-modes/mmm
gnuinstalldir=$(HOME)/lib/emacs/GNU_Emacs
xinstalldir=$(HOME)/lib/emacs/XEmacs

ELs=$(shell find $(srcdirs) -name \*.el)
ELCs=$(addsuffix c,$(ELs))

ifndef EMACS
  EMACS=emacs
endif

version=$(system $(EMACS) -version)

ifeq ($(EMACS),emacs)
  COMPILER=$(EMACS) $(addprefix -L ,$(loadpaths))
  installdir=$(gnuinstalldir)
else
  # blegh!
  LOADPATHS=$(addsuffix \" ,$(addprefix \",$(loadpaths)))
  COMPILER=$(EMACS) -eval "(setq load-path (append '($(LOADPATHS)) load-path))"
  installdir=$(xinstalldir)
endif

COMPILER+= -batch -f batch-byte-compile

.PHONY: all install

all: install

list:
	@echo $(ELs)

install: $(ELCs) uninstall
	@echo "Installing in $(installdir):"
	@[ -d $(installdir) ] || mkdir -p $(installdir)
	@for elc in $(ELCs); do \
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
