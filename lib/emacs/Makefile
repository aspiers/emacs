srcdirs=init/common major-modes minor-modes utils
loadpaths=$(HOME)/lib/emacs/major-modes/mmm
gnuinstalldir=$(HOME)/lib/emacs/GNU_Emacs
xinstalldir=$(HOME)/lib/emacs/XEmacs
GNUEMACS=emacs
XEMACS=xemacs

ELs=$(shell find $(srcdirs) -name \*.el)
ELCs=$(addsuffix c,$(ELs))

ifdef X
  EMACS=$(XEMACS)
  installdir=$(xinstalldir)
else
  EMACS=$(GNUEMACS)
  installdir=$(gnuinstalldir)
endif

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

LOADPATHS=$(addprefix -L ,$(loadpaths))

%.elc: %.el
	@echo Compiling $< ...
	@$(EMACS) $(LOADPATHS) -batch -f batch-byte-compile $<

clean: uninstall
	@-rm -f $(ELCs)

uninstall:
	@-rm -f $(wildcard $(installdir)/*.elc)
