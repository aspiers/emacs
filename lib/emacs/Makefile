srcdirs=init/common major-modes minor-modes utils
loadpaths=$(HOME)/lib/emacs/major-modes/mmm
installdir=$(HOME)/lib/emacs/GNU_Emacs
EMACS=emacs

ELs=$(shell find $(srcdirs) -name \*.el)
ELCs=$(addsuffix c,$(ELs))

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
