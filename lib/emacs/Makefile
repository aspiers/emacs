ELs=$(shell find . -name \*.el)
ELCs=$(addsuffix c,$(ELs))
installdir=$(HOME)/lib/emacs/GNU_Emacs

.PHONY: all install

all: install

%.elc: %.el
	@echo Compiling $< ...
	@emacs -batch -f batch-byte-compile $<

install: $(ELCs)
	@[ -d $(installdir) ] || mkdir -p $(installdir)
	@for elc in $(ELCs); do \
	  echo "Installing $$elc ..." \
	  cp $$elc $(installdir) \
	done

clean: 
	-rm -i $(ELCs)
