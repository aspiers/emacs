# TODO: rewrite in Perl.  Gah.

srcdirs=init/common major-modes minor-modes utils fun
EMACSLIB=$(HOME)/lib/emacs
loadpaths = $(EMACSLIB)/minor-modes 			\
            $(EMACSLIB)/major-modes/mmm 		\
            $(EMACSLIB)/major-modes/tdtd 		\
            $(EMACSLIB)/major-modes/pcl-cvs 		\
            $(EMACSLIB)/major-modes/monkey-2 		\
            $(EMACSLIB)/major-modes/nxml-mode-20041004	\
            $(EMACSLIB)/major-modes/xtla                \
            $(EMACSLIB)/major-modes/etask-0.3.91a       \
            $(EMACSLIB)/major-modes/emacs-w3m-1.4.4     \
            $(EMACSLIB)/major-modes/planner		\
            $(EMACSLIB)/major-modes/emacs-wiki	        \
            $(EMACSLIB)/major-modes/remember  	        \
            $(EMACSLIB)/major-modes			\
            $(EMACSLIB)/utils/tree-widget-2.0
gnuinstall_dir=$(EMACSLIB)/GNU_Emacs
xinstall_dir=$(EMACSLIB)/XEmacs

ELs = $(shell find $(srcdirs) -name \*.el 				\
			    ! -name psgml\* 				\
			    ! -name monkey\* 				\
			    ! -name tdtd\* 				\
			    ! -name sdf.el 				\
			    ! -name planner-bbdb.el 			\
			    ! -name planner-erc.el 			\
			    ! -name planner-gnats.el 			\
			    ! -name planner-ical.el 			\
			    ! -name planner-ledger.el 			\
			    ! -name planner-psvn.el 			\
			    ! -name planner-vm.el 			\
			    ! -name planner-wl.el 			\
			    ! -name planner-cyclic-test.el 		\
			    ! -name remember-bbdb.el 			\
			    ! -name remember-bibl.el 			\
			    ! -name remember-blosxom.el 		\
			    ! -name remember-emacs-wiki-journal.el 	\
			    ! -path '*/emacs-w3m-*/*'			\
			    ! -path '*/attic/*'			\
			    ! -path '*/shimbun/*'			\
     )

# Do a separate search for elcs since we want to include ones which were
# built from vendor Makefiles.
#ELCs = $(addsuffix c,$(ELs))
ELCs = $(shell find $(srcdirs) -name \*.elc)

ifndef EMACS
  EMACS=emacs
endif

version=$(shell $(EMACS) --version)
version_number=$(shell echo "$(version)" | \
                   perl -ne '/^\s*(GNU |X)Emacs (\d+\.\d+)/ && print "$$2"')
system_type=$(shell $(EMACS) -q --batch --eval '(message (format "%s" system-type))' 2>&1 | grep -v '^Loading ')

site_install_dir=$(shell $(EMACS) -q --batch --eval '(princ (car load-path))')

ifeq (, $(findstring XEmacs,$(version)))
  COMPILER=$(EMACS) $(addprefix -L ,$(loadpaths))
  install_dir=$(gnuinstall_dir)/$(version_number)/$(system_type)
else
  # blegh!
  LOADPATHS=$(addsuffix \" ,$(addprefix \",$(loadpaths)))
  COMPILER=$(EMACS) -eval "(setq load-path (append '($(LOADPATHS)) load-path))"
  install_dir=$(xinstall_dir)/$(version_number)/$(system_type)
endif

COMPILER+= -batch -f batch-byte-compile

.PHONY: all install

all: install

list:
	@for el in $(ELs); do echo $$el; done

install: nxml emacs-wiki $(ELCs) psgml uninstall
	@echo "Installing all .elcs in $(install_dir)"
	@[ -d $(install_dir) ] || mkdir -p $(install_dir)
	@for elc in $(ELCs) major-modes/psgml/*.elc; do \
	  cp $$elc $(install_dir); \
	done

%.elc: %.el
	@echo Compiling $< with $(EMACS) ...
	@$(COMPILER) $<

# Some shitty Solaris makes have broken $(wildcard foo/*.bar)
clean: uninstall
	@-find $(EMACSLIB) -name \*.elc -print | xargs rm -f

uninstall:
	-find $(install_dir) -name \*.elc -print | xargs rm -f

psgml:
	cd major-modes/psgml && $(MAKE)

nxml:
	cd major-modes/nxml-mode-20041004 && $(MAKE)

emacs-wiki:
	cd major-modes/emacs-wiki
	perl -pi 's/(EMACS\s*=\s*).*/$1$(EMACS)/' Makefile.defs
	$(MAKE)
