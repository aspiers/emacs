# TODO: rewrite in Perl.  Gah.

srcdirs = init/common major-modes minor-modes utils fun

# These ones have their own Makefiles
THIRD_PARTY_PKGS = nxml-mode-20041004 emacs-wiki psgml emacs-w3m
THIRD_PARTY_PKG_DIRS = $(addprefix major-modes/, \
                   nxml-mode-20041004 emacs-wiki psgml emacs-w3m-1.4.4 xtla)
IGNORE_DIRS = major-modes/xtla-main

# Phony targets are bad except for top-level rules because if a rule A
# has a phony prerequisite B, that phony rule B will always get
# executed, even if B's prerequisites don't need to be, hence the
# parent rule will too.
THIRD_PARTY_TARGETS = $(addprefix .,$(THIRD_PARTY_PKGS))
THIRD_PARTY_TARGETS := $(addsuffix -made,$(THIRD_PARTY_TARGETS))

EMACSLIB = $(HOME)/lib/emacs

# Add these loadpaths during compilation only.
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
            $(EMACSLIB)/major-modes/pgg  	        \
            $(EMACSLIB)/major-modes			\
            $(EMACSLIB)/utils/tree-widget-2.0

gnuinstall_dir = $(EMACSLIB)/GNU_Emacs
xinstall_dir = $(EMACSLIB)/XEmacs

# Horrible horrible horrible.  Prune 3rd party packages from imminent find(1).
prune = $(shell echo $(THIRD_PARTY_PKG_DIRS) $(IGNORE_DIRS) | perl -040lpe 's|(.+)|! -path "$$1/*" |g')

ELs_TO_COMPILE_cmd =   				        \
	find $(srcdirs)					\
	        -follow					\
		-name \*.el 				\
	      $(prune)					\
	      ! -name monkey\* 				\
	      ! -name tdtd\* 				\
	      ! -name sdf.el 				\
	      ! -name planner-bbdb.el 			\
	      ! -name planner-erc.el 			\
	      ! -name planner-gnats.el 			\
	      ! -name planner-ical.el 			\
	      ! -name planner-ledger.el 		\
	      ! -name planner-psvn.el 			\
	      ! -name planner-vm.el 			\
	      ! -name planner-wl.el 			\
	      ! -name planner-cyclic-test.el 		\
	      ! -name remember-bbdb.el 			\
	      ! -name remember-bibl.el 			\
	      ! -name remember-blosxom.el 		\
	      ! -name remember-emacs-wiki-journal.el 	\
	      ! -path '*/attic/*'			\
	      ! -path '*/shimbun/*'
ELs_TO_COMPILE = $(shell $(ELs_TO_COMPILE_cmd)) 

# These are the .elcs we want to build ourselves
ELCs_TO_COMPILE = $(addsuffix c,$(ELs_TO_COMPILE))

# Do a separate search for elcs since we want to include ones which were
# built from vendor Makefiles.
THIRD_PARTY_ELCs = $(shell \
	find $(THIRD_PARTY_PKG_DIRS) -name \*.elc \
	  ! -path 'major-modes/emacs-w3m-1.4.4/*' \
)

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

.PHONY: all elcs install clean uninstall

all: elcs install

list-loadpaths:
	@echo "$(loadpaths)"

list-prune:
	@echo "$(prune)"

list-els-to-compile-cmd:
	@echo "$(ELs_TO_COMPILE_cmd)"

list-els-to-compile:
	@for el in $(ELs_TO_COMPILE); do echo $$el; done

list-third-party-elcs:
	@for elc in $(THIRD_PARTY_ELCs); do echo $$elc; done

elcs: $(ELCs_TO_COMPILE) $(THIRD_PARTY_TARGETS) Makefile

install: .installed

.installed: Makefile #uninstall
	@echo "Installing all .elcs in $(install_dir)"
	@[ -d $(install_dir) ] || mkdir -p $(install_dir)
	@for elc in $(ELCs_TO_COMPILE) $(THIRD_PARTY_ELCs); do \
	  cp $$elc $(install_dir); \
	done
	@touch $@

%.elc: %.el
	@echo Compiling $< with $(EMACS) ...
	@$(COMPILER) $<

# Some shitty Solaris makes have broken $(wildcard foo/*.bar)
clean: uninstall
	@-find $(EMACSLIB) -name \*.elc -print | xargs rm -f
	rm major-modes/nxml-mode-20041004/stamp-byte-compile

uninstall:
	-find $(install_dir) -name \*.elc -print | xargs rm -f

.psgml-made:
	( cd major-modes/psgml && $(MAKE) ) && \
	touch $@

.nxml-mode-20041004-made:
	( \
	  cd major-modes/nxml-mode-20041004 && \
	  $(MAKE) && \
	  cp rng-auto.el $(install_dir) \
	) && \
	touch $@

.emacs-wiki-made: $(wildcard major-modes/emacs-wiki/*.el)
	( \
	  cd major-modes/emacs-wiki && \
	  perl -pi -e 's/^(EMACS\s*=\s*).*/$$1$(EMACS)/; \
                       s!^(PREFIX\s*=\s*).*!$${1}$(HOME)/local!' \
	  	Makefile.defs && \
	  make && \
	  make doc && \
	  make install \
	) && \
	touch $@

.emacs-w3m-made:
	( \
	  cd major-modes/emacs-w3m-1.4.4     && \
	  ./configure --prefix=$(HOME)/local && \
	  make				     && \
	  make install			     && \
	  cp w3m-load.el $(install_dir) \
	) && \
	touch $@

.pgg-made: major-modes/pgg/pgg.elc
	@touch $@
