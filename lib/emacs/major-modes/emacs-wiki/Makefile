.PHONY: all lisp doc clean realclean distclean fullclean install test dist release debclean debrelease
.PRECIOUS: %.elc

include Makefile.defs

EL  = $(wildcard *.el)
ELC = $(patsubst %.el,%.elc,$(wildcard *.el))

all: lisp emacs-wiki.info

lisp: $(ELC)

emacs-wiki-build.elc: ./scripts/emacs-wiki-build.el
	@echo emacs-wiki-build.el is not byte-compiled

%.elc: %.el
	@$(EMACS) -q $(SITEFLAG) -batch -l ./scripts/emacs-wiki-build.el \
		-f batch-byte-compile $<

%.info: %.texi
	makeinfo $<

%.html: %.texi
	makeinfo --html --no-split $<

doc: emacs-wiki.info emacs-wiki.html

clean:
	-rm -f *.elc *~

realclean fullclean: clean
	-rm -f emacs-wiki.info emacs-wiki.html

install: $(ELC) emacs-wiki.info
	install -d $(ELISPDIR)/contrib
	install -m 0644 $(EL) $(ELC) $(ELISPDIR)
	install -m 0644 $(wildcard contrib/*.el) $(ELISPDIR)/contrib
	install -d $(INFODIR)
	install -m 0644 emacs-wiki.info $(INFODIR)/emacs-wiki
	$(INSTALLINFO) $(INFODIR)/emacs-wiki

test: $(ELC)
	$(EMACS) -q $(SITEFLAG) -batch -l ./scripts/emacs-wiki-build.el \
		-f emacs-wiki-elint-files emacs-wiki-*.el

distclean: realclean
	-rm -f debian/dirs debian/files
	test -d ../emacs-wiki-$(VERSION) && rm -r ../emacs-wiki-$(VERSION) || :

dist: distclean
	tla inventory -sB | tar -cf - --no-recursion -T- | \
	  (mkdir -p ../emacs-wiki-$(VERSION); cd ../emacs-wiki-$(VERSION) && \
	  tar xf -)
	rm -fr ../emacs-wiki-$(VERSION)/debian ../emacs-wiki-$(VERSION)/test

release: dist
	(cd .. && tar -czf emacs-wiki-$(VERSION).tar.gz \
	          emacs-wiki-$(VERSION) ; \
	  zip -r emacs-wiki-$(VERSION).zip emacs-wiki-$(VERSION))

debclean:
	(cd ../../dist && rm -f emacs-wiki_*)

debrelease: dist debclean
	(cd .. && tar -czf emacs-wiki_$(VERSION).orig.tar.gz \
	          emacs-wiki-$(VERSION))
	cp -r debian ../emacs-wiki-$(VERSION)
	rm -fr ../emacs-wiki-$(VERSION)/debian/.arch-ids
	(cd ../emacs-wiki-$(VERSION) && debuild -rfakeroot)
	cp ../emacs-wiki_$(VERSION)* ../../dist

upload:
	(cd .. && gpg --detach emacs-wiki-$(VERSION).tar.gz && \
	  gpg --detach emacs-wiki-$(VERSION).zip && \
	  echo open ftp://savannah.nongnu.org > upload.lftp ; \
	  echo cd /incoming/savannah/emacs-wiki >> upload.lftp ; \
	  echo mput emacs-wiki-$(VERSION).zip* >> upload.lftp ; \
	  echo mput emacs-wiki-$(VERSION).tar.gz* >> upload.lftp ; \
	  echo close >> upload.lftp ; \
	  lftp -f upload.lftp ; \
	  rm -f upload.lftp)
