subdirs = src

all install uninstall clean realclean mostlyclean:
	@for f in $(subdirs); do \
		(cd $$f && $(MAKE) $@) \
	done

distclean:
	@for f in $(subdirs); do \
		(cd $$f && $(MAKE) $@) \
	done
	-rm -f Makefile
