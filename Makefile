.PHONY: all clean install uninstall reinstall

all clean install uninstall reinstall:
	$(MAKE) -C src $@
