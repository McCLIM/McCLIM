
SOURCES=mcclim.texi docstrings

.PHONY: all clean distclean

all: mcclim.pdf mcclim.info mcclim.html
mcclim.info: $(SOURCES)
	makeinfo mcclim.texi -I docstrings --output mcclim.info
mcclim.pdf: $(SOURCES)
	texi2pdf mcclim.texi -I docstrings
mcclim.html: $(SOURCES)
	makeinfo mcclim.texi -I docstrings --html
clean:
	rm -rf mcclim.cp mcclim.cps mcclim.fn mcclim.fns mcclim.toc \
		mcclim.tp mcclim.tps mcclim.vr mcclim.vrs mcclim.aux mcclim.log
distclean: clean
	rm -rf mcclim.info mcclim.pdf mcclim.html
