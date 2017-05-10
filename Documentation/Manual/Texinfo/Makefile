
SOURCES=mcclim.texi user-manual.txi reference-manual.txi developer-manual.txi extensions.txi auxiliary-material.txi chap-debugger.texi chap-inspector.texi chap-listener.texi docstrings

.PHONY: all clean distclean

all: mcclim.pdf mcclim.info mcclim
mcclim.info: $(SOURCES)
	makeinfo mcclim.texi -I docstrings --output mcclim.info
mcclim.pdf: $(SOURCES)
	texi2pdf mcclim.texi -I docstrings
mcclim: $(SOURCES)
	makeinfo mcclim.texi -I docstrings --html
clean:
	rm -rf mcclim.cp mcclim.cps mcclim.fn mcclim.fns mcclim.toc \
		mcclim.tp mcclim.tps mcclim.vr mcclim.vrs mcclim.aux mcclim.log
distclean: clean
	rm -rf mcclim.info mcclim.pdf mcclim
