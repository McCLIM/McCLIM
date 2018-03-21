
IMAGES=ex2.pdf inspect-as-cells.pdf inspect-object-1.pdf inspect-object-2.pdf inspect-object-3.pdf

SOURCES=mcclim.texi user-manual.txi reference-manual.txi developer-manual.txi extensions.txi auxiliary-material.txi chap-debugger.texi chap-inspector.texi chap-listener.texi docstrings $(IMAGES)

INCLUDES=-I docstrings -I examples -I figures

VPATH=./figures

.PHONY: all clean distclean

%.pdf: figures/%.fig
	fig2dev -Lpdftex -m 0.75  $< $@

all: mcclim.pdf mcclim.info mcclim
mcclim.info: $(SOURCES)
	makeinfo mcclim.texi $(INCLUDES) --output mcclim.info
mcclim.pdf: $(SOURCES)
	texi2pdf mcclim.texi $(INCLUDES)
mcclim: $(SOURCES)
	makeinfo mcclim.texi $(INCLUDES) --html
clean:
	rm -rf mcclim.cp mcclim.cps mcclim.fn mcclim.fns mcclim.toc \
		mcclim.tp mcclim.tps mcclim.vr mcclim.vrs mcclim.aux mcclim.log
distclean: clean
	rm -rf mcclim.info mcclim.pdf mcclim
