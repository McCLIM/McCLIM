IMAGES=figures/inspect-as-cells.pdf                                                             \
       figures/inspect-object-1.pdf figures/inspect-object-2.pdf figures/inspect-object-3.pdf

SOURCES=mcclim.texi user-manual.texi reference-manual.texi developer-manual.texi extensions.texi auxiliary-material.texi chap-debugger.texi chap-inspector.texi chap-listener.texi docstrings $(IMAGES)

INCLUDES=-I docstrings -I examples -I figures

RESULTS=mcclim.pdf mcclim.info mcclim mcclim.html

VPATH=./figures

.PHONY: all clean distclean

figures/%.pdf: figures/%.fig
	fig2dev -Lpdftex -m 0.75  $< $@

all: $(RESULTS)

mcclim.info: $(SOURCES)
	makeinfo $(INCLUDES) --output mcclim.info mcclim.texi

mcclim.pdf: $(SOURCES)
	texi2pdf $(INCLUDES) mcclim.texi

mcclim: $(SOURCES)
	makeinfo $(INCLUDES) --html --css-include=style-multi.css mcclim.texi

mcclim.html: $(SOURCES)
	makeinfo $(INCLUDES) --html --no-split --css-include=style-common.css --css-include=style-single.css mcclim.texi

clean:
	rm -rf mcclim.cp mcclim.cps mcclim.fn mcclim.fns mcclim.toc \
		mcclim.tp mcclim.tps mcclim.vr mcclim.vrs mcclim.aux mcclim.log

distclean: clean
	rm -rf $(RESULTS)
