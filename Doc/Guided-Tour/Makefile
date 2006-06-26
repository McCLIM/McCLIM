#!/usr/bin/make

guided-tour.dvi: guided-tour.tex hello-world.cut draw-frame.cut scheduler.cut file-browser.cut techno-dep.pstex_t techno-dep.pstex
	latex guided-tour.tex
	bibtex guided-tour
	latex guided-tour.tex
	latex guided-tour.tex

%.pstex: %.fig
	fig2dev -L pstex $(value $@) -b 0 $< $@

%.pstex_t: %.pstex %.fig
	fig2dev -L pstex_t $(value $@) -E 1 -p $^ $@

%.cut: %.lisp
	awk '/LTAG-end/   { found=found " " active; active="" }   \
	                  { if (active!="") print > active}      \
	     /LTAG-start/ { split($$2,foo,":"); active=foo[2] } \
	     END          { print found }' $<

.PHONY: clean

clean:
	rm guided-tour.aux guided-tour.bbl guided-tour.log guided-tour.dvi guided-tour.blg hello-world-def-app hello-world-defclass hello-world-handle-repaint scheduler-part1 scheduler-part2 techno-dep.pstex techno-dep.pstex_t file-browser-all draw-frame-interfacing draw-frame-def-app draw-frame-commands
