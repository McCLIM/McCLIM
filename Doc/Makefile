NAME=manual

TEXFILES=$(NAME).tex $(shell ./tex-dependencies $(NAME).tex)
PSTEX_T=$(shell ./strip-dependence inputfig $(TEXFILES))
VERBATIM=$(shell ./strip-dependence verbatimtabinput $(TEXFILES))
PSTEX=$(subst .pstex_t,.pstex,$(PSTEX_T))

all : $(NAME).ps

%.pstex: %.fig
	fig2dev -Lpstex -m 0.75  $< $@

%.pstex_t: %.fig %.pstex
	fig2dev -Lpstex_t -m 0.75 -p $(basename $<).pstex $< $@

$(NAME).dvi: $(TEXFILES) $(PSTEX_T) $(VERBATIM)
	latex $<
#	makeindex $(NAME)
	latex $<

$(NAME).ps: $(NAME).dvi $(PSTEX)
	dvips $< -o

view: $(NAME).ps
	gv -antialias -scale 1 $<

clean:
	rm -f *.aux *.log *~

spotless:
	make clean
	rm -f *.ps *.dvi *.pstex *.pstex_t *.toc *.idx *.ilg *.ind

