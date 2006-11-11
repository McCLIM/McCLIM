NAME=mcclim
NAME2=sheet-hierarchy

TEXFILES=$(NAME2).tex $(shell ./tex-dependencies $(NAME2).tex)
PSTEX_T=$(shell ./strip-dependence inputfig $(TEXFILES))
VERBATIM=$(shell ./strip-dependence verbatimtabinput $(TEXFILES))
PSTEX=$(subst .pstex_t,.pstex,$(PSTEX_T))
IMAGES=ex2.eps inspect-as-cells.eps inspect-object-1.eps \
inspect-object-2.eps inspect-object-3.eps native.fig
IMAGETARGETTYPES=gif png eps
TARGETIMAGES=$(shell sh ./makeimages.sh -e "$(IMAGES)" "$(IMAGETARGETTYPES)")
TEXIFILES=$(NAME).texi

all : $(NAME).ps $(NAME2).ps

%.pstex: %.fig
	fig2dev -Lpstex -m 0.75  $< $@

%.pstex_t: %.fig %.pstex
	fig2dev -Lpstex_t -m 0.75 -p $(basename $<).pstex $< $@

$(NAME).dvi: $(TEXIFILES) $(TARGETIMAGES)
	texi2dvi $(NAME).texi

$(NAME2).dvi: $(NAME2).tex $(PSTEX_T) $(VERBATIM)
	latex $<
	makeindex $(NAME)
	latex $<

$(TARGETIMAGES):
	sh ./makeimages.sh "$(IMAGES)" "$(IMAGETARGETTYPES)"

$(NAME).html: $(TEXIFILES)
	makeinfo --html $<

$(NAME).ps: $(NAME).dvi
	dvips $< -o

$(NAME2).ps: $(NAME2).dvi $(PSTEX)
	dvips $< -o

view: $(NAME).ps
	gv -antialias -scale 1 $<

clean:
	rm -f *.aux *.log *~

spotless:
	make clean
	rm -f *.ps *.dvi *.pstex *.pstex_t *.toc *.idx *.ilg *.ind *pdf \
*ky *pg *tmp *tp *tps *vr *fn *fns *info
