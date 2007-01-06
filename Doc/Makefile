# This Makefile has been cribbed from SBCL.
MCCLIMTEXI:=mcclim.texi
DOCFILES:=*.texi
TMPTYPES:=aux cp cps fn fns ky log pg toc tp tps vr vrs
TMPFILES:=$(foreach target,mcclim,$(foreach type,$(TMPTYPES),$(target).$(type)))
PSFILES=mcclim.ps
PDFFILES=mcclim.pdf
INFOFILE=mcclim.info
HTMLDIRS=$(basename $(MCCLIMTEXI))
IMAGES=ex2.eps inspect-as-cells.eps inspect-object-1.eps \
inspect-object-2.eps inspect-object-3.eps native.fig
IMAGETARGETTYPES=gif png eps pdf
TARGETIMAGES=$(shell sh ./makeimages.sh -e "$(IMAGES)" "$(IMAGETARGETTYPES)")
# Place where generated documentation ends up. The value of
# DOCSTRINGDIR has to end with a slash or you lose (it's passed to
# Lisp's `pathname' function).
DOCSTRINGDIR="docstrings/"
I_FLAGS=-I $(DOCSTRINGDIR)

# SBCL_SYSTEM is an optional argument to this make program. If this
# variable is set, its contents are used as the command line for
# invoking SBCL.

# When passing a non-standard SBCL_SYSTEM, be sure to set the
# environment variable SBCL_HOME to a useful value, as well.

ifeq ($(MAKEINFO),)
  MAKEINFO:=makeinfo
endif

ifeq ($(TEXI2PDF),)
  TEXI2PDF:=texi2pdf
endif

ifeq ($(DVIPS),)
  DVIPS:=dvips
endif

.PHONY: all
all: ps pdf info html

.PHONY: dist
dist: html pdf

$(TARGETIMAGES):
	sh ./makeimages.sh "$(IMAGES)" "$(IMAGETARGETTYPES)"

# html documentation; output in $(HTMLDIRS)
.PHONY: html
html: html-stamp $(TARGETIMAGES)

html-stamp: $(DOCFILES) docstrings
	@rm -rf $(HTMLDIRS)
	$(MAKEINFO) $(I_FLAGS) --html $(HTMLDIRS)
	touch html-stamp

# Postscript documentation
.PHONY: ps
ps: $(PSFILES) $(TARGETIMAGES)

%.ps: %.dvi $(TARGETIMAGES)
	dvips -o $@ $<

# DVI generation
%.dvi: %.texi $(DOCFILES) docstrings $(TARGETIMAGES)
	texi2dvi -b $(I_FLAGS) $<

# PDF documentation
.PHONY: pdf
pdf: $(PDFFILES) $(TARGETIMAGES)

%.pdf: %.texi $(DOCFILES) docstrings $(TARGETIMAGES)
	texi2pdf $(I_FLAGS) $<

# info docfiles
.PHONY: info
info: $(INFOFILE)

%.info: %.texi $(DOCFILES) docstrings
	$(MAKEINFO) $(I_FLAGS) --output=$(INFOFILE) $<

# Texinfo docstring snippets
.PHONY: docstrings
docstrings: tempfiles-stamp

tempfiles-stamp:
	DOCSTRINGDIR=$(DOCSTRINGDIR) PACKAGES=$(PACKAGES) sh make-tempfiles.sh "$(SBCL_SYSTEM)" && touch tempfiles-stamp


.PHONY: softclean
softclean: 
	rm -f *~ *.bak *.orig \#*\# .\#* texput.log *.fasl
	rm -rf $(HTMLDIRS)
	rm -f variables.texi
	rm -f $(PSFILES) $(PDFFILES) html-stamp
	rm -f $(TMPFILES) $(INDEXFILES)
	rm -f mcclim.info mcclim.info-*

.PHONY: clean
clean: softclean
	rm -rf $(DOCSTRINGDIR)
	rm -f tempfiles-stamp

.PHONY: distclean
distclean: clean
