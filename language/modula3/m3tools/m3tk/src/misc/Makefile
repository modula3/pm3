TEXFILES = M3Args.i.tex
DOCNAME = M3Args

# TEXFILES should be a list of names of the tex files that
# are generated by m3totex

# DOCNAME should be the name of the document

# make DOCNAME.dvi will make TeX's dvi (device independent file)

# make proof will make DOCNAME.ps (a postscript file) which can
# be viewed with psview

# make print will print the document.

all: $(TEXFILES) $(DOCNAME).dvi

$(DOCNAME).dvi: $(DOCNAME).tex $(TEXFILES)
	latex $(DOCNAME).tex

print: $(DOCNAME).dvi
	dvips $(DOCNAME).dvi

proof: $(DOCNAME).dvi
	dvips -o $(DOCNAME).ps $(DOCNAME).dvi
	ghostview $(DOCNAME).ps

.SUFFIXES: .i3 .i.tex .m3 .m.tex .ig .i.tex

.i3.i.tex: 
	m3totex $*.i3

.ig.i.tex: 
	m3totex $*.ig

.m3.m.tex: 
	m3totex $*.m3


