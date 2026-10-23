PROJECT = Harmonic

SOURCES = Util.lhs CyclicList.hs IMatrix.hs Mappable.hs Permutation.hs \
	Graph.lhs GraphMap.lhs GraphIO.lhs GraphCover.lhs Circle.lhs \
	Harmonic.lhs Filling.lhs OptFilling.lhs Torus.lhs \
	Examples.lhs Julia.lhs Output.hs

PDFS = Util.pdf	Graph.pdf GraphMap.pdf GraphIO.pdf GraphCover.pdf Circle.pdf \
	Harmonic.pdf Filling.pdf OptFilling.pdf Torus.pdf \
	Examples.pdf Julia.pdf

EXTRAS = preamble.tex defs.tex macros.tex output.tex output.sage Makefile FILES

all: Examples.o Julia.o $(PDFS)

Examples.o Julia.o: %.o: %.lhs $(SOURCES)
	ghc -O3 $<

%.pdf: %.lhs defs.tex preamble.tex macros.tex
	pdflatex $<
	pdflatex $<

pack: $(PROJECT).tar.gz
$(PROJECT).tar.gz: $(SOURCES) $(PDFS) $(EXTRAS)
	mkdir $(PROJECT)
	cp -p --dereference $(SOURCES) $(PDFS) $(EXTRAS) $(PROJECT)/
	tar cfvz $(PROJECT).tar.gz $(PROJECT)
	rm -R $(PROJECT)

