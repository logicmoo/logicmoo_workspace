all: index.html

TMPFILES = index.log index.aux index.out index.toc

clean:
	rm -f *~ core $(TMPFILES)

distclean: clean
	rm -f index.html manual.pdf


manual.pdf: index.tex
	pdflatex index.tex
	pdflatex index.tex
	pdflatex index.tex
	mv index.pdf manual.pdf

index.html: manual.pdf 
	latex2html -split 0 -no_images  -no_navigation -show_section_numbers index.tex
	mv index/index.html .
	rm -f $(TMPFILES)
	rm -rf index/
