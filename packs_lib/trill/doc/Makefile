
all: manual.pdf manual.html help-trill.pdf help-trill.html

manual.pdf: manual.tex inference.tex syntax.tex semantics.tex inf_goals.tex bib.bib
	pdflatex manual
	bibtex manual
	pdflatex manual
	pdflatex manual

manual.html: manual.tex inference.tex syntax.tex semantics.tex inf_goals.tex bib.bib
	pandoc manual.tex -o manual.html -s --bibliography bib.bib  --csl acm-sigchi-proceedings.csl --table-of-contents --toc-depth=3 --mathjax --metadata link-citations
  

help-trill.pdf: help-trill.tex inference.tex syntax.tex semantics.tex inf_goals.tex bib.bib
	pdflatex help-trill
	bibtex help-trill
	pdflatex help-trill
	pdflatex help-trill

help-trill.html: help-trill.tex inference.tex syntax.tex semantics.tex inf_goals.tex bib.bib
	pandoc help-trill.tex -o help-trill.html -s --bibliography bib.bib  --csl acm-sigchi-proceedings.csl --table-of-contents --toc-depth=3 --mathjax --metadata link-citations
