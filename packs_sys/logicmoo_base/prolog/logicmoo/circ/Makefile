
SWIPLCMD= swipl -G1G -L1G -T1G -f "../pie_20180213/src/folelim/load.pl"
SWIPLCMD= swipl -G1G -L1G -T1G -f "./pie/src/folelim/load.pl"

all: circ2

circ2: scratch_circ2.pl
	echo "ppl." | $(SWIPLCMD) -l scratch_circ2.pl

bib:
	cp mybib.bib /tmp
	( cd /tmp && bibtex tmp_ppl.aux && pdflatex tmp_ppl.tex )
	( cd /tmp && bibtex tmp_ppl.aux && pdflatex tmp_ppl.tex )

final: circ2 bib
	cp /tmp/tmp_ppl.pdf On_Circumscription.pdf

interactive:
	$(SWIPLCMD) -l scratch_circ2.pl

bare:
	$(SWIPLCMD)

clean:
	rm /tmp/tmp_ppl.*

show: circ2
	open /tmp/tmp_ppl.pdf &
