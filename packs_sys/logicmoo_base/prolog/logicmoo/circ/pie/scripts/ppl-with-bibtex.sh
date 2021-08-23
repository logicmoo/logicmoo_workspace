#!/bin/bash
swipl --stack_limit=12g -f ${PIE}/folelim/load.pl -s ${1} -g ppl -t halt
TMPDIR=/tmp
cp ${PIE}/scratch/bibscratch03.bib ${TMPDIR}
cd ${TMPDIR}
pdflatex tmp_ppl
bibtex tmp_ppl
pdflatex tmp_ppl
pdflatex tmp_ppl
TEXOUTFILE=`basename ${1} .pl`_out.tex
OUTFILE=`basename ${1} .pl`.pdf
cp tmp_ppl.tex $TEXOUTFILE
cp tmp_ppl.pdf $OUTFILE
echo written ${TMPDIR}/${TEXOUTFILE}
echo written ${TMPDIR}/${OUTFILE}


