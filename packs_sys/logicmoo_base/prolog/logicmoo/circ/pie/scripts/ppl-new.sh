#!/bin/bash
#
# No explicit invocation of pdflatex, bibtex etc. as this can now (Spring
# 2019) be expressed by the latex_processing option within the document.
#
swipl --stack_limit=12g -f ${PIE}/folelim/load.pl -s ${1} -g ppl -t halt
TMPDIR=/tmp
cd ${TMPDIR}
TEXOUTFILE=`basename ${1} .pl`_out.tex
OUTFILE=`basename ${1} .pl`.pdf
cp tmp_ppl.tex $TEXOUTFILE
cp tmp_ppl.pdf $OUTFILE
echo written ${TMPDIR}/${TEXOUTFILE}
echo written ${TMPDIR}/${OUTFILE}
