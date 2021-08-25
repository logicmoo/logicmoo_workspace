MFILES	= $(MIZFILES)
MMLLAR  = $(MFILES)/mml.lar
MMLLAR100  = $(MFILES)/mml.lar.100
MIZ100     = $(shell cat $(MMLLAR100))
STANDARDMIZ     = $(shell cat $(MMLLAR))
SHMIZ = $(shell shuffle < $(MMLLAR))

XSLTPROC = /home/urban/bin/xsltproc.1.1.15 
XML2HTML = /home/urban/xsltxt/xsl4mizar/miz.xsl
ADDABS = /home/urban/xsltxt/xsl4mizar/addabsrefs.xsl
MIZPL = /home/urban/xsltxt/xsl4mizar/mizpl.xsl
MIZ2DLI = /home/urban/xsltxt/xsl4mizar/miz2dli.xsl
ACCOM = $(MFILES)/bin/accom 
VERIFIER = $(MFILES)/bin/verifier.7.8.05_hacktopcnst_bex

# use this to get rid of the REAL and ARITHM requirements:
# for i in *.miz; do echo $i; perl -e 'while(<>){if(/::/){} elsif(/\brequirements\b/) { s/, *(REAL|ARITHM)//g; s/(REAL|ARITHM), *//g;} print $_}' $i > $i.new; done


%.acc: %.miz
	$(ACCOM) $* > $*.acc 

%.bex: %.xml
	$(VERIFIER) -q -c $*
	perl -i -e 'local $$/;$$_=<>; m/((.|[\n])*?)<(PolyEval|.ByExpl)/; print $$1; while(m/(<PolyEval((.|[\n])*?)<\/PolyEval>)/g) { if(!(exists $$h{$$1})) { print $$1; $$h{$$1} = (); }} print "</ByExplanations>";' $*.bex

%.xml: %.acc
	$(VERIFIER) -q   $*  
	perl -i -e 'local $$/;$$_=<>; m/((.|[\n])*?)<(PolyEval|.ByExpl)/; print $$1; while(m/(<PolyEval((.|[\n])*?)<\/PolyEval>)/g) { if(!(exists $$h{$$1})) { print $$1; $$h{$$1} = (); }} print "</ByExplanations>";' $*.bex


%.xml1: %.xml
	$(XSLTPROC) $(ADDABS)  $*.xml > $*.xml1 

%.dli: %.xml1
	$(XSLTPROC) $(MIZ2DLI)  $*.xml1 > $*.dli


%.zrt1: %.zrt
	$(XSLTPROC) $(ADDABS)  $*.zrt > $*.zrt1 

%.zrt2: %.zrt1
	$(XSLTPROC) $(MIZPL)  $*.zrt1 > $*.zrt2 


%.xml2: %.xml1
	$(XSLTPROC) $(MIZPL)  $*.xml1 > $*.xml2 

%.html: %.xml1
	$(XSLTPROC) --param proof_links \'1\' $(XML2HTML)  $*.xml1 > $*.html 

%.htmla1: %.xml1
	$(XSLTPROC) --param ajax_proofs \'1\' --param colored \'1\' --param proof_links \'1\' $(XML2HTML)  $*.xml1 > $*.htmla1 

%.htmla2: %.xml1
	$(XSLTPROC) --param ajax_proofs \'2\' --param proof_links \'1\' $(XML2HTML)  $*.xml1 > $*.htmla2 

# all: xboole.html
allhtml: $(addsuffix .html, $(SHMIZ))
allhtmla1: $(addsuffix .htmla1, $(SHMIZ))
allhtmla2: $(addsuffix .htmla2, $(SHMIZ))
html100: $(addsuffix .html, $(MIZ100))
allacc: $(addsuffix .acc, $(STANDARDMIZ))
allxml: $(addsuffix .xml, $(STANDARDMIZ))
allxml1: $(addsuffix .xml1, $(SHMIZ))
allxml2: $(addsuffix .xml2, $(SHMIZ))
allbex: $(addsuffix .bex, $(STANDARDMIZ))
allzrt1: $(addsuffix .zrt1, $(STANDARDMIZ))
allzrt2: $(addsuffix .zrt2, $(STANDARDMIZ))
alldli: $(addsuffix .dli, $(SHMIZ))
