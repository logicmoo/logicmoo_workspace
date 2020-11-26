
all:          draft_cookbook.pdf

draft_cookbook.pdf:	cookbook.pdf
	cp cookbook.pdf draft_cookbook.pdf

cookbook.pdf:	cookbook.ps
	ps2pdf cookbook.ps $@
	#./distill <cookbook.ps >cookbook.pdf

cookbook.ps:   cookbook.dvi
	dvips cookbook -o cookbook.ps

cookbook.dvi:  cookbook.tex \
               foreword.tex \
               introduction.tex \
               downloading-and-installing.tex \
               basic-functionality.tex \
               basic-grammars.tex \
               grammar-specialisation.tex \
               english-resource-grammar.tex \
               other-resource-grammars.tex \
               parsing-and-generation.tex \
               command-line-environment.tex \
               commands.tex \
               config-file-entries.tex \
               window-based-environment.tex \
               basic-speech.tex \
               slm-and-help.tex \
               mobile-applications.tex \
               basic-dialogue.tex \
               advanced-dialogue.tex \
               basic-translation.tex \
               advanced-translation.tex 
	latex     cookbook
	bibtex    cookbook
	makeindex cookbook
	latex     cookbook
	latex     cookbook
	makeindex cookbook
	latex     cookbook
	dvips	  cookbook.dvi -o cookbook.ps

commands.tex:  write_command_section.pl ../../Prolog/regulus_top.pl
	sicstus -l make_command_doc_file.pl

config-file-entries.tex:  write_command_section.pl ../../Prolog/regulus_top.pl
	sicstus -l make_config_entry_section.pl

clean:
	rm -f commands.tex config-file-entries.tex *.ilg *.dvi *~ *.blg *.log

veryclean:
	rm -f *.ilg *.dvi *~ *.blg *.log *.ind *.bbl *.idx  *.toc *.aux

