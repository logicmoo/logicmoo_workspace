markup_text(emph(1),"\\i ").
markup_text(emph(0),"\\i0 ").
markup_text(bold(1),"\\b ").
markup_text(bold(0),"\\b0 ").
markup_text(p(0),"\\par}\n").
markup_text(p(1),"{\\pard\\sb100\\qj\\fi500\\fs24 \n").

markup_text(prologue(L1,L2)
	   ,l:[
"{\\rtf1\\ansi\\deff0\n\
{\\fonttbl\
{\\f0 ",

	       text_font(L1),
	                           ";}\
{\\f1 ",
	       nsm_font(L2),
";}}\n\
{\\colortbl;\
\\red0\\green0\\blue0;\
\\red255\\green0\\blue0;\
\\red0\\green0\\blue255;}\
\n\
{\\stylesheet\n\
{\\s1\\f1\\cf3\\fs24 NSM1;}\
{\\s2\\f2\\cf1\\fs28 NSM2;}}\n"
		       ]).


markup_text(text_font(_),"Aboriginal Serif").
markup_text(nsm_font(_),"Aboriginal Sans").

markup_text(epilogue,"}").

markup_text(nonumber_header(Title),l:[
				       "{\\pard\\qc\\fs32\\i ",
				       Title,
				       "\\par}\n"]).

markup_text(pre_nsm_sent," ").
markup_text(post_nsm_sent(no_last),"\\line").
markup_text(post_nsm_sent(last),"\n").


markup_text("titlepage","").
markup_text("/titlepage","\\page\n").

markup_text("title","\\pard\\sb4000\\qc\\f0\\cf0\\fs46 ").
markup_text("/title","\\par \n").

markup_text("subtitle","\\pard\\sb350\\qc\\f0\\cf0\\fs30 ").
markup_text("/subtitle","\\par \n").

markup_text("author","\\pard\\sb1000\\qc\\f0\\cf0\\fs26\\i ").
markup_text("/author","\\par \n").

markup_text("t1","\\pard\\sa700\\qc\\f0\\cf0\\fs34 ").
markup_text("/t1","\n").

markup_text("t2","\\pard\\sb100\\sa150\\qc\\f0\\cf2\\fs32\\keepn\\i ").
% markup_text(rtf,"t2_after_s"," ").
markup_text("/t2","\n").

markup_text("s2","\\pard\\sb350\\sa150\\qc\\f0\\cf2\\fs32\\keepn\\i ").
markup_text("/s2","\n").

markup_text("nsm",
"\\par\\pard \\li1000 \\ri1000 \\sa200 \\f1 \\cf1 \\fs22 \
\\ql \\b0 \\i0 \\widctlpar\
\\brdrt \\brdrs \\brdrw10 \\brsp20\n\
\\brdrl \\brdrs \\brdrw10 \\brsp80\n\
\\brdrb \\brdrs \\brdrw10 \\brsp20\n\
\\brdrr \\brdrs \\brdrw10 \\brsp80 \n").
markup_text("/nsm","\\par\\pard\n").


markup_text("btxt",
"\\par\\pard \\sa200 \\f1 \\cf1 \\fs22 \\qj \\b0 \\i0 \\widctlpar\
\\brdrt \\brdrs \\brdrw10 \\brsp20\n\
\\brdrl \\brdrs \\brdrw10 \\brsp80\n\
\\brdrb \\brdrs \\brdrw10 \\brsp20\n\
\\brdrr \\brdrs \\brdrw10 \\brsp80 \n").
markup_text("/btxt","\\par\\pard\n").

markup_text(id_before," {\\super\\f0\\cf3 ").
markup_text(id_after,"}\n").

markup_text(pre_nsm_sent,"").
markup_text(post_nsm_sent,"\line\n").

markup_text("p","\\par\\pard\\sa200\\qj\\b0\\i0\\f0\\fs24\\cf1 ").
markup_text("/p","\n").

markup_text(single_line_prologue,
	    "\\par{\\pard\n\\trowd\\trgaph144\\cellx4800\\cellx9600\n").
markup_text(single_line_before,"{\\pard\\s1\\cf3\\intbl ").
markup_text(single_line_infra,"\\cell}\n{\\pard\\s2\\intbl ").
markup_text(single_line_after,"\\cell}\n\\row\n").
markup_text(single_line_epilogue,"\\lastrow}\n").

markup_text(toc_prologue,"\\par\\sect\\sectd\\pard\\qc\\fs32\\i0\\b Table of Contents \\par\n").
markup_text(toc_epilogue,"\\par\n").
markup_text(pre_toc_entry("1"),"\\pard\\plain ").
markup_text(pre_toc_entry("2"),"\\pard\\plain ").
markup_text(pre_toc_entry("3"),"\\pard\\plain ").
markup_text(pre_toc_entry(_),"\\pard\\plain ").
markup_text(infra_toc_entry,"\\tab ").
markup_text(infra_toc_entry,"\\tab ").
markup_text(post_toc_entry,"\\par\n").



escaped_char(10,[10,13,32]).
escaped_char(13,[]).



/* GRAMMAR FORMATTER */

markup_text(grammar_heading(L1,L2),
	   l:[
"{\\rtf1\\ansi\\deff0\n\
{\\fonttbl\
{\\f0 ",

	       text_font(L1),
	                           ";}\
{\\f1 ",
	       nsm_font(L2),
";}}\n\
{\\colortbl;\
\\red0\\green0\\blue0;\
\\red255\\green0\\blue0;\
\\red0\\green0\\blue255;}\
\n\
{\\stylesheet\n\
{\\s1\\f1\\cf3\\fs24 NSM1;}\n\
{\\s2\\f2\\cf1\\fs28 NSM2;}\n\
{\\s3\\f1\\cf1\\fs40\\qc\\sb500\\b\
 \\brdrt\\brdrs \\brdrl\\brdrs \\brdrr\\brdrs\\lang1024 TITLE;}}\n"
		       ]).
markup_text(grammar_end,"}").
markup_text(pre_grammar_title,"{\\pard\\s3 ").
markup_text(post_grammar_title,"\\par}\n").
markup_text(pre_grammar_version,"{\\pard\\b0\\qc\\fs24\\brdrl\\brdrs\\brdrr\\brdrs\\lang1024 ").
markup_text(post_grammar_version,"\\par}\n").
markup_text(pre_grammar_author,"{\\pard\\i\\qc\\sb100\\fs28\\brdrl\\brdrs\\brdrr\\brdrs ").
markup_text(post_grammar_author,"\\par}\n").
markup_text(pre_grammar_date,"{\\pard\\i\\qc\\sb200\\fs24\\brdrl\\brdrs\\brdrr\\brdrs\\lang1024 ").
markup_text(post_grammar_date,"\\par}\n").
markup_text(pre_grammar_ackn,"{\\pard\\ql\\b\\ql\\fs28\\brdrl\\brdrs\\brdrr\\brdrs\\brdrb\\brdrs\\lang1024 ").
markup_text(pre_grammar_ackn_body,": \\b0\\i ").
markup_text(post_grammar_ackn,"\\par}\n").
markup_text(post_grammar_frontmatter,"\n\n").

markup_text(pre_grammar_abstract_title,"{\\pard\\qj\\sb200\\sa200\\b\\lang1024 ").
markup_text(post_grammar_abstract_title,"\\b0 ").
markup_text(post_grammar_abstract,"\\par}\n\n").


markup_text(pre_grammar_prologue,"\n\\page\n").
markup_text(post_grammar_prologue,"\n\\lang1024\n").
	    

markup_text(pre_dep_rule(NUM,DIRECT),l:[
"\n{\\pard\\sb250\\fs28\\cf2\\keepn ",NUM, " ", DIRECT, "\\par}\n",
"\n{\\pard\\fs20\n\
\\trowd\
\\clbrdrt\\brdrs\\clbrdrl\\brdrs\\clbrdrb\\brdrs\\clbrdrr\\brdrs\
 \\trgaph144\\cellx3200\
\\clbrdrt\\brdrs\\clbrdrl\\brdrs\\clbrdrb\\brdrs\\clbrdrr\\brdrs\
 \\trgaph144\\cellx6400\
\\clbrdrt\\brdrs\\clbrdrl\\brdrs\\clbrdrb\\brdrs\\clbrdrr\\brdrs\
 \\trgaph144 \\cellx9600\
\\clbrdrt\\brdrs\\clbrdrl\\brdrs\\clbrdrb\\brdrs\\clbrdrr\\brdrs\n"
				]).			  
markup_text(pre_cat_a,"\\pard\\intbl A: {\\i ").
markup_text(pre_dep_a,"}\\line \n").
markup_text(post_dep_a," \\cell \n").
markup_text(pre_cat_b,"\\pard\\intbl B: {\\i ").
markup_text(pre_dep_b,"}\\line\n").
markup_text(post_dep_b,"\\cell\n").
markup_text(pre_cat_c,"\\pard\\intbl C: {\\i ").
markup_text(pre_dep_c,"}\\line\n").
markup_text(post_dep_c,"\\cell\n").
markup_text(post_dep_rule,"\\row}\n").
markup_text(pre_dep_cond_list(CONDITIONS),l:["{\\pard\\b ",CONDITIONS,": \\line\n"]).
markup_text(post_dep_cond_list,"\\par}\n").
markup_text(pre_formula_item(2),". ").
markup_text(pre_formula_item(4),". . ").
markup_text(pre_formula_item(6),". . . ").
markup_text(pre_formula_item(8),". . . . ").
markup_text(pre_formula_item(10),". . . . . ").
markup_text(pre_formula_item(12),". . . . .. ").
markup_text(pre_formula_item(14),". . . . ... ").
markup_text(pre_formula_item(16),". . . . .... ").
markup_text(post_formula_item,"\\line\n").

markup_text(pre_exp_cond_list(Paradigms),l:["{\\pard\\b ",Paradigms],"\\line\\b0\n").
markup_text(post_exp_cond_list,"\\par}\n").
markup_text(post_exp_par_item,"\\line\n").

markup_text(pre_grammar_ph_header,"\n{\\pard\\qc\\sb150\\i\\fs32\\keepn ").
markup_text(post_grammar_ph_header,"\\par}\n").
markup_text(pre_ph_rule,"{\\pard\\sb70\"\\b\\keepn ").
markup_text(pre_ph_body,"\"\\par}\n{\\pard\\brdrl\\brdrs\\brdrt\\brdrs\
\\brdrr\\brdrs\\brdrb\\brdrs \\line\\b0\\i ").
markup_text(pre_ph_context," \\\\").
markup_text(post_ph_context,"; ").
markup_text(post_ph_rule,"\\line\\par}\n").
markup_text(longrightarrow," \\u8594? ").


markup_text(pre_grammar_level_table,"{\\pard\\tabx5000\\tldot\\tqc\
\\brdrt\\brdrs\\brdrl\\brdrs\\brdrb\\brdrs\\brdrr\\brdrs\\li4000\\ri4000"). 
markup_text(infra_level_header,"\\tab ").
markup_text(post_level_header,"\\line\n").
markup_text(pre_level_item,"\\line\n  ").
markup_text(infra_level_item,"\\tab ").
markup_text(post_level_item,"").
markup_text(post_grammar_level_table,"\\par}\n").


markup_text(sect_header(N,H),l:[
"\n{\\pard\\fs32\\b\\sb200\\qc\\keepn ",
				N,
" ",
				H,
"\\par}\n"				
			       ]).
markup_text(pre_grammar_sect_body,"{\\pard\\qj\\fs24 ").
markup_text(post_grammar_sect_body,"\\par}\n").


markup_text(pre_grammar_transcr_table(N,Table),l:[
"\n{\\pard\\b ",Table," ",N,"\\line\\b0 \n"]).
markup_text(post_transcr_table_item,";\n ").
markup_text(post_grammar_transcr_table,"\\par}\n").


markup_text(pre_grammar_mseq_header,"{\\pard\\sb150\\qc\\i\\fs32\\keepn ").
markup_text(post_grammar_mseq_header,"\\par}\n").
markup_text(pre_grammar_mseq,"{\\pard\n").
markup_text(post_grammar_mseq,"\\par}\n").
markup_text(pre_grammar_mseq_rule(N),l:[N,". "]).
markup_text(post_grammar_mseq_rule,"\\line\n").



markup_text(pre_rule_doc,"{\\pard ").
markup_text(post_rule_doc,"\\par}\n").

markup_text(pre_ph_example,"").
markup_text(infra_ph_example,"").
markup_text(post_ph_example,"").

markup_text(s_with_translation_prologue(EXAMPLES),l:[
	    "{\\pard\\sb200\\b\\keepn ", EXAMPLES, "\\par}\n"]).
markup_text(s_with_analysis_prologue(EXAMPLES),l:[
"{\\pard\\sb200\\b ",EXAMPLES,"\\par}\n\
{\\pard\n\\trowd\\trgaph144\\cellx2500\\cellx9600\n"]).
markup_text(s_with_analysis_epilogue,"}\n").
markup_text(ph_example_prologue,"").
markup_text(ph_example_epilogue,"\\lastrow}\\pard\n").

markup_text(pre_sent_with_analysis,"{\\pard\\fs24\\cf3\\intbl ").
markup_text(infra_sent_with_analysis,"\\cell}\n{\\pard\\cf0\\fs20\\intbl ").
markup_text(post_sent_with_analysis,"\\cell}\n\\row\n").


markup_text(ex_fail,"{\\pard\\cf2\\fs32 ** Example translation failed\\par}\n").
markup_text(format_dep_rule_fail,"{\\pard\\cf2\\fs32 ** rule formattation failed\\par}\n").
