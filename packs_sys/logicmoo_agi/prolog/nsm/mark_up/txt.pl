markup_text(pre_formula_item(Indent),Spaces) :-
	get_n_spaces(Indent,Spaces).

markup_text(post_formula_item,"\n").


markup_text(pre_formatted_rule,"\n\nRULE: ").
markup_text(pre_cat_a,"\nFIRST DEPENDANT\n CAT: ").
markup_text(pre_cat_b,"\nSECOND DEPENDANT\n CAT: ").
markup_text(pre_cat_c,"\nRESULT\n CAT: ").
markup_text(pre_dep_a,"\n").
markup_text(pre_dep_b,"\n").
markup_text(pre_dep_c,"\n").

markup_text(post_dep_a,"\n").
markup_text(post_dep_b,"\n").
markup_text(post_dep_c,"\n").

markup_text("nsm","").
markup_text("/nsm","").

markup_text(prologue,[]).
markup_text(epilogue,[]).

markup_text("title"," ** ").
markup_text("/title"," ** ").
markup_text(id_after," ").



markup_text(txt,pre_formula_item(Indent),Spaces) :-
	get_n_spaces(Indent,Spaces).

markup_text(dounlerightarrow," ==> ").
