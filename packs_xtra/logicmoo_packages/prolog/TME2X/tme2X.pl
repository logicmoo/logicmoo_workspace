%%
%% tme2X -- Convert from PROTEIN tme files into something else
%% see the shell script 'tme2X' to get an idea of what this quick hack does,
%%
%% You may do with this file whatever you want to. If you improve it or find bugs please
%% let me know.
%%
%% Enjoy!
%%
%% Peter Baumgartner (peter@informatik.uni-koblenz.de)
%%
%% ToDo/Bugs: 
%% cost annotations #(M,N) are not handled yet (Peter, 20/2/98).
%%
%% ChangeLog:
%%
%% 7.4.98:   bug fix: ?- -clauses remain ?- -clauses, even for e.g. ?- -a.
%% 18.3.98:  add BEGIN_AXIOMS ... END_AXIOMS around =-Axioms in tmeeqax setting
%% 16.12.97: skipping of simplification rules
%% 10.11.97: NAME comments for equality axioms
%% 9.9.97:   add output mode for schwerpunkt syntax.
%% 30.6.97:  the modification fo 10.4.97 is undone; scan-it won't insert 
%%           correct inference rules anyway
%% 10.4.97:  for tmeeqax: don't supply function substitution axiom's 
%%           head with result sorts,
%%           so that scan-it has a chance to identify these axioms and replace
%%           them by completed symbols
%% 10.2.97:  added conversion to restart ME Horn clause set.
%% 18.12.96: added '<-' Notation for clauses
%%           use Benno's myread instead of standard read, so that
%%           comments survive the translation
%%           add make_tme2X_exec predicate at end
%% 20.11.96: strengthened tmeclean variant so that prolog [.|.] notation
%%           is converted into a more neutral form cons(.,.)
%% 15.11.96:
%%   - added 'tmeeqax' output variant for adding equality axioms.
%% 29.7.96:
%%   - added 'tmeclean' output variant.
%% 10.4.96: 
%%   - changed output mode to "D" instead of "DV"
%%   - SETHEO output now prints with [.|.] lists instead of cons(.,.)
%%      and output is formatted with newlines
%% abolish some unwanted infix ops:
%:- current_op(_P,A,from) ->
%	global_op(0, A, from)
%    ;   true.
%


:- [misc,myread,'parser-tme2X',renaming].

:- setval(negation_sign,'-').
:- setval(comment_sign,'%').
:- setval(protein_output,false). %% true if the output goes into protein


:- lib(lists).
%% seems to be buggy:
%%:- set_flag(output_mode, "DV").
:- set_flag(output_mode, "D").



tme2X('tme=',InFileName,OutFileName) :-
	concat_atoms(InFileName,' ',H1),
	concat_atoms(H1,OutFileName,H2),
	concat_atoms('$TME2XHOME/tme2tme= ',H2,Call),
	sh(Call).


%tme2X('sat',InFileName,OutFileName) :-
%	concat_atoms(InFileName,' ',H1),
%	concat_atoms(H1,OutFileName,H2),
%	concat_atoms('/lab/ki2/AGKI/systems/TME2X/sat2tme ',H2,Call),
%	sh(Call).

tme2X(Format,InFileName,OutFileName) :-
	( \+ member(Format,[ores,semantics,satchmo,tptp,
	    thsetheo,setheo,tmeeqax,tmedomain,renamed,tmeclean,spass,otter,rme,sp,sf,tf]) ->
	    printf("Error: Format %w unknown\n",[Format]),
	    exit(1)
	  ; true),
	init_parser,
	consult_file(InFileName),
	write_X_file(Format,OutFileName).

write_X_file(otter,OutFileName) :-
	concat_atoms(OutFileName, '-otter.in', OutFileNameExt),
	open(OutFileNameExt, write, F),
	setval(comment_sign,'%'),
        setval(expand_read, true),
	banner(F),
	printf(F,"set(prolog_style_variables).\n",[]),
	printf(F,"set(auto).\n\n",[]),
	printf(F,"list(usable).\n",[]),
	write_otter_file(F),
	printf(F,"end_of_list.\n",[]),
	close(F),
	printf("Output written to %w\n",[OutFileNameExt]).

write_X_file(spass,OutFileName) :-
	concat_atoms(OutFileName, '-spass.in', OutFileNameExt),
	open(OutFileNameExt, write, F),
        setval(expand_read, true),
	setval(comment_sign,'%'),
	banner(F),
	write_spass_file(F),
	close(F),
	printf("Output written to %w\n",[OutFileNameExt]).

write_X_file(ores,OutFileName) :-
	concat_atoms(OutFileName, '.ores', OutFileNameExt),
	open(OutFileNameExt, write, F),
        setval(expand_read, true),
	setval(comment_sign,';'),
	banner(F),
	write_ores_file(F),
	close(F),
	printf("Output written to %w\n",[OutFileNameExt]).

write_X_file(semantics,OutFileName) :-
	concat_atoms(OutFileName, '.input', OutFileNameExt),
	open(OutFileNameExt, write, F),
        setval(expand_read, true),
	setval(comment_sign,';'),
	banner(F),
	printf(F,"(clauses\n",[]),
	write_semantics_file(F),
	printf(F,"\n)",[]),
	close(F),
	printf("Output written to %w\n",[OutFileNameExt]).

write_X_file(satchmo,OutFileName) :-
	concat_atoms(OutFileName, '.sat', OutFileNameExt),
	open(OutFileNameExt, write, F),
        setval(expand_read, true),
	setval(comment_sign,'%'),
	banner(F),
	write_satchmo_header(F),
	write_satchmo_file(F),
	write_satchmo_footer(F),
	close(F),
	printf("Output written to %w\n",[OutFileNameExt]).

write_X_file(setheo,OutFileName) :-
	setval(negation_sign,'~'),
	concat_atoms(OutFileName, '.lop', OutFileNameExt),
	open(OutFileNameExt, write, F),
        setval(expand_read, true),
	setval(comment_sign,'%'),
	banner(F),
	write_setheo_header(F),
	write_setheo_file(F),
	close(F),
	printf("Output written to %w\n",[OutFileNameExt]).

%% two-fault assumption
write_X_file(tf,OutFileName) :-
	concat_atoms(OutFileName, '.2f', OutFileNameExt),
	open(OutFileNameExt, write, F),
	write_tf_file(F),
	close(F),
	printf("Output written to %w\n",[OutFileNameExt]).

%% single-fault assumption
write_X_file(sf,OutFileName) :-
	concat_atoms(OutFileName, '.1f', OutFileNameExt),
	open(OutFileNameExt, write, F),
	write_sf_file(F),
	close(F),
	printf("Output written to %w\n",[OutFileNameExt]).

%% Schwerpunkt Syntax
write_X_file(sp,OutFileName) :-
	concat_atoms(OutFileName, '.sp', OutFileNameExt),
	open(OutFileNameExt, write, F),
        setval(expand_read, true),
	setval(comment_sign,'%'),
	banner(F),
	printf(F,"begin_problem(%w).\n",[OutFileName]),
	write_sp_header(F),
	printf(F,"list_of_formulae(axioms).\n",[]),
	write_sp_file(F),
	printf(F,"end_of_list.\n",[]),
	printf(F,"end_problem.\n",[]),
	close(F),
	printf("Output written to %w\n",[OutFileNameExt]).

write_X_file(thsetheo,OutFileName) :-
	setval(negation_sign,'~'),
	concat_atoms(OutFileName, '.lop', OutFileNameExt),
	open(OutFileNameExt, write, F),
        setval(expand_read, true),
	setval(comment_sign,'%'),
	banner(F),
	write_setheo_header(F),
	write_thsetheo_file(F),
	close(F),
	printf("Output written to %w\n",[OutFileNameExt]).

write_X_file(tmedomain,OutFileName) :-
	concat_atoms(OutFileName, '-domain.tme', OutFileNameExt),
	open(OutFileNameExt, write, F),
%        setval(expand_read, false),
        setval(expand_read, true),
	setval(comment_sign,'%'),
        setval(protein_output,true),
	banner(F),
	write_tmedomain_header(F),
	write_tmedomain_file(F),
	close(F),
	printf("Output written to %w\n",[OutFileNameExt]).

write_X_file(renamed,OutFileName) :-
	concat_atoms(OutFileName, '-renamed.tme', OutFileNameExt),
	open(OutFileNameExt, write, F),
%        setval(expand_read, false),
        setval(expand_read, true),
	setval(comment_sign,'%'),
        setval(protein_output,true),
	banner(F),
% now built in :-)))
%	write_tmerenamed_header(F),
	write_tmerenamed_file(F),
	close(F),
	printf("Output written to %w\n",[OutFileNameExt]).

write_X_file(tmeeqax,OutFileName) :-
	concat_atoms(OutFileName, '-eqax.tme', OutFileNameExt),
	open(OutFileNameExt, write, F),
        setval(expand_read, true),
%        setval(expand_read, false),
	setval(comment_sign,'%'),
        setval(protein_output,true),
	banner(F),
	write_tmeeqax_header(F),
	write_tme_file(F),
	close(F),
	printf("Output written to %w\n",[OutFileNameExt]).


%%% tptp
write_X_file(tptp,OutFileName) :-
	setval(tme_cleanlit,on),
	concat_atoms(OutFileName, '.p', OutFileNameExt),
	open(OutFileNameExt, write, F),
        setval(expand_read, true),
%        setval(expand_read, false),
	setval(comment_sign,'%'),
        setval(protein_output,true),
	banner(F),
	write_tptp_file(F),
	close(F),
	printf("Output written to %w\n",[OutFileNameExt]).


write_X_file(rme,OutFileName) :-
	concat_atoms(OutFileName, '-rme.tme', OutFileNameExt),
	open(OutFileNameExt, write, F),
        setval(expand_read, true),
%        setval(expand_read, false),
	setval(comment_sign,'%'),
        setval(protein_output,true),
	banner(F),
	printf(F,"%% Reduction steps:\n",[]),
	write_rme_header(F),
	write_rme_file(F),
	write_rme_footer(F),
	close(F),
	printf("Output written to %w\n",[OutFileNameExt]).




write_X_file(tmeclean,OutFileName) :-
	setval(tme_cleanlit,on),
	concat_atoms(OutFileName, '-clean.tme', OutFileNameExt),
	open(OutFileNameExt, write, F),
        setval(expand_read, true),
%        setval(expand_read, false),
	setval(comment_sign,'%'),
	banner(F),
	write_tme_file(F),
	close(F),
	printf("Output written to %w\n",[OutFileNameExt]),
	setval(tme_cleanlit,off).


skip_clause(F,C,comment) :-
	name(C,[StringCommentSign_ascii|Rest]),
	getval(comment_sign,Comment_sign),
	name(Comment_sign,[Comment_sign_ascii]),
	(StringCommentSign_ascii = Comment_sign_ascii ->
	    NewC = C %% have the correct comment sign
	%% else replace first char by correct comment sign for output language
	; name(NewC,[Comment_sign_ascii|Rest])), 
	printf(F,"%w\n",[NewC]).
skip_clause(F,C,skip) :-
%	writeq(F,C), nl(F).
	printf(F,"%q.\n",C).

banner(F) :-
	getval(comment_sign,C),
	printf(F,"%w%w File generated by tme2X utility.\n%w%w Hope you have a nice proof!\n\n",[C,C,C,C]).


%%%%%%%%% setheo  %%%%%%%%%%%%%%%%%%%%%%%%%%
write_setheo_file(F) :-
	retract(oi_clause(C,Type)),
	(skip_clause(F,C,Type) ->
	    true
	; split_clause(C,Pos,Neg,_PosVars,_SV),
	  write_setheo_clause(F,Pos,Neg)),
	write_setheo_file(F).
write_setheo_file(_F).

write_setheo_header(_F).

write_setheo_clause(F,[],Neg) :-
	printf(F,"<- ",[]),
	write_clean_litlist(F,',',Neg),
	printf(F,".\n",[]), !.
write_setheo_clause(F,Pos,[]) :-
	write_clean_litlist(F,';',Pos),
	printf(F," <-.\n",[]), !.
write_setheo_clause(F,Pos,Neg) :-
	write_clean_litlist(F,';',Pos),
	printf(F," <-%n%t ",[]),
	write_clean_litlist(F,',',Neg),
	printf(F,".\n",[]), !.


%%%%%%%%% tmeclean  %%%%%%%%%%%%%%%%%%%%%%%%%%
write_tme_file(F) :-
	retract(oi_clause(C,Type)),
	(skip_clause(F,C,Type) ->
	    true
	;  split_clause(C,Pos,Neg,_PosVars,_SV),
	write_tme_clause(F,Pos,Neg,Type)),
	write_tme_file(F).
write_tme_file(_F).


%%%%%%%%% tptp  %%%%%%%%%%%%%%%%%%%%%%%%%%
write_tptp_file(F) :-
	retract(oi_clause(C,Type)),
	(skip_clause(F,C,Type) ->
	    true
	;  split_clause(C,Pos,Neg,_PosVars,_SV),
	write_tptp_clause(F,Pos,Neg,Type)),
	write_tptp_file(F).
write_tptp_file(_F).


write_tptp_clause(F,Pos,Neg,Type) :-
	(Type == query -> TPTPType = conjecture ; TPTPType = hypothesis),
	printf(F,"input_clause(unknown,%w,%n",[TPTPType]),
	printf(F,"    [",[]),
	negate_list(Neg,N),
	append(N,Pos,Lits),
	write_tptp_litlist(F,Lits),
	printf(F,"]).%n%n",[]), !.


write_tptp_litlist(_F,[]).
write_tptp_litlist(F,[L|R]) :-
	(getval(tme_cleanlit,on) ->
	    convert_equal(L,H),
	    colon_to_s_cons(H,HL)
%	    colon_to_s(L,HL)
	;   HL = L),
%%% Ingo-servicable part: Hier werden Literale geprinted
%	printf(F,"%UPVmw",[HL]),
%	printf(F,"%Dvmw",[HL]),
%	printf(F,"%Dmw",[HL]),
        (is_pos(HL) -> Sign = '++' ; Sign = '--'),
	lit_atom(HL,A),
	printf(F,"%w%Dw",[Sign,A]),
	( R \== [] ->
	    printf(F,",%n     ",[])
	  ; true),
	write_tptp_litlist(F,R).


%%%%%%%%% thsetheo  %%%%%%%%%%%%%%%%%%%%%%%%%%
write_thsetheo_file(F) :-
	retract(oi_clause(C,Type)),
	(skip_clause(F,C,Type) ->
	    true
	;  split_clause(C,Pos,Neg,_PosVars,_SV),
	   write_thsetheo_clause(F,Pos,Neg)),
	write_thsetheo_file(F).
write_thsetheo_file(F) :-
	write_thsetheo_file_inf_rules(F).
write_thsetheo_file_inf_rules(F) :-
	retract(inf_rule(Prem,Concl)),
	write_thsetheo_inf_rule(F, Prem, Concl),
	write_thsetheo_file_inf_rules(F).
write_thsetheo_file_inf_rules(_F).

write_thsetheo_clause(F,[],Neg) :-
	printf(F,"?- ",[]),
	write_clean_litlist(F,',',Neg),
	printf(F,".\n",[]), !.
write_thsetheo_clause(F,Pos,[]) :-
	write_clean_litlist(F,';',Pos),
	printf(F,".\n",[]), !.
write_thsetheo_clause(F,Pos,Neg) :-
	write_clean_litlist(F,';',Pos),
	printf(F," <-%n%t ",[]),
	write_clean_litlist(F,',',Neg),
	printf(F,".\n",[]), !.

write_thsetheo_inf_rule(F, Prem, Concl) :-
	write_clean_litlist(F,',',Prem),
	printf(F," ->%t ",[]),
	write_clean_litlist(F,',',Concl),
	printf(F,".\n",[]), !.


%%%%%%%%% satchmo  %%%%%%%%%%%%%%%%%%%%%%%%%%
write_satchmo_file(F) :-
	retract(oi_clause(C,Type)),
	(skip_clause(F,C,Type) ->
	    true
	;  split_clause(C,Pos,Neg,PosVars,_SV),
	   make_dom_list(PosVars,DomList),
	   append(DomList,Neg,Body),
	   write_satchmo_clause(F,Pos,Body)),
	write_satchmo_file(F).
write_satchmo_file(_F).

write_satchmo_header(F) :-
	printf(F,":- dynamic dom/1",[]),
	fail.
write_satchmo_header(F) :-
	retract(pred_sym(P/N)),
	((P == (=)) -> 
	    printf(F,", %Dw/%Dw",[equal,N])
	  ; printf(F,", %Dw/%Dw",[P,N])),
	fail.
write_satchmo_header(F) :-
	printf(F,".\n\n",[]).

write_satchmo_footer(F) :-
	retract(constant(C)),
	printf(F,"dom(%Dw).\n",[C]),
	fail.
write_satchmo_footer(F) :-
	retract(template(T)),
	T =.. [_Fun|Vars],
	make_dom_list(Vars,DomList),
	write_satchmo_clause(F,[dom(T)],DomList),
	fail.
write_satchmo_footer(_F).


write_satchmo_clause(F,[],Neg) :-
	printf(F,"false <- ",[]),
	write_clean_litlist(F,',',Neg),
	printf(F,".\n",[]), !.
write_satchmo_clause(F,Pos,[]) :-
	write_clean_litlist(F,';',Pos),
	printf(F," <- true.\n",[]), !.
write_satchmo_clause(F,Pos,Neg) :-
	write_clean_litlist(F,';',Pos),
	printf(F," <- ",[]),
	write_clean_litlist(F,',',Neg),
	printf(F,".\n",[]), !.

make_eq_subst([V],V=W,[W]) :- !.
make_eq_subst([V|R],V=W,[W|R]).
make_eq_subst([U|R],V=W,[U|RRes]) :-
	make_eq_subst(R,V=W,RRes).

make_eq_subst_sorted([V:S],V:S=W:S,[W:S]) :- !.
make_eq_subst_sorted([V:S|R],V:S=W:S,[W:S|R]).
make_eq_subst_sorted([U:S|R],V:T=W:T,[U:S|RRes]) :-
	make_eq_subst_sorted(R,V:T=W:T,RRes).

all_eq_subst(Var_Vector,Res) :-
	bagof([Var_Vector,R,U],make_eq_subst(Var_Vector,R,U),Res), !.

all_eq_subst_sorted(Var_Vector,Res) :-
	bagof([Var_Vector,R,U],make_eq_subst_sorted(Var_Vector,R,U),Res), !.

write_all_p_subst(_F,_PredSym,[]) :- !.
write_all_p_subst(F,PredSym,[[Args,X=Y,ModArgs]|R]) :-
	HeadLit =.. [PredSym|Args],
	BodyLit =.. [PredSym|ModArgs],
        printf(F,"%% NAME: equality(=)\n",[]),
	write_tme_clause(F,[HeadLit],[Y=X,BodyLit],input),
	write_all_p_subst(F,PredSym,R), !.

write_all_f_subst(_F,_FSym,[]) :- !.
write_all_f_subst(F,FSym,[[Args,X=Y,ModArgs]|R]) :-
	HeadTermLeft =.. [FSym|Args],
	HeadTermRight =.. [FSym|ModArgs],
	printf(F,"%% NAME: equality(=)\n",[]),
	write_tme_clause(F,[HeadTermLeft=HeadTermRight],[X=Y],input),
	write_all_f_subst(F,FSym,R), !.

%% a sorted version
write_all_f_subst_sorted(_F,_FSym,[]) :- !.
write_all_f_subst_sorted(F,FSym,[[Args,X:ArgS=Y:ArgS,ModArgs]|R]) :-
	(fun_sym_sort(FSym:S) ; S = S),
	HeadTermLeft =.. [FSym|Args],
	HeadTermRight =.. [FSym|ModArgs],
	printf(F,"%% NAME: equality(=)\n",[]),
	write_tme_clause(F,
	%% don't supply function substitution axiom's head with result sorts,
	%% so that scan-it has a chance to identify these axioms:
	%% no - do it:
	       [HeadTermLeft:S=HeadTermRight:S],
	%%       [HeadTermLeft=HeadTermRight],
	       [X:ArgS=Y:ArgS],
	input), 
	write_all_f_subst_sorted(F,FSym,R), !.


%%%%%%%%% restart model elimination - rme    %%%%%%%%%%%%%%%%%%%%%%%%%%

write_rme_file(F) :-
	retract(oi_clause(C,Type)),
	( Type == skip -> true
	; Type == comment ->
	    skip_clause(F,C,comment) %% Keep the comments
	; split_clause(C,Pos,Neg,_PosVars,_SV),
          convert_to_rme_clause(Pos,Neg,NewPos,NewNeg),
	  write_tme_clause(F,NewPos,NewNeg,input)),
	write_rme_file(F).
write_rme_file(_F).


convert_to_rme_clause([],Neg,[restart(Anc)],Body) :-
	add_anc(Neg,Anc,Body).
convert_to_rme_clause([SelHead|RestHead],Neg,[SelHeadAnc],Body) :-
	add_anc(SelHead,Anc,SelHeadAnc),
	make_restart(RestHead,Anc,RestHeadRestart),
	add_anc(Neg,Anc,NegAnc),
	append(RestHeadRestart,NegAnc,Body).

make_restart([],_A,[]).
make_restart([FirstHead|RestHead],A,
	[restart(cons(FirstHead,A))|RestHeadRestart]) :-
	make_restart(RestHead,A,RestHeadRestart).


add_anc([],_A,[]).
add_anc([F|R],A,[AF|AR]) :-
	add_anc(F,A,AF),
	add_anc(R,A,AR).
add_anc(Atom,A,AtomA) :-
	Atom =.. [Functor|Args],
	append(Args,[A],ArgsA),
	AtomA =.. [Functor|ArgsA].
	

write_rme_footer(F) :-
	%% first write member predicate
	printf(F,"member(X,cons(X,R)).\n",[]),
	printf(F,"member(X,cons(H,R)) :- \n\tmember(X,R).\n",[]),
	%% our single goal clause:
	printf(F,"false :- restart(nil).\n",[]).

write_rme_header(F) :-
	%%% write the reduction steps:
	retract(pred_sym(P/N)),
	template_args(N,Args),
	Atom =.. [P|Args],
	append(Args,[Anc],ArgsAnc),
	AtomAnc =.. [P|ArgsAnc],
	printf(F,"%w :- member(%w,%w).\n", [AtomAnc,Atom,Anc]), 
        fail.
write_rme_header(_F).


%%%%%%%%% tmeeqax    %%%%%%%%%%%%%%%%%%%%%%%%%%
write_tmeeqax_header(F) :-
	%% first write equivalence relation
	printf(F,"%% Equality Axioms added.\n\n",[]),
	printf(F,"%% Equivalence Axioms:\n",[]),
	printf(F,"%% BEGIN_AXIOMS\n\n",[]),
        (sorted ->
	    printf(F,"%% NAME: equality(=)\n",[]),
	    write_tme_clause(F,[X:S=X:S],[],input),
	    printf(F,"%% NAME: equality(=)\n",[]),
	    write_tme_clause(F,[X:S=Y:S],[Y:S=X:S],input),
	    printf(F,"%% NAME: equality(=)\n",[]),
	    write_tme_clause(F,[X:S=Z:S],[(X:S=Y:S),(Y:S=Z:S)],input)
	;
	    printf(F,"%% NAME: equality(=)\n",[]),
	    write_tme_clause(F,[X=X],[],input),
	    printf(F,"%% NAME: equality(=)\n",[]),
	    write_tme_clause(F,[X=Y],[Y=X],input),
	    printf(F,"%% NAME: equality(=)\n",[]),
	    write_tme_clause(F,[X=Z],[(X=Y),(Y=Z)],input)),
	fail.

write_tmeeqax_header(F) :-
	%% make predicate substitution axioms:
	retract(pred_sym(P/N)),
	(sorted -> sorted_template_args(N,VArgs) ; template_args(N,VArgs)),
	(\+ (P == ('=')) ->
	    printf(F,"\n%% Predicate substitution axiom for %w:\n",[P]),
	    (sorted ->
		all_eq_subst_sorted(VArgs,Triples)
	    ;   all_eq_subst(VArgs,Triples)),
	    write_all_p_subst(F,P,Triples),
	    fail).
write_tmeeqax_header(F) :-
	%% make function substitution axioms:
	retract(fun_sym(Fun/N)),
	printf(F,"\n%% Function substitution axiom for %w:\n",[Fun]),
	(sorted -> 
	    sorted_template_args(N,VArgs),
	    all_eq_subst_sorted(VArgs,Triples),
	    write_all_f_subst_sorted(F,Fun,Triples)
	;   template_args(N,VArgs),
	    all_eq_subst(VArgs,Triples),
	    write_all_f_subst(F,Fun,Triples)),
	fail.
	
write_tmeeqax_header(F) :-
	printf(F,"\n%% END_AXIOMS\n",[]).


%%%%%%%%% tmedomain  %%%%%%%%%%%%%%%%%%%%%%%%%%
write_tmedomain_file(F) :-
	retract(oi_clause(C,Type)),
	(skip_clause(F,C,Type) ->
	    true
	; split_clause(C,Pos,Neg,PosVars,_SV),
	  make_dom_list(PosVars,DomList),
	  append(DomList,Neg,Body),
	  write_tme_clause(F,Pos,Body,Type)),
	write_tmedomain_file(F).
write_tmedomain_file(_F).

write_tmedomain_header(F) :-
	retract(constant(C)),
	printf(F,"dom(%Dw).\n",[C]),
	fail.
write_tmedomain_header(F) :-
	retract(template(T)),
	T =.. [_Fun|Vars],
	make_dom_list(Vars,DomList),
	write_tme_clause(F,[dom(T)],DomList,input),
	fail.
write_tmedomain_header(_F).

make_dom_list([],[]).
make_dom_list([F|R],[dom(F)|DR]) :-
	make_dom_list(R,DR).
	
%% write_tme_clause moved to misc.pl	

%% tmerenamed
write_tmerenamed_file(F) :-
	retract(oi_clause(C,Type)),
	(skip_clause(F,C,Type) ->
	    true
	; split_clause(C,Pos,Neg,_PosVars,_SV),
	  write_tme_clause(F,Pos,Neg,Type)),
	write_tmerenamed_file(F).
write_tmerenamed_file(_F).

write_tmerenamed_header(F) :-
	(constant(C1) ; template(C1)),
	(constant(C2) ; template(C2)),
	\+ C1 = C2,
	printf(F,"unequal(%Dw,%Dw).\n",[C1,C2]),
	fail.
write_tmerenamed_header(F) :-
	pconstant(P1),
	ptemplate(P2),
	printf(F,"unequal(%Dw,%Dw).\n",[P1,P2]),
	printf(F,"unequal(%Dw,%Dw).\n",[P2,P1]),
	fail.

write_tmerenamed_header(F) :-
	ptemplate(P1),
	ptemplate(P2),
	\+ P1 = P2,
	printf(F,"unequal(%Dw,%Dw).\n",[P1,P2]),
	fail.

write_tmerenamed_header(F) :-
	retract(template(T)),
	T =.. [Fun|Vars],
	copy_term(Vars,CVars),
	TC =.. [Fun|CVars],
	make_neq_list(Vars,CVars,NeqList),
	write_neq_clauses(F,T,TC,NeqList),
	fail.

write_tmerenamed_header(F) :-
	retract(ptemplate(T)),
	T =.. [Fun|Vars],
	copy_term(Vars,CVars),
	TC =.. [Fun|CVars],
	make_neq_list(Vars,CVars,NeqList),
	write_neq_clauses(F,T,TC,NeqList),
	fail.

write_tmerenamed_header(_F) :-
	true.
	%printf(F,"neq(X,Y) :- neq(Y,X).\n",[]).

write_neq_clauses(_F,_T1,_T2,[]).
write_neq_clauses(F,T1,T2,[Neq|R]) :-
	printf(F,"unequal(%VDw,%VDw) :- %VDw.\n",[T1,T2,Neq]),
	write_neq_clauses(F,T1,T2,R).
	


make_neq_list([],[],[]).
make_neq_list([V1|R1],[V2|R2],[unequal(V1,V2)|NeqR]) :-
	make_neq_list(R1,R2,NeqR).
	

	
%% write_tme_clause moved to misc.pl	


%%%%%%%%% Otter %%%%%%%%%%%%%%%%%%%%%%%%%%

write_otter_file(F) :-
	retract(oi_clause(C,Type)),
	(skip_clause(F,C,Type) ->
	    true
	;  write_otter_clause(F,C)),
	write_otter_file(F).
write_otter_file(_F).

write_otter_clause(F,[]) :-
	printf(F,".\n",[]).
write_otter_clause(F,[L|R]) :-
	write_clean_lit_cons(F,L),
	( R \== [] ->
	    printf(F," | ",[])
	  ; true),
	write_otter_clause(F,R).


%write_otter_lit(F,L) :-
%	colon_to_s(L,LT),
%%	printf(F,"%DVw",[L]).
%	printf(F,"%w",[LT]).

%%%%%%%%% Ores  %%%%%%%%%%%%%%%%%%%%%%%%%%

write_ores_file(F) :-
	retract(oi_clause(C, Type)),
	(skip_clause(F,C,Type) ->
	    true
	;  printf(F, "{ ",[]),
	   write_ores_clause(F, C)),
	write_ores_file(F).
write_ores_file(_F).

write_ores_clause(F,[]) :-
	printf(F, "}\n",[]).
write_ores_clause(F,[L|R]) :-
	write_ores_literal(F,L),
	printf(F, " ", []),
	write_ores_clause(F,R).

write_ores_literal(F,~A) :- !,
	printf(F, "-",[]),
	write_ores_term(F, A).
write_ores_literal(F,-A) :- !,
	printf(F, "-",[]),
	write_ores_term(F, A).
write_ores_literal(F,A) :-
	printf(F, "+",[]),
	write_ores_term(F, A).

write_ores_term(F,X) :- var(X),  !,
	printf(F, "?%Dw", [X]).
write_ores_term(F,X) :- atom(X), !,
	printf(F, "%Dw", [X]).
write_ores_term(F,X) :- compound(X), !,
	X =.. [G|A],
	(G = (=) -> printf(F, "equal(", []) ; printf(F, "%w(", [G])),
        write_ores_args(F,A).
write_ores_term(F,X) :-
	printf(F, "|%Dw|", [X]).

write_ores_args(F,[A]) :- !,
	write_ores_term(F, A),
	printf(F, ")", []).
write_ores_args(F,[A|R]) :-
	write_ores_term(F,A),
	printf(F, " ", []),
	write_ores_args(F, R).

%%%%%%%%% semantics  %%%%%%%%%%%%%%%%%%%%%%%%%%

write_semantics_file(F) :-
	retract(oi_clause(C, Type)),
	(skip_clause(F,C,Type) ->
	    true
	;  printf(F, "(",[]),
	write_semantics_clause(F, C)),
	write_semantics_file(F).
write_semantics_file(_F).

write_semantics_clause(F,[]) :-
	printf(F, ")\n",[]).
write_semantics_clause(F,[L|R]) :-
	write_semantics_literal(F,L),
	printf(F, " ", []),
	write_semantics_clause(F,R).

write_semantics_literal(F,~A) :- !,
	printf(F, "(-  ",[]),
	write_semantics_literal1(F, A).
write_semantics_literal(F,-A) :- !,
	printf(F, "(-  ",[]),
	write_semantics_literal1(F, A).
write_semantics_literal(F,A) :-
	printf(F, "(+  ",[]),
	write_semantics_literal1(F, A).

write_semantics_literal1(F, A) :-
	(atom(A)   ->
	   printf(F,"%w)",[A])
       ;   A =.. AL,
	   write_semantics_args(F,AL)).



write_semantics_term(F,X) :- var(X),  !,
	printf(F, "(%Dw . 0)", [X]).
write_semantics_term(F,X) :- string(X), !,
	printf(F, "\"%Dw\"", [X]).
write_semantics_term(F,X) :- atomic(X), !,
	printf(F, "%Dw", [X]).
write_semantics_term(F,X) :- compound(X), !,
	X =.. [G|A],
	(G = (=) -> printf(F, "(= ", []) ; printf(F, "(%w ", [G])),
        write_semantics_args(F,A).
%write_semantics_term(F,X) :-
%	printf(F, "|%Dw|", [X]).

write_semantics_args(F,[A]) :- !,
	write_semantics_term(F, A),
	printf(F, ")", []).
write_semantics_args(F,[A|R]) :-
	write_semantics_term(F,A),
	printf(F, " ", []),
	write_semantics_args(F, R).

%%%%%%%%% Spass %%%%%%%%%%%%%%%%%%%%%%%%%%

%% Should be improved by putting all negative unary literals with variable arguments, ie -P(X) 
%% into the sort part.

write_spass_file(F) :-
	retract(oi_clause(C,Type)),
	(skip_clause(F,C,Type) ->
	    true
	;  split_clause(C,Pos,Neg,_PosVars,_SV),
	   printf(F,"|| ",[]),
	   write_spass_term_list(F,Neg),
	   printf(F," -> ",[]),
	   write_spass_term_list(F,Pos),
	   printf(F,".\n",[])),
	write_spass_file(F).
write_spass_file(_F).

write_spass_term_list(_F,[]).
write_spass_term_list(F,[L|R]) :-
	write_spass_term(F,L),
	( R \== [] ->
	    printf(F," ",[])
	  ; true),
	write_spass_term_list(F,R).

write_spass_term(F,T) :-
	var(T),
	printf(F,"%Dw",[T]).
%write_spass_term(F,T) :-
%	T =.. [Constant],
%	printf(F,Constant,[]).
write_spass_term(F,T) :-
	T =.. [Functor|Args],
	printf(F,"(%Dw", [Functor]),
	( Args \== [] ->
	    printf(F," ",[])
	  ; true),
	write_spass_term_list(F,Args),
	printf(F,")", []).


write_tf_file(F) :-
	component(C1), component(C2), C1 @< C2,
	component(C3), C2 @< C3,
	printf(F,"false :- ab(%w), ab(%w), ab(%w).\n",[C1,C2,C3]),
	fail.

write_sf_file(F) :-
%	retract(component(C1)), 
	component(C1),
	component(C2), C1 @< C2, 
%	retract(component(C2)), 
	printf(F,"false :- ab(%w), ab(%w).\n",[C1,C2]),
	fail.

%%%%%%%%% Schwerpunkt %%%%%%%%%%%%%%%%%%%%%%%%%%

write_sp_header(F) :-
	printf(F,"\nlist_of_symbols.\n",[]),
	printf(F,"functions([",[]),
	fail.

write_sp_header(F) :-
	retract(constant(C)),
	printf(F,"(%w,%w)",[C,0]),
	((constant(_C) ; fun_sym(_F/_N)) -> printf(F,",",[]) ; true),
	fail.

write_sp_header(F) :-
	retract(fun_sym(Fun/N)),
	printf(F,"(%w,%w)",[Fun,N]),
	(fun_sym(_Fun/_N) -> printf(F,",",[]) ; true),
	fail.

write_sp_header(F) :-
	printf(F,"]).\n",[]),
	printf(F,"predicates([",[]),
	fail.

write_sp_header(F) :-
	retract(pred_sym(P/N)),
	printf(F,"(%w,%w)",[P,N]),
	(pred_sym(_P/_N) -> printf(F,",",[]) ; true),
	fail.
write_sp_header(F) :-
	printf(F,"]).\n",[]),
	printf(F,"end_of_list.\n\n",[]),
	fail.

write_sp_header(F) :-
	printf(F,"list_of_declarations.\n",[]),
	printf(F,"end_of_list.\n\n",[]).

write_sp_file(F) :-
	retract(oi_clause(C,Type)),
	(skip_clause(F,C,Type) ->
	    true
	;  split_clause(C,Pos,Neg,_PosVars,_SV),
	   term_variables(C,Vars),
	   (Vars == [] ->
	       printf(F,"formula(",[])
	   ;   printf(F,"formula(forall(%w,",[Vars])
           ),
	   ( Pos == [] ->
	       printf(F,"impl(",[]),
	       write_sp_nop_list(F,'and',Neg),
	       printf(F,",false)",[])
	   ; Neg == [] ->
	       write_sp_nop_list(F,'or',Pos)
	   ; true ->
	       printf(F,"impl(",[]),
	       write_sp_nop_list(F,'and',Neg),
	       printf(F,",",[]),
	       write_sp_nop_list(F,'or',Pos),
	       printf(F,")",[])
	   ),
	   (Vars == [] ->
	       printf(F,").\n",[])   % formula
	   ;   printf(F,")).\n",[])
           )),
	write_sp_file(F).
write_sp_file(_F).

write_sp_nop_list(F,_NOp,[Atom]) :- !,
	write_clean_litlist_cons(F,',',[Atom]).
write_sp_nop_list(F,NOp,List) :-
	printf(F,"%w(",[NOp]),
	write_clean_litlist_cons(F,',',List),
	printf(F,")",[]).

%write_sp_term_list(_F,[]).
%write_sp_term_list(F,[L|R]) :-
%	write_sp_term(F,L),
%	( R \== [] ->
%	    printf(F," ",[])
%	  ; true),
%	write_sp_term_list(F,R).
%
%write_sp_term(F,T) :-
%	var(T),
%	printf(F,"%Dw",[T]).
%%write_sp_term(F,T) :-
%%	T =.. [Constant],
%%	printf(F,Constant,[]).
%write_sp_term(F,T) :-
%	T =.. [Functor|Args],
%	printf(F,"(%Dw", [Functor]),
%	( Args \== [] ->
%	    printf(F," ",[])
%	  ; true),
%	write_sp_term_list(F,Args),
%	printf(F,")", []).

%%% misc

%% colon_to_s: convert "colon notation" for sort annotation, e.g. peter : [animal,plant]
%% into an equivalent syntactic form needed for some provers. The sorted peter would yield
%% sort(peter,mycons(animal,mycons(plant,nil))).


colon_to_s(X,X) :- var(X), !.
colon_to_s(T:S,sort(TN,S)) :- 
	colon_to_s(T,TN).
colon_to_s(T,TN) :-
	T =.. [F|A],
	colon_to_s_(A,TA),
	TN =.. [F|TA].

colon_to_s_([],[]).
colon_to_s_([F|R],[TF|TR]) :-
	colon_to_s(F,TF),
	colon_to_s_(R,TR).

%Also converts from '=' infix to 'equal'
write_clean_lit(F,(S = T)) :-
	write_clean_lit(F, equal(S , T)), !.
write_clean_lit(F,-(S = T)) :-
	write_clean_lit(F, -equal(S , T)), !.
write_clean_lit(F,~(S = T)) :-
	write_clean_lit(F, -equal(S , T)), !.

write_clean_lit(F,-Lit) :-
	colon_to_s(Lit,CleanLit),
	getval(negation_sign,N),
	printf(F,"%w%Dw",[N,CleanLit]).
write_clean_lit(F,~Lit) :-
	colon_to_s(Lit,CleanLit),
	getval(negation_sign,N),
	printf(F,"%w%Dw",[N,CleanLit]).
%% else positive literal:
write_clean_lit(F,Lit) :-
	colon_to_s(Lit,CleanLit),
	printf(F,"%Dw",[CleanLit]).
	
write_clean_litlist(_F,_Sep,[]).
write_clean_litlist(F,Sep,[L|R]) :-
	write_clean_lit(F,L),
	( R \== [] ->
	    printf(F,"%w%n%t",[Sep])
	  ; true),
	write_clean_litlist(F,Sep,R).


%% same, but also resolve [.|.] lists into cons(.,.) expressions
colon_to_s_cons(X,X) :- var(X), !.
colon_to_s_cons(T:S,sort(TN,SN)) :- 
        pl_to_cons(S,SN),
	colon_to_s_cons(T,TN).
colon_to_s_cons(T,TN) :-
	T =.. [F|A],
	colon_to_s_cons_(A,TA),
	TN =.. [F|TA].

colon_to_s_cons_([],[]).
colon_to_s_cons_([F|R],[TF|TR]) :-
	colon_to_s_cons(F,TF),
	colon_to_s_cons_(R,TR).



pl_to_cons(X,X) :- var(X), !.
pl_to_cons([],nil).
pl_to_cons([T|S],mycons(TN,SN)) :- 
       pl_to_cons(T,TN), 
       pl_to_cons(S,SN).
pl_to_cons(Term,ResTerm) :-
	Term =.. [F|Args],
	pl_to_cons_(Args,ResArgs),
	ResTerm =.. [F|ResArgs].
	
pl_to_cons_([],[]).
pl_to_cons_([F|R],[TF|TR]) :-
	pl_to_cons(F,TF),
	pl_to_cons_(R,TR).
	

write_clean_lit_cons(F,Lit) :-
	colon_to_s_cons(Lit,CleanLit),
	printf(F,"%Dw",[CleanLit]).
	
write_clean_litlist_cons(_F,_Sep,[]).
write_clean_litlist_cons(F,Sep,[L|R]) :-
	write_clean_lit_cons(F,L),
	( R \== [] ->
	    printf(F,"%w%n%t",[Sep])
	  ; true),
	write_clean_litlist_cons(F,Sep,R).
	
make_tme2X_exec :-
    save( 'tme2X'),
    (argc( 4) ->
       argv( 1, FormatString),
       atom_string( Format, FormatString),
       argv( 2, InFileString),
       atom_string( InFile, InFileString),
       argv( 3, OutFileString),
       atom_string( OutFile, OutFileString),
       tme2X( Format, InFile, OutFile)
    ;
       true), 
    halt.
