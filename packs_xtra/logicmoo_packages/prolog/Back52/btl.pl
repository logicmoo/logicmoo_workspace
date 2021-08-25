
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                     %
%                           I N I T                                   %
%                                                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% b5init offers predicates for initalizing BACK. Additionally it is 
%% possible to reset only the ABox or only the IBox.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b5init_backinit :-
	backversion('BACK'),
	b5init_tboxinit,
	b5init_iboxinit,
	b5init_aboxinit,
% 	b5sta_init,   -okp-  2.5.  passiert schon in b5init_tboxinit
	b5desc_init,
%	addp(b5desc),
	t5out_info(backinit),
	!.

%%%%%%

b5init_tboxinit :-
	tboxinit.

b5init_iboxinit :-
	i5db_init.

b5init_aboxinit :-
	a5odb_init,
	a5odb_tmp_init,
	a5dep_init,
	a5ind_init.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% It makes no sense to reset only the TBox, because the IBox as well as
%% the ABox depends on the definitions in the TBox.

b5init_only_tboxinit :-
	t5out_warning(only_tbox_init),
	b5init_backinit.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Initalizing the IBox means that the IBox is removed and all Information
%% derived with I-Links in the ABox is reset.

b5init_only_iboxinit :-
	backversion('BACK IBOX'),
	i5db_init,
	a5odb_ibox_init,
	a5dep_ibox_init,
	b5inst_ibox_init,
	b5st_ilink_init,
	a5ind_ibox_init,
	t5cdb_ibox_init,
	t5out_info(iboxinit),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Aboxinit deletes all objects from the ABox. Objects and values, that
%% occur in TBox definitions, were reset into the same state they have, if
%% only the TBox is read in.

b5init_only_aboxinit :-
	backversion('BACK ABOX'),
	t5cdb_used_objects_and_values(Objects,Values),
	a5odb_tmp_init,
	a5odb_abox_init(Objects),
	a5dep_abox_init,
	b5inst_abox_init(Values),
	b5par_abox_init(Objects,Values),
	a5ind_abox_init(Objects),
	t5dm_abox_init(Objects),
	b5sta_set_flag(aboxfilled,false),  % -okp-
	t5out_info(aboxinit),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

backversion(_Component) :-!,nl.

backversion(Component) :-
	write(Component),
	write(' Version '),
	version_number(VersionNumber),
	write(VersionNumber),
	version_date(VersionDate),
	write(' ('),
	write(VersionDate),
	write(')'),
	nl,!.

version_number('5.2').
version_date('September 9, 93').





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			BACK PARSER MODULE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TOP UIF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Shorter, more convenient syntax of BACK Tell/Ask expressions.
% Originally, these clauses should be in a separate file. With operators, three
% problems can arise: 1. The operator is built-in in another prolog dialect and
% can therefore not be redefined. 2. The user uses the same operator with a
% different weight. 3. The user uses the same operator as functor of his own
% clauses, such that the following clauses can not be defined.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:=(Name,Definition) :-
	backtell(Name := Definition).

:<(Name,Definition) :-
	backtell(Name :< Definition).

=>(Concept1,Concept2) :-
	backtell(Concept1 => Concept2).

::(Object,Definition) :-
	backtell(Object :: Definition).

*=(MacroName,MacroDefinition) :-
	backmacro(MacroName *= MacroDefinition).

?<(Term1,Term2) :-
	backask(Term1 ?< Term2).

?:(Object,Term) :-
	backask(Object ?: Term).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% USER INTERFACE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The 'official' BACK V User Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

backinit :-
	b5init_backinit.

backinit(tbox) :-
	b5init_only_tboxinit.

backinit(ibox) :-
	b5init_only_iboxinit.

backinit(abox) :-
	b5init_only_aboxinit.

backinit(macros) :-
	b5st_macro_init.


backtell(Name := Definition) :-
	b5par_tbox_tell(Name,Definition,defined).

backtell(Name :< Definition) :-
	b5par_tbox_tell(Name,Definition,primitive).

backtell(Term1 => Term2) :-
	b5par_iboxtell(Term1,Term2).

backtell(O::T) :-
	(  b5sta_check_flag(aboxfilled,abox) -> 
	       true
	;  b5sta_set_flag(aboxfilled,abox)),
	b5par_abox_tell(O,T).
	

backtell(disjoint(Arg1,Arg2)) :-
	b5par_tbox_tell_disjoint(Arg1,Arg2).

backtell(individual(Arg)) :-
	b5par_tbox_tell_indiv(Arg).

backtell(forget(LHS => RHS)) :-
	!,b5par_ibox_forget(LHS,RHS).

backtell(forget(Arg)) :-
	b5par_abox_forget(Arg).  %% a5jt

backtell(redescribe(Obj :: Term)) :-
        (ground(Obj),!,b5par_abox_redescribe(Obj :: Term);
	    t5out_error(instantiation('The object'))).

backtell(name(UC,NewName)) :-
	b5par_rename(UC,NewName).

backask(Object ?: Term) :-
	b5par_abox_ask(Object,Term,ibox).

backask(Term1 ?< Term2) :-
	b5par_tbox_ask_subsumes(Term2,Term1,ibox).   

backask(subsumes(Term1,Term2)) :-
	b5par_tbox_ask_subsumes(Term1,Term2,ibox).  

backask(incoherent(Term)) :-
	b5par_tbox_ask_incoherent(Term,ibox).

backask(disjoint(Term1,Term2)) :-
	b5par_tbox_ask_disjoint(Term1,Term2,ibox).

backask(equivalent(Term1,Term2)) :-
	b5par_tbox_ask_equi(Term1,Term2,ibox).

backask(Object ?: Term,noibox) :-
	b5par_abox_ask(Object,Term,noibox).

backask(Term1 ?< Term2,noibox) :-
	b5par_tbox_ask_subsumes(Term2,Term1,noibox).

backask(subsumes(Term1,Term2),noibox) :-
	b5par_tbox_ask_subsumes(Term1,Term2,noibox).  

backask(incoherent(Term),noibox) :-
	b5par_tbox_ask_incoherent(Term,noibox).

backask(disjoint(Term1,Term2),noibox) :-
	b5par_tbox_ask_disjoint(Term1,Term2,noibox).

backask(equivalent(Term1,Term2),noibox) :-
	b5par_tbox_ask_equi(Term1,Term2,noibox).


backstate :-
	b5sta_print_all_flags,!.
backstate(Flag = Value) :-
	\+ var(Flag),!,
	(var(Value) ->
	b5sta_user_asks(Flag,Value);
	b5sta_user_sets(Flag,Value)).
backstate(_) :-
	t5out_error(instantiation('Flagnames ')),
	!,fail.
	
backretrieve(RetExp) :-
	b5par_retrieval(RetExp).

backmacro(Macro *= Definition) :-
	b5par_macro_tell(Macro,Definition).

backread(File) :- b5par_backread(File).

backdump :-
	b5par_backdump(_).

backdump(File) :-
	b5par_backdump(File).

backload(File) :-
	b5par_backload(File).

backwrite :-
	b5desc_backwrite.

backwrite(BoxOrFileName) :-
	b5desc_backwrite(BoxOrFileName).

backwrite(FileName, Box) :-
	b5desc_backwrite(FileName, Box).

noibox(T1 ?< T2) :-
	b5par_tbox_ask_subsumes(T2,T1,noibox).

noibox(O ?: T) :-
	b5par_abox_ask(O,T,noibox).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% I/O
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% read, write, load, dump operations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


b5desc_backwrite :-
	write('%% ----------------------------------------------------'), nl,
	write('%%              TBox                                   '), nl,
	write('%% ----------------------------------------------------'), nl,
	nl, b5desc_xboxwrite(tbox),
	write('%% ----------------------------------------------------'), nl,
	write('%%              IBox                                   '), nl,
	write('%% ----------------------------------------------------'), nl,
	nl, b5desc_xboxwrite(ibox),
	write('%% ----------------------------------------------------'), nl,
	write('%%              ABox                                   '), nl,
	write('%% ----------------------------------------------------'), nl,
	nl, b5desc_xboxwrite(abox),
	write('%% ----------------------------------------------------').

b5desc_backwrite(Arg) :-
	((Arg == tbox; Arg == ibox; Arg == abox) ->
	      b5desc_xboxwrite(Arg)
	; tell(Arg),
	  b5desc_backwrite,
	  told).
	 
b5desc_backwrite(FileName, Box) :-
	tell(FileName),
	b5desc_xboxwrite(Box),
	told.

b5desc_xboxwrite(tbox) :-
	b5desc_domain_write,
	!, b5desc_class_write.
b5desc_xboxwrite(ibox) :-
	b5st_get_ilink_bt(_,Term1 => Term2),
	writeq(Term1), write(' => '), writeq(Term2), write(.), nl,
	fail.
b5desc_xboxwrite(abox) :-
	b5st_get_instance_bt(Name, _, _, (UserDef1,_)),
	b5desc_list_to_term(UserDef1, UserDef3),
	writeq(Name), write(' :: '), writeq(UserDef3),
	write(.), nl,
	fail.
b5desc_xboxwrite(_) :- nl.

b5desc_domain_write :-
	b5st_get_domain_bt(Name, _, _, _, UserDef1),
	UserDef1 \== (internal,[]),
	writeq(Name), write(' := '), writeq(UserDef1),
	write(.), nl,
	fail.
b5desc_domain_write :- nl.

b5desc_class_write :-
	b5st_get_class_bt(Name, _, _, UserDef1),
	atom(Name), 
	UserDef1 \== internal,
	UserDef1 \== forward, 
	UserDef1 =.. [Op, L, R],
	write(L), write(' '), write(Op), write(' '), 
	writeq(R), write(.), nl,
	fail.
b5desc_class_write :- nl.





b5par_backread(FileName) :-	
	open(FileName,read,Stream),
	on_exception(E,b5par_read_term(Stream,Flag),(t5out_error((exception,E)),close(Stream),fail)),
	close(Stream),
	!,Flag = success.

b5par_read_term(Stream,Flag) :-
	repeat,
	read_term(Stream,[syntax_errors(error)],T),
	(T == end_of_file,
	!,
	t5out_info(read_successful),
	Flag = success
    ;
	\+ b5par_call_term(T),
	Flag = failure).

b5par_call_term(T) :-
	write(T),nl,
	(call(T),!
	;
	t5out_error(read_stopped),fail).


b5par_backdump(File) :-
	(\+ ground(File) ->
	    b5par_backdump(user_output)
	;
	b5par_dump_to_file(File)).

b5par_dump_to_file(File) :-
	b5par_open_file(write,File,Stream),
	b5par_dump,
	b5par_close_file(Stream).


b5par_dump :-
	b5dump_these_predicates(L),
	b5dump_predicates(L),
	fail.
b5par_dump.

b5dump_predicates([]) :- !.
b5dump_predicates([Pred/Arity|R]) :-
	b5dump_predicate(Pred,Arity),
	b5dump_predicates(R).

b5dump_predicate(Pred,Arity) :-
	functor(Term,Pred,Arity),
	Term,
	writeq(Term),put(0'.),nl,
	fail.
b5dump_predicate(_,_).


b5par_backload(File) :-
	(\+ ground(File) ->
	    t5out_error('Load File must be instantiated')
	;
	b5par_load_from_file(File)).

b5par_load_from_file(File) :-
	b5par_open_file(read,File,Stream),
	b5par_reset_db,
	b5par_load,
	b5par_close_file(Stream).


b5par_reset_db :-
	b5dump_these_predicates(L),
	b5par_reset_predicates(L),
	fail.
b5par_reset_db.

b5par_reset_predicates([]) :- !.
b5par_reset_predicates([Pred/Arity|R]) :-
	b5par_reset_predicate(Pred,Arity),
	b5par_reset_predicates(R).

b5par_reset_predicate(Pred,Arity) :-
	functor(Term,Pred,Arity),
	retractall(Term).

b5par_load :-
	repeat,
	read(Term),
	(Term == end_of_file,!
    ;	
	assertz(Term),
	fail).

b5par_open_file(write,user_output,user_output) :- !.
b5par_open_file(write,File,Stream) :-
	open(File,write,Stream),
	set_output(Stream).
b5par_open_file(read,File,Stream) :-
	open(File,read,Stream),
	set_input(Stream).
b5par_close_file(user_output) :- !.
b5par_close_file(Stream) :-
	close(Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% renaming of uc(i)'s

b5par_rename(uc(I),NewName) :-
	!,
	(\+ b5st_get_instance(NewName,(c,0),_,_) ->
	    (b5st_del_instance(uc(I),(c,0),Key,Def) ->
		b5st_add_instance(NewName,(c,0),Key,Def)
	    ;
	    true),
	    b5par_rename_update_definitions(uc(I),NewName)
	;
	t5out_error(rename(uc(I),NewName))).
b5par_rename(OldName,NewName) :-
	(b5par_uc(OldName,I) ->
	    b5par_rename(uc(I),NewName)
	;
	t5out_error(rename(OldName))).
	

b5par_rename_update_definitions(uc(I),NewName) :-
	b5st_get_class(Name,(c,0),Key,Def),
	\+ Name = prim(_),
	b5par_replace_in_term(uc(I),NewName,Def,NewDef),
	b5par_replace_in_kb(class,Name,(c,0),Key,Def,NewDef),
	fail.
b5par_rename_update_definitions(uc(I),NewName) :-
	b5st_get_class(Name,(r,0),Key,Def),
	\+ Name = prim(_),
	b5par_replace_in_term(uc(I),NewName,Def,NewDef),
	b5par_replace_in_kb(class,Name,(r,0),Key,Def,NewDef),
	fail.
b5par_rename_update_definitions(uc(I),NewName) :-
	b5st_get_instance(Name,(c,0),Key,Def),
	b5par_replace_in_term(uc(I),NewName,Def,NewDef),
	b5par_replace_in_kb(object,Name,(c,0),Key,Def,NewDef),
	fail.
b5par_rename_update_definitions(_,_).	


b5par_replace_in_kb(_,_,_,_,Def,Def) :- !.
b5par_replace_in_kb(object,Name,Type,Key,Def,NewDef) :-
	b5st_del_instance(Name,Type,Key,Def),
	b5st_add_entry(Name,Type,Key,NewDef).
b5par_replace_in_kb(class,Name,Type,Key,Def,NewDef) :-
	b5st_del_class(Name,Type,Key,Def),
	b5st_add_class(Name,Type,Key,NewDef).



b5par_replace_in_term(uc(I),NewName,uc(I),NewName) :- !.
b5par_replace_in_term(uc(I),NewName,Def,NewDef) :-
	functor(Def,Functor,Arity),
	functor(EmptyDef,Functor,Arity),
	b5par_replace_in_term(Arity,uc(I),NewName,EmptyDef,Def,NewDef).

b5par_replace_in_term(0,_,_,NewDef,_,NewDef) :- !.
b5par_replace_in_term(N,uc(I),NewName,EmptyDef,Def,NewDef) :-
	arg(N,Def,Arg),
	b5par_replace_in_term(uc(I),NewName,Arg,NewArg),
	arg(N,EmptyDef,NewArg),
	M is N-1,
	b5par_replace_in_term(M,uc(I),NewName,EmptyDef,Def,NewDef).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TBOX TELL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SWITCH
% There are three types of tboxtells. Either a new class (i.e. concept or role)
% or domain is being introduced, or an existing class is being revised.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b5par_tbox_tell(Name,attribute_domain,PrimDef) :-
	!,b5par_new_open_domain(PrimDef,Name).

b5par_tbox_tell(Name,attribute_domain(Liste),PrimDef) :-
	!,b5par_new_closed_domain(PrimDef,Liste,Name).

b5par_tbox_tell(Name,Definition,PrimDef) :-
	nonvar(Name),
	b5st_get_class(Name,_,_,_),!,
	b5par_revision(Name,Definition,PrimDef).

b5par_tbox_tell(Name,Definition,PrimDef) :-
	b5par_new_class(PrimDef,Name,Definition,[class/Name]),
	( ( b5sta_check_flag(aboxfilled, abox),
	    PrimDef = defined )  ->                               %% -jt-
	         a5rev_new_class(Name,Definition)
	; true ).

b5par_tbox_tell_disjoint(Name1,Name2) :-
	b5st_get_class(Name1,_,_,Name1:<_),
	b5st_get_class(Name2,_,_,Name2:<Def2),
	NewDef = (Name2 :< not(Name1) and Def2),
	backtell(NewDef).

b5par_tbox_tell_indiv(_) :-
	t5out_warning(indiv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NEW CLASS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Classes are introduced either defined or primitive. In the latter case a new
% primitive component has to be created and added to the class description.
% Parsing the class definition yields the class' type and three data 
% structures. First a list of all class names used in the definition, which is
% send to the dependency module. Second a list of concept forming operators, in
% which all occurences of names and nested terms of the original definition
% are replaced by keys. The class definition is a conjunction of the list's 
% elements, which will be send incrementally to the kernel module.
% The third structure is a forward introduction list. It contains all the 
% unknown names and nested terms of the original definition in the proper 
% order, for which a key has to be generated before the definition can be send 
% to the kernel. Many of the keys in the second list are not instantiated, but
% variables corresponding to variables in the forward introduction list.
% Class definitions must be syntactically correct, they may not contain cycles,
% names must be known or not depending on the system's mode, and the definition
% must be semantically correct, that is all names must be of the appropriate
% type (c1 := role and concept is syntactically correct, but not semantically).
% Concrete Concepts must be introduced as defined.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b5par_new_class(defined,Name,Definition,PN) :-
	b5par_valid_name(Name),
	b5par_tt_analyze_definition(Definition,Term,Type,RoleType),
	b5par_correct_role_term(Term),
	b5par_main(Term,normal,Type,PN,[],KeyTerm,[],Names,[],FI),
	b5par_def_type_test(Type),
	t5dm_uses(class/Name,Names),
	b5par_forward_introduction(FI,_),
	t5fil_filter(user_defined,Filter),
	b5par_tt_to_kernel(Type,KeyTerm,RoleType,Filter,Key,_,def),
	b5st_add_class(Name,Type,Key,Name := Definition).

b5par_new_class(primitive,Name,Definition,PN) :-
	b5par_tt_analyze_definition(Definition,Term,Type,RoleType),
	atom(Term),!,
	b5par_valid_name(Name),
	b5par_main(Term,normal,Type,PN,[],KeyTerm,[],Names,[],FI),
	b5par_prim_type_test(Type),
	b5par_create_prim(Type,Name,prim(PrimKey)),
	t5dm_uses(class/Name,Names),
	b5par_forward_introduction(FI,_),
	t5fil_filter(user_primitive,Filter),
	b5par_tt_atomic_prim_to_kernel(Type,PrimKey,KeyTerm,RoleType,Filter,Key),
	b5st_add_class(Name,Type,Key,Name :< Definition).

b5par_new_class(primitive,Name,Definition,PN) :-
	b5par_valid_name(Name),
	\+ b5par_correct(Definition),!,
	b5par_tt_analyze_definition(Definition,Term,(r,0),RoleType),
	b5par_main(Term,normal,(r,0),PN,[],KeyTerm,[],Names,[],FI),
	b5par_create_prim((r,0),Name,prim(PrimKey)),
	t5dm_uses(class/Name,Names),
	b5par_forward_introduction(FI,_),
	t5fil_filter(user_primitive,Filter),
	b5par_tt_domran_role_to_kernel(PrimKey,KeyTerm,RoleType,Filter,Key),
	b5st_add_class(Name,(r,0),Key,Name :< Definition).

b5par_new_class(primitive,Name,Definition,PN) :-
	b5par_valid_name(Name),
	b5par_tt_analyze_definition(Definition,Term,Type,RoleType),
	b5par_main(Term,normal,Type,PN,[],KeyTerm,[],Names,[],FI),
	b5par_prim_type_test(Type),
	b5par_create_prim(Type,Name,PrimKey),
	t5dm_uses(class/Name,Names),
	b5par_forward_introduction(FI,_),
	t5fil_filter(user_primitive,Filter),
	b5par_tt_to_kernel(Type,[PrimKey|KeyTerm],RoleType,Filter,Key,_,prim),
	b5st_add_class(Name,Type,Key,Name :< Definition).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NEW DOMAIN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% New attribute domains can be specified by the user. A Domain is either open
% (i.e. no extension is specified initially) or closed (i.e. all the domains'
% attributes are specified in the domains' definition).
% Domains must be introduced as defined.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b5par_new_open_domain(primitive,_) :-
	!,t5out_error(no_prim_domains),fail.
b5par_new_open_domain(defined,Name) :-
	!,b5par_new_open_domain(Name).
b5par_new_closed_domain(primitive,_,_) :-
	!,t5out_error(no_prim_domains),fail.
b5par_new_closed_domain(defined,Liste,Name) :-
	b5par_new_closed_domain(Name,Liste).


b5par_new_open_domain(Name) :-
	t5fil_filter(user_defined,Filter),
	t5dom_new_open_domain(Filter,PrimDomKey,DomKey),
	b5st_next_domain_counter(N),
	b5st_add_class(prim(Name),(a,N),PrimDomKey,internal),
	b5st_add_class(Name,(a,N),DomKey,internal),
	b5st_add_domain(Name,(a,N),DomKey,open,[]),
	b5st_new_nothing((a,N)). %uk

b5par_new_closed_domain(Name,Liste) :-
	length(Liste,L),
	t5fil_filter(user_defined,Filter),
	t5dom_new_closed_domain(L,Filter,PrimDomKey,DomKey,AttKeyList),
	b5st_next_domain_counter(N),
	b5par_enter_attributes(Liste,AttKeyList,(a,N)),
	b5st_add_class(prim(Name),(a,N),PrimDomKey,internal),
	b5st_add_class(Name,(a,N),DomKey,internal),
	b5st_add_domain(Name,(a,N),DomKey,closed,Liste),
	b5st_new_nothing((a,N)). %uk


b5par_enter_attributes([],[],_) :- !.
b5par_enter_attributes([Name1|Liste],[Key1|AttKeyList],(a,N)) :-
	b5st_add_instance(Name1,(a,N),Key1,no_definition),
	b5par_enter_attributes(Liste,AttKeyList,(a,N)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TBOX REVISION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% All tbox entries can be revised, except for system classes (such as 'nothing')
% and attribute domains. If a class is revised, all classes that used it in
% their own definition must be revised too. This is achieved in a naive way
% by retelling these classes, whereby the order of retells must account for
% further dependencies between the classes themselves. In short, amongst the
% classes that form the transitive closure wrt to dependency of the original
% revised concept, those classes of which all the classes they depend on have 
% already been revised, must be revised first. This proper order is computed 
% by the dependency module.
% Revision must detect and refuse the introduction of cyclic definitions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b5par_revision(_,_,_) :-
	b5sta_check_flag(revision,false),
	!,fail.
b5par_revision(Name,_,_) :-
	b5st_get_domain(Name,_,_,_,_),
	!,fail.
b5par_revision(Name,Definition,PrimDef) :-
	b5par_tbox_rev_dump,	
	b5st_get_class(Name,OldType,KeyOld,_),
	t5dm_trans_used_by(class/Name,Dependents),
	b5st_del_class(Name,_,_,_),
	(b5st_del_class(prim(Name),_,_,_),!;
	    true),
	t5dm_remove(Name),
	(b5par_new_class(PrimDef,Name,Definition,[class/Name|Dependents]),
	b5st_get_class(Name,NewType,KeyNew,_),
	b5par_revision_type_check(OldType,NewType,Type),
	b5par_changed_status(NewType,KeyOld,KeyNew),
	b5par_retell_tbox(Dependents,[],[(Type,Name,KeyOld,KeyNew)]),!;
	b5par_tbox_rev_load).
	
b5par_retell_tbox([],Links,TARev) :-
	!,b5par_retell_ibox(Links,TARev,[],[]).
b5par_retell_tbox([class/Name|Rest],LinksIn,TARev) :-
	!,b5st_get_class(Name,OldType,KeyOld,UserDef),
	b5st_del_class(Name,_,_,_),
	(b5st_del_class(prim(Name),_,_,_),!;
	    true),
	t5dm_remove(Name),
	backtell(UserDef),
	b5st_get_class(Name,NewType,KeyNew,_),
	b5par_revision_type_check(OldType,NewType,Type),
	b5par_changed_status(NewType,KeyOld,KeyNew),
	b5par_retell_tbox(Rest,LinksIn,[(Type,Name,KeyOld,KeyNew)|TARev]).
b5par_retell_tbox([link/N|Rest],Links,TARev) :-
	b5par_retell_tbox(Rest,[link/N|Links],TARev).

b5par_retell_ibox([],TARev,Old,New) :- 
	!,b5par_retell_abox(TARev,Old,New).
b5par_retell_ibox([link/N|R],TARev,Old,New) :-
	i5ibox_del_link(N,OldKey),
	b5st_del_ilink(N,Term1 => Term2),
	b5par_ibox_tell(Term1,Term2,NewKey),
	b5par_retell_ibox(R,TARev,[OldKey|Old],[NewKey|New]).

b5par_retell_abox(TAR,Old,New) :-
	a5rev_update_tbox(TAR,Old,New).


b5par_changed_status((r,0),OldKey,NewKey) :-
	!,t5tbox_changed_status(role,OldKey,NewKey).
b5par_changed_status(_,OldKey,NewKey) :-
	t5tbox_changed_status(conc,OldKey,NewKey).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TBOX PARSER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is the actual parser. It generates the three data structures mentioned
% above, checks for type compatibility, determines the definitions' type and
% is awfully redundant. This redundancy stems from problems with the 
% determination of the type, which can be ambigue, or user defined or implicit.
% FFS: Get rid of this redundancy.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%% main



b5par_main(Input,Mode,Type,PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	nonvar(Input),
	b5par_analyze_term(Input,Mode,Type,PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout),
	!,ground(Type).


%%%%% analyze terms

b5par_analyze_term(X,_,_,_,_,_,_,_,_,_) :-
	var(X),!,t5out_error(instantiation('TBox definitions')),fail.
 
b5par_analyze_term(intersection(Arg1,Arg2),Mode,Type,PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_term(Arg1,Mode,Type,PN,Vartermin,Varterm1,Namesin,Names1,FIin,FI1),
	b5par_analyze_term(Arg2,Mode,Type,PN,Varterm1,Vartermout,Names1,Namesout,FI1,FIout).

b5par_analyze_term(and(Arg1,Arg2),Mode,Type,PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_term(Arg1,Mode,Type,PN,Vartermin,Varterm1,Namesin,Names1,FIin,FI1),
	b5par_analyze_term(Arg2,Mode,Type,PN,Varterm1,Vartermout,Names1,Namesout,FI1,FIout).


b5par_analyze_term(not(Arg),Mode,(c,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	b5par_analyze_concept_term(not(Arg),Mode,(c,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout),!.

b5par_analyze_term(not(Arg),Mode,(r,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,b5par_analyze_role_term(not(Arg),Mode,(r,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(all(Role,Conc),Mode,(c,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_concept_term(all(Role,Conc),Mode,(c,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(atleast(N,Role),Mode,(c,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	(integer(N),!;N == macrodummy),
	b5par_analyze_concept_term(atleast(N,Role),Mode,(c,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(atmost(N,Role),Mode,(c,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	(integer(N),!;N == macrodummy),
	b5par_analyze_concept_term(atmost(N,Role),Mode,(c,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(oneof(ObjectList),Mode,(c,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_concept_term(oneof(ObjectList),Mode,(c,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(Role : FillerList,Mode,(c,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_concept_term(Role : FillerList,Mode,(c,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(rvm_equal(Role1,Role2),Mode,(c,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_concept_term(rvm_equal(Role1,Role2),Mode,(c,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(domain(Conc),Mode,(r,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_role_term(domain(Conc),Mode,(r,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(range(Conc),Mode,(r,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_role_term(range(Conc),Mode,(r,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(inv(Role),Mode,(r,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_role_term(inv(Role),Mode,(r,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(trans(Role),Mode,(r,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_role_term(trans(Role),Mode,(r,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(Role1 comp Role2,Mode,(r,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_role_term(Role1 comp Role2,Mode,(r,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(Role1.Role2,Mode,(r,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_role_term(Role1.Role2,Mode,(r,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(union(Aset1,Aset2),Mode,(a,N),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_aset_term(union(Aset1,Aset2),Mode,(a,N),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(without(Aset1,Aset2),Mode,(a,N),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_aset_term(without(Aset1,Aset2),Mode,(a,N),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(aset(Liste),Mode,(a,N),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_aset_term(aset(Liste),Mode,(a,N),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(aset(Att1 .. Att2,Domain),Mode,(a,N),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_aset_term(aset(Att1 .. Att2,Domain),Mode,(a,N),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(aset(Liste,Domain),Mode,(a,N),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_aset_term(aset(Liste,Domain),Mode,(a,N),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(N1 .. N2,Mode,(n,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_number_term(N1 .. N2,Mode,(n,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(gt(N),Mode,(n,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_number_term(gt(N),Mode,(n,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(ge(N),Mode,(n,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_number_term(ge(N),Mode,(n,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(lt(N),Mode,(n,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_number_term(lt(N),Mode,(n,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(le(N),Mode,(n,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_number_term(le(N),Mode,(n,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(Name,Mode,Type,PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	nonvar(Name),
	b5st_get_macro(Name,Macro),!,
	b5par_analyze_term(Macro,Mode,Type,PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(Name,_,Type,PN,Vartermin,[Key|Vartermin],Namesin,[class/Name|Namesin],FI,FI) :-
	ground(Name),
	b5st_get_class(Name,Type,Key,_),!,
	b5par_cycle_check(clas/sName,PN).

b5par_analyze_term(Name,_,_,_,_,_,_,_,_,_) :-
	ground(Name),
	b5st_get_class(Name,_,_,_),!,fail.

b5par_analyze_term(N,Mode,(n,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	number(N),
	!,
	b5par_analyze_number_term(N,Mode,(n,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_term(_,no_unknown_names,_,_,_,_,_,_,_,_) :-
	!,fail.

%b5par_analyze_term(_,aa_parse,_,_,_,_,_,_,_,_) :-
%	!,fail.

b5par_analyze_term(Name,_,Type,PN,Vartermin,[Key|Vartermin],Namesin,[class/Name|Namesin],FI,[forwardname/Name/Type/_/Key|FI]) :-
	\+ number(Name),
	b5sta_check_flag(introduction,forward),
	b5par_valid_name(Name),
	b5par_cycle_check(class/Name,PN).

%%%%% analyze concepts

b5par_analyze_concept_term(X,_,_,_,_,_,_,_,_,_) :-
	var(X),!,t5out_error(instantiation('TBox concept definitions')),fail.

b5par_analyze_concept_term(and(Arg1,Arg2),Mode,(c,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_concept_term(Arg1,Mode,(c,0),PN,Vartermin,Varterm1,Namesin,Names1,FIin,FI1),
	b5par_analyze_concept_term(Arg2,Mode,(c,0),PN,Varterm1,Vartermout,Names1,Namesout,FI1,FIout).

b5par_analyze_concept_term(atleast(N,Role),Mode,(c,0),PN,Varterm,[atleast(N,Key)|Varterm],Namesin,Namesout,FIin,FIout) :-
	!,
	(integer(N),!;N == macrodummy),
	b5par_correct_role_term(Role),	
	b5par_analyze_role_term(Role,Mode,(r,0),PN,[],RVarterm,Namesin,Namesout,[internal/RVarterm/ (r,0) /Key|FIin],FIout).

b5par_analyze_concept_term(atmost(N,Role),Mode,(c,0),PN,Varterm,[atmost(N,Key)|Varterm],Namesin,Namesout,FIin,FIout) :-
	!,
	(integer(N),!;N == macrodummy),
	b5par_correct_role_term(Role),
	b5par_analyze_role_term(Role,Mode,(r,0),PN,[],RVarterm,Namesin,Namesout,[internal/RVarterm/ (r,0) /Key|FIin],FIout).


b5par_analyze_concept_term(all(Role,Conc),Mode,(c,0),PN,Varterm,[all(Key2,Key1)|Varterm],Namesin,Namesout,FIin,FIout) :-
	b5par_main(Conc,Mode,CType,PN,[],CVarterm,Namesin,Names1,[internal/CVarterm/CType/Key1|FIin],FI1),
	CType \== (r,0),
	!,
	b5par_correct_role_term(Role),
	b5par_analyze_role_term(Role,Mode,(r,0),PN,[],RVarterm,Names1,Namesout,[internal/RVarterm/ (r,0) /Key2|FI1],FIout).

b5par_analyze_concept_term(all(Role,Conc),Mode,(c,0),PN,Varterm,[all(Key2,Key1)|Varterm],Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_concept_term(Conc,Mode,(c,0),PN,[],CVarterm,Namesin,Names1,[internal/CVarterm/ (c,0) /Key1|FIin],FI1),
	b5par_correct_role_term(Role),
	b5par_analyze_role_term(Role,Mode,(r,0),PN,[],RVarterm,Names1,Namesout,[internal/RVarterm/ (r,0) /Key2|FI1],FIout).

b5par_analyze_concept_term(oneof(ObjectList),Mode,(c,0),_,Varterm,[oneof(KeyList)|Varterm],Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_object_list(ObjectList,Mode,[],KeyList,Namesin,Namesout,FIin,FIout).

b5par_analyze_concept_term(Role : close(FillerList),Mode,(c,0),PN,Varterm,[close(Key,KeyList)|Varterm],Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_correct_role_term(Role),
	b5par_analyze_role_term(Role,Mode,(r,0),PN,[],RVarterm,Namesin,Namesout,[internal/RVarterm/ (r,0) /Key|FIin],FI1),
	b5par_analyze_filler_term(FillerList,Mode,Role,KeyList,FI1,FIout).

b5par_analyze_concept_term(Role : FillerList,Mode,(c,0),PN,Varterm,[fills(Key,KeyList)|Varterm],Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_correct_role_term(Role),
	b5par_analyze_role_term(Role,Mode,(r,0),PN,[],RVarterm,Namesin,Namesout,[internal/RVarterm/ (r,0) /Key|FIin],FI1),
	b5par_analyze_filler_term(FillerList,Mode,Role,KeyList,FI1,FIout).

b5par_analyze_concept_term(rvm_equal(Arg1,Arg2),Mode,(c,0),PN,Vartermin,[rvm(RKey1,RKey2)|Vartermin],Namesin,Namesout,FIin,FIout) :-
	!, 
	b5par_determine_role_range(Arg1,RR),
	b5par_determine_role_range(Arg2,RR),
	b5par_correct_role_term(Arg1),
	b5par_correct_role_term(Arg2),
	b5par_analyze_role_term(Arg1,Mode,(r,0),PN,[],Varterm1,Namesin,Names1,[internal/Varterm1/ (r,0) /RKey1|FIin],FI1),
	b5par_analyze_role_term(Arg2,Mode,(r,0),PN,[],Varterm2,Names1,Namesout,[internal/Varterm2/ (r,0) /RKey2|FI1],FIout).


b5par_analyze_concept_term(not(Name),_,(c,0),PN,Varterm,[not(Key)|Varterm],Names,[class/Name|Names],FI,FI) :-
	ground(Name),
	b5st_get_class(prim(Name),(c,0),Key,_),!,
	b5par_cycle_check(class/Name,PN).

b5par_analyze_concept_term(not(Name),_,_,_,_,_,_,_,_,_) :-
	ground(Name),
	b5st_get_class(Name,_,_,_),!,fail.

b5par_analyze_concept_term(not(Name),_,(c,0),PN,Varterm,[not(PrimKey)|Varterm],Names,[class/Name|Names],FI,[forwardname/Name/ (c,0) /PrimKey/_|FI]) :-
	!,b5par_valid_name(Name),
	b5par_cycle_check(class/Name,PN).


b5par_analyze_concept_term(Name,Mode,(c,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	nonvar(Name),
	b5st_get_macro(Name,Macro),!,
	b5par_analyze_concept_term(Macro,Mode,(c,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).


b5par_analyze_concept_term(Name,_,(c,0),PN,Varterm,[Key|Varterm],Names,[class/Name|Names],FI,FI) :-
	ground(Name),
	b5st_get_class(Name,(c,0),Key,_),!,
	b5par_cycle_check(class/Name,PN).

b5par_analyze_concept_term(Name,_,_,_,_,_,_,_,_,_) :-
	ground(Name),
	b5st_get_class(Name,_,_,_),!,fail.

b5par_analyze_concept_term(_,no_unknown_names,_,_,_,_,_,_,_,_) :-
	!,fail.

b5par_analyze_concept_term(Name,_,(c,0),PN,Varterm,[Key|Varterm],Names,[class/Name|Names],FI,[forwardname/Name/ (c,0) /_/Key|FI]) :-
	b5sta_check_flag(introduction,forward),
	b5par_valid_name(Name),
	b5par_cycle_check(class/Name,PN).


%%%%% analyze roles

b5par_analyze_role_term(X,_,_,_,_,_,_,_,_,_) :-
	var(X),!,t5out_error(instantiation('TBox role definitions')),fail.

b5par_analyze_role_term(and(Arg1,Arg2),Mode,(r,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_role_term(Arg1,Mode,(r,0),PN,Vartermin,Varterm1,Namesin,Names1,FIin,FI1),
	b5par_analyze_role_term(Arg2,Mode,(r,0),PN,Varterm1,Vartermout,Names1,Namesout,FI1,FIout).

b5par_analyze_role_term(Role1 comp Role2,Mode,(r,0),PN,Vartermin,[comp(Key1,Key2)|Vartermin],Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_correct_role_term(Role1),
	b5par_correct_role_term(Role2),
	b5par_analyze_role_term(Role1,Mode,(r,0),PN,[],RVarterm1,Namesin,Names1,[internal/RVarterm1/ (r,0) /Key1|FIin],FI1),
	b5par_analyze_role_term(Role2,Mode,(r,0),PN,[],RVarterm2,Names1,Namesout,[internal/RVarterm2/ (r,0) /Key2|FI1],FIout).

b5par_analyze_role_term(Role1.Role2,Mode,(r,0),PN,Vartermin,[comp(Key1,Key2)|Vartermin],Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_correct_role_term(Role1),
	b5par_correct_role_term(Role2),
	b5par_analyze_role_term(Role1,Mode,(r,0),PN,[],RVarterm1,Namesin,Names1,[internal/RVarterm1/ (r,0) /Key1|FIin],FI1),
	b5par_analyze_role_term(Role2,Mode,(r,0),PN,[],RVarterm2,Names1,Namesout,[internal/RVarterm2/ (r,0) /Key2|FI1],FIout).

b5par_analyze_role_term(domain(Conc),Mode,(r,0),PN,Varterm,[domain(Key)|Varterm],Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_concept_term(Conc,Mode,(c,0),PN,[],CVarterm,Namesin,Namesout,[internal/CVarterm/ (c,0) /Key|FIin],FIout).

b5par_analyze_role_term(inv(Role),Mode,(r,0),PN,Varterm,[inv(Key)|Varterm],Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_correct_role_term(Role),
	b5par_analyze_role_term(Role,Mode,(r,0),PN,[],RVarterm,Namesin,Namesout,[internal/RVarterm/ (r,0) /Key|FIin],FIout).

b5par_analyze_role_term(trans(Role),Mode,(r,0),PN,Varterm,[trans(Key)|Varterm],Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_correct_role_term(Role),
	b5par_analyze_role_term(Role,Mode,(r,0),PN,[],RVarterm,Namesin,Namesout,[internal/RVarterm/ (r,0) /Key|FIin],FIout).

b5par_analyze_role_term(range(Conc),Mode,(r,0),PN,Varterm,[range(Key)|Varterm],Namesin,Namesout,FIin,FIout) :-
	b5par_main(Conc,Mode,Type,PN,[],CVarterm,Namesin,Namesout,[internal/CVarterm/Type/Key|FIin],FIout),
	Type \== (r,0),!.

b5par_analyze_role_term(range(Conc),Mode,(r,0),PN,Varterm,[range(Key)|Varterm],Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_concept_term(Conc,Mode,(c,0),PN,[],CVarterm,Namesin,Namesout,[internal/CVarterm/ (c,0) /Key|FIin],FIout).

b5par_analyze_role_term(not(Name),_,(r,0),PN,Varterm,[not(Key)|Varterm],Names,[class/Name|Names],FI,FI) :-
	ground(Name),
	b5st_get_class(prim(Name),(r,0),Key,_),!,
	b5par_cycle_check(class/Name,PN).

b5par_analyze_role_term(not(Name),_,_,_,_,_,_,_,_,_) :-
	ground(Name),
	b5st_get_class(Name,_,_,_),!,fail.

b5par_analyze_role_term(not(Name),_,(r,0),PN,Varterm,[not(PrimKey)|Varterm],Names,[class/Name|Names],FI,[forwardname/Name/ (r,0) /PrimKey/_|FI]) :-
	!,
	b5par_valid_name(Name),
	b5par_cycle_check(class/Name,PN).

b5par_analyze_role_term(Name,Mode,(r,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	nonvar(Name),
	b5st_get_macro(Name,Macro),!,
	b5par_analyze_role_term(Macro,Mode,(r,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).


b5par_analyze_role_term(Name,_,(r,0),PN,Varterm,[Key|Varterm],Names,[class/Name|Names],FI,FI) :-
	ground(Name),
	b5st_get_class(Name,(r,0),Key,_),!,
	b5par_cycle_check(class/Name,PN).

b5par_analyze_role_term(Name,_,_,_,_,_,_,_,_,_) :-
	ground(Name),
	b5st_get_class(Name,_,_,_),!,fail.

b5par_analyze_role_term(_,no_unknown_names,_,_,_,_,_,_,_,_) :-
	!,fail.

b5par_analyze_role_term(Name,_,(r,0),PN,Varterm,[Key|Varterm],Names,[class/Name|Names],FI,[forwardname/Name/ (r,0) /_/Key|FI]) :-
	b5sta_check_flag(introduction,forward),
	b5par_valid_name(Name),
	b5par_cycle_check(class/Name,PN).


%%%%% analyze numbers

b5par_analyze_number_term(X,_,_,_,_,_,_,_,_,_) :-
	var(X),!,t5out_error(instantiation('TBox number definitions')),fail.

b5par_analyze_number_term(intersection(Arg1,Arg2),Mode,(n,0),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_number_term(Arg1,Mode,(n,0),PN,Vartermin,Varterm1,Namesin,Names1,FIin,FI1),
	b5par_analyze_number_term(Arg2,Mode,(n,0),PN,Varterm1,Vartermout,Names1,Namesout,FI1,FIout).

b5par_analyze_number_term(Num1 .. Num2,_,(n,0),_,Vartermin,[fromto(num(Num1),num(Num2))|Vartermin],Namesin,Namesin,FIin,FIin) :-
	!,
	(number(Num1),!;Num1 == macrodummy),
	(number(Num2),!;Num2 == macrodummy).

b5par_analyze_number_term(ge(N),_,(n,0),_,Varterm,[Key|Varterm],Names,Names,FI,[internal/[ge(N)]/ (n,0) /Key|FI]) :-
	!,(number(N),!;N == macrodummy).

b5par_analyze_number_term(gt(N),_,(n,0),_,Varterm,[Key|Varterm],Names,Names,FI,[internal/[gt(N)]/ (n,0) /Key|FI]) :-
	!,(number(N),!;N == macrodummy).

b5par_analyze_number_term(lt(N),_,(n,0),_,Varterm,[Key|Varterm],Names,Names,FI,[internal/[lt(N)]/ (n,0) /Key|FI]) :-
	!,(number(N),!;N == macrodummy).

b5par_analyze_number_term(le(N),_,(n,0),_,Varterm,[Key|Varterm],Names,Names,FI,[internal/[le(N)]/ (n,0) /Key|FI]) :-
	!,(number(N),!;N == macrodummy).

b5par_analyze_number_term(Number,_,(n,0),_,Varterm,[Key|Varterm],Names,Names,FI,[internal/[num(Number)]/ (n,0) /Key|FI]) :-
	!,(number(Number),!;Number == macrodummy).

b5par_analyze_number_term(Name,Mode,(n,0),PN,Vartermin,Vartermout,Namesin,Namesout,FI1,FIout) :-
	ground(Name),
	b5st_get_macro(Name,Macro),!,
	b5par_analyze_number_term(Macro,Mode,(n,0),PN,Vartermin,Vartermout,Namesin,Namesout,FI1,FIout).

b5par_analyze_number_term(Name,_,(n,0),PN,Varterm,[Key|Varterm],Names,[class/Name|Names],FI,FI) :-
	ground(Name),	
	!,b5st_get_class(Name,(n,0),Key,_),
	b5par_cycle_check(class/Name,PN).

b5par_analyze_number_term(_,_,_,_,_,_,_,_,_,_) :- fail.


%%%%% analyze asets

b5par_analyze_aset_term(X,_,_,_,_,_,_,_,_,_) :-
	var(X),!,t5out_error(instantiation('TBox aset definitions')),fail.

b5par_analyze_aset_term(intersection(Arg1,Arg2),Mode,(a,N),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_aset_term(Arg1,Mode,(a,N),PN,Vartermin,Varterm1,Namesin,Names1,FIin,FI1),
	b5par_analyze_aset_term(Arg2,Mode,(a,N),PN,Varterm1,Vartermout,Names1,Namesout,FI1,FIout).

b5par_analyze_aset_term(union(Aset1,Aset2),Mode,(a,N),PN,Varterm,[union(Key1,Key2)|Varterm],Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_aset_term(Aset1,Mode,(a,N),PN,[],Varterm1,Namesin,Names1,[internal/Varterm1/ (a,N) /Key1|FIin],FI1),
	b5par_analyze_aset_term(Aset2,Mode,(a,N),PN,[],Varterm2,Names1,Namesout,[internal/Varterm2/ (a,N) /Key2|FI1],FIout).

b5par_analyze_aset_term(without(Aset1,Aset2),Mode,(a,N),PN,Varterm,[without(Key1,Key2)|Varterm],Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_aset_term(Aset1,Mode,(a,N),PN,[],Varterm1,Namesin,Names1,[internal/Varterm1/ (a,N) /Key1|FIin],FI1),
	b5par_analyze_aset_term(Aset2,Mode,(a,N),PN,[],Varterm2,Names1,Namesout,[internal/Varterm2/ (a,N) /Key2|FI1],FIout).

b5par_analyze_aset_term(aset(Liste),Mode,(a,0),_,Varterm,[aset(KeyListe)|Varterm],Names,Names,FIin,FIout) :-
	!,
	b5par_analyze_attributes(Liste,Mode,(a,0),open,[],KeyListe,FIin,FIout).

b5par_analyze_aset_term(aset(Att1 .. Att2,Domain),_,(a,N),_,Varterm,[aset(KeyListe)|Varterm],Names,Names,FIin,FIin) :-
	!,
	b5st_get_domain(Domain,(a,N),_,_,Attributes),
	b5par_extract_attributes(Att1,Att2,Attributes,KeyListe,(a,N)).

b5par_analyze_aset_term(aset(Liste,Domain),Mode,(a,N),_,Varterm,[aset(KeyListe)|Varterm],Names,Names,FIin,FIout) :-
	!,
	b5st_get_domain(Domain,(a,N),_,OpenClosed,_),
	b5par_analyze_attributes(Liste,Mode,(a,N),OpenClosed,[],KeyListe,FIin,FIout).

b5par_analyze_aset_term(Name,Mode,(a,N),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout) :-
	ground(Name),
	b5st_get_macro(Name,Macro),!,
	b5par_analyze_aset_term(Macro,Mode,(a,N),PN,Vartermin,Vartermout,Namesin,Namesout,FIin,FIout).

b5par_analyze_aset_term(Name,_,(a,N),PN,Varterm,[Key|Varterm],Names,[class/Name|Names],FI,FI) :-
	!,
	ground(Name),
	b5st_get_class(Name,(a,N),Key,_),
	b5par_cycle_check(class/Name,PN).

b5par_analyze_aset_term(_,_,_,_,_,_,_,_,_,_) :- fail.


%%%%% analyze attributes

b5par_analyze_attributes(X,_,_,_,_,_,_,_) :-
	var(X),!,t5out_error(instantiation('Attributes')),fail.

b5par_analyze_attributes([],_,_,_,ListIn,ListIn,UnKnownIn,UnKnownIn) :- !.

b5par_analyze_attributes([Attr|Rest],Mode,(a,N),OpenClosed,ListIn,ListOut,UnKnownIn,UnKnownOut) :-
	!,
	b5par_analyze_attribute(OpenClosed,Mode,Attr,(a,N),ListIn,List1,UnKnownIn,UnKnown1),
	b5par_analyze_attributes(Rest,Mode,(a,N),OpenClosed,List1,ListOut,UnKnown1,UnKnownOut).

b5par_analyze_attributes(macrodummy,_,_,_,_,_,_,_).

b5par_analyze_attribute(_,_,X,_,_,_,_,_) :-
	var(X),!,t5out_error(instantiation('Attributes')),fail.

b5par_analyze_attribute(closed,_,Attribute,Type,ListIn,[Key|ListIn],UnKnownIn,UnKnownIn) :-
	ground(Attribute),
	!,b5st_get_instance(Attribute,Type,Key,_).

b5par_analyze_attribute(open,_,Attribute,Type,ListIn,[Key|ListIn],UnKnownIn,UnKnownIn) :-
	ground(Attribute),
	b5st_get_instance(Attribute,Type,Key,_),!.

b5par_analyze_attribute(open,no_unknown_names,_,_,_,_,_,_) :- !,fail.

b5par_analyze_attribute(open,_,Attribute,Type,ListIn,[Key|ListIn],UnKnownIn,[forwardattribute/Attribute/Type/Key|UnKnownIn]) :-
	b5sta_check_flag(introduction,forward),
	atom(Attribute).


%%%%% analyze fillers


b5par_analyze_filler_term(X,_,_,_,_,_) :-
	var(X),!,t5out_error(instantiation('Fillers')),fail.

b5par_analyze_filler_term(FillerList,Mode,Role,KeyList,FIin,FIout) :-
	b5par_determine_role_range(Role,Range),
	b5par_analyze_fillers(FillerList,Mode,Range,[],KeyList,FIin,FIout).

b5par_analyze_fillers(X,_,_,_,_,_,_) :-
	var(X),!,t5out_error(instantiation('Fillers')),fail.

b5par_analyze_fillers(Filler1 and Filler2,Mode,Range,KeyListin,KeyListout,FIin,FIout) :-
	!,
	b5par_analyze_fillers(Filler1,Mode,Range,KeyListin,KeyList1,FIin,FI1),
	b5par_analyze_fillers(Filler2,Mode,Range,KeyList1,KeyListout,FI1,FIout).


b5par_analyze_fillers(Filler,_,Range,KeyListin,[Key|KeyListin],FI,FI) :-
	ground(Filler),
	b5st_get_instance(Filler,Range,Key,_),
	!.

b5par_analyze_fillers(_,no_unknown_names,_,_,_,_,_) :- !,fail.

b5par_analyze_fillers(Filler,_,(c,0),KeyListin,[Key|KeyListin],FI,[forwardobject/Filler/ (c,0) /Key|FI]) :-
	b5sta_check_flag(introduction,forward),
	atom(Filler),!.

b5par_analyze_fillers(Filler,_,(a,N),KeyListin,[Key|KeyListin],FI,[forwardattribute/Filler/ (a,N) /Key|FI]) :-
	b5sta_check_flag(introduction,forward),
	atom(Filler),!.

b5par_analyze_fillers(Filler,_,(n,0),KeyListin,[Key|KeyListin],FI,[forwardnumber/Filler/ (n,0) /Key|FI]) :-
	b5sta_check_flag(introduction,forward),
	number(Filler),!.

b5par_analyze_fillers(Filler,_,(s,0),KeyListin,[Key|KeyListin],FI,[forwardstring/Filler/ (s,0) /Key|FI]) :-
	b5sta_check_flag(introduction,forward),
	atom(Filler).


%%%%% analyze objects

b5par_analyze_object_list(X,_,_,_,_,_,_,_) :-
	var(X),!,t5out_error(instantiation('Objects')),fail.

b5par_analyze_object_list([],_,KeyList,KeyList,Names,Names,FI,FI) :- !.

b5par_analyze_object_list([Object|List],Mode,KeyListin,KeyListout,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_analyze_object(Object,Mode,KeyListin,KeyList1,Namesin,Names1,FIin,FI1),
	b5par_analyze_object_list(List,Mode,KeyList1,KeyListout,Names1,Namesout,FI1,FIout).

b5par_analyze_object_list(macrodummy,_,_,_,_,_,_,_).


b5par_analyze_object(X,_,_,_,_,_,_,_) :-
	var(X),!,t5out_error(instantiation('Objects')),fail.

b5par_analyze_object(Object,_,KeyList,[Key|KeyList],Namesin,[object/Object|Namesin],FI,FI) :-
	ground(Object),
	b5st_get_instance(Object,(c,0),Key,_),!.

b5par_analyze_object(_,no_unknown_names,_,_,_,_,_,_) :- !,fail.

b5par_analyze_object(Object,_,KeyList,[Key|KeyList],Namesin,[object/Object|Namesin],FI,[forwardobject/Object/ (c,0) /Key|FI]) :-
	b5sta_check_flag(introduction,forward),
	b5par_valid_name(Object).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FORWARD INTRODUCTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing of expressions generates a list of 'things to do'. This list gets
% processed only after a certain level of correctness can be assured, to 
% prevent unneccessary computations. The list can contain descriptions of
% objects of all types, nested expressions as terms, aboxtells or aboxqueries.
% All of those are processed and assigned a key or a list of keys. These keys
% will in turn be used in other computations or are the final result themselves.
% After processing the list, all the variables are instantiated. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b5par_forward_introduction([],_) :- !.

b5par_forward_introduction([Ele1|Rest],Status) :-
	b5par_introduce(Ele1,Status),
	b5par_forward_introduction(Rest,Status).

b5par_introduce(bq2proto/BQ/CPF,_) :-
	!,
	b5par_keyterm_to_protoform(BQ,(c,0),CPF).
b5par_introduce(internal/Def/Type/Key,Status) :-
	!,b5par_introduce_internal(Type,Def,Key,Status).
b5par_introduce(forwardobject/Name/Type/Key,_) :-
	!,b5par_introduce_object(Type,Name,Key).
b5par_introduce(forwardname/Name/Type/PrimKey/Key,_) :-
	!,b5par_introduce_name(Type,Name,PrimKey,Key).
b5par_introduce(forwardattribute/Name/Type/Key,_) :-
	!,b5par_introduce_instance(Type,Name,Key).
b5par_introduce(forwardnumber/Name/Type/Key,_) :-
	!,b5par_introduce_instance(Type,Name,Key).
b5par_introduce(forwardstring/Name/Type/Key,_) :-
	!,b5par_introduce_instance(Type,Name,Key).


b5par_introduce_name((r,0),Name,PrimKey,Key) :-
	b5st_get_class(Name,(r,0),Key,_),!,
	(b5st_get_class(prim(Name),(r,0),PrimKey,_),!;true).
b5par_introduce_name((r,0),Name,PrimKey,Key) :-
	!,t5role_new_prim(PrimKey),
	b5st_add_class(prim(Name),(r,0),PrimKey,internal),
	b5st_get_domain(_,(r,0),DomKey,_,_),
	t5fil_filter(user_primitive,Filter),
	t5role_store_atomic_role(PrimKey,DomKey,Filter,Key),
	b5st_add_class(Name,(r,0),Key,Name:<domain(anything) and range(anything)).
b5par_introduce_name((c,0),Name,PrimKey,Key) :-
	b5st_get_class(Name,(c,0),Key,_),!,
	(b5st_get_class(prim(Name),(c,0),PrimKey,_),!;true).
b5par_introduce_name((c,0),Name,PrimKey,Key) :-
	b5st_get_domain(_,(c,0),DomKey,_,_),!,
	t5concid_new_prim(DomKey,PrimKey), 	
	b5st_add_class(prim(Name),(c,0),PrimKey,forward),
	t5fil_filter(user_primitive,Filter),
	t5concid_new_atomic_prim_conc(DomKey,DomKey,PrimKey,Filter,Key),
	b5st_add_class(Name,(c,0),Key,Name:<anything).


b5par_introduce_instance(Type,Name,Key) :-
	b5st_get_instance(Name,Type,Key,_),!.
b5par_introduce_instance(Type,Name,Key) :-
	b5st_get_domain(_,Type,DomKey,open,_),!,
	t5dom_new_instance(DomKey,Name,Key),      %% make a tmp copy
	b5st_add_instance(Name,Type,Key,no_definition).



b5par_introduce_object((c,0),Name,Key) :-
	b5st_get_instance(Name,(c,0),Key,_),!.
b5par_introduce_object((c,0),Name,Key) :-
	( (  b5sta_check_flag(aboxfilled,true) 
	  ;  b5sta_check_flag(aboxfilled,abox) ) 
	          -> true
	; b5sta_set_flag(aboxfilled,true)),
	b5par_abox_tell(Name, anything),
	b5st_get_instance(Name,(c,0),Key,_).


b5par_introduce_internal(_,[Key],Key,_) :- 
	integer(Key),!.
b5par_introduce_internal((r,0),Def,Key,Status) :-
	!,
	t5fil_filter(internal,Filter),
	b5par_tt_to_kernel((r,0),Def,role,Filter,Key,Status,def).
b5par_introduce_internal(Type,Def,Key,Status) :-
	t5fil_filter(internal,Filter),
	b5par_tt_to_kernel(Type,Def,norole,Filter,Key,Status,def).


%% creates a new prim-component and generates a symbol-table entry for it
b5par_create_prim((r,0),Name,prim(Key)) :-
	!,
	t5role_new_prim(Key),
	b5st_add_class(prim(Name),(r,0),Key,internal).
b5par_create_prim(Type,Name,prim(Key)) :-
	b5st_get_domain(_,Type,DomainKey,_,_),!,
	t5concid_new_prim(DomainKey,Key), %uk
	b5st_add_class(prim(Name),Type,Key,internal).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% KERNEL INTERFACE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generates a list of concept forming operators in which all names
% have been replaced by keys. This list is send incrementally to the kernel.
% The kernel provides an empty 'protoform' which is then filled with operators.
% When all operators have been added, the kernel is told to process the 
% protoform and computes the class' key.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b5par_tt_atomic_prim_to_kernel((T,N),PrimKey,[KeyTerm],RoleType,Filter,Key):-
	b5st_get_domain(_,(T,N),DomKey,_,_),
	b5par_tt_atomic_prim_to_kernel1(T,DomKey,KeyTerm,PrimKey,RoleType,Filter,Key).

b5par_tt_atomic_prim_to_kernel1(c,DomKey,SuperKey,PrimKey,_,Filter,Key) :-
	t5concid_new_atomic_prim_conc(DomKey,SuperKey,PrimKey,Filter,Key).
b5par_tt_atomic_prim_to_kernel1(r,DomKey,SuperKey,PrimKey,RoleType,Filter,Key) :-
%	b5par_tt_atomic_prim_to_kernel2(RoleType,_,SuperKey,PrimKey,Filter,Key).
	b5par_tt_to_kernel((r,0),[DomKey,SuperKey,PrimKey],RoleType,Filter,Key,_,prim).


b5par_tt_atomic_prim_to_kernel2(role,_,SuperKey,PrimKey,Filter,Key) :-
	t5role_store_atomic_role(PrimKey,SuperKey,Filter,Key).
b5par_tt_atomic_prim_to_kernel2(feature,_,SuperKey,PrimKey,Filter,Key) :-
	t5role_store_atomic_feature(PrimKey,SuperKey,Filter,Key).
%b5par_tt_atomic_prim_to_kernel2(inherent_feature,_,SuperKey,PrimKey,Filter,Key) :-
%	t5role_store_atomic_inherent_feature(PrimKey,SuperKey,Filter,Key).


b5par_tt_domran_role_to_kernel(PrimKey,KeyTerm,RoleType,Filter,Key) :-
	b5par_domran_separate(KeyTerm,DomList,RangeList),
	t5concid_infimum(DomList,DomKey),
	t5concid_infimum(RangeList,RangeKey),
	b5par_tt_domran_role_to_kernel1(RoleType,DomKey,RangeKey,PrimKey,Filter,Key).


b5par_tt_domran_role_to_kernel1(role,DomKey,RangeKey,PrimKey,Filter,Key) :-
	!,t5role_store_domain_range_role(PrimKey,DomKey,RangeKey,Filter,Key).
b5par_tt_domran_role_to_kernel1(feature,DomKey,RangeKey,PrimKey,Filter,Key) :-
	!,t5role_store_domain_range_feature(PrimKey,DomKey,RangeKey,Filter,Key).
%b5par_tt_domran_role_to_kernel1(inherent_feature,DomKey,RangeKey,PrimKey,Filter,Key) :-
%	t5role_store_domain_range_inherent_feature(PrimKey,DomKey,RangeKey,Filter,Key).


b5par_domran_separate([],[],[]).
b5par_domran_separate([domain(Key)|R],[Key|L],X) :-
	!,b5par_domran_separate(R,L,X).
b5par_domran_separate([range(Key)|R],X,[Key|L]) :-
	b5par_domran_separate(R,X,L).

b5par_tt_to_kernel(_,[Key],Type,Filter,Key,_,_) :- 
	Type \== feature,                             %% mf 3.3.93
	integer(Key),!,
	t5tbox_add_filter(Key,Filter). %uk

b5par_tt_to_kernel((r,0),KeyList,role,Filter,NewKey,Statusin,PrimDef) :-
	!,
	b5par_new_role(PrimDef,CPF),
	b5par_tt_send_role(KeyList,CPF,CPFfin),
	t5role_store(CPFfin,Filter,NewKey,Statusout),
	b5par_compare_status(Statusout,Statusin).
b5par_tt_to_kernel((r,0),KeyList,feature,Filter,NewKey,Statusin,PrimDef) :-
	!,
	b5par_new_role(PrimDef,CPF),
	b5par_tt_send_role(KeyList,CPF,CPFfin),
	t5role_store_feature(CPFfin,Filter,NewKey,Statusout),
	b5par_compare_status(Statusout,Statusin).
%b5par_tt_to_kernel((r,0),KeyList,inherent_feature,Filter,NewKey,Statusin,PrimDef) :-
%	!,
%	b5par_new_role(PrimDef,CPF),
%	b5par_tt_send_role(KeyList,CPF,CPFfin),
%	t5role_store_inherent_feature(CPFfin,Filter,NewKey,Statusout),
%	b5par_compare_status(Statusout,Statusin).
b5par_tt_to_kernel(Type,KeyList,norole,Filter,NewKey,_,PrimDef) :-
	b5par_new_conc(PrimDef,Type,CPF),
	b5par_tt_send_conc(KeyList,CPF,CPFfin),
	b5nf_store(CPFfin,Filter,NewKey,_).



b5par_new_role(def,CPF) :-
	t5role_new_defined_role(CPF).
b5par_new_role(prim,CPF) :-
	t5role_new_primitive_role(CPF).

b5par_new_conc(def,Type,CPF) :-
	b5st_get_domain(_,Type,Key,_,_),
	b5nf_new_defined_conc(Key,CPF).
b5par_new_conc(prim,Type,CPF) :-
	b5st_get_domain(_,Type,Key,_,_),
	b5nf_new_primitive_conc(Key,CPF).
	

b5par_tt_send_role([],CPF,CPF).
b5par_tt_send_role([Ele1|Rest],CPF,CPFfin) :-
	b5par_tt_send_role_term(Ele1,CPF,CPFnew),  
	b5par_tt_send_role(Rest,CPFnew,CPFfin),
	!.


b5par_tt_send_role_term(inv(Key),CPF,CPFnew):- !,
	t5role_add_inv(CPF,Key,CPFnew).
b5par_tt_send_role_term(trans(Key),CPF,CPFnew):- !,
	t5role_add_trans(CPF,Key,CPFnew).  
b5par_tt_send_role_term(prim(Key),CPF,CPFnew):- !,
	t5role_add_prim(CPF,Key,CPFnew).  
b5par_tt_send_role_term(not(Key),CPF,CPFnew):- !,
	t5role_add_neg_prim(CPF,Key,CPFnew).
b5par_tt_send_role_term(comp(Key1,Key2),CPF,CPFnew):- !,
	t5role_add_comp(CPF,Key1,Key2,CPFnew).
b5par_tt_send_role_term(range(Key),CPF,CPFnew):- !,
	t5role_add_range(CPF,Key,CPFnew).
b5par_tt_send_role_term(domain(Key),CPF,CPFnew):- !,
	t5role_add_domain(CPF,Key,CPFnew).
b5par_tt_send_role_term(Key,CPF,CPFnew) :-
	t5role_add_role(CPF,Key,CPFnew),!.



b5par_tt_send_conc([],CPF,CPF) :- !.
b5par_tt_send_conc([Head|Tail],CPF,CPFfin) :-
	b5par_tt_send_conc_term(Head, CPF, CPFnew), 
	b5par_tt_send_conc(Tail,CPFnew,CPFfin).
 
b5par_tt_send_conc_term(oneof(ObjKeyList),CPF,CPFnew) :- !, 
	b5nf_add_oneof(CPF,ObjKeyList,CPFnew).
b5par_tt_send_conc_term(prim(Key),CPF,CPFnew) :- !, 
	b5nf_add_prim(CPF,Key,CPFnew).
b5par_tt_send_conc_term(not(Key),CPF,CPFnew) :- !, 
	b5nf_add_neg_prim(CPF,Key,CPFnew). 
b5par_tt_send_conc_term(all(RoleKey,ConcKey),CPF,CPFnew) :- !, 
	b5nf_add_vr(CPF,ConcKey,RoleKey,CPFnew).
b5par_tt_send_conc_term(atleast(N,RoleKey),CPF,CPFnew) :- !, 
	b5nf_add_atleast(CPF,N,RoleKey,CPFnew).
b5par_tt_send_conc_term(atmost(N,RoleKey),CPF,CPFnew) :- !, 
	b5nf_add_atmost(CPF,N,RoleKey,CPFnew).
b5par_tt_send_conc_term(fills(Key1,KeyList),CPF,CPFnew) :- !,
	b5nf_add_fillers(CPF,KeyList,Key1,CPFnew).
b5par_tt_send_conc_term(close(RoleKey,OKL),CPF,CPFnew) :- !,
	b5nf_add_closed_fillers(CPF,OKL,RoleKey,CPFnew).
b5par_tt_send_conc_term(rvm(RoleKey1,RoleKey2),CPF,CPFnew) :- !,
	b5nf_add_rvm(CPF,RoleKey1,RoleKey2,CPFnew).
b5par_tt_send_conc_term(union(Key1,Key2),CPF,CPFnew) :- !,
	b5nf_add_union(CPF,Key1,Key2,CPFnew).
b5par_tt_send_conc_term(without(Key1,Key2),CPF,CPFnew) :- !,
	b5nf_add_without(CPF,Key1,Key2,CPFnew).
b5par_tt_send_conc_term(aset(KeyList),CPF,CPFnew) :- !,
	b5nf_add_aset(CPF,KeyList,CPFnew).
b5par_tt_send_conc_term(fromto(num(N1),num(N2)),CPF,CPFnew) :- !,
	b5nf_add_fromto(CPF,N1,N2,CPFnew).
b5par_tt_send_conc_term(gt(N),CPF,CPFnew) :- !,
	b5nf_add_gt(CPF,N,CPFnew).
b5par_tt_send_conc_term(ge(N),CPF,CPFnew) :- !,
	b5nf_add_ge(CPF,N,CPFnew).
b5par_tt_send_conc_term(lt(N),CPF,CPFnew) :- !,
	b5nf_add_lt(CPF,N,CPFnew).
b5par_tt_send_conc_term(le(N),CPF,CPFnew) :- !,
	b5nf_add_le(CPF,N,CPFnew).
b5par_tt_send_conc_term(num(N),CPF,CPFnew) :- !,
	b5nf_add_num(CPF,N,CPFnew).
b5par_tt_send_conc_term(Key,CPF,CPFnew) :-
	b5nf_add_conc(CPF,Key,CPFnew),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 			TBOX ASK
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The BACK V Syntax only provides ?< (i.e. subsumed-by) as tbox query.
% FFS: Which additional queries should be implemented? Macros?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b5par_tbox_ask_subsumes(Term1,Term2,IBoxMode) :-
	b5par_main(Term2,no_unknown_names,Type,[],[],KeyTerm2,[],_,[],FI2),
	b5par_main(Term1,no_unknown_names,Type,[],[],KeyTerm1,[],_,[],FI1),
	b5par_forward_introduction(FI1,_),
	b5par_forward_introduction(FI2,_),
	t5fil_filter(internal,Filter1),
	t5fil_filter(internal,Filter2),
	b5par_tt_to_kernel(Type,KeyTerm1,_,Filter1,Key1,_,def),
	b5par_tt_to_kernel(Type,KeyTerm2,_,Filter2,Key2,_,def),
	b5par_to_sub(Type,IBoxMode,Key1,Key2).


b5par_tbox_ask_incoherent(Term,Iboxmode) :-
	b5par_main(Term,no_unknown_names,Type,[],[],KeyTerm,[],_,[],FI1),
	b5par_forward_introduction(FI1,_),
	t5fil_filter(internal,Filter),
	b5par_tt_to_kernel(Type,KeyTerm,_,Filter,Key,_,def),
	b5st_get_class(nothing,_,NothingKey,_),!,
	b5par_to_sub(Type,Iboxmode,NothingKey,Key).
	

b5par_tbox_ask_disjoint(Term1,Term2,Iboxmode) :-
	b5par_main(Term1,no_unknown_names,Type,[],[],KeyTerm1,[],_,[],FI1),
	b5par_main(Term2,no_unknown_names,Type,[],[],KeyTerm2,[],_,[],FI2),
	b5par_forward_introduction(FI1,_),
	b5par_forward_introduction(FI2,_),
	t5fil_filter(internal,Filter1),
	t5fil_filter(internal,Filter2),
	b5par_tt_to_kernel(Type,KeyTerm1,_,Filter1,Key1,_,def),
	b5par_tt_to_kernel(Type,KeyTerm2,_,Filter2,Key2,_,def),
	b5par_to_dis(Type,Iboxmode,Key1,Key2).


b5par_tbox_ask_equi(Term1,Term2,IBoxMode) :-
	b5par_main(Term1,no_unknown_names,Type,[],[],KeyTerm1,[],_,[],FI1),
	b5par_main(Term2,no_unknown_names,Type,[],[],KeyTerm2,[],_,[],FI2),
	b5par_forward_introduction(FI1,_),
	b5par_forward_introduction(FI2,_),
	t5fil_filter(internal,Filter1),
	t5fil_filter(internal,Filter2),
	b5par_tt_to_kernel(Type,KeyTerm1,_,Filter1,Key1,_,def),
	b5par_tt_to_kernel(Type,KeyTerm2,_,Filter2,Key2,_,def),
	b5par_to_equi(Type,IBoxMode,Key1,Key2).





b5par_to_sub((r,0),noibox,Key1,Key2) :-
	!,t5tbox_subsumes_p(role,Key1,Key2).

b5par_to_sub(_,noibox,Key1,Key2) :-
	!,t5tbox_subsumes_p(conc,Key1,Key2).

b5par_to_sub((r,0),ibox,Key1,Key2) :-
	!,i5ibox_subsumes_p(role,Key1,Key2).

b5par_to_sub(_,ibox,Key1,Key2) :-
	i5ibox_subsumes_p(conc,Key1,Key2).


b5par_to_equi((r,0),noibox,Key1,Key2) :-
	!,t5tbox_equivalent_p(role,Key1,Key2).

b5par_to_equi(_,noibox,Key1,Key2) :-
	!,t5tbox_equivalent_p(conc,Key1,Key2).

b5par_to_equi((r,0),ibox,Key1,Key2) :-
	!,i5ibox_equivalent_p(role,Key1,Key2).

b5par_to_equi(_,ibox,Key1,Key2) :-
	i5ibox_equivalent_p(conc,Key1,Key2).


b5par_to_dis((r,0),ibox,Key1,Key2) :-
	!,i5ibox_disjoint_p(role,Key1,Key2).

b5par_to_dis(_,ibox,Key1,Key2) :-
	!,i5ibox_disjoint_p(conc,Key1,Key2).

b5par_to_dis((r,0),noibox,Key1,Key2) :-
	!,t5tbox_disjoint_p(role,Key1,Key2).

b5par_to_dis(_,noibox,Key1,Key2) :-
	t5tbox_disjoint_p(conc,Key1,Key2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			ABOX TELL
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The aboxtell parser differs from the tbox parser in several respects. Most
% important is that aboxtells can be nested. This requires a different 
% forward introduction list, namely one that contains aboxtell expressions.
% As the tbox list, it contains descriptions of nested terms. In contrary to
% it, it cannot contain unknown names, since all class names in aboxtells
% must be known. Only Objects (of all types) can be introduced forward.
% After parsing the aboxtell expression, the nested terms are send to the
% tbox kernel and then replaced by their key, just as in the tbox. Unknown 
% attributes and strings (numbers?) are handled similarily. All nested 
% aboxtells are send to the abox kernel's agenda, as is the topmost tell.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



b5par_abox_tell(O,T) :-
	b5par_at_parse(O,T,FI),
	a5sch_make_scheduler(AG),
	b5par_process(FI,AG,NAG),
	t5out_info(atp_to_a5sch),
	(a5sch_process_agenda(NAG) -> 
	    b5par_at_cleanup,
%	    b5dom_cleanup,
	    t5dm_cleanup
	;
	b5par_at_reset,
%	b5dom_reset,
	t5dm_reset,	
	fail).

b5par_at_reset :-
	b5par_at_del_tmps(Tmps),
	b5par_at_reset_tmps(Tmps).


b5par_at_del_tmps([(Name,Type,Key,Def)|Tmps]) :-
	retract(b5par_at_tmp(Name,Type,Key,Def)),!,
	b5par_at_del_tmps(Tmps).
b5par_at_del_tmps([]).


b5par_at_reset_tmps([]) :- !.
b5par_at_reset_tmps([(Name,Type,Key,Def)|Tmps]) :-
	b5st_del_instance(Name,Type,Key,_),!,
	(Def == new -> true;
	b5st_add_instance(Name,Type,Key,Def)),
	b5par_at_reset_tmps(Tmps).

b5par_at_cleanup :-
	b5par_at_del_tmps(_).


b5par_at_make_copy(Name,Type,Key) :-
	b5st_get_instance(Name,Type,Key,Def),!,
	assert(b5par_at_tmp(Name,Type,Key,Def)).
b5par_at_make_copy(Name,Type,Key) :-
	assert(b5par_at_tmp(Name,Type,Key,new)).




b5par_process([],AG,AG) :- !.
b5par_process([A|Rest],AG,NAG) :-
	b5par_at_send(A,AG,ZAG),
	b5par_process(Rest,ZAG,NAG).


b5par_at_send(object/Name/Def/Names/KeyTerm/ (c,0) /Key,AG,NAG) :-
	var(Name),!,
	t5out_trace(to_agenda(Name)),
	b5par_keyterm_to_protoform(KeyTerm,(c,0),Protoform),
	a5sch_process(Protoform,Key,AG,NAG),
	Name = uc(Key),
	t5dm_obj_uses_add(Key,Names),  %% make a tmp copy !
        b5par_update_description(add,Name,([],[]),Def,NewDef),
	b5par_at_make_copy(Name,(c,0),Key),
	b5st_add_instance(Name,(c,0),Key,NewDef).

b5par_at_send(object/Name/Def/Names/KeyTerm/ (c,0) /Key,AG,NAG) :-
	!,
	t5out_trace(to_agenda(Name)),
	b5par_keyterm_to_protoform(KeyTerm,(c,0),Protoform),
	(b5st_get_instance(Name,(c,0),Key,OldDef),!,
	a5sch_process(Protoform,Key,AG,NAG),
	b5par_update_description(add,Name,OldDef,Def,NewDef);
	a5sch_process(Protoform,Key,AG,NAG),
	b5par_update_description(add,Name,([],[]),Def,NewDef)),
	t5dm_obj_uses_add(Key,Names),   %% make a tmp copy !
	b5par_at_make_copy(Name,(c,0),Key),
	b5par_update_entry(object,(Name,(c,0),Key,NewDef)).

b5par_at_send(object/Name/_/_/_/Type/Key,AG,AG) :- 
	!,
	t5out_trace(intro(Name)),
	b5par_at_make_copy(Name,Type,Key),
	b5par_introduce_instance(Type,Name,Key).

b5par_at_send(internal/Type/KeyTerm/Key,AG,AG) :-
	t5out_trace(intro('A nested term')),
	b5par_introduce_internal(Type,KeyTerm,Key,old).


b5par_keyterm_to_protoform(KeyTerm,Type,Protoform) :-
	b5st_get_domain(_,Type,Key,_,_),
	b5nf_create(Key,CPF),
	b5par_tt_send_conc(KeyTerm,CPF,Protoform).




b5par_at_parse(theknown(Term),N,FI) :-
	ground(Term),
	!,
	(b5st_get_class(Term,_,Key,_),!,
	 b5par_selector(IBoxMode,Selector),
	 a5ret_key_retrieve(Key,Selector,[ObjectKey]),
	 b5st_get_instance(Object,(c,0),ObjectKey,_);
	 b5par_dnf(Term,DNF),
	 b5par_handle_dnf(DNF,Object,(c,0),IBoxMode,at)),
	 b5par_at_parse(Object,N,FI).


b5par_at_parse(O,N,[object/OName/N/[class/N]/[Key]/ (c,0) /OK]) :-
	atom(N),
	(ground(O),b5par_uc(O,OK),!,b5st_get_instance(O,_,OK,_),OName = uc(OK)
    ;
	!,b5par_objname_test(O),OName = O),
	b5st_get_class(N,(c,0),Key,_),!,
	t5out_trace(start(at)),
	t5out_trace(end(at)),
	t5out_info(at_parse_ok).

b5par_at_parse(O,T,FI) :-
	t5out_trace(start(at)),
	(ground(O),b5par_uc(O,OK),!,b5st_get_instance(O,_,OK,_),OName = uc(OK)
    ;
	!,b5par_objname_test(O),OName = O),
	b5par_at_parse_term(T,(c,0),[],VKTX,Def,[],Names,[object/OName/Def/Names/VKTX/ (c,0) /OK],FI),!,
	t5out_trace(end(at)),
	t5out_info(at_parse_ok).

b5par_at_parse(_,_,_) :-
	!,t5out_info(at_parse_failed),fail.



b5par_at_parse_term(A1 and A2,Type,VKTin,VKTout,Def1 and Def2,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_at_parse_term(A1,Type,VKTin,VKT1,Def1,Namesin,Names1,FIin,FI1),
	b5par_at_parse_term(A2,Type,VKT1,VKTout,Def2,Names1,Namesout,FI1,FIout).

b5par_at_parse_term(R:close(Fillers),(c,0),VKT,[close(RK,OKL)|VKT],RDef:Def2,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_correct_role_term(R),
	b5par_at_parse_term(R,(r,0),[],RVKT,RDef,Namesin,Namesout,[internal/ (r,0) /RVKT/RK|FIin],FI1),
	b5par_determine_role_range(R,RRType),
	b5par_at_parse_fillers(Fillers,RRType,[],OKL,Def2,FI1,FIout).

b5par_at_parse_term(R:Fillers,(c,0),VKT,[fills(RK,OKL)|VKT],RDef:Def2,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_correct_role_term(R),
	b5par_at_parse_term(R,(r,0),[],RVKT,RDef,Namesin,Namesout,[internal/ (r,0) /RVKT/RK|FIin],FI1),
	b5par_determine_role_range(R,RRType),
	b5par_at_parse_fillers(Fillers,RRType,[],OKL,Def2,FI1,FIout).

b5par_at_parse_term(oneof(Objects),(c,0),VKT,[oneof(KeyList)|VKT],oneof(Objects),Namesin,Namesin,FIin,FIout) :-
	!,
	b5par_at_parse_objects(Objects,(c,0),[],KeyList,FIin,FIout).

b5par_at_parse_term(atleast(N,R),(c,0),VKT,[atleast(N,RK)|VKT],atleast(N,RDef),Namesin,Namesout,FIin,FIout) :-
	!,
	integer(N),
	b5par_correct_role_term(R),
	b5par_at_parse_term(R,(r,0),[],RVKT,RDef,Namesin,Namesout,[internal/ (r,0) /RVKT/RK|FIin],FIout).

b5par_at_parse_term(atmost(N,R),(c,0),VKT,[atmost(N,RK)|VKT],atmost(N,RDef),Namesin,Namesout,FIin,FIout) :-
	!,
	integer(N),
	b5par_correct_role_term(R),
	b5par_at_parse_term(R,(r,0),[],RVKT,RDef,Namesin,Namesout,[internal/ (r,0) /RVKT/RK|FIin],FIout).

b5par_at_parse_term(rvm_equal(R1,R2),(c,0),VKT,[rvm(R1K,R2K)|VKT],rvm_equal(R1Def,R2Def),Namesin,Namesout,FIin,FIout) :-
								% ?????????:w
	!,
	b5par_correct_role_term(R1),
	b5par_at_parse_term(R1,(r,0),[],R1VKT,R1Def,Namesin,Names1,[internal/ (r,0) /R1VKT/R1K|FIin],R1FI),
	b5par_correct_role_term(R2),
	b5par_at_parse_term(R2,(r,0),[],R2VKT,R2Def,Names1,Namesout,[internal/ (r,0) /R2VKT/R2K|R1FI],FIout).

b5par_at_parse_term(all(R,C),(c,0),VKT,[all(RKey,CKey)|VKT],all(RDef,CDef),Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_correct_role_term(R),
	b5par_at_parse_term(R,(r,0),[],RVKT,RDef,Namesin,Names1,[internal/ (r,0) /RVKT/RKey|FIin],RFI),
	b5par_at_parse_term(C,Type,[],CVKT,CDef,Names1,Namesout,[internal/Type/CVKT/CKey|RFI],FIout),
	Type \== (r,0).

b5par_at_parse_term(not(Name),Type,VKT,[not(Key)|VKT],not(Name),Namesin,[class/Name|Namesin],FI,FI) :-
	!,
	b5st_get_class(prim(Name),Type,Key,_).


b5par_at_parse_term(domain(C),(r,0),VKT,[domain(CKey)|VKT],domain(CDef),Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_at_parse_term(C,(c,0),[],CVKT,CDef,Namesin,Namesout,[internal/ (r,0) /CVKT/CKey|FIin],FIout).

b5par_at_parse_term(range(T),(r,0),VKT,[range(TKey)|VKT],range(TDef),Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_at_parse_term(T,Type,[],TVKT,TDef,Namesin,Namesout,[internal/ (r,0) /TVKT/TKey|FIin],FIout),
	Type \== (r,0).


b5par_at_parse_term(inv(R),(r,0),VKT,[inv(RKey)|VKT],inv(RDef),Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_correct_role_term(R),
	b5par_at_parse_term(R,(r,0),[],RVKT,RDef,Namesin,Namesout,[internal/ (r,0) /RVKT/RKey|FIin],FIout).

b5par_at_parse_term(trans(R),(r,0),VKT,[trans(RKey)|VKT],trans(RDef),Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_correct_role_term(R),
	b5par_at_parse_term(R,(r,0),[],RVKT,RDef,Namesin,Namesout,[internal/ (r,0) /RVKT/RKey|FIin],FIout).

b5par_at_parse_term(R1 comp R2,(r,0),VKT,[comp(RKey1,RKey2)|VKT],RDef1 comp RDef2,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_correct_role_term(R1),
	b5par_correct_role_term(R1),
	b5par_at_parse_term(R1,(r,0),[],RVKT1,RDef1,Namesin,Names1,[internal/ (r,0) /RVKT1/RKey1|FIin],FI1),
	b5par_at_parse_term(R2,(r,0),[],RVKT2,RDef2,Names1,Namesout,[internal/ (r,0) /RVKT2/RKey2|FI1],FIout).

b5par_at_parse_term(R1.R2,(r,0),VKT,[comp(RKey1,RKey2)|VKT],RDef1.RDef2,Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_correct_role_term(R1),
	b5par_correct_role_term(R1),
	b5par_at_parse_term(R1,(r,0),[],RVKT1,RDef1,Namesin,Names1,[internal/ (r,0) /RVKT1/RKey1|FIin],FI1),
	b5par_at_parse_term(R2,(r,0),[],RVKT2,RDef2,Names1,Namesout,[internal/ (r,0) /RVKT2/RKey2|FI1],FIout).

b5par_at_parse_term(intersection(A1,A2),Type,VKTin,VKTout,intersection(Def1,Def2),Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_at_parse_term(A1,Type,VKTin,VKT1,Def1,Namesin,Names1,FIin,FI1),
	b5par_at_parse_term(A2,Type,VKT1,VKTout,Def2,Names1,Namesout,FI1,FIout).

b5par_at_parse_term(union(A1,A2),(a,N),VKTin,[union(Key1,Key2)|VKTin],union(Def1,Def2),Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_at_parse_term(A1,(a,N),[],VKT1,Def1,Namesin,Names1,[internal/ (r,0) /VKT1/Key1|FIin],FI1),
	b5par_at_parse_term(A2,(a,N),[],VKT2,Def2,Names1,Namesout,[internal/ (r,0) /VKT2/Key2|FI1],FIout).

b5par_at_parse_term(without(A1,A2),(a,N),VKTin,[without(Key1,Key2)|VKTin],without(Def1,Def2),Namesin,Namesout,FIin,FIout) :-
	!,
	b5par_at_parse_term(A1,(a,N),[],VKT1,Def1,Namesin,Names1,[internal/ (r,0) /VKT1/Key1|FIin],FI1),
	b5par_at_parse_term(A2,(a,N),[],VKT2,Def2,Names1,Namesout,[internal/ (r,0) /VKT2/Key2|FI1],FIout).

b5par_at_parse_term(aset(AL),(a,0),VKT,[aset(KL)|VKT],aset(AL),Namesin,Namesin,FIin,FIout) :-
	!,
	b5par_at_parse_attributes(AL,KL,FIin,FIout,(a,0),open).

b5par_at_parse_term(aset(A1 .. A2,D),(a,N),VKT,[aset(KL)|VKT],aset(A1 .. A2),Namesin,Namesin,FI,FI) :-
	!,
	b5st_get_domain(D,(a,N),_,_,Attributes),
	b5par_extract_attributes(A1,A2,Attributes,KL,(a,N)).

b5par_at_parse_term(aset(AL,D),(a,N),VKT,[aset(KL)|VKT],aset(AL,D),Namesin,Namesin,FIin,FIout) :-
	!,
	b5st_get_domain(D,(a,N),_,OC,_),
	b5par_at_parse_attributes(AL,KL,FIin,FIout,(a,N),OC).


b5par_at_parse_term(N1 .. N2,(n,0),VKT,[fromto(num(N1),num(N2))|VKT],N1 .. N2,Namesin,Namesin,FIin,FIin) :-
	!,
	b5par_number_test(N1),
	b5par_number_test(N2).

b5par_at_parse_term(gt(N),(n,0),VKT,[gt(N)|VKT],gt(N),Namesin,Namesin,FI,FI) :-
	!,
	b5par_number_test(N).

b5par_at_parse_term(lt(N),(n,0),VKT,[gt(N)|VKT],gt(N),Namesin,Namesin,FI,FI) :-
	!,
	b5par_number_test(N).

b5par_at_parse_term(ge(N),(n,0),VKT,[gt(N)|VKT],gt(N),Namesin,Namesin,FI,FI) :-
	!,
	b5par_number_test(N).

b5par_at_parse_term(le(N),(n,0),VKT,[gt(N)|VKT],gt(N),Namesin,Namesin,FI,FI) :-
	!,
	b5par_number_test(N).

b5par_at_parse_term(N,(n,0),VKT,[num(N)|VKT],N,Namesin,Namesin,FI,FI) :-
	number(N),!.


b5par_at_parse_term(Name,Type,VKT,[Key|VKT],Name,Namesin,[class/Name|Namesin],FI,FI) :-
	b5st_get_class(Name,Type,Key,_),!.

b5par_at_parse_term(Name,X1,X2,X3,X4,X5,X6,X7,X8) :-  % uk
	b5st_get_macro(Name,XName),!,
	b5par_at_parse_term(XName,X1,X2,X3,X4,X5,X6,X7,X8). 

b5par_at_parse_term(Item,_,_,_,_,_,_,_,_) :-
	b5st_get_class(Item,_,_,_),!,
	t5out_error(wrong_type(Item)),
	fail.

b5par_at_parse_term(Item,_,_,_,_,_,_,_,_) :-
	b5par_valid_name(Item),
	!,
	t5out_error(unknown(Item)),
	fail.

%%%%%%

b5par_at_parse_objects([],_,VKT,VKT,FI,FI) :- !.
b5par_at_parse_objects([Object|Rest],(c,0),VKTin,[OK|VKTout],FIin,FIout) :-
	b5st_get_instance(Object,Type,OK,_),!,
	b5par_at_parse_objects(Rest,Type,VKTin,VKTout,FIin,FIout).
b5par_at_parse_objects([Object|Rest],(c,0),VKTin,[OK|VKTout],FIin,[object/Object/Name/[]/[Key]/ (c,0) /OK|FIout]) :-
	b5sta_check_flag(introduction,forward),
	b5st_get_domain(Name,(c,0),Key,open,_),
	b5par_at_parse_objects(Rest,(c,0),VKTin,VKTout,FIin,FIout).

%%%%%%

b5par_at_parse_attributes([],[],FI,FI,_,_) :- !.
b5par_at_parse_attributes([A1|AL],[K|KL],FIin,FIout,Type,OC) :-
	b5st_get_instance(A1,Type,K,_),!,
	b5par_at_parse_attributes(AL,KL,FIin,FIout,Type,OC).
b5par_at_parse_attributes([A1|AL],[K|KL],FIin,FIout,Type,open) :-
	b5sta_check_flag(introduction,forward),!,
	b5par_at_parse_attributes(AL,KL,[object/A1/_/_/Type/K|FIin],FIout,Type,open).

b5par_at_parse_attributes([A|_],_,_,_,_,_) :-
	!,
	t5out_error(unknown(A)),
	fail.
%%%%%%

b5par_at_parse_fillers(Filler,_,_,_,_,_,_) :-
	var(Filler),!,
	fail.

b5par_at_parse_fillers([],_,_,_,_,_,_) :-
	!,
	t5out_error(wrong_name([])),
	fail.

b5par_at_parse_fillers([X|R],_,_,_,_,_,_) :-
	!,
	t5out_error(wrong_name([X|R])),
	fail.


b5par_at_parse_fillers(O::N,(c,0),VKT,[OK|VKT],O,FIin,[object/O/[N]/[class/N]/[Key]/ (c,0) /OK|FIin]) :-
	(atom(O);b5par_uc(O,_)),
	atom(N),!,
	b5st_get_class(N,(c,0),Key,_).


b5par_at_parse_fillers(O::T,(c,0),VKT,[OK|VKT],O,FIin,FIout) :-
	(atom(O);b5par_uc(O,_)),!,
	b5par_at_parse_term(T,(c,0),[],VKTX,Def,[],Names,[object/O/Def/Names/VKTX/ (c,0) /OK|FIin],FIout).

b5par_at_parse_fillers(UCi::N,(c,0),VKT,[OK|VKT],UCi,FIin,[object/UCi/[N]/[class/N]/[Key]/ (c,0) /OK|FIin]) :-
	var(UCi),
	atom(N),!,
	b5st_get_class(N,(c,0),Key,_).


b5par_at_parse_fillers(UCi::T,(c,0),VKT,[OK|VKT],UCi,FIin,FIout) :-
	var(UCi),!,
	b5par_at_parse_term(T,(c,0),[],VKTX,Def,[],Names,[object/UCi/Def/Names/VKTX/ (c,0) /OK|FIin],FIout).

b5par_at_parse_fillers(someknown(_),(c,0),_,_,_,_,_) :-
	!,t5out_error('Illegal FillerExpression: someknown.'),fail.

b5par_at_parse_fillers(allknown(C),(c,0),VKTin,VKTout,AllNames,FI,FI) :-
	!,
	function3(C, nfi,Result),
	(Result = [],!,VKTout = VKTin;
	    VKTout = Result),
	b5par_all_obj_names(Result,AllNames).

b5par_at_parse_fillers(theknown(C),(c,0),VKT,[FKey|VKT],Name,FI,FI) :-
	!,
	b5par_obj_name2key(theknown(C),(c,0),FKey),
	b5st_get_instance(Name,(c,0),FKey,_).


b5par_at_parse_fillers(Filler1 and Filler2,Type,VKTin,VKTout,Def1 and Def2,FIin,FIout) :-
	!,
	b5par_at_parse_fillers(Filler1,Type,VKTin,VKT1,Def1,FIin,FI1),
	b5par_at_parse_fillers(Filler2,Type,VKT1,VKTout,Def2,FI1,FIout).

b5par_at_parse_fillers(Object,Type,VKT,[OK|VKT],Object,FI,FI) :-
	b5st_get_instance(Object,Type,OK,_),!.

b5par_at_parse_fillers(Object,Type,VKT,[OK|VKT],Object,FI,[object/Object/[Name]/[class/Name]/[Key]/Type/OK|FI]) :-
	b5sta_check_flag(introduction,forward),
	b5st_get_domain(Name,Type,Key,open,_),!.

b5par_at_parse_fillers(Object,_,_,_,_,_,_) :-
	!,
	t5out_error(unknown(Object)),
	fail.
%%%%%%
	
function3(Term,NFX,Keys) :-
	b5par_aa_parse(Term,(c,0),[],FI,BQ,RQ),  %% for abstract objects only
	b5par_forward_introduction(FI,old),
	b5par_keyterm_to_protoform(BQ,(c,0),CPF),
	a5ret_retrieve(CPF,RQ,NFX,Keys).
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		ABOX REVISION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%----------------------------------------------------------------------
%%      redescribe
%%----------------------------------------------------------------------

b5par_abox_redescribe(Name::Term) :-
        b5par_object_description(Name,(c,0),ObjID),
        a5rev_redescribe_object(ObjID, Term).

%%----------------------------------------------------------------------
%%      forget(Obj :: Description)
%%      forget(Obj)
%%----------------------------------------------------------------------

b5par_abox_forget(Obj :: RetrDesc) :-
        !, ground(RetrDesc),
        b5par_object_description(Obj, (c,0), ObjID),
	a5rev_forget_description(ObjID, RetrDesc).
b5par_abox_forget(Obj) :-
	b5par_object_description(Obj,(c,0),ObjID),
	a5rev_forget_object(ObjID), !.
b5par_abox_forget(Obj) :-
	t5out_error(cannot_forget(Obj)),
        !, fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%				ABOX ASK
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b5par_abox_ask(Object,Name,IBoxMode) :-
	b5par_obj_name2key(Object,(c,0),ObjKey),!,
	(b5st_get_class(Name,(c,0),Key,_),!,
	 b5par_selector(IBoxMode,Selector),
	 a5ret_key_instantiates_p(Key,Selector,ObjKey);
	 b5par_dnf(Name,DNF),
	 b5par_handle_dnf(DNF,Object,(c,0),IBoxMode,aa)).

b5par_abox_ask(Object,Name,IBoxMode) :-
	b5par_obj_name2key(Object,Type,ObjKey),
	b5st_get_class(Name,Type,Key,_),
	b5par_selector(IBoxMode,_Selector),
	b5inst_is_instance_p(ObjKey,Key).  %% Selector ??
		 

b5par_handle_dnf(Term1 or Rest,Object,Type,IBoxMode,Origin) :-
	!,
	b5par_aa_parse(Term1,(c,0),[],FI,BQ,RQ),
	b5par_forward_introduction(FI,old),
	b5par_instance(Object,Type,BQ,RQ,Rest,IBoxMode,Origin).

b5par_handle_dnf(Term,Object,Type,IBoxMode,aa) :-
	!,
	b5par_aa_parse(Term,(c,0),[],FI,BQ,RQ),
	b5par_forward_introduction(FI,old),
	b5par_keyterm_to_protoform(BQ,(c,0),CPF),
	b5par_selector(IBoxMode,Selector),
	(var(Object),!,
	a5ret_retrieve(CPF,RQ,Selector,IKL),
	b5par_object_from_ikl(Object,Type,IKL);
	b5st_get_instance(Object,Type,Key,_),
	a5ret_instantiates_p(CPF,RQ,Selector,Key)).

b5par_handle_dnf(Term,Object,(c,0),_,at) :-
	b5par_aa_parse(Term,(c,0),[],FI,BQ,RQ),
	b5par_forward_introduction(FI,old),
	b5par_keyterm_to_protoform(BQ,(c,0),CPF),
	(a5ret_retrieve(CPF,RQ,nfi,[ObjectKey]),!;
	    atom(Object),
	    a5ret_retrieve(CPF,RQ,nfi,[])),
	b5st_get_instance(Object,(c,0),ObjectKey,_).

b5par_instance(Object,Type,BQ,RQ,Rest,IBoxMode,aa) :-
	!,
	b5par_keyterm_to_protoform(BQ,(c,0),CPF),
	b5par_selector(IBoxMode,Selector),
	a5ret_retrieve(CPF,RQ,Selector,IKL),
	(b5par_object_from_ikl(Object,Type,IKL),!;
	 b5par_handle_dnf(Rest,Object,Type,IBoxMode,aa)).

b5par_instance(Object,(c,0),BQ,RQ,Rest,IBoxMode,at) :-
	b5par_keyterm_to_protoform(BQ,(c,0),CPF),
	a5ret_retrieve(CPF,RQ,nfi,Erg),
	(Erg == [],!;
	    Erg = [ObjectKey],
	    b5st_get_instance(Object,(c,0),ObjectKey,_)),
	 b5par_handle_dnf(Rest,Object,(c,0),IBoxMode,at).




b5par_aa_parse(T,Type,FIi,FIo,BQ,RQ) :-
	b5par_separate(T,B,R),
	b5par_aa_parse_bq(B,Type,BQ,FIi,FI1),
	b5par_aa_parse_rq(R,FI1,FIo,RQ),
	(RQ == [],!
    ;
	Type = (c,0)).



b5par_aa_parse_bq([],_,[],FI,FI) :- !.
b5par_aa_parse_bq(B,Type,BQ,FIin,FIout) :-
	b5par_main(B,no_unknown_names,Type,[],[],BQ,_,_,FIin,FIout).




b5par_aa_parse_rq([],FI,FI,[]) :- !.   %%FFS: legal role terms??

b5par_aa_parse_rq(R1 and R2,FIi,FIo,E3) :-
	!,
	b5par_aa_parse_rq(R1,FIi,FI1,E1),
	b5par_aa_parse_rq(R2,FI1,FIo,E2),
	append(E1,E2,E3).

b5par_aa_parse_rq(R:F,FI,FI2,[RK:FKE]) :-
	atom(R),!,
	b5st_get_class(R,(r,0),RK,_),
	b5par_determine_role_range(R,RT),
	b5par_aa_parse_fillers(F,RT,FKE,FI,FI2).


b5par_aa_parse_rq(inv(R):F,FI,FI2,[RK:FKE]) :-
	atom(R),
	b5par_main(inv(R),no_unknown_names,_,[],[],RVKT,_,_,[internal/RVKT/ (r,0) /RK|FI],FI1),
	b5par_determine_role_range(R,RT),
	b5par_aa_parse_fillers(F,RT,FKE,FI1,FI2).


b5par_aa_parse_fillers(F1 and F2,Type,FKE1 and FKE2,FIi,FIo) :-
	!,
	b5par_aa_parse_fillers(F1,Type,FKE1,FIi,FI1),
	b5par_aa_parse_fillers(F2,Type,FKE2,FI1,FIo).

b5par_aa_parse_fillers(F1 or F2,Type,FKE1 or FKE2,FIi,FIo) :-
	!,
	b5par_aa_parse_fillers(F1,Type,FKE1,FIi,FI1),
	b5par_aa_parse_fillers(F2,Type,FKE2,FI1,FIo).

b5par_aa_parse_fillers(close(F),Type,close(FKE),FIi,FIo) :-
	!,
	b5par_aa_parse_fillers(F,Type,FKE,FIi,FIo).

b5par_aa_parse_fillers(allknown(C),Type,allknown(CPF,RQ),FIi,FIo) :-
	!,b5par_aa_parse(C,Type,[bq2proto/BQ/CPF|FIi],FIo,BQ,RQ).

b5par_aa_parse_fillers(theknown(F),Type,theknown(CPF,RQ),FIi,FIo) :-
	!,
	b5par_aa_parse(F,Type,[bq2proto/BQ/CPF|FIi],FIo,BQ,RQ).

b5par_aa_parse_fillers(someknown(F),Type,someknown(CPF,RQ),FIi,FIo) :-
	!,
	b5par_aa_parse(F,Type,[bq2proto/BQ/CPF|FIi],FIo,BQ,RQ).

b5par_aa_parse_fillers(Value,Type,Key,FI,FI) :-
	b5st_get_instance(Value,Type,Key,_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ABOX INIT (Symbol Table update only)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b5par_abox_init(Objects,Values) :-
	collect_needed_info(Objects,Values,AssertList),
	b5st_instance_init,
	reassert_facts(AssertList).


collect_needed_info([],[],[]) :- !.

collect_needed_info([],KeyList,AssertList) :-
	!,
	collect_needed_info(KeyList,[],AssertList).

collect_needed_info([Key|KeyList1],KeyList2,[Name/Type/Key|AssertList]) :-
	b5st_get_instance(Name,Type,Key,_),
	collect_needed_info(KeyList1,KeyList2,AssertList).

reassert_facts([]) :- !.

reassert_facts([Name/Type/Key|Rest]) :-
	reassert_object(Type,Name,Key),
	reassert_facts(Rest).

reassert_object((c,0),Name,Key) :-
	!,
	b5st_add_instance(Name,(c,0),Key,([],[])).

reassert_object(Type,Name,Key) :-
	!,
	b5st_add_instance(Name,Type,Key,no_definition).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IBOX TELL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b5par_iboxtell(Term1,Term2) :-
	b5par_ibox_tell(Term1,Term2,Key),
	a5rev_update_ibox(new,Key).

b5par_ibox_tell(Term1,Term2,Key1) :-
	b5par_main(Term1,normal,(c,0),[],[],KeyTerm1,[],Names1,[],FI1),
	b5par_main(Term2,normal,(c,0),[],[],KeyTerm2,[],Names2,[],FI2),
	b5par_forward_introduction(FI1,_),
	b5par_forward_introduction(FI2,_),
	t5fil_filter(ibox_lhs,Filter1),
	t5fil_filter(internal,Filter2),
	b5par_tt_to_kernel((c,0),KeyTerm1,_,Filter1,Key1,_,def),
	b5par_tt_to_kernel((c,0),KeyTerm2,_,Filter2,Key2,_,def),
	i5ibox_add_link(Key1,Key2,LinkNumber),
	append(Names1,Names2,Names),
	t5dm_uses(link/LinkNumber,Names),						(b5sta_check_flag(iboxfilled,true),!
    ;
	b5sta_set_flag(iboxfilled,true)),
	b5st_add_ilink(LinkNumber,Term1 => Term2).

b5par_ibox_forget(LHS,RHS) :-
	(b5st_del_ilink(N,LHS => RHS),!
    ;
	t5out_error(ilink_unretractable)),
	i5ibox_del_link(N,Key),
	(b5st_get_ilink(_,_),!
    ;
	b5sta_set_flag(iboxfilled,false)),
	a5rev_update_ibox(forget,Key).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MACRO DEFINITIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Macro definitions consist of the macro, which is a skeletal predicate
% definition and a term, which must be syntactically correct wrt BACK V.
% Every variable occuring in the term must be bound by an occurence in the
% macro.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b5par_macro_tell(Macro,_) :-
	b5st_get_macro(Macro,_),!,
	t5out_error(not_implemented('macro revision')),fail.

b5par_macro_tell(Macro,Definition) :-
	compound(Macro),
	\+ b5par_inconsistent_macro(Macro,Definition),
	b5st_add_macro(Macro,Definition).

b5par_inconsistent_macro(Macro,Definition) :-
	b5par_instantiate_macro(Macro),
	\+ b5par_analyze_term(Definition,normal,_,[],[],_,[],_,[],_).

b5par_instantiate_macro(Macro) :-
	functor(Macro,_,Ari),
	b5par_instantiate_args(Ari,Macro).

b5par_instantiate_args(0,_) :- !.
b5par_instantiate_args(N,Macro) :-
	arg(N,Macro,Arg),
	Arg = macrodummy,
	M is N - 1,
	b5par_instantiate_args(M,Macro).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TESTS and MISCELLANEOUS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% names must be ground, atomic and unprotected
b5par_valid_name(Name) :-
	var(Name),
	!,t5out_error(wrong_name(Name)),fail.
b5par_valid_name(Name) :-
	b5par_protected_name(Name),
	!,t5out_error(wrong_name(Name)),fail.
b5par_valid_name(Name) :-
	atom(Name),!.
b5par_valid_name(Name) :-
	t5out_error(wrong_name(Name)),fail.


%% the names of built-in concepts and some keywords may not be redefined
b5par_protected_name(all).
b5par_protected_name(nothing).
b5par_protected_name(anything).
%b5par_protected_name(anyrole).
b5par_protected_name(thetopmostrole).
b5par_protected_name(number).
b5par_protected_name(aset).
b5par_protected_name(string).
b5par_protected_name(attribute_domain).


b5par_objname_test(ObjName) :-
	var(ObjName),!.
b5par_objname_test([]) :-     %% [] is a prolog atom
	!,t5out_error((objname,[])),
	fail.
b5par_objname_test(ObjName) :-
	atom(ObjName),!.
b5par_objname_test(ObjName) :-
	t5out_error((objname,ObjName)),
	!,fail.



%% converts the modifier to a type 
b5par_tt_analyze_definition(Term type concept,Term,(c,0),norole) :- !.
b5par_tt_analyze_definition(Term type role,Term,(r,0),role) :- !.
b5par_tt_analyze_definition(Term type feature,Term,(r,0),feature) :- !.
%b5par_tt_analyze_definition(Term type inherent_feature,Term,(r,0),inherent_feature) :- !.
b5par_tt_analyze_definition(Term type attribute,Term,(a,_),norole) :- !.
b5par_tt_analyze_definition(Term type number,Term,(n,0),norole) :- !.
b5par_tt_analyze_definition(Term type string,Term,(s,0),norole) :- !.
b5par_tt_analyze_definition(Term,Term,_,_).


%% Only abstract concepts and roles may be introduced as primitive
b5par_prim_type_test((s,0)) :- 
	!,t5out_error(string_error),fail.
b5par_prim_type_test((n,0)) :-
	!,t5out_error(wrong_primdef),fail.
b5par_prim_type_test((a,_)) :-
	!,t5out_error(wrong_primdef),fail.
b5par_prim_type_test(_).


%% Concepts of Type 'string' may not be defined
b5par_def_type_test(T) :-
	T == (s,0),
	!,t5out_error(string_error),fail.
b5par_def_type_test(_).


%% new definition must be of same type as old definition when revising TBox
b5par_revision_type_check((a,N),(a,N),aset) :- !.
b5par_revision_type_check((n,0),(n,0),number) :- !.
b5par_revision_type_check((c,0),(c,0),conc) :- !.
b5par_revision_type_check((r,0),(r,0),role) :- !.
b5par_revision_type_check(_,_,changedtype) :- !,t5out_error(no_type_change),fail.


%% TBox definitions may not contain cycles.
b5par_cycle_check(Name,PN) :-
	member(Name,PN),!,
	t5out_error(cycle),fail.
b5par_cycle_check(_,_).


%% ABox Expressions may not complex roles unless they're equivalent to some
%% Expression that's been introduced before (Status=old)
b5par_compare_status(old,_) :- !.
b5par_compare_status(new,Status) :- var(Status),!.
b5par_compare_status(new,old) :- t5out_error(complex_role),!,fail.


b5par_number_test(N) :-
	integer(N).



b5par_determine_role_range(domain(_) and Rest,Range) :-
	!,b5par_determine_role_range(Rest,Range).
b5par_determine_role_range(Role and _,Range) :-
	!,b5par_determine_role_range(Role,Range).
b5par_determine_role_range(range(C),Range) :-
	b5par_analyze_term(C,_,Range,[],[],_,[],_,[],_),
	ground(Range),!,
	Range \== (r,0).
b5par_determine_role_range(range(_),(c,0)) :- !.
b5par_determine_role_range(not(_),(c,0)) :- !.
b5par_determine_role_range(inv(_),(c,0)) :- !.
b5par_determine_role_range(trans(Role),Range) :-
	!,b5par_determine_role_range(Role,Range).
b5par_determine_role_range(_ comp Role2,Range) :-
	!,b5par_determine_role_range(Role2,Range).
b5par_determine_role_range(_.Role2,Range) :-
	!,b5par_determine_role_range(Role2,Range).
b5par_determine_role_range(Name,Range) :-
	atom(Name),
	b5st_get_class(Name,(r,0),Key,_),
	!,
	t5role_range_type_key(Key,RangeKey),
	b5st_get_domain(_,Range,RangeKey,_,_),!.
b5par_determine_role_range(Name,(c,0)) :-
	atom(Name).


% Rollenterme nur aus domain und/oder range sind verboten.

b5par_correct_role_term(Arg) :-
	b5par_correct(Arg),!.
b5par_correct_role_term(Arg) :-
	t5out_error(wrong_role(Arg)),fail.


b5par_correct(inv(_)) :- !.
b5par_correct(trans(_)) :- !.
b5par_correct(comp(_,_)) :- !.
b5par_correct(and(Role1,Role2)) :-
	!,(b5par_correct(Role1)
    ;	
	b5par_correct(Role2)),!.
b5par_correct(domain(_)) :- !,fail.
b5par_correct(range(_)) :- !,fail.
b5par_correct(_).



b5par_extract_attributes(Name1,Name2,Liste,Erg,Type) :-
	b5par_find_first(Name1,Liste,Out),
	b5par_find_second(Name2,Out,Erg,Type).

b5par_find_first(_,[],_) :- !,fail.
b5par_find_first(X,[X|Rest],[X|Rest]) :- !.
b5par_find_first(X,[_|Rest],Erg) :-
	b5par_find_first(X,Rest,Erg).

b5par_find_second(_,[],_,_) :- !,fail.
b5par_find_second(X,[X|_],[Key],Type) :- 		
	b5st_get_instance(X,Type,Key,_),!. 
b5par_find_second(X,[Y|Rest],[Key|Erg],Type) :-
	b5st_get_instance(Y,Type,Key,_),!,
	b5par_find_second(X,Rest,Erg,Type).

b5par_all_obj_names([],[]) :- !.
b5par_all_obj_names([O|R],[Name|L]) :-
	b5st_get_instance(Name,_,O,_),!,
	b5par_all_obj_names(R,L).

b5par_member(_,[]) :- !,fail.
b5par_member(X,[Y|_]) :-
	X == Y,!.
b5par_member(X,[_|R]) :-
	b5par_member(X,R).


b5par_uc(uc(N),M) :-
	(integer(N);integer(M)),!,
	N = M.
b5par_uc(O,Key) :-
	( atom(O) ->
	    atom_chars(O,L),
	    L = [117,99,95|R],
	    number_chars(Key,R)
	;
	integer(Key),
	number_chars(Key,R),
	L = [117,99,95|R],
	atom_chars(O,L)).

b5par_selector(ibox,nfi).
b5par_selector(noibox,nfs).

b5par_dnf(or(A,B), or(B5par_dnfA,B5par_dnfB)) :-
	!,
	b5par_dnf(A, B5par_dnfA),
	b5par_dnf(B, B5par_dnfB).
b5par_dnf(and(A,or(B,C)), or(B5par_dnfAB,B5par_dnfAC)) :-
	!, 
	b5par_dnf(and(A,B), B5par_dnfAB),
	b5par_dnf(and(A,C), B5par_dnfAC).
b5par_dnf(and(or(A,B),C), B5par_dnf) :-				
	!, b5par_dnf(and(C,or(A,B)), B5par_dnf).
b5par_dnf(and(A,B), B5par_dnf) :-
	!,
	b5par_dnf(A, B5par_dnfA),
	b5par_dnf(B, B5par_dnfB),
	( B5par_dnfA = A, B5par_dnfB = B ->
		B5par_dnf = and(B5par_dnfA, B5par_dnfB)
	; b5par_dnf(and(B5par_dnfA,B5par_dnfB), B5par_dnf) ).
b5par_dnf(A,A).

b5par_object_from_ikl(Object,Type,IKL) :-
	(var(Object) -> 
	    member(Key,IKL),
	    b5st_get_instance(Object,Type,Key,_)
	;   b5st_get_instance(Object,Type,Key,_),!,
	     memberchk(Key,IKL)).



b5par_separate(A and B,BT,RT) :-
	!,
	b5par_separate(A,BTa,RTa),
	b5par_separate(B,BTb,RTb),
	b5par_sep_result(BTa,BTb,BT),
	b5par_sep_result(RTa,RTb,RT).
b5par_separate(R:F,[],R:F) :- !.
b5par_separate(A,A,[]).


b5par_sep_result([],[],[]) :- !.
b5par_sep_result([],A,A) :- !.
b5par_sep_result(A,[],A) :- !.
b5par_sep_result(A,B,A and B).


b5par_concat(StringA, StringB, String) :-
	name(StringA, ListA),
	name(StringB, ListB),
	name('/',[N]),
	append(ListA, [N|ListB], List),
	name(String, List),!.


b5par_tbox_rev_dump :-
	b5sta_check_flag(tboxdumpfile,File),
	b5sta_check_flag(tboxdumpdir,Dir),
	b5par_concat(Dir,File,DumpFile),
	save_predicates([
			t5tbox_key/1,
			b5st_domain_counter_db/1,
			b5st_domain_db/5,
			b5st_class_db/4,
			b5st_instance_db/4,
			b5st_ilink_db/2,
			t5rdb/7,
			t5cdb/2,
			t5dom/3,
			t5dom_allprims/1,
			t5hc_infimum_hierarchy/3,
			t5hc_role_hierarchy/4,
			t5hc_conc_hierarchy/4,
			t5hc_subsumption_hierarchy/4,
			t5dm_trans_used_by_relation/2,
			t5dm_trans_uses_relation/2,
			t5dm_used_by_relation/2,
			t5dm_uses_relation/2
			],DumpFile).

b5par_tbox_rev_load :-
	load_files('tboxrev.dmp'),
	t5out_error(tbox_rev_fail),
	!,fail.

b5st_name_type_key(UserName,Type,Key,SysName) :-
	name(UserName,L),
	(L = [117,99,95|R] ->
	    name(Key,R),
	    (integer(Key) ->
		(Type == (c,0) ->
		    (b5st_get_instance(SysName,Type,Key,_) ->
			true
		    ;
		    SysName = UserName)
		;
		t5out_error(st(ntk(type_mismatch(uc,Type)))))
	    ;
	    t5out_error(st(ntk(illegal_uc(UserName)))))
	;
	fail),!.
b5st_name_type_key(Name,_,_,Name) :-
	atom(Name),!.
b5st_name_type_key(UserName,Type,Key,SysName) :-
	(functor(UserName,Functor,N) ->
	    (N == 1 ->
		(b5st_functor_type(Functor,FunctorType) ->
		    (Type == FunctorType ->
			(arg(1,UserName,Key) ->
			    (b5st_get_instance(SysName,Type,Key,_) ->
				true
			    ;
			    (b5par_key_exists_p(Functor,Key) ->
				true
			    ;
			    t5out_error(st(ntk(illegal_key)))))
			;
			t5out_error(st(ntk(key_mismatch(UserName,Key)))))
		    ;
		    t5out_error(st(ntk(type_mismatch(Functor,Type)))))
		;
		t5out_error(st(ntk(illegal_functor(Functor)))))
	    ;
	    t5out_error(st(ntk(illegal_name(UserName)))))
	;
	fail),!.

b5st_functor_type(conc,(c,0)) :- !.
b5st_functor_type(obj,(c,0)) :- !.
b5st_functor_type(uc,(c,0)) :- !.
b5st_functor_type(role,(r,0)) :- !.
b5st_functor_type(Domain,Type) :-
	b5st_get_domain(Domain,Type,_,_,_).

b5par_key_exists_p(obj,Key) :-
	!,b5kif_key_exists_p(obj,Key).
b5par_key_exists_p(Functor,Key) :-
	b5st_functor_type(Functor,Type),
	b5kif_key_exists_p(Type,Key).


/* this one is for an entry for nothing in every domain */
b5st_new_nothing(Type) :- %uk
	t5tbox_nothing_key(Nothing),
	b5st_add_class(nothing,Type,Nothing,internal).

b5par_update_entry(object,(Name,Type,Key,Def)) :-
	(b5st_del_instance(Name,Type,Key,_),!;true),
	b5st_add_instance(Name,Type,Key,Def).

b5par_object_description(Name,Type,Key) :-
	ground(Key),!,
	( b5st_get_instance(Name,Type,Key,_) ->
	      true
	; t5out_error(obj_does_not_exist(Key)),
	  !, fail).
b5par_object_description(Name,Type,Key) :-
	ground(Name),!,
	( b5par_obj_name2key(Name,Type,Key) ->
	      true
	; t5out_error(unknown(Name)),
	  !, fail).

b5par_obj_name2key(theknown(Term),(c,0),Key) :-
	!,
	ground(Term),
	(b5st_get_class(Term,_,ClassKey,_),!,
	 b5par_selector(IBoxMode,Selector),
	 a5ret_key_retrieve(ClassKey,Selector,[Key]);
	 b5par_dnf(Term,DNF),
	 b5par_handle_dnf(DNF,Object,(c,0),IBoxMode,at)),
	 b5st_get_instance(Object,(c,0),Key,_).
b5par_obj_name2key(Name,Type,Key) :-
	b5par_uc(Name,Key),
	!,
	b5st_get_instance(uc(Key),Type,Key,_).
b5par_obj_name2key(Name,Type,Key) :-
	findall(Key,b5st_get_instance(Name,Type,Key,_),[Key]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% END OF FILE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 				SYMBOL TABLE				       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile b5dump_these_predicates/1.

b5dump_these_predicates([b5st_class_db/4,
			 b5st_instance_db/4,
			 b5st_domain_db/5,
			 b5st_domain_counter_db/1,
			 b5st_macro_db/2,
			 b5st_ilink_db]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b5st_init :-
	b5st_class_init,
	b5st_instance_init,
	b5st_domain_init,
	b5st_domain_counter_init,
	b5st_macro_init,
	b5st_ilink_init.

b5st_class_init :-
	b5st_del_all_class(_,_,_,_).

b5st_instance_init :-
	b5st_del_all_instance(_,_,_,_).

b5st_domain_init :-
	b5st_del_all_domain(_,_,_,_,_).

b5st_domain_counter_init :-
	b5st_set_domain_counter(1).

b5st_macro_init :-
	b5st_del_all_macro(_,_).

b5st_ilink_init :-
	b5st_del_all_ilink(_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b5st_set_domain_counter(N) :-
	retractall(b5st_domain_counter_db(_)),
	assert(b5st_domain_counter_db(N)).

b5st_dec_domain_counter(OldValue,NewValue) :-
	retract(b5st_domain_counter_db(OldValue)),
	NewValue is OldValue-1,
	assert(b5st_domain_counter_db(NewValue)).

b5st_inc_domain_counter(OldValue,NewValue) :-
	retract(b5st_domain_counter_db(OldValue)),
	NewValue is OldValue+1,
	assert(b5st_domain_counter_db(NewValue)).

b5st_next_domain_counter(NextCounter) :-
	b5st_inc_domain_counter(_,NextCounter).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b5st_add_ilink(Reference,ILink) :-
	(nonvar(Reference) ->
	    (nonvar(ILink) ->
		assert(b5st_ilink_db(Reference,ILink))
	    ;
	    t5out_error(st(var(ilink))))
	;
	t5out_error(st(var(reference)))).

b5st_get_ilink_bt(Reference,ILink) :-
	b5st_ilink_db(Reference,ILink).

b5st_get_ilink(Reference,ILink) :-
	b5st_ilink_db(Reference,ILink),!.

b5st_del_ilink(Reference,ILink) :-
	retract(b5st_ilink_db(Reference,ILink)).

b5st_del_all_ilink(Reference,ILink) :-
	retractall(b5st_ilink_db(Reference,ILink)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b5st_add_macro(MacroName,MacroExpansion) :-
	(nonvar(MacroName) ->
	    (nonvar(MacroExpansion) ->
		assert(b5st_macro_db(MacroName,MacroExpansion))
	    ;
	    t5out_error(st(var(macro))))
	;
	t5out_error(st(var(macroname)))).

b5st_get_macro_bt(MacroName,MacroExpansion) :-
	b5st_macro_db(MacroName,MacroExpansion).

b5st_get_macro(MacroName,MacroExpansion) :-
	b5st_macro_db(MacroName,MacroExpansion),!.

b5st_del_macro(MacroName,MacroExpansion) :-
	b5st_del_macro_dt(MacroName,MacroExpansion).

b5st_del_all_macro(MacroName,MacroExpansion) :-
	retractall(b5st_macro_db(MacroName,MacroExpansion)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b5st_add_class(Name,Type,Key,Definition) :-
	(nonvar(Name) ->
	    (nonvar(Type) ->
		(nonvar(Key) ->
		    (nonvar(Definition) ->
			assert(b5st_class_db(Name,Type,Key,Definition))
		    ;
		    t5out_error(st(var(definition))))
		;
		t5out_error(st(var(key))))
	    ;
	    t5out_error(st(var(type))))
	;
	t5out_error(st(var(name)))).

b5st_get_class_bt(Name,Type,Key,Definition) :-
	b5st_class_db(Name,Type,Key,Definition).

b5st_get_class(Name,Type,Key,Definition) :-
	b5st_class_db(Name,Type,Key,Definition),!.

b5st_del_class(Name,Type,Key,Definition) :-
	retract(b5st_class_db(Name,Type,Key,Definition)).

b5st_del_all_class(Name,Type,Key,Definition) :-
	retractall(b5st_class_db(Name,Type,Key,Definition)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b5st_add_instance(Name,Type,Key,Definition) :-
	(nonvar(Name) ->
	    (nonvar(Type) ->
		(nonvar(Key) ->
		    (nonvar(Definition) ->
			assert(b5st_instance_db(Name,Type,Key,Definition))
		    ;
		    t5out_error(st(var(definition))))
		;
		t5out_error(st(var(key))))
	    ;
	    t5out_error(st(var(type))))
	;
	t5out_error(st(var(name)))).

b5st_get_instance_bt(Name,Type,Key,Definition) :-
	b5st_instance_db(Name,Type,Key,Definition).

b5st_get_instance(Name,Type,Key,Definition) :-
	b5st_instance_db(Name,Type,Key,Definition),!.

b5st_del_instance(Name,Type,Key,Definition) :-
	retract(b5st_instance_db(Name,Type,Key,Definition)).

b5st_del_all_instance(Name,Type,Key,Definition) :-
	retractall(b5st_instance_db(Name,Type,Key,Definition)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b5st_add_domain(Name,Type,Key,Flag,Attributes) :-
	(nonvar(Name) ->
	    (nonvar(Type) ->
		(nonvar(Key) ->
		    (nonvar(Flag) ->
			(nonvar(Attributes) ->
			    assert(b5st_domain_db(Name,Type,Key,Flag,Attributes))
			;
			t5out_error(st(var(attributes))))
		    ;
		    t5out_error(st(var(flag))))
		;
		t5out_error(st(var(key))))
	    ;
	    t5out_error(st(var(type))))
	;
	t5out_error(st(var(name)))).

b5st_get_domain_bt(Name,Type,Key,Flag,Attributes) :-
	b5st_domain_db(Name,Type,Key,Flag,Attributes).

b5st_get_domain(Name,Type,Key,Flag,Attributes) :-
	b5st_domain_db(Name,Type,Key,Flag,Attributes),!.

b5st_del_domain(Name,Type,Key,Flag,Attributes) :-
	retract(b5st_domain_db(Name,Type,Key,Flag,Attributes)).

b5st_del_all_domain(Name,Type,Key,Flag,Attributes) :-
	retractall(b5st_domain_db(Name,Type,Key,Flag,Attributes)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b5st_class_entry(Name,Type,Key,Definition) :-
	b5st_get_class_bt(Name,Type,Key,Definition).

b5st_class_entry_dt(Name,Type,Key,Definition) :-
	b5st_get_class(Name,Type,Key,Definition).

b5st_object_entry(Name,Type,Key,Definition) :-
	b5st_get_instance_bt(Name,Type,Key,Definition).

b5st_object_entry_dt(Name,Type,Key,Definition) :-
	b5st_get_instance(Name,Type,Key,Definition).

b5st_domain_entry(Name,Type,Key,Flag,Attributes) :-
	b5st_get_domain_bt(Name,Type,Key,Flag,Attributes).

b5_ilink_def(Reference,ILink) :-
	b5st_get_ilink_bt(Reference,ILink).

b5_ilink_def_dt(Reference,ILink) :-
	b5st_get_ilink(Reference,ILink).

b5st_macro_entry(MacroName,MacroExpansion) :-
	b5st_get_macro_bt(MacroName,MacroExpansion).

b5st_macro_entry_dt(MacroName,MacroExpansion) :-
	b5st_get_macro(MacroName,MacroExpansion).

b5st_objkey_userdef(Obj, UserDef) :-
	b5st_get_instance(_,_,Obj,UserDef).

b5st_objname_userdef(Obj, UserDef) :-
	b5st_get_instance(Obj,_,_,UserDef).

b5st_objname_key(ObjName, Key) :-
	b5st_get_instance(ObjName,_,Key,_).

b5st_obj_delete_entry(Obj) :-
	b5st_del_all_instance(Obj,_,_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 				END OF FILE				       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Die Klauseln die eine Ausgabe erzeugen muss jedeR selbst schreiben.

% Errors z.B. werden mit t5out_error(ErrorTyp) erzeugt.

% ErrorTyp besteht immer aus einem Funktor, eben dem Fehlertyp, und evtl.
% Argumenten, die mit ausgegeben werden, oder aus denen eine Ausgabe
% generiert wird (z.B. Namen aus Keys).
% Es kann auch sein, dass abhaengig von den Argumenten eine oder keine 
% Ausgabe erfolgen soll.

% Fuer die anderen Ausgabetypen gilt alles analog.

% Weitere Einzelheiten stehen unten, wo die Klauseln eingetragen werden
% sollen. Tragt die Klauseln bitte nach dem Text ein und gruppiert sie 
% nach Modulen.

% Ihr koennt auch in "~fischli/microback/qsystem/outger" reinschauen.


% **********************************************************************
% *             TEST ERRORTYP                                          *
% **********************************************************************

t5out_test_panic :- !.

t5out_test_error :-
	b5sta_flag(verbosity,V),
	\+ V == silent.

t5out_test_warning :-
	b5sta_flag(verbosity,V),
	\+ V == silent,		
	\+ V == error.

t5out_test_info :-
	b5sta_flag(verbosity,V),
	(V == info,!
	;
	V == trace).

t5out_test_trace :-
	b5sta_flag(verbosity,V),
	V == trace.

% **********************************************************************
% **********************************************************************

% **********************************************************************
% *                write predicates                                    *
% **********************************************************************

t5out_writelist(_,[]) :- 
	nl,
	!.

t5out_writelist(_,[nl(0)|List]) :-
	!,
	t5out_writelist(_,List).

t5out_writelist(_,[nl(N)|List]) :-
	!,
	nl,
	N_minus_1 is N - 1,
	t5out_writelist(_,[nl(N_minus_1)|List]).

t5out_writelist(_,[quote(H)|List]) :-
	!,
	writeq(H),
	t5out_writelist(_,List).


t5out_writelist(_,[H|List]) :-
	write(H),
	t5out_writelist(_,List).

% **********************************************************************
% *                   PANIC                                            *
% **********************************************************************

t5out_panic(ErrorTyp) :-
	( t5out_test_panic ->
	    t5out_panic_output(ErrorTyp)
	; true).

% Hier alles eintragen, was system in panic versetzt
% das sind z.b. preconditions und postconditions,
% die gelten *m"ussen*

t5out_panic_output(noconcept(X,Key)) :-  
	t5out_write_panic,
	t5out_writelist(_,[quote(X),' called but key points to no concept',
	' Key = ',quote(Key)]), !.

t5out_panic_output(t5res_split(Res,I_Res)) :-
	t5out_write_panic,
	t5out_writelist(_,['t5res_split(',quote(Res),',',quote(I_Res),',_)']), 
	!.	
%% catchall

t5out_panic_output(PANIC) :-     % If there is no clause for the ErrorType
	!, t5out_write_panic,    % yet, then the ErrorType itself is written.
	write(PANIC),
	nl.

t5out_write_panic :-
	write('*** PANIC   ').


% **********************************************************************
% *                   ERRORS                                           *
% **********************************************************************

t5out_error(ErrorTyp) :-
	( t5out_test_error ->
	    t5out_error_output(ErrorTyp)
	; true).


% Hier sollen die Klauseln eingetragen werden, die ERRORS erzeugen !!
% Alle Klauseln, die eine Ausgabe erzeugen, beginnen mit 
% "t5out_write_error," und enden mit "nl, !.".

%%%%% okp



t5out_error_output(ambigue_flagname(Flag)) :-
	t5out_write_error,
	write(Flag),
	write(' is an ambigue flag name.'),
	nl,!.

t5out_error_output(no_valid_flagname(Flag)) :-
	t5out_write_error,
	write(Flag),
	write(' is not a valid flag name.'),
	nl,!.


t5out_error_output(ambigue_flagvalue(Flag)) :-
	t5out_write_error,
	write(Flag),
	write(' is an ambigue flag value.'),
	nl,!.

t5out_error_output(no_valid_flagvalue(Flag)) :-
	t5out_write_error,
	write(Flag),
	write(' is not a valid flag value.'),
	nl,!.

t5out_error_output(not_impl(Operator,_)) :-
	t5out_write_error,	
	write(Operator),
	write(' is not yet implemented. '),
	nl,!.


t5out_error_output(no_such_object) :-
	t5out_write_error,
	write('The given object does not exist.'),
	nl,!.

t5out_error_output(no_rev(Name,sys)) :-
	t5out_write_error,
	write(Name),
	write(' is a built-in concept and cannot be revised.'),
	nl,!.

t5out_error_output(no_rev(Name,ext)) :-
	t5out_write_error,
	write(Name),
	write(' is already defined but revision flag is zero.'),
	nl,!.


t5out_error_output(domain(Term,Action)) :-
	t5out_write_error,
	write(Term),
	write(Action),
	nl,!.


t5out_error_output(dom(Dom)) :-
	t5out_write_error,
	write('This is not a valid '),
	write(Dom),
	write(' definition.'),
	nl,!.

t5out_error_output(ambigue(O)) :-
	t5out_write_error,
	write(O),
	write(' is an ambiguous identifier.'),
	nl,!.

t5out_error_output(notype) :-
	t5out_write_error,
	write('The definition is ambiguous. No type could be infered.'),
	nl,!.

t5out_error_output(no_prim_def) :-
	t5out_write_error,
	write('Concrete concepts may not be defined as primitive.'),
	nl,!.

t5out_error_output(wrong_syntax(atom)) :-
	t5out_write_error,
	write('Names to be defined must be atoms.'),
	nl,!.

t5out_error_output(twice_fi(Name,Type1,Type2)) :-
	t5out_write_error,
	write(Name),
	write(' is to be forward introduced twice, with incompatible types.'),
	nl,
	t5out_write_error,
	write('First type:  '),
	write(Type1),
	write('   second type:  '),
	write(Type2),
	nl,!.
%%
t5out_error_output(tbox_rev_fail) :-
	t5out_write_error,
	write('TBox revision failed.'),nl,!.

t5out_error_output(instantiation(Arg)) :-
	t5out_write_error,
	write('Instantiation error.'),nl,
	write(Arg),write(' must be ground.'),nl,!.

t5out_error_output(unknown(T)) :-
	t5out_write_error,
	t5out_writelist(_, ['''', T, '''', 
	          ' does not refer to a known object.']),
	!.

t5out_error_output(wrong_type(T)) :-
	t5out_write_error,
	write(T),
	write(' is of wrong type.'),
	nl,!.

t5out_error_output(no_type_change) :-
	t5out_write_error,
	write('New definition must be of the same type'),nl,
	t5out_write_error,
	write('as previous definition.'),nl,!.

t5out_error_output(cycle) :-
	t5out_write_error,
	write('The definition contains a cycle somewhere.'),nl,!.

t5out_error_output(no_number(N)) :-
	t5out_write_error,
	write(N),
	write(' is not a number.'),
	nl,!.

t5out_error_output(complex_role) :-
	t5out_write_error,
	write('ABox Expressions may not contain complex role terms.'),nl,!.
%%
t5out_error_output(type(Obj,Type)) :-
	t5out_write_error,
	write('type_mismatch :'),
	b5inst_get(Obj,Inst),
	b5inst_type(Inst,T),
	b5inst_value(Inst,V),
	write('object ('),write(Obj),write(') = '),write(V),
	write('has type:'),write(T),write('and not :'),write(Type),
	nl,!.

t5out_error_output(f_type(Obj,VR)) :-
	t5out_write_error,
	write('instance error : '),
	b5st_object_entry(Obj_Name,_,Obj,_),!,
	b5st_class_entry(VR_Name,_,VR,_),!,
	write('object '),write(Obj_Name), 
	write(' (key:'),write(Obj),write(') '),
	write('is no instance of '),
	write(VR_Name),
	write(' (key:'),write(VR),write(') .'),
	nl,!.

t5out_error_output(sorry_no_definition) :-
	t5out_write_error,
	write('Sorry, no definition available.'),
	nl,!.

t5out_error_output(not_implemented('Use of role chains')) :-
	t5out_write_error,
	write('Sorry, use of role chains in output functions '), 
	write('not yet implemenented'),
	nl,!.

t5out_error_output(wrong_name(Name)) :-
	t5out_write_error,
	write(Name),
	write(' is not a valid name.'),
	nl,!.

t5out_error_output((objname,Name)) :-
	t5out_write_error,
	write(Name),
	write(' is not a valid object name.'),
	nl,!.

t5out_error_output(not_implemented(Action)) :-
	t5out_write_error,
	write('Sorry, '),
	write(Action),
	write(' is not yet implemenented.'),
	nl,!.


t5out_error_output(unspecified_obj_update) :-
	t5out_write_error,
	write('Object update not correctly specified.'),
	nl,!.

t5out_error_output(not_told(Obj, Desc)) :-
	t5out_write_error,
	t5out_writelist(_, [Desc, ' was not told for ', Obj, '.']),
	!.

t5out_error_output(cannot_forget(Obj)) :-
	t5out_write_error,
	t5out_writelist(_,['Cannot forget ', Obj, '.']),
	!.
	
t5out_error_output(abox_revision_not_possible) :-
	t5out_write_error,
	write('ABox revision is not possible.'),
	nl,!.

t5out_error_output(not_yet_implemented('Retrieve difference')) :-
	t5out_write_error,
	write('Sorry: computing the difference is not yet implemented.'),
	nl,!.

t5out_error_output(invalid_retrieve(Expr)) :-
	t5out_write_error,
	write('Invalid retrieve expression: '), write(Expr),
	nl,!.

t5out_error_output(empty_attribute_domain) :-
	t5out_write_error,
	write('empty attribute domains are not accepted '),
	nl,!.

t5out_error(downward_migration(ObjName, _Undesc)) :-
	t5out_write_error,
	t5out_writelist(_, ['Term revision conflict at object ', ObjName,
	                    nl(1), 
			    '          Old state restored.']),!.
			    

%% Module a5obj (uk)

t5out_error_output(a5obj_incoherent_p(ONFsel, ObjId)) :-
	t5out_write_error,
	b5st_object_entry(ObjName,_,ObjId,_),
	(ONFsel == nfi ->
	    ModeText = '          (I-Links have been applied to objects).'
	;   ModeText = '          (no I-Links have been applied).'),
	t5out_writelist(_,
		['Normal form of object ', ObjName,
		' is incoherent', nl(1), ModeText]),
		!.


%% catchall

t5out_error_output(ErrorTyp) :-  % If there is no clause for the ErrorTyp
	!, t5out_write_error,    % yet, then the ErrorTyp itself is written.
	write(ErrorTyp),
	nl.

t5out_write_error :-
	write('ERROR     ').


% **********************************************************************
% *                   WARNINGS                                           *
% **********************************************************************

t5out_warning(WarningTyp) :-
	( t5out_test_warning ->
	    t5out_write_warning, 
	    t5out_warning_output(WarningTyp)
	; true).


% Hier sollen die Klauseln eingetragen werden, die WARNINGS erzeugen !!

t5out_warning_output(only_tbox_init) :-
	write('It is not possible to initialize the TBox only.'),nl,
	t5out_write_warning,
	write('ABox and IBox will be initialized as well.'),
	nl,!.

t5out_warning_output(default_range(K1,K2)) :-
	key_view(K1,V1),
	key_view(K2,V2),
	write(default_range(V1,V2)),nl,!.

t5out_warning_output(close) :-
	write('The close operator will be ignored.'),
	nl,!.

t5out_warning_output(not_impl(rep_by_any,Operator)) :-
	write(Operator),
	write(' is not yet implemented. '),
	nl,
	t5out_write_warning,
	write('It''ll be replaced by ''anything'' in the concept definition.'),
	nl,!.

t5out_warning_output(not_impl(abox_query)) :-
	write('The ATL-Expression contains an AQL-Expression.'),nl,
	write('The AQL is not yet implemented.'),nl,
	write('Therefore, the subterm will be ignored. Sorry.'),
	nl,!.

t5out_warning_output(inco) :-
	write('Incoherent concept'),nl,!.
	
t5out_warning_output(no_user_def(Obj)) :-
	t5out_writelist(_, ['No user definition for ', Obj]),
	!.

t5out_warning_output(indiv) :-
	write('Individual concepts cannot be declared in BACK V5.'),nl,
	write('          The definition will be ignored.'),
	nl,!.

t5out_warning_output(downward_failed(ObjName, Untold)) :-
	b5desc_list_to_term(Untold, Term),
	t5out_writelist(_, ['Downward migration of ', ObjName, ' failed: ',
	      nl(1), '          ', ObjName, ' :: ', Term, 
	      ' is no more tellable and has been withdrawn.']),
	!.


t5out_warning_output(object_not_totally_removed(Obj)) :-
	t5out_writelist(_, ['forget ', Obj, ': ', Obj, 
	                    ' is introduced by the TBox or IBox', nl(1),
			    '          therefore it cannot be totally removed.']),
	!.
	
			
%% catchall

t5out_warning_output(WarningTyp) :- % If there is no clause for the WarningTyp
	write(WarningTyp),
	nl, !.

t5out_write_warning :-
	write('WARNING   ').


% **********************************************************************
% *                   INFOS                                           *
% **********************************************************************

t5out_info(InfoTyp) :-
	( t5out_test_info ->
	    t5out_info_output(InfoTyp)
	; true).




% Hier sollen die Klauseln eingetragen werden, die INFOS erzeugen !!
% Alle Klauseln, die eine Ausgabe erzeugen, beginnen mit
% "t5out_write_info," und enden mit "nl, !.".

%%% mf

t5out_info_output(range(Range,Key)) :-
	t5tbox_all_key(Range),
	!,
	t5out_write_info,
	write('Range "anything" is added to role '),
	key_view(Key,V),	
	write(V),
	write('.'),
	nl,!.

t5out_info_output(range(_,_)) :-
	!.

%%%%% okp 

t5out_info_output(at_parse_failed) :-
	 t5out_write_info,
	 write('Parsing of Abox Tell Expression failed.'),
	 nl,!.


t5out_info_output(at_parse_ok) :-
	t5out_write_info,
	write('Parsing of Abox Tell Expression succeeded'),
	nl,!.

t5out_info_output(atp_to_a5sch) :-
	t5out_write_info,
	write('Forward Introduction and filling of Agenda succeeded.'),nl,
	write('Control from Parser to ABox-Kernel.'),
	nl,!.

t5out_info_output(def_ok) :-
	t5out_write_info,
	write('Definition accepted'),nl,!.

t5out_info_output(def_reject) :-
	t5out_write_info,
	write('Definition rejected'),nl,!.

t5out_info_output(init) :-
	t5out_write_info,
	write('Initialization completed.'),
	nl,!.


t5out_info_output(start_downward(ObjName)) :-
	t5out_write_info,
	write('Start downward migration of: '), write(ObjName),
	nl, !.
	
t5out_info_output(end_downward(ObjName)) :-
	t5out_write_info,
	write('Downward migration of '), write(ObjName), write(' succeeded.'),
	nl, !.


t5out_info_output(retell(Obj, Number)) :-
	t5out_write_info,
	write('Start retell concerning '), write(Obj),
	write('of tell no. '), write(Number),
	nl, !.

t5out_info_output(start_upward(ObjId)) :-
	t5out_write_info,
	b5st_object_entry(Obj,_,ObjId,_),
	write('Start upward migration of: '), write(Obj),
	nl, !.

t5out_info_output(end_upward(ObjId)) :-
	t5out_write_info,
	b5st_object_entry(Obj,_,ObjId,_),
	write('End upward migration of '), write(Obj),
	nl, !.


t5out_info_output(new_dependency(depends_on(NFx,X,Y,cinst(Y,C)))) :-
	t5out_write_info,
	b5st_object_entry(XN,_,X,_),
	b5st_object_entry(YN,_,Y,_),
	t5out_writelist(_, ['New ', NFx, ' dependency: ', XN, 
	                    ' depends on ', YN, ' because ']),
        write('           '), write(YN), write(' :: '),
        ( C = all(R,VR) ->
	      b5st_class_entry(RN,_,R,_),
	      b5st_class_entry(VRN,_,VR,_),
              write(all(RN,VRN)), ! 
	; C = fills(R, Os1) ->
	      b5desc_objs_postprocess(Os1, Os2),
	      ( b5st_class_entry(RN,_,R,_),
	        write(fills(RN,Os2)), !
	      ; t5role_inv_role(InvR, R),
		b5st_class_entry(RN,_,InvR,_),
	        write(fills(inv(RN),Os2))), !
	; C = Conc ->
	      b5st_class_entry(CN,_,Conc,_),
	      write(CN), !
	; !, true),
	nl, !.

t5out_info_output(dependency_replaced(Sel, O1, Pold, Pnew)) :-
	t5out_write_info,
	t5out_writelist(_, [Pnew, ' replaced ', Pold, ' at ', O1, 
	                    ' (', Sel, ').']),
	!.

t5out_info_output(computing_depending_objects(Sel, Obj)) :-
	t5out_write_info,
	t5out_writelist(_, ['Computing objects depending on ', Obj, 
                             ' wrt ', Sel]),
	!.

t5out_info_output(no_dependencies(Sel,Obj)) :-
	t5out_write_info,
	t5out_writelist(_, ['There is no object depending on ', Obj, 
                            ' wrt ', Sel]),
	!.

t5out_info_output(a5sch_process_object(ObjID)) :-
	b5kif_name_key(Name,_,ObjID),
	t5out_write_info,
	t5out_writelist(_, ['a5sch_process_object( ',Name,' )']),
	!.


%% catchall

t5out_info_output(InfoTyp) :-     % If there is no clause for the InfoTyp
	t5out_write_info,         % yet, then the InfoTyp itself is written.
	write(InfoTyp),
	nl.

t5out_write_info :-
	write('INFO      ').


% **********************************************************************
% *                   TRACES                                           *
% **********************************************************************

t5out_trace(TraceTyp) :-
	( t5out_test_trace ->
	    t5out_trace_output(TraceTyp)
	; true).


% Hier sollen die Klauseln eingetragen werden, die TRACES erzeugen !!
% Alle Klauseln, die eine Ausgabe erzeugen, beginnen mit
% "t5out_write_trace," und enden mit "nl, !.".



%%%%% okp

t5out_trace_output(start(at)) :-
	t5out_write_trace,
	write('Starting to parse ABoxTellExpression.'),
	nl,!.

t5out_trace_output(intro(Name)) :-
	t5out_write_trace,
	write(Name),
	write(' is being forward introduced.'),
	nl,!.

t5out_trace_output(to_agenda(Name)) :-
	t5out_write_trace,
	write(Name),
	write(' is being added to the Agenda.'),
	nl,!.

t5out_trace_output(end(at)) :-
	t5out_write_trace,
	write('Parsing of ABoxTellExpression ended.'),
	nl,!.

t5out_trace_output(added_userdef(Obj, Additions)) :-
	t5out_write_trace, 
	t5out_writelist(_, ['Added user definitions ', Additions, 
                            ' to ', Obj]),
	!.

t5out_trace_output(retracted_userdef(Obj, Desc)) :-
	t5out_write_trace,
	t5out_writelist(_, ['Retracted description ', Desc, 
			' from user definition of ', Obj]),
        !.

%% Module a5obj (uk)

t5out_trace_output(a5obj_coherent_p(ONFsel, ObjId)) :-
	t5out_write_trace,
	b5st_object_entry(ObjName,_,ObjId,_),
	(ONFsel == nfi ->
	    ModeText = '          (I-Links have been applied to objects).'
	;   ModeText = '          (no I-Links have been applied).'),
	t5out_writelist(_,
		['Normal form of object ', quote(ObjName),
		' is coherent', nl(1), ModeText]),!.

%% IBox
t5out_trace_output(ilink(Link)) :-
	i5db_get_link(Link,C1,C2),
	b5kif_name_key(N1,_,C1),
	b5kif_name_key(N2,_,C2),
	t5out_write_trace,
	t5out_writelist(_,['Applying ',ilink(Link),' : ', N1, ' => ', N2]),
	!.
	
t5out_trace_output(ibox_info(Conc)) :-
	b5kif_name_key(Name,_,Conc),
	t5out_write_trace,
	t5out_writelist(_,['Adding IBox information of concept ',Name]),
	!.

%% cmk

t5out_trace_output(forward_propagate_vr(VR,Obj,Role,Fillers)) :-
	t5out_write_trace,
	b5kif_name_key(VRN,_,VR),
	b5kif_name_key(ObjN,_,Obj),
	b5kif_name_key(RoleN,_,Role),
	b5kif_name_key(VRN,_,VR),
	b5kif_names_keys(FillersN,_,Fillers),
	t5out_writelist(_,['forward_propagate_vr(',VRN,
			', ',
			ObjN,
			', ',
			RoleN,
			', ',
			FillersN,')']),	!.


t5out_trace_output(a5sch_process_single_job(Job,Id)) :-
	t5out_write_trace,
	b5kif_name_key(Name,_,Id),
	t5out_writelist(_,['a5sch_process_single_job(',Job,', ',Name,')']),
	!.

/*
	xxtell(Job).


xxtell(tell(Obj,Job,DEP)) :-
	b5kif_name_key(Name,_,Obj),
	writeq(Name),nl,
	xxjobs(Job),!.
xxtell(_) :- !.

xxjobs([]) :- !.
xxjobs([X|Xs]) :- 
	xxjob(X),
	xxjobs(Xs).

xxjob(fills(Role,Fillers)) :-
	b5kif_name_key(Name,_,Role),
 	b5kif_names_keys(Fillers,FN),
	t5out_writelist(_,[Name,' : ',FN]),!.

xxjob(Concs) :-
 	b5kif_names_keys(Concs,_,N),
	t5out_writelist(_,[N]),!.

xxjob(GG) :- 
	t5out_writelist(_,[GG]).
*/

t5out_trace_output(ibox_result(Id,Old,New)) :-
	t5out_write_trace,
	(b5nf_incoherent_p(New) ->
		t5out_writelist(_,['ibox leads to incoherent NF',New])
		;
		b5desc_nf_diff(obj,Id,New,Old,X1),
		b5kif_name_key(N,_,Id),
		t5out_writelist(_,['applying ibox to ',N,'leads to new info']),
		nl,
		b5desc_write(X1),
		nl
		).



%% catchall

t5out_trace_output(TraceTyp) :-    % If there is no clause for the TraceTyp
	!, t5out_write_trace,      % yet, then the TraceTyp itself is written.
	write(TraceTyp),
	nl.

t5out_write_trace :-
	write('TRACE     ').














% LAST EDIT: Wed Aug 18 17:41:22 1993 by Mirjam Kuehne (madonna!mir) 
% LAST EDIT: Tue Aug 17 15:25:32 1993 by Mirjam Kuehne (anfall!mir) 
% LAST EDIT: Wed Feb 10 14:27:25 1993 by Mirjam Kuehne (anfall!mir) 
% LAST EDIT: Fri Feb  5 13:13:03 1993 by Mirjam Kuehne (anfall!mir) 
% LAST EDIT: Wed Feb  3 17:24:27 1993 by Mirjam Kuehne (jenseits!mir) 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 			PROGRAMMING INTERFACE				 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% IMPORT-predicates: b5st_class_entry/4.
%%                    b5kif_name_type_key/3.
%%                    b5kif_var/3.
%%                    b5kif_all_aset/1.
%%                    b5kif_super_aset/2.
%%                    b5kif_aset_element/2.
%%                    b5kif_subsx/2.
%%                    b5kif_subsx/3.
%%                    b5kif_direct_subsx/2.
%%                    b5kif_direct_subsx/3.
%%		      b5sort_unify/3.
%%		      t5concid_filter/2.
%%		      t5concid_filter_holds_p/2.
%%		      t5concid_incoherent_p/1.
%%		      t5concid_disjoint_p/1.
%%		      t5concid_disjoints/2.
%%		      t5concid_vr/3.
%%		      t5concid_min-max/4.
%%		      t5concid_vr_min-max/4.
%%		      t5concid_supers/2.
%%		      t5concid_supers/3.
%%		      t5concid_direct_supers/2.
%%		      t5concid_direct_supers/3.
%%		      t5concid_subsumes_p/d23.
%%		      t5fil_holds_p/2.
%%		      t5hc_supers/3.
%%		      t5hc_supers/4.
%%		      t5hc_subs/3.
%%		      t5hc_subs/4.
%%		      t5hc_direct_supers/3.
%%                    t5hc_minimize_special/4.
%%                    t5rdb_filter/2.
%%		      t5role_supers/2.
%%		      t5role_subs/2.
%%		      t5role_direct_supers/2.
%%		      t5role_direct_supers/3.
%%		      t5role_direct_subs/2.
%%		      t5role_direct_supers/3.
%%		      t5role_subsumes_p/2.
%%		      t5role_filter_holds_p/2.
%%		      t5role_incoherent_p/1.
%%		      t5role_disjoints/2.
%%		      t5role_disjoint_p/2.
%%		      t5role_ibox_equivalent_p/2.
%%		      t5role_ibox_incoherent_p/1.
%%		      t5role_range/2.
%%		      t5role_domain/2.
%%		      t5tbox_nothing_key/1.
%%		      t5tbox_hc/1.
%%		      i5ibox_get_ibox_conc/2.
%%		      i5ibox_equivalent_p/1.
%%		      i5ibox_disjoint_p/1.
 
% ********************************************************************
% ********************************************************************
 
%       T B O X  G E T
 
% ********************************************************************
% ********************************************************************
% This interface is completely in terms of names.
% No synonyms are accepted as input.
 
% With the 'kind' expression the sources of names can be found out,
% (and in case a non-user defined name is found, the describe function
% of tboxask can be used to generate an equivalent description).
 
% All the following expressions (to be used from within tboxget)
% take or return as arguments names of the kind indicated
% (except for Min, Max, Type, Source).
 
% None of the arguments need be instantiated, backtracking
% is performed over all known names in the system (of the appropiate
% type). 

% There are no error messages. Calls with unknown names or otherwise
% ill-formed will simply fail.
 
% ********************************************************************
% Some remarks on the semantics of the tboxget interface:
% (These remarks apply to the revised version, November 1992)
% EMPTY or INCOHERENT entities:
% all predicates except
%  - super_prim_concept,
%  - primitive_concept
%  - defined_concept
% never succeed
% with empty or inconsistent concepts, roles, asets, or numbers.
% Neither do they generate such entities upon backtracking.
% Empty entities must be dealt with only by predicates containing
% 'incoherent'.
 
% TOP elements:
% For concepts 'anything'
% For asets    'aset'
% For numbers  'number'
% For strings  'string'
% For roles    'anyrole' 
% Top elements are generated by super and direct_super predicates
% (The top-element for roles doesn't exist anymore, but it's 
% possible to get all roles with the help of tboxget(role(Role))
 
% EQUIVALENT predicates:
% never succeed with two identical names (contrary to the semantics)
 
% SUPER predicates:
% The super-relations never includes names for equivalent entities
% (i.e., only proper super concepts, asets, etc.)

% ********************************************************************
% Some remarks on the pecularities of the new BACK Version 5.0:

% the following predicates doesn't exist anymore:
% - those containing `user' in their name
% - tboxget(synonym)
% - tboxget(inividual) 

% new predicates:
% - tboxget(primitive_role(Role)),
% - tboxget(defined_role(Role)),
% - tboxget(super_prim_role(Role1,Role2)) 

% Those predicates containing 'disjoint` in their name generate all
% disjoints, not only for one special domain, so it is better to 
% instantiate at least one argument.


% ********************************************************************
% ********************************************************************
% **  For all Types:
% **  These procedures backtrack over names of all types !
% ********************************************************************
% super(Name1, Name2)
% direct_super(Name1, Name2)        
% equivalent(Name1, Name2)
% disjoint(Name1, Name2)
% incoherent(Name)
% kind(Name, Type, Source)
% ********************************************************************
% **  Concepts
% ********************************************************************
% primitive_concept(Concept)
% defined_concept(Concept)
% super_concept(Concept1, Concept2)
% direct_super_concept(Concept1, Concept2)
% equivalent_concept(Concept1, Concept2)
% super_prim_concept(Concept1, Concept2)
% disjoint_concept(Concept1, Concept2)
% incoherent_concept(Concept)
% value_restriction(Concept, Role, ConceptOrAsetOrNumberOrString)
% number_restriction(Concept, Role, Min, Max)
% restriction(Concept, Role, ConceptOrAsetOrNumberOrString, Min, Max)
% ********************************************************************
% **  Asets
% ********************************************************************
% element(Attribute, Aset)
% attribute_domain(AttributeOrAset, AttributeDomain)
% super_aset(Aset1, Aset2)
% direct_super_aset(Aset1, Aset2)
% equivalent_aset(Aset1, Aset2)
% disjoint_aset(Aset1, Aset2)
% incoherent_aset(Aset)
% ********************************************************************
% **  Numbers
% ********************************************************************
% super_number(Number1, Number2)
% direct_super_number(Number1, Number2)
% equivalent_aset(Number1, Number2)
% disjoint_number(Number1, Number2)
% incoherent_number(Number)
% ********************************************************************
% **  Roles
% ********************************************************************
% range(Role, ConceptOrAsetOrNumberOrString)
% domain(Role, Concept)
% role(Role),
% super_role(Role1, Role2)
% direct_super_role(Role1, Role2)
% super_prim_role(Role1, Role2)
% equivalent_role(Role1, Role2)
% disjoint_role(Role1, Role2)
% incoherent_role(Role1, Role2)
% defined_role(Role),
% primitive_role(Role)
% ********************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% concepts								 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tboxget(super_concept(ConcName,SuperConcName)) :-
	b5kif_var(ConcName,SuperConcName,XY),
	t5pif_super_concept(XY,ConcName,SuperConcName).

tboxget(user_super_concept(ConcName,UserSuperConcName)) :-
	b5kif_var(ConcName,UserSuperConcName,XY),
	t5pif_user_super_concept(XY,ConcName,UserSuperConcName).

tboxget(direct_super_concept(ConcName,DirectSuperName)) :-
	b5kif_var(ConcName,DirectSuperName,XY),
	t5pif_direct_super_concept(XY,ConcName,DirectSuperName).

tboxget(user_direct_super_concept(ConcName,UserDirectName)) :-
	b5kif_var(ConcName,UserDirectName,XY),
	t5pif_user_direct_super_concept(XY,ConcName,UserDirectName).

tboxget(super_prim_concept(ConcName,SuperPrimName)) :-
	b5kif_var(ConcName,SuperPrimName,XY),
	t5pif_super_prim_concept(XY,ConcName,SuperPrimName).

tboxget(equivalent_concept(ConcName,EquivalentName)) :-
	b5st_class_entry(ConcName,Type,ConcKey,_),
	b5st_class_entry(EquivalentName,Type,ConcKey,_),
	t5pif_conc_test_p(EquivalentName,Type,ConcKey),
	\+ EquivalentName = prim(_),
	EquivalentName \== ConcName.

tboxget(disjoint_concept(ConcName,DisjointName)) :-
	b5kif_var(ConcName,DisjointName,XY),
	t5pif_disjoint_concept(XY,ConcName,DisjointName).

tboxget(incoherent_concept(ConcName)) :-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	\+ Type = role,
	\+ ConcName = prim(_),
	t5concid_incoherent_p(ConcKey).

tboxget(defined_concept(ConcName)) :-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	t5pif_conc_test_p(ConcName,Type,ConcKey),
	t5concid_filter_holds_p(ConcKey,user_defined).

tboxget(primitive_concept(ConcName)) :-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	t5pif_conc_test_p(ConcName,Type,ConcKey),
	t5concid_filter_holds_p(ConcKey,user_primitive).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% roles 								 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tboxget(role(RoleName)) :-
	b5kif_name_type_key(RoleName,role,_),
	t5pif_role_test_p(RoleName).

tboxget(super_role(RoleName,SuperRoleName)) :-
	b5kif_var(RoleName,SuperRoleName,XY),
	t5pif_super_role(XY,RoleName,SuperRoleName).

tboxget(direct_super_role(RoleName,DirectSuperName)) :-
	b5kif_var(RoleName,DirectSuperName,XY),
	t5pif_direct_super_role(XY,RoleName,DirectSuperName).

tboxget(super_prim_role(RoleName,SuperPrimName)) :-
	b5kif_var(RoleName,SuperPrimName,XY),
	t5pif_super_prim_role(XY,RoleName,SuperPrimName).

tboxget(equivalent_role(RoleName,EquivalentName)) :-
	b5st_class_entry(RoleName,(r,0),RoleKey,_),
	b5st_class_entry(EquivalentName,(r,0),RoleKey,_),
	t5pif_role_test_p(RoleName,RoleKey),
	\+ EquivalentName = prim(_),
	EquivalentName \== RoleName.

tboxget(disjoint_role(RoleName,DisjointName)) :-
	b5kif_var(RoleName,DisjointName,XY),
	t5pif_disjoint_role(XY,RoleName,DisjointName).

tboxget(incoherent_role(RoleName)) :-
	b5kif_name_type_key(RoleName,role,RoleKey),
	t5role_incoherent_p(RoleKey).

tboxget(defined_role(RoleName)) :-
	b5kif_name_type_key(RoleName,role,RoleKey),
	t5pif_role_test_p(RoleName,RoleKey),
	t5role_filter_holds_p(RoleKey,user_defined).

tboxget(primitive_role(RoleName)) :-
	b5kif_name_type_key(RoleName,role,RoleKey),
	t5pif_role_test_p(RoleName,RoleKey),
	t5role_filter_holds_p(RoleKey,user_primitive).

tboxget(range(RoleName,RangeName)) :-
	b5kif_var(RoleName,RangeName,XY),
	t5pif_range(XY,RoleName,RangeName).

tboxget(user_range(RoleName,RangeName)) :-
	b5kif_var(RoleName,RangeName,XY),
	t5pif_user_range(XY,RoleName,RangeName).

tboxget(domain(RoleName,DomainName)) :-
	b5kif_var(RoleName,DomainName,XY),
	t5pif_domain(XY,RoleName,DomainName).

tboxget(user_domain(RoleName,DomainName)) :-
	b5kif_var(RoleName,DomainName,XY),
	t5pif_user_domain(XY,RoleName,DomainName).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% general 						                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tboxget(super(TermName,SuperName)) :-
	b5kif_var(TermName,SuperName,XY),
	t5pif_super(XY,TermName,SuperName).

tboxget(direct_super(TermName,DirectSuperName)) :-
	b5kif_var(TermName,DirectSuperName,XY),
	t5pif_direct_super(XY,TermName,DirectSuperName).

tboxget(equivalent(TermName,EquiTermName)) :-
	b5st_class_entry(TermName,_,TermKey,_),
	b5st_class_entry(EquiTermName,_,TermKey,_),
	EquiTermName \== TermName,
	t5pif_conc_test_p(TermName,TermKey),
	\+ EquiTermName = prim(_).

tboxget(disjoint(TermName,DisjointName)) :-
	b5kif_var(TermName,DisjointName,XY),
	t5pif_disjoint(XY,TermName,DisjointName).

tboxget(incoherent(TermName)) :-
	b5kif_name_type_key(TermName,Type,TermKey),
	(Type = role,
	t5role_incoherent_p(TermKey)
    ;
	t5concid_incoherent_p(TermKey)
    ).

tboxget(defined(TermName)) :-
	b5kif_name_type_key(TermName,Type,_),
	(Type = role,
	tboxget(defined_role(TermName))
    ;
	tboxget(defined_concept(TermName))
    ).

tboxget(primitive(TermName)) :-
	b5kif_name_type_key(TermName,Type,_),
	(Type = role,
	tboxget(primitive_role(TermName))
    ;
	tboxget(primitive_concept(TermName))
    ).

tboxget(kind(TermName,Type,Source)) :-
	(Type = role/K,
	t5pif_role_kind(TermName,role/K,Source)
    ;
	t5pif_conc_kind(TermName,Type,Source)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% restrictions								 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tboxget(restriction(ConcName,RoleName,VRName,Min,Max)) :-
	b5kif_name_type_key(ConcName,conc,ConcKey),
	t5pif_conc_test_p(ConcName,ConcKey),
	b5kif_name_type_key(RoleName,role,RoleKey),
	t5pif_role_test_p(RoleName,RoleKey),
	t5concid_vr_min_max(ConcKey,RoleKey,VRKey,Min,Max),
	b5kif_name_type_key(VRName,Type,VRKey),
	t5pif_conc_type_name_test_p(VRName,Type).

tboxget(user_restriction(ConcName,RoleName,VRName,Min,Max)) :-
	b5st_class_entry(ConcName,(c,0),ConcKey,_),
	t5pif_conc_test_p(ConcName,ConcKey),
	b5st_class_entry(RoleName,(r,0),RoleKey,_),
	t5pif_role_test_p(RoleName,RoleKey),
	t5concid_vr_min_max(ConcKey,RoleKey,VRKey,Min,Max),
	b5st_class_entry(VRName,(c,0),VRKey,_),
	\+ VRName = prim(_).

tboxget(value_restriction(ConcName,RoleName,VRName)) :-
	b5kif_name_type_key(ConcName,conc,ConcKey),
	t5pif_conc_test_p(ConcName,ConcKey),
	b5kif_name_type_key(RoleName,role,RoleKey),
	t5pif_role_test_p(RoleName,RoleKey),
	t5concid_vr(ConcKey,RoleKey,VRKey),
	b5kif_name_type_key(VRName,Type,VRKey),
	t5pif_conc_type_name_test_p(VRName,Type).

tboxget(user_value_restriction(ConcName,RoleName,VRName)) :-
	b5st_class_entry(ConcName,(c,0),ConcKey,_),
	t5pif_conc_test_p(ConcName,ConcKey),
	b5st_class_entry(RoleName,(r,0),RoleKey,_),
	t5pif_role_test_p(RoleName,RoleKey),
	t5concid_vr(ConcKey,RoleKey,VRKey),
	b5st_class_entry(VRName,(c,0),VRKey,_),
	\+ VRName = prim(_).

tboxget(number_restriction(ConcName,RoleName,Min,Max)) :-
	b5kif_name_type_key(ConcName,conc,ConcKey),
	t5pif_conc_test_p(ConcName,ConcKey),
	b5kif_name_type_key(RoleName,role,RoleKey),
	t5pif_role_test_p(RoleName,RoleKey),
	t5concid_min_max(ConcKey,RoleKey,Min,Max).

tboxget(user_number_restriction(ConcName,RoleName,Min,Max)) :-
	b5st_class_entry(ConcName,(c,0),ConcKey,_),
	t5pif_conc_test_p(ConcName,ConcKey),
	b5st_class_entry(RoleName,(r,0),RoleKey,_),
	t5pif_role_test_p(RoleName,RoleKey),
	t5concid_min_max(ConcKey,RoleKey,Min,Max).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% asets									 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tboxget(super_aset(AsetName,SuperAsetName)) :-
	b5kif_super_aset(AsetName,SuperAsetName).

tboxget(equivalent_aset(AsetName,EquiAsetName)) :-
	b5st_class_entry(AsetName,(a,_),AsetKey,_),
	b5st_class_entry(EquiAsetName,(a,_),AsetKey,_),
	EquiAsetName \== AsetName,
	t5pif_conc_test_p(AsetName,AsetKey),
	\+ EquiAsetName = prim(_).

tboxget(disjoint_aset(AsetName,DisjointAsetName)) :-
	b5kif_var(AsetName,DisjointAsetName,XY),
	t5pif_disjoint_aset(XY,AsetName,DisjointAsetName).

tboxget(incoherent_aset(AsetName)) :-
        b5kif_name_type_key(AsetName,aset-_,AsetKey),
	t5concid_incoherent_p(AsetKey).

tboxget(element(AttributeName,AsetName)) :-
	b5kif_aset_element(AttributeName,AsetName).

tboxget(attribute_domain(AsetName,AttributeDomainName)) :-
	b5kif_name_type_key(AsetName,aset-DomainKey,_),
	\+ AsetName = prim(_),
	(DomainKey = 0 -> AttributeDomainName = system;
	 b5kif_name_type_key(AttributeDomainName,aset-DomainKey,DomainKey)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% numbers								 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tboxget(super_number(NumberName,SuperNumberName)) :-
	b5kif_var(NumberName,SuperNumberName,XY),
	t5pif_super_number(XY,NumberName,SuperNumberName).

tboxget(equivalent_number(NumberName,EquivalentName)) :-
	b5kif_name_type_key(NumberName,number,NumberKey),
	b5kif_name_type_key(EquivalentName,number,NumberKey),
	t5pif_conc_test_p(NumberName,NumberKey),
	\+ EquivalentName = prim(_),
	EquivalentName \== NumberName.

tboxget(disjoint_number(NumberName,DisjointName)) :-
	b5kif_var(NumberName,DisjointName,XY),
	t5pif_disjoint_number(XY,NumberName,DisjointName).
	
tboxget(incoherent_number(NumberName)) :-
        b5kif_name_type_key(NumberName,number,NumberKey),
	t5concid_incoherent_p(NumberKey).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5pif_conc_test_p(ConcName,ConcKey) :-
	\+ ConcName = prim(_),
	\+ t5concid_incoherent_p(ConcKey).

t5pif_conc_test_p(ConcName,Type,ConcKey) :-
	\+ ConcName = prim(_),
	\+ Type = role, 
	\+ Type = (r,0),
	\+ Type = attribute-_,
	\+ t5concid_incoherent_p(ConcKey).

t5pif_conc_type_name_test_p(Name,Type) :-
	\+ Name = prim(_),
	\+ Type = role.

t5pif_super_concept(nv,ConcName,SuperConcName):-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	\+ Type = role,
	t5concid_supers(ConcKey,Supers),
	member(SuperConcKey,Supers),
	b5kif_name_type_key(SuperConcName,Type,SuperConcKey),
	t5pif_conc_test_p(SuperConcName,SuperConcKey).

t5pif_super_concept(nn,ConcName,SuperConcName):-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	b5kif_name_type_key(SuperConcName,Type,SuperConcKey),
	\+ Type = role,
	t5concid_subsumes_p(SuperConcKey,ConcKey).

t5pif_super_concept(vn,ConcName,SuperConcName):-
	b5kif_name_type_key(SuperConcName,Type,SuperConcKey),
	\+ Type = role,
	b5kif_subsx(SuperConcKey,Subs),      %% eliminates "nothing"
	member(ConcKey,Subs),
	b5kif_name_type_key(ConcName,Type,ConcKey),
	\+ ConcName = prim(_).

t5pif_super_concept(vv,ConcName,SuperConcName):-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	t5pif_conc_test_p(ConcName,Type,ConcKey),
	t5concid_supers(ConcKey,Supers),
	member(SuperConcKey,Supers),
	b5kif_name_type_key(SuperConcName,Type,SuperConcKey),
	t5pif_conc_test_p(ConcName,ConcKey).


%% 17.8.93:

t5pif_user_super_concept(nv,ConcName,UserSuperName):-
	b5st_class_entry(ConcName,Type,ConcKey,_),
	\+ Type = role,
	t5concid_supers(ConcKey,user,UserSupers),
	member(UserSuperKey,UserSupers),
	b5st_class_entry(UserSuperName,Type,UserSuperKey,_),
	t5pif_conc_test_p(UserSuperName,UserSuperKey).

t5pif_user_super_concept(nn,ConcName,UserSuperName):-
	b5st_class_entry(ConcName,Type,ConcKey,_),
	b5st_class_entry(UserSuperName,Type,UserSuperKey,_),
	\+ Type = role,
	t5concid_supers(ConcKey,user,UserSupers),
	member(UserSuperKey,UserSupers).

t5pif_user_super_concept(vn,ConcName,UserSuperName):-
	b5st_class_entry(UserSuperName,Type,UserSuperKey,_),
	\+ Type = role,
	t5concid_filter_holds_p(UserSuperKey,user),
	b5kif_subsx(UserSuperKey,Subs),
	member(ConcKey,Subs),
	b5st_class_entry(ConcName,Type,ConcKey,_),
	\+ ConcName = prim(_).

/*
t5pif_user_direct_super_concept(vn,ConcName,UserDirectName):-
	b5st_class_entry(UserDirectName,Type,UserDirectKey,_),
	\+ Type = role,
	b5kif_direct_subsx(UserDirectKey,user,Subs),
	member(ConcKey,Subs),
	b5st_class_entry(ConcName,Type,ConcKey,_),
	\+ ConcName = prim(_).
*/

t5pif_user_super_concept(vv,ConcName,UserSuperName):-
	b5st_class_entry(ConcName,Type,ConcKey,_),
	t5pif_conc_test_p(ConcName,Type,ConcKey),
	t5concid_supers(ConcKey,user,UserSupers),
	member(UserSuperKey,UserSupers),
	b5st_class_entry(UserSuperName,Type,UserSuperKey,_),
	t5pif_conc_test_p(UserSuperName,UserSuperKey).



t5pif_direct_super_concept(nv,ConcName,DirectSuperName):-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	\+ Type = role,
	t5concid_direct_supers(ConcKey,DirectSupers),
	member(DirectSuperKey,DirectSupers),
	b5kif_name_type_key(DirectSuperName,Type,DirectSuperKey),
	t5pif_conc_test_p(DirectSuperName,DirectSuperKey).

t5pif_direct_super_concept(nn,ConcName,DirectSuperName):-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	b5kif_name_type_key(DirectSuperName,Type,DirectSuperKey),
	\+ Type = role,
	t5concid_direct_supers(ConcKey,DirectSupers),
	member(DirectSuperKey,DirectSupers).

t5pif_direct_super_concept(vn,ConcName,DirectSuperName):-
	b5kif_name_type_key(DirectSuperName,Type,DirectSuperKey),
	\+ Type = role,
	b5kif_direct_subsx(DirectSuperKey,Subs),   % eliminates "nothing"
	member(ConcKey,Subs),
	b5kif_name_type_key(ConcName,Type,ConcKey),
	\+ ConcName = prim(_).

t5pif_direct_super_concept(vv,ConcName,DirectSuperName):-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	t5pif_conc_test_p(ConcName,Type,ConcKey),
	t5concid_direct_supers(ConcKey,Supers),
	member(DirectSuperKey,Supers),
	b5kif_name_type_key(DirectSuperName,Type,DirectSuperKey),
	t5pif_conc_test_p(DirectSuperName,DirectSuperKey).

t5pif_user_direct_super_concept(nv,ConcName,UserDirectName):-
	b5st_class_entry(ConcName,Type,ConcKey,_),
	\+ Type = role,
	t5concid_direct_supers(ConcKey,user,DirectSupers),
	member(UserDirectKey,DirectSupers),
	b5st_class_entry(UserDirectName,Type,UserDirectKey,_),
	t5pif_conc_test_p(UserDirectName,UserDirectKey).

t5pif_user_direct_super_concept(nn,ConcName,UserDirectName):-
	b5st_class_entry(ConcName,Type,ConcKey,_),
	b5st_class_entry(UserDirectName,Type,UserDirectKey,_),
	\+ Type = role,
	t5concid_direct_supers(ConcKey,user,DirectSupers),
	member(UserDirectKey,DirectSupers).


t5pif_user_direct_super_concept(vn,ConcName,UserDirectName):-
	b5st_class_entry(UserDirectName,Type,UserDirectKey,_),
	\+ Type = role,
	t5concid_filter_holds_p(UserDirectKey,user),
	b5kif_direct_subsx(UserDirectKey,Subs),
	member(ConcKey,Subs),
	b5st_class_entry(ConcName,Type,ConcKey,_),
	\+ ConcName = prim(_).

/*
t5pif_user_direct_super_concept(vn,ConcName,UserDirectName):-
	b5st_class_entry(UserDirectName,Type,UserDirectKey,_),
	\+ Type = role,
	b5kif_direct_subsx(UserDirectKey,user,Subs),
	member(ConcKey,Subs),
	b5st_class_entry(ConcName,Type,ConcKey,_),
	\+ ConcName = prim(_).
*/

t5pif_user_direct_super_concept(vv,ConcName,UserDirectName):-
	b5st_class_entry(ConcName,Type,ConcKey,_),
	t5pif_conc_test_p(ConcName,Type,ConcKey),
	t5concid_direct_supers(ConcKey,user,Supers),
	member(UserDirectKey,Supers),
	b5st_class_entry(UserDirectName,Type,UserDirectKey,_),
	t5pif_conc_test_p(UserDirectName,UserDirectKey).

t5pif_super_prim_concept(nv,ConcName,SuperPrimName):-
	b5st_class_entry(ConcName,Type,ConcKey,_),
	\+ Type = role,
	t5concid_supers(ConcKey,user_primitive,Supers),
	member(SuperPrimKey,Supers),
	b5st_class_entry(SuperPrimName,Type,SuperPrimKey,_),
	t5pif_conc_test_p(SuperPrimName,SuperPrimKey).

t5pif_super_prim_concept(nn,ConcName,SuperPrimName):-
	b5st_class_entry(ConcName,Type,ConcKey,_),
	b5st_class_entry(SuperPrimName,Type,SuperPrimKey,_),
	\+ Type = role,
	t5concid_supers(ConcKey,user_primitive,DirectSupers),
	member(SuperPrimKey,DirectSupers).


t5pif_super_prim_concept(vn,ConcName,SuperPrimName):-
	b5st_class_entry(SuperPrimName,Type,SuperPrimKey,_),
	\+ Type = role,
	t5concid_filter_holds_p(SuperPrimKey,user_primitive),
	t5concid_subs(SuperPrimKey,Subs),
	member(ConcKey,Subs),
	b5st_class_entry(ConcName,Type,ConcKey,_),
	t5pif_conc_test_p(ConcName,ConcKey).

/*
t5pif_super_prim_concept(vn,ConcName,SuperPrimName):-
	b5st_class_entry(SuperPrimName,Type,SuperPrimKey,_),
	\+ Type = role,
	t5concid_direct_supers(SuperPrimKey,user_primitive,Supers),
	member(ConcKey,Supers),
	b5st_class_entry(ConcName,Type,ConcKey,_),
	t5pif_conc_test_p(ConcName,ConcKey).
*/

t5pif_super_prim_concept(vv,ConcName,SuperPrimName):-
	b5st_class_entry(ConcName,Type,ConcKey,_),
	t5pif_conc_test_p(ConcName,Type,ConcKey),
	t5concid_supers(ConcKey,user_primitive,Supers),
	member(SuperPrimKey,Supers),
	b5st_class_entry(SuperPrimName,Type,SuperPrimKey,_),
	t5pif_conc_test_p(SuperPrimName,SuperPrimKey).


t5pif_disjoint_concept(nv,ConcName,DisjointName):-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	\+ Type = role,
	t5concid_disjoints(ConcKey,Disjoints),
	member(DisjointKey,Disjoints),
	b5kif_name_type_key(DisjointName,_,DisjointKey),
	t5pif_conc_test_p(DisjointName,DisjointKey).

t5pif_disjoint_concept(nn,ConcName,DisjointName):-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	\+ Type = role,
	b5kif_name_type_key(DisjointName,_,DisjointKey),
	t5concid_disjoint_p(ConcKey,DisjointKey).

t5pif_disjoint_concept(vn,ConcName,DisjointName):-
	b5kif_name_type_key(DisjointName,Type,DisjointKey),
	\+ Type = role,
	t5concid_disjoints(DisjointKey,Disjoints),
	member(ConcKey,Disjoints),
	b5kif_name_type_key(ConcName,_,ConcKey),
	t5pif_conc_test_p(ConcName,ConcKey).

t5pif_disjoint_concept(vv,ConcName,DisjointName):-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	t5pif_conc_test_p(ConcName,Type,ConcKey),
	t5concid_disjoints(ConcKey,Disjoints),
	member(DisjointKey,Disjoints),
	b5kif_name_type_key(DisjointName,_,DisjointKey),
	t5pif_conc_test_p(DisjointName,DisjointKey).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5pif_role_test_p(RoleName) :-
	\+ RoleName = prim(_).

t5pif_role_test_p(RoleName,RoleKey) :-
	\+ RoleName = prim(_),
	\+ t5role_incoherent_p(RoleKey).

t5pif_subs_without_nothing(N,Subs) :-
	t5role_subs(N,S),
	t5tbox_nothing_key(Nothing),
	b5sort_difference(S,[Nothing],Subs).

t5pif_direct_subs_without_nothing(N,Subs) :-
	t5role_direct_subs(N,S),
	t5tbox_nothing_key(Nothing),
	b5sort_difference(S,[Nothing],Subs).

t5pif_direct_subs_without_nothing(N,Filter,Subs) :-
	t5role_direct_subs(N,Filter,S),
	t5tbox_nothing_key(Nothing),
	b5sort_difference(S,[Nothing],Subs).

t5pif_super_role(nv,RoleName,SuperRoleName):-
	b5kif_name_type_key(RoleName,role,RoleKey),
	t5role_supers(RoleKey,Supers),
	member(SuperRoleKey,Supers),
	b5kif_name_type_key(SuperRoleName,role,SuperRoleKey),
	t5pif_role_test_p(SuperRoleName,SuperRoleKey).

t5pif_super_role(nn,RoleName,SuperRoleName):-
	b5kif_name_type_key(RoleName,role,RoleKey),
	b5kif_name_type_key(SuperRoleName,role,SuperRoleKey),
	t5role_subsumes_p(SuperRoleKey,RoleKey).

t5pif_super_role(vn,RoleName,SuperRoleName):-
	b5kif_name_type_key(SuperRoleName,role,SuperRoleKey),
	t5pif_subs_without_nothing(SuperRoleKey,Subs),
	member(RoleKey,Subs),
	b5kif_name_type_key(RoleName,role,RoleKey),
	\+ RoleName = prim(_).

t5pif_super_role(vv,RoleName,SuperRoleName):-
	b5kif_name_type_key(RoleName,role,RoleKey),
	t5pif_role_test_p(RoleName,RoleKey),
	t5role_supers(RoleKey,Supers),
	member(SuperRoleKey,Supers),
	b5kif_name_type_key(SuperRoleName,role,SuperRoleKey),
	t5pif_role_test_p(SuperRoleName,SuperRoleKey).

t5pif_direct_super_role(nv,RoleName,DirectSuperName):-
	b5kif_name_type_key(RoleName,role,RoleKey),
	t5role_direct_supers(RoleKey,DirectSupers),
	member(DirectSuperKey,DirectSupers),
	b5kif_name_type_key(DirectSuperName,role,DirectSuperKey),
	t5pif_role_test_p(DirectSuperName,DirectSuperKey).

t5pif_direct_super_role(nn,RoleName,DirectSuperName):-
	b5kif_name_type_key(RoleName,role,RoleKey),
	b5kif_name_type_key(DirectSuperName,role,DirectSuperKey),
	t5role_direct_supers(RoleKey,DirectSupers),
	member(DirectSuperKey,DirectSupers).

t5pif_direct_super_role(vn,RoleName,DirectSuperName):-
	b5kif_name_type_key(DirectSuperName,role,DirectSuperKey),
	t5pif_direct_subs_without_nothing(DirectSuperKey,Subs),
	member(RoleKey,Subs),
	b5kif_name_type_key(RoleName,role,RoleKey),
	\+ RoleName = prim(_).

t5pif_direct_super_role(vv,RoleName,DirectSuperName):-
	b5kif_name_type_key(RoleName,role,RoleKey),
	t5pif_role_test_p(RoleName,RoleKey),
	t5role_direct_supers(RoleKey,Supers),
	member(DirectSuperKey,Supers),
	b5kif_name_type_key(DirectSuperName,role,DirectSuperKey),
	t5pif_role_test_p(DirectSuperName,DirectSuperKey).

t5pif_super_prim_role(nv,RoleName,SuperPrimName):-
	b5st_class_entry(RoleName,(r,0),RoleKey,_),
	t5role_direct_supers(RoleKey,user_primitive,Supers),
	member(SuperPrimKey,Supers),
	b5st_class_entry(SuperPrimName,(r,0),SuperPrimKey,_),
	t5pif_role_test_p(SuperPrimName,SuperPrimKey).

t5pif_super_prim_role(nn,RoleName,SuperPrimName):-
	b5st_class_entry(RoleName,(r,0),RoleKey,_),
	b5st_class_entry(SuperPrimName,(r,0),SuperPrimKey,_),
	t5role_direct_supers(RoleKey,user_primitive,DirectSupers),
	member(SuperPrimKey,DirectSupers).

t5pif_super_prim_role(vn,RoleName,SuperPrimName):-
	b5st_class_entry(SuperPrimName,(r,0),SuperPrimKey,_),
	t5role_direct_subs(SuperPrimKey,Subs),
	member(RoleKey,Subs),
	b5st_class_entry(RoleName,(r,0),RoleKey,_),
	t5pif_role_test_p(RoleName,RoleKey).

t5pif_super_prim_role(vv,RoleName,SuperPrimName):-
	b5st_class_entry(RoleName,(r,0),RoleKey,_),
	t5pif_role_test_p(RoleName,RoleKey),
	t5role_direct_supers(RoleKey,user_primitive,Supers),
	member(SuperPrimKey,Supers),
	b5st_class_entry(SuperPrimName,(r,0),SuperPrimKey,_),
	t5pif_role_test_p(SuperPrimName,SuperPrimKey).

t5pif_disjoint_role(nv,RoleName,DisjointName):-
	b5kif_name_type_key(RoleName,role,RoleKey),
	t5role_disjoints(RoleKey,Disjoints),
	member(DisjointKey,Disjoints),
	b5kif_name_type_key(DisjointName,role,DisjointKey),
	t5pif_role_test_p(DisjointName,DisjointKey).

t5pif_disjoint_role(nn,RoleName,DisjointName):-
	b5kif_name_type_key(RoleName,role,RoleKey),
	b5kif_name_type_key(DisjointName,role,DisjointKey),
	t5role_disjoint_p(RoleKey,DisjointKey).

t5pif_disjoint_role(vn,RoleName,DisjointName):-
	b5kif_name_type_key(DisjointName,role,DisjointKey),
	t5role_disjoints(DisjointKey,Disjoints),
	member(RoleKey,Disjoints),
	b5kif_name_type_key(RoleName,role,RoleKey),
	t5pif_role_test_p(RoleName,RoleKey).

t5pif_disjoint_role(vv,RoleName,DisjointName):-
	b5kif_name_type_key(RoleName,role,RoleKey),
	t5pif_role_test_p(RoleName,RoleKey),
	t5role_disjoints(RoleKey,Disjoints),
	member(DisjointKey,Disjoints),
	b5kif_name_type_key(DisjointName,role,DisjointKey),
	t5pif_role_test_p(DisjointName,DisjointKey).

t5pif_range(nv,RoleName,RangeName) :-
	b5kif_name_type_key(RoleName,role,RoleKey),
	t5role_range(RoleKey,RangeKey),
	b5kif_name_type_key(RangeName,Type,RangeKey),
	t5pif_conc_test_p(RangeName,Type,RangeKey).

t5pif_range(nn,RoleName,RangeName) :-
	b5kif_name_type_key(RoleName,role,RoleKey),
	b5kif_name_type_key(RangeName,Type,RangeKey),
	t5pif_conc_test_p(RangeName,Type,RangeKey),
	t5role_range(RoleKey,RangeKey).

t5pif_range(vn,RoleName,RangeName) :-
	b5kif_name_type_key(RangeName,Type,RangeKey),
	\+ Type = role,
	t5role_range(RoleKey,RangeKey),
	b5kif_name_type_key(RoleName,role,RoleKey),
	t5pif_role_test_p(RoleName,RoleKey).

t5pif_range(vv,RoleName,RangeName) :-
	b5kif_name_type_key(RoleName,role,RoleKey),
	t5pif_role_test_p(RoleName,RoleKey),
	t5role_range(RoleKey,RangeKey),
	b5kif_name_type_key(RangeName,_,RangeKey),
	t5pif_conc_test_p(RangeName,RangeKey).

t5pif_user_range(nv,RoleName,RangeName) :-
	b5st_class_entry(RoleName,(r,0),RoleKey,_),
	t5role_range(RoleKey,RangeKey),
	t5concid_filter_holds_p(RangeKey,user_defined),
	b5st_class_entry(RangeName,Type,RangeKey,_),
	t5pif_conc_test_p(RangeName,Type,RangeKey).

t5pif_user_range(nn,RoleName,RangeName) :-
	b5st_class_entry(RoleName,(r,0),RoleKey,_),
	b5st_class_entry(RangeName,Type,RangeKey,_),
	\+ Type = (r,0),
	t5role_range(RoleKey,RangeKey).

t5pif_user_range(vn,RoleName,RangeName) :-
	b5st_class_entry(RangeName,Type,RangeKey,_),
	\+ Type = (r,0),
	t5role_range(RoleKey,RangeKey),
	t5role_filter_holds_p(RoleKey,user_defined),
	b5st_class_entry(RoleName,(r,0),RoleKey,_),
	t5pif_role_test_p(RoleName,RoleKey).

t5pif_user_range(vv,RoleName,RangeName) :-
	b5st_class_entry(RoleName,(r,0),RoleKey,_),
	t5role_range(RoleKey,RangeKey),
	t5concid_filter_holds_p(RangeKey,user_defined),
	b5st_class_entry(RangeName,Type,RangeKey,_),
	t5pif_conc_test_p(RangeName,Type,RangeKey).

t5pif_domain(nv,RoleName,DomainName) :-
	b5kif_name_type_key(RoleName,role,RoleKey),
	t5role_domain(RoleKey,DomainKey),
	b5kif_name_type_key(DomainName,Type,DomainKey),
	t5pif_conc_test_p(DomainName,Type,DomainKey).

t5pif_domain(nn,RoleName,DomainName) :-
	b5kif_name_type_key(RoleName,role,RoleKey),
	b5kif_name_type_key(DomainName,Type,DomainKey),
	\+ Type = role,
	t5role_domain(RoleKey,DomainKey).

t5pif_domain(vn,RoleName,DomainName) :-
	b5kif_name_type_key(DomainName,Type,DomainKey),
	\+ Type = role,
	t5role_domain(RoleKey,DomainKey),
	b5kif_name_type_key(RoleName,role,RoleKey),
	t5pif_role_test_p(RoleName,RoleKey).

t5pif_domain(vv,RoleName,DomainName) :-
	b5kif_name_type_key(RoleName,role,RoleKey),
	t5pif_role_test_p(RoleName,RoleKey),
	t5role_domain(RoleKey,DomainKey),
	b5kif_name_type_key(DomainName,_,DomainKey),
	t5pif_conc_test_p(DomainName,DomainKey).

t5pif_user_domain(nv,RoleName,DomainName) :-
	b5st_class_entry(RoleName,(r,0),RoleKey,_),
	t5role_domain(RoleKey,DomainKey),
	t5concid_filter_holds_p(DomainKey,user_defined),
	b5st_class_entry(DomainName,_,DomainKey,_),
	t5pif_conc_test_p(DomainName,DomainKey).

t5pif_user_domain(nn,RoleName,DomainName) :-
	b5st_class_entry(RoleName,(r,0),RoleKey,_),
	b5st_class_entry(DomainName,Type,DomainKey,_),
	\+ Type = (r,0),
	t5role_domain(RoleKey,DomainKey).

t5pif_user_domain(vn,RoleName,DomainName) :-
	b5st_class_entry(DomainName,Type,DomainKey,_),
	\+ Type = (r,0),
	t5role_domain(RoleKey,DomainKey),
	t5role_filter_holds_p(RoleKey,user_defined),
	b5st_class_entry(RoleName,(r,0),RoleKey,_),
	t5pif_role_test_p(RoleName,RoleKey).

t5pif_user_domain(vv,RoleName,DomainName) :-
	b5st_class_entry(RoleName,(r,0),RoleKey,_),
	t5role_domain(RoleKey,DomainKey),
	t5concid_filter_holds_p(DomainKey,user_defined),
	b5st_class_entry(DomainName,_,DomainKey,_),
	t5pif_conc_test_p(DomainName,DomainKey).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5pif_super(nv,TermName,SuperName) :-
	b5kif_name_type_key(TermName,Type,_),
	(Type = role,
	t5pif_super_role(nv,TermName,SuperName)
    ;
	t5pif_super_concept(nv,TermName,SuperName)). 

t5pif_super(nn,TermName,SuperName) :-
	b5kif_name_type_key(TermName,Type,_),
	(Type = role,
	t5pif_super_role(nn,TermName,SuperName)
    ;
	t5pif_super_concept(nn,TermName,SuperName)). 

t5pif_super(vn,TermName,SuperName) :-
	b5kif_name_type_key(SuperName,Type,_),
	(Type = role,
	t5pif_super_role(vn,TermName,SuperName)
    ;
	t5pif_super_concept(vn,TermName,SuperName)). 

t5pif_super(vv,TermName,SuperName) :-
	b5kif_name_type_key(TermName,Type,_),
	(Type = role,
	t5pif_super_role(vv,TermName,SuperName)
    ;
	t5pif_super_concept(vv,TermName,SuperName)). 

t5pif_direct_super(nv,TermName,DirectSuperName) :-
	b5kif_name_type_key(TermName,Type,_),
	(Type = role,
	t5pif_direct_super_role(nv,TermName,DirectSuperName)
    ;
	t5pif_direct_super_concept(nv,TermName,DirectSuperName)). 

t5pif_direct_super(nn,TermName,DirectSuperName) :-
	b5kif_name_type_key(TermName,Type,_),
	(Type = role,
	t5pif_direct_super_role(nn,TermName,DirectSuperName)
    ;
	t5pif_direct_super_concept(nn,TermName,DirectSuperName)). 

t5pif_direct_super(vn,TermName,DirectSuperName) :-
	b5kif_name_type_key(DirectSuperName,Type,_),
	(Type = role,
	t5pif_direct_super_role(vn,TermName,DirectSuperName)
    ;
	t5pif_direct_super_concept(vn,TermName,DirectSuperName)). 

t5pif_direct_super(vv,TermName,DirectSuperName) :-
	b5kif_name_type_key(TermName,Type,_),
	(Type = role,
	t5pif_direct_super_role(vv,TermName,DirectSuperName)
    ;
	t5pif_direct_super_concept(vv,TermName,DirectSuperName)). 

t5pif_disjoint(nv,TermName,DisjointName) :-
	b5kif_name_type_key(TermName,Type,_),
	(Type = role,
	t5pif_disjoint_role(nv,TermName,DisjointName)
    ;
	t5pif_disjoint_concept(nv,TermName,DisjointName)). 
	
t5pif_disjoint(nn,TermName,DisjointName) :-
	b5kif_name_type_key(TermName,Type,_),
	(Type = role,
	t5pif_disjoint_role(nv,TermName,DisjointName)
    ;
	t5pif_disjoint_concept(nv,TermName,DisjointName)). 

t5pif_disjoint(vn,TermName,DisjointName) :-
	b5kif_name_type_key(DisjointName,Type,_),
	(Type = role,
	t5pif_disjoint_role(vn,TermName,DisjointName)
    ;
	t5pif_disjoint_concept(vn,TermName,DisjointName)). 

t5pif_disjoint(vv,TermName,DisjointName) :-
	b5kif_name_type_key(DisjointName,Type,_),
	(Type = role,
	t5pif_disjoint_role(vv,TermName,DisjointName)
    ;
	t5pif_disjoint_concept(vv,TermName,DisjointName)). 

t5pif_role_kind(TermName,T/K,Source) :-
	b5kif_name_type_key(TermName,T,TermKey),
	t5pif_role_test_p(TermName,TermKey),
	t5role_range(TermKey,RangeKey),
	b5kif_name_type_key(_,KType,RangeKey),
	(KType = aset-AttrDomKey ->
	    b5st_domain_entry(AttrDomName,(a,_),AttrDomKey,_,_),	
	    K = AttrDomName
	;
	K = KType),
	t5rdb_filter(TermKey,Filter),
	(t5fil_holds_p(user_defined,Filter) ->
	    Source = user
	;
	(t5fil_holds_p(predef,Filter) ->
	    Source = predef
	;
	Source = anonym)).

t5pif_conc_kind(TermName,Type,Source) :-
	b5kif_name_type_key(TermName,Type,TermKey),
	t5pif_conc_test_p(TermName,Type,TermKey),
	t5concid_filter(TermKey,Filter),
	(t5fil_holds_p(user_defined,Filter) ->
	    Source = user
	;
	(t5fil_holds_p(predef,Filter) ->
	    Source = predef
	;
	Source = anonym)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5pif_disjoint_aset(nv,AsetName,DisjointAsetName) :-
	b5kif_name_type_key(AsetName,aset-_,AsetKey),
	t5concid_disjoints(AsetKey,Disjoints),
	member(DisjointAsetKey,Disjoints),
	b5kif_name_type_key(DisjointAsetName,aset-_,DisjointAsetKey),
	t5pif_conc_test_p(DisjointAsetName,DisjointAsetKey).

t5pif_disjoint_aset(nn,AsetName,DisjointAsetName):-
	b5kif_name_type_key(AsetName,aset-_,AsetKey),
	b5kif_name_type_key(DisjointAsetName,aset-_,DisjointAsetKey),
	t5concid_disjoint_p(AsetKey,DisjointAsetKey).

t5pif_disjoint_aset(vn,AsetName,DisjointAsetName):-
	b5kif_name_type_key(DisjointAsetName,aset-_,DisjointAsetKey),
	t5concid_disjoints(DisjointAsetKey,Disjoints),
	member(AsetKey,Disjoints),
	b5kif_name_type_key(AsetName,aset-_,AsetKey),
	t5pif_conc_test_p(AsetName,AsetKey).

t5pif_disjoint_aset(vv,AsetName,DisjointAsetName):-
	b5kif_all_aset(AsetName),
	t5pif_disjoint_aset(nv,AsetName,DisjointAsetName).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5pif_super_number(nv,NumberName,SuperNumberName) :-
	b5kif_name_type_key(NumberName,number,NumberKey),
	t5concid_supers(NumberKey,Supers),
	member(SuperNumberKey,Supers),
	b5kif_name_type_key(SuperNumberName,number,SuperNumberKey),
	t5pif_conc_test_p(SuperNumberName,SuperNumberKey).

t5pif_super_number(nn,NumberName,SuperNumberName):-
	b5kif_name_type_key(NumberName,number,NumberKey),
	b5kif_name_type_key(SuperNumberName,number,SuperNumberKey),
	t5concid_subsumes_p(SuperNumberKey,NumberKey).

t5pif_super_number(vn,NumberName,SuperNumberName):-
	b5kif_name_type_key(SuperNumberName,number,SuperNumberKey),
	b5kif_subsx(SuperNumberKey,Subs),      %% eliminates "nothing"
	member(NumberKey,Subs),
	b5kif_name_type_key(NumberName,number,NumberKey).

t5pif_super_number(vv,NumberName,SuperNumberName):-
	b5kif_name_type_key(NumberName,number,NumberKey),
	t5pif_conc_test_p(NumberName,NumberKey),
	t5concid_supers(NumberKey,Supers),
	member(SuperNumberKey,Supers),
	b5kif_name_type_key(SuperNumberName,number,SuperNumberKey),
	t5pif_conc_test_p(NumberName,NumberKey).

t5pif_disjoint_number(nv,NumberName,DisjointName):-
	b5kif_name_type_key(NumberName,number,NumberKey),
	t5concid_disjoints(NumberKey,Disjoints),
	member(DisjointKey,Disjoints),
	b5kif_name_type_key(DisjointName,number,DisjointKey),
	t5pif_conc_test_p(DisjointName,DisjointKey).

t5pif_disjoint_number(nn,NumberName,DisjointName):-
	b5kif_name_type_key(NumberName,number,NumberKey),
	b5kif_name_type_key(DisjointName,number,DisjointKey),
	t5concid_disjoint_p(NumberKey,DisjointKey).

t5pif_disjoint_number(vn,NumberName,DisjointName):-
	b5kif_name_type_key(DisjointName,number,DisjointKey),
	t5concid_disjoints(DisjointKey,Disjoints),
	member(NumberKey,Disjoints),
	b5kif_name_type_key(NumberName,number,NumberKey),
	t5pif_conc_test_p(NumberName,NumberKey).

t5pif_disjoint_number(vv,NumberName,DisjointName):-
	b5kif_name_type_key(NumberName,number,NumberKey),
	t5pif_conc_test_p(NumberName,number,NumberKey),
	t5concid_disjoints(NumberKey,Disjoints),
	member(DisjointKey,Disjoints),
	b5kif_name_type_key(DisjointName,number,DisjointKey),
	t5pif_conc_test_p(DisjointName,DisjointKey).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IBox: iboxget								 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% concepts								 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iboxget(super_concept(ConcName,SuperConcName)) :-
	b5kif_var(ConcName,SuperConcName,XY),
	t5pif_i_super_concept(XY,ConcName,SuperConcName).

iboxget(direct_super_concept(ConcName,DirectSuperName)) :-
	b5kif_var(ConcName,DirectSuperName,XY),
	t5pif_direct_super_concept(XY,ConcName,DirectSuperName).

iboxget(user_direct_super_concept(ConcName,UserDirectName)) :-
	b5kif_var(ConcName,UserDirectName,XY),
	t5pif_i_user_direct_super_concept(XY,ConcName,UserDirectName).

iboxget(equivalent_concept(ConcName,EquivalentName)) :-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	i5ibox_get_ibox_conc(ConcKey,IBoxConcKey),
	b5kif_name_type_key(EquivalentName,Type,EquivalentKey),
	i5ibox_get_ibox_conc(EquivalentKey,IBoxEquivalentKey),
	i5ibox_equivalent_p(conc,IBoxConcKey,IBoxEquivalentKey),
	EquivalentName \== ConcName.

iboxget(disjoint_concept(ConcName,DisjointName)) :-
	b5kif_var(ConcName,DisjointName,XY),
	t5pif_i_disjoint_concept(XY,ConcName,DisjointName).

iboxget(incoherent_concept(ConcName)) :-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	\+ Type = role,
	i5ibox_incoherent_p(conc,ConcKey).

iboxget(restriction(ConcName,RoleName,VRName,Min,Max)) :-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	t5pif_conc_test_p(ConcName,Type,ConcKey),
	b5kif_name_type_key(RoleName,role,RoleKey),
	t5pif_role_test_p(RoleName,RoleKey),
	i5ibox_get_ibox_conc(ConcKey,IBoxConcKey),
	t5concid_vr_min_max(IBoxConcKey,RoleKey,VRKey,Min,Max),
	b5kif_name_type_key(VRName,Type,VRKey),
	\+ VRName = prim(_).

iboxget(value_restriction(ConcName,RoleName,VRName)) :-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	t5pif_conc_test_p(ConcName,Type,ConcKey),
	b5kif_name_type_key(RoleName,role,RoleKey),
	t5pif_role_test_p(RoleName,RoleKey),
	i5ibox_get_ibox_conc(ConcKey,IBoxConcKey),
	t5concid_vr(IBoxConcKey,RoleKey,VRKey),
	b5kif_name_type_key(VRName,Type,VRKey),
	t5pif_conc_type_name_test_p(VRName,Type).

iboxget(number_restriction(ConcName,RoleName,Min,Max)) :-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	t5pif_conc_test_p(ConcName,Type,ConcKey),
	i5ibox_get_ibox_conc(ConcKey,IBoxConcKey),
	b5kif_name_type_key(RoleName,role,RoleKey),
	t5pif_role_test_p(RoleName,RoleKey),
	t5concid_min_max(IBoxConcKey,RoleKey,Min,Max).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% roles 								 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iboxget(equivalent_role(RoleName,EquiRoleName)) :-
	b5kif_name_type_key(RoleName,role,RoleKey),
	b5kif_name_type_key(EquiRoleName,role,EquiRoleKey),
	EquiRoleName \== RoleName,
	i5ibox_equivalent_p(role,RoleKey,EquiRoleKey).


iboxget(disjoint_role(RoleName,DisjointName)) :-
	b5kif_var(RoleName,DisjointName,XY),
	t5pif_i_disjoint_role(XY,RoleName,DisjointName).

iboxget(incoherent_role(RoleName)) :-
	b5kif_name_type_key(RoleName,role,RoleKey),
	t5role_ibox_incoherent_p(RoleKey).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5pif_i_super_concept(nv,ConcName,SuperConcName) :-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	\+ Type = role,
	i5ibox_get_ibox_conc(ConcKey,IBoxConcKey),
	t5concid_supers(IBoxConcKey,Supers),
	member(SuperConcKey,Supers),
	b5kif_name_type_key(SuperConcName,Type,SuperConcKey),
	t5pif_conc_test_p(SuperConcName,SuperConcKey).

t5pif_i_super_concept(nn,ConcName,SuperConcName):-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	b5kif_name_type_key(SuperConcName,Type,SuperConcKey),
	\+ Type = role,
	i5ibox_get_ibox_conc(ConcKey,IBoxConcKey),
	t5concid_subsumes_p(SuperConcKey,IBoxConcKey).

t5pif_i_super_concept(vn,IBoxConcName,SuperConcName):-
	b5kif_name_type_key(SuperConcName,Type,SuperConcKey),
	\+ Type = role,
	b5kif_subsx(SuperConcKey,Subs),      %% eliminates "nothing"
	member(IBoxConcKey,Subs),
	i5ibox_get_ibox_conc(_,IBoxConcKey),
	t5concid_subsumes_p(SuperConcKey,IBoxConcKey),
	b5kif_name_type_key(IBoxConcName,Type,IBoxConcKey),
	\+ IBoxConcName = prim(_).

t5pif_i_super_concept(vv,ConcName,SuperConcName):-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	t5pif_conc_test_p(ConcName,Type,ConcKey),
	i5ibox_get_ibox_conc(ConcKey,IBoxConcKey),
	t5concid_supers(IBoxConcKey,Supers),
	member(SuperConcKey,Supers),
	b5kif_name_type_key(SuperConcName,Type,SuperConcKey),
	t5pif_conc_test_p(SuperConcName,SuperConcKey).

t5pif_i_direct_super_concept(nv,ConcName,DirectSuperName):-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	\+ Type = role,
	i5ibox_get_ibox_conc(ConcKey,IBoxConcKey),
	t5concid_direct_supers(IBoxConcKey,DirectSupers),
	member(DirectSuperKey,DirectSupers),
	b5kif_name_type_key(DirectSuperName,Type,DirectSuperKey),
	t5pif_conc_test_p(DirectSuperName,DirectSuperKey).

t5pif_i_direct_super_concept(nn,ConcName,DirectSuperName):-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	b5kif_name_type_key(DirectSuperName,Type,DirectSuperKey),
	\+ Type = role,
	i5ibox_get_ibox_conc(ConcKey,IBoxConcKey),
	t5concid_direct_supers(IBoxConcKey,DirectSupers),
	member(DirectSuperKey,DirectSupers).

t5pif_i_direct_super_concept(vn,IBoxConcName,DirectSuperName):-
	b5kif_name_type_key(DirectSuperName,Type,DirectSuperKey),
	\+ Type = role,
	b5kif_direct_subsx(DirectSuperKey,Subs),   % eliminates "nothing"
	member(ConcKey,Subs),
	i5ibox_get_ibox_conc(ConcKey,IBoxConcKey),
	t5concid_subsumes_p(DirectSuperKey,IBoxConcKey),
	b5kif_name_type_key(IBoxConcName,Type,IBoxConcKey),
	\+ IBoxConcName = prim(_).

t5pif_i_direct_super_concept(vv,ConcName,DirectSuperName):-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	t5pif_conc_test_p(ConcName,Type,ConcKey),
	i5ibox_get_ibox_conc(ConcKey,IBoxConcKey),
	t5concid_direct_supers(IBoxConcKey,Supers),
	member(DirectSuperKey,Supers),
	b5kif_name_type_key(DirectSuperName,Type,DirectSuperKey),
	t5pif_conc_test_p(DirectSuperName,DirectSuperKey).

t5pif_i_user_direct_super_concept(nv,ConcName,UserDirectName):-
	b5st_class_entry(ConcName,Type,ConcKey,_),
	\+ Type = role,
	i5ibox_get_ibox_conc(ConcKey,IBoxConcKey),
	t5concid_direct_supers(IBoxConcKey,user_defined,DirectSupers),
	member(UserDirectKey,DirectSupers),
	b5st_class_entry(UserDirectName,Type,UserDirectKey,_),
	t5pif_conc_test_p(UserDirectName,UserDirectKey).

t5pif_i_user_direct_super_concept(nn,ConcName,UserDirectName):-
	b5st_class_entry(ConcName,Type,ConcKey,_),
	b5st_class_entry(UserDirectName,Type,UserDirectKey,_),
	\+ Type = role,
	i5ibox_get_ibox_conc(ConcKey,IBoxConcKey),
	t5concid_direct_supers(IBoxConcKey,user_defined,DirectSupers),
	member(UserDirectKey,DirectSupers).

t5pif_i_user_direct_super_concept(vn,IBoxConcName,UserDirectName):-
	b5st_class_entry(UserDirectName,Type,UserDirectKey,_),
	\+ Type = role,
	b5kif_direct_subsx(UserDirectKey,user_defined,Subs),
	member(ConcKey,Subs),
	i5ibox_get_ibox_conc(ConcKey,IBoxConcKey),
	t5concid_subsumes_p(UserDirectKey,IBoxConcKey),
	b5st_class_entry(IBoxConcName,Type,IBoxConcKey,_),
	\+ IBoxConcName = prim(_).

t5pif_i_user_direct_super_concept(vv,ConcName,UserDirectName):-
	b5st_class_entry(ConcName,Type,ConcKey,_),
	t5pif_conc_test_p(ConcName,Type,ConcKey),
	i5ibox_get_ibox_conc(ConcKey,IBoxConcKey),
	t5concid_direct_supers(IBoxConcKey,user_defined,Supers),
	member(UserDirectKey,Supers),
	b5st_class_entry(UserDirectName,Type,UserDirectKey,_),
	t5pif_conc_test_p(UserDirectName,UserDirectKey).

t5pif_i_disjoint_concept(nv,ConcName,DisjointName) :-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	\+ Type = role,
	b5kif_name_type_key(DisjointName,Type,DisjointKey),
	t5pif_conc_test_p(DisjointName,Type,DisjointKey),
	i5ibox_disjoint_p(conc,ConcKey,DisjointKey).

t5pif_i_disjoint_concept(nn,ConcName,DisjointName) :-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	b5kif_name_type_key(DisjointName,Type,DisjointKey),
	\+ Type = role,
	i5ibox_disjoint_p(conc,ConcKey,DisjointKey).

t5pif_i_disjoint_concept(vn,ConcName,DisjointName) :-
	b5kif_name_type_key(DisjointName,Type,DisjointKey),
	\+ Type = role,
	b5kif_name_type_key(ConcName,Type,ConcKey),
	t5pif_conc_test_p(ConcName,Type,ConcKey),
	i5ibox_disjoint_p(conc,ConcKey,DisjointKey).

t5pif_i_disjoint_concept(vv,ConcName,DisjointName):-
	b5kif_name_type_key(ConcName,Type,ConcKey),
	t5pif_conc_test_p(ConcName,ConcKey),
	b5kif_name_type_key(DisjointName,Type,DisjointKey),
	t5pif_conc_test_p(DisjointName,Type,DisjointKey),
	i5ibox_disjoint_p(conc,ConcKey,DisjointKey),
	t5pif_conc_test_p(DisjointName,DisjointKey).

t5pif_i_disjoint_role(nv,RoleName,DisjointName):-
	b5kif_name_type_key(RoleName,role,RoleKey),
	i5ibox_disjoint_p(role,RoleKey,DisjointKey),
	b5kif_name_type_key(DisjointName,role,DisjointKey),
	t5pif_role_test_p(DisjointName,DisjointKey).

t5pif_i_disjoint_role(nn,RoleName,DisjointName) :-
	b5kif_name_type_key(RoleName,role,RoleKey),
	b5kif_name_type_key(DisjointName,role,DisjointKey),
	i5ibox_disjoint_p(role,RoleKey,DisjointKey).

t5pif_i_disjoint_role(vn,RoleName,DisjointName) :-
	b5kif_name_type_key(DisjointName,role,DisjointKey),
	i5ibox_disjoint_p(role,RoleKey,DisjointKey),
	b5kif_name_type_key(RoleName,role,DisjointKey),
	t5pif_role_test_p(RoleName,RoleKey).

t5pif_i_disjoint_role(vv,RoleName,DisjointName) :-
	b5kif_name_type_key(RoleName,role,RoleKey),
	b5kif_name_type_key(DisjointName,role,DisjointKey),
	t5pif_role_test_p(DisjointName,DisjointKey),
	i5ibox_disjoint_p(role,RoleKey,DisjointKey).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%				BACK FLAGS				       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded(library(lists)).
:- ensure_loaded(library(basics)).

:- multifile b5dump_these_predicates/1.

b5dump_these_predicates([b5sta_flag/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Hack: Die Zuordnung von Zahlen (z.B. {0,1} zu {false,true}) haengt einzig
%% 	 von der Reihenfolge in der Liste PossibleValues ab.


%% flag definitions (flag names must be unique!)
b5sta_flag_definition(aboxfilled,[false,true,abox],system).

b5sta_flag_definition(iboxfilled,[false,true],system).

b5sta_flag_definition(tarevision,[false,true],system).

b5sta_flag_definition(verbosity,[silent,error,warning,info,trace],user).

b5sta_flag_definition(introduction,[noforward,forward],user).

b5sta_flag_definition(revision,[false,true],user).

b5sta_flag_definition(retrieval,[fail,succeed],user).

b5sta_flag_definition(tboxrevision,[fail,succeed],user).

b5sta_flag_definition(tboxdumpfile,[],user).

b5sta_flag_definition(tboxdumpdir,[],user).

%% initial flag values
b5sta_init :-
	b5sta_set_flag(aboxfilled,false),
	b5sta_set_flag(iboxfilled,false),
	b5sta_set_flag(tarevision, false),
	b5sta_set_flag(verbosity,warning),
	b5sta_set_flag(introduction,forward),
	b5sta_set_flag(revision,true),
	b5sta_set_flag(retrieval,succeed),
	b5sta_set_flag(tboxrevision,succeed),
	b5sta_set_flag(tboxdumpfile,'tboxrev.dmp'),
	b5sta_set_flag(tboxdumpdir,'/tmp').

b5sta_user_asks(Flag,Value) :-
	b5sta_determine_flagname(Flag,FlagName),
	b5sta_check_flag(FlagName,Value).

b5sta_user_sets(Flag,Value) :-
	b5sta_determine_flagname(Flag,FlagName),
	b5sta_flag_definition(FlagName,PossibleValues,user),
	b5sta_determine_flagvalue(Value,PossibleValues,FlagValue),
	b5sta_set_flag(FlagName,FlagValue).

%% setting a flag
b5sta_set_flag(Flag,Value) :-
	retractall(b5sta_flag(Flag,_)),
	assert(b5sta_flag(Flag,Value)),!.

%% checking flag value
b5sta_check_flag(Flag,Value) :-
	b5sta_flag(Flag,Value),!.


%% abbreviation expansion for flag names (must be unambiguous !) 
b5sta_determine_flagname(Flag,Flag) :-
	b5sta_flag_definition(Flag,_,_),!.
b5sta_determine_flagname(Flag,FlagName) :-
	findall(FlagName,
			(b5sta_flag_definition(FlagName,_,_),
			 b5sta_prefix_p(Flag,FlagName)),
		FlagNameList),
	(FlagNameList == [] -> 
	    t5out_error(no_valid_flagname(Flag)),!,fail
	;
	(FlagNameList = [FlagName]
    ;
	t5out_error(ambigue_flagname(Flag)),!,fail)),!.

%% .. and for flag values  (must be unambiguous too !) 
b5sta_determine_flagvalue(V,[],V) :- !.
b5sta_determine_flagvalue(Value,PossibleValues,Value) :-
	member(Value,PossibleValues),!.
b5sta_determine_flagvalue(Value,PossibleValues,Flagvalue) :-
	findall(Flagvalue,
	           b5sta_prefix_member(PossibleValues,Value,Flagvalue),
		FlagvalueList),
		(FlagvalueList = [Flagvalue]
	    ;
	    FlagvalueList = [_|_],
	    t5out_error(ambigue_flagvalue(Value),!,fail)),!.
b5sta_determine_flagvalue(Value,PossibleValues,FlagValue) :-
	number(Value),
	nth0(Value,PossibleValues,FlagValue), !.
b5sta_determine_flagvalue(Value,_,_) :-
	t5out_error(no_valid_flagvalue(Value)),
	!,fail.

b5sta_prefix_p(Word1,Word2) :-
	name(Word1,List1),
	name(Word2,List2),
	append(List1,_,List2).

b5sta_prefix_member([V1|_],Value,V1) :-
	b5sta_prefix_p(Value,V1),!.
b5sta_prefix_member([_|R],Value,FlagValue) :-
	b5sta_prefix_member(R,Value,FlagValue).


b5sta_print_all_flags :-
	nl,
	write('Current flag settings:'),nl,
	write('----------------------'),nl,nl,
	b5sta_flag(Flag,Value),
	name(Flag,List),
	length(List,L),
	b5sta_format(List,L,[32,32],FlagForm),
	write(FlagForm),write(Value),nl,
	fail.
b5sta_print_all_flags.


b5sta_format(List,L,_,FlagForm) :-
	L > 25,!,
	name(FlagForm,List).
b5sta_format(List,L,Points,FlagForm) :-
	L < 25,!,
	L1 is L + 1,
	b5sta_format(List,L1,[46|Points],FlagForm).
b5sta_format(List,L,Points,FlagForm) :-
	L = 25,!,
	append(List,[32,32|Points],LP),
	name(FlagForm,LP).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%				END OF FILE				       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BACKRETRIEVE
% For details see below. One basic decision had to be taken: Should a retrieval
% function always succeed, eventually returning error-tags at appropriate
% places but trying to partially evaluate the query as far as possible, or
% should the function fail when detecting any kind of error?
% Since we could not find a conclusion I settled for a flag (what else?).
% The flag is named retrieval and has two states: fail and succeed.
% Succeed means that as much as possible will be evaluated, fail means that the
% function stops at the slightest fault.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- [library(printlength)].


b5par_retrieval(noibox(RetExp)) :-
	!,
	b5par_retrieval(RetExp,nfs).		
b5par_retrieval(RetExp) :-
	b5par_retrieval(RetExp,nfi).


b5par_retrieval(Var = difference(A,B),NFX) :-
	!,b5par_retrieval(difference(A,B),NFX,_,_,Var).
/*b5par_retrieval(RetExp,NFX) :-
	b5par_retrieval(RetExp,NFX,_,_,Result),
	writeq(Result).	*/
b5par_retrieval(Var = RetExp,NFX) :-
	!,b5par_retrieval(RetExp,NFX,_,_,Result),
	b5par_retrieval_postprocess(Result,Var).
b5par_retrieval(RetExp,NFX) :-
	b5par_retrieval(RetExp,NFX,Actions,Arguments,Result),
	b5par_retrieval_postprocess(Arguments,Result,Actions).


b5par_retrieval(difference(Arg1,Arg2),NFX,_,_,Result) :-
	b5par_obj_name2key(Arg1,Type,Key1),
	b5par_obj_name2key(Arg2,Type,Key2),
	!,
	b5kif_difference(Key1,Key2,NFX,Diff1,Diff2),
	Result = [Diff1,Diff2].
b5par_retrieval(difference(Arg1,Arg2),NFX,_,_,Result) :-
	b5par_main(Arg1,no_unknown_names,Type,[],[],KeyTerm1,[],_,[],FI1),
	b5par_main(Arg2,no_unknown_names,Type,[],[],KeyTerm2,[],_,[],FI2),
	b5par_forward_introduction(FI1,_),
	b5par_forward_introduction(FI2,_),
	t5fil_filter(internal,Filter1),
	t5fil_filter(internal,Filter2),
	b5par_tt_to_kernel(Type,KeyTerm1,_,Filter1,Key1,_,def),
	b5par_tt_to_kernel(Type,KeyTerm2,_,Filter2,Key2,_,def),
	b5kif_difference(Key1,Key2,NFX,Diff1,Diff2),
	Result = [Diff1,Diff2].
b5par_retrieval([Generator|R] for Arguments,NFX,[Generator|R],Arguments,RetrievalResult) :-
	!,
	b5par_evaluate_generator([Generator|R],Transmitter,EmptyResult),
	b5par_transform_phase_one(Arguments,Transmitter,EmptyResult,Phase1List),	
	b5par_transform_phase_two(Phase1List,NFX,Phase2List),
	b5par_kernel_retrieval(Phase2List,NFX,RetrievalResult).

b5par_retrieval(describe Argument,NFX,Actions,Arguments,Result) :-
	!,
	b5par_retrieval([describe] for Argument,NFX,Actions,Arguments,Result).

b5par_retrieval(describe_fully Argument,NFX,Actions,Arguments,Result) :-
	!,
	b5par_retrieval([describe_fully] for Argument,NFX,Actions,Arguments,Result).

b5par_retrieval(introduced_as Argument,NFX,Actions,Arguments,Result) :-
	!,
	b5par_retrieval([introduced_as] for Argument,NFX,Actions,Arguments,Result).

b5par_retrieval(defined_as Argument,NFX,Actions,Arguments,Result) :-
	!,
	b5par_retrieval([defined_as] for Argument,NFX,Actions,Arguments,Result).

b5par_retrieval(msc Argument,NFX,Actions,Arguments,Result) :-
	!,
	b5par_retrieval([msc] for Argument,NFX,Actions,Arguments,Result).

b5par_retrieval(self Argument,NFX,Actions,Arguments,Result) :-
	!,
	b5par_retrieval([self] for Argument,NFX,Actions,Arguments,Result).

b5par_retrieval(Argument,NFX,Actions,Arguments,Result) :-
	!,
	b5par_retrieval([self] for Argument,NFX,Actions,Arguments,Result).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EVALUATING THE 'TUPEL GENERATOR'
%
% The tupel generator is a list of actions that are to be performed on all
% arguments.
% Users might 	- specify wrong actions
%		- specify the same action several times
%		- want to have the results ordered according to the
%		  ordering of the actions instead of a standard ordering
%		- specify actions which don't fit the arguments
%
% The system should 
%		- compute each action for any argument only once
%		- list the results in the ordering of the actions, thus
%		  eventually displaying the same result several times
%
% The system will
%	depending on the state of the retrieval flag 
%		- stop when encountering a wrong action or
%		- continue to process the correct actions and return
%		  some special answer for the wrong action
%
% This function computes 
%		- The EmptyResult. The ER is a list that contains exactly
%		  one element for every action in the TupelGenerator.
%		  An element corresponding to an illegal action will be
%		  instantiated with wrong_action(Explanation). All other
%		  elements will be variables. Elements corresponding to
%		  the same action are instantiated with the same variable.
%
%		- The Transmitter. The TR is a list of length n, n being
%		  the number of legal actions. Every element of the TR
%		  corresponds to one of them. Elements corresponding
%		  to actions that do not occur in the TupelGenerator will
%		  be instantiated with skip. The others will be 
%		  instantiated with either perform(action,VAR) or
%		  perform(role(Key),VAR). The VAR's are the same as in 
%		  the ER.
%
% An example:
%
%	TupelGenerator : 
%	[self,junkaction]
%
%	Transmitter    : 
%	[skip,skip,skip,skip,perform(action(_selfvar)),skip,skip,skip,skip]
%
%	EmptyResult    : [_selfvar,wrong_action(illegal(junkaction))]
%
%
%
% The purpose of this is to compute every action needed only once and bind
% the result to all elements of the ResultList simultaneously
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b5par_evaluate_generator(Generator,Transmitter,EmptyResult) :-
	b5par_evaluate_generator(Generator,[],Transmitter,EmptyResult).

b5par_evaluate_generator([],TM,TM,[]) :- !.
b5par_evaluate_generator([Generator|R],TMi,TMo,[ResultVar|L]) :-
	b5par_evaluate_generator(R,TMi,TMx,L),
	b5par_evaluate_single_generator(Generator,TMx,TMo,ResultVar).


b5par_evaluate_single_generator(Generator,TM,TM,Var) :-
	var(Generator),!,
	(b5sta_check_flag(retrieval,succeed) ->
	    Var = [wrong_generator(variable)]
	;
	t5out_error(wrong_generator(variable)),
	!,fail).
b5par_evaluate_single_generator(describe,TMi,TMo,Var) :-
	!,b5par_add_to_transmitter(perform(describe,Var),TMi,TMo).
b5par_evaluate_single_generator(describe_fully,TMi,TMo,Var) :-
	!,b5par_add_to_transmitter(perform(describe_fully,Var),TMi,TMo).
b5par_evaluate_single_generator(introduced_as,TMi,TMo,Var) :-
	!,b5par_add_to_transmitter(perform(introduced_as,Var),TMi,TMo).
b5par_evaluate_single_generator(defined_as,TMi,TMo,Var) :-
	!,b5par_add_to_transmitter(perform(defined_as,Var),TMi,TMo).
b5par_evaluate_single_generator(self,TMi,TMo,Var) :-
	!,b5par_add_to_transmitter(perform(self,Var),TMi,TMo).
b5par_evaluate_single_generator(msc,TMi,TMo,Var) :-
	!,b5par_add_to_transmitter(perform(msc,Var),TMi,TMo).
b5par_evaluate_single_generator(vr(inv(Role)),TMi,TMo,Var) :-
	!,
	((atom(Role),b5st_class_entry(Role,(r,0),Key,_)) ->
	    t5role_inv_role(Key,InvKey),
	    (InvKey == notexists ->
		b5par_add_to_transmitter(perform(vr(inv(Key)),Var),TMi,TMo)
	    ;
	    b5par_add_to_transmitter(perform(vr(InvKey),Var),TMi,TMo))
	;
	(b5sta_check_flag(retrieval,succeed) ->
	    Var = [wrong_generator(vr(Role))]
	;
	t5out_error(wrong_generator(vr(Role))),
	!,fail)).
b5par_evaluate_single_generator(vr(Role),TMi,TMo,Var) :-
	!,
	((atom(Role),b5st_class_entry(Role,(r,0),Key,_)) ->
	    b5par_add_to_transmitter(perform(vr(Key),Var),TMi,TMo)
	;
	(b5sta_check_flag(retrieval,succeed) ->
	    Var = [wrong_generator(vr(Role))]
	;
	t5out_error(wrong_generator(vr(Role))),
	!,fail)).
b5par_evaluate_single_generator(rf(inv(Role)),TMi,TMo,Var) :-
	!,
	((atom(Role),b5st_class_entry(Role,(r,0),Key,_)) ->
	    t5role_inv_role(Key,InvKey),
	    (InvKey == notexists ->
		b5par_add_to_transmitter(perform(rf(inv(Key)),Var),TMi,TMo)
	    ;
	    b5par_add_to_transmitter(perform(rf(InvKey),Var),TMi,TMo))
	;
	(b5sta_check_flag(retrieval,succeed) ->
	    Var = [wrong_generator(rf(Role))]
	;
	t5out_error(wrong_generator(rf(Role))),
	!,fail)).
b5par_evaluate_single_generator(rf(Role),TMi,TMo,Var) :-
	!,
	((atom(Role),b5st_class_entry(Role,(r,0),Key,_)) ->
	    b5par_add_to_transmitter(perform(rf(Key),Var),TMi,TMo)
	;
	(b5sta_check_flag(retrieval,succeed) ->
	    Var = [wrong_generator(rf(Role))]
	;
	t5out_error(wrong_generator(rf(Role))),
	!,fail)).
b5par_evaluate_single_generator(nr(inv(Role)),TMi,TMo,Var) :-
	!,
	((atom(Role),b5st_class_entry(Role,(r,0),Key,_)) ->
	    t5role_inv_role(Key,InvKey),
	    (InvKey == notexists ->
		b5par_add_to_transmitter(perform(nr(inv(Key)),Var),TMi,TMo)
	    ;
	    b5par_add_to_transmitter(perform(nr(InvKey),Var),TMi,TMo))
	;
	(b5sta_check_flag(retrieval,succeed) ->
	    Var = [wrong_generator(nr(Role))]
	;
	t5out_error(wrong_generator(nr(Role))),
	!,fail)).
b5par_evaluate_single_generator(nr(Role),TMi,TMo,Var) :-
	!,
	((atom(Role),b5st_class_entry(Role,(r,0),Key,_)) ->
	    b5par_add_to_transmitter(perform(nr(Key),Var),TMi,TMo)
	;
	(b5sta_check_flag(retrieval,succeed) ->
	    Var = [wrong_generator(nr(Role))]
	;
	t5out_error(wrong_generator(nr(Role))),
	!,fail)).
b5par_evaluate_single_generator(Generator,TM,TM,Var) :-
	(b5sta_check_flag(retrieval,succeed) ->
	    Var = [wrong_generator(illegal(Generator))]
	;
	t5out_error(wrong_generator(illegal(Generator))),
	!,fail).

b5par_add_to_transmitter(Action,TMi,TMo) :-
	(member(Action,TMi) ->
	    TMo = TMi
	;
	TMo = [Action|TMi]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TRANSFORMATION OF ARGUMENTS TO KEYS
% 
% If the TR contains at least one legal action, all arguments are transformed
% into keys. Otherwise the predicate returns 'no_valid_action'.
%
% The transformation is a two-step process:
%
% 1. In phase one, all arguments will be disambiguated. All arguments but terms
%	will be replaced by their keys
% 2. In phase two, keys for the term-arguments will be computed
%
% This distinction is made to compute the term's keys the latest possible.
% Otherwise, if the retrieval flag is 'fail', the system might start to compute
% keys for some terms before detecting a faulty argument and aborting the
% retrieval, which would take unnecessarily long. Accordingly, 'theknown'
% and 'getall' expressions will only be evaluated in phase two.
%
%
% PHASE I: DISAMBIGUATION OF ARGUMENTS
%
% This predicate returns the Phase1List. It contains one element for every
% element of the ArgumentList. For wrong elements, it returns
% 'wrong_element(Explanation)'. Otherwise, it returns a tuple (A-B-C)-(D-E), 
% where
%
%	- A is either	key(Key) or
%			getall(Term) or
%			theknown(Term) or
%			compute(Term) or
%			ambigue(Name)
%
%	- B is either 	conc or
%			role or
%			Domain
%
%	- C is either 	name or
%			obj or
%			term
%
%	- D is the argument's transmitter copy
%
%	- E is the argument's (empty) result form
%	
% An Argument can be wrong for different reasons: Syntax, type errors,
% ambiguity etc. Beside of that, for every correct argument, the actions
% have to be tested for feasability. Not every argument/action combination
% is allowed. This test is performed right after the disambiguation.
% For illegal combinations 'illegal_action(Triple, Action)' will be inserted
% into the argument's TR.
%
% Ambiguity is a special case, because 'introduced_as' is a legal action
% on ambigue names. Therefore, if the ambigue argument is a name,
% all other actions become illegal but the argument is considered correct.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% b5par_transform_phase_one( +Arguments, +TM, +ER, -Phase1List) 

b5par_transform_phase_one(_,TR,_,no_valid_action) :-
	\+ member(perform(_,_),TR),!.	
b5par_transform_phase_one([X|R]/DA,TM,ER,Phase1List) :-
	!,b5par_disamb_all_args([X|R],DA,TM,ER,Phase1List).
b5par_transform_phase_one([X|R],TM,ER,Phase1List) :-
	!,b5par_disamb_all_args([X|R],_,TM,ER,Phase1List).
b5par_transform_phase_one(A/DA,TM,ER,Phase1List) :-
	!,b5par_disamb_all_args([A],DA,TM,ER,Phase1List).
b5par_transform_phase_one(A,TM,ER,Phase1List) :-
	!,b5par_disamb_all_args([A],_,TM,ER,Phase1List).


b5par_disamb_all_args([],_,_,_,[]) :- !.
b5par_disamb_all_args([Arg1|R],DA,TM,ER,[Phase1Arg|L1]) :-
	copy_term(DA,ADA),
	b5par_disamb_arg(Arg1,ADA,Triple),
	b5par_combinations(Triple,TM,ER,Phase1Arg),
	b5par_disamb_all_args(R,DA,TM,ER,L1).


b5par_disamb_arg(Arg/ADA,ADA,Triple) :-
	!,b5par_da0(Arg,ADA,Triple).
b5par_disamb_arg(_/ADA,LDA,wrong_argument(mismatch(ADA,LDA))) :- !.
b5par_disamb_arg(Arg,LDA,Triple) :-
	b5par_da0(Arg,LDA,Triple).



b5par_da0(Arg,Domain^Sort,wrong_argument(wrong_da(Arg,Domain^Sort))) :-
	ground(Domain^Sort),
	(\+ b5st_domain_entry(Domain,_,_,_,_)
    ;
	(Sort \== cls,
	Sort \== obj)),!.
b5par_da0(getall(Term),DA,Triple) :-
	!,
	b5par_dnf(Term,DNF),
	(b5par_dnf_term(DNF,DA,Type) ->
	    Triple = getall(Term)-Type-obj
	;
	Triple = wrong_argument(wrong_getall(Term))).
b5par_da0(Other,UDA,Triple) :-
	b5par_da1(Other,UDA,Triple).


b5par_da1(MacroLHS,DA,Triple) :-
	b5st_macro_entry(MacroLHS,MacroRHS),!,
	b5par_da1(MacroRHS,DA,Triple).
b5par_da1(Name,DA,Triple) :-
	b5par_da2(Name,DA,Triple),!.
b5par_da1(Term,DA,Triple) :-
	b5par_da3(Term,DA,Triple),!.


b5par_da2(Arg,DA,Triple) :-
	(b5par_uc(Arg,I) ->
	    ((b5par_key_exists_p(obj,conc,I),DA = obj) ->
		Triple = key(I)-conc-obj
	    ;
	    Triple = wrong_argument(existence(Arg,DA)))
	;
	(Arg = conc(I) ->
	    ((b5st_class_entry(ConcName,(c,0),I,_), DA = conc) ->
		Triple = key(I)-conc-name(ConcName)
	    ;
	    ((b5par_key_exists_p(class,conc,I), DA = conc) ->
		Triple = key(I)-conc-term
	    ;
	    Triple = wrong_argument(existence(conc(I),DA))))
	;
	(Arg = role(I) ->
	    ((b5st_class_entry(RoleName,(r,0),I,_), DA = role) ->
		Triple = key(I)-role-name(RoleName)
	    ;
	    ((b5par_key_exists_p(class,role,I), DA = role) ->
		Triple = key(I)-role-term
	    ;
	    Triple = wrong_argument(existence(role(I),DA))))
	;
	(((functor(Arg,Domain,1),\+ Arg = aset([]), \+ Arg = aset([_|_]), b5st_domain_entry(Domain,Type,_,_,_),arg(1,Arg,I)) ->
	    ((b5st_class_entry(ArgName,Type,I,_), DA = Domain^cls) ->
		Triple = key(I)-Domain-name(ArgName)
	    ;
	    ((b5par_key_exists_p(class,Domain,I), DA = conc) ->
		Triple = key(I)-Domain-term
	    ;
	    Triple = wrong_argument(existence(Arg,DA)))))	    
	;
	((integer(Arg) ->
	    (DA = number^obj ->
		Triple = num(Arg)-number-obj
	    ;
	    Triple = wrong_argument(wrong_number(Arg)))
	;
	(atom(Arg) ->
	    ( ground(DA) ->
		((DA = obj,b5st_object_entry(Arg,(c,0),Key,_)) ->
		    Triple = key(Key)-conc-obj
		;
		((DA = conc,b5st_class_entry(Arg,(c,0),Key,_)) ->
		    Triple = key(Key)-conc-name(Arg)
		;
		((DA = role,b5st_class_entry(Arg,(r,0),Key,_)) ->
		    Triple = key(Key)-role-name(Arg)
		;
		(DA = Domain^Sort ->
		    (b5st_domain_entry(Domain,Type,Key,_,_) ->
			(Sort = cls ->
			    (b5st_class_entry(Arg,Type,CKey,_) ->
				Triple = key(CKey)-Domain-name(Arg)
			    ;
			    Triple = wrong_argument(existence(Arg,DA)))
			;		
			(Sort = obj ->
			    (b5st_object_entry(Arg,Type,OKey,_) ->
				Triple = key(OKey)-Domain-obj
			    ;
			    Triple = wrong_argument(existence(Arg,DA)))
			;
			Triple = wrong_argument(wrong_sort(Arg,Sort))))
		    ;
		    Triple = wrong_argument(wrong_domain(Domain,Arg)))
		;
		Triple = wrong_argument(wrong_da(DA,Arg))))))
	    ;
	    findall(Type,b5st_object_entry(Arg,Type,_,_),Types),
	    (Types = [] ->
		(b5st_class_entry(Arg,ArgType,Key,_) ->
		    (ArgType = (c,0) ->
			Type = conc
		    ;
		    (ArgType = (r,0) ->
			Type = role
		    ;
		    b5st_domain_entry(Type,ArgType,_,_,_))),
		    Triple = key(Key)-Type-name(Arg)
		;
		Triple = wrong_argument(existence(Arg,DA)))
	    ;
	    (Types = [ArgType] ->
		(b5st_class_entry(Arg,_,_,_) ->
		    Triple = wrong_argument(ambigue(Arg))
		;
		b5st_object_entry(Arg,ArgType,Key,_),
		(ArgType = (c,0) ->
		    Type = conc
		;
		b5st_domain_entry(Type,ArgType,_,_,_)),
		Triple = key(Key)-Type-obj)
	    ;
	    Triple = wrong_argument(ambigue(Arg))))))
	;
	(Arg = theknown(Term) ->
	    (DA = obj ->
		((b5par_dnf_term(Term,obj,conc),
		  Triple = theknown(Term)-conc-obj),!
	      ;
		  Triple = wrong_argument(theknown(Term)))
	      ;
	      Triple = wrong_argument(existence(getall(Term))))
	  ;
	  fail))))))).



b5par_da3(and(Arg1,Arg2),DA,Triple) :-
	!,b5par_da1(Arg1,DA,Triple1),
	(Triple1 = wrong_argument(ambigue(_)) ->
	    b5par_da1(Arg2,DA,Triple2),
	    (Triple2 = wrong_argument(Exp) ->
		Triple = wrong_argument(Exp)
	    ;
	    b5par_da1(Arg1,DA,Triple1),
	    (Triple1 = wrong_argument(Exp) ->
		Triple = wrong_argument(Exp)
	    ;
	    ((DA = conc ; DA = role) ->
		Triple = compute(and(Arg1,Arg2))-DA-term
	    ;
	    Triple = wrong_argument(wrong_term(and(Arg1,Arg2))))))
	;
	(Triple1 = wrong_argument(Exp) ->
	    Triple = wrong_argument(Exp)
	;
	b5par_da1(Arg2,DA,Triple2),
	(Triple2 = wrong_argument(Exp) ->
	    Triple = wrong_argument(Exp)
	;
	((DA = conc ; DA = role) ->
	    Triple = compute(and(Arg1,Arg2))-DA-term
	;
	Triple = wrong_argument(wrong_term(and(Arg1,Arg2))))))).
b5par_da3(intersection(Arg1,Arg2),Type^Sort,Triple) :-
	!,b5par_da1(Arg1,Type^Sort,Triple1),
	(Triple1 = wrong_argument(ambigue(_)) ->
	    b5par_da1(Arg2,Type^Sort,Triple2),
	    (Triple2 = wrong_argument(Exp) ->
		Triple = wrong_argument(Exp)
	    ;
	    b5par_da1(Arg1,Type^Sort,Triple1),
	    (Triple1 = wrong_argument(Exp) ->
		Triple = wrong_argument(Exp)
	    ;
	    (((Type = number ; Type = aset ;
		b5st_domain_entry(Type,_,_,_,_)),
		Sort \== obj) ->
		Triple = compute(intersection(Arg1,Arg2))-Type-term
	    ;
	    Triple = wrong_argument(wrong_term(intersection(Arg1,Arg2))))))
	;
	(Triple1 = wrong_argument(Exp) ->
	    Triple = wrong_argument(Exp)
	;
	b5par_da1(Arg2,Type^Sort,Triple2),
	(Triple2 = wrong_argument(Exp) ->
	    Triple = wrong_argument(Exp)
	;
	(((Type = number ; Type = aset ;
	   b5st_domain_entry(Type,_,_,_,_)),
	   Sort \== obj) ->
	        Triple = compute(intersection(Arg1,Arg2))-Type-term
	;
	Triple = wrong_argument(wrong_term(intersection(Arg1,Arg2))))))).
b5par_da3(not(Arg),DA,Triple) :-
	!,
	(b5st_class_entry(prim(Arg),Type,_,_) ->
	    ((Type == (c,0), DA = conc) ->
		Triple = compute(not(Arg))-conc-term
	    ;
	    ((Type == (r,0), DA = role) ->
		Triple = compute(not(Arg))-conc-term
	    ;
	    (Triple = wrong_argument(wrong_not_type(not(Arg))))))
	;
	(atom(Arg) ->
	    Triple = wrong_argument(existence(Arg))
	;
	Triple = wrong_argument(wrong_not_term(not(Arg))))).
b5par_da3(all(Role,Conc),conc,Triple) :-
	!,b5par_da1(Conc,DA,Triple1),
	(DA == role ->
	    Triple1 = wrong_argument(wrong_all_conc(all(Role,Conc)))
	;
	(Triple1 = wrong_argument(Exp) ->
	    Triple = wrong_argument(Exp)
	;
	b5par_da1(Role,role,Triple2),
	(Triple2 = wrong_argument(Exp) ->
	    Triple = wrong_argument(Exp)
	;
	Triple = compute(all(Role,Conc))-conc-term))).
b5par_da3(atleast(N,Role),conc,Triple) :-
	!,b5par_da1(Role,role,Triple1),
	(\+ integer(N) ->
	    Triple = wrong_argument(wrong_atleast_number(atleast(N,Role)))
	;
	(Triple1 = wrong_argument(Exp) ->
	    Triple = wrong_argument(Exp)
	;
	Triple = compute(atleast(N,Role))-conc-term)).
b5par_da3(atmost(N,Role),conc,Triple) :-
	!,b5par_da1(Role,role,Triple1),
	(\+ integer(N) ->
	    Triple = wrong_argument(wrong_atmost_number(atmost(N,Role)))
	;
	(Triple1 = wrong_argument(Exp) ->
	    Triple = wrong_argument(Exp)
	;
	Triple = compute(atmost(N,Role))-conc-term)).
b5par_da3(oneof(Objects),conc,Phase1Arg) :-
	!,
	(b5par_da4(objects(Objects),conc) ->
	    Phase1Arg = compute(oneof(Objects))-conc-term
	;
	Phase1Arg = wrong_argument(wrong_oneof(Objects))).
b5par_da3(:(Role,Fillers),conc,Triple) :-
	!,b5par_da1(Role,role,Triple1),
	(Triple1 = wrong_argument(Exp) ->
	    Triple = wrong_argument(Exp)
	;
	(\+ b5par_da4(fillers(Fillers),Role) ->
	    Triple = wrong_argument(wrong_fillers(Fillers))
	;
	Triple = compute(:(Role,Fillers))-conc-term)).
b5par_da3(domain(Conc),role,Triple) :-
	!,b5par_da1(Conc,conc,Triple1),
	(Triple1 = wrong_argument(Exp) ->
	    Triple = wrong_argument(Exp)
	;
	Triple = compute(domain(Conc))-role-term).
b5par_da3(range(Term),role,Triple) :-
	!,b5par_da1(Term,DA,Triple1),
	(Triple1 = wrong_argument(Exp) ->
	    Triple = wrong_argument(Exp)
	;
	(DA = role ->
	    Triple = wrong_argument(wrong_range(Term))
	;
	Triple = compute(range(Term))-role-term)).
b5par_da3(inv(Role),role,Triple) :-
	!,b5par_da1(Role,role,Triple1),
	(Triple1 = wrong_argument(Exp) ->
	    Triple = wrong_argument(Exp)
	;
	Triple = compute(inv(Role))-role-term).
b5par_da3(trans(Role),role,Triple) :-
	!,b5par_da1(Role,role,Triple1),
	(Triple1 = wrong_argument(Exp) ->
	    Triple = wrong_argument(Exp)
	;
	Triple = compute(trans(Role))-role-term).
b5par_da3(comp(Role1,Role2),role,Triple) :-
	!,b5par_da1(Role1,role,Triple1),
	(Triple1 = wrong_argument(Exp) ->
	    Triple = wrong_argument(Exp)
	;
	b5par_da1(Role2,role,Triple2),
	(Triple2 = wrong_argument(Exp) ->
	    Triple = wrong_argument(Exp)
	;
	Triple = compute(comp(Role1,Role2))-role-term)).
b5par_da3(.(Role1,Role2),role,Phase1Arg) :-
	!,b5par_da3(comp(Role1,Role2),role,Phase1Arg).
b5par_da3(union(Aset1,Aset2),Dom^cls,Triple) :-
	!,b5par_da1(Aset1,Dom^cls,Triple1),
	(Triple1 = wrong_argument(Exp) ->
	    Triple = wrong_argument(Exp)
	;
	b5par_da1(Aset2,Dom^cls,Triple2),
	(Triple2 = wrong_argument(Exp) ->
	    Triple = wrong_argument(Exp)
	;
	Triple = compute(union(Aset1,Aset2))-Dom-term)).
b5par_da3(without(Aset1,Aset2),Dom^cls,Triple) :-
	!,b5par_da1(Aset1,Dom^cls,Triple1),
	(Triple1 = wrong_argument(Exp) ->
	    Triple = wrong_argument(Exp)
	;
	b5par_da1(Aset2,Dom^cls,Triple2),
	(Triple2 = wrong_argument(Exp) ->
	    Triple = wrong_argument(Exp)
	;
	Triple = compute(without(Aset1,Aset2))-Dom-term)).
b5par_da3(aset(Attributes),aset^cls,Triple) :-
	!,
	(b5par_da4(attributes(Attributes),aset) ->
	    Triple = compute(aset(Attributes))-aset-term
	;
	Triple = wrong_argument(wrong_attributes(Attributes))).
b5par_da3(aset(Attributes,Domain),Domain^cls,Triple) :-
	!,
	(b5par_da4(attributes(Attributes),Domain) ->
	    Triple = compute(aset(Attributes,Domain))-Domain-term
	;
	Triple = wrong_argument(wrong_attributes(Attributes,Domain))).
b5par_da3(..(Number1,Number2),number^cls,Triple) :-
	!,
	(integer(Number1) ->
	    (integer(Number2) ->
		Triple = compute(..(Number1,Number2))-number-term
	    ;
	    Triple = wrong_argument(wrong_number(Number2)))
	;
	Triple = wrong_argument(wrong_number(Number1))).
b5par_da3(lt(Number),number^cls,Triple) :-
	!,(integer(Number) ->
	    Triple = compute(lt(Number))-number-term
	;
	Triple = wrong_argument(wrong_number(Number))).
b5par_da3(gt(Number),number^cls,Triple) :-
	!,(integer(Number) ->
	    Triple = compute(gt(Number))-number-term
	;
	Triple = wrong_argument(wrong_number(Number))).
b5par_da3(ge(Number),number^cls,Triple) :-
	!,(integer(Number) ->
	    Triple = compute(ge(Number))-number-term
	;
	Triple = wrong_argument(wrong_number(Number))).
b5par_da3(le(Number),number^cls,Triple) :-
	!,(integer(Number) ->
	    Triple = compute(le(Number))-number-term
	;
	Triple = wrong_argument(wrong_number(Number))).



b5par_da4(attributes(L),Domain) :-
	b5st_domain_entry(Domain,Type,_,_,_),
	b5par_disamb_args_loop(L,Type).
b5par_da4(objects(L),conc) :-
	b5par_disamb_args_loop(L,(c,0)).
b5par_da4(fillers(Term),Role) :-
	b5par_determine_role_range(Role,Range),
	b5par_disamb_fillers(Term,Range).


b5par_disamb_fillers(close(Term),Range) :-
	!,b5par_disamb_fillers(Term,Range).
b5par_disamb_fillers(theknown(Term),(c,0)) :-
	!,b5par_dnf_term(Term,conc,obj).
b5par_disamb_fillers(allknown(Term),(c,0)) :-
	!,b5par_da3(Term,conc,Phase1Arg,_),
	Phase1Arg \== wrong_argument.
b5par_disamb_fillers(Filler1 and Filler2,Type) :-
	!,b5par_disamb_fillers(Filler1,Type),
	b5par_disamb_fillers(Filler2,Type).
b5par_disamb_fillers(Filler,Type) :-
	(b5st_object_entry(Filler,Type,_,_),!
    ;
	b5par_uc(Filler,I),
	b5st_object_entry(uc(I),Type,_,_)).

b5par_disamb_args_loop([],_) :- !.
b5par_disamb_args_loop([Arg1|R],Type) :-
	b5st_object_entry(Arg1,Type,_,_),
	b5par_disamb_args_loop(R,Type).


% SYNTAX CHECK OF GETALL EXPRESSIONS

b5par_dnf_term(T1 or T2,DA,Type) :-
	!,
	b5par_dnf_term(T1,DA,Type),
	b5par_dnf_term(T2,DA,Type).
b5par_dnf_term(Term,DA,Type) :-
	b5par_getall(DA,Term,Type).


b5par_getall(DA,Term,Type) :-
	var(DA),!,
	b5par_aa_parse(Term,TermType,_,_,_,_),
	(TermType == (c,0) ->
	    Type = conc,
	    DA = obj
	;
	(TermType == (s,0) ->
	    DA = string^obj,
	    Type = string
	;
	(var(TermType) ->
	    fail
	;
	(TermType = (a,N) ->
	    b5st_domain_entry(Type,(a,N),_,_,_)),
	    DA = Type^obj))).
b5par_getall(obj,Term,conc) :-
	!,b5par_aa_parse(Term,(c,0),_,_,_,_).
b5par_getall(Domain,Term,Domain) :-
	b5st_domain_entry_dt(Domain,Type,_,_,_),
	b5par_aa_parse(Term,Type,_,_,_,_).



b5par_key_exists_p(obj,conc,Key) :-
	b5kif_key_exists_p(obj,Key).
b5par_key_exists_p(class,conc,Key) :-
	!,b5kif_key_exists_p((c,0),Key).
b5par_key_exists_p(class,role,Key) :-
	!,b5kif_key_exists_p((r,0),Key).
b5par_key_exists_p(class,Domain,Key) :-
	b5st_domain_entry(Domain,Type,_,_,_),
	b5kif_key_exists_p(Type,Key).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% VALID ARGUMENT/ACTION COMBINATIONS
%
% After an argument has been disambiguated, argument/action-pairs have to
% be tested. Here's where every correct argument receives it's copy of
% the TR-ER tuple. The predicate returns wrong arguments unchanged, i.e.
% as wrong_argument(Explanation), except for ambigue names. All actions
% on ambigue names are marked 'illegal_action(Explanation)', except for
% 'introduced_as'. For all other arguments all Varaibles in their ER are
% treated according to the specifications.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



b5par_combinations(wrong_argument(ambigue(Name)),TM,ER,Name-ambigue-ambigue-ATM-AER) :-
	atom(Name),
	member(perform(introduced_as,_),TM),!,
	copy_term(TM-ER,NTM-AER),
	b5par_produce_ambigue_result(NTM,ATM).
	
b5par_combinations(wrong_argument(Exp),_,_,wrong_argument(Exp)) :- !.

b5par_combinations(Arg,TM,ER,Arg-ATM-AER) :-
	copy_term(TM-ER,NTM-AER),
	b5par_combinations1(NTM,ATM,Arg).


b5par_combinations1([],[],_) :- !.
b5par_combinations1([perform(NAction1,Var)|NTM],[AAction1|ATM],Arg) :-
	b5par_combination(NAction1,Var,AAction1,Arg),
	b5par_combinations1(NTM,ATM,Arg).
    

b5par_combination(describe,Var,perform(describe,Var),_) :- !.
b5par_combination(describe_fully,Var,perform(describe_fully,Var),_) :- !.
b5par_combination(introduced_as,Var,Action,Arg-Type-Sort) :-
	(Sort == term ->
	    (b5sta_check_flag(retrieval,fail) ->
		t5out_error(illegal_action(Arg-Type-Sort,intro)),fail
	    ;
	    Action = perform(skip,_),
	    Var = illegal_action(term))
	;
	Action = perform(introduced_as,Var)).
b5par_combination(defined_as,Var,Action,Arg-Type-Sort) :-
	    (Sort == term ->
		(b5sta_check_flag(retrieval,fail) ->
		    t5out_error(illegal_action(Arg-Type-Sort,def)),fail
		;
		Var = illegal_action(term),
		Action = perform(skip,_)) 
	    ;
	    (Sort = name(_) ->
		Action = perform(defined_as,Var)
	    ;
	    (Type^Sort \== conc^obj ->
		(b5sta_check_flag(retrieval,fail) ->
		    t5out_error(illegal_action(Arg-Type-Sort,def)),fail
		;
		Var = illegal_action(noconcobj),
		Action = perform(skip,_))
	    ;
	    Action = perform(defined_as,Var)))).
b5par_combination(self,Var,perform(self,Var),_) :- !.
b5par_combination(msc,Var,Action,Arg-Type-Sort) :-
	    (Sort == term ->
		(b5sta_check_flag(retrieval,fail) ->
		    t5out_error(illegal_action(Arg-Type-Sort,msc)),fail
		;
		Action = perform(skip,_),
		Var = illegal_action(term))
	    ;
	    (Sort = name(_) ->
		(b5sta_check_flag(retrieval,fail) ->
		    t5out_error(illegal_action(Arg-Type-Sort,msc)),fail
		;
		Var = illegal_action(name),
		Action = perform(skip,_))
	    ;
	    Action = perform(msc,Var))).
b5par_combination(vr(Role),Var,Action,Arg-Type-Sort) :-
	    (Type \== conc ->
		(b5sta_check_flag(retrieval,fail) ->
		    t5out_error(illegal_action(Arg-Type-Sort,vr)),fail
		;
		Var = illegal_action(noconc),
		Action = perform(skip,_))
	    ;
	    Action = perform(vr(Role),Var)).
b5par_combination(rf(Role),Var,Action,Arg-Type-Sort) :-
	    (Type \== conc ->
		(b5sta_check_flag(retrieval,fail) ->
		    t5out_error(illegal_action(Arg-Type-Sort,rf)),fail
		;
		Var = illegal_action(noconc),
		Action = perform(skip,_))
	    ;
	    Action = perform(rf(Role),Var)).
b5par_combination(nr(Role),Var,Action,Arg-Type-Sort) :-
	    (Type \== conc ->
		(b5sta_check_flag(retrieval,fail) ->
		    t5out_error(illegal_action(Arg-Type-Sort,nr)),fail
		;
		Var = illegal_action(noconc),
		Action = perform(skip,_))
	    ;
	    Action = perform(nr(Role),Var)).
b5par_combination(diff,Var,Action,Arg-Type-Sort) :-
	(Type = obj ->
	    (Sort \== conc ->
		(b5sta_check_flag(retrieval,fail) ->
		    t5out_error(illegal_action(Arg-Type-Sort,diff)),
		    fail
		;
		Var = illegal_action(diff),
		Action = perform(skip,_))
	    ;
	    Action = perform(diff,Var))
	;
	Action = perform(diff,Var)).

b5par_produce_ambigue_result([],[]) :- !.
b5par_produce_ambigue_result([perform(introduced_as,Var)|R],[perform(introduced_as,Var)|L]) :- 
	!,b5par_produce_ambigue_result(R,L).
b5par_produce_ambigue_result([perform(_,illegal_action(ambigue))|R],[perform(skip,_)|L]) :-
	b5par_produce_ambigue_result(R,L).



b5par_diff_combi_test([_-_-_-[perform(skip,_)]-_],_) :- 
	!,t5out_error(wrong_difference),
	fail.
b5par_diff_combi_test(_,[_-_-_-[perform(skip,_)]-_]) :- 
	!,t5out_error(wrong_difference),
	fail.
b5par_diff_combi_test([_-_-obj-_-_],L) :-
	!,
	(L = [_-_-obj-_-_] ->
	    true
	;
	t5out_error(wrong_difference),
	fail).
b5par_diff_combi_test(L,[_-_-obj-_-_]) :-
	!,
	( L = [_-_-obj-_-_] ->
	    true
	;
	t5out_error(wrong_difference),
	fail).
b5par_diff_combi_test([_-Type-_-_-_],[_-Type-_-_-_]) :- !.
b5par_diff_combi_test(_,_) :-	
	!,t5out_error(wrong_difference),
	fail.
	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% PHASE II: COMPUTATION OF ARGUMENTS' KEYS
%
% Now that everything detecable by the parser has been checked, it's time
% to do the computations necessary. Terms will be transformed into keys and
% theknown and getall expressions will be evaluated. All other elements
% of the list can remain unchanged.
% ER's for getall(Term) are replaced by n ER's in ERL, n being the number of
% instances of Term. If anything goes wrong with the computations, the ER
% will be replaced by 'wrong_argument(kernel_error(Explanation))'.
% Otherwise, every element of Phase1List will be transformed to either
% key(Key)-B-C or num(N)-number-obj (for B and C see above).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b5par_transform_phase_two(Phase1List,NFX,Phase2List) :-
	b5par_transform_phase_two(Phase1List,NFX,[],Phase2List).


b5par_transform_phase_two([],_,Phase2List,Phase2List) :- !.
b5par_transform_phase_two([Arg1|R],NFX,Akk1,Phase2List) :-
	b5par_transform_single_arg(Arg1,NFX,Akk2,Phase2List),
	b5par_transform_phase_two(R,NFX,Akk1,Akk2).
b5par_transform_phase_two(no_valid_action,_,[],no_valid_action).


b5par_transform_single_arg(_-_-_-_-wrong_difference,_,_,_-_-_-_-wrong_difference) :- !.
b5par_transform_single_arg(Arg-ambigue-ambigue-TM-ER,_,Akk,[Arg-ambigue-ambigue-TM-ER|Akk]) :- !.
b5par_transform_single_arg(num(N)-number-obj-TM-ER,_,Akk,[num(N)-number-obj-TM-ER|Akk]) :- !.
b5par_transform_single_arg(key(N)-Type-Sort-TM-ER,_,Akk,[key(N)-Type-Sort-TM-ER|Akk]) :- !.
b5par_transform_single_arg(wrong_argument(Exp),_,Akk,[wrong_argument(Exp)|Akk]) :- !.
b5par_transform_single_arg(theknown(Concept)-conc-obj-TM-ER,_,Akk,Phase2List) :-
	!,
	(b5par_obj_name2key(theknown(Concept),(c,0),Key) ->
	    Phase2List = [key(Key)-conc-obj-TM-ER|Akk]
	;
	(b5sta_check_flag(retrieval,succeed) ->
	    Phase2List = [wrong_argument(kernel_error(theknown(Concept)))|Akk]
	;
	t5out_error(wrong_argument(kernel_error(theknown(Concept)))),!,fail)).
b5par_transform_single_arg(getall(Term)-_-_-TM-ER,NFX,Akk,[next(N)|Phase2List]) :-
	!,
	(function1(Term,NFX,Objs) ->
	    length(Objs,N),
	    b5par_add_arguments(Objs,Akk,TM-ER,Phase2List)
	;
	(b5sta_check_flag(retrieval,succeed) ->
	    Phase2List = [wrong_argument(kernel_error(getall(Term)))|Akk]
	;
	t5out_error(wrong_argument(kernel_error(getall(Term)))),!,fail)).

b5par_transform_single_arg(compute(Term)-Type-Sort-TM-ER,_,Akk,Phase2List) :-
	!,
	((Type-Sort = conc-term ->
	    b5par_transform_term_to_key((c,0),Term,Key),
	    Phase2List = [key(Key)-Type-Sort-TM-ER|Akk]
	;
	(Type-Sort = role-term ->
	    b5par_transform_term_to_key((r,0),Term,Key),
	    Phase2List = [key(Key)-Type-Sort-TM-ER|Akk]
	;
	(Type-Sort = Domain-term ->
	    b5st_domain_entry(Domain,DomType,_,_,_),
	    b5par_transform_term_to_key(DomType,Term,Key),
	    Phase2List = [key(Key)-Type-Sort-TM-ER|Akk]
	;
	fail)))
	;
	(b5sta_check_flag(retrieval,succeed) ->
	    Phase2List = [wrong_argument(kernel_error(Term))|Akk]
	;
	t5out_error(wrong_argument(kernel_error(Term))),!,fail)),!.



b5par_add_arguments([],Akk,_,Akk) :- !.
b5par_add_arguments([O-T-S|R],Akk,TM-ER,[O-T-S-OTM-OER|Phase2List]) :-
	copy_term(TM-ER,OTM-OER),
	b5par_add_arguments(R,Akk,TM-ER,Phase2List).



b5par_transform_term_to_key((c,0),and(Term1,Term2),Key) :-
	!,
	b5par_transform_term_to_key((c,0),Term1,Key1),
	b5par_transform_term_to_key((c,0),Term2,Key2),
	b5par_introduce_internal((c,0),[Key1,Key2],Key,_).
b5par_transform_term_to_key((c,0),not(Name),Key) :-
	!,
	b5st_class_entry(prim(Name),(c,0),Key1,_),
	b5par_introduce_internal((c,0),[not(Key1)],Key,_).	
b5par_transform_term_to_key((c,0),all(R,C),Key) :-
	!,
	b5par_transform_term_to_key((r,0),R,RKey),
	b5st_domain_entry(_,Type,_,_,_),     %% backtracking over domains
	Type \== (r,0),
	b5par_transform_term_to_key(Type,C,CKey),
	b5par_introduce_internal((c,0),[all(RKey,CKey)],Key,_).
b5par_transform_term_to_key((c,0),atleast(N,R),Key) :-
	!,
	b5par_transform_term_to_key((r,0),R,RKey),
	b5par_introduce_internal((c,0),[atleast(N,RKey)],Key,_).	
b5par_transform_term_to_key((c,0),atmost(N,R),Key) :-
	!,
	b5par_transform_term_to_key((r,0),R,RKey),
	b5par_introduce_internal((c,0),[atmost(N,RKey)],Key,_).	
b5par_transform_term_to_key((c,0),oneof(L),Key) :-
	!,
	b5par_transform_term_to_key(objects,L,KL),
	b5par_introduce_internal((c,0),[oneof(KL)],Key,_).
b5par_transform_term_to_key((c,0),R:F,Key) :-
	!,
	b5par_transform_term_to_key((r,0),R,RKey),
	b5par_determine_role_range(RKey,RRange),
	b5par_transform_term_to_key(fillers(RRange),F,FKL),
	b5par_introduce_internal((c,0),[fills(RKey,FKL)],Key,_).
b5par_transform_term_to_key((r,0),and(Term1,Term2),Key) :-
	!,
	b5par_transform_term_to_key((r,0),Term1,Key1),
	b5par_transform_term_to_key((r,0),Term2,Key2),
	b5par_introduce_internal((r,0),[Key1,Key2],Key,_).
b5par_transform_term_to_key((r,0),not(Name),Key) :-
	!,
	b5st_class_entry(prim(Name),(r,0),Key1,_),
	b5par_introduce_internal((r,0),[not(Key1)],Key,_).
b5par_transform_term_to_key((r,0),domain(C),Key) :-
	!,
	b5par_transform_term_to_key((c,0),C,CKey),
	b5par_introduce_internal((r,0),[domain(CKey)],Key,_).
b5par_transform_term_to_key((r,0),range(T),Key) :-
	!,
	b5st_domain_entry(_,Type,_,_,_),     %% backtracking over domains
	Type \== (r,0),
	b5par_transform_term_to_key(Type,T,TK),
	b5par_introduce_internal((r,0),[range(TK)],Key,_).
b5par_transform_term_to_key((r,0),inv(R),Key) :-
	!,
	b5par_transform_term_to_key((r,0),R,RKey),
	b5par_introduce_internal((r,0),[inv(RKey)],Key,_).	
b5par_transform_term_to_key((r,0),trans(R),Key) :-
	!,
	b5par_transform_term_to_key((r,0),R,RKey),
	b5par_introduce_internal((r,0),[trans(RKey)],Key,_).	
b5par_transform_term_to_key((r,0),R1.R2,Key) :-
	!,
	b5par_transform_term_to_key((r,0),R1,RKey1),
	b5par_transform_term_to_key((r,0),R2,RKey2),
	b5par_introduce_internal((r,0),[comp(RKey1,RKey2)],Key,_).
b5par_transform_term_to_key((r,0),comp(R1,R2),Key) :-
	!,
	b5par_transform_term_to_key((r,0),R1,RKey1),
	b5par_transform_term_to_key((r,0),R2,RKey2),
	b5par_introduce_internal((r,0),[comp(RKey1,RKey2)],Key,_).	
b5par_transform_term_to_key((r,0),role(Key),Key) :- !.
b5par_transform_term_to_key((n,0),intersection(N1,N2),Key) :-
	!,
	b5par_transform_term_to_key((n,0),N1,Key1),
	b5par_transform_term_to_key((n,0),N2,Key2),
	b5par_introduce_internal((n,0),[Key1,Key2],Key,_).
b5par_transform_term_to_key((n,0),N1 .. N2,Key) :-
	!,
	integer(N1),
	integer(N2),
	b5par_introduce_internal((n,0),[fromto(num(N1),num(N2))],Key,_).
b5par_transform_term_to_key((n,0),lt(N),Key) :-
	!,integer(N),
	b5par_introduce_internal((n,0),[lt(N)],Key,_).
b5par_transform_term_to_key((n,0),gt(N),Key) :-
	!,integer(N),
	b5par_introduce_internal((n,0),[gt(N)],Key,_).
b5par_transform_term_to_key((n,0),le(N),Key) :-
	!,integer(N),
	b5par_introduce_internal((n,0),[le(N)],Key,_).
b5par_transform_term_to_key((n,0),ge(N),Key) :-
	!,integer(N),
	b5par_introduce_internal((n,0),[ge(N)],Key,_).
b5par_transform_term_to_key((a,N),intersection(N1,N2),Key) :-
	!,
	b5par_transform_term_to_key((a,N),N1,Key1),
	b5par_transform_term_to_key((a,N),N2,Key2),
	b5par_introduce_internal((a,N),[Key1,Key2],Key,_).
b5par_transform_term_to_key((a,N),union(N1,N2),Key) :-
	!,
	b5par_transform_term_to_key((a,N),N1,Key1),
	b5par_transform_term_to_key((a,N),N2,Key2),
	b5par_introduce_internal((a,N),[union(Key1,Key2)],Key,_).
b5par_transform_term_to_key((a,N),without(N1,N2),Key) :-
	!,
	b5par_transform_term_to_key((a,N),N1,Key1),
	b5par_transform_term_to_key((a,N),N2,Key2),
	b5par_introduce_internal((a,N),[without(Key1,Key2)],Key,_).
b5par_transform_term_to_key((a,0),aset(List),Key) :-
	!,
	b5par_transform_term_to_key(attributes(0),List,KL),
	b5par_introduce_internal((a,0),[aset(KL)],Key,_).
b5par_transform_term_to_key((a,N),aset(List,Domain),Key) :-
	!,
	b5st_domain_entry(Domain,(a,N),_,_,_),
	b5par_transform_term_to_key(attributes(N),List,KL),
	b5par_introduce_internal((a,N),[aset(KL)],Key,_).
b5par_transform_term_to_key(objects,[],[]) :- !.
b5par_transform_term_to_key(objects,[O1|R],[K1|L]) :-
	!,
	b5st_object_entry(O1,(c,0),K1,_),
	b5par_transform_term_to_key(objects,R,L).
b5par_transform_term_to_key(fillers(Type),F1 and F2,FKL) :-
	!,
	b5par_transform_term_to_key(fillers(Type),F1,FKL1),
	b5par_transform_term_to_key(fillers(Type),F2,FKL2),
	append(FKL1,FKL2,FKL).
b5par_transform_term_to_key(fillers(Type),close(F),close(FKL)) :-
	!,
	b5par_transform_term_to_key(fillers(Type),F,FKL).
b5par_transform_term_to_key(fillers(Type),Filler,[Key]) :-
	!,
	b5st_object_entry(Filler,Type,Key,_).
b5par_transform_term_to_key(fillers((n,0)),Filler,[num(Filler)]) :-
	!,
	integer(Filler).
b5par_transform_term_to_key(attributes(_),[],[]) :- !.
b5par_transform_term_to_key(attributes(N),[A1|R],[K1|L]) :-
	!,
	b5st_object_entry(A1,(a,N),K1,_),
	b5par_transform_term_to_key(attributes(N),R,L).
b5par_transform_term_to_key(Type,Name,Key) :-
	b5st_class_entry(Name,Type,Key,_),!.


% EVALUATION OF GETALL EXPRESSIONS

function1(Term,NFX,ObjKeys) :-
	b5par_dnf(Term,DNF),
	function2(DNF,NFX,ObjKeys).



function2(Term1 or Term2,NFX,ObjKeys) :-
	!,function2(Term1,NFX,ObjKeys1),
	function2(Term2,NFX,ObjKeys2),
	append(ObjKeys1,ObjKeys2,ObjKeys).
function2(Term,NFX,Objs) :-
	b5par_aa_parse(Term,Type,[],FI,BQ,RQ),
	(Type == (n,0) ->
	    fail
	;
	(Type == (r,0) ->
	    fail
	;
	(Type == (s,0) ->
	    findall(Key,b5st_object_entry(_,(s,0),Key,_),Objs1),
	    b5par_add_sda(Objs1,Objs,string-obj)
	;
	(Type == (c,0) ->
	    b5par_forward_introduction(FI,old),
	    b5par_keyterm_to_protoform(BQ,Type,CPF),
	    a5ret_retrieve(CPF,RQ,NFX,Objs1),
	    b5par_add_sda(Objs1,Objs,conc-obj)
	;
	b5par_forward_introduction(FI,old),
	b5par_keyterm_to_protoform(BQ,Type,CPF),
	t5fil_filter(internal,Filter),
	b5nf_store(CPF,Filter,Key,_),
	a5ret_retrieve(Key,NFX,Objs1),
	b5st_domain_entry(Domain,Type,_,_,_),
	b5par_add_sda(Objs1,Objs,Domain-obj))))).

b5par_add_sda([],[],_) :- !.
b5par_add_sda([A|R],[key(A)-T-S|L],T-S) :-
	b5par_add_sda(R,L,T-S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Problems: 
%		- Does b5par_aa_parse parse all syntactically correct terms ?
%		* seemingly
%		- Which terms are syntactically correct ?
%		* all terms but role terms, including disjunction
%		- Can b5par_forward_introduction and b5par_keyterm_to_protoform
%		  handle all possible inputs from their predecessors ?
%		* seemingly
%		- a5ret_retrieve must be extended to handle all possible
%		  queries
%		* -> mir
%		* until now a5ret_retrieve handles queries for abstract
%		* concepts only. Queries for strings are trivial and can
%		* be answered using the symbol table. Queries for asets
%		* have to be resolved by the kernel.
%		- Which queries are possible? Which make sense ?
%		  (numbers,strings...?)
%
% getall conc -> list of known abstract objects
% getall aset -> list of known concrete objects
% getall string -> list of known strings
% getall numbers -> undefined, since number concepts are defined on ranges
%			of floats which can not be enumerated.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CALLS OF RETRIEVAL FUNCTIONS
%
% In principle, all these actions could work the same way:
%	- for each (disambiguated) argument a key is generated
%	- the kernel is called according to the action for every  key 
%	- the kernel returns a kind of keyterm
% These keyterms would then have to be transformed for output, by generating
% a term and eventually  a prettyprint. Every key of the keyterm will be 
% replaced by the corresponding name in the key's first entry found in the 
% symbol table.
%
% The scheme described above doesn't apply to all the functions, since some
% of them must be executed at parser level, operating on names rather
% than keys.
%
% Functions on names: introduced_as, defined_as, self
% Functions on keys : the rest
%
% The kernel must be able to distinguish between queries that fail due to one
% of the errors  that eventually will be detected by the parser (depending
% on the retrieval flag) and others.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% b5par_kernel_retrieval(+ Phase2List, + NFX, - Result)

b5par_kernel_retrieval([],_,[]) :- !.

b5par_kernel_retrieval([next(N)|R],NFX,[next(N)|L]) :-
	!,
	b5par_kernel_retrieval(R,NFX,L).

b5par_kernel_retrieval([wrong_argument(Expl)|R],NFX,[wrong_argument(Expl)|L]) :-
	!,
	b5par_kernel_retrieval(R,NFX,L).

b5par_kernel_retrieval([Arg-ambigue-ambigue-TM-ER|R],NFX,[ER|L]) :-
	!,b5par_retrieval_functions(TM,Arg-ambigue-ambigue,_),
	b5par_kernel_retrieval(R,NFX,L).

b5par_kernel_retrieval([key(Key)-Type-Sort-TM-ER|R],NFX,[ER|L]) :-
	!,
	b5par_retrieval_functions(TM,key(Key)-Type-Sort,NFX),
	b5par_kernel_retrieval(R,NFX,L).

b5par_kernel_retrieval([num(N)-Type-Sort-TM-ER|R],NFX,[ER|L]) :-
	!,
	b5par_retrieval_functions(TM,num(N)-Type-Sort,NFX),
	b5par_kernel_retrieval(R,NFX,L).

b5par_kernel_retrieval(no_valid_action,_,no_valid_action).


b5par_retrieval_functions([],_,_) :- !.
b5par_retrieval_functions([perform(Action1,Res)|R],Arg,NFX) :-
	b5par_call_retrieval_function(Action1,Res,Arg,NFX),
	b5par_retrieval_functions(R,Arg,NFX).


b5par_call_retrieval_function(skip,_,_,_) :- !.
b5par_call_retrieval_function(describe,Res,key(Key)-_-_,NFX) :-
	b5kif_describe(Key,NFX,Res).
b5par_call_retrieval_function(describe_fully,Res,key(Key)-_-_,NFX) :-
	b5kif_describe_fully(Key,NFX,Res).
b5par_call_retrieval_function(introduced_as,Res,Key-Type-Sort,_) :-
	b5par_introduced_as(Key-Type-Sort,Res).
b5par_call_retrieval_function(defined_as,Res,Key-Type-Sort,_) :-
	b5par_defined_as(Key-Type-Sort,Res).
b5par_call_retrieval_function(self,Res,Key-Type-Sort,_) :-
	b5par_self(Key-Type-Sort,Res).
b5par_call_retrieval_function(msc,Res,key(Key)-_-_,NFX) :-
	b5kif_msc(Key,NFX,Res1),
	b5par_postprocess(Res1,Res).
b5par_call_retrieval_function(vr(RoleKey),Res,key(Key)-_-_,NFX) :-
	b5kif_vr(Key,RoleKey,NFX,Res1),
	b5par_postprocess(Res1,Res).
b5par_call_retrieval_function(rf(RoleKey),Res,key(Key)-_-_,NFX) :-
	b5kif_fillers(Key,RoleKey,NFX,Res1),
	b5par_postprocess(Res1,Res).
b5par_call_retrieval_function(nr(RoleKey),Min-Max,key(Key)-_-_,NFX) :-
	b5kif_nr(Key,RoleKey,NFX,(Min,Max)).



b5par_postprocess([],[]) :- !.
b5par_postprocess([Res|R],[Erg|L]) :-
	!,b5par_postprocess(Res,Erg),
	b5par_postprocess(R,L).
b5par_postprocess(Arg,Name) :-
	b5kif_name_key(Name,_,Arg),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RETRIEVAL FUNCTIONS OPERATING AT NAME LEVEL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% b5par_introduced_as/2
% This function is executed at parser level
% FFS: Should Arguments to b5par_introduced_as/2  be names i.e. atoms only?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


b5par_introduced_as(num(_)-number-obj,[]-[number]) :- !.
b5par_introduced_as(key(Key)-_-obj,Class-Objects) :-
	!,
	b5st_object_entry(Name,_,Key,_),
	findall(Type,b5st_object_entry(Name,Type,_,_),OTypes),
	findall(Type,b5st_class_entry(Name,Type,_,_),CType),
	b5par_types2da(CType,Class),
	b5par_types2da(OTypes,Objects).
b5par_introduced_as(_-_-name(Name),Class-Objects) :-
	!,
	b5st_class_entry(Name,CType,_,_),
	findall(Type,b5st_object_entry(Name,Type,_,_),OTypes),
	b5par_types2da([CType],Class),
	b5par_types2da(OTypes,Objects).
b5par_introduced_as(Name-ambigue-ambigue,Class-Objects) :-
	findall(Type,b5st_class_entry(Name,Type,_,_),CType),
	findall(OType,b5st_object_entry(Name,OType,_,_),OTypes),
	b5par_types2da(CType,Class),
	b5par_types2da(OTypes,Objects).

b5par_types2da([],[]) :- !.
b5par_types2da([T|R],[D|L]) :-
	b5par_types2da(T,D),
	b5par_types2da(R,L).
b5par_types2da((c,0),conc) :- !.
b5par_types2da((r,0),role) :- !.
b5par_types2da((n,0),number) :- !.
b5par_types2da((s,0),string) :- !.
b5par_types2da((a,N),Domain) :-
	b5st_domain_entry(Domain,(a,N),_,_,_),!.



% b5par_defined_as/2
% This function is executed at parser level

b5par_defined_as(key(Key)-_-obj,Name :: Def) :-
	!,
	b5st_object_entry_dt(Name,_,Key,(ObjDef,_)),
	b5desc_list_to_term(ObjDef,Def).

b5par_defined_as(_-_-name(Name),Definition) :-
	b5st_class_entry(Name,_,_,Def),
	(Def == forward ->
	    Definition = (Name :< anything)
	;
	Definition = Def).


b5desc_list_to_term([Part], Part) :- !.
b5desc_list_to_term([Part1| List], Part1 and Part2) :-
	!, b5desc_list_to_term(List, Part2).

b5desc_term_to_list(Part1 and Part2, [Part1| List]) :-
	!, b5desc_term_to_list(Part2, List).
b5desc_term_to_list(Part, [Part]) :- !.


% b5par_self/2
% This function is executed at parser level

b5par_self(key(Key)-_-term,Name) :-
	b5st_class_entry_dt(Name,_,Key,_),!.
b5par_self(key(Key)-Type-term,Term) :-
	!,functor(Term,Type,1),
	arg(1,Term,Key).
b5par_self(_-_-name(Name),Name) :- !.
b5par_self(num(N)-number-obj,N) :- !.
b5par_self(key(Key)-_-obj,Name) :-
	b5st_object_entry_dt(Name,_,Key,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% POSTPROCESSING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%b5par_retrieval_postprocess(_,L1-L2,_) :-
%	writeq(L1),nl,
%	writeq(L2),nl.

b5par_retrieval_postprocess([A|L]/_,R,Actions) :-
	!,b5par_retrieval_postprocess1(Actions,[A|L],R).
b5par_retrieval_postprocess([A|L],R,Actions) :-
	!,b5par_retrieval_postprocess1(Actions,[A|L],R).
b5par_retrieval_postprocess(A/_,R,Actions) :-
	!,b5par_retrieval_postprocess1(Actions,[A],R).
b5par_retrieval_postprocess(A,R,Actions) :-
	b5par_retrieval_postprocess1(Actions,[A],R).

b5par_retrieval_postprocess1([self],[getall(_)],[next(_)|Results]) :-
	!,cmk(Results,_,Display),
	writeq(Display),nl.
b5par_retrieval_postprocess1([self],Arguments,Results) :-
	\+ member(getall(_),Arguments),
	!,cmk(Results,Arguments,Display),
	writeq(Display),nl.
b5par_retrieval_postprocess1(Actions,Arguments,Results) :-
	b5par_retrieval_postprocess2(Arguments,Results,Actions).

cmk([],[],[]).
cmk([wrong_argument(_)|L],[A1|R],[wrong_argument:A1|Akk]) :-
	!,cmk(L,R,Akk).
cmk([[R1]|L],[_|R],[R1|Akk]) :-
	cmk(L,R,Akk).


b5par_retrieval_postprocess2([],[],_) :- !.
b5par_retrieval_postprocess2([A1|Arguments],[R1|Results],Actions) :-
	b5par_retrieval_postprocess(R1,A1,Results,RestResults,Actions),
	b5par_retrieval_postprocess2(Arguments,RestResults,Actions).

b5par_retrieval_postprocess(wrong_argument(_),Arg,Res,Res,_) :-
	!,b5par_display_argument(Arg),
	tab(2),write('Error. This is no valid argument.'),nl.
b5par_retrieval_postprocess(next(N),getall(Term),Results,RestResults,Actions) :-
	!,b5par_display_getall(N,1,Term,Results,RestResults,Actions).
b5par_retrieval_postprocess(R1,A1,Results,Results,Actions) :-
	b5par_display_argument(A1),
	b5par_display_action_results(Actions,R1).

b5par_display_getall(0,_,_,R,R,_) :- !.
b5par_display_getall(N,I,Term,[R1|R],Rest,Actions) :-
	b5par_retrieval_postprocess(instance(I,Term),[R1],Actions),
	J is I+1,
	M is N-1,
	b5par_display_getall(M,J,Term,R,Rest,Actions).
    
b5par_display_argument(instance(I,Term)) :-
	!,nl,write('Instance #'),writeq(I),
	write(' of getall('),writeq(Term),
	write(')'),nl.
b5par_display_argument(A1) :-
	nl,write('>>> '),writeq(A1),nl.

b5par_display_action_results([],[]) :- !.
b5par_display_action_results([A1|Actions],[R1|Results]) :-
	b5par_display_action_results1(R1,A1),
	b5par_display_action_results(Actions,Results).


b5par_display_action_results1(wrong_generator(illegal(Generator)),_) :-
	!,b5par_display_action_results1('Error. This is no legal Action.',Generator).
b5par_display_action_results1(wrong_generator(variable),_) :-
	!,b5par_display_action_results1('Error. The action is a variable.',_).
b5par_display_action_results1(wrong_generator(vr(Role)),_) :-
	!,b5par_display_action_results1('Error. The specified role is not correct.',vr(Role)).
b5par_display_action_results1(wrong_generator(nr(Role)),_) :-
	!,b5par_display_action_results1('Error. The specified role is not correct.',nr(Role)).
b5par_display_action_results1(wrong_generator(rf(Role)),_) :-
	!,b5par_display_action_results1('Error. The specified role is not correct.',rf(Role)).
b5par_display_action_results1(illegal_action(_),A1) :-
	!,b5par_display_action_results1('Error. This action can not be performed on the argument.',A1).
b5par_display_action_results1(desc(Result),Action) :-
	!,tab(2),
	writeq(Action),
	b5par_position_result(Action,Pos),
	Pos,
	b5desc_write(desc(Result)),nl.
b5par_display_action_results1(Result,Action) :-
	tab(2),
	writeq(Action),
	b5par_position_result(Action,Pos),
	Pos,
	writeq(Result),nl.

b5par_position_result(Action,Pos) :-
	print_length(writeq(Action),L),
	(L > 7 ->
	    Pos = (write(:),nl,tab(10))
	;
	N is 7-L,
	Pos = (write(:),tab(N))).


b5par_retrieval_postprocess(L1-L2,L1-L2) :- !.

b5par_retrieval_postprocess([],[]) :- !.
b5par_retrieval_postprocess([R1|Result],FinalResult) :-
	b5par_postprocess_result(R1,LoopResult,FinalResult),
	b5par_retrieval_postprocess(Result,LoopResult).

b5par_postprocess_result(next(_),Res,Res) :- !.
b5par_postprocess_result(R,Res,[R|Res]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% messages
%
%t5out_error_output(no_valid_generator(Generator)) :-
%	t5out_write_error,
%	write(Generator),
%	write(' is not a valid generator.'),
%	!,nl.
%
%t5out_error_output(wrong_name(Name)) :-
%	t5out_write_error,
%	write(Name),
%	write(' is not a valid name.'),
%	nl,!.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MISC

% b5desc_objs_postprocess is used in t5out.pl

b5desc_objs_postprocess(X,Name) :-
        (  integer(X) ->
	       b5st_object_entry(Name,_,X,_)
        ;  X = ID/obj ->
	       b5st_object_entry(Name,_,ID,_)),!.
b5desc_objs_postprocess([], []) :- !. 
b5desc_objs_postprocess([X|Objs], [X|ProcObjs]) :-
	var(X),
	!, %% RED %%
	b5desc_objs_postprocess(Objs, ProcObjs).
b5desc_objs_postprocess([X|Objs], [ProcX|ProcObjs]) :-
        is_list(X),
        !,
        b5desc_objs_postprocess(X, ProcX), !,
	b5desc_objs_postprocess(Objs, ProcObjs), !.
b5desc_objs_postprocess([Obj| Objs], [ObjName| ProcObjs]) :-
        ( integer(Obj) ->
	      b5st_object_entry(ObjName, _, Obj, _)
        ; Obj = ID/obj ->
	      b5st_object_entry(ObjName, _, ID, _)
	; ObjName = Obj ),
	!, b5desc_objs_postprocess(Objs, ProcObjs).
 

b5par_difference([key(Key1)-_-_-[perform(diff,Term1)]-_],[key(Key2)-_-_-[perform(diff,Term2)]-_],NFX,Term1-Term2) :-
	b5kif_difference(Key1,Key2,NFX,Term1,Term2).

b5desc_difference(conc,Key1,Key2,SEL,Z1,Z2) :-
	b5kif_nfx_map(SEL,Key1,NKey1),
	b5kif_nfx_map(SEL,Key2,NKey2),
	t5concid_normal_form(NKey1,NF1),
	t5concid_normal_form(NKey2,NF2),
	b5desc_nf_diff(conc,Key1,NF2,NF1,Z1),
	b5desc_nf_diff(conc,Key2,NF1,NF2,Z2).

b5desc_difference(obj,Key1,Key2,SEL,Z1,Z2) :-
	a5objid_nf_x(SEL,Key1,_,NF1),
	a5objid_nf_x(SEL,Key2,_,NF2),
	b5desc_nf_diff(obj,Key1,NF2,NF1,Z1),
	b5desc_nf_diff(obj,Key2,NF1,NF2,Z2).

b5desc_nf_diff(Wrap,Key,NF2,NF1,Z1) :- 
	b5nf_diff(NF2,NF1,ZZ1),
	b5nf_type(NF2,Type2),
	b5nf_describe_type(Type2,ZZ1,Key/*?*/,min,[],O1),
	b5desc_empty_fill(O1,NF2,OO1),
	W =.. [Wrap,OO1],
	b5desc_map(W,Z1).

b5desc_empty_fill([],NF,[H]) :-
	!,
	b5nf_domain(NF,H).

b5desc_empty_fill(X,_,X).

%-----------------------------------------------------------------------
% this one is for description
% buggy, sorry ...

b5desc_entity(conc,SEL,L,Key,Z) :-
	b5kif_nfx_map(SEL,Key,NKey),
	t5concid_describe(NKey,L,[],[D]),
	b5desc_map(D,Z).

b5desc_entity(role,_Sel,L,Key,Z) :-
	t5role_describe(Key,L,[],[D]),
	b5desc_map(D,Z).

b5desc_entity(obj,Sel,L,Key,Z) :-
	a5objid_describe(Sel,Key,L,[],[D]),
	b5desc_map(D,Z).

b5desc_entity(inst,_Sel,L,Key,Z) :-
	b5inst_describe(Key,L,[],[D]),
	b5desc_map(D,Z).

%-----------------------------------------------------------------------

b5desc_desc_entity(conc(List),X)  :-  
	b5desc_desc_conc(List,X).

b5desc_desc_entity(role(List,T),Res)  :-  
	b5desc_desc_role(List,X),
	(var(T) -> Res =X;Res= type(X,T)).

% extension wird zur zur beschreibung intension 
b5desc_desc_entity(value(List),X) :-  
	b5desc_desc_conc(List,X).

b5desc_desc_entity(obj(List),X) :-    
	b5desc_desc_conc(List,X).

%-----------------------------------------------------------------------

b5desc_desc_conc([X],NX) :- 
	!,
	b5desc_desc_conc_atom(X,NX).

b5desc_desc_conc([X|Xs],and(NX,NXs)) :-	 
	b5desc_desc_conc_atom(X,NX),
	b5desc_desc_conc(Xs,NXs).

%-----------------------------------------------------------------------

b5desc_desc_role([X],NX) :- 
	!,
	b5desc_desc_role_atom(X,NX).

b5desc_desc_role([X|Xs],and(NX,NXs)) :- 
	b5desc_desc_role_atom(X,NX),
	b5desc_desc_role(Xs,NXs).

b5desc_desc_conc_atom(primc(Key),Name) :-
	b5kif_name_key(Name,_,Key),!.

%-----------------------------------------------------------------------
b5desc_desc_conc_atom(prim(Key),Name)     :- 
	b5kif_name_key(Name,_,Key),!.

b5desc_desc_conc_atom(not(Key),not(Name)) :- 
	b5kif_name_key(prim(Name),_,Key),!.

b5desc_desc_conc_atom(all(R,C),all(RR,CC)) :- 
	b5desc_desc_entity(R,RR),
	b5desc_desc_entity(C,CC),!.

b5desc_desc_conc_atom(atleast(N,ROLE),atleast(N,RR)) :- 
	b5desc_desc_entity(ROLE,RR),!.

b5desc_desc_conc_atom(atmost(N,ROLE),atmost(N,RR)) :- 
	b5desc_desc_entity(ROLE,RR),!.

b5desc_desc_conc_atom(attribute_domain(open,_Keys),attribute_domain) :- !.

b5desc_desc_conc_atom(attribute_domain(close,Keys),attribute_domain(Y)) :- 
	b5kif_names_keys(Y,_,Keys),!.

b5desc_desc_conc_atom(aset(ValueKeys,DomainKey),aset(VX)) :- 
	b5kif_name_key(aset,_,DomainKey),!,
	b5kif_names_keys(VXX,_,ValueKeys),
	reverse(VXX,VX).

b5desc_desc_conc_atom(aset(ValueKeys,DomainKey),aset(VX,DX)) :- 
	b5kif_names_keys(VXX,_,ValueKeys),
	reverse(VXX,VX),
	b5kif_name_key(DX,_,DomainKey),!.

b5desc_desc_conc_atom(fillers(ROLE,ObjKeys),:(RD,Objs)) :-
	b5desc_desc_entity(ROLE,RD),
	reverse(ObjKeys,Rev),
	b5desc_andlist(Rev,Objs),!.

b5desc_desc_conc_atom(rvms_eq(Roles),X) :-
	b5desc_rvms(Roles,X),!. 

b5desc_desc_conc_atom(oneof(O),oneof(ROO)) :-  
	b5kif_names_keys(OO,_,O),!,
	reverse(OO,ROO).

b5desc_desc_conc_atom(ge(X),ge(X)) :- !.

b5desc_desc_conc_atom(gt(X),gt(X)) :- !.

b5desc_desc_conc_atom(lt(X),lt(X)) :- !.

b5desc_desc_conc_atom(le(X),le(X)) :- !.

b5desc_desc_conc_atom(num(X),X) :- !.
% das concept (intension)

b5desc_desc_conc_atom(Key,Name) :- 
	integer(Key),
	b5kif_name_key(Name,_,Key),!.

b5st_primnoprim(PrimKey,Key) :-
	b5st_class_entry(prim(Name),_,PrimKey,_),!,
	b5st_class_entry(Name,_,Key,_).

%-----------------------------------------------------------------------

b5desc_ands([X],X) :- !. 
b5desc_ands([X,Y],XX and YY) :- 
	!,b5desc_ands(X,XX),b5desc_ands(Y,YY).
b5desc_ands([X,Y|Xs],XY and ZZ) :-
	%Xs \== [],
	b5desc_ands([X,Y],XY),
	b5desc_ands(Xs,ZZ).

b5desc_rvms([Role],RD) :- 
	!,
	b5desc_desc_entity(Role,RD).

b5desc_rvms([X,Y],rvm_equal(XX,YY)) :- 
	!,
	b5desc_rvms([X],XX),
	b5desc_rvms([Y],YY).

b5desc_rvms([X,Y|Xs],and(XY,ZZ)) :-
	%Xs \== [],
	b5desc_rvms([X,Y],XY),
	b5desc_rvms(Xs,ZZ).

%-----------------------------------------------------------------------


b5desc_andlist([X],XN) :- 
	!,
	b5kif_name_key(XN,_,X),!.
	
b5desc_andlist([X,Y],XN and YN) :- 
	!,
	b5kif_name_key(XN,_,X),
	b5kif_name_key(YN,_,Y),!.

b5desc_andlist([X,Y|Xs],XN and YN and ZN) :-
	b5kif_name_key(XN,_,X),
	b5kif_name_key(YN,_,Y),
	b5desc_andlist(Xs,ZN),!.


%-----------------------------------------------------------------------

b5desc_map(Y,Z) :- b5desc_desc_entity(Y,YY),Z=desc(YY).
/*
b5desc_pretty_print(and(A,B)) :- 
	tab(3),
	b5desc_pretty_print(A),nl,
	write(and),
	b5desc_pretty_print_sec(B).

b5desc_pretty_print(A) :- 
	tab(5),
	writeq(A).

b5desc_pretty_print_sec(and(A,B)) :- 
	b5desc_pretty_print(A),
	nl,
	write(and),
	b5desc_pretty_print_sec(B).

b5desc_pretty_print_sec(A) :- 
	tab(5),
	writeq(A).
*/

b5desc_pretty_print(and(A,B),X) :- 
	!,
	tab(3),% length(and)
	tab(X),
	b5desc_pretty_print(A,X),nl,
	tab(X),
	write(and),
	b5desc_pretty_print_sec(B,X).

b5desc_pretty_print(A,_X) :- 
	tab(2),
	write(A).

b5desc_pretty_print_sec(and(A,B),X) :- 
	!,
	%tab(2),
	b5desc_pretty_print(A,X),
	nl,
	tab(X),
	write(and),
	b5desc_pretty_print_sec(B,X).

b5desc_pretty_print_sec(A,_X) :- 
	tab(2),
	write(A).

b5desc_write(desc(X)) :-
	X = type(A,B),!,
	nl,
	(A =and(_,_) -> true;tab(10),tab(3)),
	b5desc_pretty_print(A,10),!,
	nl,tab(10),tab(3),tab(2),write('type '),write(B),nl.

b5desc_write(desc(X)) :-
	nl,
	(X =and(_,_) -> true;tab(10),tab(3)),
	b5desc_pretty_print(X,10),!,
	nl.

	

/*
d(X) :-
	b5kif_name_key(X,_,K),
	b5kif_module(K,Mod),
	b5desc_entity(Mod,mid,K,Y),
	b5desc_write(Y),nl.

dm(X) :-
	b5kif_name_key(X,_,K),
	b5kif_module(K,Mod),
	b5desc_entity(Mod,min,K,Y),
	b5desc_write(Y),nl.

df(X) :-
	b5kif_name_key(X,_,K),
	b5kif_module(K,Mod),
	b5desc_entity(Mod,max,K,Y),
	b5desc_write(Y),nl.

writeh(Y) :- b5desc_desc_entity(Y,YY),Z=desc(YY),b5desc_write(Z),nl.

writeg(X) :- writeg([X]),!.

writeg([D|DD]) :- write(D),write(' and'),nl,writeg(DD).
*/


b5desc_recursive :-
	retractall(b5desc_modus(_)),
	assert(b5desc_modus(recursive)).

b5desc_flat :-
	retractall(b5desc_modus(_)),
	assert(b5desc_modus(flat)).

b5desc_user :-
	retractall(b5desc_filter(_)),
	assert(b5desc_filter(named)).

b5desc_all :- 
	retractall(b5desc_filter(_)),
	assert(b5desc_filter(top)).


b5desc_init :-
	b5desc_flat,
	b5desc_user,!.


/*
b5inst_describe(Key,_,I,O) :-
	append(I,[value(Key)],O),
	b5inst_get(Key,_).
*/

b5inst_describe(Key,_L,I,O) :-
	b5inst_intension(Key,NF),
	append(I,[value(X)],O),
	b5nf_describe(NF,Key,max,[],X). % oder oder min ??

a5objid_describe(Sel,Key,Level,I,O) :-
	a5odb_get(Key,Obj,_),!,
	a5obj_describe(Sel,Obj,Level,I,O).

a5obj_describe(Sel,Obj,Level,I,O) :-
	a5obj_nf_x(Sel,Obj,NFX),
	a5obj_id(Obj,Key),
	append(I,[obj(X)],O),
	b5nf_describe(NFX,Key,Level,[],X).

/* Level e {min,mid,max} */
t5concid_describe(Key,min,I,O) :-
	!,
	b5desc_filter(Filter),
	(t5concid_filter_holds_p(Key,Filter) -> 	
		append(I,[conc([Key])],O)
	;
		t5cdb_get(Key,Conc),
		t5conc_describe(Conc,Key,min,I,O)
	).

t5concid_describe(Key,Level,I,O) :-
	!,
	t5cdb_get(Key,Conc),
	t5conc_describe(Conc,Key,Level,I,O).

t5concid_describe(Key,Source,Level,I,O) :-
	!,
	t5cdb_get(Key,Conc),
	t5conc_describe(Conc,Source,Level,I,O).


t5concid_describe(Key,Source,min,I,O) :-
	!,
	b5desc_filter(Filter),
	(t5concid_filter_holds_p(Key,Filter) -> 	
		append(I,[conc(Key)],O)
	;
		t5cdb_get(Key,Conc),
		t5conc_describe(Conc,Source,min,I,O)
	).


t5conc_describe(Conc,SourceKey,Level,I,O) :-
	t5conc_nf(Conc,NF),
	append(I,[conc(O2)],O),
	((b5nf_aset_p(NF);b5nf_number_p(NF)) -> NLevel = max; NLevel=Level),
	b5nf_describe(NF,SourceKey,NLevel,[],O2).


/* Level e {min,mid,max} */
b5nf_describe(NF,SourceKey,min,I,O) :-
	b5nf_classify(NF,Key,Status),
	(Status == old ->
		(SourceKey == Key ->
			b5nf_describe(NF,Key,mid,I,O)
		;
			t5concid_describe(Key,Key,min,I,O)
		)

	;
		b5nf_describe(NF,SourceKey,mid,I,O) 
	),!.


b5nf_describe(NF,SourceKey,_,I,O) :-
	b5nf_incoherent_p(NF),!,
	t5tbox_nothing_key(Nothing),
	t5concid_normal_form(Nothing,NN),
	b5nf_describe_type(bottom,NN,SourceKey,_,I,O).

b5nf_describe(NF,SourceKey,mid,I,O) :-
	!,
	b5nf_type(NF,Type),
	b5nf_minimize(NF,SourceKey,NF1,H),
	append(I,H,O1),
	b5nf_x_domain2(NF1,_Dom,NF2),
%	b5kif_wrapped_key(Dom,DOM),
%	append(I,[type(DOM)],NI),
%	b5nf_describe_type(Type,NF2,SourceKey,min,NI,O).
%	append(NI,[Dom],O1),   % dies nur bei bedarf machen --> parameter
	b5nf_describe_type(Type,NF2,SourceKey,min,O1,O).


b5nf_describe(NF,SourceKey,max,I,O) :- % beschreibung einer domain
	b5nf_type(NF,Type),
	b5nf_domain(NF,SourceKey),!,   %sic
	b5nf_r_prims(NF,[Prims],[],NN),
	t5dom_allprims(DomPrims),
	b5nf_r_neg_prims(NN,Old,New,NF2),
	b5sort_difference(DomPrims,[Prims],Rest),
	b5sort_unify(Old,Rest,New),
	%b5kif_wrapped_key(Dom,DOM),
	%append(I,[type(DOM)],NI),
	append(I,[primc(Prims)],OOO),
	b5nf_describe_type(Type,NF2,SourceKey,max,OOO,O).


b5nf_describe(NF,SourceKey,max,I,NO) :- % beschreibumg einer nicht domain
	b5nf_type(NF,Type),
	%b5nf_domain(NF,Dom),
	b5nf_x_domain2(NF,Dom,NF2),
	%b5kif_wrapped_key(Dom,DOM),
	%append(I,[type(DOM)],NI),
	append(I,[Dom],O),
	b5nf_r_rl(NF2,RL2,RL3,NF3),
	t5rl_minimize3(RL2,RL3),
	b5nf_describe_type(Type,NF3,SourceKey,max,O,NO).


b5st_modifier((c,0),concept) :- !.
b5st_modifier((a,_),attribute) :- !.

b5nf_x_domain2(NF,Dom,N_NF) :-
	b5nf_raw_create(T,Dom,P,NP,RL,NR,X,Num,S,NF),
	b5nf_raw_create(T,Dom,N_P,N_NP,RL,NR,X,Num,S,N_NF),
	t5dom_allprims(AllDomPrims),
	b5sort_difference(NP,AllDomPrims,N_NP),% top ????
	b5sort_difference(P,AllDomPrims,N_P). % top ????

b5nf_x_domain(NF,Dom,N_NF) :-
	b5nf_raw_create(T,Dom,P,NP,RL,NR,X,Num,S,NF),
	b5nf_raw_create(T,Dom,P,N_NP,RL,NR,X,Num,S,N_NF),
	t5dom_allprims(AllDomPrims),
	b5sort_difference(NP,AllDomPrims,N_NP). % top ????

/* level min max */
b5nf_describe_type(prim,NF,_Source,_Level,I,O) :-
/* oder nur wenn Prim = Source ??? */
	b5nf_prims(NF,[Prim]),
	append(I,[Prim],O).
	
b5nf_describe_type(top,_NF,Source,_Level,I,O) :-
	append(I,[Source],O).

b5nf_describe_type(bottom,_NF,Source,_Level,I,O) :-
	append(I,[Source],O).

b5nf_describe_type(string,NF,_Source,_Level,I,O)  :-
	b5nf_raw_create(_T,_Domain,P,NP,_RL,_NR,_X,_Num,_S,NF), 
	b5nf_describe_prims(P,I,O1),
	b5nf_describe_negprims(NP,O1,O).

b5nf_describe_type(number,NF,_Source,_Level,I,O)  :-
	b5nf_raw_create(_T,_Domain,P,NP,_RL,_NR,_X,Num,_S,NF), 
	b5nf_describe_prims(P,I,O1),
	b5nf_describe_negprims(NP,O1,O2),
	t5number_describe(Num,O2,O).

b5nf_describe_type(aset-State,NF,Source,L,I,O) :-
	b5nf_raw_create(_T,Source,P,NP,_RL,_NR,X,_Num,_S,NF), % domain
	!,
	b5nf_describe_prims(P,I,O1),
	b5nf_describe_negprims(NP,O1,O2),
	(var(X) -> O = O2
	;
		%b5nf_describe_values(X,[],O4),
		b5desc_list(X,Source,L,[],O4),
		append(O2,[attribute_domain(State,O4)],O)
	).

b5nf_describe_type(aset-_State,NF,Source,Level,I,O) :-
	b5nf_raw_create(_T,Domain,P,NP,_RL,_NR,X,_Num,_S,NF), 
	!,
	b5nf_describe_prims(P,I,O1),
	b5nf_describe_negprims(NP,O1,O2),
	(var(X) -> O = O2
	;
	%	b5nf_describe_values(X,[],O4),
		b5desc_list(X,Source,Level,[],O4),
		append(O2,[aset(O4,Domain)],O)
	).

b5nf_describe_type(norm-_Status,NF,Source,Level,I,O) :-
	b5nf_raw_create(_T,_D,P,NP,RL,NR,X,_Num,_S,NF), % domain
	b5nf_describe_prims(P,I,O1),
	b5nf_describe_negprims(NP,O1,O2),
	t5rl_describe(RL,Source,Level,O2,O3),
	b5nf_describe_card(NR,O3,O4),
	b5nf_describe_oneof(X,Source,Level,O4,O).

b5nf_describe_prims([],I,I) :- !. % cut wegen wenn var!
b5nf_describe_prims([P|Ps],I,O) :-
	append(I,[prim(P)],O1),
	b5nf_describe_prims(Ps,O1,O).

b5nf_describe_negprims([],I,I) :- !.
b5nf_describe_negprims([P|Ps],I,O) :-
	append(I,[not(P)],O1),
	b5nf_describe_negprims(Ps,O1,O).


b5nf_describe_card(_NR,O,O).


b5nf_describe_oneof([],_Source,_Level,I,I) :- !.
b5nf_describe_oneof([Obj|Objs],Source,_Level,I,O) :-
	b5desc_list([Obj|Objs],Source,min,[],OL),
	append(I,[oneof(OL)],O).

/* objecte nicht rekursiv
b5desc_list([],_,_,I,I).
b5desc_list([Obj|Objs],Source,Level,I1,O) :-
	a5objid_describe(Obj,Source,Level,I1,O1),
	b5nf_describelist(Objs,Source,Level,I1,O1,O).
*/

b5desc_list(X,_Source,_Level,I1,O) :-
	append(I1,X,O).


t5rl_describe(RL,Source,Level,I,O) :-
	t5rl_ress(RL,Ress),
	b5desc_nextlevel(Level,NextLevel),
	t5ress_describe(Ress,Source,NextLevel,I,O).

t5ress_describe([],_Source,_Level,I,I) :- !.
t5ress_describe([R-ES|Ress],Source,Level,I,O) :-
	t5res_describe(R-ES,Source,Level,I,O1),
	t5ress_describe(Ress,Source,Level,O1,O).

b5desc_nextlevel(min,min).
b5desc_nextlevel(max,max) :- b5desc_modus(recursive),!.
b5desc_nextlevel(max,min).

t5res_describe_vr(_DR,VR,_,_Level,I,I) :- var(VR),!.
t5res_describe_vr(DR,VR,_,Level,I,O) :- 
	t5concid_describe(VR,Level,[],[DVR]),
	append(I,[all(DR,DVR)],O).

t5res_describe_nr(_,NR,_,_Level,I,I) :- var(NR),!.
t5res_describe_nr(DR,NR,_,_Level,I,O) :- 
	t5nr_minmax(NR,Min,Max),
	t5res_describe_minnr(DR,Min,I,O1),
	t5res_describe_maxnr(DR,Max,O1,O).

t5res_describe_minnr(_,Min,I,I) :- var(Min),!.
t5res_describe_minnr(DR,Min,I,O) :- 
	nonvar(Min),
	append(I,[atleast(Min,DR)],O).

t5res_describe_maxnr(_,Max,I,I) :- var(Max),!.
t5res_describe_maxnr(DR,Max,I,O) :- 
	nonvar(Max),
	append(I,[atmost(Max,DR)],O).

t5res_describe_fillers(_,[],_Source,_Level,I,I) :- !.
t5res_describe_fillers(DR,Fs,Source,Level,I,O) :-
	b5desc_list(Fs,Source,Level,[],L),
	append(I,[fillers(DR,L)],O).

t5res_describe_state(un,_DCS,_Source,_NextLevel,O,O) :- !.
t5res_describe_state(_S,_DCS,_Source,_NextLevel,O,O) :- !.
		
t5res_describe_rvm(RVM,_Source,_NextLevel,O,O) :- 
	t5rvm_empty_p(RVM),!.

t5res_describe_rvm(RVM,Source,NextLevel,I,O) :- 
	t5rvm_raw_create(Equals,_Supers,_Subs,RVM),
	t5rvm_describe_eq(Equals,Source,NextLevel,[],O1),
	(O1 =[] -> O =I ; append(I,[rvms_eq(O1)],O)).

t5rvm_describe_eq([],_,_,O,O).
t5rvm_describe_eq([R|Rs],S,N,I,O) :-
	t5role_describe_without_type(R,N,I,I1),
	t5rvm_describe_eq(Rs,S,N,I1,O).


t5res_describe(R-_ES,_Source,_Level,I,I) :- var(R),!.
t5res_describe(R-ES,Source,Level,I,O) :-
	t5res_raw_create(R,VR,NR,Fs,RVM,State,DCS,R-ES),
	b5desc_nextlevel(Level,NextLevel),
	t5role_describe_without_type(R,NextLevel,[],[DR]),
	t5res_describe_vr(DR,VR,Source,NextLevel,I,O1),
	t5res_describe_nr(DR,NR,Source,NextLevel,O1,O2),
	t5res_describe_fillers(DR,Fs,Source,NextLevel,O2,O3),
	t5res_describe_rvm(RVM,Source,NextLevel,O3,O4),
	t5res_describe_state(State,DCS,Source,NextLevel,O4,O).

b5st_primkey(Key,PrimKey) :-
	b5st_class_entry(Name,_,Key,_),
	b5st_class_entry(prim(Name),_,PrimKey,_),!.

b5st_primkeys(X,Y) :- 
	b5st_primkeys(X,[],Y).

b5st_primkeys([],X,X) :- !.
b5st_primkeys([X|Xs],Y,Z) :- 
	(b5st_primkey(X,P) -> b5sort_unify(Y,[P],PP) ; PP = Y),
	b5st_primkeys(Xs,PP,Z).




t5role_typeset(notype,_,_) :- !.
t5role_typeset(type,role,_) :- !.
t5role_typeset(type,X,X) :- !.




t5role_describe(K,Level,I,O) :-
	t5role_describe(type,K,Level,I,O).


t5role_describe_without_type(R,Level,I,O) :-
	t5role_describe(notype,R,Level,I,O).


t5role_describe(TypeInfo,K,min,I,O) :-
	!,
	b5desc_filter(Filter),
	(t5role_filter_holds_p(K,Filter) ->
		t5rdb_get(K,_,Type),
		t5role_typeset(TypeInfo,Type,MType),
		append(I,[role([K],MType)],O)
	;

		t5role_inv_role(K,INV),
		(INV == notexists ->
			t5rdb_get(K,ROLE),
			x5role_describe(TypeInfo,ROLE,K,mid,I,O)
		;
			t5rdb_get(INV,_,Type),
			(t5role_filter_holds_p(INV,Filter) ->	
				t5role_typeset(TypeInfo,Type,MType),
				append(I,[role([inv(INV)],MType)],O)
			;
				t5rdb_get(K,RR),
				x5role_describe(TypeInfo,RR,K,mid,I,O)
			% oder :append(I,[role([K],MType)],O
			)
		)
	).


/* inv(Rname) gilt sowohl als minimale als auch alm mittlere beschreibung !*/
t5role_describe(TypeInfo,K,mid,I,O) :-
	!,
	b5desc_filter(Filter),
	t5role_inv_role(K,INV),
	(INV == notexists ->
			t5rdb_get(K,ROLE),
			x5role_describe(TypeInfo,ROLE,K,mid,I,O)
		;
			t5rdb_get(INV,_,Type),
			(t5role_filter_holds_p(INV,Filter) ->	
				t5role_typeset(TypeInfo,Type,MType),
				append(I,[role([inv(INV)],MType)],O)
			;
				t5rdb_get(K,ROLE),
				x5role_describe(TypeInfo,ROLE,K,mid,I,O)
			)
	).


t5role_describe(TypeInfo,K,max,I,O) :- 
	t5rdb_get(K,ROLE),
	x5role_describe(TypeInfo,ROLE,K,max,I,O).



x5role_xx(f,feature) :- !.
x5role_xx(i,inherent_feature) :- !.
x5role_xx(_,_) :- !.

x5role_describe(TypeInfo,ROLE,Source,L,I,O) :- 
	append(I,[role(X,T)],O),
	x5role_type(ROLE,Type),
	(TypeInfo = type -> x5role_xx(Type,T); true),
	x5role_describe_2(ROLE,Source,L,[],X).

x5role_describe_2(ROLE,Source,L,I,O) :-
	x5role_raw_create(Key,_Type,RNF,_Filter,_Inv,_Comps,_Transflag,Key,ROLE),
	(L = mid -> t5rnf_minimize(RNF,Source,M_RNF,DS),NL = min
	;M_RNF =RNF,DS =[],NL=L),
	append(I,DS,I2),
	t5rnf_x_domain(M_RNF,SRNF),
	t5rnf_describe(SRNF,Source,NL,I2,O).

t5rnf_x_domain(RNF,NRNF) :-
	t5rnf_raw_create(Dom,Ran,Pri,NegPri,Inv,NegInv,Tr,Co,RNF),
	t5rnf_raw_create(Dom,Ran,NPri,NNegPri,NInv,NNegInv,Tr,Co,NRNF),
	t5tbox_anyrole_key(TOPROLE),
	b5sort_difference(Pri,[TOPROLE],NPri),
	b5sort_difference(NegPri,[TOPROLE],NNegPri),
	b5sort_difference(Inv,[TOPROLE],NInv),
	b5sort_difference(NegInv,[TOPROLE],NNegInv).


t5rnf_minimize(RNF,Source,N_RNF,DS1) :-
	b5desc_filter(Filter),
	t5role_direct_supers(Source,Filter,DS), % hack i.e. t5rnf_direc..(RNF
	(
	 DS = [] -> 
		DS1 = DS,
		t5rnf_minimize2(RNF,N_RNF) 
	 ; 	
		t5role_infimum(DS,NF3),
		t5rnf_diff(RNF,NF3,N_RNF),
		t5rnf_filter(DS,DS1)
	).

t5rnf_filter([],[]).
t5rnf_filter([K|Ks],New) :-
	(t5tbox_anyrole_key(K) -> New = T;New=[K|T]),
	t5rnf_filter(Ks,T). 

t5rnf_minimize2(X,X).


t5role_infimum(Ks,RNF) :-
	t5role_new_defined_role(RPF),	
	t5role_inf(Ks,RPF,RPF2),
	t5role_rpf_rnf(RPF2,RNF).


t5role_inf([],RPF,RPF). 
t5role_inf([K|Ks],RPF,RNF) :-
	t5role_add_role(RPF,K,RPF2),
	t5role_inf(Ks,RPF2,RNF).

%first is more special
t5rnf_diff(RNF1,RNF2,RNF4) :-
	t5rnf_raw_create(Dom1,Ran1,P1,NP1,I1,NI1,Tr1,C1,RNF1),
	t5rnf_raw_create(Dom2,Ran2,P2,NP2,I2,NI2,Tr2,C2,RNF2),
	t5rnf_raw_create(Dom3,Ran3,P3,NP3,I3,NI3,Tr3,C3,RNF3),
	(Dom1 == Dom2 -> true;Dom3a = Dom1),
	(Ran1 == Ran2 -> true;Ran3a = Ran1),
	b5sort_difference(P1,P2,P3),
	b5sort_difference(NP1,NP2,NP3),
	b5sort_difference(I1,I2,I3),
	b5sort_difference(NI1,NI2,NI3),
	b5sort_difference(Tr1,Tr2,Tr3),
	t5rnf_range_domain_trans(_Tr3a,Dom3a,Ran3a,Dom3,Ran3),
	t5rnf_comp_mimimize(C1,C2,C3), 
	t5rnf_minimize2(RNF3,RNF4).

t5rnf_range_domain_trans([],Dom,Ran,Dom,Ran) :- !.
t5rnf_range_domain_trans(Tr3a,Dom3a,Ran3a,Dom3,Ran3) :-
	t5rnf_domain_trans(Tr3a,Dom3a,Dom3),
	t5rnf_range_trans(Tr3a,Ran3a,Ran3).


t5rnf_domain_trans(_,Dom,_) :-
	var(Dom),!.

t5rnf_domain_trans([T|Ts],Dom,Dom3) :-
	t5role_domain(T,Dom),!
	;
	t5rnf_domain_trans(Ts,Dom,Dom3).

t5rnf_domain_trans([],Dom,Dom).


t5rnf_range_trans(_,Ran,_) :-
	var(Ran),!.

t5rnf_range_trans([T|Ts],Ran,Ran3) :-
	t5role_range(T,Ran),!
	;
	t5rnf_range_trans(Ts,Ran,Ran3).

t5rnf_range_trans([],Ran,Ran).



t5rnf_comp_mimimize(C1,C2,C3) :- 
	(C1 = C2 -> true; C3 = C1).

t5rnf_describe(RNF,S,L,I,O) :-
	t5rnf_raw_create(Domain,Range,Prims,NegPrims,Invs,NegInvs,Transs,Comps,RNF),
	t5rnf_d_prims(Prims,S,L,I,I1),
	t5rnf_d_negprims(NegPrims,S,L,I1,I2),
	t5rnf_d_invs(Invs,S,L,I2,I3),
	t5rnf_d_neginvs(NegInvs,S,L,I3,I4),
	t5rnf_d_transs(Transs,S,L,I4,I5),
	t5rnf_d_comps(Comps,S,L,I5,I6),
	t5rnf_d_domain(Domain,S,L,I6,I7),
	t5rnf_d_range(Range,S,L,I7,O).
		
t5rnf_d_prims(Prims,_S,_L,I,I) :- var(Prims),!.
t5rnf_d_prims([],_S,_L,I,I).
t5rnf_d_prims([Prim|Prims],S,L,I,O) :- 
	append(I,[prim(Prim)],O1),
	t5rnf_d_prims(Prims,S,L,O1,O).

t5rnf_d_negprims(NegPrims,_S,_L,I,I) :- var(NegPrims),!.
t5rnf_d_negprims([],_S,_L,I,I).
t5rnf_d_negprims([NegPrim|NegPrims],S,L,I,O) :- 
	append(I,[negprim(NegPrim)],O1),
	t5rnf_d_negprims(NegPrims,S,L,O1,O).

t5rnf_d_invs(Invs,_S,_L,I,I) :- var(Invs),!.
t5rnf_d_invs([],_S,_L,I,I).
t5rnf_d_invs([Inv|Invs],S,L,I,O) :- 
	append(I,[inv(Inv)],O1),
	t5rnf_d_invs(Invs,S,L,O1,O).

t5rnf_d_neginvs(NegInvs,_S,_L,I,I) :- var(NegInvs),!.
t5rnf_d_neginvs([],_S,_L,I,I).
t5rnf_d_neginvs([NegInv|NegInvs],S,L,I,O) :- 
	append(I,[neginv(NegInv)],O1),
	t5rnf_d_neginvs(NegInvs,S,L,O1,O).

t5rnf_d_transs(Transs,_S,_L,I,I) :- var(Transs),!.
t5rnf_d_transs([],_S,_L,I,I).
t5rnf_d_transs([Trans|Transs],S,L,I,O) :- 
	b5desc_nextlevel(L,NL),
	t5role_describe(Trans,NL,[],[TR]),
	t5rnf_r_source(TR,S,NTR),
	append(I,[trans(NTR)],O1),
	t5rnf_d_transs(Transs,S,L,O1,O).

t5rnf_r_source(role(L,S),Source,role(NL,S)) :-
	(select(Source,L,NL) -> true;NL =L).

t5rnf_d_comps(Comps,_S,_L,I,I) :- var(Comps),!.
t5rnf_d_comps([],_S,_L,I,I).
t5rnf_d_comps([Comp|Comps],S,L,I,O) :- 
	b5desc_nextlevel(L,NL),
	t5rnf_d_comps2(Comp,NL,I,O1),
	t5rnf_d_comps(Comps,S,L,O1,O).

t5rnf_d_comps2([],_L,I,I).
t5rnf_d_comps2([Comp],L,I,O) :-
	!,
	t5role_describe(Comp,L,[],O1),
	append(I,O1,O).

t5rnf_d_comps2([Comp|Comps],L,I,O) :-
	t5role_describe(Comp,L,[],[O1]),
	append(I,[comp(O1,O2)],O),
	t5rnf_d_comps2(Comps,L,[],[O2]).


t5rnf_d_domain(Domain,_S,_L,I,I) :- var(Domain),!.
t5rnf_d_domain(Domain,_S,min,I,I) :- t5tbox_anything_key(Domain),!.
t5rnf_d_domain(Domain,_S,L,I,O) :- 
	b5desc_nextlevel(L,NL),
	t5concid_describe(Domain,NL,[],[CDom]),	
	append(I,[domain(CDom)],O).

t5rnf_d_range(Range,_S,_L,I,I) :- var(Range),!.
t5rnf_d_range(Range,_S,min,I,I) :- t5tbox_anything_key(Range),!.
% anything is default range
t5rnf_d_range(Range,_S,L,I,O) :- 
	b5desc_nextlevel(L,NL),
	t5concid_describe(Range,NL,[],[CRange]),	
	append(I,[range(CRange)],O).

b5desc_comps([X],X) :- !.
b5desc_comps([X,Y],XX comp YY) :-
	!,b5desc_comps(X,XX),b5desc_comps(Y,YY).
b5desc_comps([X,Y|Xs],XY comp ZZ) :-
	%Xs \== [],
	b5desc_comps([X,Y],XY),
	b5desc_comps(Xs,ZZ).


b5desc_desc_role_atom(prim(Key),Name) :-
	b5kif_name_key(Name,_,Key),!.

b5desc_desc_role_atom(not(Key),not(Name)) :-
b5kif_name_key(prim(Name),_,Key),!.

b5desc_desc_role_atom(Key,Name) :- integer(Key), b5kif_name_key(Name,_,Key),!.

b5desc_desc_role_atom(inv(Key),inv(Name)) :- 
	integer(Key),
	b5kif_name_key(Name,_,Key),!.

b5desc_desc_role_atom(inv(Role),inv(Term)) :- 
	b5desc_desc_entity(Role,Term).
	
b5desc_desc_role_atom(neginv(Key),not(inv(Name))) :- 
	integer(Key),
	b5kif_name_key(Name,_,Key),!.

b5desc_desc_role_atom(trans(Role),trans(RD)) :- 
	b5desc_desc_entity(Role,RD).

b5desc_desc_role_atom(comp(R1,R2),comp(DR1,DR2)) :- 

	(b5desc_desc_entity(R1,DR1),!; b5desc_desc_role_atom(R1,DR1)),
	(b5desc_desc_entity(R2,DR2),! ; b5desc_desc_role_atom(R2,DR2)).

b5desc_desc_role_atom(domain(K),domain(C)) :-
	b5desc_desc_entity(K,C).

b5desc_desc_role_atom(range(K),range(C)) :-
	b5desc_desc_entity(K,C).
	


b5nf_unbottom(NF1,NF2) :-
	 b5nf_raw_create(T,Dom,P,NP,RL1,NR,X,Num,S,NF1),
	 b5nf_raw_create(T,Dom,P,NP,RL2,NR,X,Num,S,NF2),
	 t5rl_unbottom(RL1,RL2).

t5rl_unbottom(RL1,RL2) :-
	t5rl_r_ress(RL1,Ress,NRess,RL2),
	b5typelist_map(Ress,NRess,t5res_unbottom).

t5res_unbottom(Res1,Res2) :-
	t5res_raw_create(R,VR1,NR1,Fs1,RVM1,_State,Res1),
	t5res_raw_create(R,VR2,NR2,Fs2,RVM1,_State,Res2),
	t5nr_unbottom(NR1,NR2),
	b5desc_unbottom(VR1,VR2),
	t5res_fillers_unbottom(Fs1,Fs2).

t5res_fillers_unbottom([],[]).
t5res_fillers_unbottom([bottom(X)|T],[X|NT]) :-
	!,
	t5res_fillers_unbottom(T,NT).

t5res_fillers_unbottom([X|T],[X|NT]) :-
	t5res_fillers_unbottom(T,NT).

b5desc_unbottom(bottom(X),X) :- !.
b5desc_unbottom(X,X).
t5nr_unbottom(-(A,B),-(AA,BB)) :-
	b5desc_unbottom(A,AA),
	b5desc_unbottom(B,BB).

b5nf_unify_substatus(X,X,X) :- !.
b5nf_unify_substatus(open,close,close).
b5nf_unify_substatus(close,open,close).

b5nf_subtract(NF1,NF2,NF3 ) :-
	b5nf_raw_create(T1,Dom,P1,NP1,RL1,NR1,_X1,Num1,S1,NF1),
	b5nf_raw_create(T2,Dom,P2,NP2,RL2,_NR2,X2,Num2,S2,NF2),
	b5nf_raw_create(T3,Dom,P3,NP3,RL3,NR1,X3,Num3,S3,NF3),

	(((T1=X-Y,T2=X-Z) ->
		b5nf_unify_substatus(Y,Z,YZ),T3 =X-YZ
		;T1=T2,T3=T2
         ),! 
	 ;T3=T1
	),

	b5sort_difference(P2,P1,P3),
	b5sort_difference(NP2,NP1,NP3),
	%b5sort_unify(X1,X2,X3), %??????
	%b5sort_difference(X1,X2,X3), %??????
	X3 = X2,
	t5number_subtract(Num1,Num2,Num3),
	t5rl_subtract(RL1,RL2,RL3),
	b5sort_difference(S2,S1,S3).

t5number_subtract(num,num,num) :- !.

/* second sollte  more special i.e. possible longer */
/* oder inco ?????????????????*/

t5rl_subtract(RL1,RL2,RL3) :-
	t5rl_raw_create(state(i,RR),Ress3,RL3), % no rvms yet
	((t5rl_rvms_p(RL1); t5rl_rvms_p(RL2)) -> RR=r;RR=e),
	t5rl_ress(RL1,Ress1),
	t5rl_ress(RL2,Ress2),
	t5ress_subtract(Ress1,Ress2,Ress3).

t5ress_subtract([],[],[]) :- !.
t5ress_subtract([],[R-ES|Tail],[Diff|NTail]) :-
	!,
	t5res_create(R,RES),
	t5res_subtract(RES,R-ES,Diff),
	t5ress_subtract([],Tail,NTail).
t5ress_subtract([R-ES|Tail],[],[Diff|NTail]) :-
	!,
	t5res_create(R,RES),
	t5res_subtract(R-ES,RES,Diff),
	t5ress_subtract([],Tail,NTail).
t5ress_subtract([H1|T1],[H2|T2],Diff) :- 
	b5typelist_order(H1,H2,Order),
	t5rl_subtract_order(Order,H1,T1,H2,T2,Diff).

t5rl_subtract_order(=,H1,T1,H2,T2,[H3|T3]) :-
	t5res_subtract(H1,H2,H3),
	t5ress_subtract(T1,T2,T3).

t5rl_subtract_order(<,R-H1,T1,H2,T2,[H3|T3]) :-
	t5res_create(R,RES),	
	t5res_subtract(R-H1,RES,H3),
	t5ress_subtract(T1,[H2|T2],T3).

t5rl_subtract_order(>,H1,T1,R-H2,T2,[H3|T3]) :-
	t5res_create(R,RES),	
	t5res_subtract(RES,R-H2,H3),
	t5ress_subtract([H1|T1],T2,T3).


% not (Res2 > Res1) --> bottom
t5res_subtract(Res1,Res2,Diff) :-
	t5res_raw_create(R,VR1,NR1,Fs1,RVM1,_State1,Res1),
	t5res_raw_create(R,VR2,NR2,Fs2,RVM2,_State2,Res2),
	t5res_raw_create(R,VR3,NR3,Fs3,RVM3,un-[],Diff),
	t5res_create(R,DRes),
	t5res_raw_create(R,DVR,DNR,_DFs,_DRVM,_State,DRes),
	b5sort_difference(Fs2,Fs1,Fs3),
	t5nr_subtract(DNR,NR1,NR2,NR3),
	(t5concid_subsumes_p(VR1,VR2),VR1 \== VR2 -> 
	    VR3=VR2 /* subtract(VR2,VR1,VR3)? */
	; VR3 = bottom(DVR)),
	t5rvm_subtract(RVM1,RVM2,RVM3).
		
t5rvm_subtract(RVM1,RVM2,RVM3) :- 
	(t5rvm_empty_p(RVM1) -> t5rvm_empty(RVM11);RVM11=RVM1),
	(t5rvm_empty_p(RVM2) -> t5rvm_empty(RVM22);RVM22=RVM2),
	t5rvm_raw_create(E1,Sp1,Sb1,RVM11),
	t5rvm_raw_create(E2,Sp2,Sb2,RVM22),
	t5rvm_raw_create(E3,Sp3,Sb3,RVM33),
	b5sort_difference(E2,E1,E3),
	b5sort_difference(Sp2,Sp1,Sp3),
	b5sort_difference(Sb2,Sb1,Sb3),
	(t5rvm_empty_p(RVM33) -> t5rvm_create(RVM3);RVM3=RVM33).





t5nr_subtract(-(DMin,DMax),-(Min1,Max1),-(Min2,Max2),-(Min3,Max3)) :-
	(t5point_comp(Min2,Min1,>) -> Min3=Min2
	;	Min3=bottom(DMin)
	),
	(t5point_comp(Max2,Max1,<) -> Max3=Max2
	;	Max3=bottom(DMax)
	).



b5nf_minimize(NF,Source,NNF,[Source]) :-
	b5nf_raw_create(T,Source,_P,_NP,_RL,_NR,X,Num,S,NF),
	b5nf_raw_create(T,Source,_,_,_,_,X,Num,S,NNF),!.

b5nf_minimize(NF1,_Source,NF3,DS) :-
	%FILTER= [user_defined],% & oder filter holen ????
	b5desc_filter(FILTER),
	b5nf_direct_supers(NF1,FILTER,DS),
	(DS = [] ->  	b5nf_r_rl(NF1,RLO,RLN,NF3),
		        t5rl_minimize2(RLO,RLN)

		; 	b5nf_infimum(DS,NF2),
			b5nf_diff(NF1,NF2,NF3),!
	).



b5nf_diff(NF1,NF2,NF3 ) :-
	b5nf_subtract(NF2,NF1,NF),
	b5nf_r_rl(NF,RL,NRL,NF3),
	t5rl_minimize(RL,NRL).

/*
% first is more special i.e. possible longer 
t5rl_minimize(RL,F_RL) :-
	t5rl_r_ress(RL,Ress,F_Ress,N_RL),
	t5ress_minimize(Ress,[],F_Ress),
	t5rl_minimize2(N_RL,F_RL).

t5ress_mini([],Ress,_,Ress) :- !.
t5ress_mini([_|_],[],_,[]) :- !.
t5ress_mini([Role|Roles],[H-Es|Ress],RES,N_Ress) :-
	b5typelist_order(Role,H,Order),
	t5ress_mini_order(Order,Role,Roles,H-Es,Ress,RES,N_Ress).

t5ress_mini_order(<,_Role,Roles,H-Es,Ress,RES,N_Ress) :-
	t5ress_mini(Roles,[H-Es|Ress],RES,N_Ress).

t5ress_mini_order(=,Role,Roles,HRES,Ress,R-ES,N_Ress) :-
	t5res_unbottom(HRES,RES1),
	t5res_unbottom(Role-ES,RES2),
	(t5res_subsumes_p(RES1,RES2) ->	N_Ress = Tail
	% Role-ES is a hack 
		; N_Ress = [HRES|Tail]
	),
	t5ress_mini(Roles,Ress,R-ES,Tail).

t5ress_mini_order(>,Role,Roles,H-Es,Ress,RES,[H-Es|N_Ress]) :-
	t5ress_mini([Role|Roles],Ress,RES,N_Ress).

t5ress_minimize([],Akku,Akku) :- !.
t5ress_minimize([R-Es|Ress],Accu,Final) :- 
 	t5role_supers(R,Supers),
	t5rl_filter(Supers,LessSupers),
	t5ress_mini(LessSupers,Ress,R-Es,N_Ress),
	t5ress_mini(LessSupers,Accu,R-Es,N_Accu),
	b5typelist_add_ele(N_Accu,R-Es,VN_Accu),
	t5ress_minimize(N_Ress,VN_Accu,Final).				

*/



/* first is more special i.e. possible longer */

t5rl_minimize(RL,F_RL) :-
	t5rl_unbottom(RL,XX),
	t5rl_r_ress(XX,Ress,F_Ress,N_RL),
	t5ress_minimize(Ress,Ress,F_Ress),
	t5rl_minimze2a(N_RL,VN_RL),
	t5rl_minimize2(VN_RL,F_RL).

t5rl_minimze2a(RL,N_RL) :-
	t5rl_r_ress(RL,Ress,F_Ress,N_RL),
	t5ress_minimize2a(Ress,F_Ress).

t5ress_minimize2a([],[]).
t5ress_minimize2a([H|T],N) :-
	(t5res_redundant_p(H) -> N=NT; N=[H|NT]),
	t5ress_minimize2a(T,NT).

t5ress_mini([],_,_,[]) :- !.

t5ress_mini([H-Res1|T],X,H-Res2,[NH|T]) :-
	!,
	( X == super ->
		t5res_sup_super(H-Res1,H-Res2,NH)
	  ;
		t5res_sup_sub(H-Res1,H-Res2,NH)
	).

t5ress_mini([H|T],X,Res,[H|NT]) :-
	t5ress_mini(T,X,Res,NT). 
	

t5ress_minimize([],Akku,Akku) :- !.
t5ress_minimize([Res|Ress],Accu,Final) :- 
	t5res_generate(R,VR,Min,Max,Fillers,Res),
 	t5role_supers(R,Supers),
	t5rl_filter(Supers,LessSupers),
	t5ress_minimize_supers(LessSupers,Fillers,Min,Accu,New_Accu),
 	t5role_subs(R,Subs),
	t5rl_filter(Subs,LessSubs),
	t5ress_minimize_subs(LessSubs,VR,Max,New_Accu,Very_New_Accu),
	t5ress_minimize(Ress,Very_New_Accu,Final).

t5ress_minimize_supers([],_,_,Accu,Accu).
t5ress_minimize_supers([Super|Supers],Fillers,Min,Accu,Final_Accu) :-
	t5res_gen_r_min_fillers(Super,Min,Fillers,Res),
	t5ress_mini(Accu,super,Res,New_Accu),
	t5ress_minimize_supers(Supers,Fillers,Min,New_Accu,Final_Accu).

t5ress_minimize_subs([],_,_,Accu,Accu). 
t5ress_minimize_subs([Sub|Subs],VR,Max,Accu,Final_Accu) :-
	t5res_gen_r_vr_max(Sub,VR,Max,Res),
	t5ress_mini(Accu,sub,Res,New_Accu),
	t5ress_minimize_subs(Subs,VR,Max,New_Accu,Final_Accu).

t5min_sup_super(A,A,D,D) :-  !.
t5min_sup_super(A,B,_D,A) :-  % min ist spezieller, darf nicht geloescht werden
	t5point_comp(A,B,>),!.
t5min_sup_super(_A,_B,D,D) :- !.
%	write('Min ??????'),nl.

t5max_sup_sub(A,A,D,D) :-  !. % loeschen
t5max_sup_sub(A,B,_D,A) :-   % max ist spezieller, nicht loeschen. 
	t5point_comp(A,B,<),!.
t5max_sup_sub(_A,_B,D,D) :-  !.
%	write('Max ??????'),nl.


/*
t5nr_sup(-(Min1,Max1),-(Min2,Max2),-(Min3,Max3)) :-
	t5min_sup(Min1,Min2,Min3),
	t5max_sup(Max1,Max2,Max3).
*/

t5nr_sup_super(-(Min1,Max1),-(Min2,_Max2),-(DMin,_DMax),-(Min3,Max1)) :-
	t5min_sup_super(Min1,Min2,DMin,Min3).

t5nr_sup_sub(-(Min1,Max1),-(_Min2,Max2),-(_DMin,DMax),-(Min1,Max3)) :-
	t5max_sup_sub(Max1,Max2,DMax,Max3).

t5concid_sup(A,A,D,D) :-  !. % A=B also setze default D
t5concid_sup(A,B,D,C) :- 
	(t5concid_subsumes_p(B,A) -> C=A; % A ist spezieller,
					  % darf nicht getilt werden	
%	write('VR ??????'),nl,
	C =D).	



/*
t5res_sup(Res1,Res2,SupRes) :-
	t5res_raw_create(R,VR1,NR1,Fs1,norvm,_State,Res1),
	t5res_raw_create(R,VR2,NR2,Fs2,norvm,_State,Res2),
	t5res_raw_create(R,VR3,NR3,Fs3,norvm,un-[],SupRes),
	b5sort_intersect(Fs2,Fs1,Fs3),
	t5nr_sup(NR1,NR2,NR3),
	t5concid_sup(VR1,VR2,VR3).
*/


t5res_sup_fillers(Fs,Fs,[]) :- !.
t5res_sup_fillers(Fs1,Fs2,[]) :-
	b5sort_subset_p(Fs1,Fs2),!.
	%write('fillers ???? '),nl.
t5res_sup_fillers(Fs1,_,Fs1).


t5res_sup_super(Res1,Res2,SupRes) :-
	t5res_raw_create(R,VR1,NR1,Fs1,RVM1,_State,Res1),
	t5res_raw_create(R,_VR2,NR2,Fs2,RVM2,_State,Res2),
	t5res_raw_create(R,VR1,NR3,Fs3,RVM3,un-[],SupRes),
	t5res_create(R,DRes),
	t5res_raw_create(R,_,DNR,_,_,_,DRes),
	t5res_sup_fillers(Fs1,Fs2,Fs3),
	t5nr_sup_super(NR1,NR2,DNR,NR3),
	t5rvm_unify(RVM1,RVM2,RVM3).
% rvm later

t5res_sup_sub(Res1,Res2,SupRes) :-
	t5res_raw_create(R,VR1,NR1,Fs1,RVM1,_State,Res1),
	t5res_raw_create(R,VR2,NR2,_Fs2,RVM2,_State,Res2),
	t5res_raw_create(R,VR3,NR3,Fs1,RVM3,un-[],SupRes),
	t5res_create(R,DRes),
	t5res_raw_create(R,DVR,DNR,_,_,_,DRes),
	t5nr_sup_sub(NR1,NR2,DNR,NR3),
	t5concid_sup(VR1,VR2,DVR,VR3),
	t5rvm_unify(RVM1,RVM2,RVM3).
% rvm later







t5rl_minimize2(RL,N_RL) :-
	t5rl_unbottom(RL,URL),
	t5rl_r_ress(URL,Ress,F_Ress,N_RL),
	t5ress_minimize2(Ress,F_Ress).


t5ress_minimize2([],[]).
t5ress_minimize2([Res|Ress],G) :-
	t5res_minimize2(Res,MRES),
	(t5res_ganz_wech_p(MRES) -> G=T;G=[MRES|T]),
	t5ress_minimize2(Ress,T).


t5res_ganz_wech_p(Res) :-
	t5res_raw_create(_R,VR,NR,Fs,RVM,_State,Res),
	%var(R),
	var(VR),var(NR),var(Fs),var(RVM).
/*
	functor(Res,_,Arity),
	between(1,Arity,ARG_I),
	arg(ARG_I,Res,Arg),
	( nonvar(Arg) -> fail ; %%hier jetzt weiter testen

	t5res_raw_create(R,VR,NR,Fs,RVM,State,RES).
*/

t5res_minimize2(RES,MRES) :-
	t5res_raw_create(R,VR,NR,Fs,RVM,State,RES),
	t5res_create(R,DRES),
	t5res_raw_create(R,DVR,DNR,_,_,_,DRES),
	t5res_raw_create(R,MVR,MNR,MFs,MRVM,MState,MRES),
	t5res_set_vr(DVR,VR,MVR),
	t5res_set_nr(Fs,DNR,NR,MVR,MNR),
	t5res_set_fs(Fs,MFs),
	t5res_set_rvm(RVM,MRVM),
	t5res_set_state(State,MState).

t5res_set_vr(_,VR,_) :- t5tbox_nothing_key(VR),!.
t5res_set_vr(VR,VR,_) :- !.
t5res_set_vr(DVR,VR,VR) :-
	% evtuell status und f"uller ber"ucksichtigen,
	% also wenn closed, dann bestimmen evt schon all
	% obj die vr vollst"andig
	t5concid_subsumes_p(DVR,VR),!.
t5res_set_vr(_,_,_).

t5res_set_nr(_Fs,NR,NR,_,_) :- !.
t5res_set_nr(Fs,DNR,NR,VR,F_NR) :-
	% evtuell close ber"ucksichtigen
	% 8 - tung close schein auf zweierlei weisen 
	% verwendet zu sein !!!
	b5sort_card(Fs,C),
	t5nr_add_min(DNR,C,NNR),
	(var(VR) -> t5nr_max(DNR,X_Max);t5concid_max(VR,X_Max)),
	t5nr_add_max(NNR,X_Max,X_NNR),
	t5nr_minmax(X_NNR,NMin,NMax),
	t5nr_minmax(NR,Min,Max),
	t5nr_set_min(NMin,Min,FMin),
	t5nr_set_max(NMax,Max,FMax),
	(var(FMin),var(FMax) -> true; t5nr_minmax(F_NR,FMin,FMax)).

t5nr_set_min(Min,Min,_) :- !.
t5nr_set_min(NMin,Min,Min) :-
	 t5nr_le_p(NMin,Min),!.
t5nr_set_min(_,_,_). 

t5nr_set_max(Max,Max,_) :- !.
t5nr_set_max(NMax,Max,Max) :-
	 t5nr_ge_p(NMax,Max),!.
t5nr_set_max(_,_,_).

t5res_set_fs([],_) :- !.
t5res_set_fs(X,X).

t5res_set_rvm(norvm,_) :- !.
t5res_set_rvm(RVM,RVM).

t5res_set_state(cl-State,cl-State) :- !. % ??????????????
t5res_set_state(_,_) :- !.


t5res_minimize3(RES,MRES) :-
	t5res_raw_create(R,VR,NR,Fs,RVM,State,RES),
	t5res_create(R,DRES),
	t5res_raw_create(R,_DVR,DNR,_,_,_,DRES),
	t5res_raw_create(R,MVR,MNR,Fs,RVM,State,MRES),
	%(nonvar(VR) -> t5res_set_vr(DVR,VR,MVR)),
	MVR = VR,
	(nonvar(NR) -> t5res_set_nr(DNR,NR,MNR)).


%t5res_set_nr(NR,NR,_) :- !.
t5res_set_nr(_DNR,NR,F_NR) :-
	% evtuell close ber"ucksichtigen
	% 8 - tung close schein auf zweierlei weisen 
	% verwendet zu sein !!!
	t5nr_minmax(NR,Min,Max),
	t5nr_minmax(F_NR,F_Min,F_Max),
	(Min =0 -> true;F_Min=Min),
	(Max =in -> true;F_Max=Max).

	

t5rl_minimize3(RL,N_RL) :-
	t5rl_r_ress(RL,Ress,F_Ress,N_RL),
	t5ress_minimize3(Ress,F_Ress).


t5ress_minimize3([],[]).
t5ress_minimize3([Res|Ress],G) :-
	t5res_minimize3(Res,MRES),
	(t5res_ganz_wech_p(MRES) -> G=T;G=[MRES|T]),
	t5ress_minimize3(Ress,T).





