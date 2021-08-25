% LAST EDIT: Tue Mar 10 15:33:11 1992 by Mirjam Kuehne (kit!mir) 
%%%%% EXPORT
%%%   t5tbox_all_key(-All)
%%%        Returns the key of the builtin all
%%%   t5tbox_anyrole_key(-Anyrole)
%%%        Returns the key of the builtin anyrole
%%%   t5tbox_anything_prim_key(-AnythingPrim)
%%%        Returns the key of the primitive component anything
%%%   t5tbox_aset_prim_key(-AsetPrim)
%%%        Returns the key of the primitive component aset
%%%   t5tbox_built_in_prims_keys(-Anything,-Aset,-Float,-Integer,-String)
%%%        Returns the keys of the built in primitive components
%%%   t5tbox_disjoint_p(+Type,+Key1,+Key2)
%%%        Type is either conc or role.
%%%   t5tbox_float_prim_key(-FloatPrim)
%%%        Returns the key of the primitive component float
%%%   t5tbox_init
%%%        Initializes the TBox (Key Counter, Hierarchy Cache, Built-Ins)
%%%   t5tbox_integer_prim_key(-IntegerPrim)
%%%        Returns the key of the primitive component integer
%%%   t5tbox_next_key   (or b5keys_gen_key)
%%%        Returns the next key and increments key counter
%%%   t5tbox_nothing_key(-Nothing)
%%%        Returns the name of the builtin nothing
%%%   t5tbox_string_prim_key(-StringPrim)
%%%        Returns the key of the primitive component string
%%%   t5tbox_subsumes_p(+Type,+Subsumer,+Subsumee)
%%%        Type is either conc or role.

tboxinit :-
	b5sta_init,
	b5inst_init,
	b5tbox_init_key,
	b5tbox_init_keys,
	t5tbox_init_pure_defs.
%%	t5tbox_init_domain_prims.

:- multifile b5dump_these_predicates/1.

b5dump_these_predicates([t5tbox_key/1,t5_pure_defs/2,t5tbox_all_key/1,t5tbox_anyrole_key/1, t5tbox_nothing_key/1,t5tbox_anything_prim_key/1,t5tbox_aset_prim_key/1,t5tbox_number_prim_key/1,t5tbox_string_prim_key/1,t5tbox_conc_hc/1,t5tbox_role_hc/1,t5tbox_feature_prim_key/1]).

%----------------

b5tbox_init_key :-
	retractall(t5tbox_key(_)),
	assert(t5tbox_key(0)),
	!.

t5tbox_next_key(Key) :-
	retract(t5tbox_key(Key)), !,
	NewKey is Key + 1,
	assert(t5tbox_key(NewKey)),
	!.

%% patch for the ABox
b5keys_gen_key(Key) :-
	t5tbox_next_key(Key). %% a5abox_key_next(Key).

%----------------
/*
t5tbox_init_role(Anyrole,Nothing) :-
	t5role_top(Anyrole),
	t5role_bottom(Nothing), 
	b5st_add_domain(thetopmostrole,(r,0),Anyrole,open,[]),
	b5st_new_nothing((r,0)),
	!.
*/

t5tbox_init_role(Anyrole,Nothing,FeaturePrim) :-
	t5role_top(Anyrole),
	t5role_bottom(Nothing), 
	b5st_add_domain(thetopmostrole,(r,0),Anyrole,open,[]),
	b5st_new_nothing((r,0)),
	t5role_feature_prim(FeaturePrim),
	!.

b5tbox_init_keys :-
	retractall(t5tbox_all_key(_)),
	retractall(t5tbox_anyrole_key(_)),
	retractall(t5tbox_nothing_key(_)),
	retractall(t5tbox_anything_prim_key(_)),
	retractall(t5tbox_aset_prim_key(_)),
	retractall(t5tbox_number_prim_key(_)),
	retractall(t5tbox_string_prim_key(_)),
	retractall(t5tbox_feature_prim_key(_)),
	t5tbox_next_key(All),
	assert(t5tbox_all_key(All)),
	b5tbox_init_st,
	t5cdb_init,
	t5rdb_init,
	t5dom_init,
	t5dom_init_top(All),
	t5tbox_next_key(Anyrole),
	assert(t5tbox_anyrole_key(Anyrole)),
	t5tbox_next_key(Nothing),
	assert(t5tbox_nothing_key(Nothing)),
	t5dom_init_bottom(Nothing,All),
	X = [anything-_-_,number-_-_,string-_-_,aset-_-_],
	t5dom_init_keys(X),
	b5tbox_init_hcs,
	t5dm_init,
	%b5tbox_init_hcs,
	t5dom_init_prims(X),
	t5dom_init_defs(X),
	t5dom_update_prims(X),
	t5tbox_init_built_ins(X),
	t5tbox_next_key(FeaturePrim),
	assert(t5tbox_feature_prim_key(FeaturePrim)),
	t5tbox_init_role(Anyrole,Nothing,FeaturePrim).

t5tbox_top_prim_key_p(X) :- t5tbox_map(_,X,_,Y),call(Y).

t5tbox_map(anything,P,(c,0),t5tbox_anything_prim_key(P)).
t5tbox_map(aset,P,(a,0),t5tbox_aset_prim_key(P)).
t5tbox_map(number,P,(n,0),t5tbox_number_prim_key(P)).
t5tbox_map(string,P,(s,0),t5tbox_string_prim_key(P)).


t5tbox_init_built_ins([]).
t5tbox_init_built_ins([Name-Prim-Def|T]) :-
	t5tbox_map(Name,Prim,X,Y),
	assert(Y),
	b5st_add_class(prim(Name),X,Prim,internal),
	b5st_add_domain(Name,X,Def,open,[]),
	b5st_add_class(Name,X,Def,internal),
	b5st_new_nothing(X),
	t5tbox_init_built_ins(T).
				
%----------------

b5tbox_init_hcs :-
	retractall(t5tbox_conc_hc(_)),
	retractall(t5tbox_role_hc(_)),
	t5hc_init_module,
	t5tbox_all_key(All),
	t5tbox_anyrole_key(Anyrole),
	t5tbox_nothing_key(Nothing),
	t5hc_init(All,Nothing,ConcHC),
	t5hc_init(Anyrole,Nothing,RoleHC),
	assert(t5tbox_conc_hc(ConcHC)),
	assert(t5tbox_role_hc(RoleHC)),
	!.
	
%----------------

b5tbox_init_st :-
	b5st_init.

%----------------

%%%%%%%%%%% Information on Concs and Roles
t5tbox_hc(conc,HC) :-
	t5tbox_conc_hc(HC),
	!.
t5tbox_hc(role,HC) :-
	t5tbox_role_hc(HC),
	!.

t5tbox_type(Key,conc) :-
	t5cdb(Key,_),!.
t5tbox_type(Key,role) :-
	t5rdb(Key,_,_,_,_,_,_),!.

t5tbox_add_filter(Key,Filter) :-
	t5tbox_type(Key,Type),
	t5tbox_add_filter(Type,Key,Filter).

t5tbox_add_filter(conc,Key,Filter) :-
	t5concid_add_filter(Key,Filter).

t5tbox_add_filter(role,Key,Filter) :-
	t5role_add_filter(Key,Filter).


t5tbox_direct_supers(Key,Supers) :-
	t5tbox_type(Key,T),
	t5tbox_direct_supers(T,Key,Supers).

t5tbox_direct_supers(Type,Term,Supers) :-
	t5tbox_hc(Type,HC),
	t5hc_direct_supers(HC,Term,Supers),
	!.
t5tbox_direct_subs(Type,Term,Subs) :-
	t5tbox_hc(Type,HC),
	t5hc_direct_supers(HC,Term,Subs),
	!.


t5tbox_supers(Key,Supers) :-
	t5tbox_type(Key,T),
	t5tbox_supers(T,Key,Supers).

t5tbox_supers(conc,Conc,Supers) :-
	t5concid_supers(Conc,Supers),
	!.
t5tbox_supers(role,Role,Supers) :-
	t5role_supers(Role,Supers),
	!.


t5tbox_subs(Key,Subs) :-
	t5tbox_type(Key,T),
	t5tbox_subs(T,Key,Subs).

t5tbox_subs(conc,Conc,Subs) :-
	t5concid_subs(Conc,Subs),
	!.
t5tbox_subs(role,Role,Subs) :-
	t5role_subs(Role,Subs),
	!.


t5tbox_sub_union(Key,Subs) :-
	t5tbox_type(Key,T),
	t5tbox_sub_union(T,Key,Subs).

t5tbox_sub_union(conc,Concs,Subs) :-
	t5concid_sub_union(Concs,Subs),
	!.
t5tbox_sub_union(role,Roles,Subs) :-
	t5role_sub_union(Roles,Subs),
	!.



%%% jjq: patch
t5tbox_sub_intersection(_,[],[]) :- !.
t5tbox_sub_intersection(conc,Concs,Subs) :-
	t5concid_sub_intersection(Concs,Subs),
	!.
t5tbox_sub_intersection(role,Roles,Subs) :-
	t5role_sub_intersection(Roles,Subs),
	!.


t5tbox_normal_form(conc,Conc,CNF) :-
	t5concid_normal_form(Conc,CNF),
	!.
t5tbox_normal_form(role,Role,RNF) :-
	t5role_normal_form(Role,RNF),
	!.

t5tbox_nf_prims(conc,Conc,Prims) :-
	b5nf_prims(Conc,Prims),
	!.
t5tbox_nf_prims(role,Role,Prims) :-
	t5rnf_prims(Role,Prims),
	!.

t5tbox_nf_neg_prims(conc,Conc,NegPrims) :-
	b5nf_neg_prims(Conc,NegPrims),
	!.
t5tbox_nf_neg_prims(role,Role,NegPrims) :-
	t5rnf_neg_prims(Role,NegPrims),
	!.

t5tbox_nf_comp_res(conc,Start,NF1,NF2,Result) :-
	b5nf_compare_without_prims(Start,NF1,NF2,Result),
	!.
t5tbox_nf_comp_res(role,Start,NF1,NF2,Result) :-
	t5rnf_compare_without_prims(Start,NF1,NF2,Result),
	!.

t5tbox_compare(conc,Sub1,CNF1,CNF2,Sub2) :-
	b5nf_compare(Sub1,CNF1,CNF2,Sub2),
	!.
t5tbox_compare(role,Sub1,RNF1,RNF2,Sub2) :-
	t5rnf_compare(Sub1,RNF1,RNF2,Sub2),
	!.

t5tbox_top_key(conc,All) :-
	t5tbox_all_key(All),
	!.
t5tbox_top_key(role,Anyrole) :-
	t5tbox_anyrole_key(Anyrole),
	!.

t5tbox_bottom_key(_,Nothing) :-
	t5tbox_nothing_key(Nothing),
	!.

t5tbox_incoherent_normal_form_p(conc,CNF) :-
	b5nf_incoherent_p(CNF),
	!.
t5tbox_incoherent_normal_form_p(role,RNF) :-
	t5rnf_incoherent_p(RNF),
	!.


t5tbox_remove(r,Role) :-
	t5tbox_role_hc(HC),
	t5hc_remove(HC,[Role]),
	t5role_remove(Role),
	!.
t5tbox_remove(_,Conc) :-
	t5tbox_conc_hc(HC),
	t5hc_remove(HC,[Conc]),
	t5concid_remove(Conc),
	!.


t5tbox_filter_holds_p(conc,Conc,Filter) :-
	t5concid_filter_holds_p(Conc,Filter),
	!.
t5tbox_filter_holds_p(role,Role,Filter) :-
	t5role_filter_holds_p(Role,Filter),
	!.
t5tbox_primitive_p(conc,Conc) :-
	t5concid_primitive_p(Conc),
	!.
t5tbox_primitive_p(role,Role) :-
	t5role_primitive_p(Role),
	!.

/* t5tbox_prim_top(role,AnyRole) :-
	t5tbox_anyrole_key(AnyRole),
	!.
t5tbox_prim_top(conc,AnythingPrim) :-
	t5tbox_anything_prim_key(AnythingPrim).
t5tbox_prim_top(conc,AsetPrim) :-
	t5tbox_aset_prim_key(AsetPrim).
t5tbox_prim_top(conc,NumberPrim) :-
	t5tbox_number_prim_key(NumberPrim).
t5tbox_prim_top(conc,StringPrim) :-
	t5tbox_string_prim_key(StringPrim). */
	

t5tbox_init_pure_defs :-
	retractall(t5_pure_defs(_,_)),
	assert(t5_pure_defs(conc,[])),
	assert(t5_pure_defs(role,[])).

t5tbox_update_pure_defs(Type,Term) :-
%%	t5tbox_domain_hack(Type,Term),
	t5tbox_normal_form(Type,Term,NF),
	t5tbox_nf_prims(Type,NF,Prims),
	t5tbox_top_and_prims(Type,Prims,Top,[]),
	(t5tbox_anything_prim_key(Top);t5tbox_anyrole_key(Top)),
	!,
	retract(t5_pure_defs(Type,Defs)),
	b5sort_unify(Defs,[Term],NewDefs),
	assert(t5_pure_defs(Type,NewDefs)),
	!.
t5tbox_update_pure_defs(_,_).

t5tbox_pure_defs(Type,Defs) :-
	t5_pure_defs(Type,Defs),
	!.

t5tbox_top_and_prims(role,Prims,Prim,RealPrims) :-
	%t5tbox_prim_top(role,Prim), %failt immer
	t5tbox_anyrole_key(Prim),
	%b5sort_member_p(Prim,Prims),% redundant
	b5sort_difference(Prims,[Prim],RealPrims).

t5tbox_top_and_prims(conc,Prims,Prim,RealPrims) :-
	t5dom_allprims(AllDomainPrims),% auch *neue* domains
	b5sort_very_special(AllDomainPrims,Prims,Prim,RealPrims).
	%same as
	%b5sort_intersect(AllDomainPrims,Prims,[Prim]),
	%b5sort_difference(Prims,[Prim],RealPrims).
	%but faster

%%% Dependance between prim-components and role-domains.
%%% Used to infer atmost(0,r) for concepts which are 
%%% primitively disjoint from the domain of r.

t5tbox_init_domain_prims :-
	retractall(t5tbox_domain_prim(_,_)).

t5tbox_domain_hack(role,Role) :-
	t5role_domain(Role,Domain),
	t5tbox_store_domain(Role,Domain).
t5tbox_domain_hack(conc,_).


t5tbox_store_domain(Role,Domain) :-
	t5concid_prims(Domain,Prims),
	t5tbox_process_domain_prims(Prims,Role).

t5tbox_process_domain_prims([],_).
t5tbox_process_domain_prims([Prim|Prims],Role) :-
	t5tbox_add_role_to_domain(Prim,Role),
	!,t5tbox_process_domain_prims(Prims,Role),
	!.

t5tbox_add_role_to_domain(AnyPrim,_) :-
	t5tbox_anything_prim_key(AnyPrim),
	!.
t5tbox_add_role_to_domain(Prim,Role) :-
	(retract(t5tbox_domain_prim(Prim,Roles)),
	b5sort_unify(Roles,[Role],NewRoles),
	assert(t5tbox_domain_prim(Prim,NewRoles));
	   assert(t5tbox_domain_prim(Prim,[Role]))),
	!.


t5tbox_disjoint_roles(NF,Roles) :-
	b5nf_neg_prims(NF,NegPrims),
	t5tbox_collect_disjoint_roles(NegPrims,Roles).

t5tbox_collect_disjoint_roles([],[]).
t5tbox_collect_disjoint_roles([Prim|Prims],AllRoles) :-
	t5tbox_domain_prim(Prim,Roles),
	!,
	t5tbox_collect_disjoint_roles(Prims,StoreRoles),
	b5sort_unify(Roles,StoreRoles,AllRoles).
t5tbox_collect_disjoint_roles([_|Prims],Roles) :-
	!,t5tbox_collect_disjoint_roles(Prims,Roles),
	!.


%% okp patch

t5tbox_anything_key(Key) :-
	b5st_get_class(anything,_,Key,_),!.

t5tbox_changed_status(conc,OldKey,NewKey) :-
	!,t5concid_changed_status(OldKey,NewKey).
t5tbox_changed_status(role,OldKey,NewKey) :-
	t5role_changed_status(OldKey,NewKey).

%%% Subsumption/Disjointness Information
t5tbox_subsumes_p(conc,Key1,Key2) :-
	t5concid_subsumes_p(Key1,Key2),
	!.
t5tbox_subsumes_p(role,Key1,Key2) :-
	t5role_subsumes_p(Key1,Key2),
	!.

t5tbox_disjoint_p(conc,Key1,Key2) :-
	t5concid_disjoint_p(Key1,Key2),
	!.
t5tbox_disjoint_p(role,Key1,Key2) :-
	t5role_disjoint_p(Key1,Key2),
	!.

t5tbox_equivalent_p(_,Key,Key) :-
	!.

t5tbox_incoherent_p(Type,Bottom) :-
	t5tbox_bottom_p(Type,Bottom),
	!.

t5tbox_bottom_p(conc,Key) :-
	!,
	t5tbox_nothing_key(Key).

t5tbox_bottom_p(role,Key) :-
	t5tbox_nothing_key(Key).
%----------------



%% "not" is used in some modules yet.

not(X) :-
	\+ X.




t5cls_conc(NF,ToldSups,ToldSubs,Sups,Subs,NegPrims,Key,Stat) :-
	t5cls_class(conc,NF,[],ToldSups,ToldSubs,Sups,Subs,NegPrims,Key,Stat),
	!.
t5cls_role(NF,ToldSups,ToldSubs,Sups,Subs,NegPrims,Key,Stat) :-
	t5cls_class(role,NF,[],ToldSups,ToldSubs,Sups,Subs,NegPrims,Key,Stat),
	!.

/* -----------------------------------------------------*/
/* patch von uk */
/*
t5cls_class(Type,NF,Filter,Told,_,AllSups,Subs,NegPrims,Key,Status) :-
	t5cls_class_xxx(Type,NF,Filter,Told,_,Sups,Subs,NegPrims,Key,Status),
	(Status == old -> true;
	(
	b5sort_subset_p(Told,Sups) ->
		AllSups = Sups
	;
		nl,
		write('*** classifier fehler ***'),nl,
		write('ToldSupers :'),writev(Told),nl,
		write('AllSupers :'),writev(Sups),nl,nl,nl,
		b5sort_unify(Told,Sups,AllSups) %%% PATCH
	)).
writev([]).
writev([X|Xs]) :-
	b5kif_name_key(N,_,X),
	write(X),write(/),write(N),write(' '),
	writev(Xs).
*/
/* -----------------------------------------------------*/

% ab jetzt wieder orginal, allerderdings mit _xxx 

t5cls_class(Type,NF,_,_,_,Supers,Subs,_,Bottom,old) :-
	t5tbox_incoherent_normal_form_p(Type,NF),
	t5tbox_bottom_key(Type,Bottom),
	t5tbox_supers(Type,Bottom,Supers),
	t5tbox_subs(Type,Bottom,Subs),
	!.
t5cls_class(Type,NF,Filter,Told,_,Sups,Subs,NegPrims,Key,Status) :-
	t5cls_mode(Filter,Mode),
	t5cls_complete_supers(Mode,Type,Told,KSups),
	t5cls_terms(Type,NF,Terms,MPrims),
	(Mode == prim,
	t5cls_prim(Terms,Type,NF,Told,MPrims,KSups,[],Sups,Subs,Key,Status);
	   Mode == def,
	   t5cls_def(Terms,Type,NF,Told,MPrims,KSups,[],Sups,Subs,Key,Status)),
	t5tbox_nf_neg_prims(Type,NF,NegPrims),
	!.

t5cls_prim([],Type,NF,_,_,KSups,KSubs,FinSups,FinSubs,_,new) :- 
	t5tbox_nf_prims(Type,NF,Prims),
	t5tbox_top_key(Type,Top),
	b5sort_unify_plus_one(KSups,Prims,Top,FinSups),
	t5tbox_bottom_key(Type,Bottom),
	b5sort_unify(KSubs,[Bottom],FinSubs),
	!.
t5cls_prim([Term|Terms],Type,NF,Told,MPrims,KSups,KSubs,Sups,Subs,Key,Stat) :-
%	(t5tbox_filter_holds_p(Type,Term,class_primitive);
	(t5cls_filter_holds_p(Type,Term,class_primitive);
	   t5tbox_primitive_p(Type,Term);
	       b5sort_member_p(Term,KSups);
	          b5sort_member_p(Term,KSubs)),
	!,t5cls_prim(Terms,Type,NF,Told,MPrims,KSups,KSubs,Sups,Subs,Key,Stat),
	!.
t5cls_prim([Term|Ts],Type,NF,Told,MPrims,KSups,KSubs,Sups,Subs,Key,Stat) :-
	t5cls_compare_prims(Type,first,NF,Term,Res),
	(Res == none,
	t5cls_eval_prim(prim,Type,NF,Term,Ts,MPrims,NTerms),
	NSups = KSups,
	NSubs = KSubs,
	!;
	   t5tbox_normal_form(Type,Term,TNF),
	   t5tbox_nf_comp_res(Type,Res,NF,TNF,R2),
           t5cls_eval(R2,prim,Type,Term,Ts,KSups,KSubs,NTerms,NSups,NSubs)),
	t5cls_prim(NTerms,Type,NF,Told,MPrims,NSups,NSubs,Sups,Subs,Key,Stat),
	!.

t5cls_def([],Type,NF,Told,_,KSups,KSubs,FinSups,FinSubs,_,new) :- 
	t5tbox_nf_prims(Type,NF,Prims),
	t5tbox_top_key(Type,Top),
	b5sort_unify(Prims,Told,AddSups),
	b5sort_unify_plus_one(KSups,AddSups,Top,FinSups),
	t5tbox_bottom_key(Type,Bottom),
	b5sort_unify(KSubs,[Bottom],FinSubs),
	!.
t5cls_def([Term|Terms],Type,NF,Told,MPrims,KSups,KSubs,Sups,Subs,Key,Stat) :-
	(t5tbox_primitive_p(Type,Term);
	    b5sort_member_p(Term,KSups);
	       b5sort_member_p(Term,KSubs)),
	!,t5cls_def(Terms,Type,NF,Told,MPrims,KSups,KSubs,Sups,Subs,Key,Stat),
	!.
t5cls_def([Term|Ts],Type,NF,Told,MPs,KSups,KSubs,Sups,Subs,Key,Stat) :-
	t5cls_compare_prims(Type,equal,NF,Term,Res),
	(Res == none,
	t5cls_eval_prim(def,Type,NF,Term,Ts,MPs,NTerms),
	NSups = KSups,
	NSubs = KSubs,
	!;
	   t5tbox_normal_form(Type,Term,TNF),
	   t5tbox_nf_comp_res(Type,Res,NF,TNF,R2),
           t5cls_eval(R2,def,Type,Term,Ts,KSups,KSubs,NTerms,NSups,NSubs)),
	(R2 == equal,
	Key = Term,
	Stat = old,
	t5tbox_supers(Type,Term,Sups),
	t5tbox_subs(Type,Term,Subs),
	!;
	   t5cls_def(NTerms,Type,NF,Told,MPs,NSups,NSubs,Sups,Subs,Key,Stat)).


t5cls_compare_prims(Type,Start,NF1,Term,Res) :-
	t5tbox_normal_form(Type,Term,NF2),
	t5tbox_nf_prims(Type,NF1,Prims1),
	t5tbox_nf_neg_prims(Type,NF1,NegPrims1),
	t5tbox_nf_prims(Type,NF2,Prims2),
	t5tbox_nf_neg_prims(Type,NF2,NegPrims2),
	b5sort_compare(Start,Prims1,Prims2,Store),
	b5sort_compare(Store,NegPrims1,NegPrims2,Res),
	!.

%%% For primitive terms only supers but no subs can be
%%% detected.Therefore all subs of those prims which are not
%%% prims of the term need not be compared.
%%% For defined terms, however, we still have to compare
%%% those terms that have all prims of the terms as their
%%% prims (MPrims).
t5cls_eval_prim(prim,Type,NF,Term,Terms,_,FinTerms) :-
	t5tbox_nf_prims(Type,NF,NPrims),
	t5tbox_normal_form(Type,Term,TNF),
	t5tbox_nf_prims(Type,TNF,TPrims),
	b5sort_difference(TPrims,NPrims,DiffPrims),
	t5tbox_sub_union(Type,DiffPrims,NonTerms),
	b5sort_difference(Terms,NonTerms,FinTerms),
	!.
t5cls_eval_prim(def,Type,NF,Term,Terms,MPrims,FinTerms) :-
	t5tbox_nf_prims(Type,NF,NPrims),
	t5tbox_normal_form(Type,Term,TNF),
	t5tbox_nf_prims(Type,TNF,TPrims),
	b5sort_difference(TPrims,NPrims,DiffPrims),
	t5tbox_sub_union(Type,DiffPrims,NonTerms),
	b5sort_difference(NonTerms,MPrims,SubTerms),
	b5sort_difference(Terms,SubTerms,FinTerms),
	!.

t5cls_eval(first,_,Type,Term,Terms,Sups,Subs,Terms,NSups,Subs) :-
	t5tbox_supers(Type,Term,TermSupers),
	b5sort_unify_plus_one(Sups,TermSupers,Term,NSups),
	!.
t5cls_eval(second,_,Type,Term,Terms,Sups,Subs,Terms,Sups,NSubs) :-
	t5tbox_subs(Type,Term,TermSubs),
	b5sort_unify_plus_one(Subs,TermSubs,Term,NSubs),
	!.
t5cls_eval(none,prim,Type,Term,Terms,Sups,Subs,NTerms,Sups,Subs) :- 
	t5tbox_subs(Type,Term,TSubs),
	b5sort_difference(Terms,TSubs,NTerms),
	!.
t5cls_eval(none,def,_,_,Terms,Sups,Subs,Terms,Sups,Subs).
t5cls_eval(equal,_,_,_,Terms,Sups,Subs,Terms,Sups,Subs).

t5cls_mode(Filter,prim) :-
	t5cls_holds_p(class_primitive,Filter),
	!.
t5cls_mode(_,def).

%%% A defined concept can be equivalent to one of its told
%%% supers. Therefore it has to be compared with them, and
%%% hence super_union has to be called instead of super_union_id.
t5cls_complete_supers(prim,Type,Told,Supers) :-
	t5tbox_hc(Type,HC),
	t5hc_super_union_id(HC,Told,Supers),
	!.
t5cls_complete_supers(def,Type,Told,Supers) :-
	t5tbox_hc(Type,HC),
	t5hc_super_union(HC,Told,Supers),
	!.

t5cls_terms(Type,NF,Terms,MorePrims) :-
	t5tbox_nf_prims(Type,NF,Prims),
	t5tbox_top_and_prims(Type,Prims,Top,RealPrims),
	!,
	t5cls_prim_subs(RealPrims,Type,Top,Terms),
	t5tbox_hc(Type,HC),
	t5hc_sub_intersection(HC,Prims,MorePrims),
	!.
%%% For the initial concepts, when the tops and prims are
%%% not yet introduced!
t5cls_terms(Type,NF,Terms,MorePrims) :-  
	t5tbox_nf_prims(Type,NF,Prims),
	t5tbox_hc(Type,HC),
	t5hc_sub_intersection(HC,Prims,MorePrims),
	t5tbox_top_key(Type,Top),
	t5tbox_subs(Type,Top,Subs),
	b5sort_unify(Subs,[Top],Terms),
	!.

t5cls_prim_subs([],Type,Top,Terms) :-
	t5tbox_hc(Type,HC),
	t5hc_subs(HC,Top,AllSubs),  
	b5sort_unify(AllSubs,[Top],Terms),
	!.
t5cls_prim_subs(Prims,Type,_,Terms) :-
	t5tbox_hc(Type,HC),
	t5hc_sub_union_id(HC,Prims,Subs),
	t5tbox_pure_defs(Type,PureDefs),
	b5sort_unify(Subs,PureDefs,Terms),
	!.


t5cls_super_concs(NF,_,AllSupers,Bottom,old) :-
	b5nf_incoherent_p(NF),
	t5tbox_nothing_key(Bottom),
	t5concid_supers(Bottom,AllSupers),
	!.
t5cls_super_concs(NF,KnownSupers,FinSupers,Eq,Status) :-
	t5cls_complete_supers(def,conc,KnownSupers,StoreSupers),
	t5cls_terms(conc,NF,Terms,_),
	t5cls_nf_supers(Terms,NF,StoreSupers,AllSupers,Eq,Status),
	(Status == old,!,
	FinSupers = AllSupers;
	   b5sort_unify(KnownSupers,AllSupers,FinSupers)),
	!.

t5cls_nf_supers([],NF,StoreSupers,AllSupers,_,new) :-
	b5nf_prims(NF,Prims),
	b5sort_unify(StoreSupers,Prims,AllSupers), 
	!.
t5cls_nf_supers([Term|Terms],NF,StoreSupers,AllSupers,Key,Stat) :-
	(t5tbox_filter_holds_p(Type,Term,user_primitive);
	   t5tbox_primitive_p(Type,Term);
	       b5sort_member_p(Term,StoreSupers)),
	!,t5cls_nf_supers(Terms,NF,StoreSupers,AllSupers,Key,Stat),
	!.
t5cls_nf_supers([Term|Terms],NF,StoreSupers,AllSupers,Key,Stat) :-
	t5cls_compare_prims(conc,equal,NF,Term,Res),
	((Res == none; Res == second),
	!, t5cls_eval_prim(prim,conc,NF,Term,Terms,_,NewTerms),
	NewSupers = StoreSupers;
	   t5tbox_normal_form(Type,Term,TNF),
	   t5tbox_nf_comp_res(Type,Res,NF,TNF,R2),
	   t5cls_super_eval(R2,Term,Terms,StoreSupers,NewTerms,NewSupers)),
	(R2 == equal,
	Key = Term,
	Stat = old,
	t5tbox_supers(Type,Term,AllSupers),
	!;
	   t5cls_nf_supers(NewTerms,NF,NewSupers,AllSupers,Key,Stat)),
	!.

t5cls_super_eval(first,Conc,Concs,Supers,Concs,NewSupers) :-
	t5concid_supers(Conc,ConcSupers),
	b5sort_unify_plus_one(Supers,ConcSupers,Conc,NewSupers),
	!.
t5cls_super_eval(_,Conc,Concs,Supers,NewConcs,Supers) :-
	t5concid_subs(Conc,Subs),
	b5sort_difference(Concs,Subs,NewConcs),
	!.

t5cls_sub_concs(NF,Supers,Subs) :-
	t5tbox_conc_hc(HC),
	t5hc_sub_intersection(HC,Supers,Concs),
	t5cls_nf_subs(Concs,NF,[],Subs),
	!.

t5cls_nf_subs([],_,Subs,Subs).
t5cls_nf_subs([Conc|Concs],NF,Subs,FinSubs) :-
	b5sort_member_p(Conc,Subs),
	!,t5cls_nf_subs(Concs,NF,Subs,FinSubs),
	!.
t5cls_nf_subs([Conc|Concs],NF,Subs,FinSubs) :-
	t5concid_normal_form(Conc,CNF),
	b5nf_compare(second,NF,CNF,Result),
	t5cls_sub_eval(Result,Conc,Subs,NewSubs),
	!,t5cls_nf_subs(Concs,NF,NewSubs,FinSubs),
	!.

t5cls_sub_eval(second,Conc,Subs,NewSubs) :-
	t5concid_subs(Conc,ConcSubs),
	b5sort_unify_plus_one(Subs,ConcSubs,Conc,NewSubs),
	!.
t5cls_sub_eval(_,_,Subs,Subs).


%%%%patch
t5cls_holds_p(class_primitive,Filter) :-
	b5sort_common_member_p([inv_primitive,user_primitive],Filter).

t5cls_filter_holds_p(conc,Term,class_primitive) :-
	t5concid_filter(Term,Filter),
	b5sort_common_member_p([inv_primitive,user_primitive],Filter).
t5cls_filter_holds_p(role,Term,class_primitive) :-
	t5rdb_filter(Term,Filter),
	b5sort_common_member_p([inv_primitive,user_primitive],Filter).


	
/* ------------------------------------------------------------ */  
/*			states of subsumtion 			*/
/* ------------------------------------------------------------ */  
/*
     ffs : unify(none,A,B)  ?
     t5sub_unify(A,B,C) fi t5sub_reverse(C,RC),t5sub_unify(B,A,RC)
*/

t5sub_unify(Sub,Sub,Sub) :- !.
t5sub_unify(Sub,equal,Sub) :- !.
t5sub_unify(equal,Sub,Sub) :- !.
t5sub_unify(_,_,none).

t5sub_reverse(none,none).
t5sub_reverse(equal,equal).
t5sub_reverse(first,second).
t5sub_reverse(second,first).


/* ------------------------------------------------------------ */ 
/*			concepts				*/
/* ------------------------------------------------------------ */ 
/*
	key
	flag  p-prim d-def 
	NF
	[]
	Filter
*/


t5conc_raw_create(Flag,CNF,RI,Filter,conc(Flag,CNF,RI,Filter)).
t5conc_p(conc(_Flag,_CNF,_RI,_Filter)).

t5conc_flag(conc(Flag,_,_,_),Flag) :- !.
t5conc_nf(conc(_,NF,_,_),NF) :- !.
t5conc_ri(conc(_,_,RI,_),RI) :- !.
t5conc_filter(conc(_,_,_,F),F) :- !.

t5conc_s_flag(conc(_,C,R,Fi),F,conc(F,C,R,Fi)) :- !.
t5conc_s_nf(conc(F,_,R,Fi),C,conc(F,C,R,Fi)) :- !.
t5conc_s_r(conc(F,C,_,Fi),R,conc(F,C,R,Fi)) :- !.
t5conc_s_filter(conc(F,C,R,_),Fi,conc(F,C,R,Fi)) :- !.

t5conc_r_flag(conc(F,C,R,Fi),F,NF,conc(NF,C,R,Fi)) :- !.
t5conc_r_nf(conc(F,C,R,Fi),C,NC,conc(F,NC,R,Fi)) :- !.
t5conc_r_r(conc(F,C,R,Fi),R,NR,conc(F,C,NR,Fi)) :- !.
t5conc_r_filter(conc(F,C,R,Fi),Fi,NFi,conc(F,C,R,NFi)) :- !.


t5conc_create(Conc) :-
	b5nf_create_cnf(CNF),
	t5fil_create(FIL),
	t5conc_raw_create(_,CNF,_,FIL,Conc).

t5conc_new_prim(Conc) :-
	b5nf_create_cnf(CNF),
	t5fil_create(FIL),
	t5conc_raw_create(p,CNF,_,FIL,Conc).

t5conc_init_bottom(Key,Domain,Bottom) :-
        b5nf_create_bottom(Key,Domain,B),
        t5fil_create(FIL),
	t5fil_add(builtin,FIL,FILTER),
        t5conc_raw_create(p,B,_,FILTER,Bottom).

t5conc_init_top(Topkey,Topconc) :-
        b5nf_create_top(Topkey,Top_NF),
        t5fil_create(FIL),
	t5fil_add(builtin,FIL,FILTER),
        t5conc_raw_create(p,Top_NF,_,FILTER,Topconc).


t5conc_new_prim(Conc,Prim) :-
        b5nf_create_prim(Prim,PrimNF),
        t5fil_create(FIL),
        t5conc_raw_create(p,PrimNF,_,FIL,Conc).

%t5conc_new_prim(-Conc,+Domain,+Prim) :-
t5conc_new_prim(Conc,Domain,Prim) :-
        b5nf_create_prim(Prim,Domain,PrimNF),
        t5fil_create(FIL),
        t5conc_raw_create(p,PrimNF,_,FIL,Conc).

t5conc_new_def(Conc) :-
	b5nf_create_cnf(CNF),
	t5conc_raw_create(d,CNF,_,_,Conc).
	
t5conc_new_def(NF,Filter,Conc) :-
	t5fil_create(FIL),
	t5fil_unify(FIL,Filter,F),
	t5conc_raw_create(d,NF,[],F,Conc).

t5conc_new_atomic_prim_conc(Dom,Super,Prim,Filter,Conc) :-
	b5nf_create(Dom,NF),
	b5nf_add_conc(NF,Super,N_NF),
	b5nf_add_prim(N_NF,Prim,VN_NF),
	b5nf_complete(VN_NF,F_NF),
	t5conc_raw_create(d,F_NF,[],Filter,Conc).

t5conc_used_objects(Conc,Objs) :-
	t5conc_nf(Conc,NF),
	b5nf_used_objects(NF,Objs).

/*  nf */
t5conc_prims(C,Prims) :-
        t5conc_nf(C,NF),
        b5nf_prims(NF,Prims).

t5conc_neg_prims(C,NegPrims) :-
        t5conc_nf(C,CNF),
        b5nf_neg_prims(CNF,NegPrims).

t5conc_reslist(C,ResList) :-
        t5conc_nf(C,CNF),
        b5nf_reslist(CNF,ResList).

t5conc_aset_open_p(C) :- 
	t5conc_nf(C,NF),
	b5nf_aset_open_p(NF).

t5conc_aset_close_p(C) :-
	t5conc_nf(C,NF),
	b5nf_aset_close_p(NF).

t5conc_aset_p(C) :-
	t5conc_nf(C,NF),
	b5nf_aset_p(NF).

t5conc_number_p(C) :-
	t5conc_nf(C,NF),
	b5nf_number_p(NF).

t5conc_string_p(C) :-
	t5conc_nf(C,NF),
	b5nf_string_p(NF).

t5conc_concept_p(C) :-
	t5conc_nf(C,NF),
	b5nf_concept_p(NF).

t5conc_closed_concept_p(C) :-
	t5conc_nf(C,NF),
	b5nf_type(NF,norm-close).

t5conc_type_key(C,Key) :-
	t5conc_nf(C,NF),
	b5nf_type_key(NF,Key).

t5conc_type(C,Type) :-
	t5conc_nf(C,NF),
	b5nf_type(NF,T),
	t5concmap(T,Type).

t5concmap(prim,primitive).
t5concmap(number,number).
t5concmap(string,string).
t5concmap(aset-X,aset-X) :- !.
t5concmap(norm-_,concept) :- !.
t5concmap(top,top).
t5concmap(bottom,bottom).

t5conc_disjoint_p(Conc1,Conc2) :-
	t5conc_nf(Conc1,NF1),
	t5conc_nf(Conc2,NF2),
	b5nf_disjoint_p(NF1,NF2).
	
t5conc_incoherent_p(Conc) :-
	t5conc_nf(Conc,NF),    %% cmk 28.1.93
	b5nf_incoherent_p(NF).	

/* cnf.rl */
t5conc_role_res(C,Role,Res) :-
        t5conc_nf(C,CNF),
        b5nf_role_res(CNF,Role,Res).

/*cnf.rl.res */
t5conc_vr(C,Role,Vr) :-
        t5conc_nf(C,CNF),
        b5nf_vr(CNF,Role,Vr).

t5conc_nr(C,Role,Nr) :-
        t5conc_nf(C,CNF),
        b5nf_nr(CNF,Role,Nr).

/*cnf.rl.res.nr */
t5conc_min(C,Role,Min) :-
        t5conc_nf(C,CNF),
        b5nf_min(CNF,Role,Min).

t5conc_max(C,Role,Max) :-
        t5conc_nf(C,CNF),
        b5nf_max(CNF,Role,Max).

t5conc_vr_min_max(C,Role,VR,Min,Max) :-
        t5conc_nf(C,NF),
	b5nf_vr_min_max(NF,Role,VR,Min,Max).		

/* nf.rl.res */
t5conc_fillers(C,Role,Fillers) :-
        t5conc_nf(C,CNF),
        b5nf_fillers(CNF,Role,Fillers).

t5conc_rvm_equals(C,Role,Eqs) :-
        t5conc_nf(C,CNF),
        b5nf_rvm_equals(CNF,Role,Eqs).

t5conc_rvm_supers(C,Role,Supers) :-
        t5conc_nf(C,CNF),
        b5nf_rvm_supers(CNF,Role,Supers).

t5conc_rvm_subs(C,Role,Subs) :-
        t5conc_nf(C,CNF),
        b5nf_rvm_subs(CNF,Role,Subs).


/* nf */
t5conc_min(C,MinC) :-
        t5conc_nf(C,CNF),
        b5nf_min(CNF,MinC).

t5conc_max(C,MaxC) :-
        t5conc_nf(C,CNF),
        b5nf_max(CNF,MaxC).

t5conc_minmax(C,NR) :- 
        t5conc_nf(C,CNF),
        b5nf_nr(CNF,NR).

t5conc_extension(C,Ext) :-
        t5conc_nf(C,CNF),
        b5nf_x(CNF,Ext).

t5conc_add_max(Conc,Max,Role,N_Conc) :-
        t5conc_r_nf(Conc,CNF,N_CNF,N_Conc),
        b5nf_add_max(CNF,Role,Max,N_CNF).

t5conc_add_min(Conc,Min,Role,N_Conc) :-
        t5conc_r_nf(Conc,CNF,N_CNF,N_Conc),
        b5nf_add_min(CNF,Role,Min,N_CNF).

t5conc_add_vr1(Conc,VR,Role,N_Conc) :-
        t5conc_r_nf(Conc,CNF,N_CNF,N_Conc),
        b5nf_add_vr(CNF,VR,Role,N_CNF).

t5conc_add_prim(Conc,Prim,N_Conc) :-
	t5conc_r_nf(Conc,CNF,N_CNF,N_Conc),
	b5nf_add_prims(CNF,[Prim],N_CNF).

t5conc_add_neg_prim(Conc,NegPrim,N_Conc) :-
	t5conc_r_nf(Conc,CNF,N_CNF,N_Conc),
	b5nf_add_neg-prims(CNF,[NegPrim],N_CNF).

t5conc_add_min_card(Conc,Min,N_Conc) :-
	t5conc_r_nf(Conc,CNF,N_CNF,N_Conc),
	b5nf_add_min_card(CNF,Min,N_CNF).

t5conc_add_max_card(Conc,Max,N_Conc) :-
	t5conc_r_nf(Conc,CNF,N_CNF,N_Conc),
	b5nf_add_max_card(CNF,Max,N_CNF).

t5conc_add_fillers(Conc,Fillers,Role,N_Conc) :-
	t5conc_r_nf(Conc,CNF,N_CNF,N_Conc),
	b5nf_add_fillers(CNF,Fillers,Role,N_CNF).

t5conc_add_filter(Conc,Filter,N_Conc) :-
	t5conc_r_filter(Conc,O_Filter,N_Filter,N_Conc),
	t5fil_unify(O_Filter,Filter,N_Filter).

t5conc_del_filter(Conc,Filter,N_Conc) :-
	t5conc_r_filter(Conc,O_Filter,N_Filter,N_Conc),
	t5fil_del(O_Filter,Filter,N_Filter).

t5conc_new_attributes(Conc,List,N_Conc):-
	t5conc_r_nf(Conc,NF,N_NF,N_Conc),
	b5nf_new_attributes(NF,List,N_NF). 

t5conc_rvrl(Conc,RVRL) :- 
	t5conc_nf(Conc,NF),
	b5nf_rvrl(NF,RVRL).

t5conc_include_role(Conc,Role,Supers,Subs,N_Conc) :-
	t5conc_r_nf(Conc,NF,N_NF,N_Conc),
	b5nf_include_role(NF,Role,Supers,Subs,N_NF).

t5conc_direct_supers(Conc,Filter,DS) :-
	t5conc_nf(Conc,NF),
	b5nf_direct_supers(NF,Filter,DS).
% LAST EDIT: Sat Jan 30 14:03:40 1993 by Mirjam Kuehne (madonna!mir) 
/* ------------------------------------------------------------ */ 
/* 		Concept id					*/	
/* ------------------------------------------------------------ */ 

/* a conceptid is a key, i.e. a pointer to a concept */

/* hc */
t5concid_supers(C,Supers) :-
        t5tbox_conc_hc(HC),
        t5hc_supers(HC,C,Supers).

t5concid_supers(C,Filter,Supers) :-
        t5tbox_conc_hc(HC),
        t5hc_supers(HC,C,Filter,Supers).

t5concid_direct_supers(C,D_Supers) :-
        t5tbox_conc_hc(HC),
        t5hc_direct_supers(HC,C,D_Supers).

t5concid_direct_supers(C,Filter,DS) :-
	 t5tbox_conc_hc(HC),
         t5hc_supers(HC,C,D_Supers),
	 t5hc_minimize_special(HC,D_Supers,Filter,DS).

% ------ %

t5concid_subs(C,Filter,Subs) :-
        t5tbox_conc_hc(HC),
        t5hc_subs(HC,C,Filter,Subs).

t5concid_subs(C,Subs) :-
        t5tbox_conc_hc(HC),
        t5hc_subs(HC,C,Subs).

t5concid_direct_subs(C,D_Subs) :-
        t5tbox_conc_hc(HC),
        t5hc_direct_subs(HC,C,D_Subs).

t5concid_direct_subs(C,Filter,D_Subs) :-
        t5tbox_conc_hc(HC),
        t5hc_direct_subs(HC,C,Filter,D_Subs).

% ------ %

t5concid_known_disjoints(C,Disj) :-
        t5tbox_conc_hc(HC),
        t5hc_known_disjoints(HC,C,Disj).

t5concid_non_disjoints(C,NonDisj) :-
        t5tbox_conc_hc(HC),
        t5hc_non_disjoints(HC,C,NonDisj).

t5concid_possibly_disjoints(C,PossDisj) :-
        t5tbox_conc_hc(HC),
        t5hc_possibly_disjoints(HC,C,PossDisj).

t5concid_disjoints(C,AllDisj) :-
        t5tbox_conc_hc(HC),
        t5hc_possibly_disjoints(HC,C,PossDisj),
        t5hc_known_disjoints(HC,C,KnownDisj),
	t5hc_sub_union_id(HC,KnownDisj,MoreDis),
        %t5hc_non_disjoints(HC,C,NonDisj),
	t5concid_test_disj(PossDisj,C,HC,RealDisj),
	b5sort_unify(MoreDis,RealDisj,AllDisj).
	
t5concid_test_disj([],_C,_HC,[]).
t5concid_test_disj([Test|Tests],C,HC,RealDisj) :-
	(
	t5concid_disjoint_p(unknown,HC,Test,C) ->
		%t5hc_subs(HC,Test,Subs),
		b5kif_subsx(Test,Subs),
		b5sort_difference(Tests,Subs,LessTests),
		RealDisj = [Test|TailReal]
	;
		t5hc_supers(HC,Test,Supers),
		b5sort_difference(Tests,Supers,LessTests),
		RealDisj = TailReal
	),	
	t5concid_test_disj(LessTests,C,HC,TailReal).

t5concid_sub_union(C,SubUnion) :-
        t5tbox_conc_hc(HC),
        t5hc_sub_union(HC,C,SubUnion).

t5concid_super_union(C,SuperUnion):-
        t5tbox_conc_hc(HC),
        t5hc_super_union(HC,C,SuperUnion).

t5concid_super_intersection(C,SuperInter) :-
        t5tbox_conc_hc(HC),
        t5hc_super_intersection(HC,C,SuperInter).

t5concid_sub_intersection(C,SubInter):-
        t5tbox_conc_hc(HC),
        t5hc_sub_intersection(HC,C,SubInter).

t5concid_subsumes_p(Key1,Key2) :-
        t5tbox_conc_hc(HC),
        t5hc_subsumes_p(HC,Key1,Key2).

%% t5concid_subsumes_some_p(+Conc, +ConcList)
%%  True if Conc subsumes at least one of the elements of ConcList

t5concid_subsumes_some_p(A,L) :-
	member(B,L),
	t5concid_subsumes_p(A,B),
	!.

/* nf */
t5concid_prims(C,Prims) :-
	t5cdb_get(C,Conc,_Filter),
	t5conc_prims(Conc,Prims).

t5concid_neg_prims(C,NegPrims) :-
	t5cdb_get(C,Conc,_Filter),
	t5conc_neg_prims(Conc,NegPrims).

t5concid_reslist(C,ResList) :-
	t5cdb_get(C,Conc,_Filter),
	t5conc_reslist(Conc,ResList).


t5concid_aset_open_p(C) :- 
	t5cdb_get(C,Conc),
	t5conc_aset_open_p(Conc).

t5concid_aset_close_p(C) :-
	t5cdb_get(C,Conc),
	t5conc_aset_close_p(Conc).

t5concid_aset_p(C) :-
	t5cdb_get(C,Conc),
	t5conc_aset_p(Conc).

t5concid_number_p(C) :-
	t5cdb_get(C,Conc),
	t5conc_number_p(Conc).

t5concid_string_p(C) :-
	t5cdb_get(C,Conc),
	t5conc_string_p(Conc).

t5concid_concept_p(C) :-
	t5cdb_get(C,Conc),
	t5conc_concept_p(Conc).

t5concid_closed_concept_p(C) :-
	t5cdb_get(C,Conc),
	t5conc_closed_concept_p(Conc).

t5concid_type_key(C,Key) :-
	t5cdb_get(C,Conc), 
	t5conc_type_key(Conc,Key).

t5concid_type(C,Type) :-
	t5cdb_get(C,Conc), 
	t5conc_type(Conc,Type).


t5concid_primitive_p(Key) :-
	t5cdb_get(Key,Conc,_),
	t5conc_flag(Conc,p).

t5concid_aset_extension(Key,X) :-
	t5concid_aset_p(Key),
	t5concid_extension(Key,X).

	
/* nf.rl */
t5concid_role_res(C,Role,Res) :-
	t5cdb_get(C,Conc,_Filter),
	t5conc_role_res(Conc,Role,Res).

/*nf.rl.res */
t5concid_vr(C,Role,Vr) :-
	t5cdb_get(C,Conc,_Filter),
	t5conc_vr(Conc,Role,Vr).

t5concid_nr(C,Role,NR) :-
	t5cdb_get(C,Conc,_Filter),
	t5conc_nr(Conc,Role,NR).


/*nf.rl.res.nr */
t5concid_min(C,Role,Min) :-
	t5cdb_get(C,Conc,_Filter),
	t5conc_min(Conc,Role,Min).

t5concid_max(C,Role,Max) :-
	t5cdb_get(C,Conc,_Filter),
	t5conc_max(Conc,Role,Max).

t5concid_min_max(C,Role,Min,Max) :-
	t5cdb_get(C,Conc,_Filter),
	t5conc_vr_min_max(Conc,Role,_VR,Min,Max).

t5concid_vr_min_max(C,Role,VR,Min,Max) :-
	t5cdb_get(C,Conc,_Filter),
	t5conc_vr_min_max(Conc,Role,VR,Min,Max).

/* nf.rl.res */
t5concid_fillers(C,Role,Fillers) :-
	t5cdb_get(C,Conc,_Filter),
	t5conc_fillers(Conc,Role,Fillers).

t5concid_rvm_equals(C,Role,Eqs) :-
	t5cdb_get(C,Conc,_Filter),
	t5conc_rvm_equals(Conc,Role,Eqs).

t5concid_rvm_supers(C,Role,Supers) :-
	t5cdb_get(C,Conc,_Filter),
	t5conc_rvm_supers(Conc,Role,Supers).

t5concid_rvm_subs(C,Role,Subs) :-
	t5cdb_get(C,Conc,_Filter),
	t5conc_rvm_subs(Conc,Role,Subs).

/* nf */
t5concid_min(C,MinC) :-
	t5cdb_get(C,Conc,_Filter),
	t5conc_min(Conc,MinC).

t5concid_max(C,MaxC) :-
	t5cdb_get(C,Conc,_Filter),
	t5conc_max(Conc,MaxC).

t5concid_minmax(C,NR) :- 
	t5cdb_get(C,Conc,_Filter),
	t5conc_minmax(Conc,NR).

t5concid_extension(C,Ext) :-
	t5cdb_get(C,Conc,_Filter),
	t5conc_extension(Conc,Ext).

t5concid_extension_memberchk(C, ObjId) :-
	t5concid_extension(C, Ext),
	memberchk(ObjId, Ext).	

t5concid_rvrl(C,RVRL) :-
	t5cdb_get(C,Conc,_Filter),
	t5conc_rvrl(Conc,RVRL).

t5concid_conc(C,Conc) :-
	t5cdb_get(C,Conc,_Filter).

t5concid_add_max(Key,Max,Role) :-
	t5cdb_get(Key,Conc),
	t5conc_add_max(Conc,Max,Role,N_Conc),
	t5cdb_overstore(Key,N_Conc).

t5concid_add_min(Key,Min,Role) :-
	t5cdb_get(Key,Conc),
	t5conc_add_min(Conc,Min,Role,N_Conc),
	t5cdb_overstore(Key,N_Conc).

t5concid_add_vr1(Key,VR,Role) :-
	t5cdb_get(Key,Conc),
	t5conc_add_vr1(Conc,VR,Role,N_Conc),
	t5cdb_overstore(Key,N_Conc).

t5concid_add_filter(Key,Filter) :-
	t5cdb_get(Key,Conc),
	t5conc_add_filter(Conc,Filter,N_Conc),
	t5cdb_overstore(Key,N_Conc).

t5concid_del_filter(Key,Filter) :-
	t5cdb_get(Key,Conc),
	t5conc_del_filter(Conc,Filter,N_Conc),
	t5cdb_overstore(Key,N_Conc).

t5concid_new_attributes(Key,Keys) :-
	t5cdb_get(Key,Conc),
	t5conc_new_attributes(Conc,Keys,N_Conc),
	t5cdb_overstore(Key,N_Conc). 

t5concid_built_in(Key) :-
        t5tbox_conc_hc(HC),
        t5tbox_all_key(All),
        t5tbox_nothing_key(Nothing),
        t5hc_insert(HC,Key,[All],[Nothing],[]),
	t5conc_new_prim(Conc,Key),
        t5cdb_store(Key,Conc).

/* the domain is addted to the prim , only for buildins */
t5concid_update_prim(Prim,Domain) :-
	t5cdb_get(Prim,Conc),
	t5conc_r_nf(Conc,NF,NNF,NConc),
	b5nf_s_domain(NF,Domain,NNF),
	t5cdb_overstore(Prim,NConc).


%t5concid_new_atomic_prim_conc(+DomKey,+SuperKey,+Primkey,+Filter,-Key) :-
t5concid_new_atomic_prim_conc(Dom,SuperKey,PrimKey,Filter,Key) :-
        t5tbox_conc_hc(HC),
	t5tbox_next_key(Key),
	t5conc_new_atomic_prim_conc(Dom,SuperKey,PrimKey,Filter,Conc),
	t5hc_supers(HC,SuperKey,Supers),
	b5sort_unify([PrimKey],Supers,AllSupers1),
	b5sort_unify([SuperKey],AllSupers1,AllSupers),
        t5tbox_nothing_key(Nothing),
	t5conc_neg_prims(Conc,Disjoints),
        t5hc_insert(HC,Key,AllSupers,[Nothing],Disjoints),
        t5cdb_store(Key,Conc).


t5concid_new_prim(Key) :-
        t5tbox_next_key(Key),
        t5tbox_conc_hc(HC),
        t5tbox_all_key(All),
        t5tbox_nothing_key(Nothing),
        t5hc_insert(HC,Key,[All],[Nothing],[]),
        t5conc_new_prim(Conc,Key),
        t5cdb_store(Key,Conc).

%t5concid_new_prim(+Domain,-Key)
t5concid_new_prim(Domain,Key) :-
        t5tbox_next_key(Key),
        t5tbox_conc_hc(HC),
        t5tbox_all_key(All),
        t5tbox_nothing_key(Nothing),
        t5hc_insert(HC,Key,[All],[Nothing],[]),
        t5conc_new_prim(Conc,Domain,Key),
        t5cdb_store(Key,Conc).

t5concid_new_def(NF,Filter,Supers,Subs,Disj,Key) :-
	t5tbox_next_key(Key),
	t5tbox_conc_hc(HC),
	t5conc_new_def(NF,Filter,Conc),
	t5hc_insert(HC,Key,Supers,Subs,Disj),
	t5cdb_store(Key,Conc).

t5concid_new_def2(NF,Filter,Supers,Subs,Disj,Key) :-
	t5tbox_next_key(Key),
	t5tbox_conc_hc(HC),
	t5conc_new_def(NF,Filter,Conc), /* ??*/
	t5hc_insert(HC,Key,Supers,Subs,Disj),
	t5cdb_store(Key,Conc).		/* ?? */

t5concid_init_top(Topkey) :-
        t5conc_init_top(Topkey,Top),
        t5cdb_store(Topkey,Top).

t5concid_init_number(P,NP,Conc) :-
        b5nf_create_top_number(P,NP,NumNF),
        b5nf_store(NumNF,[],Conc,_).

t5concid_init_string(P,NP,Conc) :-
        b5nf_create_top_string(P,NP,StrNF),
        b5nf_store(StrNF,[],Conc,_).

t5concid_init_concept(P,NP,Conc) :-
        b5nf_create_top_concept(P,NP,ConcNF),
        b5nf_store(ConcNF,[],Conc,_).

t5concid_init_aset(P,NP,Conc) :-
        b5nf_create_top_aset(P,NP,AsetNF),
        b5nf_store(AsetNF,[],Conc,_).

t5concid_init_bottom(Key) :-
        t5conc_init_bottom(Key,N_Conc),
%	t5conc_new_prim(Conc,Key),
%	t5conc_add_max_card(Conc,0,N_Conc),
	t5cdb_store(Key,N_Conc).

t5concid_unify2(Conc1,Conc2,Conc3) :-
	t5tbox_conc_hc(HC),
	t5hc_infimum(HC,Conc1,Conc2,Result),
	(Result == unknown, 
	b5nf_create(Conc1,NF1),
	b5nf_add_conc(NF1,Conc2,NF2),
	t5fil_filter(internal,Filter),
	b5nf_store2(NF2,Filter,Conc3,_),
	t5hc_insert_infimum(HC,[Conc1,Conc2],Conc3);
	Conc3 = Result),!.

t5concid_unify(Conc,Conc,Conc) :- !.
t5concid_unify(Conc1,Conc2,Conc2) :-
	var(Conc1),!.

t5concid_unify(Conc1,Conc2,Conc1) :-
	var(Conc2),!.

t5concid_unify(Conc1,Conc2,Conc3) :-
	t5tbox_conc_hc(HC),
	t5hc_infimum(HC,Conc1,Conc2,Result),
	(	
		Result == unknown, 
		b5nf_create(Conc1,NF1),
		b5nf_add_conc(NF1,Conc2,NF2),
		t5fil_filter(internal,Filter),
		b5nf_store(silent,NF2,Filter,Conc3,_), 
		t5hc_insert_infimum(HC,[Conc1,Conc2],Conc3)
	;
		Conc3 = Result
	),!.


t5concid_unify(Conc,Conc,Filter,Conc) :- 
	!,
	t5concid_add_filter(Conc,Filter).

t5concid_unify(Conc1,Conc2,Filter,Conc2) :-
	var(Conc1),!,
	t5concid_add_filter(Conc2,Filter).

t5concid_unify(Conc1,Conc2,Filter,Conc1) :-
	var(Conc2),!,
	t5concid_add_filter(Conc1,Filter).

t5concid_unify(Conc1,Conc2,Filter,Conc3) :-
	t5tbox_conc_hc(HC),
	t5hc_infimum(HC,Conc1,Conc2,Result),
	(	
		Result == unknown, 
		b5nf_create(Conc1,NF1),
		b5nf_add_conc(NF1,Conc2,NF2),
		b5nf_store(NF2,Filter,Conc3,_), 
		t5hc_insert_infimum(HC,[Conc1,Conc2],Conc3)
	;
		Conc3 = Result,
		t5concid_add_filter(Conc3,Filter)

	),!.

t5concid_unify_max(C1,C2,C3,Max) :-
	t5concid_unify(C1,C2,C3),
	t5concid_max(C3,Max).

t5concid_normal_form(Conc,CNF) :-
	t5cdb_cnf(Conc,CNF),
	!.

t5concid_compare(Sub1,Key1,Key2,Sub3) :- 
	t5tbox_conc_hc(HC),
	t5hc_subsumption(HC,Key1,Key2,Sub2),
	t5sub_unify(Sub1,Sub2,Sub3),
	!.

t5concid_incoherent_p(Nothing) :-
	t5tbox_bottom_key(conc,Nothing),
	!.

t5concid_remove(Key) :- 
	t5cdb_remove(Key),!.

t5concid_disjoint_p(Key,Key) :- !,fail.
t5concid_disjoint_p(Key1,Key2) :-
	t5tbox_conc_hc(HC),
	t5hc_disjoint(HC,Key1,Key2,X),
	t5concid_disjoint_p(X,HC,Key1,Key2).

t5concid_disjoint_p(yes,_,_,_).
t5concid_disjoint_p(unknown,HC,Key1,Key2) :-
	t5cdb_get(Key1,Conc1),
	t5cdb_get(Key2,Conc2),
	(t5conc_disjoint_p(Conc1,Conc2) ->
		t5hc_disjoint_info(HC,Key1,Key2,yes)
	;
		t5hc_disjoint_info(HC,Key1,Key2,no),!,fail
	).
%t5concid_disjoint_p(no,_,_,_) :- fail.
/*
t5concid_disjoint_p(yes,_,_,_).
t5concid_disjoint_p(unknown,HC,Key1,Key2) :-
	t5concid_unify2(Key1,Key2,Nothing),!,
	(t5tbox_nothing_key(Nothing) -> X = yes;X=no),
	t5hc_disjoint_info(HC,Key1,Key2,X),
	(X == yes -> true;fail).
%t5concid_disjoint_p(no,_,_,_) :- fail.
*/

t5concid_filter_holds_p(Key,Filter) :- 
	(
	t5cdb_get(Key,_Conc,Fil) ->
		t5fil_holds_p(Filter,Fil)
	;
	t5out_panic_output(noconcept(t5concid_filter_holds_p,Key))
	).

t5concid_filter(Key,Filter) :- 
	(t5cdb_get(Key,_Conc,Filter) -> true
	;
	t5out_panic_output(noconcept(t5concid_filter,Key))
	).

t5concid_changed_status(Old,Old) :- !.
% OLD = NEW nothing to change
t5concid_changed_status(Old,New) :- 
	t5fil_filter(revised,Filter),
	t5concid_add_filter(Old,Filter),
	t5concid_del_filter(New,Filter).              %% jt 09.02.93

t5concid_atomize(ConcID,AtomS) :- 
	t5concid_normal_form(ConcID,NF),
	b5nf_atomize(NF,AtomS).

t5concid_include_role(Key,Role,Supers,Subs) :-
	t5rl_filter(Supers,LessSupers),
	t5rl_filter(Subs,LessSubs),
	t5cdb_get(Key,Conc),
	t5conc_include_role(Conc,Role,LessSupers,LessSubs,N_Conc),
	(
	 	Conc == N_Conc -> true
			; t5cdb_overstore(Key,N_Conc)
	).

t5concid_used_objects(Id,Objs) :-
	t5cdb_get(Id,Conc),
	t5conc_used_objects(Conc,Objs).




%% t5concid_compute_prim_subs(+CandConcs, +PrimsComponents, -PrimSubsumers)
%%  Returns PrimSubsumers(PrimsComponents) = 
%%  		{ C | C is in CandConcs &
%%			prim-comps(C) is subset of PrimComponents }
/*
t5concid_compute_prim_subs([],_,[]).
t5concid_compute_prim_subs([C|Cs], Prims, PrimSubs) :-
	(t5concid_primsubsumer_p(Prims,C) ->
	     PrimSubs = [C|PrimSubs1]
	;    PrimSubs = PrimSubs1),
	t5concid_compute_prim_subs(Cs, Prims, PrimSubs1).
*/


%% t5concid_compute_prim_subs(+CandConcs, +PrimComponents, 
%%			+NegativePrimComponents, -PrimSubsumers)
%%  Returns PrimSubsumers(and(PrimComponents,NegativePrimComponents)) = 
%%  	{ C | C is in CandConcs &
%%		prim-comps(C) is subset of PrimComponents &
%%		neg-prim-comps(C) is subset of NegativePrimComponents }

t5concid_compute_prim_subs([],_,_,[]).
t5concid_compute_prim_subs([C|Cs], Prims, NegPrims, PrimSubs) :-
	((t5concid_primsubsumer_p(Prims,C),
	  t5concid_negprimsubsumer_p(NegPrims,C)) ->
	     PrimSubs = [C|PrimSubs1]
	;    PrimSubs = PrimSubs1),
	t5concid_compute_prim_subs(Cs, Prims, NegPrims, PrimSubs1).

t5concid_primsubsumer_p(Prims,Key) :-
	t5concid_prims(Key,Key_Prims),
	b5sort_subset_p(Key_Prims,Prims).
%%	\+ t5concid_primitive_p(Key).
%%   This test isn't necessary, because in b5nf_compute_prim_subsumers 
%%   anything's subconcepts form the initial set which then contains
%%   no primitive-components.


t5concid_negprimsubsumer_p(NegPrims,Key) :-
	t5concid_neg_prims(Key,KeyNegPrims),
	b5sort_subset_p(KeyNegPrims,NegPrims).
%%	\+ t5concid_primitive_p(Key).
%%   This test isn't necessary, because in b5nf_compute_prim_subsumers 
%%   anything's subconcepts form the initial set which then contains
%%   no primitive-components.



% infimum
%t5concid_infimum(+Keylist,-Key)
t5concid_infimum([],none).   % okp
t5concid_infimum([Key],Key) :- !.  % -okp-
t5concid_infimum([K|KeyList],Key) :-
	b5nf_infimum_key([K|KeyList],Key).

t5cpf_create(CNF,Supers,CNF) :-
	b5nf_xs(CNF,Supers).

t5cpf_cnf(CNF,CNF).
t5cpf_supers(CNF,Supers) :- 
	b5nf_explicit_supers(CNF,Supers).

t5cpf_s_cnf(NF,X,NNF) :-  
	b5nf_explicit_supers(NF,S),
	b5nf_raw_create(A,P,NP,RL,Ext,MM,_,X),
	b5nf_raw_create(A,P,NP,RL,Ext,MM,S,NNF).

t5cpf_s_supers(NF,Supers,NNF) :-
	b5nf_s_xs(NF,Supers,NNF).

t5cpf_r_cnf(NF,O_CNF,N_CNF,N_NF) :-
	b5nf_raw_create(A,P,NP,RL,Ext,MM,S,NF),
	b5nf_raw_create(A,P,NP,RL,Ext,MM,_,O_CNF),
	b5nf_raw_create(A,P,NP,RL,Ext,MM,_,N_CNF),
	b5nf_raw_create(A,P,NP,RL,Ext,MM,S,N_NF).

t5cpf_r_supers(NF,O_Supers,N_Supers,NNF) :-
	b5nf_r_xs(NF,O_Supers,N_Supers,NNF).

t5cpf_new(APF) :-
        t5cpf_create(APF).

t5cpf_create_(CPF) :-
        b5nf_create(CPF).

t5cpf_create_cpf(Domain,CPF) :-
        b5nf_create(Domain,CPF).

t5cpf_create_cpf(CPF) :-
        b5nf_create(CPF).

t5cpf_create_opf(OPF) :-
        b5nf_create_onf(OPF).

t5cpf_add_prim(CPF,Prim,N_CPF) :-
        b5nf_add_prim(CPF,Prim,N_CPF).

t5cpf_add_neg_prim(CPF,NegPrim,N_CPF) :-
        b5nf_add_neg_prim(CPF,NegPrim,N_CPF).

t5cpf_add_conc(CPF,Key,N_CPF) :-
	b5nf_add_conc(CPF,Key,N_CPF).

t5cpf_add_atleast(CPF1,Min,Role,CPF3) :-
	b5nf_add_atleast(CPF1,Min,Role,CPF3).

t5cpf_add_atmost(CPF1,Max,Role,CPF2) :- 
        b5nf_add_atmost(CPF1,Max,Role,CPF2).

t5cpf_add_vr(CPF1,VR,Role,CPF2) :-
        b5nf_add_vr(CPF1,VR,Role,CPF2).

t5cpf_store(CPF,Filter,Key,OldOrNew) :-
	b5nf_store(CPF,Filter,Key,OldOrNew).

t5cpf_store2(CPF,Filter,Key,OldOrNew) :-
	b5nf_store2(CPF,Filter,Key,OldOrNew).


/* NEU */

t5cpf_add_min_card(CPF,Min,N_CPF) :- 
        b5nf_add_min_card(CPF,Min,N_CPF).
        /* warnung bei redundanter erweiterung, sowie cardinalit"at
        der extension ber"ucksichtigen */
          

t5cpf_add_max_card(CPF,Max,N_CPF) :-
        b5nf_add_max_card(CPF,Max,N_CPF).
        /* s.o */


t5cpf_add_minmax(CPF,MinMax,N_CPF) :-
        b5nf_add_minmax(CPF,MinMax,N_CPF).

t5cpf_add_minmax(CPF,Min,Max,N_CPF) :-
	b5nf_add_minmax(CPF,Min,Max,N_CPF).

t5cpf_add_aset(CPF,Keys,N_CPF) :-
        b5nf_add_aset(CPF,Keys,N_CPF).
        
t5cpf_add_union(NF,Key1,Key2,N_NF) :-
	b5nf_add_union(NF,Key1,Key2,N_NF). 

t5cpf_add_without(NF,Key1,Key2,N_NF) :-
	b5nf_add_without(NF,Key1,Key2,N_NF). 

t5cpf_add_oneof(NF,Keys,N_NF) :-
	b5nf_add_oneof(NF,Keys,N_NF). 

t5cpf_add_gt(NF,Num,N_NF) :-
	b5nf_add_gt(NF,Num,N_NF). 

t5cpf_add_ge(NF,Num,N_NF) :-
	b5nf_add_ge(NF,Num,N_NF). 

t5cpf_add_lt(NF,Num,N_NF) :-
	b5nf_add_lt(NF,Num,N_NF). 

t5cpf_add_le(NF,Num,N_NF) :-
	b5nf_add_le(NF,Num,N_NF). 

t5cpf_add_num(NF,Num,N_NF) :-
	b5nf_add_num(NF,Num,N_NF). 

t5cpf_add_fromto(NF,From,To,N_NF) :-
	b5nf_add_fromto(NF,From,To,N_NF). 

t5cpf_add_extension(CPF,Exts,N_CPF) :-
        b5nf_add_extension(CPF,Exts,N_CPF).

t5cpf_add_fillers(CPF,Fillers,Role,N_CPF) :-
        b5nf_add_fillers(CPF,Fillers,Role,N_CPF).

t5cpf_add_equals(CPF,Eqs,Role,N_CPF) :- 
        b5nf_add_equals(CPF,Eqs,Role,N_CPF).

t5cpf_add_supers(CPF,Eqs,Role,N_CPF) :- 
        b5nf_add_supers(CPF,Eqs,Role,N_CPF).

t5cpf_add_subs(CPF,Eqs,Role,N_CPF) :- 
        b5nf_add_subs(CPF,Eqs,Role,N_CPF).

t5cpf_reslist(CPF, ResList) :-
	b5nf_reslist(CPF,ResList).

t5cpf_unify(CPF1,CPF2,CPF) :-
	b5nf_unify(CPF1,CPF2,CPF).


t5cpf_add_concs(CPF, Keys, NewCPF) :- 
	b5nf_add_concs(Keys, CPF, NewCPF).

t5cpf_splitting(CPF,LIC /* Keys*/,NCPF,AD,Flag) :-
	b5nf_splitting(CPF,LIC, NCPF,AD,Flag). 

t5cpf_modified_p(CPF1,CPF2) :-
	b5nf_modified_p(CPF1,CPF2).

t5cpf_complete(CPF0, CPF) :-
	b5nf_complete(CPF0,CPF).

t5cpf_atomize(CPF,Atoms) :-
	b5nf_atomize(CPF,Atoms).
/*------------------------------------------------------------ */ 
/*			concept data base                      */
/*------------------------------------------------------------ */ 

:- multifile b5dump_these_predicates/1.
b5dump_these_predicates([t5cdb/2]).

t5cdb_init :-
	retractall(t5cdb(_,_)),
	!.


t5cdb_ibox_init([]) :- !.
t5cdb_ibox_init([Key|Keys]) :-
	t5concid_filter_holds_p(Key,ibox_lhs),!,
	retract(t5cdb(Key,Conc)),
	t5conc_r_filter(Conc,Filter,N_Filter,N_Conc),
	t5fil_remove(ibox_lhs,Filter,N_Filter),
	assert(t5cdb(Key,N_Conc)),
	t5cdb_ibox_init(Keys).
t5cdb_ibox_init([_|Keys]) :-
	t5cdb_ibox_init(Keys).

t5cdb_ibox_init :-
	findall(Key,t5cdb(Key,_),Keys),!,
	t5cdb_ibox_init(Keys).

t5cdb_ibox_init.

t5cdb_remove(Key) :-
	retract(t5cdb(Key,_)).

t5cdb_store(Key,Conc) :-
        assert(t5cdb(Key,Conc)).

t5cdb_get(Key,Con) :-
	t5cdb(Key,Con).

t5cdb_get(Key,Conc,Filter) :-
        t5cdb(Key,Conc),
	t5conc_filter(Conc,Filter).

t5cdb_cnf(Key,CNF) :-  /* 8-tung ohne filter !!!!! */
        t5cdb_get(Key,Conc,_),
        t5conc_nf(Conc,CNF).
/*
t5cdb_overstore(Key,NConc) :-
	retract(t5cdb(Key,_)),
	t5cdb_store(Key,NConc).
*/

t5cdb_overstore(Key,NConc) :-
	t5cdb_get(Key,Old),
	(Old == NConc -> true 
		;
		retract(t5cdb(Key,_)),
		t5cdb_store(Key,NConc)
        ).

t5cdb_store(Key,Conc,Filter) :-
	t5conc_r_filter(Conc,_OldFilter,Filter,NewConc),
        assert(t5cdb(Key,NewConc)).

t5cdb_store_new_filter(Key,Filter) :-
        t5cdb(Key,Conc),
	retract(t5cdb(Key,_)),
	t5conc_r_filter(Conc,OldFilter,NewFilter,NewConc),
	t5fil_unify(Filter,OldFilter,NewFilter),
	t5cdb_store(Key,NewConc).    
	
t5cdb_include_role(Role,Supers,Subs) :-
	t5rl_filter(Supers,LessSupers),
	t5rl_filter(Subs,LessSubs),
	t5cdb_incl_role(Role,LessSupers,LessSubs).
%	t5cdb_incl_role(Role,Supers,Subs).

t5cdb_incl_role(Role,Supers,Subs) :-
	t5cdb_get(Key,Conc),
	t5conc_include_role(Conc,Role,Supers,Subs,N_Conc),  %->? 
		(
			Conc == N_Conc -> true
			; t5cdb_overstore(Key,N_Conc)
		),fail.
	
t5cdb_incl_role(_,_,_).


t5cdb_used_objects(AllObjs) :-
	setof(Objs,Conc^t5concid_used_objects(Conc,Objs),ListsOfObjs),
	flatten(ListsOfObjs, Objs),
	sort(Objs, AllObjs), !.
t5cdb_used_objects([]).

t5cdb_used_objects_and_values(Objects,Values) :-
	t5cdb_used_objects(All),
	t5cdb_split(All,Objects,Values).

t5cdb_split([],[],[]) :- !.

t5cdb_split([E|Es],[E|Objects],Values) :-
	a5odb_get(E,_),!,
	t5cdb_split(Es,Objects,Values).

t5cdb_split([E|Es],Objects,[E|Values]) :-
	b5inst_get(E,_),
	t5cdb_split(Es,Objects,Values).

t5cdb_primitives(Primitives) :-
	setof(X,t5concid_primitive_p(X),[_Top,_Bottom|Primitives]).
					 /* hack */
/* ------------------------------------------------------------ */ 
/*		      Role restrictions				*/
/* ------------------------------------------------------------ */ 

/* RoleRes=R-res(VR,NR,Fs,rvm,State-DCS) */
/*
        State e {un,cl,op,icl,ffs,inc}
        unknown,closed,open,indirctly closed,ffs ::= icl or op,
	inconsistent
*/


/* raw_create : R,VR,NR,Fs,Inv,Eqs,Sups,Subs,State -> RoleRes */

t5res_raw_create(R,VR,NR,Fs,Eq,Sups,Subs,State,RES) :- 
	       t5res_raw_create(R,VR,NR,Fs,RVM,State,RES),
	       t5rvm_raw_create(Eq,Sups,Subs,RVM).

t5res_raw_create(R,VR,NR,Fs,RVM,State,DCS,R-res(VR,NR,Fs,RVM,State-DCS)).
t5res_raw_create(R,VR,NR,Fs,RVM,State,R-res(VR,NR,Fs,RVM,State)).


t5res_role(    	 R-_,R).
t5res_vr(       _R-res(VR,_,_,_,_),VR).
t5res_nr(       _R-res(_,NR,_,_,_),NR).
t5res_fillers(  _R-res(_,_,Fi,_,_),Fi).
t5res_rvm(      _R-res(_,_,_,RVM,_),RVM).
t5res_sdcs(     _R-res(_,_,_,_,X),X).
t5res_state(    _R-res(_,_,_,_,State-_),State).
t5res_dcs(      _R-res(_,_,_,_,_-DCS),DCS).

t5res_vr_min_max(_-res(VR,NR,_,_,_),VR,Min,Max) :-
	t5nr_minmax(NR,Min,Max).

t5res_equals(Res,Eq) :- 
       t5res_rvm(Res,RVM),
       t5rvm_equals(RVM,Eq).

t5res_supers(Res,Supers) :- 
	t5res_rvm(Res,RVM), 
t5rvm_supers(RVM,Supers).

t5res_subs(Res,Subs) :-
       t5res_rvm(Res,RVM),
       t5rvm_subs(RVM,Subs).

t5res_min(Res,Min) :- t5res_nr(Res,NR),t5nr_min(NR,Min).
t5res_max(Res,Max) :- t5res_nr(Res,NR),t5nr_max(NR,Max).


t5res_s_r(_X-Y,R,R-Y).
t5res_s_vr(R-res(_,NR,Fs,RVM,S),VR,R-res(VR,NR,Fs,RVM,S)).
t5res_s_nr(R-res(VR,_,Fs,RVM,S),NR,R-res(VR,NR,Fs,RVM,S)).
t5res_s_fillers(R-res(VR,NR,_,RVM,S),Fs,R-res(VR,NR,Fs,RVM,S)).
t5res_s_rvm(R-res(VR,NR,Fs,_,S),RVM,R-res(VR,NR,Fs,RVM,S)).
t5res_s_sdcs(R-res(VR,NR,Fs,RVM,_SD),SD,R-res(VR,NR,Fs,RVM,SD)).
t5res_s_state(R-res(VR,NR,Fs,RVM,_-X),S,R-res(VR,NR,Fs,RVM,S-X)).
t5res_s_dcs(R-res(VR,NR,Fs,RVM,S-_),DCS,R-res(VR,NR,Fs,RVM,S-DCS)).

t5res_s_equals(RES,EQ,NRES) :- 
        t5res_r_rvm(RES,RVM,N_RVM,NRES),
        t5rvm_s_equals(RVM,EQ,N_RVM).

t5res_s_supers(RES,Supers,N_RES) :-
        t5res_r_rvm(RES,RVM,N_RVM,N_RES),
        t5rvm_s_supers(RVM,Supers,N_RVM).

t5res_s_subs(RES,Subs,N_RES) :-
        t5res_r_rvm(RES,RVM,N_RVM,N_RES),
        t5rvm_s_subs(RVM,Subs,N_RVM).

t5res_s_min(R-res(VR,NR,Fs,RVM,S),Min,R-res(VR,N_NR,Fs,RVM,S)) :-
        t5nr_s_min(NR,Min,N_NR).

t5res_s_max(R-res(VR,NR,Fs,RVM,S),Max,R-res(VR,N_NR,Fs,RVM,S)) :-
        t5nr_s_max(NR,Max,N_NR).


t5res_r_r(R-RES,R,N_R,N_R-RES).
t5res_r_vr(R-res(VR,NR,Fs,RVM,S),VR,N_VR,R-res(N_VR,NR,Fs,RVM,S)).
t5res_r_nr(R-res(VR,NR,Fs,RVM,S),NR,N_NR,R-res(VR,N_NR,Fs,RVM,S)).
t5res_r_fillers(R-res(VR,NR,Fs,RVM,S),Fs,N_Fs,R-res(VR,NR,N_Fs,RVM,S)).
t5res_r_rvm(R-res(VR,NR,Fs,RVM,S),RVM,N_RVM,R-res(VR,NR,Fs,N_RVM,S)).
t5res_r_sdcs(R-res(VR,NR,Fs,RVM,S),S,N_S,R-res(VR,NR,Fs,RVM,N_S)).
t5res_r_state(R-res(VR,NR,Fs,RVM,S-X),S,N_S,R-res(VR,NR,Fs,RVM,N_S-X)).
t5res_r_dcs(R-res(VR,NR,Fs,RVM,S-X),X,N_X,R-res(VR,NR,Fs,RVM,S-N_X)).

t5res_r_equals(RES,Eq,N_Eq,N_RES) :-
        t5res_r_rvm(RES,RVM,N_RVM,N_RES),
        t5rvm_r_equals(RVM,Eq,N_Eq,N_RVM).

t5res_r_supers(RES,Supers,N_Supers,N_RES):-
        t5res_r_rvm(RES,RVM,N_RVM,N_RES),
        t5rvm_r_supers(RVM,Supers,N_Supers,N_RVM).

t5res_r_subs(RES,Subs,N_Subs,N_RES) :-
        t5res_r_rvm(RES,RVM,N_RVM,N_RES),
        t5rvm_r_subs(RVM,Subs,N_Subs,N_RVM).

t5res_r_min(R-res(VR,NR,Fs,RVM,S),Min,N_Min,R-res(VR,N_NR,Fs,RVM,S)) :- 
        t5nr_r_min(NR,Min,N_Min,N_NR).

t5res_r_max(R-res(VR,NR,Fs,RVM,S),Max,N_Max,R-res(VR,N_NR,Fs,RVM,S)) :- 
        t5nr_r_max(NR,Max,N_Max,N_NR).

t5res_r_vnr(R-res(VR,NR,Fs,RVM,S),VR,NR,N_VR,N_NR,R-res(N_VR,N_NR,Fs,RVM,S)).

t5res_ss_r(_-res(VR,NR,Fs,RVM,_),R,R-res(VR,NR,Fs,RVM,un-[])).
t5res_ss_vr(R-res(_,NR,Fs,RVM,_),VR,R-res(VR,NR,Fs,RVM,un-[])).
t5res_ss_nr(R-res(VR,_,Fs,RVM,_),NR,R-res(VR,NR,Fs,RVM,un-[])).
t5res_ss_fillers(R-res(VR,NR,_,RVM,_),Fs,R-res(VR,NR,Fs,RVM,un-[])).
t5res_ss_rvm(R-res(VR,NR,Fs,_,_),RVM,R-res(VR,NR,Fs,RVM,un-[])).
t5res_ss_sdcs(R-res(VR,NR,Fs,RVM,_SD),SD,R-res(VR,NR,Fs,RVM,SD)).
t5res_ss_state(R-res(VR,NR,Fs,RVM,_-X),S,R-res(VR,NR,Fs,RVM,S-X)).
t5res_ss_dcs(R-res(VR,NR,Fs,RVM,S-_),DCS,R-res(VR,NR,Fs,RVM,S-DCS)).

t5res_ss_equals(RES,EQ,NRES) :- 
        t5res_rr_rvm(RES,RVM,N_RVM,NRES),
        t5rvm_s_equals(RVM,EQ,N_RVM).

t5res_ss_supers(RES,Supers,N_RES) :-
        t5res_rr_rvm(RES,RVM,N_RVM,N_RES),
        t5rvm_s_supers(RVM,Supers,N_RVM).

t5res_ss_subs(RES,Subs,N_RES) :-
        t5res_rr_rvm(RES,RVM,N_RVM,N_RES),
        t5rvm_s_subs(RVM,Subs,N_RVM).

t5res_ss_min(R-res(VR,NR,Fs,RVM,_),Min,R-res(VR,N_NR,Fs,RVM,un-[])) :-
        t5nr_s_min(NR,Min,N_NR).

t5res_ss_max(R-res(VR,NR,Fs,RVM,_),Max,R-res(VR,N_NR,Fs,RVM,un-[])) :-
        t5nr_s_max(NR,Max,N_NR).

t5res_rr_r(R-res(VR,NR,Fs,RVM,_),R,N_R,N_R-res(VR,NR,Fs,RVM,un-[])).
t5res_rr_vr(R-res(VR,NR,Fs,RVM,_),VR,N_VR,R-res(N_VR,NR,Fs,RVM,un-[])).
t5res_rr_nr(R-res(VR,NR,Fs,RVM,_),NR,N_NR,R-res(VR,N_NR,Fs,RVM,un-[])).
t5res_rr_fillers(R-res(VR,NR,Fs,RVM,_),Fs,N_Fs,R-res(VR,NR,N_Fs,RVM,un-[])).
t5res_rr_rvm(R-res(VR,NR,Fs,RVM,_),RVM,N_RVM,R-res(VR,NR,Fs,N_RVM,un-[])).
t5res_rr_sdcs(R-res(VR,NR,Fs,RVM,S),S,N_S,R-res(VR,NR,Fs,RVM,N_S)).
t5res_rr_state(R-res(VR,NR,Fs,RVM,S-X),S,N_S,R-res(VR,NR,Fs,RVM,N_S-X)).
t5res_rr_dcs(R-res(VR,NR,Fs,RVM,S-X),X,N_X,R-res(VR,NR,Fs,RVM,S-N_X)).

t5res_rr_equals(RES,Eq,N_Eq,N_RES) :-
        t5res_rr_rvm(RES,RVM,N_RVM,N_RES),
        t5rvm_r_equals(RVM,Eq,N_Eq,N_RVM).

t5res_rr_supers(RES,Supers,N_Supers,N_RES):-
        t5res_rr_rvm(RES,RVM,N_RVM,N_RES),
        t5rvm_r_supers(RVM,Supers,N_Supers,N_RVM).

t5res_rr_subs(RES,Subs,N_Subs,N_RES) :-
        t5res_rr_rvm(RES,RVM,N_RVM,N_RES),
        t5rvm_r_subs(RVM,Subs,N_Subs,N_RVM).

t5res_rr_min(R-res(VR,NR,Fs,RVM,_),Min,N_Min,R-res(VR,N_NR,Fs,RVM,un-[])) :- 
        t5nr_r_min(NR,Min,N_Min,N_NR).

t5res_rr_max(R-res(VR,NR,Fs,RVM,_),Max,N_Max,R-res(VR,N_NR,Fs,RVM,un-[])) :- 
        t5nr_r_max(NR,Max,N_Max,N_NR).

t5res_rr_vnr(R-res(VR,NR,Fs,RVM,_),VR,NR,N_VR,N_NR,R-res(N_VR,N_NR,Fs,RVM,un-[])).

t5res_role_range_nr(R,VR,NR) :-
        t5role_range(R,VR),
        t5concid_max(VR,Max),
	(t5role_defined_p(R) -> t5nr_create(NR1)
		;  (t5role_feature_p(R) -> t5nr_f_create(NR1)
			;t5nr_a_create(NR1))),
        t5nr_add_max(NR1,Max,NR).

t5res_create(R, RoleRes) :- 
        t5res_role_range_nr(R,VR,NR),
        t5rvm_create(RVM),
        t5res_raw_create(R,VR,NR,[],RVM,un,[],RoleRes).

t5res_create(R-ES) :- 
        t5res_role_range_nr(R,VR,NR),
        t5rvm_create(RVM),
        t5res_raw_create(R,VR,NR,[],RVM,un,[],R-ES).

t5res_modified_p(Res1,Res2) :-
	t5res_raw_create(R,VR,NR,Fs,RVM,_,Res1),
	t5res_raw_create(R,VR,NR,Fs,RVM,_,Res2),!,fail.

t5res_modified_p(_,_) :- !.

t5res_add_vr(RoleRes,VR,FinRoleRes) :-
        t5res_rr_vnr(RoleRes,Old_VR,Old_NR,Fin_VR,Fin_NR,FinRoleRes), 
        t5concid_unify_max(VR,Old_VR,Fin_VR,Max),
        t5nr_add_max(Old_NR,Max,Fin_NR). 

t5res_add_min(Res,Min,N_Res) :- 
        t5res_rr_nr(Res,NR,N_NR,N_Res),
        t5nr_add_min(NR,Min,N_NR).

t5res_add_max(Res,Max,N_Res) :-
        t5res_rr_vnr(Res,VR,NR,N_VR,N_NR,N_Res),
        t5nr_add_max(NR,Max,N_NR),
        (t5nr_max(N_NR,0) -> t5tbox_nothing_key(N_VR)
        ; N_VR = VR).

t5res_add_nr_res(Res,Num,N_Res) :-
	t5concid_number(Num,Number),
	t5number_nr_minmax(Number,Min,Max),
	t5res_add_min(Res,Min,N_Res1),
	t5res_add_max(N_Res1,Max,N_Res).


t5res_add_fillers(RoleRes,Objs-New,N_RoleRes):-
	t5res_raw_create(R,VR,NR,Fs,RVM,_,RoleRes),
	t5res_raw_create(R,VR,N_NR,N_Fs,RVM,un-[],N_RoleRes),
        b5sort_unify(Fs,Objs,N_Fs,New),
	b5sort_card(N_Fs,Card),
	t5nr_add_min(NR,Card,N_NR).

t5res_add_closed_fillers(RoleRes,Objs-New,N_RoleRes):-
	t5res_raw_create(R,VR,NR,Fs,RVM,_,RoleRes),
	t5res_raw_create(R,VR,F_NR,N_Fs,RVM,cl-N_Fs,N_RoleRes),
        b5sort_unify(Fs,Objs,N_Fs,New),
	b5sort_card(Objs,Max),
	b5sort_card(N_Fs,Min),
	t5nr_add_min(NR,Min,N_NR),
	t5nr_add_max(N_NR,Max,F_NR).

t5res_close_role_filler_set(Res,N_Res) :-
	t5res_raw_create(R,VR,NR,Fs,RVM,_,Res),
	t5res_raw_create(R,VR,N_NR,Fs,RVM,cl-Fs,N_Res),
	b5sort_card(Fs,Card),
	t5nr_add_max(NR,Card,N_NR).
 			
t5res_add_rvm(RoleRes,RVM,N_RoleRes) :-
        t5res_rr_rvm(RoleRes,O_RVM,N_RVM,N_RoleRes),
        t5rvm_unify(O_RVM,RVM,N_RVM).

t5res_add_rvm_equal(RoleRes,Role,FinRoleRes) :- 
	t5res_role(RoleRes,MyRole),
	b5sort_unify([Role],[MyRole],Roles),
        t5res_rr_equals(RoleRes,OldEqs,NewEqs,FinRoleRes),
        b5sort_unify(OldEqs,Roles,NewEqs).

t5res_add_rvm_super(RoleRes,Role,FinRoleRes) :- 
        t5res_rr_supers(RoleRes,OldSups,NewSups,FinRoleRes),
        b5sort_unify(OldSups,[Role],NewSups).

t5res_add_rvm_sub(RoleRes,Role,FinRoleRes ) :-
        t5res_rr_subs(RoleRes,OldSubs,NewSubs,FinRoleRes),
        b5sort_unify(OldSubs,[Role],NewSubs).

t5res_add_rvm_equals(RoleRes,Roles,FinRoleRes) :- 
	t5res_role(RoleRes,MyRole),
	b5sort_unify(Roles,[MyRole],Roles1),
        t5res_rr_equals(RoleRes,OldEqs,NewEqs,FinRoleRes),
        b5sort_unify(OldEqs,Roles1,NewEqs).

t5res_add_rvm_supers(RoleRes,Roles,FinRoleRes) :- 
        t5res_rr_supers(RoleRes,OldSups,NewSups,FinRoleRes),
        b5sort_unify(OldSups,Roles,NewSups).

t5res_add_rvm_subs(RoleRes,Roles,FinRoleRes ) :-
        t5res_rr_subs(RoleRes,OldSubs,NewSubs,FinRoleRes),
        b5sort_unify(OldSubs,Roles,NewSubs).

t5res_generate(R,VR,Min,Max,Fillers,Equals,Supers,Subs,RoleRes) :-
        var(RoleRes),!,
        t5res_create(R,R_Res1),
        t5res_add_vr(R_Res1,VR,R_Res2)  , 
        t5res_add_min(R_Res2,Min,R_Res3),
        t5res_add_max(R_Res3,Max,R_Res4),
        t5concid_max(VR ,Card),
        t5res_add_max(R_Res4,Card,R_Res5),
        (var(Fillers) -> R_Res6 = R_Res5 ;
        ( b5sort_card(Fillers,CardFillers),
        t5res_add_min(R_Res5,CardFillers,R_Res6))),
        t5res_add_fillers(R_Res6,Fillers,R_Res7),
        t5rvm_generate(Equals,Supers,Subs,RVM),
        t5res_s_rvm(R_Res7,RVM,RoleRes),!.

t5res_generate(R,VR,Min,Max,Fillers,Equals,Supers,Subs,RoleRes) :-
        t5res_raw_create(R,VR,NR,Fillers,Equals,Supers,Subs,_S,RoleRes),
        t5nr_minmax(NR,Min,Max),!.

t5res_gen_r_vr_max(R,VR1,Max1,Res) :-
        % same as t5res_generate(R,VR1,_,Max1,_,_,_,_,Res) but faster
        var(Res),!,
        t5res_role_range_nr(R,Range,NR1),
        t5nr_add_max(NR1,Max1,NR2),
        t5concid_unify_max(Range,VR1,VR2,Max2),
        t5nr_add_max(NR2,Max2,NR3),
        t5rvm_create(RVM),
        t5res_raw_create(R,VR2,NR3,[],RVM,un,[],Res).

t5res_gen_r_min_fillers(R,Min,Fillers,Res) :-
        % same as t5res_generate(R,_,Min,_,Fillers,_,_,_,Res) but faster 
        t5res_role_range_nr(R,Range,NR1),
        t5nr_add_min(NR1,Min,NR2),
        b5sort_card(Fillers,Card),
        t5nr_add_min(NR2,Card,NR3),
        t5rvm_create(RVM),
        t5res_raw_create(R,Range,NR3,Fillers,RVM,un,[],Res).        

t5res_generate(R,VR,Min,Max,Fillers,RES) :-
        t5res_raw_create(R,VR,NR,Fillers,_RVM,_S,RES),
        %t5res_raw_create(R,VR,NR,Fillers,RVM,_S,RES),
        %(t5rvm_empty_p(RVM) -> true; nl,write('rvm prop ?'),nl), 
        t5nr_minmax(NR,Min,Max),!.

t5res_generate(R,VR,Min,Max,Fillers,RVM,RES) :-
        t5res_raw_create(R,VR,NR,Fillers,RVM,_S,RES),
        t5nr_minmax(NR,Min,Max),!.


t5res_unify(RoleRes1,RoleRes2,RoleRes3) :- 
        % only for coherent restriktion
        t5res_raw_create(R,VR1,NR1,Fs1,RVM1,_S1,RoleRes1),
        t5res_raw_create(R,VR2,NR2,Fs2,RVM2,_S2,RoleRes2),
        b5sort_unify(Fs1,Fs2,Fs3),
        t5rvm_unify(RVM1,RVM2,RVM3),
        t5nr_unify(NR1,NR2,NR_t1),
        b5sort_card(Fs3,CardFiller),
        t5nr_add_min(NR_t1,CardFiller,NR_t2),
        t5concid_unify_max(VR1,VR2,VR3,CardVr),
        %t5concid_max(VR3,CardVr),
        t5nr_add_max(NR_t2,CardVr,NR3),
	%t5nr_max(NR3,Max),
	%(CardFiller == Max -> S = cl; S = un),
        t5res_raw_create(R,VR3,NR3,Fs3,RVM3,un,[],RoleRes3).


t5res_unify(RoleRes1,RoleRes2,RoleRes3,N_VR) :-
        t5res_unify(RoleRes1,RoleRes2,RoleRes3),
        t5res_vr(RoleRes1,VR1),
        t5res_vr(RoleRes3,VR3),
        VR1 = VR3 -> N_VR = [] ; N_VR = VR3.


t5res_compare(St0,Res1,Res2,St4) :- 
        t5res_raw_create(R,VR1,NR1,Fs1,RVM1,_S1,Res1),
        t5res_raw_create(R,VR2,NR2,Fs2,RVM2,_S2,Res2), 
        t5concid_compare(St0,VR1,VR2,St1),
        St1 \== none,
        t5nr_compare(St1,NR1,NR2,St2),
        St2 \== none,
        b5sort_compare(St2,Fs1,Fs2,St3),
        St3 \== none,
        t5rvm_compare(St3,RVM1,RVM2,St4),!.

t5res_compare(_,_,_,none).

t5res_subsumes_p(Res1,Res2) :-
         t5res_raw_create(R,VR1,NR1,Fs1,RVM1,_S1,Res1),
         t5res_raw_create(R,VR2,NR2,Fs2,RVM2,_S2,Res2),
         t5concid_subsumes_p(VR1,VR2),
         t5nr_subsumes_p(NR1,NR2),
         b5sort_subset_p(Fs1,Fs2),
         t5rvm_subsumes_p(RVM1,RVM2).

t5res_type_fillers_inco_p([],_) :- !,fail.
t5res_type_fillers_inco_p(Objs,VR) :- 
	t5concid_normal_form(VR,NF),
	b5nf_type(NF,Type),
	(
		b5nf_concept_p(NF),!,fail % stimmt das ???
	;
		\+ t5res_typecheck(Objs,VR,Type)
	).


t5res_typecheck([],_,_). 
t5res_typecheck([Obj|Objs],VR,Type) :-
	b5inst_get_type(Obj,Type1),
	(Type = Type1 -> true;t5out_error_output(type(Obj,Type)),!,fail),
	(b5inst_is_instance_p(Obj,VR) -> true;
		t5out_error_output(f_type(Obj,VR)),!,fail),
	t5res_typecheck(Objs,VR,Type).


t5res_incoherent_p(RoleRes) :- 
        t5res_raw_create(_R,VR,NR,Fs,_RVM,S,_X,RoleRes),
        (
		t5nr_incoherent_p(NR),!
	; 
		(S == un-[] -> t5res_fixstate_h(Fs,NR,State) ; State = S), 
		State = in-_,!
	;	
		t5res_type_fillers_inco_p(Fs,VR),!
	).

t5res_fixstate_h(Fs,NR,State) :-
	b5sort_card(Fs,Card),
	t5nr_max(NR,Max),
	t5point_comp(Max,Card,Order),
	t5res_fixstate_by_order(Order,Max,Fs,State).

t5res_fixstate_by_order(<,_,_,in-[]).  /* inkonsistent 		*/
t5res_fixstate_by_order(>,in,_Fs_,op-[]) :- !. /* system handles only finite
					number of objects*/
t5res_fixstate_by_order(>,_,_Fs_,ffs-[]). /* open oder indirectly closed	*/
t5res_fixstate_by_order(=,_,Fs,cl-Fs).  /* closed 			*/

t5res_ibox_transform(Res,In,Status,N_Res) :- 
        t5res_rr_vnr(Res,Old_VR,Old_NR,Fin_VR,Fin_NR,N_Res), 
        i5ibox_get_ibox_conc(Old_VR,In,Status,Fin_VR),
	t5concid_max(Fin_VR,Max),
	t5nr_add_max(Old_NR,Max,Fin_NR).

/*
t5res_split(Res,I_Res,N_Res) :- 
        t5res_role(Res,Role),
        t5res_role(I_Res,Role),
        t5res_vr(Res,VR),
        t5res_vr(I_Res,I_VR),
        t5concid_subsumes_p(I_VR,VR),
        t5role_range(Role,DefaultRange),
        t5res_s_vr(Res,DefaultRange,N_Res),!.

t5res_split(Res,I_Res,_N_Res) :- 
	t5out_panic(t5res_split(Res,I_Res)),fail.
*/

/*new 20.08.93 */

t5res_split_prem_p(Res,IRes) :-
	t5res_max(Res,0), 
	(t5res_max(IRes,0) -> fail ;true). 

t5res_split(Res,I_Res,F_Res) :-
	t5res_role(Res,Role),
	t5res_role(I_Res,Role),
	t5res_vr(Res,VR),
	t5res_vr(I_Res,I_VR),
	t5concid_subsumes_p(I_VR,VR),
	t5res_role_range_nr(Role,DefaultRange,DefaultNr),
	t5res_s_vr(Res,DefaultRange,N_Res),
	(t5res_split_prem_p(Res,I_Res) ->
		t5nr_max(DefaultNr,Max),
		t5res_s_max(N_Res,Max,F_Res)
	;
	F_Res = N_Res),!.


t5res_split(Res,I_Res,_N_Res) :-
	t5out_panic(t5res_split(Res,I_Res)),fail.


t5res_atomize(RES,res(atmost(Max,R),atleast(Min,R),all(R,VR),Tail)) :-
	t5res_raw_create(R,VR,NR,Fs,_RVM_,_State,RES),
	(b5sort_empty_p(Fs) -> Tail = [] ; Tail = fills(R,Fs)),
	t5nr_min(NR,Min),
	t5nr_max(NR,Max).

/* --------------- 	*/

t5res_unify_without_roles(RoleRes1,RoleRes2,RoleRes3) :- 
        % only for coherent restriktion
        t5res_raw_create(_R1,VR1,NR1,Fs1,RVM1,_S1,RoleRes1),
        t5res_raw_create(_R2,VR2,NR2,Fs2,RVM2,_S2,RoleRes2),
        b5sort_unify(Fs1,Fs2,Fs3),
        t5rvm_unify(RVM1,RVM2,RVM3),
        t5nr_unify(NR1,NR2,NR_t1),
        b5sort_card(Fs3,CardFiller),
        t5nr_add_min(NR_t1,CardFiller,NR_t2),
        t5concid_unify_max(VR1,VR2,VR3,CardVr),
        %t5concid_max(VR3,CardVr),
        t5nr_add_max(NR_t2,CardVr,NR3),
	%t5nr_max(NR3,Max),
	%(CardFiller == Max -> S = cl; S = un),
        t5res_raw_create(_R,VR3,NR3,Fs3,RVM3,un,[],RoleRes3).

t5res_unify_list_without_roles([H|T],Uni) :-
	t5res_list_uni(T,H,Uni).

t5res_list_uni([],X,X) :- !.
t5res_list_uni([H|T],Old,Final) :- 
	t5res_unify_without_roles(H,Old,New),
	t5res_list_uni(T,New,Final).
 


t5res_redundant_p(RoleRes) :-
        t5res_raw_create(Role,VR,NR,[],RVM,_S_,RoleRes),
	t5res_role_range_nr(Role,Range,NR2),
        t5rvm_empty_p(RVM),
	t5nr_subsumes_p(NR,NR2),
        t5concid_subsumes_p(VR,Range).

/*
t5res_redundant_p(RoleRes) :-
        t5res_raw_create(Role,VR,NR,[],RVM,_S_,RoleRes),
        t5rvm_empty_p(RVM),
        (t5role_feature_p(Role) -> t5nr_f_redundant_p(NR) ;
                (t5role_attribute_p(Role) -> t5nr_a_redundant_p(NR) ;
                        (t5nr_redundant_p(NR)))),
        t5role_range(Role,Range),
        t5concid_subsumes_p(VR,Range).

*/

/* ------------------------------------------------------------ */ 
/* 			number restriction			*/
/* ------------------------------------------------------------ */ 


t5point_comp(A,B,C) :- nonvar(A),nonvar(B),compare(C,A,B),!.

t5point_g_min(A,B,A) :- (var(B) ; A @< B),!.
t5point_g_min(_,B,B). 

t5point_g_max(A,B,A) :- (var(B); A @> B),!.
t5point_g_max(_,B,B).

t5point_lt_p(A,B) :- t5point_comp(A,B,<).

t5point_gt_p(A,B) :- t5point_comp(A,B,>).

t5point_eq_p(A,B) :- t5point_comp(A,B,=).

t5point_not_eq_p(A,B) :- t5point_comp(A,B,=),!,fail.

t5point_not_eq_p(_,_).

t5point_le_p(A,B) :- ( t5point_comp(A,B,<),! ; t5point_comp(A,B,=) ).

t5point_ge_p(A,B) :- ( t5point_comp(A,B,>),! ; t5point_comp(A,B,=) ).

% ---------------------------------------------------------------------

t5min_p(X) :- (integer(X),!;X == in).
t5min_create(0).
t5min_f_create(0).
%t5min_a_create(1).  %ffs
t5min_a_create(0).

t5min_generate(A,B) :- (var(A) -> t5min_create(A); A=B).
t5min_f_generate(A,B) :- (var(A) -> t5min_f_create(A); A=B).
t5min_a_generate(A,B) :- 
	t5min_a_create(X),
	(var(A) -> B = X
	;	t5min_unify(A,X,B)
	).
t5min_unify(A,B,A) :-  (var(B); A @> B),!. 
t5min_unify(_,B,B).   

t5min_subsumes_p(A,B) :- 
	 ( t5point_comp(A,B,<),! ; t5point_comp(A,B,=) ).

% ---------------------------------------------------------------------

t5max_p(X) :- (integer(X),!;X == in).
t5max_create(in).
t5max_f_create(1).
t5max_a_create(1).

t5max_generate(A,B) :- (var(A) -> t5max_create(A); A=B).
t5max_f_generate(A,B) :- 
	t5max_f_create(X),
	(var(A) -> B = X
	;	t5max_unify(A,X,B)
	).
		
t5max_a_generate(A,B) :- 
	t5max_a_create(X),
	(var(A) -> B = X
	;	t5max_unify(A,X,B)
	).

t5max_unify(A,B,A) :-  (var(B) ; A @< B),!. 
t5max_unify(_,B,B).   

t5max_subsumes_p(A,B) :- 
	 ( t5point_comp(A,B,>),! ; t5point_comp(A,B,=) ).

% ---------------------------------------------------------------------

t5nr_p(-(A,B)) :- t5min_p(A),t5max_p(B).

t5nr_raw_create(Min,Max,-(Min,Max)).

t5nr_create(-(0,in)).
%t5nr_create(-(A,B)) :- t5min_create(A),t5max_create(B).

t5nr_f_create(-(0,1)).
%t5nr_f_create(-(A,B)) :- t5min_f_create(A),t5max_f_create(B).

t5nr_a_create(-(1,1)).
%t5nr_a_create(-(A,B)) :- t5min_a_create(A),t5max_a_create(B).

t5nr_add_min(-(Oldmin,Max),Newmin,-(Finmin,Max)) :- 
        t5min_unify(Oldmin,Newmin,Finmin).

t5nr_add_max(-(Min,Oldmax),Newmax,-(Min,Finmax)) :- 
	t5max_unify(Oldmax,Newmax,Finmax).

t5nr_s_min(-(_,Max),Min,-(Min,Max)).

t5nr_s_max(-(Min,_),Max,-(Min,Max)).

t5nr_r_min(-(Min,Max),Min,N_Min,-(N_Min,Max)).

t5nr_r_max(-(Min,Max),Max,N_Max,-(Min,N_Max)).

t5nr_generate(Min,Max,-(FinMin,FinMax)) :- 
	t5min_generate(Min,FinMin),
	t5max_generate(Max,FinMax).

t5nr_f_generate(Min,Max,-(FinMin,FinMax)) :- 
	t5min_f_generate(Min,FinMin),
	t5max_f_generate(Max,FinMax).

t5nr_a_generate(Min,Max,-(FinMin,FinMax)) :- 
	t5min_a_generate(Min,FinMin),
	t5max_a_generate(Max,FinMax).

t5nr_unify(-(Min1,Max1),-(Min2,Max2),-(Min3,Max3)) :- 
        t5min_unify(Min1,Min2,Min3),
        t5max_unify(Max1,Max2,Max3).

t5nr_subsumes_p(-(Min1,Max1),-(Min2,Max2)) :-
        t5min_subsumes_p(Min1,Min2),
        t5max_subsumes_p(Max1,Max2).

getsub(Nr,Nr,equal):-!.

getsub(Nr1,Nr2,first) :-
        t5nr_subsumes_p(Nr2,Nr1),!.

getsub(Nr1,Nr2,second) :-
        t5nr_subsumes_p(Nr1,Nr2),!.

getsub(_,_,none).
                                        
t5nr_compare(Sub1,Nr1,Nr2,Sub2) :- 
        getsub(Nr1,Nr2,Sub),
        t5sub_unify(Sub1,Sub,Sub2).

t5nr_incoherent_p(-(Min,Max)) :- 
        t5point_lt_p(Max,Min).

t5nr_redundant_p(-(0,in)).

t5nr_f_redundant_p(-(0,1)).

t5nr_a_redundant_p(-(1,1)).

t5nr_functional_p(-(_,1)).

t5nr_attribute_p(-(1,1)). /* same as a_redundant_p  */

t5nr_min(-(Min,_),Min).

t5nr_max(-(_,Max),Max).

t5nr_minmax(-(Min,Max),Min,Max).   


%---- old ones for compatibi


t5nr_comp(A,B,C) :- nonvar(A),nonvar(B),compare(C,A,B),!.

t5nr_g_min(A,B,A) :- (var(B) ; A @< B),!.
t5nr_g_min(_,B,B). 

t5nr_g_max(A,B,A) :- (var(B); A @> B),!.
t5nr_g_max(_,B,B).

t5nr_lt_p(A,B) :- t5nr_comp(A,B,<).

t5nr_gt_p(A,B) :- t5nr_comp(A,B,>).

t5nr_eq_p(A,B) :- t5nr_comp(A,B,=).

t5nr_not_eq_p(A,B) :- t5nr_comp(A,B,=),!,fail.

t5nr_not_eq_p(_,_).

t5nr_le_p(A,B) :- ( t5nr_comp(A,B,<),! ; t5nr_comp(A,B,=) ).

t5nr_ge_p(A,B) :- ( t5nr_comp(A,B,>),! ; t5nr_comp(A,B,=) ).

/* end of old ones */

%% OR on NRs !!  -cmk-
t5nr_generalize(NR0, NR1, NR) :-
	t5nr_minmax(NR0, Min0, Max0),
	t5nr_minmax(NR1, Min1, Max1),
	t5nr_minmax(NR,  Min,  Max), 
	t5nr_g_min(Min0, Min1, Min),
	t5nr_g_max(Max0, Max1, Max).

/* auch cmk */ 
%! new
t5nr_optional_p(-(0,_)).

%! new
t5nr_empty_p(-(0,0)).


/* ------------------------------------------------------------ */ 
/*			numbers					*/
/* ------------------------------------------------------------ */ 

/* minimum von rechter schranke */ 
t5number_min(X,X,X) :- !.
t5number_min(o(inf(-)),_,o(inf(-))) :- !.
t5number_min(_,o(inf(-)),o(inf(-))) :- !.
t5number_min(o(inf(+)),X,X) :- !.
t5number_min(X,o(inf(+)),X) :- !.

t5number_min(o(X),c(X),o(X)) :- !.
t5number_min(c(X),o(X),o(X)) :- !.
%t5number_min(o(X),c(X),c(X)) :- !.
%t5number_min(c(X),o(X),c(X)) :- !.

t5number_min(o(X),o(Y),o(Z)) :- t5number_imin(X,Y,Z),!.
t5number_min(c(X),c(Y),c(Z)) :- t5number_imin(X,Y,Z),!.
t5number_min(o(X),c(Y),o(X)) :- t5number_imin(X,Y,X),!.
t5number_min(o(X),c(Y),c(Y)) :- t5number_imin(X,Y,Y),!.
t5number_min(c(X),o(Y),c(X)) :- t5number_imin(X,Y,X),!.
t5number_min(c(X),o(Y),o(Y)) :- t5number_imin(X,Y,Y),!.

/* maximimum von linker schranke */
t5number_max(X,X,X) :- !.
t5number_max(o(inf(+)),_,o(inf(+))) :- !.
t5number_max(_,o(inf(+)),o(inf(+))) :- !.
t5number_max(o(inf(-)),X,X) :- !.
t5number_max(X,o(inf(-)),X) :- !.

t5number_max(o(X),c(X),o(X)) :- !.
t5number_max(c(X),o(X),o(X)) :- !.

t5number_max(o(X),o(Y),o(Z)) :- t5number_imax(X,Y,Z),!.
t5number_max(c(X),c(Y),c(Z)) :- t5number_imax(X,Y,Z),!.
t5number_max(o(X),c(Y),o(X)) :- t5number_imax(X,Y,X),!.
t5number_max(o(X),c(Y),c(Y)) :- t5number_imax(X,Y,Y),!.
t5number_max(c(X),o(Y),c(X)) :- t5number_imax(X,Y,X),!.
t5number_max(c(X),o(Y),o(Y)) :- t5number_imax(X,Y,Y),!.





t5number_inco_p(L,R) :- arg(1,L,X),arg(1,R,X).


t5number_imin(X,Y,Z) :-
	Z is min(X,Y).

t5number_imax(X,Y,Z) :-
	Z is max(X,Y).

t5number_describe_left(o(inf(-)),[]) :- !. 
t5number_describe_left(o(X),[gt(X)]) :- !.
t5number_describe_left(c(X),[ge(X)]) :- !.

t5number_describe_right(o(inf(+)),[]) :- !. 
t5number_describe_right(o(X),[lt(X)]) :- !.
t5number_describe_right(c(X),[le(X)]) :- !.

t5number_describe(num,[]) :- !.
t5number_describe(Number,[num(V)]) :-
	t5number_raw_create(c(V),c(V),Number),!.

t5number_describe(Number,Desc_Number) :-
	t5number_raw_create(L,R,Number),
	t5number_describe_left(L,Desc_L),
	t5number_describe_right(R,Desc_R),
	append(Desc_L,Desc_R,Desc_Number).

t5number_describe(Number,In,In) :- var(Number),!.
t5number_describe(Number,In,Out) :- 
	t5number_describe(Number,Desc),
	append(In,Desc,Out).

t5number_minimize(num,num,num) :- !.
t5number_minimize(Num1,Num2,Num3) :- 
/* i.e. maximize but describes negliates infinite */ 
	(Num2 == num -> t5number_create2(NN);NN=Num2),
	t5number_raw_create(L1,R1,Num1),
	t5number_raw_create(L2,R2,NN),
	t5number_raw_create(L3,R3,Num3),
	(L1 == L2 -> L3 = o(inf(-)) ; L3 = L1),
	(R1 == R2 -> R3 = o(inf(+)) ; R3 = R1).


t5number_item_close(X,c(X)).
t5number_item_open(X,o(X)).

t5number_raw_create(L,R,n(L,R)) :- !.

t5number_create(num) :- !.
t5number_create2(N) :- 
        t5number_raw_create(o(inf(-)),o(inf(+)),N).

t5number_create_instance(Num,Value) :-
	t5number_raw_create(c(Value),c(Value),Num).

t5number_add_min_open(num,Min,N_N) :-
	t5number_create2(N),
	t5number_add_min_open(N,Min,N_N),!.

t5number_add_min_open(N,Min,N_N) :-
        t5number_raw_create(L,R,N),
        t5number_item_open(Min,X),
        t5number_max(X,L,N_L),
        t5number_raw_create(N_L,R,N_N).

t5number_add_min_close(num,Min,N_N) :-
	t5number_create2(N),
	t5number_add_min_close(N,Min,N_N),!.

t5number_add_min_close(N,Min,N_N) :-
        t5number_raw_create(L,R,N),
        t5number_item_close(Min,X),
        t5number_max(X,L,N_L),
        t5number_raw_create(N_L,R,N_N).

t5number_add_max_open(num,Max,N_N) :-
	t5number_create2(N),
	t5number_add_max_open(N,Max,N_N),!. 

t5number_add_max_open(N,Max,N_N) :-
        t5number_raw_create(L,R,N),
        t5number_item_open(Max,X),
        t5number_min(X,R,N_R),
        t5number_raw_create(L,N_R,N_N).

t5number_add_max_close(num,Max,N_N) :-
	t5number_create2(N),
	t5number_add_max_close(N,Max,N_N),!. 

t5number_add_max_close(N,Max,N_N) :-
        t5number_raw_create(L,R,N),
        t5number_item_close(Max,X),
        t5number_min(X,R,N_R),
        t5number_raw_create(L,N_R,N_N).

t5number_subsumes_p(num,_) :- !. 
t5number_subsumes_p(X1,X2) :- 
        t5number_raw_create(L1,R1,X1),
        t5number_raw_create(L2,R2,X2),
        t5number_max(L1,L2,L3),
        t5number_min(R1,R2,R3),!,
	R3 == R2,
	L3 == L2.


t5number_minn(L,R,Z) :- t5number_min(L,R,Z),!.
t5number_maxx(L,R,Z) :- t5number_max(L,R,Z),!.

t5number_incoherent_p(num) :- !,fail.
t5number_incoherent_p(Y) :- 
        t5number_raw_create(c(X),c(X),Y) ,!,fail.
t5number_incoherent_p(X) :-
        t5number_raw_create(L,R,X),
	(
        t5number_minn(L,R,Z),Z==R
	;
	t5number_inco_p(L,R)
	).
	
t5number_unify(num,X2,X2) :- !.
t5number_unify(X1,num,X1) :- !.
t5number_unify(X1,X2,X3) :-
        t5number_raw_create(L1,R1,X1),
        t5number_raw_create(L2,R2,X2),
        t5number_raw_create(L3,R3,X3),
        t5number_min(R1,R2,R3),
        t5number_max(L1,L2,L3).


t5number_get_sub(X,X,equal) :- !.
t5number_get_sub(X,Y,first) :-
        t5number_subsumes_p(Y,X),!.
t5number_get_sub(X,Y,second) :-
        t5number_subsumes_p(X,Y),!.
t5number_get_sub(_,_,none).

t5number_compare(S,num,num,S) :- !.
t5number_compare(S1,X,Y,S2) :-
        t5number_get_sub(X,Y,S),
        t5sub_unify(S1,S,S2).



/* ------------------------------------------------------------ */
/*		    restrictionlist				*/
/* ------------------------------------------------------------ */

/*
* RL : rl(state,[Res1,Res2,...Resn])
* Res : [Role,...]
* state e state(A,B) A e {c,i} B e {e,r}  
*/


t5rl_create(rl(state(c,e),[])).

t5rl_raw_create(S,L,rl(S,L)).
t5rl_ress(rl(_,ResS),ResS).
t5rl_status(rl(S,_),S).

t5rl_s_state(rl(_,L),S,rl(S,L)).
t5rl_s_ress(rl(S,_),L,rl(S,L)).
t5rl_r_state(rl(S,L),S,NS,rl(NS,L)).
t5rl_r_ress(rl(S,L),L,NL,rl(S,NL)).

/*
t5rl_modified_p(RL1,RL2) :-
	%% reset closedness markers first !!
	t5rl_mark(RL1,RL1a),
	t5rl_mark(RL2,RL2a),
	t5rl_raw_create(_,L,RL1a),
	t5rl_raw_create(_,L,RL2a),
	!,fail.
t5rl_modified_p(_,_) :- !.
*/

t5rl_modified_p(RL1,RL2) :-
	t5rl_raw_create(_,L1,RL1),
	t5rl_raw_create(_,L2,RL2),
	t5ress_modified_p(L1,L2).

t5ress_modified_p([],L) :- L \== [].
t5ress_modified_p([L1|LL1],L2) :- 
	t5ress_modified2_p(L2,[L1|LL1]).

t5ress_modified2_p([],_). % _ \== []
t5ress_modified2_p([L1|LL1],[L2|LL2]) :-
	(
	t5res_modified_p(L1,L2) -> true
		;
	t5ress_modified_p(LL2,LL1)).
	


t5rl_done(rl(_,[])).

%% t5rl_next(?RoleRes, +RoleResList, -ResidueRoleResList)

t5rl_next(Res, RL0, RL) :-
	t5rl_raw_create(S,L0,RL0),
	t5rl_raw_create(S,L ,RL ),
	select(Res, L0, L).

t5rl_next_1(RoleRes, RoleResList, ResidueResList) :-
	t5rl_next(RoleRes, RoleResList, ResidueResList),
	!.

t5rl_comlete_p(rl(state(c,_),_)) :- !.
t5rl_rvms_p(rl(state(_,r),_)) :- !.

t5rl_role_res(RL,R,RES):-
        t5rl_ress(RL,Ress),
	(b5typelist_get_p(Ress,R,RES),!
        ; t5res_create(R,RES)).

t5rl_role_res(RL,R,RES,Flag):-
        t5rl_ress(RL,Ress),
	(b5typelist_get_p(Ress,R,RES),Flag = old,!
        ; t5res_create(R,RES)),Flag = new.

t5rl_vr_min_max(RL,Role,VR,Min,Max) :- 
        t5rl_role_res(RL,Role,Res),
        t5res_vr_min_max(Res,VR,Min,Max).

t5rl_vr(RL,Role,VR) :- 
        t5rl_role_res(RL,Role,Res),
        t5res_vr(Res,VR).

t5rl_min(RL,Role,Min) :-
        t5rl_role_res(RL,Role,Res),
        t5res_min(Res,Min).

t5rl_max(RL,Role,Max) :-
        t5rl_role_res(RL,Role,Res),
        t5res_max(Res,Max).

t5rl_nr(RL,Role,NR) :-
        t5rl_role_res(RL,Role,Res),
        t5res_nr(Res,NR).

t5rl_fillers(RL,Role,Fillers) :-
        t5rl_role_res(RL,Role,Res),
        t5res_fillers(Res,Fillers).

t5rl_invs(RL,Role,Invs) :-
        t5rl_role_res(RL,Role,Res),
        t5res_invs(Res,Role,Invs).

t5rl_rvm_equals(RL,Role,Equals) :-
        t5rl_role_res(RL,Role,Res),
        t5res_equals(Res,Equals).

t5rl_rvm_supers(RL,Role,Supers) :-
        t5rl_role_res(RL,Role,Res),
        t5res_supers(Res,Supers).

t5rl_subs(RL,Role,Subs) :-
        t5rl_role_res(RL,Role,Res),
        t5res_subs(Res,Subs).


t5rl_add_role_res(RL,Res,RL) :-
        t5res_redundant_p(Res),!.

/*
t5rl_add_role_res([[_,ER],List],Res,[[i,ER],NewList]) :-
	b5typelist_add_ele(List,Res,NewList).
*/


t5rl_add_role_res(RL,Res,NewRL) :-
	t5rl_ress(RL,Ress),
	b5typelist_add_ele(Ress,Res,NewRess),
	t5rl_raw_create(i,NewRess,NewRL).


t5rl_ressadd(RL,Res,RL) :-
        t5res_redundant_p(Res),!.

t5rl_ressadd(RESS,Res,NRESS) :-
        b5typelist_add_ele(RESS,Res,NRESS).

t5rl_add_role_res_c(RL,Res,RL) :-
        t5res_redundant_p(Res),!.
t5rl_add_role_res_c(List,Res,NewList) :-
        b5typelist_add_ele(List,Res,NewList).

/* correct, if ResLists do never contain redundant restriktions */

t5rl_comp(none,_,_,none):-!.

t5rl_comp(St,[],[],St) :-!.

t5rl_comp(St,[],_,NewSt) :- 
        t5sub_unify(St,second,NewSt),!.

t5rl_comp(St,_,[],NewSt) :-
        t5sub_unify(St,first,NewSt),!.

t5rl_comp(St,[H1|T1],[H2|T2],FinSt) :- 
	b5typelist_order(H1,H2,Order),
	t5rl_comp_order(Order,H1,T1,H2,T2,St,FinSt).

t5rl_comp_order(=,H1,T1,H2,T2,St1,St3) :-
	t5res_compare(St1,H1,H2,St2),
	t5rl_comp(St2,T1,T2,St3).

t5rl_comp_order(<,_H1,T1,H2,T2,St1,St3) :-
        t5sub_unify(St1,first,St2),
	t5rl_comp(St2,T1,[H2|T2],St3).

t5rl_comp_order(>,H1,T1,_H2,T2,St1,St3) :-
        t5sub_unify(St1,second,St2),
	t5rl_comp(St2,[H1|T1],T2,St3).

t5rl_compare(Sub1,rl(state(c,_),List1),rl(state(c,_),List2),Sub2) :-
        /* failed bei incomplete! was ist mit stati ? */
        t5rl_comp(Sub1,List1,List2,Sub2).


t5rl_compl_supers([],_,_,Final,Final):- !.

t5rl_compl_supers([Super|Supers],Fillers,Min,Accu,Final) :-
        %t5res_generate(Super,_,Min,_,Fillers,_,_,_,Res),
        t5res_gen_r_min_fillers(Super,Min,Fillers,Res),
        t5rl_ressadd(Accu,Res,NewAccu),
        t5rl_compl_supers(Supers,Fillers,Min,NewAccu,Final).

t5rl_compl_subs([],_,_,F,F) :- !.

t5rl_compl_subs([Sub|Subs],VR,Max,Accu,Final):-       
        %t5res_generate(Sub,VR,_,Max,_,_,_,_,Res),
        t5res_gen_r_vr_max(Sub,VR,Max,Res),
        t5rl_ressadd(Accu,Res,NewAccu),
        t5rl_compl_subs(Subs,VR,Max,NewAccu,Final).        

t5rl_compl_list([],F,F) :- !.

t5rl_compl_list([Head|Tail],Accu,Final) :- 
        t5res_generate(Role,VR,Min,Max,Fillers,Head),
        t5role_supers(Role,Supers),
        b5typelist_filter_out(Supers,t5role_primitive_p,LessSupers),
        t5rl_compl_supers(LessSupers,Fillers,Min,Accu,NewAccu),
        t5role_subs(Role,Subs),
        b5typelist_filter_out(Subs,t5tbox_nothing_key,LessSubs),
        t5rl_compl_subs(LessSubs,VR,Max,NewAccu,VeryNewAccu),
        t5rl_compl_list(Tail,VeryNewAccu,Final).

/* old : 
t5rl_complete(rl(state(i,X),List1),rl(state(c,X),List2)) :-
        % no completion for complete Reslist 
        !,
        t5rl_compl_list(List1,List1,List2).        
*/
% new 10.9.92
% new 27.02.93
t5rl_complete(rl(state(i,X),List1),rl(state(c,X),List3)) :-
        /* no completion for complete Reslist */
        !,
	%t5rl_rvm_propagate(X,List1,[],_UUU,List2),
	t5rl_rvm_propagate(X,List1,List2),
        t5rl_compl_list(List2,List2,List3).        

t5rl_complete(rl(state(c,X),List1),rl(state(c,X),List1)). 



/* propagates fillers,vr and number restriction
   according the rvm-equal entry. the other rvm entries 
   are not used yet */
	
t5rl_rvm_propagate(e,L,L). /* flag is e i.e. empty ,norvms*/
t5rl_rvm_propagate(r,L1,L2) :- /* flag is r i.e. with rvms */ 
	t5ress_rvm_propagate(L1,L2).


t5ress_rvm_propagate(L1,L3) :-
	t5ress_propagate_phase_one(L1,[],RVM),
	t5eq_rvms2eq(RVM,EQ),
	t5ress_progagate_phase_two(L1,EQ,F_EQ,L2),
	t5ress_progagate_phase_three(L2,F_EQ,L3),
	t5eq_eval_new(F_EQ).


t5ress_propagate_phase_one([],RVMS,RVMS). 
t5ress_propagate_phase_one([H|T],RVMS,F_RVMS) :-
	t5eq_include_new(RVMS,H,N_RVMS),
	t5ress_propagate_phase_one(T,N_RVMS,F_RVMS).

t5ress_progagate_phase_two([],EQ,EQ,[]).
t5ress_progagate_phase_two([H|T],EQ,FEQ,[NH|NT]) :-
	t5eq_process_res(EQ,H,NH,NEQ),
	t5ress_progagate_phase_two(T,NEQ,FEQ,NT).

t5ress_progagate_phase_three(L2,EQ,L3) :-
	b5rvrl_build(EQ,RVARL),
	b5rvrl_unwrap(RVARL,UN),
	t5ress_include_rvars(UN,L2,L3).


t5ress_include_rvars([],L,L) :- !.
t5ress_include_rvars(L,[],L) :- !.

t5ress_include_rvars([R-VAR|RVARS],[S-ES|Ress],N_Ress) :-
	b5typelist_order(R,S,Order),
	t5ress_include_rvars_order(Order,R-VAR,RVARS,S-ES,Ress,N_Ress).

t5ress_include_rvars_order(<,R-VAR,RVARS,S-ES,Ress,[R-VAR|N_Ress]) :-
	t5ress_include_rvars(RVARS,[S-ES|Ress],N_Ress).

t5ress_include_rvars_order(=,R-VAR,RVARS,R-VAR,Ress,[R-VAR|N_Ress]) :-
	t5ress_include_rvars(RVARS,Ress,N_Ress).

t5ress_include_rvars_order(>,R-VAR,RVARS,S-Es,Ress,[S-Es|N_Ress]) :-
	t5ress_include_rvars([R-VAR|RVARS],Ress,N_Ress).


t5rl_down(RL1,NF,RL2,Flag) :-
        t5rl_r_ress(RL1,Ress,N_Ress,RL2),
        t5ress_down(Ress,Ress,NF,N_Ress,Flag).

t5ress_down([],X,_,X,no) :- !.
t5ress_down([],X,_,X,yes).
t5ress_down([RES|Ress],AllRes,NF,N_AllRes,Flag) :-
        t5res_generate(R,VR,Min,_Max,Fillers,_RVM,RES),
        (
        t5point_not_eq_p(Min,0) ->
                t5role_subs(R,Subs),
                t5rl_filter(Subs,LessSubs),
                t5rl_more_filter(LessSubs,[R],NF,VR,Aply),
                t5rl_dodown(Aply,Min,Fillers,AllRes,AllRes1,Flag1),
		(Flag1 == no -> 
			t5ress_down(Ress,AllRes1,NF,N_AllRes,Flag)
			;
			t5ress_down(Ress,AllRes1,NF,N_AllRes,_Flag),
			Flag = Flag1)
        ;
        t5ress_down(Ress,AllRes,NF,N_AllRes,Flag)
	).


t5rl_more_filter([],_,_,_,[]).
t5rl_more_filter([Sub|Subs],R,NF,VR,[Sub|XSubs]) :-
        t5role_downward_hereditary_p(Sub,R,NF,VR),!,
        t5rl_more_filter(Subs,R,NF,VR,XSubs).
t5rl_more_filter([_|Subs],R,NF,VR,XSubs) :-
        t5rl_more_filter(Subs,R,NF,VR,XSubs).

t5rl_dodown([],_,_,X,X,no) :- !.
t5rl_dodown([],_,_,X,X,yes).

t5rl_dodown([Role|Roles],Min,Fillers,[],[N_Res|N_Ress],yes) :-
	!,
	t5res_gen_r_min_fillers(Role,Min,Fillers,N_Res),
	t5rl_dodown(Roles,Min,Fillers,[],N_Ress,_).

t5rl_dodown([Role|Roles],Min,Fillers,[R-Es|Ress],N_Ress,Flag) :-
	b5typelist_order(Role,R,Order),
	t5rl_dodown_order(Order,Role,Roles,Min,Fillers,R-Es,Ress,N_Ress,Flag).

t5rl_dodown_order(<,Role,Roles,Min,Fillers,RES,Ress,[N_RES|N_Ress],yes) :-
	t5res_gen_r_min_fillers(Role,Min,Fillers,N_RES),
	t5rl_dodown(Roles,Min,Fillers,[RES|Ress],N_Ress,_).

t5rl_dodown_order(=,_Role,Roles,Min,Fillers,Res,Ress,[N_Res|N_Ress],Flag) :-
        t5res_add_min(Res,Min,Res1),
        t5res_add_fillers(Res1,Fillers-_,N_Res),
	(N_Res == Res ->  
        	t5rl_dodown(Roles,Min,Fillers,Ress,N_Ress,Flag)
        ;	t5rl_dodown(Roles,Min,Fillers,Ress,N_Ress,_),
		Flag = yes).

t5rl_dodown_order(>,Role,Roles,Min,Fillers,Res,Ress,[Res|N_Ress],Flag) :-
        t5rl_dodown([Role|Roles],Min,Fillers,Ress,N_Ress,Flag).

t5rl_inherit_min_fillers([],_,Res-Res) :- !. 
t5rl_inherit_min_fillers(_,[],Res-Res) :- !. 

t5rl_inherit_min_fillers([Role|Roles],[R-ES|Ress],ResRes) :-
        compare(Order,Role,R),
        t5rl_inherit_min_fillers_order(Order,Role,Roles,R-ES,Ress,ResRes).

t5rl_inherit_min_fillers_order(<,_,Roles,RES,Ress,OldNew) :-
        t5rl_inherit_min_fillers(Roles,[RES|Ress],OldNew).
t5rl_inherit_min_fillers_order(>,R1,Rs,_Res,Ress,OldNew) :-
        t5rl_inherit_min_fillers([R1|Rs],Ress,OldNew).
t5rl_inherit_min_fillers_order(=,_R,Rs,RES,Ress,OLD-NEW) :-
        t5res_min(RES,Min),
        t5res_fillers(RES,Fillers),
        t5res_add_min(OLD,Min,NotSoOld),
        t5res_add_fillers(NotSoOld,Fillers-_,LessOld),
        t5rl_inherit_min_fillers(Rs,Ress,LessOld-NEW).


t5rl_inherit_max_vr([],_,Res-Res) :- !.
t5rl_inherit_max_vr(_,[],Res-Res) :- !.
 
t5rl_inherit_max_vr([Role|Roles],[R-ES|Ress],ResRes) :-
        compare(Order,Role,R),
        t5rl_inherit_max_vr_order(Order,Role,Roles,R-ES,Ress,ResRes).

t5rl_inherit_max_vr_order(<,_,Roles,Res,Ress,OldNew) :-
        t5rl_inherit_max_vr(Roles,[Res|Ress],OldNew).
t5rl_inherit_max_vr_order(>,R1,Rs,_RES,Ress,OldNew) :-
        t5rl_inherit_max_vr([R1|Rs],Ress,OldNew).
t5rl_inherit_max_vr_order(=,_R,Rs,RES,Ress,OLD-NEW) :-
        t5res_max(RES,Max),
        t5res_vr(RES,VR),
        t5res_add_max(OLD,Max,NotSoOld),
        t5res_add_vr(NotSoOld,VR,LessOld),
        t5rl_inherit_max_vr(Rs,Ress,LessOld-NEW).

t5rl_filter(Role) :- t5role_primitive_p(Role),!;t5tbox_nothing_key(Role).
t5rl_filter(Roles,NRoles) :-
	b5typelist_filter_out(Roles,t5rl_filter,NRoles).

/* include_role( +RL,+Role, Roles, Roles, -RL) */ 
t5rl_include_role(I_RL,Role,FatherRs,SonRs,O_RL) :-
	t5rl_filter(FatherRs,L_FatherRs),
	t5rl_filter(SonRs,L_SonRs),
        t5res_create(Role,Res),
        t5rl_raw_create(ST,RESS,I_RL),
        t5rl_raw_create(ST,O_RESS,O_RL),
        t5rl_inherit_min_fillers(L_SonRs,RESS,Res-NewRes),
        t5rl_inherit_max_vr(L_FatherRs,RESS,NewRes-VeryNewRes),
        t5rl_add_role_res_c(RESS,VeryNewRes,O_RESS).

t5rl_sub_p([],_) :- !. /* no restriction subsumes any restriction */
t5rl_sub_p([_|_],[]) :- !,fail.

t5rl_sub_p([H1|T1],[H2|T2]) :-
        b5typelist_order(H1,H2,Order),
        t5rl_sub_order_p(Order,H1,T1,H2,T2).

t5rl_sub_order_p(=,H1,T1,H2,T2) :-
        t5res_subsumes_p(H1,H2),
        t5rl_sub_p(T1,T2).

t5rl_sub_order_p(>,H1,T1,_H2,T2) :-
        t5rl_sub_p([H1|T1],T2).

t5rl_sub_order_p(<,_,_,_,_) :- !,fail.

 
t5rl_subsumes_p(rl(state(_,_),List1),rl(state(_,_),List2)):-
        t5rl_sub_p(List1,List2).


t5rl_help_unify(r,_,r):- !.
t5rl_help_unify(_,r,r):- !.
t5rl_help_unify(_,_,e).

t5rl_unify(rl(state(_,Y1),List1),rl(state(_,Y2),List2),rl(state(i,Y3),List3)) :- 
        /* Y3 = f(Y1,Y2) ??? */
        t5rl_help_unify(Y1,Y2,Y3),
        b5typelist_unify(List1,List2,List3).



t5rl_inco_p([]) :- fail,!.
t5rl_inco_p([H|_]) :-
        t5res_incoherent_p(H),!.
t5rl_inco_p([_|T]) :-
        t5rl_inco_p(T),!.
        

t5rl_incoherent_p(rl(state(_,_),RL)) :- 
        t5rl_inco_p(RL).

t5rl_fun(t5res_add_vr,X,Y,Z) :-         	t5res_add_vr(X,Y,Z).
t5rl_fun(t5res_add_min,X,Y,Z) :-        	t5res_add_min(X,Y,Z).
t5rl_fun(t5res_add_max,X,Y,Z) :-        	t5res_add_max(X,Y,Z).
t5rl_fun(t5res_add_nr_res,X,Y,Z) :-        	t5res_add_nr_res(X,Y,Z).
t5rl_fun(t5res_add_fillers,X,Y,Z) :-    	t5res_add_fillers(X,Y,Z).
t5rl_fun(t5res_add_closed_fillers,X,Y,Z) :-     t5res_add_closed_fillers(X,Y,Z).
t5rl_fun(t5res_add_equals,X,Y,Z) :-     	t5res_add_rvm_equals(X,Y,Z).
t5rl_fun(t5res_add_supers,X,Y,Z) :-     	t5res_add_rvm_supers(X,Y,Z).
t5rl_fun(t5res_add_subs,X,Y,Z) :-       	t5res_add_rvm_subs(X,Y,Z).
t5rl_fun(t5res_fixstatus,X,Y1-Y2,Z) :-       	t5res_fixstatus(X,Y1,Y2,Z).
t5rl_fun(t5res_close_role_filler_set,X,_,Z) :-  t5res_close_role_filler_set(X,Z).


t5rl_incl([],FUN,ARG,Role,F_Res) :-
        t5res_create(Role,Res),
        t5rl_fun(FUN,Res,ARG,N_Res),
	(t5res_redundant_p(N_Res) ->
		F_Res = []
	;
		F_Res = [N_Res]
	).

t5rl_incl([R-H|T],FUN,ARG,Role,NRESS) :-  
        compare(Order,R,Role), 
        t5rl_incl_order(Order,R-H,T,Role,FUN,ARG,NRESS).

t5rl_incl_order(=,H,T,_Role,FUN,ARG,[N_H|T]) :-
        t5rl_fun(FUN,H,ARG,N_H).
t5rl_incl_order(<,H,T,Role,FUN,ARG,[H|N_T]) :-
        t5rl_incl(T,FUN,ARG,Role,N_T).
t5rl_incl_order(>,H,T,Role,FUN,ARG,Result) :-
        t5res_create(Role,RES),
        t5rl_fun(FUN,RES,ARG,N_H),	
	(t5res_redundant_p(N_H) ->
		Result = [H|T]
	;
		Result = [N_H,H|T]
	).


t5rl_add_vr(rl(state(_,Y),RL),VR,Role,rl(state(i,Y),N_RL)) :-
        t5rl_incl(RL,t5res_add_vr,VR,Role,N_RL).

t5rl_add_min(rl(state(_,Y),RL),Min,Role,rl(state(i,Y),N_RL))  :-
        t5rl_incl(RL,t5res_add_min,Min,Role,N_RL).

t5rl_add_max(rl(state(_,Y),RL),Max,Role,rl(state(i,Y),N_RL))  :-
        t5rl_incl(RL,t5res_add_max,Max,Role,N_RL).

t5rl_add_nr_res(rl(state(_,Y),RL),Num,Role,rl(state(i,Y),N_RL))  :-
        t5rl_incl(RL,t5res_add_nr_res,Num,Role,N_RL).

t5rl_add_fillers(rl(state(_,Y),RL),Fs,Role,rl(state(i,Y),N_RL))  :-
        t5rl_incl(RL,t5res_add_fillers,Fs,Role,N_RL).

t5rl_add_closed_fillers(rl(state(_,Y),RL),Fs,Role,rl(state(i,Y),N_RL))  :-
        t5rl_incl(RL,t5res_add_closed_fillers,Fs,Role,N_RL).

t5rl_add_equals(rl(state(_,_),RL),Eqs,Role,rl(state(i,r),N_RL))  :-
        t5rl_incl(RL,t5res_add_equals,Eqs,Role,N_RL).

t5rl_add_supers(rl(state(_,_),RL),Supers,Role,rl(state(i,r),N_RL))  :-
        t5rl_incl(RL,t5res_add_supers,Supers,Role,N_RL).

t5rl_add_subs(rl(state(_,_),RL),Subs,Role,rl(state(i,r),N_RL))  :-
        t5rl_incl(RL,t5res_add_subs,Subs,Role,N_RL).

t5rl_close_role_filler_set(rl(state(X,Y),RL),Role,rl(state(X,Y),N_RL))  :-
        t5rl_incl(RL,t5res_close_role_filler_set,dummy,Role,N_RL).

t5ress_fixstatus(Ress,Old,New,Role,N_Ress)  :-
        t5rl_incl(Ress,t5res_fixstatus,Old-New,Role,N_Ress).


t5rl_delta(RL1,RL2,D1,D2) :- 
/* D1 liste von spezialisierten VR */
/* D2 liste von neuen VR, d.h. neue role */
        t5rl_ress(RL1,Ress1),
        t5rl_ress(RL2,Ress2),
        t5rl_delta_help(Ress1,Ress2,[],[],D1,D2).
      
/* R2 groesser R1 */

t5rl_delta_help([],[],A1,A2,A1i,A2i) :-
	t5rl_inv(A1,A1i),
	t5rl_inv(A2,A2i),!.

t5rl_delta_help([],[H|T],A1,A2,D1,D2) :- 
	t5res2rvr(H,RVR),!,
	t5rl_delta_help([],T,A1,[RVR|A2],D1,D2).

t5rl_delta_help([],X,A1,A2,D1,D2) :- 
	t5res2rvr(X,RVR),!,    %yy
	t5rl_delta_help([],[],A1,[RVR|A2],D1,D2) .

t5rl_delta_help([H1|R1],[H2|R2],A1,A2,D1,D2) :-
        b5typelist_order(H1,H2,=),
        t5res_vr(H1,VR),
        t5res_vr(H2,VR),!,
	%t5res_role(H1,Role), ????????
        t5rl_delta_help(R1,R2,A1,A2,D1,D2).

t5rl_delta_help([H1|R1],[H2|R2],A1,A2,D1,D2) :-
        /* b5typelist_order(H1,H2,=), */
        /* vr1 != vr2 i.e. vr2 < vr1 */
        t5res_role(H1,R),
        t5res_role(H2,R),!,
        t5res2rvr(H2,RVR),
        t5rl_delta_help(R1,R2,[RVR|A1],A2,D1,D2).

t5rl_delta_help(R1,[H2|R2],A1,A2,D1,D2) :-
        /* R1.first != R2  */
        t5res2rvr(H2,RVR),
        t5rl_delta_help(R1,R2,A1,[RVR|A2],D1,D2).

t5rl_inv(A,Ainv) :-
	t5rl_inv_dl(A,Ainv/[]).

t5rl_inv_dl([X|Xs],Ys/Zs) :-
	t5rl_inv_dl(Xs,Ys/[X|Zs]).
	  
t5rl_inv_dl([],Xs/Xs).



/*new 20.8.93*/

t5res_update_atmost_0_accu(Accu,RES,[New|Accu]) :-
	t5res_max(RES,0), 
	%t5res_vr(RES,VR),
	t5res_role(RES,Role),
	t5role_range(Role,VR),
	New = Role-nullvr(VR).

t5res_update_atmost_0_accu(Accu,_,Accu).


t5rl_delta(RL1,RL2,D1,D2,D3) :- 
/* RL1 indexing supers Rl      */
/* RL2  pr"ufling RL */ 
/* D1 liste von spezialisierten VR */
/* D2 liste von neuen VR, d.h. neue role */
/* D3 liste von atmost 0 restriktionen, nicht subumiert von RL1 */ 
        t5rl_ress(RL1,Ress1),
        t5rl_ress(RL2,Ress2),
        t5rl_delta_help(Ress1,Ress2,[],[],[],D1,D2,D3).
      
/* R2 groesser R1 */

t5rl_delta_help([],[],A1,A2,A3,A1i,A2i,A3i) :-
	t5rl_inv(A1,A1i),
	t5rl_inv(A2,A2i),
	t5rl_inv(A3,A3i),!.

t5rl_delta_help([],[H|T],A1,A2,A3,D1,D2,D3) :- 
	t5res2rvr(H,RVR),!,
	t5res_update_atmost_0_accu(A3,H,NA3),
	t5rl_delta_help([],T,A1,[RVR|A2],NA3,D1,D2,D3).

t5rl_delta_help([],X,A1,A2,A3,D1,D2,D3) :- 
	t5res2rvr(X,RVR),!,    %yy
	t5res_update_atmost_0_accu(A3,X,NA3),
	t5rl_delta_help([],[],A1,[RVR|A2],NA3,D1,D2,D3) .

t5rl_delta_help([H1|R1],[H2|R2],A1,A2,A3,D1,D2,D3) :-
        b5typelist_order(H1,H2,=),
        t5res_vr(H1,VR),
        t5res_vr(H2,VR),!,
	%t5res_role(H1,Role), ????????
	(t5res_max(H1,0) -> NA3 = A3
		;
		t5res_update_atmost_0_accu(A3,H2,NA3)
	),
        t5rl_delta_help(R1,R2,A1,A2,NA3,D1,D2,D3).

t5rl_delta_help([H1|R1],[H2|R2],A1,A2,A3,D1,D2,D3) :-
        /* b5typelist_order(H1,H2,=), */
        /* vr1 != vr2 i.e. vr2 < vr1 */
        t5res_role(H1,R),
        t5res_role(H2,R),!,
        t5res2rvr(H2,RVR),
	(t5res_max(H1,0) -> NA3 = A3
		;
		t5res_update_atmost_0_accu(A3,H2,NA3)
	),
        t5rl_delta_help(R1,R2,[RVR|A1],A2,NA3,D1,D2,D3).

t5rl_delta_help(R1,[H2|R2],A1,A2,A3,D1,D2,D3) :-
        /* R1.first != R2  */
        t5res2rvr(H2,RVR),
	t5res_update_atmost_0_accu(A3,H2,NA3),
        t5rl_delta_help(R1,R2,A1,[RVR|A2],NA3,D1,D2,D3).
/* s.o.

t5rl_inv(A,Ainv) :-
	t5rl_inv_dl(A,Ainv/[]).

t5rl_inv_dl([X|Xs],Ys/Zs) :-
	t5rl_inv_dl(Xs,Ys/[X|Zs]).
	  
t5rl_inv_dl([],Xs/Xs).
*/

t5rl_ibox_transform(RL,In,Status,N_RL) :-
        t5rl_r_ress(RL,RESS,NRESS,N_RL),
        t5rl_do_i_transform(RESS,In,Status,NRESS).

t5rl_do_i_transform([],_In,_,[]) :- !.
t5rl_do_i_transform([H|T],In,Status,[NH|NT]) :-
        t5res_ibox_transform(H,In,Status,NH),
        t5rl_do_i_transform(T,In,Status,NT).



t5rl_splitting(RL1,RL2,N_RL1,AD12) :-
	t5rl_delta(RL2,RL1,AD1,AD2),  
	/* evt letzen parameter auch noch benuten, und mit
	AD  unifizieren, dieses h"angt von weiterer Verwendumg der
	Liste ab */

	t5rl_raw_create(S,Ress1,RL1),
        t5rl_raw_create(_,Ress2,RL2),
        t5rl_raw_create(S,N_Ress1,N_RL1), % S ??
	%t5rl_split(AD1,Ress1,Ress2,N_Ress1), %oldand new s.u
	t5rl_split(Ress1,Ress2,N_Ress1), %verynew
	b5typelist_unify(AD1,AD2,AD12).      

/* new 20.8.93 */
t5rl_splitting(RL1,RL2,N_RL1,AD12,AN) :-
	t5rl_delta(RL2,RL1,AD1,AD2,AN),  
	/* evt letzen parameter auch noch benuten, und mit
	AD  unifizieren, dieses h"angt von weiterer Verwendumg der
	Liste ab */

	t5rl_raw_create(S,Ress1,RL1),
        t5rl_raw_create(_,Ress2,RL2),
        t5rl_raw_create(S,N_Ress1,N_RL1), % S ??
	%t5rl_split(AD1,Ress1,Ress2,N_Ress1), %oldand new s.u
	t5rl_split(Ress1,Ress2,N_Ress1), %verynew
	b5typelist_unify(AD1,AD2,AD12).      

/* new new */

t5rl_split([],_,[]).
t5rl_split([R-ES1|Ress1],Ress2,N_Ress1) :-
	(b5typelist_get_p(Ress2,R,RES),!;t5res_create(R,RES)),  
	t5res_split(R-ES1,RES,N_RES1),
	(	
	  	t5res_redundant_p(N_RES1) -> N_Ress1 = Tail
	  		;	N_Ress1 = [N_RES1|Tail]
	),
	t5rl_split(Ress1,Ress2,Tail).


t5rl_map(RL,Fon,N_LIST) :-
	t5rl_ress(RL,RESS),
	b5typelist_map(RESS,N_LIST,Fon).


t5rl_atomize(RL,ATOMS) :-
	t5rl_ress(RL,RESS),
	t5rl_atom(RESS,ATOMS).

t5rl_atom([],[]).
t5rl_atom([R|ES],ATOMS) :-
	t5res_atomize(R,res(A,B,C,D)),
	(
		D == [] -> ATOMS = [A,B,C|Tail] ; ATOMS = [A,B,C,D|Tail]
	),
	t5rl_atom(ES,Tail).


t5rl_used_objects(RL,Objs) :-
	t5rl_ress(RL,Ress),
	t5ress_used_objects(Ress,[],Objs).


t5ress_used_objects([],Y,Y) :- !.
t5ress_used_objects([R-Es|Ress],Objs,Final) :-
	t5res_fillers(R-Es,F),
	b5sort_unify(Objs,F,N_Objs),
	t5ress_used_objects(Ress,N_Objs,Final).


t5rl_roles_fillers(RL,Roles,Fillers) :-
	t5rl_ress(RL,Ress),
	t5ress_roles_fillers(Roles,Ress,[],Fillers).


t5ress_roles_fillers([],_,Fillers,Fillers) :- !.

t5ress_roles_fillers([R|Roles],[R-ES|RESS],Accu,Fillers) :-
	!,
	t5res_fillers(R-ES,New),
	b5sort_unify(Accu,New,NewAccu),
	t5ress_roles_fillers(Roles,RESS,NewAccu,Fillers).

t5ress_roles_fillers(Roles,[_|RESS],Accu,Fillers) :-
	t5ress_roles_fillers(Roles,RESS,Accu,Fillers).



/* -------------------------------------------------------------------- */
/*									*/
/* 	                       K  I  F					*/
/*									*/
/* -------------------------------------------------------------------- */
/* this module is the KernelInterFace, which realizes the kernel	*/
/* entry level for some retrieval operations 			        */
/* -------------------------------------------------------------------- */
/*									*/
/* exports :								*/
/*									*/
/*  b5kif_name_key(?Name,?Type,?Key)					*/
/*  b5kif_fillers(+Entity:Key,+Role:Key,+NFX,-Fillers:list(Key))	*/
/*  b5kif_nr(+Entity:Key,+Role:Key,+NFX,-(Min,Max))			*/
/*  b5kif_vr(+Entity:Key,+Role:Key,+NFX,-VR:Key)			*/
/*  b5kif_msc(+Entity:Key,+NFX,-MSC:list(Key))				*/
/*  b5kif_extension(+Conc:Key,+NFX,-Ext:list(Key))			*/
/*  b5key_exists_p(+X:Tset,+K:Key)					*/
/*       Tset = {value,obj,(c,0),(r,0),(n,0),(s,0),(a,N:integer)}	*/
/*									*/
/* -------------------------------------------------------------------- */

/*
b5kif_name_key(Name,Type,Key)
	Name is the userdefined name for the entity referred to by Key.
	entities are concepts, roles, objects, attributsets, and
	attributes and number_concepts and numbers.
	if there is more than one name for a Key, i.e. if there
	are equivalent entities all names are retrieved by 
	backtracking.
	if the entity has no userdefined name a name is created:
	the schema is Functor(Arg). For concepts the
	Functor is conc, for roles the functor is role, feature
	or inherent_feature, for objects the functor is obj for
	asets the functor is the named attribute domain,
	attributes have always userdefined name and so there is
	no functor. the functor for numbers is number, all
	instances of numberconcepts have a predefined name,
	i.e. the prolog representation for numbers.
	for all primitive concepts (roles) there are primitive
	components for these the functor c_prim (r_prim) is
	used, the top concept is functored by top.
	The Arg is the Key.
	Type e {bottom-bottom,conc-CSpec,role-RSpec,obj-conc-CSpec,value-VSpec}
	CSpec prim,bottom,top,conc,string,number,aset/ASpec ++ /Dom
	RSpec prim,bottom,role,feature,inherent_feature	    ++ /Dom
	VSpec number,aset/ASpec			
	ASpec open/DomainKey close/DomainKey bottom/DomainKey 
*/


b5kif_cmap((c,T),conc/TT) :- 
	b5st_domain_entry(_,(c,T),TT,_,_),!.
b5kif_cmap((a,T),aset/TT) :- 
	b5st_domain_entry(_,(a,T),TT,_,_),!.
b5kif_cmap((n,T),number/TT) :- 
	b5st_domain_entry(_,(n,T),TT,_,_),!.
b5kif_cmap((s,T),string/TT) :- 
	b5st_domain_entry(_,(s,T),TT,_,_),!.
b5kif_cmap((r,T),role/TT) :- 
	b5st_domain_entry(_,(r,T),TT,_,_),!.

b5kif_cmap2(T,(KeyType,empty)) :- 
	b5st_domain_entry(_,T,TT,_,_),
	b5kif_key2type(TT,(KeyType,_)).

b5kif_user_defined_name_p(Name) :- % or predefined i.e. anything,...
	(atom(Name);number(Name)),
	(
	b5st_object_entry(Name,_,Key,_);
	b5st_class_entry(Name,_,Key,_)).

b5kif_st_entry(Name,Type,Key) :-
	b5st_class_entry(Name,T,Key,_),
	b5kif_key2type(Key,KeyType),
	(KeyType = (bottom-bottom,empty) -> 
		b5kif_cmap2(T,Type)
		;
		Type = KeyType).

b5kif_st_entry(Name,Type,Key) :-
	b5st_object_entry(Name,_,Key,_),
	b5kif_key2type(Key,Type).



b5kif_names_keys([],_,[]).
b5kif_names_keys([Name|Names],Type,[Key|Keys]) :-
	b5kif_name_key(Name,Type,Key),
	b5kif_names_keys(Names,Type,Keys).


b5kif_var(X,Y,XY) :-
	(var(X)->(var(Y)->XY=vv;XY=vn);(var(Y)->XY=nv;XY=nn)).

% role -nothing ???
b5kif_ttt(role-prim/_,role) :-!.
b5kif_ttt(role-role,role) :-!.
b5kif_ttt(role-feature,feature) :-!.
b5kif_ttt(role-inhernet_feature,inherent_feature) :-!.
b5kif_ttt(conc-conc/_,conc) :-!.
b5kif_ttt(conc-aset/_/D,aset-D) :-!.
b5kif_ttt(conc-number/_,number) :-!.
b5kif_ttt(conc-string/_,string) :-!.
b5kif_ttt(conc-prim/_,conc) :-!.
b5kif_ttt(value-aset/_/D,attribute-D) :-!.


%Type is conc,number,aset-Domain,string,role,feature,inherent_feature,attribute-Domain
b5kif_name_type_key(Name,Type,Key) :-
	b5kif_name_key(Name,T1,Key),
	T1 = (A,_X),
	b5kif_ttt(A,Type).


b5kif_name_key(Name,Type,Key) :-
	b5kif_var(Name,Key,XY),
	b5kif_name_key(XY,Name,Type,Key).


b5kif_name_key(nn,Name,Type,Key) :-
	b5kif_name_key(vn,Name,Type,Key).

b5kif_name_key(nv,Name,Type,Key) :-
	%nonvar(Name),
	%var(Key),!,
	(
	b5kif_user_defined_name_p(Name) -> 
		b5kif_st_entry(Name,Type,Key)
	;
		arg(1,Name,Key),
		b5kif_key2type(Key,Type)
	).

b5kif_name_key(vn,Name,Type,Key) :-
	%var(Name),
	%nonvar(Key),!,
	(
		b5kif_st_entry(Name,Type,Key)
	;
		\+ b5kif_st_entry(Name,Type,Key),
		b5kif_wrapped_key(Key,Name),
		b5kif_key2type(Key,Type)
	).


b5kif_name_key(vv,Name,Type,Key) :-
	%var(Name),
	%var(Key),!,
	t5tbox_key(Max),
	between(1,Max,Key),
	b5kif_name_key(vn,Name,Type,Key).


b5kif_wrapped_key(Key,Name) :-
	nonvar(Key),!,  
	b5kif_wrap(Key,Wrap),
	Name =..[Wrap,Key].

b5kif_wrap(Key,obj) :- a5odb_get(Key,_),!.
b5kif_wrap(Key,Wrap) :- t5concid_wrap(Key,Wrap),!.
b5kif_wrap(Key,Wrap) :- t5rdb_get(Key,_Role,Wrapi),!,
	(Wrapi =prim/_ ->Wrap =prim;Wrap=Wrapi).
b5kif_wrap(Key,value) :- b5inst_get(Key,_Value),!.
		
b5kif_wrap2type(obj,obj) :- !.
b5kif_wrap2type(role,role) :- !.
b5kif_wrap2type(feature,role) :- !.
b5kif_wrap2type(inherent_feature,role) :- !.
b5kif_wrap2type(_,conc) :- !.

b5kif_key2type(Key,(F,S)) :-
	b5kif_key2typex(Key,F),
	b5kif_sec(Key,S).

b5kif_sec(K,empty) :-
	t5tbox_nothing_key(K).
b5kif_sec(K,notempty) :-
	\+ t5tbox_nothing_key(K).

b5kif_key2typex(Key,bottom-bottom) :- t5tbox_nothing_key(Key),!.
b5kif_key2typex(Key,obj-conc-TY) :- a5objid_nf_u(Key,_,NF),
				    b5nf_kif_type(NF,TY),!.
b5kif_key2typex(Key,conc-TY) :- t5concid_kif_type(Key,TY),!.
b5kif_key2typex(Key,role-TY) :- t5rdb_get(Key,_Role,TY).
				%t5tbox_anyrole_key(Y),
				%!.
b5kif_key2typex(Key,value-Ty) :- b5inst_get(Key,Value),!,
				b5inst_dom(Value,DomKey),
				b5kif_key2type(DomKey,(conc-Ty,_)).

t5concid_kif_type(Key,T) :-
	t5cdb_get(Key,Conc),
	t5conc_kif_type(Conc,T).

t5conc_kif_type(Conc,T) :-
	t5conc_nf(Conc,NF),
	b5nf_kif_type(NF,T).

b5nf_kif_type(NF,T) :-
	b5nf_type(NF,Y),
	b5nf_domain(NF,D),
	t5conc_map2(Y,D,T).	

t5conc_map2(prim,D,prim/D).
t5conc_map2(number,D,number/D).
t5conc_map2(string,D,string/D).
t5conc_map2(aset-open,D,aset/open/D).
t5conc_map2(aset-close,D,aset/close/D).
t5conc_map2(norm-_,D,conc/D).
t5conc_map2(top,D,top/D).
t5conc_map2(bottom,D,bottom/D).


t5concid_wrap(Key,Wrap) :-
	t5cdb_get(Key,Conc),
	t5conc_wrap(Conc,Wrap).

t5conc_wrap(Conc,Wrap) :-
	t5conc_nf(Conc,NF),
	b5nf_wrap(NF,Wrap).
	
b5nf_wrap(NF,Wrap) :-
	b5nf_type(NF,Type),
	b5nf_domain(NF,DomKey),
	b5nf_xxmap(Type,DomKey,Wrap).

b5nf_xxmap(norm-_,_DomKey,conc) :- !. /* == anything */
b5nf_xxmap(prim,_DomKey,prim) :- !. 
b5nf_xxmap(top,_DomKey,top) :- !. 
b5nf_xxmap(bottom,_DomKey,bottom) :- !. 
%b5nf_xxmap(conc-_,DomKey,conc) :- !. /* == anything */
b5nf_xxmap(_,DomKey,Wrap) :- 
	b5st_class_entry(Wrap,_,DomKey,_),!.

x5role_typemap(p,K,bottom) :- t5tbox_nothing_key(K).
x5role_typemap(p,K,prim/TopRole) :- t5tbox_anyrole_key(TopRole),\+ t5tbox_nothing_key(K).
x5role_typemap(d,_,role).
x5role_typemap(f,_,feature).
x5role_typemap(i,_,inherent_feature).

t5rdb_get(Key,Role,Type) :-
	t5rdb_get(Key,Role),
	x5role_type(Role,T),
	x5role_typemap(T,Key,Type).


t5rdb_get(Key,role(Key,Type,RNF,Filter,Inv,Comps,Transflag)) :- 
t5rdb(Key,Type,RNF,Filter,Inv,Comps,Transflag).

x5role_raw_create(Key,Type,RNF,Filter,Inv,Comps,Transflag,Key,role(Key,Type,RNF,Filter,Inv,Comps,Transflag)).
x5role_p(role(_Key,_Type,_RNF,_Filter,_Inv,_Comps,_Transflag)).

x5role_key(role(Key,_Type,_RNF,_Filter,_Inv,_Comps,_Transflag),Key).
x5role_type(role(_Key,Type,_RNF,_Filter,_Inv,_Comps,_Transflag),Type).
x5role_rnf(role(_Key,_Type,RNF,_Filter,_Inv,_Comps,_Transflag),RNF).
x5role_filter(role(_Key,_Type,_RNF,Filter,_Inv,_Comps,_Transflag),Filter).
x5role_inv(role(_Key,_Type,_RNF,_Filter,Inv,_Comps,_Transflag),Inv).
x5role_comps(role(_Key,_Type,_RNF,_Filter,_Inv,Comps,_Transflag),Comps).
x5role_transflag(role(_Key,_Type,_RNF,_Filter,_Inv,_Comps,Transflag),Transflag).


t(X) :-
b5kif_name_key(Name,T,X),
write(Name),write('  '), write(X),write(' '),write(T),nl,
Y is X+1,
t(Y).

t(_). 

b5kif_entity(Key,E) :- a5odb_get(Key,E),!.
b5kif_entity(Key,E) :- t5cdb_get(Key,E),!.
b5kif_entity(Key,E) :- t5rdb_get(Key,E,_Wrap),!.
b5kif_entity(Key,E) :- b5inst_get(Key,E),!.


/*  module dispatcher */
 
b5kif_module(Key,obj)  :- a5odb_get(Key,_),!.
b5kif_module(Key,conc) :- t5concid_wrap(Key,_Wrap),!.
b5kif_module(Key,role) :- t5rdb_get(Key,_Role,_Wrap),!.
b5kif_module(Key,inst) :- b5inst_get(Key,_Value),!.
		

/* NFX e (nfu,nfi,nfs) */
/* NFX is a selector  */

/* -------------------------------------------------------------------- */
/*  b5kif_vr(+Entity:Key,+Role:Key,+NFX,-VR:Key)			*/
/*  VR is the key of the value restriction for the Entity for		*/ 
/*  the role Role wrt the selector.					*/
/*  The entity is either a concept or an object				*/
/* -------------------------------------------------------------------- */

b5kif_vr(Key,Role,NFX,VR) :-
	b5kif_module(Key,Mod),
	b5kif_vr(Mod,Key,Role,NFX,VR).

b5kif_vr(conc,Key,Role,NFX,VR) :-
	b5kif_nfx_map(NFX,Key,IKey),
	t5concid_vr(IKey,Role,VR).
b5kif_vr(obj,Key,Role,NFX,VR) :-
	a5objid_vr(NFX,Key,Role,VR).

/* -------------------------------------------------------------------- */
/*  b5kif_nr(+Entity:Key,+Role:Key,+NFX,-(Min,Max))			*/
/*  Min,Max are the number restriction of the entity Entity		*/ 
/*  on the role Role wrt the selector.					*/
/*  the representation of +infinity is in.				*/
/*  The entity is either a concept or an object				*/
/* -------------------------------------------------------------------- */

b5kif_nr(Key,Role,NFX,MinMax)  :-
	b5kif_module(Key,Mod),
	b5kif_nr(Mod,Key,Role,NFX,MinMax).

b5kif_nr(conc,Key,Role,NFX,(Min,Max)) :-
	b5kif_nfx_map(NFX,Key,IKey),
	t5concid_min_max(IKey,Role,Min,Max).
b5kif_nr(obj,Key,Role,NFX,(Min,Max)) :-
	a5objid_min_max(NFX,Key,Role,Min,Max).

/* -------------------------------------------------------------------- */
/*  b5kif_fillers(+Entity:Key,+Role:Key,+NFX,-Fillers:list(Key))	*/
/*  Fillers is a list of fillers at the entity Entity			*/
/*  on the role Role wrt the selector.					*/
/*  The fillers are represented by Keys 				*/
/*  The entity is either a concept or an object				*/
/* -------------------------------------------------------------------- */

b5kif_fillers(Key,Role,NFX,Fillers)  :-
	b5kif_module(Key,Mod),
	b5kif_fillers(Mod,Key,Role,NFX,Fillers).

b5kif_fillers(conc,Key,Role,NFX,Fillers) :-
	b5kif_nfx_map(NFX,Key,IKey),
	t5concid_fillers(IKey,Role,Fillers).

b5kif_fillers(obj,Key,Role,NFX,Fillers) :-
	a5objid_fillers(NFX,Key,Role,Fillers).

/* -------------------------------------------------------------------- */
/*  b5kif_msc(+Entity:Key,+NFX,-MSC:list(Key))				*/
/*  MSC (most specific concept) is list of Keys	of conceptual type.     */
/*  The conjunction of that keys represents the most specific concept   */
/*  of the entity Entity.		 				*/
/*  The entity is either a value (attribute or number) or an object	*/
/* -------------------------------------------------------------------- */

b5kif_msc(Key,NFX,MSC) :-
	b5kif_module(Key,Mod),
	b5kif_msc(Mod,Key,NFX,MSC).

b5kif_msc(obj,Key,NFX,MSC) :-
	%a5objid_direct_supers(Key,NFX,user_defined,MSC).
	a5objid_direct_supers(Key,NFX,named,MSC).

b5kif_msc(inst,Key,_NFX,MSC) :-
	%b5inst_direct_supers(Key,user_defined,MSC).
	b5inst_direct_supers(Key,named,MSC).

b5kif_nfx_map(nfi,Key,IKey) :- 
	!,
	i5ibox_get_ibox_conc(Key,IKey).

b5kif_nfx_map(_,Key,Key).

/* -------------------------------------------------------------------- */
/*  b5kif_extension(+Conc:Key,+NFX,-Ext:list(Key))			*/
/*  Ext  is the extension of the concept Conc, 				*/
/*  only the tbox is used 						*/ 
/* -------------------------------------------------------------------- */

b5kif_extension(Key,NFX,Extension) :-
	b5kif_nfx_map(NFX,Key,IKey),
	t5concid_extension(IKey,Extension).

b5kif_describe(Key,Sel,Res) :-
	b5kif_module(Key,Mod),
	b5desc_entity(Mod,Sel,mid,Key,Res),!.

b5kif_describe_fully(Key,Sel,Res) :- 
	b5kif_module(Key,Mod),
	b5desc_entity(Mod,Sel,max,Key,Res),!.

/* -------------------------------------------------------------------- */
/*  b5kif_difference(Key1,Key2,Sel,D1,D2)            			*/
/* -------------------------------------------------------------------- */

b5kif_difference(Key1,Key2,SEL,D1,D2) :-
	b5kif_module(Key1,Mod),
	b5desc_difference(Mod,Key1,Key2,SEL,D1,D2).


/* -------------------------------------------------------------------- */
/*  b5key_exists_p(+X:Tset,+K:Key)					*/
/*  Tset = {value,obj,(c,0),(r,0),(n,0),(s,0),(a,N:integer)}		*/
/*  is true iff K is a Key of type X					*/ 
/* -------------------------------------------------------------------- */

b5kif_key_exists_p(obj,Key) :-
	!,
	a5odb_get(Key,_),!.
b5kif_key_exists_p((r,0),Key) :-
	!,
	t5rdb_get(Key,_Role,_),!.
b5kif_key_exists_p(value,Key) :-
	!,
	b5inst_get(Key,_),!.
b5kif_key_exists_p(_,Key) :-
	t5cdb_get(Key,_),!.


b5kif_aset_extension(AsetName,Attributes) :-
	%nonvar(AsetName),
	%var(Attributes),
	b5kif_name_key(AsetName,(conc-aset/_/_,_),Key),
	t5concid_aset_extension(Key,AttributeKeys),
	b5kif_names_keys(Attributes,(value-aset/_/_,_),AttributeKeys).

b5kif_aset_element(AsetName,AttributeName) :-
	b5kif_var(AsetName,AttributeName,XY),
	b5kif_aset_element(XY,AsetName,AttributeName). 

b5kif_aset_element(nv,AsetName,AttributeName) :-
	%nonvar(AsetName),
	%var(AttributeName),
	b5kif_aset_extension(AsetName,Attributes),
	member(AttributeName,Attributes).

b5kif_aset_element(nn,AsetName,AttributeName) :-
	%nonvar(AsetName),
	%nonvar(AttributeName),
	b5kif_name_key(AsetName,(conc-aset/_/_,_),AsetKey),
	b5kif_name_key(AttributeName,(value-aset/_/_,_),AttributeKey),
	b5inst_is_instance_p(AttributeKey,AsetKey).

b5kif_aset_element(vn,AsetName,AttributeName) :-
	%var(AsetName),
	%nonvar(AttributeName),
	b5kif_name_key(AttributeName,(value-aset/_/_,_),AttributeKey),
	b5inst_supers(AttributeKey,SuperKeys),
	b5typelist_filter_in(SuperKeys,t5concid_aset_p,LessSuperKeys),
	member(AsetKey,LessSuperKeys),
	b5kif_name_key(AsetName,(conc-aset/_/_,_),AsetKey).

b5kif_aset_element(vv,OneAsetName,AttributeName) :-
	%var(AsetName),
	%var(AttributeName),
	b5kif_all_aset(OneAsetName),
	b5kif_aset_element(nv,OneAsetName,AttributeName).

/* backtracks over all asets */
b5kif_all_aset(AsetName) :-
	t5dom_all_aset_doms(AllDoms),
	member(Dom,AllDoms),
	b5kif_subsx(Dom,DomSubs),
	b5sort_unify([Dom],DomSubs,X),
	member(AsetKey,X),
	b5kif_name_key(AsetName,(conc-aset/_/_,_),AsetKey).


b5kif_subs_x(conc,N,Subs) :- 
	t5concid_subs(N,Subs).
b5kif_subs_x(role,N,Subs) :- 
	t5role_subs(N,Subs).

b5kif_subsx(N,Subs) :-
	b5kif_module(N,Mod),
	b5kif_subs_x(Mod,N,S), 
	t5tbox_nothing_key(Nothing),
	b5sort_difference(S,[Nothing],Subs).

b5kif_direct_subsx(N,Subs) :-
	t5concid_direct_subs(N,S),
	t5tbox_nothing_key(Nothing),
	b5sort_difference(S,[Nothing],Subs).

b5kif_direct_subsx(N,Filter,Subs) :-
	t5concid_direct_subs(N,Filter,S),
	t5tbox_nothing_key(Nothing),
	b5sort_difference(S,[Nothing],Subs).

b5kif_super_aset(Name1,Name2) :-
	b5kif_var(Name1,Name2,XY),
	b5kif_super_aset(XY,Name1,Name2).

b5kif_super_aset(nn,Name1,Name2) :-
	b5kif_name_key(Name1,(conc-aset/_/_,_),Key1),
	b5kif_name_key(Name2,(conc-aset/_/_,_),Key2),
	t5concid_subsumes_p(Key1,Key2).

b5kif_super_aset(vn,Name1,Name2) :-
	b5kif_name_key(Name2,(conc-aset/_/_,_),Key2),
	b5kif_subsx(Key2,Subs),
	member(Sub,Subs),
	b5kif_name_key(Name1,(conc-aset/_/_,_),Sub).

b5kif_super_aset(nv,Name1,Name2) :-
	b5kif_name_key(Name1,(conc-aset/_/_,_),Key1),
	t5concid_aset_p(Key1),
	t5concid_supers(Key1,Supers),
	member(Key2,Supers),
	t5concid_aset_p(Key2), % no not remove this
	b5kif_name_key(Name2,(conc-aset/_/_,_),Key2).

b5kif_super_aset(vv,Name1,Name2) :-
	b5kif_all_aset(Name1),
	b5kif_super_aset(nv,Name1,Name2). 


key_view(K,V) :- (b5kif_name_key(N,_,K)->V=N;V=key(K)).

b5nf_some_role_closed(NF,Roles,N_NF,Flag) :-
	b5nf_some_role_closed_i(Roles,NF,N_NF,Flag).

b5nf_some_role_closed_i([],NF,NF,no) :- !.
b5nf_some_role_closed_i([Role|Roles],NF,N_NF,Flag) :- 
	b5nf_closed(NF,Role,_,N_NF1,Flag1),
	(Flag1 == yes -> Flag=Flag1,N_NF=N_NF1
		;
		b5nf_some_role_closed_i(Roles,N_NF1,N_NF,Flag)
	). 


/* ------------------------------------------------------------ */
/* mark 							*/
/* ------------------------------------------------------------ */
b5nf_mark(NF,N_NF) :-
	b5nf_r_rl(NF,RL,N_RL,N_NF),
	t5rl_mark(RL,N_RL).

t5rl_mark(RL,N_RL) :-
	t5rl_r_ress(RL,Ress,N_Ress,N_RL),
	b5typelist_map(Ress,N_Ress,t5res_mark).

t5res_mark(Res,N_Res) :-
	t5res_s_sdcs(Res,un-[],N_Res).

/* ------------------------------------------------------------ */
/* directly_closed 						*/
/* ------------------------------------------------------------ */


%% b5nf_directly_closed(+NF,+Role,-Res,-NewNF,-Flag)
%%  where Flag is one of {yes,no}

b5nf_directly_closed(NF,Role,Res,N_NF,Flag) :-
	b5nf_r_rl(NF,RL,N_RL,N_NF),
	t5rl_directly_closed(RL,Role,Res,N_RL,Flag).
	
t5rl_directly_closed(RL,Role,Res,N_RL,Flag) :-
	t5rl_r_ress(RL,Ress,N_Ress,N_RL),
	t5ress_directly_closed(Ress,Role,Res,N_Ress,Flag).

t5ress_directly_closed(Ress,Role,N_Res,F_Ress,Flag) :-
	b5typelist_r_ele(Ress,Role,O_Res,N_Res,N_Ress),
	(var(O_Res) -> 
		F_Ress = Ress, Flag = no
	;
		F_Ress = N_Ress,
		t5res_get_state(O_Res,State,N_State,N_Res),
 		(State = cl-_ -> Flag = yes ; Flag = no),
		N_State = State
	).
			
t5res_get_state(Res,State,N_State,N_Res) :-
	t5res_raw_create(R,VR,NR,Fs,RVM,SDCS,Res),
	t5res_raw_create(R,VR,NR,Fs,RVM,N_State,N_Res),
	(SDCS == un-[] -> t5res_fixstate_h(Fs,NR,State);State = SDCS).

/* ------------------------------------------------------------ */
/* indirectly_closed 						*/
/* ------------------------------------------------------------ */

%% b5nf_indirectly_closed(+NF,+Role,-DirectClosedSupers,-NewNF,-Flag) 
%%  where Flag is one of {yes,no}

b5nf_indirectly_closed(NF,Role,DCS,N_NF,Flag) :-
	b5nf_r_rl(NF,RL,N_RL,N_NF),
	t5rl_indirectly_closed(RL,Role,DCS,N_RL,Flag).

t5rl_indirectly_closed(RL,Role,DCS,N_RL,Flag) :-
	t5rl_r_ress(RL,Ress,N_Ress,N_RL),
	t5ress_indirectly_closed(Ress,Role,DCS,N_Ress,Flag).

t5ress_indirectly_closed(Ress,Role,DCS,F_Ress,Flag) :-
	b5typelist_r_ele(Ress,Role,O_Res,N_Res,N_Ress1),
	(var(O_Res) ->
		DCS = [],F_Ress=Ress,Flag=no
	;
		t5res_get_state(O_Res,State,N_State,N_Res),
		t5ress_indi_state(State,N_State,N_Ress1,DCS,Role,F_Ress,Flag)
	).

t5ress_indi_state(icl-DCS,icl-DCS,Ress,DCS,_Role,Ress,yes) :- !.
t5ress_indi_state(ffs-_,State,Ress,DCS,Role,N_Ress,Flag) :-
	!,	
	t5role_direct_supers(Role,Supers),
	t5rl_filter(Supers,LessSupers),
	t5ress_indirectly_cl(LessSupers,Ress,DCS,N_Ress,Flag),
	(Flag == yes -> State = icl-DCS; State = op-[]). 

t5ress_indi_state(X,X,Ress,[],_Role,Ress,no) :- !.

t5ress_indirectly_cl([],Ress,[],Ress,no) :- !.
t5ress_indirectly_cl(Supers,Ress,DCS,N_Ress,Flag) :-
	t5ress_testall_cl(Supers,Ress,DCS,N_Ress,Flag).

t5ress_testall_cl([],Ress,[],Ress,yes).
t5ress_testall_cl([Role|Roles],Ress,DCS,N_Ress,Flag) :-
	t5ress_closed(Ress,Role,Supers1,N_Ress1,Flag1),
	(
	Flag1 == no  -> !,Flag = Flag1,DCS= [],N_Ress=N_Ress1 %1111 (Ress) 
	;
	t5ress_testall_cl(Roles,N_Ress1,Supers2,N_Ress,Flag),
	(Flag == yes -> !,
		b5sort_unify(Supers1,Supers2,DCS)
		; DCS = []
	)
	).

	
/* ------------------------------------------------------------ */
/* closed 							*/
/* ------------------------------------------------------------ */
 
%% b5nf_closed(+NF,+Role,-DirectClosedSupersOrRole,-NewNF,-Flag)
%%  where Flag is one of {yes,no}

b5nf_closed(NF,Role,DCS,N_NF,Flag) :-
	b5nf_r_rl(NF,RL,N_RL,N_NF),
	t5rl_closed(RL,Role,DCS,N_RL,Flag).

t5rl_closed(RL,Role,DCS,N_RL,Flag) :-
	t5rl_r_ress(RL,Ress,N_Ress,N_RL),
	t5ress_closed(Ress,Role,DCS,N_Ress,Flag). 

t5ress_closed(Ress,Role,DCS,F_Ress,Flag) :-
	b5typelist_r_ele(Ress,Role,O_Res,N_Res,N_Ress1),
	(var(O_Res) ->
		DCS =[],F_Ress = Ress,Flag = no
	;
		t5res_get_state(O_Res,State,N_State,N_Res),
   		(State = cl-_ -> !,Flag = yes,DCS = [Role],N_State = State,F_Ress = N_Ress1
			;
		t5ress_indi_state(State,N_State,N_Ress1,DCS,Role,F_Ress,Flag)
		)
	).	


/* ------------------------------------------------------------ */
/* hacks */
/* ------------------------------------------------------------ */
t5res_directly_closed_p(Res) :-
t5res_get_state(Res,State,_,_),
State = cl-_.

b5nf_some_role_closed_p(NF,Roles) :- 
	b5nf_some_role_closed(NF,Roles,_N_NF,Flag),!,
	Flag == yes. 


/* ------------------------------------------------------------ */  
/*		conceptnormalform				*/	
/* ------------------------------------------------------------ */  

b5nf_raw_create(T,D,P,NP,RL,NR,X,Num,S,D-nf(T,P-NP,RL,NR-X,Num,S)).
/* T e {empty,top,bottom,prim,num,str,aset-open,aset-close,norm-open,norm-close} */

b5nf_type(_-nf(T,_,_,_,_,_),T).
b5nf_pnp(_-nf(_,PNP,_,_,_,_),PNP).
b5nf_prims(_-nf(_,P-_NP,_,_,_,_),P).
b5nf_neg_prims(_-nf(_,_P-NP,_,_,_,_),NP).
b5nf_rl(_-nf(_,_,RL,_,_,_),RL).
b5nf_nrx(_-nf(_,_,_,NRX,_,_),NRX).
b5nf_nr(_-nf(_,_,_,NR-_,_,_),NR).
b5nf_x(_-nf(_,_,_,_-X,_,_),X).
b5nf_xs(_-nf(_,_,_,_,_,S),S).
b5nf_num(_-nf(_,_,_,_,Num,_),Num).
b5nf_domain(D-_,D).

b5nf_reslist(_-nf(_,_,RL,_,_,_),RL).

b5nf_type_key(NF,Key) :- b5nf_domain(NF,Key).

b5nf_explicit_supers(NF,Supers) :-
	b5nf_xs(NF,Supers).
	
b5nf_min(NF,Min) :-
        b5nf_nr(NF,NR),
        t5nr_min(NR,Min).

b5nf_max(NF,Max) :-
        b5nf_nr(NF,NR),
        t5nr_max(NR,Max).

b5nf_s_type(D-nf(_T,PNP,RL,NRX,N,S),N_T,D-nf(N_T,PNP,RL,NRX,N,S)).
b5nf_s_pnp(D-nf(T,_PNP,RL,NRX,N,S),N_PNP,D-nf(T,N_PNP,RL,NRX,N,S)).
b5nf_s_prims(D-nf(T,_P-NP,RL,NRX,N,S),N_P,D-nf(T,N_P-NP,RL,NRX,N,S)).
b5nf_s_neg_prims(D-nf(T,P-_NP,RL,NRX,N,S),N_NP,D-nf(T,P-N_NP,RL,NRX,N,S)).
b5nf_s_rl(D-nf(T,PNP,_RL,NRX,N,S),N_RL,D-nf(T,PNP,N_RL,NRX,N,S)).
b5nf_s_nrx(D-nf(T,PNP,RL,_NRX,N,S),N_NRX,D-nf(T,PNP,RL,N_NRX,N,S)).
b5nf_s_nr(D-nf(T,PNP,RL,_NR-X,N,S),N_NR,D-nf(T,PNP,RL,N_NR-X,N,S)).
b5nf_s_x(D-nf(T,PNP,RL,NR-_X,N,S),N_X,D-nf(T,PNP,RL,NR-N_X,N,S)).
b5nf_s_num(D-nf(T,PNP,RL,NRX,_N,S),N_N,D-nf(T,PNP,RL,NRX,N_N,S)).
b5nf_s_xs(D-nf(T,PNP,RL,NRX,N,_S),N_S,D-nf(T,PNP,RL,NRX,N,N_S)).
b5nf_s_domain(_D-nf(T,PNP,RL,NRX,N,S),N_D,N_D-nf(T,PNP,RL,NRX,N,S)).
b5nf_s_min(D-nf(T,PNP,RL,NR-X,N,S),Min,D-nf(T,PNP,RL,N_NR-X,N,S)) :- 
        t5nr_s_min(NR,Min,N_NR).
b5nf_s_max(D-nf(T,PNP,RL,NR-X,N,S),Max,D-nf(T,PNP,RL,N_NR-X,N,S)) :- 
        t5nr_s_max(NR,Max,N_NR).

b5nf_r_type(D-nf(T,PNP,RL,NRX,N,S),T,N_T,D-nf(N_T,PNP,RL,NRX,N,S)).
b5nf_r_pnp(D-nf(T,PNP,RL,NRX,N,S),PNP,N_PNP,D-nf(T,N_PNP,RL,NRX,N,S)).
b5nf_r_prims(D-nf(T,P-NP,RL,NRX,N,S),P,N_P,D-nf(T,N_P-NP,RL,NRX,N,S)).
b5nf_r_neg_prims(D-nf(T,P-NP,RL,NRX,N,S),NP,N_NP,D-nf(T,P-N_NP,RL,NRX,N,S)).
b5nf_r_rl(D-nf(T,PNP,RL,NRX,N,S),RL,N_RL,D-nf(T,PNP,N_RL,NRX,N,S)).
b5nf_r_nrx(D-nf(T,PNP,RL,NRX,N,S),NRX,N_NRX,D-nf(T,PNP,RL,N_NRX,N,S)).
b5nf_r_nr(D-nf(T,PNP,RL,NR-X,N,S),NR,N_NR,D-nf(T,PNP,RL,N_NR-X,N,S)).
b5nf_r_x(D-nf(T,PNP,RL,NR-X,N,S),X,N_X,D-nf(T,PNP,RL,NR-N_X,N,S)).
b5nf_r_num(D-nf(T,PNP,RL,NRX,N,S),N,N_N,D-nf(T,PNP,RL,NRX,N_N,S)).
b5nf_r_xs(D-nf(T,PNP,RL,NRX,N,S),S,N_S,D-nf(T,PNP,RL,NRX,N,N_S)).
b5nf_r_domain(D-nf(T,PNP,RL,NRX,N,S),D,N_D,N_D-nf(T,PNP,RL,NRX,N,S)).
b5nf_r_min(D-nf(T,PNP,RL,NR-X,N,S),Min,N_Min,D-nf(T,PNP,RL,N_NR-X,N,S)) :- 
        t5nr_r_min(NR,Min,N_Min,N_NR).
b5nf_r_max(D-nf(T,PNP,RL,NR-X,N,S),Max,N_Max,D-nf(T,PNP,RL,N_NR-X,N,S)) :- 
        t5nr_r_max(NR,Max,N_Max,N_NR).


b5nf_init :- 
	retractall(b5nf_empty(_)),
	b5nf_create_empty(NF),
	assert(b5nf_empty(NF)).

b5nf_create_empty(_-nf(_,[]-[],RL,NR-[],Num,[])) :- 
	t5rl_create(RL), 
	t5nr_create(NR), 
	t5number_create(Num).

b5nf_prim_p(_D-nf(prim,[Key]-[],_,_,_,_),Key). 

b5nf_create_prim(Key,_D-nf(prim,[Key]-[],RL,NR-[],Num,[])) :- t5number_create(Num),t5rl_create(RL),t5nr_create(NR).
b5nf_create_prim(Key,D,D-nf(prim,[Key]-[],RL,NR-[],Num,[])) :- t5number_create(Num),t5rl_create(RL),t5nr_create(NR).
b5nf_create_top(Key,Key-nf(top,[]-[],RL,NR-[],Num,[])) :- t5number_create(Num),t5rl_create(RL),t5nr_create(NR).
b5nf_create_bottom(Key,D,D-nf(bottom,[Key]-[],RL,N_NR-[],Num,[])) :- t5number_create(Num),t5rl_create(RL),t5nr_create(NR), t5nr_add_max(NR,0,N_NR).
b5nf_create_bottom(Key,_D-nf(bottom,[Key]-[],RL,N_NR-[],Num,[])) :- t5number_create(Num),t5rl_create(RL),t5nr_create(NR), t5nr_add_max(NR,0,N_NR).

b5nf_create_top_number(P,NP,_Key-nf(number,P-NP,RL,NR-[],Num,[])) :- t5rl_create(RL),t5nr_create(NR),t5number_create(Num).
b5nf_create_top_string(P,NP,_Key-nf(string,P-NP,RL,NR-[],Num,[])) :- t5rl_create(RL),t5nr_create(NR),t5number_create(Num).
b5nf_create_top_concept(P,NP,_Key-nf(norm-open,P-NP,RL,NR-[],Num,[])) :- t5rl_create(RL),t5nr_create(NR),t5number_create(Num).
b5nf_create_top_aset(P,NP,_Key-nf(aset-open,P-NP,RL,NR-[],Num,[])) :- t5rl_create(RL),t5nr_create(NR),t5number_create(Num).
b5nf_create_new_open_aset(P,NP,_Key-nf(aset-open,P-NP,RL,NR-[],Num,[])) :- t5rl_create(RL),t5nr_create(NR),t5number_create(Num).
b5nf_create_new_closed_aset(P,NP,Count,X,_Key-nf(aset-close,P-NP,RL,NNR-X,Num,[])) :- 
	t5rl_create(RL),t5nr_create(NR),
	t5nr_add_max(NR,Count,NNR),
	t5number_create(Num).

b5nf_aset_open_p(NF) :- b5nf_type(NF,aset-open).
b5nf_aset_close_p(NF) :- b5nf_type(NF,aset-close).
b5nf_aset_p(NF) :- b5nf_type(NF,aset-_).
b5nf_number_p(NF) :- b5nf_type(NF,number).
b5nf_string_p(NF) :- b5nf_type(NF,string).
b5nf_concept_p(NF) :- b5nf_type(NF,Norm),!,Norm = norm-_.
b5nf_concept_open_p(NF) :- b5nf_type(NF,Norm),!,Norm == norm-open.
b5nf_concept_close_p(NF) :- b5nf_type(NF,Norm),!,Norm == norm-close.
b5nf_close_p(NF) :- b5nf_type(NF,_-close).
b5nf_open_p(NF) :- b5nf_type(NF,_-open).

b5nf_create_conc_nf(NF) :-
	t5dom(anything,Dom,_),
	b5nf_create(Dom,NF).

b5nf_create(Domain,NF) :-
        t5cdb_cnf(Domain,CNF),
        b5nf_s_xs(CNF,[Domain],NF).

b5nf_create(X) :- b5nf_create_empty(X).

b5nf_not(X) :- X,!,fail.
b5nf_not(_) :- !.

b5nf_modified_p(NF1,NF2) :-
	b5nf_raw_create(_T,_D,P,NP,RL1,NR,X,N,_S,NF1),
	b5nf_raw_create(_T,_D,P,NP,RL2,NR,X,N,_S,NF2),
	b5nf_not(t5rl_modified_p(RL1,RL2)),!,fail.

b5nf_modified_p(_,_) :- !.


/* adds ...    */
 

b5nf_add_super_conc(NF0,Key,NF) :-
	b5nf_r_xs(NF0,XSupers0,XSupers,NF),
	ord_add_element(XSupers0,Key,XSupers).

b5nf_add_prim(NF,Prim,F_NF) :- 
        b5nf_r_prims(NF,Prims2,Prims3,F_NF),
        b5sort_unify([Prim],Prims2,Prims3).

b5nf_add_prims(NF,Prims1,F_NF) :- 
        b5nf_r_prims(NF,Prims2,Prims3,F_NF),
        b5sort_unify(Prims1,Prims2,Prims3).

b5nf_add_neg_prim(NF,Neg1,F_NF) :-
        b5nf_r_neg_prims(NF,Neg2,Neg3,F_NF),
        b5sort_unify([Neg1],Neg2,Neg3).

b5nf_add_neg_prims(NF,Neg1,F_NF) :-
        b5nf_r_neg_prims(NF,Neg2,Neg3,F_NF),
        b5sort_unify(Neg1,Neg2,Neg3).

b5nf_add_rl(NF,RL1,F_NF) :-
        b5nf_r_rl(NF,RL2,RL3,F_NF),
        t5rl_unify(RL1,RL2,RL3).

/* inherited by reslist : */

b5nf_add_vr(NF,VR,Role,N_NF) :-
        b5nf_r_rl(NF,RL,N_RL,N_NF),
        t5rl_add_vr(RL,VR,Role,N_RL).

b5nf_add_min(NF,Min,Role,N_NF)  :-
        b5nf_r_rl(NF,RL,N_RL,N_NF),
        t5rl_add_min(RL,Min,Role,N_RL).

b5nf_add_max(NF,Max,Role,N_NF)  :-
        b5nf_r_rl(NF,RL,N_RL,N_NF),
        t5rl_add_max(RL,Max,Role,N_RL).

b5nf_add_nr_res(NF,NumKey,RoleKey,N_NF) :-
        b5nf_r_rl(NF,RL,N_RL,N_NF),
	t5rl_add_nr_res(RL,NumKey,RoleKey,N_RL).


/*
b5nf_add_fillers(NF,Fs,Role,New,N_NF)  :-
	b5nf_add_atleast(NF,1,Role,NF1),
        b5nf_r_rl(NF1,RL,N_RL,N_NF),
	sort(Fs,FFs),
        t5rl_add_fillers(RL,FFs-New,Role,N_RL).

b5nf_add_fillers(NF,Fs,Role,N_NF)  :-
	b5nf_add_atleast(NF,1,Role,NF1),
        b5nf_r_rl(NF1,RL,N_RL,N_NF),
	sort(Fs,FFs),
        t5rl_add_fillers(RL,FFs-_,Role,N_RL).

%% 27.1.93 -cmk-
*/

b5nf_add_fillers(NF,Fs,Role,N_NF)  :-
	b5nf_add_fillers(NF,Fs,Role,_,N_NF).

b5nf_add_fillers(NF,Fs,Role,New,N_NF)  :-
	sort(Fs,FFs),
	length(FFs,N),
	(N == 0 ->
	    N_NF = NF
	;
	b5nf_add_atleast(NF,N,Role,NF1),
        b5nf_r_rl(NF1,RL,N_RL,N_NF),
        t5rl_add_fillers(RL,FFs-New,Role,N_RL)).

b5nf_add_closed_fillers(NF,Fs,Role,N_NF) :-
	b5nf_add_atleast(NF,1,Role,NF1),
        b5nf_r_rl(NF1,RL,N_RL,N_NF),
	sort(Fs,FFs),
	t5rl_add_closed_fillers(RL,FFs-_,Role,N_RL).

b5nf_add_closed_fillers(NF,Fs,Role,New,N_NF) :-
	b5nf_add_atleast(NF,1,Role,NF1),
        b5nf_r_rl(NF1,RL,N_RL,N_NF),
	sort(Fs,FFs),
	t5rl_add_closed_fillers(RL,FFs-New,Role,N_RL).

b5nf_close_role_filler_set(NF,Role,N_NF) :-
        b5nf_r_rl(NF,RL,N_RL,N_NF),
	t5rl_close_role_filler_set(RL,Role,N_RL).

/*
b5nf_add_equal(NF,Role,Eq,N_NF) :-
	b5nf_r_rl(NF,RL,F_RL,N_NF),
	t5rl_add_equals(RL,[Eq],Role,N_RL),
	t5role_inv_role(Role,Inv),
	(integer(Inv) ->
		t5role_inv_role(Eq,InvEq),
		(integer(InvEq) ->
			t5rl_add_equals(N_RL,[InvEq],Inv,F_RL);
			F_RL=N_RL
		)

	;
		F_RL = N_RL
	).
*/

b5nf_add_equal(NF,Role,Eq,N_NF) :-
	b5nf_r_rl(NF,RL,N_RL,N_NF),
	t5rl_add_equals(RL,[Eq],Role,N_RL).

b5nf_add_equals(NF,Eqs,Role,N_NF)  :-
        b5nf_r_rl(NF,RL,N_RL,N_NF),
        t5rl_add_equals(RL,Eqs,Role,N_RL).

b5nf_add_supers(NF,Supers,Role,N_NF)  :-
        b5nf_r_rl(NF,RL,N_RL,N_NF),
        t5rl_add_supers(RL,Supers,Role,N_RL).

b5nf_add_subs(NF,Subs,Role,N_NF)  :-
        b5nf_r_rl(NF,RL,N_RL,N_NF),
        t5rl_add_subs(RL,Subs,Role,N_RL).

b5nf_add_min_card(NF,Min,F_NF) :-
        b5nf_r_nr(NF,MinMax,N_MinMax,F_NF),
        t5nr_add_min(MinMax,Min,N_MinMax).

b5nf_add_max_card(NF,Max,F_NF) :-
        b5nf_r_nr(NF,MinMax,N_MinMax,F_NF),
        t5nr_add_max(MinMax,Max,N_MinMax).

b5nf_add_extension(NF,Ext1,F_NF) :- 
        b5nf_r_x(NF,Ext2,Ext3,F_NF),
        b5sort_unify(Ext1,Ext2,Ext3).


/* subcomponents */
b5nf_role_res(NF,Role,RoleRes) :- 
        b5nf_rl(NF,ResList),
        t5rl_role_res(ResList,Role,RoleRes).

b5nf_vr(NF,Role,VR) :-
        b5nf_rl(NF,RL),
        t5rl_vr(RL,Role,VR).

b5nf_nr(NF,Role,NR) :-
        b5nf_rl(NF,RL),
        t5rl_nr(RL,Role,NR). /* neu */ 

b5nf_min(NF,Role,Min) :-
        b5nf_rl(NF,RL),
        t5rl_min(RL,Role,Min).

b5nf_max(NF,Role,Max) :-
        b5nf_rl(NF,RL),
        t5rl_max(RL,Role,Max).

b5nf_vr_min_max(NF,Role,VR,Min,Max) :-
        b5nf_rl(NF,RL),
	t5rl_vr_min_max(RL,Role,VR,Min,Max).

b5nf_fillers(NF,Role,Fillers) :-
        b5nf_rl(NF,RL),
        t5rl_fillers(RL,Role,Fillers).

b5nf_rvm_p(NF) :-
	b5nf_rl(NF,RL),
	t5rl_rvms_p(RL).

b5nf_rvm_equals(NF,Role,Eqs) :-
        b5nf_rl(NF,RL),
        t5rl_rvm_equals(RL,Role,Eqs).

b5nf_rvm_supers(NF,Role,Supers) :-
        b5nf_rl(NF,RL),
        t5rl_rvm_supers(RL,Role,Supers).

b5nf_rvm_subs(NF,Role,Subs) :-
        b5nf_rl(NF,RL),
        t5rl_subs(RL,Role,Subs).

b5nf_unify(NF1,NF2,NF3) :-
        b5nf_raw_create(aset-close,D,P1,NP1,RL,NR1,X1,Num,S1,NF1),
        b5nf_raw_create(aset-close,D,P2,NP2,RL,NR2,X2,Num,S2,NF2),!,
        b5nf_raw_create(aset-close,D,P3,NP3,RL,NR4,X3,Num,S3,NF3),
        b5sort_unify(P1,P2,P3),
        b5sort_unify(NP1,NP2,NP3),
        t5nr_unify(NR1,NR2,NR3),
	b5sort_unify(S1,S2,S3),
        b5sort_intersect(X1,X2,X3),
	b5sort_card(X3,Card),
	t5nr_add_max(NR3,Card,NR4),!.

b5nf_unify(NF1,NF2,NF3) :-
        b5nf_raw_create(T1,D,P1,NP1,RL1,NR1,X1,Num1,S1,NF1),
        b5nf_raw_create(T2,D,P2,NP2,RL2,NR2,X2,Num2,S2,NF2),
	(T1 == norm-close;T2 == norm-close),!,
        b5nf_raw_create(norm-close,D,P3,NP3,RL3,NR4,X3,Num3,S3,NF3),
        b5sort_unify(P1,P2,P3),
        b5sort_unify(NP1,NP2,NP3),
        t5rl_unify(RL1,RL2,RL3),
        t5nr_unify(NR1,NR2,NR3),
        t5number_unify(Num1,Num2,Num3),
	b5sort_unify(S1,S2,S3),
        b5nf_extension_intersect(X1,X2,X3),
	b5sort_card(X3,Card),
	t5nr_add_max(NR3,Card,NR4),!.	

b5nf_unify(NF1,NF2,NF3) :-
        b5nf_raw_create(T,D,P1,NP1,RL1,NR1,X1,Num1,S1,NF1),
        b5nf_raw_create(T,D,P2,NP2,RL2,NR2,X2,Num2,S2,NF2),!,
        b5nf_raw_create(T,D,P3,NP3,RL3,NR3,X3,Num3,S3,NF3),
        b5sort_unify(P1,P2,P3),
        b5sort_unify(NP1,NP2,NP3),
        t5rl_unify(RL1,RL2,RL3),
        t5nr_unify(NR1,NR2,NR3),
        t5number_unify(Num1,Num2,Num3),
	b5sort_unify(S1,S2,S3),
        b5nf_extension_intersect(X1,X2,X3),!.

b5nf_unify(NF1,_NF2,NF1) :-
	b5nf_type(NF1,bottom),!.

b5nf_unify(_NF1,NF2,NF2) :-
	b5nf_type(NF2,bottom),!.

b5nf_unify(NF1,NF2,NF3) :-
	b5nf_prim_p(NF1,Key),!,
	b5nf_add_prim(NF2,Key,NF3).

b5nf_unify(NF1,NF2,NF3) :-
	b5nf_prim_p(NF2,Key),!,
	b5nf_add_prim(NF1,Key,NF3).

b5nf_unify(_NF1,_NF2,Nothing_NF) :-
	t5tbox_nothing_key(Nothing),
	t5concid_normal_form(Nothing,Nothing_NF).

b5nf_extension_intersect([],X2,X2) :-
	!.

b5nf_extension_intersect([X|X1],[],[X|X1]) :-
	!.

b5nf_extension_intersect([X|X1],X2,X3) :-
	b5sort_intersect([X|X1],X2,X3).


b5nf_sep_inherent_features([],[],[]).
b5nf_sep_inherent_features([Role|Roles],[Role|TailRoles],I_Features) :-
	\+ t5role_inherent_feature_p(Role),
	!,
	b5nf_sep_inherent_features(Roles,TailRoles,I_Features).
b5nf_sep_inherent_features([Role|Roles],TailRoles,[Role|I_Features]) :-
	b5nf_sep_inherent_features(Roles,TailRoles,I_Features).


b5nf_handle_disjoint_roles(NF,N_NF) :-
		%t5tbox_disjoint_roles(+NF,-Roles)
	t5tbox_disjoint_roles(NF,Roles),
	b5nf_sep_inherent_features(Roles,R_and_F,_I_Features),
	% ffs was mit den inherent_features machen ???
	
	b5nf_handle_disjoint_roles(R_and_F,NF,N_NF). 
	
b5nf_handle_disjoint_roles([],NF,NF). 
b5nf_handle_disjoint_roles([Role|Roles],NF,N_NF)  :-
	b5nf_add_atmost(NF,0,Role,NF1),
	b5nf_handle_disjoint_roles(Roles,NF1,N_NF).

% REL is a REL of new -fillers from completetion, i.e. by rvm propagation 
b5nf_complete(NF,C_NF,REL) :-
	b5nf_complete(NF,C_NF),
%	(b5nf_rvm_p(NF) ->
		b5nf_diff_filled_roles_and_fillers(NF,C_NF,[],REL).
%	;
%		REL =[]
%	).

b5nf_complete(NF,FC_NF) :-
	%b5nf_handle_disjoint_roles(NF,NF1),
        %b5nf_r_rl(NF1,RL,FC_RL,C_NF),

        b5nf_r_rl(NF,RL,FC_RL,C_NF),
	
        t5rl_complete(RL,C_RL),
        t5rl_down(C_RL,NF,NC_RL,Flag),
	t5rl_r_state(NC_RL,state(c,X),state(S,X),FC_RL),
	(
	Flag == yes -> 
		S = i,
		b5nf_complete(C_NF,FC_NF)
		; S = c,FC_NF = C_NF
	).


b5nf_compare_without_prims(none,_,_,none) :- !. 
b5nf_compare_without_prims(Sub3,NF1,NF2,Sub7) :-
	b5nf_raw_create(_T1,_D1,_P1,_NP1,RL1,NR1,X1,Num1,_S1,NF1),
	b5nf_raw_create(_T2,_D2,_P2,_NP2,RL2,NR2,X2,Num2,_S2,NF2),
	t5nr_compare(Sub3,NR1,NR2,Sub4),
        Sub4 \== none,
        t5number_compare(Sub4,Num1,Num2,Sub5),
        Sub4 \== none,
        b5nf_extension_compare(Sub5,X1,X2,Sub6),
        Sub6 \== none,
	t5rl_compare(Sub6,RL1,RL2,Sub7),!.
b5nf_compare_without_prims(_,_,_,none) :- !. 


b5nf_compare(none,_,_,none) :- !.
b5nf_compare(Sub1,NF1,NF2,Sub9) :-
        b5nf_raw_create(prim,_D1,P1,NP1,_RL1,_NR1,_X1,_Num1,_S1,NF1),
        b5nf_raw_create(_T2,_D2,P2,NP2,_RL2,_NR2,_X2,_Num2,_S2,NF2),
        b5sort_compare(Sub1,P1,P2,Sub2),
        b5sort_compare(Sub2,NP1,NP2,Sub9),!.


b5nf_compare(Sub1,NF1,NF2,Sub9) :-
        b5nf_raw_create(prim,_D2,P2,NP2,_RL2,_NR2,_X2,_Num2,_S2,NF2),
        b5nf_raw_create(_,_D1,P1,NP1,_RL1,_NR1,_X1,_Num1,_S1,NF1),
        b5sort_compare(Sub1,P1,P2,Sub2),
        b5sort_compare(Sub2,NP1,NP2,Sub9),!.


b5nf_compare(Sub0,NF1,NF2,Sub9) :-
        b5nf_raw_create(T1,_D1,P1,NP1,RL1,NR1,X1,Num1,_S1,NF1),
        b5nf_raw_create(T2,_D2,P2,NP2,RL2,NR2,X2,Num2,_S2,NF2),
	b5nf_type_compare(Sub0,T1,T2,Sub1,Stop),
	(Stop == yes -> Sub9 = Sub1
	;
        Sub1 \== none,
        b5sort_compare(Sub1,P1,P2,Sub2),
        Sub2 \== none,
        b5sort_compare(Sub2,NP1,NP2,Sub3),
        Sub3 \== none,
        t5rl_compare(Sub3,RL1,RL2,Sub4),
        Sub4 \== none,
        t5nr_compare(Sub4,NR1,NR2,Sub5),
        Sub5 \== none,
        b5nf_extension_compare(Sub5,X1,X2,Sub8),
        t5number_compare(Sub8,Num1,Num2,Sub9)),!.
b5nf_compare(_,_,_,none).

b5nf_type_compare(F,T-_,T-_,FF,no) :- !,F = FF. 
b5nf_type_compare(F,T,T,FF,no) :- !,F = FF. 
b5nf_type_compare(first,_,top,first,yes) :- !.
b5nf_type_compare(second,top,_,second,yes) :- !.
b5nf_type_compare(first,bottom,_,first,yes) :- !.
b5nf_type_compare(second,_,bottom,second,yes) :- !.

b5nf_type_compare(equal,top,_,second,yes) :- !. %_ \== top !
b5nf_type_compare(equal,_,top,first,yes) :- !.

b5nf_type_compare(equal,bottom,_,first,yes) :- !.
b5nf_type_compare(equal,_,bottom,second,yes) :- !.


b5nf_type_compare(_,_,_,none).



b5nf_extension_compare(Sub1,X,[],Sub2) :- 
	X \== [],
	!,
	t5sub_unify(Sub1,first,Sub2).

b5nf_extension_compare(Sub1,[],X,Sub2) :-
	X \== [],
	!,
	t5sub_unify(Sub1,second,Sub2).

b5nf_extension_compare(Sub1,Ex1,Ex2,Sub2) :-
	t5sub_reverse(Sub1,SubRev1),
	b5sort_compare(SubRev1,Ex1,Ex2,SubRev2),
	t5sub_reverse(SubRev2,Sub2),
	!.

b5nf_extension_compare(_,_,_,none).

b5nf_subsumes_p(NF1,NF2) :-
	b5nf_raw_create(_T1,_D1,P1,NP1,RL1,NR1,X1,Num1,_S1,NF1),
	b5nf_raw_create(_T2,_D2,P2,NP2,RL2,NR2,X2,Num2,_S2,NF2),
        b5sort_subset_p(P1,P2),!,
        b5sort_subset_p(NP1,NP2),!,
        t5rl_subsumes_p(RL1,RL2),!,
        t5nr_subsumes_p(NR1,NR2),!,
        b5nf_extension_subsumes_p(X1,X2),
        t5number_subsumes_p(Num1,Num2),!.

b5nf_extension_subsumes_p([],_).

b5nf_extension_subsumes_p([E1|Ext1],[E2|Ext2]) :-
	b5sort_subset_p([E2|Ext2],[E1|Ext1]).

b5nf_instance_of_p(NF1,NF2) :-
        b5nf_subsumes_p(NF1,NF2).

/*	
b5nf_incoherent_p(NF) :-
	b5nf_raw_create(_T,_D,P,NP,RL,MM,Ext,Num,_S1,NF),
	t5tbox_nothing_key(Nothing),
	(
	var(MM) -> t5nr_create(MMM) ; MMM = MM
	),
	t5nr_min(MMM,Min), 
	%t5nr_max(MMM,Max),
	b5sort_card(Ext,Card),
	(b5sort_common_member_p(P,NP);
	t5rl_incoherent_p(RL);           
	b5sort_member_p(Nothing,P);
	%  Card > Max ,
	 Card < Min;    
	 t5nr_incoherent_p(MM);
 	t5number_incoherent_p(Num)
	 ), !.
*/

b5nf_incoherent_p(NF) :-
	b5nf_type(NF,bottom),!.

b5nf_incoherent_p(NF) :-
	b5nf_aset_p(NF),!,
	(
		b5nf_aset_close_p(NF) ->
			b5nf_x(NF,[])
			/* else (i.e. open aset) */
		;	(
				b5nf_x(NF,[]) -> 	
					\+ b5nf_max(NF,in)	
				; fail /* false*/ 
			)
	).
	
b5nf_incoherent_p(NF) :-
	b5nf_raw_create(_T,_D,P,NP,RL,MM,Ext,Num,_S1,NF),
	t5tbox_nothing_key(Nothing),
	(var(MM) -> t5nr_create(MMM) ; MMM = MM ),
	t5nr_min(MMM,Min), 
	t5nr_max(MMM,Max),
	b5sort_card(Ext,Card),
	(
		Max == 0 

	;	(Max == in -> /*false*/ fail ; (b5nf_concept_p(NF) -> t5point_gt_p(Max,Card) ; false))
	;	b5sort_common_member_p(P,NP)
	;	t5rl_incoherent_p(RL)          
	;	b5sort_member_p(Nothing,P)
		/* Card > Max ,*/
		/* Card < Min;   */
	;	t5point_lt_p(Card,Min)
	; 	t5nr_incoherent_p(MM)
	;	t5number_incoherent_p(Num)
	), !.


b5nf_disjoint_p(NF1,NF2) :-
	b5nf_unify(NF1,NF2,NF3),
	b5nf_complete(NF3,NF4), %% ????
	b5nf_incoherent_p(NF4).

b5nf_filled_roles_and_fillers(NF,REL) :- 
         b5nf_reslist(NF,RL),
         b5rel_build_rel(RL,REL).

b5nf_diff_filled_roles_and_fillers(NF1,NF2,Diff1,Diff2) :-
	b5nf_filled_roles_and_fillers(NF1,REL1),
	b5nf_filled_roles_and_fillers(NF2,REL2),
	b5rel_diff(REL1,REL2,Diff1,Diff2).


/* 1. version : no explicit completion */
b5nf_known(NF,Role,Known) :-
        b5nf_fillers(NF,Role,Fillers),
        b5sort_card(Fillers,Known).

/* 2. version : with completetion */
b5nf_known(NF,Role,Known,C_NF) :-
        b5nf_complete(NF,C_NF),
        b5nf_fillers(C_NF,Role,Fillers),
        b5sort_card(Fillers,Known).

b5nf_direct_supers(NF,Filter,DS) :-
	b5nf_supers(NF,Supers), 
	t5tbox_conc_hc(HC),
	t5hc_minimize_special(HC,Supers,Filter,DS).

b5nf_direct_supers(NF,DS) :-
	b5nf_supers(NF,Supers), 
	t5tbox_conc_hc(HC),
	t5hc_minimize_special(HC,Supers,DS).

/*
b5nf_supers(NF,AllSupers) :-
	t5tbox_conc_hc(HC),
	b5nf_complete(NF,CNF),
        %t5cls_conc(C_NF,Supers,[],AllSupers,AllSubs,Disj,Key,OldOrNew),
	t5cls_class(conc,CNF,[],[],[],Supers,_,_,Key,Status),
	(Status == old -> t5concid_supers(Key,AllSupers) 
	; 
	t5hc_super_union_id(HC,Supers,AllSupers)
	).
*/

b5nf_supers(NF,Supers) :-
	b5nf_supers(NF,Supers,_,_).

b5nf_supers(NF,Supers,Key,Status) :-
	t5tbox_conc_hc(HC),
	b5nf_xs(NF,ToldSupers),
	b5nf_complete(NF,CNF),
	t5cls_super_concs(CNF,ToldSupers,AllSupers,Key,Status),
	(Status == old -> 
		t5hc_supers(HC,Key,Supers)
	;
		Supers = AllSupers
	).


b5nf_supers_plus_eq(NF,Supers) :-
	t5tbox_conc_hc(HC),
	b5nf_xs(NF,ToldSupers),
	b5nf_complete(NF,CNF),
	t5cls_super_concs(CNF,ToldSupers,AllSupers,Key,Status),
	(Status == old -> 
		t5hc_supers(HC,Key,ASupers),
		b5sort_unify(ASupers,[Key],Supers)
	;
		Supers = AllSupers
	).
/*
b5nf_supers_plus_eq(NF,AllSupers) :-
	t5tbox_conc_hc(HC),
	b5nf_complete(NF,CNF),
        %t5cls_conc(C_NF,Supers,[],AllSupers,AllSubs,Disj,Key,OldOrNew),
	t5cls_class(conc,CNF,[],[],[],Supers,_,_,Key,Status),
	(Status == old -> t5concid_supers(Key,X_AllSupers) ,
	 	b5sort_unify(X_AllSupers,[Key],AllSupers)
	; 
	t5hc_super_union_id(HC,Supers,AllSupers)
	).
*/

b5nf_subs(NF,AllSubs) :-
	b5nf_classify(NF,[],_Supers,Subs,_Disj,Key,Status),
	(Status == old -> 
		t5concid_subs(Key,AllSubs) 
	; 
		AllSubs = Subs	
	).


b5nf_known_disjoints(NF,Dis) :-
	b5nf_complete(NF,CNF),
	t5cls_class(conc,CNF,[],[],[],_,_,Dis,_,_).

b5nf_delta(NF1,NF2,RVRl) :-
	b5nf_reslist(NF1,RL1),
	b5nf_reslist(NF2,RL2),
	t5rl_delta(RL1,RL2,RVRl,_).
	/* bis jetzt nur speziellere Rollen, nicht neue */

b5nf_delta_1(NF1,NF2,RVrL) :-
	b5nf_reslist(NF1,RL1),
	b5nf_reslist(NF2,RL2),
	t5rl_delta(RL1,RL2,RVrL1,RVrL2),
%% jetzt speziellere Rollen, und neue !!
%% ggfs. gleich zusammen in einer Liste zurueckgeben ?!
	ord_union(RVrL1,RVrL2,RVrL).

b5nf_new_attributes(NF,Keys,N_NF) :-
        b5nf_r_nrx(NF,NR-X,N_NR-N_X,N_NF),
        b5sort_unify(X,Keys,N_X),
	(b5nf_aset_open_p(NF) -> N_NR = NR ; b5sort_card(N_X,N_NR)).

b5nf_add_aset(NF,Keys,N_NF) :-
	sort(Keys,Skeys),
        b5nf_r_nrx(NF,NR-X,N_NR-N_X,N_NF),
        b5sort_intersect(X,Skeys,N_X),
        b5sort_card(N_X,Max),
	t5nr_add_max(NR,Max,N_NR).
	/* ??????????????????/ open -close ????????????? */

b5nf_add_union(NF,Key1,Key2,N_NF) :-
/* test ob dieselbe domain ? test ob in domain ?? */
        t5concid_extension(Key1,X1),
        t5concid_extension(Key2,X2),
        b5sort_unify(X1,X2,X3),
        b5nf_add_aset(NF,X3,N_NF).

b5nf_add_without(NF,Key1,Key2,N_NF) :-
        t5concid_extension(Key1,X1),
        t5concid_extension(Key2,X2),
        b5sort_difference(X1,X2,X3),
        b5nf_add_aset(NF,X3,N_NF).        

b5nf_add_oneof(NF,Keys,F_NF) :-
	sort(Keys,SKeys),
        b5nf_r_nrx(NF,NR-X,N_NR-N_X,N_NF),
	/* [] =^= *  i.e. all objects*/
       	(X == [] -> N_X = SKeys ; b5sort_intersect(X,SKeys,N_X)),
        b5sort_card(N_X,Max),
	/* 8-tung:  Max = 0 -> N_X = [] =^= <empty> i.e. no object */
	t5nr_add_max(NR,Max,N_NR),
	b5nf_r_type(N_NF,norm-_,norm-close,F_NF).


b5nf_add_gt(NF,Num,N_NF) :-
        b5nf_r_num(NF,O,N,N_NF),
        t5number_add_min_open(O,Num,N).

b5nf_add_ge(NF,Num,N_NF) :-
        b5nf_r_num(NF,O,N,N_NF),
        t5number_add_min_close(O,Num,N).

b5nf_add_lt(NF,Num,N_NF) :-
        b5nf_r_num(NF,O,N,N_NF),
        t5number_add_max_open(O,Num,N).

b5nf_add_le(NF,Num,N_NF) :-
        b5nf_r_num(NF,O,N,N_NF),
        t5number_add_max_close(O,Num,N).

b5nf_add_num(NF,Num,N_NF) :-
        b5nf_r_num(NF,O,N,N_NF),
        t5number_add_min_close(O,Num,Z),
        t5number_add_max_close(Z,Num,N).

b5nf_add_fromto(NF,From,To,N_NF) :-
	b5nf_r_num(NF,O,N,N_NF),
	t5number_add_min_close(O,From,NO),
	t5number_add_max_close(NO,To,N).

b5nf_add_number_expr(NF,List,N_NF) :-
	b5nf_add_numberlist(List,NF,N_NF).

b5nf_add_numberlist([],NF,NF).
b5nf_add_numberlist([X|Xs],NF,NNF) :-
	b5nf_add_numberlist(X,Xs,NF,NNF).

b5nf_add_numberlist(fromto(num(N1),num(N2)),L,NF1,NF3) :-
	!, b5nf_add_fromto(NF1,N1,N2,NF2),
	b5nf_add_numberlist(L,NF2,NF3).

b5nf_add_numberlist(gt(N),L,NF1,NF3) :-
	!, b5nf_add_gt(NF1,N,NF2),
	b5nf_add_numberlist(L,NF2,NF3).

b5nf_add_numberlist(ge(N),L,NF1,NF3) :-
	!, b5nf_add_ge(NF1,N,NF2),
	b5nf_add_numberlist(L,NF2,NF3).

b5nf_add_numberlist(lt(N),L,NF1,NF3) :-
	!, b5nf_add_lt(NF1,N,NF2),
	b5nf_add_numberlist(L,NF2,NF3).

b5nf_add_numberlist(le(N),L,NF1,NF3) :-
	!, b5nf_add_le(NF1,N,NF2),
	b5nf_add_numberlist(L,NF2,NF3).

b5nf_add_numberlist(num(N),L,NF1,NF3) :-
	!, b5nf_add_num(NF1,N,NF2),
	b5nf_add_numberlist(L,NF2,NF3).

b5nf_add_numberlist(Key,L,NF1,NF3) :-
	integer(Key),
	b5nf_add_conc(NF1,Key,NF2),
	b5nf_add_numberlist(L,NF2,NF3).

b5nf_ibox_transform(CNF,In,Status,INF) :- 
        b5nf_r_rl(CNF,RL,N_RL,INF),
        t5rl_ibox_transform(RL,In,Status,N_RL).

b5nf_rvrl(NF,RVRL) :-
        b5nf_reslist(NF,RL),
        b5rvrl_build_rvrl(RL,RVRL).

b5nf_add_conc(nf,Key,NF) :-
	!,
        b5nf_create(Key,NF).

b5nf_add_conc(NF,Key,F_NF) :-
        b5nf_xs(NF,S),
        b5sort_unify(S,[Key],NS),
        t5cdb_cnf(Key,NF1),
        b5nf_unify(NF,NF1,N_NF),
        b5nf_s_xs(N_NF,NS,F_NF). 


/*
b5nf_add_atleast(NF1,Min,Role,NF3) :-
	b5nf_xs(NF1,Supers1),
	t5role_domain(Role,Domain),
	t5tbox_conc_hc(HC),
	t5hc_subs(HC,Domain,Subs),
	b5sort_unify([Domain],Subs,AllSubs),
		 ( b5sort_common_member_p(AllSubs,Supers1) ->
			 NF2 = NF1;
			 b5nf_add_conc(NF1,Domain,NF2)
		 ),
	 b5nf_add_min(NF2,Min,Role,NF3).

*/


b5nf_add_atleast(NF1,Min,Role,NF3) :-
	(
	t5point_not_eq_p(Min,0) ->	
		t5role_domain(Role,Domain),
		b5nf_add_conc(NF1,Domain,NF2)
	;
		NF2 = NF1
	),
	b5nf_add_min(NF2,Min,Role,NF3).



b5nf_add_atmost(NF,Max,Role,NNF) :-
	b5nf_add_max(NF,Max,Role,NNF).

b5nf_filter_help(X,[X]) :- atom(X),!.
b5nf_filter_help(X,X).


/*
b5nf_error(Super,AllSupers) :-
	(
	b5sort_subset_p(Super,AllSupers) ->
	true;
	nl,
	write('*** classifier fehler ***'),nl,
	write('Supers :'),write(Super),nl,
	write('AllSupers :'),write(AllSupers),nl,nl,nl
	).
*/
b5nf_classify(NF,Filter,Supers,Subs,Disj,Key,Status) :-
        b5nf_xs(NF,ToldSupers),
        b5nf_complete(NF,C_NF),
        t5cls_class(conc,C_NF,Filter,ToldSupers,[],Supers,Subs,Disj,Key,Status).

b5nf_classify(NF,Key,Status) :-
	b5nf_classify(NF,[],_,_,_,Key,Status).

b5nf_store(NF,Filtr,Key,OldOrNew) :-
	b5nf_store(verbose,NF,Filtr,Key,OldOrNew).

b5nf_store(Verbosity,NF,Filtr,Key,OldOrNew) :-
	b5nf_filter_help(Filtr,Filter),
        b5nf_xs(NF,Supers),
        b5nf_complete(NF,C_NF),
        t5cls_class(conc,C_NF,Filter,Supers,[],AllSupers,AllSubs,Disj,Key,OldOrNew),
        (   OldOrNew = new,
	    !,
            t5concid_new_def(C_NF,Filter,AllSupers,AllSubs,Disj,Key),
	    t5tbox_update_pure_defs(conc,Key)
        ;   (   t5tbox_nothing_key(Key) -> 
		    (Verbosity = silent -> true;t5out_warning(inco))
            ;       t5cdb_store_new_filter(Key,Filter),
		    OldOrNew = old
            )
        ), !.


%b5nf_raw_store(NF,Filter,Supers,Subs,-Key) 
b5nf_raw_store(NF,Filter,Supers,Subs,Key) :-
	b5nf_neg_prims(NF,Disj),
	t5concid_new_def(NF,Filter,Supers,Subs,Disj,Key),
	/* abox flag abfragen fehlt noch */
	a5ind_insert_conc(Key).
	


b5nf_store2(NF,Filter,Key,OldOrNew) :-
        b5nf_xs(NF,Supers),
        b5nf_complete(NF,C_NF),
        t5cls_class(conc,C_NF,Filter,Supers,[],AllSupers,AllSubs,Disj,Key,OldOrNew),
        (OldOrNew = new,
	!,
        t5concid_new_def2(C_NF,Filter,AllSupers,AllSubs,Disj,Key),
	   t5tbox_update_pure_defs(conc,Key)
        ;
        %t5cdb_store_new_filter(Key,Filter),
        OldOrNew = old).


b5nf_new_domain(NF,Filter,Key) :-
        b5nf_xs(NF,Supers),
        b5nf_complete(NF,C_NF),
        t5cls_class(conc,C_NF,Filter,Supers,[],AllSupers,AllSubs,Disj,Key,OldOrNew),
        OldOrNew = new,
        b5nf_s_domain(C_NF,Key,DNF),
        t5concid_new_def(DNF,Filter,AllSupers,AllSubs,Disj,Key),
	t5tbox_update_pure_defs(conc,Key).
 

 /*
b5nf_new_domain(NF,Filter,Key) :-
        b5nf_xs(NF,Supers),
        b5nf_complete(NF,C_NF),
	b5nf_prims(NF,Prims),
	b5nf_negprims(Disj),
	b5sort_unify(Prims,Supers,AllSupers),
    %    t5cls_class(conc,C_NF,Filter,Supers,[],AllSupers,AllSubs,Disj,Key,OldOrNew),
    %    OldOrNew = new,
        b5nf_s_domain(C_NF,Key,DNF),
        t5concid_new_def(DNF,Filter,AllSupers,AllSubs,Disj,Key),
	t5tbox_update_pure_defs(conc,Key). ??????????????
*/ 

 
b5nf_add_minmax(CPF,Min,Max,N_CPF) :-
        b5nf_add_min_card(CPF,Min,CPF1),
        b5nf_add_max_card(CPF1,Max,N_CPF).

b5nf_add_concs(CPF, Keys, NewCPF) :- 
        b5nf_add_concs_hepp(Keys, CPF, NewCPF).

b5nf_add_concs_hepp([],CPF,CPF).

b5nf_add_concs_hepp([Key|Keys], CPF0, CPF) :-
        b5nf_add_conc(CPF0,Key,CPF1),
        b5nf_add_concs_hepp(Keys,CPF1,CPF).

/* 
union(LIC) + N_NF1 + AD = NF1
N_NF1: all non vr-restrictions of NF1; vr changed to range(Role)
AD   : vr-restrictins of NF1 which are more specific than
       the respective restriction in union(LIC)
flag : no iff N_NF1 is fully covered by union(LIC)
       (no = not more specific)
       yes, else.
*/
b5nf_splitting(NF1,LIC /* keys */,N_NF1,AD,Flag) :-
/* yes =^= ex diff */
	b5nf_complete(NF1,CNF1),
        b5nf_create(NF2),
        b5nf_add_concs(NF2,LIC,N_NF2),
        b5nf_complete(N_NF2,C_N_NF2),
        b5nf_rl(C_N_NF2,RL2),
        b5nf_r_rl(CNF1,RL1,N_RL1,N_NF1),
        t5rl_splitting(RL1,RL2,N_RL1,AD),
	(
	b5nf_subsumes_p(N_NF1,N_NF2) -> Flag = no; Flag = yes 
	)
	.


/* new :  20.8.93*/

/* 
union(LIC) + N_NF1 + AD = NF1
N_NF1: all non vr-restrictions of NF1; vr changed to range(Role);
       nr changed to in iff atmost0 is subsumed by union(LIC)
AD   : vr-restrictins of NF1 which are more specific than
       the respective restriction in union(LIC)
AN   : Atmost0 Restrictions not subsumed by union(LIC) 
flag : no iff N_NF1 is fully covered by union(LIC)
       (no = not more specific)
       yes, else.
*/
b5nf_splitting(NF1,LIC /* keys */,N_NF1,AD,AN,Flag) :-
/* yes =^= ex diff */
	b5nf_complete(NF1,CNF1),
        b5nf_create(NF2),
        b5nf_add_concs(NF2,LIC,N_NF2),
        b5nf_complete(N_NF2,C_N_NF2),
        b5nf_rl(C_N_NF2,RL2),
        b5nf_r_rl(CNF1,RL1,N_RL1,N_NF1),
        t5rl_splitting(RL1,RL2,N_RL1,AD,AN),
	(
	b5nf_subsumes_p(N_NF1,N_NF2) -> Flag = no; Flag = yes 
	)
	.

b5nf_map_prim(X,prim(X)).
b5nf_map_negprim(X,negprim(X)).
b5nf_atomize(NF,Atoms) :-
	b5nf_raw_create(_T_,_D_,P,NP,RL,_NR_,_X_,_Num_,_S,NF),
	b5typelist_map(P,P1,b5nf_map_prim),
	b5typelist_map(NP,NP1,b5nf_map_negprim),
	append(P1,NP1,PNP),
	t5rl_atomize(RL,RL1),
	append(PNP,RL1,Atoms).

b5nf_include_role(NF,Role,Supers,Subs,N_NF) :-
	b5nf_r_rl(NF,RL,N_RL,N_NF),
	t5rl_include_role(RL,Role,Supers,Subs,N_RL).

b5nf_infimum(Keys,INF) :-
	(t5keys_domain_p(Keys,Dom) -> b5nf_create(Dom,CPF)  
	 ; b5nf_create(CPF)
	),
	b5nf_add_concs(CPF,Keys,NF),
	b5nf_complete(NF,INF).

b5nf_infimum_key(Keys,InfKey) :-
	b5nf_infimum(Keys,I_NF),
	t5fil_filter(internal,Filter),
	b5nf_store(I_NF,Filter,InfKey,_Status).

t5keys_domain_p([Key|Keys],Dom) :-
(
	t5concid_primitive_p(Key) -> 
		t5keys_domain_p(Keys,Dom)
	;       t5concid_type_key(Key,Dom)
).

b5nf_instantiates_keys_p(NF,Concs) :-
	b5nf_xs(NF, ExplicitSupers),
	b5sort_difference(Concs,ExplicitSupers,Cs),
	( Cs == [] -> true 
	;
		b5nf_infimum(Cs,Inf),
		b5nf_subsumes_p(Inf,NF)
	).

b5nf_used_objects(NF,Objs) :-
	b5nf_x(NF,X),
	b5nf_rl(NF,RL),
	t5rl_used_objects(RL,Y),
	b5sort_unify(X,Y,Objs).

b5nf_new_defined_conc(Key,CPF) :-
	b5nf_create(Key,CPF).

b5nf_new_primitive_conc(Key,CPF):-
		b5nf_create(Key,CPF).

b5nf_nf_minimize(NF,NF) :- !.


b5nf_add_rvm(X,R1,R2,Y) :-
	b5nf_add_equal(X,R1,R2,Y).

%% b5nf_compute_prim_subsumers(+ONF, -MinimizedPrimSubsumers)
%%  Returns MinimizedPrimSubsumers = PrimSubsumers(ONF) \ Supers(ONF)
%%  where PrimSubsumers(ONF) = 
%%  		{ C | C is a rec-cand & 
%%			prim-comps(C) is subset of prim_comps(ONF) & 
%%			neg-prim-comps(C) is subset of neg-prim_comps(ONF) }.
%% NB: t5concid_subs/2 implicitly ensures that all prim-components
%%  are removed from PrimSubsumers(ONF); filter them out explicitly
%%  if this changes!
%% FFS: t5fil_holds_p("C is a recognition_candidate") is not applied
%%  at the moment (since the rec-cand filter is redundant).
%% 
%% cmk 24.8.93: A CandConc must also be prim-subsumed by ONF wrt. 
%%  the negative prim components

b5nf_compute_prim_subsumers(ONF, MinCandConcs) :-
	t5tbox_anything_key(AnythingKey),
	t5concid_subs(AnythingKey,L),
	b5nf_prims(ONF,Prims),
	b5nf_neg_prims(ONF,NegPrims),
	t5concid_compute_prim_subs(L,Prims,NegPrims,CandConcs),
	b5nf_xs(ONF,Supers),
	b5nf_minimize_prim_subsumers(CandConcs, Supers, MinCandConcs).


%% b5nf_minimize_prim_subsumers(+Concs, +ExplicitSupersOfNF, -MinimizedConcs)
%%  True if MinimizedConcs is the subset of Concs consisting of concept-ids
%%  which do NOT subsume any of the concepts in ExplicitSupersOfNF.

b5nf_minimize_prim_subsumers([],_,[]).
b5nf_minimize_prim_subsumers([C|Cs], Supers,MinCs0) :-
	( t5concid_subsumes_some_p(C,Supers) ->
	    MinCs0 = MinCs 
	;   MinCs0 = [C|MinCs]),
	b5nf_minimize_prim_subsumers(Cs,Supers,MinCs).


% --------------------------------------------------------------------
/*
b5nf_complete(NF,FC_NF) :-
	(b5nf_incoherent_p(NF) ->
		t5tbox_nothing_key(Nothing),
		t5concid_normal_form(Nothing,FC_NF)
	;
		b5nf_domain_prop(NF,DNF),
        	b5nf_r_rl(DNF,RL,FC_RL,C_NF),
        	t5rl_complete(RL,C_RL),
        	t5rl_down(C_RL,NF,NC_RL,Flag),
		t5rl_r_state(NC_RL,state(c,X),state(S,X),FC_RL),
		(
		  Flag == yes -> 
			S = i,
			b5nf_complete(C_NF,FC_NF)
			; S = c,FC_NF = C_NF
		)
	).

b5nf_domain_prop(NF,F_NF) :-
	b5nf_r_rl(NF,RL,N_RL,N_NF),		
	t5rl_domain_prop(RL,NF,N_RL,S),
	(S == changed ->
		b5nf_domain_prop(N_NF,F_NF) 
	;
		F_NF = N_NF
	).

t5rl_domain_prop(RL,NF,N_RL,S) :-
	t5rl_r_ress(RL,Ress,N_Ress,N_RL),
	t5ress_domain_prop(Ress,NF,N_Ress,S).


t5ress_domain_prop([],_NF,[],notchanged). 
t5ress_domain_prop([Res|Ress],NF,[N_Res|N_Ress],F_S) :-
	t5res_domain_prop(Res,NF,N_Res,S),
	(S == changed -> N_Ress = Ress,F_S = S % noch mal von vorn !!
	; t5ress_domain_prop(Ress,NF,N_Ress,F_S)
	).

t5res_domain_prop(Res,_NF,Res,notchanged) :-
	t5res_max(Res,0),!.

t5res_domain_prop(R-ES,NF,N_Res,S) :-
	t5role_domain(R,Domain),
	t5concid_normal_form(Domain,Domain_NF),
	(b5nf_disjoint_w_c_p(NF,Domain_NF) -> 
		t5res_add_max(R-ES,0,N_Res),
		S =changed
	 ; N_Res = R-ES,S = notchanged
	).


b5nf_disjoint_w_c_p(NF1,NF2) :-
	b5nf_unify(NF1,NF2,NF3),
	b5nf_incoherent_p(NF3).

%t5tbox_disjoint_roles(+NF,-Roles)
*/


% b5nf_roles_fillers(+NF,+Roles,-Fillers)
b5nf_roles_fillers(NF,Roles,Fillers) :-
	sort(Roles,SRoles),
	b5nf_rl(NF,RL),
	t5rl_roles_fillers(RL,SRoles,Fillers).

/* ------------------------------------------------------------ */ 
/*		      Domains 					*/ 
/* ------------------------------------------------------------ */ 

:- multifile b5dump_these_predicates/1.
b5dump_these_predicates([t5dom/3,t5dom_allprims/1]).

t5dom_init_top(Topkey) :-
        t5conc_init_top(Topkey,Top),
        t5cdb_store(Topkey,Top).

t5dom_init_bottom(Key,Domain) :-
        t5conc_init_bottom(Key,Domain,N_Conc),
	t5cdb_store(Key,N_Conc).

t5dom_init_domains(DomainList) :-
        t5dom_init_prims(DomainList),
        t5dom_init_defs(DomainList).

t5dom_init_keys(X) :-
	t5dom_init_keys(X,Y),
	t5dom_all_prims(store,Y).

t5dom_init_keys([],[]).
t5dom_init_keys([_Name-Prim-_Def|Tail],[Prim|Prims]) :-
        t5tbox_next_key(Prim),
        t5dom_init_keys(Tail,Prims).

t5dom_init_prims([]).
t5dom_init_prims([_Name-Prim-_Def|Tail]) :-
	t5concid_built_in(Prim),
        t5dom_init_prims(Tail).

t5dom_init_defs([]).
t5dom_init_defs([Name-Prim-Def|Tail]) :-
        t5dom_allprims(ALL),
        b5sort_difference(ALL,[Prim],NP),
        t5dom_init_x(Name,[Prim],NP,NF),
        %b5nf_new_domain(NF,_,Def),
        b5nf_new_domain(NF,[predef],Def),
        assert(t5dom(Name,Def,open)),
	t5dom_init_defs(Tail).

t5dom_update_prims([]).
t5dom_update_prims([_Name-Prim-Def|Tail]) :-
	t5concid_update_prim(Prim,Def),
	t5dom_update_prims(Tail).




t5dom_init_x(anything,P,NP,NF):-
        b5nf_create_top_concept(P,NP,NF).
t5dom_init_x(number,P,NP,NF) :-
        b5nf_create_top_number(P,NP,NF).
t5dom_init_x(string,P,NP,NF) :-
        b5nf_create_top_string(P,NP,NF).
t5dom_init_x(aset,P,NP,NF) :-
        b5nf_create_top_aset(P,NP,NF).

t5dom_new_open_domain(Filter,Key,DomKey) :-
 	t5concid_new_prim(Key),
	t5dom_all_prims(add,Key,OldPrims,_),
	b5nf_create_new_open_aset([Key],OldPrims,NF),
	b5nf_new_domain(NF,Filter,DomKey),
	t5concid_update_prim(Key,DomKey),
        assert(t5dom(aset,DomKey,open)).


t5dom_new_attribute_keys(0,[]) :- !.
t5dom_new_attribute_keys(Count,[Key|Keys]) :-
		t5tbox_next_key(Key),
		NC is Count-1,
		t5dom_new_attribute_keys(NC,Keys).


t5dom_new_closed_domain(Count,Filter,Key,DomKey,KeyList) :-
	(Count == 0 -> 
		t5out_error(empty_attribute_domain),fail
	;
 		t5concid_new_prim(Key),
		t5dom_new_attribute_keys(Count,KeyList),
		t5dom_all_prims(add,Key,OldAllPrims,_Prims),
		b5nf_create_new_closed_aset([Key],OldAllPrims,Count,KeyList,NF),
		b5nf_new_domain(NF,Filter,DomKey),
		t5concid_update_prim(Key,DomKey),
		t5dom_new_instances(KeyList,DomKey),
        	assert(t5dom(aset,DomKey,close))
	).

t5dom_new_instances([],_DomKey).
t5dom_new_instances([Key|List],Dom) :-
	b5inst_new(Dom,aset-close,_,Key),
	t5dom_new_instances(List,Dom).

t5dom_new_instance(Dom,Inst,Key) :-
	t5dom(aset,Dom,open),
 	t5tbox_next_key(Key),
	b5inst_new(Dom,aset-open,Inst,Key),
	t5concid_new_attributes(Dom,[Key]),!.	

t5dom_new_instance(Dom,Inst,Key) :-
	t5dom(number,Dom,open),
 	t5tbox_next_key(Key),
	t5number_create_instance(VInst,Inst),
	b5inst_new(Dom,number,VInst,Key),!.

t5dom_new_instance(Dom,Inst,Key) :-
	t5dom(T,Dom,open),!,
 	t5tbox_next_key(Key),
	b5inst_new(Dom,T,Inst,Key).

t5dom_init :-
	retractall(t5dom_allprims(_)),
	retractall(t5dom(_,_,_)),
	assert(t5dom_allprims([])).

t5dom_all_prims(store,All) :-
 	retractall(t5dom_allprims(_)),
	assert(t5dom_allprims(All)),!.

t5dom_all_prims(add,Prim,All) :-
	t5dom_allprims(Old),
	b5sort_unify(Old,[Prim],All),
	t5dom_all_prims(store,All).

t5dom_all_prims(add,Prim,Old,All) :-
	t5dom_allprims(Old),
	b5sort_unify(Old,[Prim],All),
	t5dom_all_prims(store,All).

t5dom_all_aset_doms(Doms) :-
	setof(X,Status^t5dom(aset,X,Status),Doms).



/*------------------------------------------------------------ */
/*		        valueinstance			       */ 
/*------------------------------------------------------------ */

:- multifile b5dump_these_predicates/1.
b5dump_these_predicates([b5inst/6]).
   

b5inst_p(b5inst(_Key,_Dom,_Type,_Value,_BP)).

b5inst_raw_create(Key,Dom,Type,Value,BPs,BPi,b5inst(Key,Dom,Type,Value,BPs,BPi)).
b5inst_type(b5inst(_,_,Type,_,_,_),Type).
b5inst_dom(b5inst(_,Dom,_,_,_,_),Dom).
b5inst_value(b5inst(_,_,_,Value,_,_),Value).
b5inst_bps(b5inst(_,_,_,_,BPs,_),BPs).
b5inst_bpi(b5inst(_,_,_,_,_,BPi),BPi).

b5inst_init :-
	retractall(b5inst(_,_,_,_,_,_)).

b5inst_new(Dom,Type,Value,Key) :- 
	assert(b5inst(Key,Dom,Type,Value,[],[])).

b5inst_ibox_init :-
	findall(Key,b5inst(Key,_,_,_,_,_),Keys),!,
	b5inst_ibox_init_keys(Keys). 
b5inst_ibox_init :- !.

b5inst_ibox_init_keys([]).
b5inst_ibox_init_keys([Key|Keys]) :-
	retract(b5inst(Key,Dom,Type,Value,BPs,_)),
	assert(b5inst(Key,Dom,Type,Value,BPs,BPs)),
	b5inst_ibox_init_keys(Keys).

b5inst_abox_init_one(ThisOne) :-
	retract(b5inst(ThisOne,Dom,Type,Value,_,_)),
	assert(b5inst(ThisOne,Dom,Type,Value,[],[])).

b5inst_abox_init(NotThese) :-
	setof(Key,A^B^C^D^E^b5inst(Key,A,B,C,D,E),All),!, 
	b5sort_difference(All,NotThese,These),
	b5inst_delete_all(These),
	b5inst_abox_reset(NotThese).

b5inst_abox_init(_).

b5inst_abox_reset([]).
b5inst_abox_reset([ThisOne|NotThese]) :-
	b5inst_abox_init_one(ThisOne), 
	b5inst_abox_reset(NotThese). 

b5inst_delete_all([]).
b5inst_delete_all([This|These]) :-
	retract(b5inst(This,_,_,_,_,_)),
	b5inst_delete_all(These). 



	
b5inst_r_bp(nfs,b5inst(Key,Domain,Type,Value,BPs,BPi),BPs,NBPs,b5inst(Key,Domain,Type,Value,NBPs,BPi)).
b5inst_r_bp(nfi,b5inst(Key,Domain,Type,Value,BPs,BPi),BPi,NBPi,b5inst(Key,Domain,Type,Value,BPs,NBPi)).

b5inst_get_type(Key,Type) :-
	b5inst(Key,_,Type,_,_,_).

b5inst_get(Key,INST) :-
	b5inst(Key,Dom,Type,V,BPs,BPi),
	b5inst_raw_create(Key,Dom,Type,V,BPs,BPi,INST).

b5inst_o_store(Key,INST) :-
	retract(b5inst(Key,_,_,_,_,_)),
	assert(INST). 

b5inst_add_bp_ele(nfs,INST,BRE,N_INST) :-
	b5inst_r_bp(nfs,INST,BPL,NBPL,N_INST),
	b5typelist_add_ele(BPL,BRE,NBPL).

b5inst_add_bp_ele(nfi,INST,BRE,F_INST) :-
	b5inst_r_bp(nfi,INST,BPL,NBPL,N_INST),
	b5typelist_add_ele(BPL,BRE,NBPL),
	b5inst_add_bp_ele(nfs,N_INST,BRE,F_INST).

b5inst_add_backpointer(InstKey,NFType,RoleKey,ObjKey) :-
	b5inst_get(InstKey,INST),
	b5bre_raw_create(ObjKey,[RoleKey],BRE),
	b5inst_add_bp_ele(NFType,INST,BRE,N_INST),
	b5inst_o_store(InstKey,N_INST).

b5inst_is_instance_p(ValueKey,ConcId) :-
	t5concid_normal_form(ConcId,NF),
	b5inst(ValueKey,_Dom,Type,Inst,_,_),
	b5nf_type(NF,Type),
	b5inst_is_x(Type,ValueKey,Inst,NF).

b5inst_is_x(number,_KEY,Inst,NF) :-
	!,
	b5nf_num(NF,Num),
	t5number_subsumes_p(Num,Inst).

b5inst_is_x(string,_,_,_) :- !.

b5inst_is_x(_,Key,_,NF) :-
	b5nf_x(NF,X),
	b5sort_member_p(Key,X).


%% b5inst_are_instances_p(+SortedListOfValueInstKeys,+ValueInstConcKey)

b5inst_are_instances_p([],_).
b5inst_are_instances_p([I|Is], C) :-
	b5inst_is_instance_p(I,C),
	b5inst_are_instances_p(Is,C).

b5inst_supers(InstKey,Supers) :-
	b5inst_get(InstKey,INST),
	b5inst_dom(INST,Dom),
	b5nf_new_defined_conc(Dom,NF),
	b5nf_add_aset(NF,[InstKey],NNF),
	b5nf_supers_plus_eq(NNF,Supers).

b5inst_direct_supers(InstKey,Filter,DirectSupers) :-
	b5inst_get(InstKey,INST),
	b5inst_dom(INST,Dom),
	b5nf_new_defined_conc(Dom,NF),
	b5nf_add_aset(NF,[InstKey],NNF),
	b5nf_direct_supers(NNF,Filter,DirectSupers).

b5inst_number_direct_supers(N,Filter,DirectSupers) :-
	number(N),
	b5inst_number_direct_supers([num(N)],Filter,DirectSupers).

b5inst_number_direct_supers(X,Filter,DirectSupers) :-
	t5dom(number,Dom,_),
	b5nf_new_defined_conc(Dom,NF),
	b5nf_add_number_expr(NF,X,NNF),
	b5nf_direct_supers(NNF,Filter,DirectSupers).


b5inst_intension(InstKey,NNF) :-
	b5inst_get(InstKey,INST),
	b5inst_type(INST,number),!,
	b5inst_dom(INST,Dom),
	b5inst_value(INST,Value),
	b5nf_new_defined_conc(Dom,NF),
	t5number_describe(Value,[],YY),
	YY=[num(X)],
	b5nf_add_num(NF,X,NNF).
	
b5inst_intension(InstKey,NNF) :-
	b5inst_get(InstKey,INST),
	b5inst_dom(INST,Dom),
	b5nf_new_defined_conc(Dom,NF),
	b5nf_add_aset(NF,[InstKey],NNF).
	



