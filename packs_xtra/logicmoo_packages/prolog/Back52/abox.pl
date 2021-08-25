%% --------------------------------------------------------------------------
%  A5DEP.PL   2.2
%  
%  History:
%    2.1: Generalization of dependency entries selected by an additional
%         parameter indicating the respective ONF (being one of {nfs, nfi})
%
%    2.2: 
%      
%% --------------------------------------------------------------------------
% Interface
%
%   a5dep_dependencies_init.

%   a5dep_dependency(+Sel,+Obj1,+Obj2,+Pred,-Dependency)
%   a5dep_dependency(-Sel,-Obj1,-Obj2,-Pred,+Dependency)
%      Obj1 and Obj2 are keys, Pred is a cinst(Key,CNF),
%      Dependency is of the form depends_on(Key,Key,Pred)
%      Sel selects the ONF
%
%   a5dep_dependency_p(-Dependency)
%      succeeds, if Dependency is of the form 
%      depends_on(Sel,Key,Key,cinst(_,_).)
%
%   a5dep_dependencies_create(-Dependencies)
%      Dependencies is an empty set of dependency entries.
%
%   a5dep_dependencies_add_entry(+OldDepSet,+Dependency,-NewDepSet)
%      NewDepSet is OldDepSet union {Dependency}; Dependency is 
%      syntactically checked. 
%
%   a5dep_object_depending_objects(+Sel, +Obj, -DepObjs, -NegDeps)
%      Obj is a key; DepObjs is a list of the keys of all objects that 
%      depend transitively on Obj and where the associated predication does
%      not hold any longer. The according invalid dependency entries are 
%      accumulated in NegDeps. For correctness, requires that all new 
%      dependencies have been stored before!
%
%   a5dep_dependencies_store(+ADTDependencies)
%      Asserts a new set of ADTDependencies by merging it with already
%      existing ones (including redundancy checks)
%
%   a5dep_dependencies_delete(+NegDeps).
%      Deletes the set of NegDeps from the Prolog DB.
%
%   a5dep_retract_all_object_dependencies.
%      Retracts all dependency entries from the Prolog DB.
%
%   a5dep_retract_object_dependencies(+Obj)
%      Retracts all dependency entries concerning Obj, i.e.
%      depends_on(_,Obj,_,_) and depends_on(_,_,Obj,_).    
%
%
%%---------------------------------------------------------------------------
% Contextual assumptions
% 1. Revision Algorithm
%
%    revise(O,NewDesc) :-
%        delete_nfc(O),
%        delete_nfu(O),
%        depending_objects(O,Os),
%        rerecognize(O, NewDesc),
%        rerecognize(Os).
%
% ---------------------------------------------------------------------------
% Implementation notes:
%   This is the first version of the dependency graph being realized by 
%   simple asserts and retracts; building the transitive closure and 
%   checking the predications is done in a single phase
% ---------------------------------------------------------------------------

% qlib library files:
%   use_module(library(basics), [memberchk/2]),
%   use_module(library(ordsets),[ord_union/3]),
%   use_module(library(lists),  [delete/3,is_list/1]).

% ------------------------------------------------------------------------- 

:- multifile b5dump_these_predicates/1.

b5dump_these_predicates([depends_on/4]).

%% a5dep_dependencies_init.
%%   Initialize object dependencies.
a5dep_init :-
	a5dep_retract_all_object_dependencies.

a5dep_abox_init :-
	retractall(depends_on(_,_,_,_)).

a5dep_ibox_init :-
	retractall(depends_on(nfi,_,_,_)).

%% ADT Dependency
%% a5dep_dependency(?Sel,?Obj1,?Obj2,?Pred,?Dependency)
%%   Pred is assumed to have the form cinst(Obj,Term);
%%   If called with 4 parameters the default nfs-dependency is 
%%   returned; otherwise as indicated by Sel
%%   (currently one of {nfs,nfi})
a5dep_dependency(Sel, Obj1, Obj2, Pred, 
            depends_on(Sel, Obj1, Obj2, Pred)) :- !.  

%% a5dep_dependency_p(+ADTDependency)
%!   add checks on Objs and Pred
a5dep_dependency_p(depends_on(_,_,_,_)) :- !.

%% a5dep_empty_dependency(?EmptyDep)
%%   Returns or checks an empty dependency entry;
a5dep_empty_dependency(depends_on(nfs, null, null, null)) :- !.

%% a5dep_empty_dependency(+Sel, ?EmptyDep)
a5dep_empty_dependency(Sel, depends_on(Sel, null, null, null)) :- !.


%% a5dep_self_dependency(Sel, Dep).
%%   True if an object is dependent on itself.
a5dep_self_dependency(Sel, depends_on(Sel,O,O,_)).


%% a5dep_dependencies_create(-Dependencies)
%%   Dependencies is an empty set of dependency entries.
a5dep_dependencies_create([]).


%% a5dep_dependencies_add_entry(+OldDepSet,+Dependency,-NewDepSet)
%%   NewDepSet is OldDepSet union {Dependency}; Dependency may be a 
%%   set of dependency entries as well;  redundant dependencies
%%   are not added. 

a5dep_dependencies_add_entry(Deps, PlusDeps, NewDeps) :-
	is_list(PlusDeps),
	!,
	a5dep_dependencies_add_entries(Deps, PlusDeps, NewDeps).
a5dep_dependencies_add_entry(Deps, NewDep, Deps) :-
	( a5dep_empty_dependency(_, NewDep)
        ; a5dep_self_dependency(_,NewDep)),
	!.
a5dep_dependencies_add_entry([], NewDep, [NewDep]) :-
	a5dep_dependency_p(NewDep),
	t5out_info(new_dependency(NewDep)),
	!.
a5dep_dependencies_add_entry(Deps, depends_on(Sel,O1,O2,Pnew), NewDeps) :-
	member(depends_on(Sel, O1,O2,Pold), Deps),
	a5dep_pred_to_protoform([Pnew, Pold], [PNFnew, PNFold]),
	( Pnew == Pold -> NewDeps = Deps
        ; b5nf_subsumes_p(PNFnew, PNFold) -> NewDeps = Deps
        ; b5nf_subsumes_p(PNFold, PNFnew) ->
	      delete(Deps, depends_on(Sel, O1,O2,Pold), TmpDeps),
              append(TmpDeps, [depends_on(Sel, O1,O2,Pnew)], NewDeps),
              t5out_info(dependency_replaced(Sel, O1, Pold, Pnew))   
	  ; fail),
	!.  %% ensure no choice pts left %%
a5dep_dependencies_add_entry(Deps, NewDep, NewDeps) :-
	append(Deps, [NewDep], NewDeps),
	t5out_info(new_dependency(NewDep)).


%% a5dep_dependencies_add_entries(+OldDeps, +NewDeps, -OldAndNewDeps).
a5dep_dependencies_add_entries([],[],[]) :- !.
a5dep_dependencies_add_entries(Deps, [], Deps) :- !.
a5dep_dependencies_add_entries([], PlusDeps, PlusDeps) :- !.
a5dep_dependencies_add_entries(Deps, [PlusDep|PlusDeps], NewDeps) :- 
	a5dep_dependencies_add_entry(Deps, PlusDep, TmpNewDeps),
	a5dep_dependencies_add_entries(TmpNewDeps, PlusDeps, NewDeps).
 

%% a5dep_dependencies_store(+ADTDependencies)
%%  Stores a list of new ADTDependencies to the Prolog DB; 
%%  redundancy checks are done.

a5dep_dependencies_store([]) :- !.
a5dep_dependencies_store([depends_on(Sel,O1,O2,Pnew)|Deps]) :-
	depends_on(Sel,O1,O2,Pold),  
	a5dep_pred_to_protoform([Pnew, Pold], [PNFnew, PNFold]),
	( Pnew == Pold ->  
	    a5dep_dependencies_store(Deps)
	; (b5nf_subsumes_p(PNFnew, PNFold) ->
	    a5dep_dependencies_store(Deps)
	; (b5nf_subsumes_p(PNFold, PNFnew) ->
	    retract(depends_on(Sel, O1, O2, Pold)),
            assert(depends_on(Sel, O1, O2, Pnew)),
	    !,
            a5dep_dependencies_store(Deps)
	; fail))),
	!.                     
a5dep_dependencies_store([depends_on(Sel,O1,O2,Pnew)|Deps]) :-
	assert(depends_on(Sel, O1, O2, Pnew)),
	!,
	a5dep_dependencies_store(Deps).


%% a5dep_dependencies_delete(+NegDeps).
a5dep_dependencies_delete([]) :- !.
a5dep_dependencies_delete([Dep | Deps]) :-
	retract(Dep), !,
	a5dep_dependencies_delete(Deps).

%% a5dep_pred_to_protoform(+Preds, -PNFs).
%%   Converts a list of cinst-terms in normal forms
a5dep_pred_to_protoform([], []) :- !.
a5dep_pred_to_protoform(cinst(_, onf(NF)), NF) :- !.
a5dep_pred_to_protoform(cinst(_, Term), PNF) :- 
	a5dep_make_list(Term, TermList),
	b5par_keyterm_to_protoform(TermList, (c,0), NF),
	t5cpf_create(NF, _, PNF),
	!.
a5dep_pred_to_protoform([Pred | Preds], [PNF |  PNFs]) :-
	a5dep_pred_to_protoform(Pred, PNF),
	a5dep_pred_to_protoform(Preds, PNFs).


%% a5dep_retract_all_object_dependencies.
a5dep_retract_all_object_dependencies :-
	retractall(depends_on(_,_,_,_)).

%% a5dep_retract_all_object_dependencies(+Sel).
%%  retracts all dependencies selected by Sel
a5dep_retract_all_object_dependencies(Sel) :-
	retractall(depends_on(Sel,_,_,_)).

%% a5dep_retract_object_dependencies(+Sel,+Obj)
%%  Retracts all dependency entries concerning Obj, i.e.
%%  depends_on(Sel,Obj,_,_) and depends_on(Sel,_,Obj,_).
a5dep_retract_object_dependencies(Sel,Obj) :-
	retractall(depends_on(Sel,Obj,_,_)),
	retractall(depends_on(Sel,_,Obj,_)).


%% a5dep_retract_object_dependency(ADTDependency)
%%  retracts one specific dependency entry
a5dep_retract_object_dependency(ADTDependency) :-
	a5dep_dependency_p(ADTDependency),
	retract(ADTDependency), !.


%% a5dep_negligable_object(+Sel, +Obj)
%%   Inverse of independent_object; Obj is negligable if there is no
%%   relation depends_on(_, Obj, _). This can be used for optimizing the
%%   computation of depending_objects.
a5dep_negligable_object(Sel, Obj) :-
	not(depends_on(Sel, _, Obj, _)).


%% a5dep_object_depending_objects(+Obj, +DepObjs, +NegDeps)
%%  for downward compatibility reasons; default on t-dependencies.
a5dep_object_depending_objects(Obj, DepObjs, NegDeps) :-
	a5dep_object_depending_objects(nfs, Obj, DepObjs, NegDeps).

%% a5dep_object_depending_objects(+Sel, +Obj, -DepObjs, -NegDeps)
%%   computes the transitive closure of the asserted dependency links;
%%   algorithm taken from /O'Keefe 1990, p.171/ and adapted to the structure
%%   of dependency entries.
%%   DepObjs is a list of the form [Obj-Pred|PObjs]; 
%%   If Obj is not instantiated DepObjs is returned for all Objs.
%%   NegDeps is the accumulated list of dependency links which have
%%   have been followed and can be therefore deleted.
a5dep_object_depending_objects(Sel, Obj, [], []) :-
	a5dep_negligable_object(Sel,Obj),
	!.
a5dep_object_depending_objects(Sel, Obj, DepObjs4, NegDeps) :-
	setof(O2-Os,
	    setof(O1-Pred, depends_on(Sel,O1,O2,Pred), Os),
	    DependencyGraph),
	t5out_info(computing_depending_objects(Sel, Obj)),
	!,
	a5dep_warshall(Sel, DependencyGraph, Closure),
	(   member(Obj-DepObjs1, Closure),
	    a5dep_clean_list(DepObjs1, Obj, DepObjs2),
            a5dep_extract_objs_and_deps(Sel, DepObjs2, DepObjs3, NegDeps),
	    sort(DepObjs3, DepObjs4),
	    !  %% ensure that there are no further choice points
	;   DepObjs4 = [], NegDeps = []
        ), !.
a5dep_object_depending_objects(Sel,Obj,[],[]) :-
	t5out_info(no_dependencies(Sel,Obj)).

%% a5dep_warshall(+Sel, +DepGraph, -Closure).
%%  Computes the transitive Closure of DepGraph, cutting off all safe 
%%  branches of DepGraph parallely;  taken from  /O'Keefe 1990 p.172/ 
%%  and adapted to structure and purpose of DepGraph and Closure;
%%  Sel is passed to control t/i-dependencies.
a5dep_warshall(Sel, Graph, Closure) :-
	a5dep_warshall(Sel, Graph, Graph, Closure).

a5dep_warshall(_, [], Closure, Closure) :- !.
a5dep_warshall(Sel, [V-_|G], E, Closure) :-
	memberchk(V-Y1, E),    %% Y := E(v)
	a5dep_unsafe_object_filter(Sel,Y1,Y2), %% what, if Y2 = []??
	a5dep_warshall(Sel, E, V, Y2, NewE),
	a5dep_warshall(Sel, G, NewE, Closure).

a5dep_warshall(_, [], _, _, []) :- !.
a5dep_warshall(Sel, [X-NeibsIn|G], V, Y, [X-NewNeibs| NewG]) :-
	a5dep_memberchk(V, NeibsIn),
	a5dep_unsafe_object_filter(Sel,NeibsIn,NeibsOut),
	!,
	ord_union(NeibsOut, Y, NewNeibs),
	a5dep_warshall(Sel, G, V, Y, NewG).
a5dep_warshall(Sel, [X-Neibs| G], V, Y, [X-Neibs| NewG]) :-
	a5dep_warshall(Sel, G, V, Y, NewG).
    

%% a5dep_unsafe_object_filter(+Sel, +DepPObjs, -DepObjs).
%%  + Sel: selector of dependency-entries
%%  + DepPObjs: list of objects and predications of the form [O-P | ...]
%%  - DepObjs:  filtered out list of those O where P does not hold any
%%              longer; of the form [O | ...]  
a5dep_unsafe_object_filter(_, [], []) :- !.
a5dep_unsafe_object_filter(S, [O-P | OsPs], [O-P| OPs]) :-
	a5dep_predication_unsafe(S,P),
	!,   %% RED %%
	a5dep_unsafe_object_filter(S, OsPs, OPs).
a5dep_unsafe_object_filter(S, [_ | OsPs], OPs) :-
	a5dep_unsafe_object_filter(S, OsPs, OPs).

%% a5dep_predication_unsafe(+Sel, +Pred)
%%  A given predication of the form 'cinst(ObjId, Term)' is considered
%%  unsafe if the previous NF of the object belonging to ObjId is no more 
%%  subsumed by that predication (more exactly,  the current NF).
a5dep_predication_unsafe(NFSelId, cinst(ObjId, Term)) :-
	a5dep_pred_to_protoform(cinst(ObjId, Term), NNF),
	a5sch_previous_nf(NFSelId, NFPrevId),
	a5obj_fetch(ObjId, Obj),
	a5obj_nf(NFPrevId, Obj, NFPrev),
	t5cpf_create(NNFPrev, _, NFPrev),
	\+ b5nf_subsumes_p(NNF, NNFPrev).


%% a5dep_extract_objs_and_deps(+Sel, +Objs-Preds, -Objs, -NegDeps)
a5dep_extract_objs_and_deps(_, [], [], []) :- !.
a5dep_extract_objs_and_deps(Sel, [O1-P1 | OPs], [O1| Os], 
                   [depends_on(Sel, O1,O2,P1) | NegDeps]) :- 
	a5dep_pred_obj(P1, O2),
	a5dep_extract_objs_and_deps(Sel, OPs, Os, NegDeps),
	!.


a5dep_pred_obj(cinst(O1, _), O1).

a5dep_pred_term(cinst(_, Term), Term).

%% ------------------------------------------------------------------------
%%    Utilities
%% ------------------------------------------------------------------------

a5dep_make_list(A, A) :-
	is_list(A),
	!.
a5dep_make_list(A and B1, [A | B2]) :-
	a5dep_make_list(B1, B2),
	!.
a5dep_make_list(A, [A]).    


%% a5dep_memberchk(+Element, +Set)
%%   adapted  from  Quintus library(basics:memberchk/2) to lists of the 
%%   form [O-P | ...];  only to be used to test whether a known Element 
%%   occurs in a known Set.  In return for this limited use, it is more 
%%   efficient than member/2 when it is applicable.
a5dep_memberchk(X, [X-_|_]    ) :- !.
a5dep_memberchk(X, [_,X-_|_]  ) :- !.
a5dep_memberchk(X, [_,_,X-_|_]) :- !.
a5dep_memberchk(X, [_,_,_|L]) :-
	a5dep_memberchk(X, L).


a5dep_clean_list([], _, []).
a5dep_clean_list([X-_|L], X, Lc) :- 
	!,
	a5dep_clean_list(L, X, Lc). 
a5dep_clean_list([Y|L], X, [Y|Lc]) :-
	!,
	a5dep_clean_list(L, X, Lc). 


%------------------------ End of a5dep.pl -----------------------------------



% LAST EDIT: Sat Jan 23 20:17:39 1993 by Mirjam Kuehne (mitropa!mir) 
% LAST EDIT: Thu Dec 17 17:50:23 1992 by Mirjam Kuehne (madonna!mir) 
% LAST EDIT: Tue Nov 17 18:20:10 1992 by Mirjam Kuehne (madonna!mir) 
% LAST EDIT: Tue Aug  4 15:21:51 1992 by Mirjam Kuehne (madonna!mir) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		              INDEXING-MODULE				 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              Mirjam Kuehne                             %
%                               August 1992                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% IMPORT-predicates: t5cpf_create/3.
%              	     b5nf_complete2.
%		     t5cls_conc/8.
%                    t5concid_filter/2.
%		     t5hc_direct_supers/4.
%		     t5hc_minimize_special/4.
%		     b5sort_unify/3.
%		     t5cdb_store_new_filter/2           % -mf- 23.6.92
%                    t5fil_filter/2
%                    t5tbox_anything_key/1

% EXPORT-predicates: a5ind_init.
%		     a5ind_ibox_init.
%		     a5ind_insert_conc(+ConceptKey),
%	             a5ind_insert(+ObjectIDList),
%		     a5ind_remove(+Object).
%		     a5ind_remove(+Object,+Conc). 
%		     a5ind_remove(+Object,+[ConcList]). 
% 		     a5ind_modify(+ObjectIDList)
%		     a5ind_modify(+Object,+OldConc,+NewConc).
%		     a5ind_get_all_instances(+Conc,+Selector,-Objects).


:-multifile(b5dump_these_predicates/1).

b5dump_these_predicates([
a5ind_cinst_s/2,            
a5ind_cinst_i/2]).


% :- dynamic a5ind_cinst_s/2.
% :- dynamic a5ind_cinst_i/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a5ind_init: initializing the indexing-module				 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% -mf- 29.7.
a5ind_abox_init(Objects) :-
	a5ind_init,
	t5tbox_anything_key(Anything),
	a5ind_insert_abox_init(Objects,Anything).

a5ind_insert_abox_init([],_).

a5ind_insert_abox_init([O|Objects],Anything) :-
	assert(a5ind_cinst_s(Anything,O)),
	assert(a5ind_cinst_i(Anything,O)),
	!, a5ind_insert_abox_init(Objects,Anything).

a5ind_init :-
	a5ind_add_indexing_to_anything,
	retractall(a5ind_cinst_s(_,_)),
	retractall(a5ind_cinst_i(_,_)).

a5ind_add_indexing_to_anything :-
	t5tbox_anything_key(Anything),
	t5fil_filter(indexing,Indexing),
	t5cdb_store_new_filter(Anything,Indexing).

a5ind_ibox_init :-
	retractall(a5ind_cinst_i(_,_)),
	a5ind_get_s(ConcS,ObjID),
	a5ind_set_i(ConcS,ObjID),
	fail.

a5ind_ibox_init.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a5ind_insert_conc(ConceptKey): 					 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% mf 23.7.92:
a5ind_insert_conc(ConceptKey) :-
	b5sta_check_flag(aboxfilled, abox),
	t5out_trace(a5ind_insert_conc(ConceptKey)),
	t5concid_filter(ConceptKey,TrueFilter),
	t5fil_holds_p(indexing,TrueFilter),
	!, 
	a5ret_insert_conc(ConceptKey,Instances_S,Instances_I),
       	a5ind_insert_conc_in_db_s(Instances_S,ConceptKey),
	a5ind_insert_conc_in_db_i(Instances_I,ConceptKey).
a5ind_insert_conc(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a5ind_insert(ObjIDs): 						 %
%		fetches two of the three ObjectNormalforms:      	 %
%		1. the completed Normalform WITHOUT taking account IBox- %
%		   Rules (system-completed)	 			 %
%		2. the completed Normalform together with IBox-rules,	 %
%		computes the indexing superconcepts of these two Normal- %
%		forms and sets the Object as instance of these indexing  %
%		supers into the database except they were already set    %
%               before (a5ind_diff).                    	         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a5ind_insert(L) :-
	(b5sta_check_flag(aboxfilled, abox) -> %% jt 02.02.93
	     a5ind_insert1(L)
	; true).

a5ind_insert1([]).
a5ind_insert1([ObjID|ObjIDTail]) :-
	t5out_trace(a5ind_insert([ObjID|ObjIDTail])),
	a5ind_compute_ind_supers(ObjID,IndConcs_S,IndConcs_I),
	a5ind_diff_s(IndConcs_S,ObjID),
	a5ind_diff_i(IndConcs_I,ObjID),
	a5ind_insert1(ObjIDTail).


%% -mf 23.7.
a5ind_compute_ind_supers(ObjID,IndConcs_S,IndConcs_I) :-
	a5obj_fetch(ObjID,Object),
	a5obj_nf_usi(Object,_,OPF_S,OPF_I),
	a5ind_get_indexing_supers(OPF_S,IndConcs_S),
	t5out_info(indexing_concs_computed(ObjID,nfs)),
	(b5sta_check_flag(iboxfilled,true),  % -okp-
	 !,
	 a5ind_get_indexing_supers(OPF_I,IndConcs_I),
	 t5out_info(indexing_concs_computed(ObjID,nfi));
	    IndConcs_I = IndConcs_S),
	!.

%% cmk, 5.4.93: Patch; compute trans supers of explicit-supers which
%%  are not guarranteed to be complete!
%% Note: SupersOfONF is not the complete closure! E.g., if c1 and c2
%%  are in ExplicitSupers the concept 'c1 and c2' will not be included
%%  SupersOfONF. This pred relies on the fact that ABox classification
%%  would have included 'c1 and c2' in the exlicit supers slot if
%%  appropriate.

a5ind_get_indexing_supers(SelOPF,SelIndConcs) :-
        %b5nf_supers(SelOPF,AllSupers,_,_),     %old uk 28.1.93 hope thats ok
	b5nf_explicit_supers(SelOPF,ExplicitSupers), %new ......
	t5tbox_hc(conc,HC),
	t5hc_super_union_id(HC,ExplicitSupers,AllSupers), % cmk: 5.4.93
	t5hc_minimize(HC,AllSupers,indexing,SelIndConcs),
	!.

/*
a5ind_get_indexing_supers(SelOPF,SelIndConcs) :-
	b5nf_complete(SelOPF,CompletedSelOPF),
	t5cls_conc(CompletedSelOPF,SelSupers,[],SelAllSupers,_,_,SelKey,
                   Status),
	(Status == old ->
	 t5hc_supers(1,SelKey,SelSupers),
	 b5sort_unify([SelKey],SelSupers,SelUnifySupers);
	SelUnifySupers = SelAllSupers),
	t5hc_minimize(1,SelUnifySupers,indexing,SelIndConcs),
	!.
*/
a5ind_diff_s([],_).

a5ind_diff_s([IndConcs_S1|Tail],ObjID) :-
	(a5ind_get_s(IndConcs_S1,ObjID);
	 a5ind_insert_in_db_s([IndConcs_S1],ObjID)),
	a5ind_diff_s(Tail,ObjID),
	!.

a5ind_diff_i([],_).

a5ind_diff_i([IndConcs_I1|Tail],ObjID) :-
	(a5ind_get_i(IndConcs_I1,ObjID);
	 a5ind_insert_in_db_i([IndConcs_I1],ObjID)),
	a5ind_diff_i(Tail,ObjID),
	!.

a5ind_insert_in_db_s([],_).

a5ind_insert_in_db_s([S_Head|S_Tail],ObjID) :-
	a5ind_set_s(S_Head,ObjID),
	a5ind_insert_in_db_s(S_Tail,ObjID).

a5ind_insert_conc_in_db_s([],_).

a5ind_insert_conc_in_db_s([ObjID1|ObjIDTail],ConceptKey) :-
	a5ind_insert_in_db_s([ConceptKey],ObjID1),
	a5ind_insert_conc_in_db_s(ObjIDTail,ConceptKey).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a5ind_insert_in_db_i([],_).

a5ind_insert_in_db_i([I_Head|I_Tail],ObjID) :-
	a5ind_set_i(I_Head,ObjID),
	a5ind_insert_in_db_i(I_Tail,ObjID).


a5ind_insert_conc_in_db_i([],_).

a5ind_insert_conc_in_db_i([ObjID1|ObjIDTail],ConceptKey) :-
	a5ind_insert_in_db_i([ConceptKey],ObjID1),
	a5ind_insert_conc_in_db_i(ObjIDTail,ConceptKey).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a5ind_remove(Obj): removes Obj from from all concepts in the           %
%                    knowledgebase                 	                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a5ind_remove(Obj) :-
	a5ind_retractall(_,Obj),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a5ind_remove(Obj,Conc): removes from knowledgebase, that Obj is an 	 %
%			  instance of Conc.				 %
% a5ind_remove(Obj,[ConcList]): removes from knowledgebase, that Obj is  %
%				an instance of the concepts in the list  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a5ind_remove(Obj,Conc) :-
	(a5ind_retract_s(Conc,Obj);
	 true),
	(a5ind_retract_i(Conc,Obj);
	 true),
	!.
	
a5ind_remove(_,[]) :- !.

a5ind_remove(Obj,[Conc1|Tail]) :-
	a5ind_remove(Obj,Conc1),
	a5ind_remove(Obj,Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a5ind_modify(Obj,OldConc,NewConc): removes from knowledgebase, that 	 %
%				     Obj is an instance of OldConc and   %
%				     asserts, that Obj now is an         %
%				     instance of NewConc.		 %
% The concepts have to be inserted before (else nothing happens only     %
% "true")			 					 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a5ind_modify(Obj,OldConc,NewConc) :-
	(a5ind_retract_s(OldConc,Obj),
	 a5ind_set_s(NewConc,Obj),
	 !;
	true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a5ind_modify(ObjIDList): updates the Conceptual Indexing entries for   %
%			   every X in ObjIDList.			 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a5ind_modify([]).

a5ind_modify([ObjIDHead|Tail]) :- 
        a5ind_remove(ObjIDHead), 
	a5ind_insert([ObjIDHead]), 
        a5ind_modify(Tail).

a5ind_get_all_concs_s(Object,ListOfConcs_S) :-
	setof(X,a5ind_cinst_s(X,Object),ListOfConcs_S).

a5ind_get_all_concs_i(Object,ListOfConcs_I) :-
	setof(X,a5ind_cinst_i(X,Object),ListOfConcs_I).

a5ind_remove_diffs_s([Diff_SHead|S_Tail],ObjID) :-
	a5ind_retract_s(Diff_SHead,ObjID),
	a5ind_remove_diffs_s(S_Tail,ObjID),
	!.

a5ind_remove_diffs_i([Diff_IHead|I_Tail],ObjID) :-
	a5ind_retract_i(Diff_IHead,ObjID),
	a5ind_remove_diffs_i(I_Tail,ObjID),
	!.

a5ind_set_all_adds_s([Add_SHead|S_Tail],ObjID) :-
	a5ind_set_s(Add_SHead,ObjID),
	a5ind_set_all_adds_s(S_Tail,ObjID).

a5ind_set_all_adds_i([Add_IHead|I_Tail],ObjID) :-
	a5ind_set_i(Add_IHead,ObjID),
	a5ind_set_all_adds_i(I_Tail,ObjID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a5ind_get_all_instances(Conc,Selector,Obj): 				 %
%		gives all instances of Conc depending on what the        %
%		selector contains (with or without IBox-rules).		 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a5ind_get_all_instances(Conc,Selector,List) :-
	(Selector == nfs ->
	 setof(X,a5ind_cinst_s(Conc,X),List);
	setof(X,a5ind_cinst_i(Conc,X),List)),
	!.

a5ind_get_all_instances(_,_,[]) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a5ind_set_s(ConcS,ObjID) :-
	assert(a5ind_cinst_s(ConcS,ObjID)).

a5ind_set_i(ConcI,ObjID) :-
	assert(a5ind_cinst_i(ConcI,ObjID)).

%%%%%%%%%%%%%%%%%%%%%%%%%
a5ind_get_s(ConcS,ObjID) :-
	a5ind_cinst_s(ConcS,ObjID).

a5ind_get_i(ConcI,ObjID) :-
	a5ind_cinst_i(ConcI,ObjID).

%%%%%%%%%%%%%%%%%%%%%%%%%
a5ind_retract_s(Conc_S,Obj) :-
	retract(a5ind_cinst_s(Conc_S,Obj)),
	!.

a5ind_retract_i(Conc_I,Obj) :-
	retract(a5ind_cinst_i(Conc_I,Obj)),
	!.

a5ind_retract(Conc,Obj) :-
	retract(a5ind_cinst_s(Conc,Obj)),
	retract(a5ind_cinst_i(Conc,Obj)),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%
a5ind_retractall_s(Concs_S,ObjID) :-
	retractall(a5ind_cinst_s(Concs_S,ObjID)).

a5ind_retractall_i(Concs_I,ObjID) :-
	retractall(a5ind_cinst_i(Concs_I,ObjID)).

a5ind_retractall(Conc,Obj) :-
	retractall(a5ind_cinst_s(Conc,Obj)),
	retractall(a5ind_cinst_i(Conc,Obj)).


a5obj_p(_Key-obj(_Flag,_ONFs,_RI,_Filter,_N_Fillers)).
a5obj_raw_create(Key,Flag,ONFs,RI,Filter,N_Fillers,Key-obj(Flag,ONFs,RI,Filter,N_Fillers)).

a5obj_create(OBJ) :- 
        a5onfs_create(ONFs),
        b5rel_create(REL),
        a5obj_raw_create(_,a,ONFs,_,_,REL,OBJ). /* ????????? */

a5obj_create(Key,OBJ) :- 
        a5onfs_create(Key,ONFs),
        b5rel_create(REL),
        a5obj_raw_create(Key,a,ONFs,_,_,REL,OBJ). /* ????????? */

a5obj_id(K-obj(_,_,_,_,_),K) :- !.
a5obj_flag(_-obj(Flag,_,_,_,_),Flag) :- !.
a5obj_nf(_-obj(_,NF,_,_,_),NF) :- !.
a5obj_ri(_-obj(_,_,RI,_,_),RI) :- !.
a5obj_filter(_-obj(_,_,_,F,_),F) :- !.
a5obj_new_fillers(_-obj(_,_,_,_,NFs),NFs) :- !.

a5obj_s_id(_-obj(F,NF,R,Fi,NFs),NK,NK-obj(F,NF,R,Fi,NFs)) :- !.
a5obj_s_flag(K-obj(_,NF,R,Fi,NFs),F,K-obj(F,NF,R,Fi,NFs)) :- !.
a5obj_s_nf(K-obj(F,_,R,Fi,NFs),NF,K-obj(F,NF,R,Fi,NFs)) :- !.
a5obj_s_r(K-obj(F,NF,_,Fi,NFs),R,K-obj(F,NF,R,Fi,NFs)) :- !.
a5obj_s_filter(K-obj(F,NF,R,_,NFs),Fi,K-obj(F,NF,R,Fi,NFs)) :- !.
a5obj_s_new_fillers(K-obj(F,NF,R,Fi,_),NFs,K-obj(F,NF,R,Fi,NFs)) :- !.

a5obj_r_id(K-obj(F,NF,R,Fi,NFs),K,NK,NK-obj(F,NF,R,Fi,NFs)) :- !.
a5obj_r_flag(K-obj(F,NF,R,Fi,NFs),F,Nf,K-obj(Nf,NF,R,Fi,NFs)) :- !.
a5obj_r_nf(K-obj(F,NF,R,Fi,NFs),NF,NNF,K-obj(F,NNF,R,Fi,NFs)) :- !.
a5obj_r_r(K-obj(F,NF,R,Fi,NFs),R,NR,K-obj(F,NF,NR,Fi,NFs)) :- !.
a5obj_r_filter(K-obj(F,NF,R,Fi,NFs),Fi,NFi,K-obj(F,NF,R,NFi,NFs)) :- !.
a5obj_r_new_fillers(K-obj(F,NF,R,Fi,NFs),NFs,NNFs,K-obj(F,NF,R,Fi,NNFs)) :- !.


%% a5obj_modified_p(O1,O2) 
%% a5obj_modified_p(NFsel, O1,O2).
a5obj_modified_p(O1,O2) :- 
%-cmk-	a5obj_raw_create(Key,Flag,ONF1s,RI,Filter,N_Fillers,O1),
%-cmk-	a5obj_raw_create(Key,Flag,ONF2s,RI,Filter,N_Fillers,O2),
	a5obj_raw_create(Key,_,ONF1s,_,_,_,O1),
	a5obj_raw_create(Key,_,ONF2s,_,_,_,O2),
	b5nf_modified_p(ONF1s,ONF2s).

a5obj_modified_p(NFsel, O1,O2) :-
	a5obj_id(O1,Id),
	(a5obj_id(O2,Id) ->
	    a5obj_nf_x(NFsel,O1,NF1),
	    a5obj_nf_x(NFsel,O2,NF2),
	    b5nf_modified_p(NF1,NF2)   
	;t5out_error(bug_invalid_comparison_of_different_objs(O1,O2))).


a5obj_active_p(Obj) :- 
	a5obj_flag(Obj,a).
	/* .... */

/* onfs */
a5obj_nf_usi(Obj,U,S,I) :-
	a5obj_nf(Obj,NF),
        a5onfs_raw_create(U,S,I,NF).		

a5obj_nf_u(Obj,UNF) :-
        a5obj_nf(Obj,NF),
        a5onfs_nfu(NF,UNF).

a5obj_nf_s(Obj,SNF) :-
	a5obj_nf(Obj,NF),
	a5onfs_nfs(NF,SNF).

a5obj_nf_i(Obj,INF) :-
	a5obj_nf(Obj,NF),
	a5onfs_nfi(NF,INF).

a5obj_nf_x(X,Obj,XNF) :-
	a5obj_nf(Obj,NF),
	a5onfs_x(X,NF,XNF).

a5obj_s_u(Obj,U,NObj) :- 
	a5obj_r_nf(Obj,NF,NNF,NObj),
	a5onfs_nfs_u(NF,U,NNF).

a5obj_s_s(Obj,U,NObj) :- 
	a5obj_r_nf(Obj,NF,NNF,NObj),
	a5onfs_nfs_s(NF,U,NNF).

a5obj_s_i(Obj,U,NObj) :- 
	a5obj_r_nf(Obj,NF,NNF,NObj),
	a5onfs_nfs_i(NF,U,NNF).

a5obj_s_x(X,Obj,NEWNFX,NObj) :-
	a5obj_r_nf(Obj,NF,NNF,NObj),
	a5onfs_nfs(X,NF,NEWNFX,NNF).

a5obj_r_u(Obj,U,NU,NObj) :- 
	a5obj_r_nf(Obj,NF,NNF,NObj),
	a5onfs_r_nfu(NF,U,NU,NNF).

a5obj_r_s(Obj,S,NS,NObj) :- 
	a5obj_r_nf(Obj,NF,NNF,NObj),
	a5onfs_r_nfs(NF,S,NS,NNF).

a5obj_r_i(Obj,I,NI,NObj) :- 
	a5obj_r_nf(Obj,NF,NNF,NObj),
	a5onfs_r_nfi(NF,I,NI,NNF).

a5obj_r_x(X,Obj,OLDNF,NEWNF,NOBJ) :- 
	a5obj_r_nf(Obj,NF,NNF,NOBJ),
	a5onfs_r(X,NF,OLDNF,NEWNF,NNF).

/* nf */


a5obj_prims(X,C,Prims) :-
	a5obj_nf_x(X,C,NF),
        b5nf_prims(NF,Prims).

a5obj_neg_prims(X,C,NegPrims) :-
	a5obj_nf_x(X,C,NF),
        b5nf_neg_prims(NF,NegPrims).

a5obj_reslist(X,C,ResList) :-
	a5obj_nf_x(X,C,NF),
        b5nf_reslist(NF,ResList).

/* nf.rl */
a5obj_role_res(X,C,Role,Res) :-
	a5obj_nf_x(X,C,NF),
        b5nf_role_res(NF,Role,Res).

/*nf.rl.res */
a5obj_vr(X,C,Role,Vr) :-
	a5obj_nf_x(X,C,NF),
        b5nf_vr(NF,Role,Vr).

a5obj_nr(X,C,Role,Nr) :-
	a5obj_nf_x(X,C,NF),
        b5nf_nr(NF,Role,Nr).

/*nf.rl.res.nr */
a5obj_min(X,C,Role,Min) :-
	a5obj_nf_x(X,C,NF),
        b5nf_min(NF,Role,Min).

a5obj_max(X,C,Role,Max) :-
	a5obj_nf_x(X,C,NF),
        b5nf_max(NF,Role,Max).

/* nf.rl.res */
a5obj_fillers(X,C,Role,Fillers) :-
	a5obj_nf_x(X,C,NF),
        b5nf_fillers(NF,Role,Fillers).

a5obj_rvm_equals(X,C,Role,Eqs) :-
	a5obj_nf_x(X,C,NF),
        b5nf_rvm_equals(NF,Role,Eqs).

a5obj_rvm_supers(X,C,Role,Supers) :-
	a5obj_nf_x(X,C,NF),
        b5nf_rvm_supers(NF,Role,Supers).

a5obj_rvm_subs(X,C,Role,Subs) :-
	a5obj_nf_x(X,C,NF),
        b5nf_rvm_subs(NF,Role,Subs).


/* nf */
a5obj_min_card(X,C,MinC) :-
	a5obj_nf_x(X,C,NF),
        b5nf_min_card(NF,MinC).

a5obj_max_card(X,C,MaxC) :-
	a5obj_nf_x(X,C,NF),
        b5nf_max_card(NF,MaxC).

/* Neu */
a5obj_minmax(X,C,NR) :-  /* warning ! */
	a5obj_nf_x(X,C,NF),
        b5nf_nr(NF,NR).

a5obj_extension(X,C,Ext) :- /* warning */
	a5obj_nf_x(X,C,NF),
        b5nf_extension(NF,Ext).

a5obj_add_max(X,Obj,Max,Role,N_Obj) :-
        a5obj_r_nf_x(X,Obj,NF,N_NF,N_Obj),
        b5nf_add_max(NF,Role,Max,N_NF).

a5obj_add_min(X,Obj,Min,Role,N_Obj) :-
        a5obj_r_nf_x(X,Obj,NF,N_NF,N_Obj),
        b5nf_add_min(NF,Role,Min,N_NF).

a5obj_add_vr1(X,Obj,VR,Role,N_Obj) :-
        a5obj_r_nf_x(X,Obj,NF,N_NF,N_Obj),
        b5nf_add_vr(NF,VR,Role,N_NF).

a5obj_add_fillers(X,Obj,Fs,Role,F_Obj) :-
        a5obj_r_x(X,Obj,NF,N_NF,N_Obj),
	b5nf_add_fillers(NF,Fs,Role,New,N_NF),
	(
	b5sort_empty_p(New) ->  F_Obj = N_Obj
		;
        	a5obj_r_new_fillers(N_Obj,OldNew_Fillers,NewNew_Fillers,F_Obj),
        	b5re_raw_create(Role,New,RE),
        	b5rel_add(OldNew_Fillers,RE,NewNew_Fillers)
	).


/* ???????????????????????? */
a5obj_add_prim(X,Obj,Prim,N_Obj) :-
        a5obj_r_nf_x(X,Obj,NF,N_NF,N_Obj),
        b5nf_add_prims(NF,[Prim],N_NF).

a5obj_add_neg_prim(X,Obj,NPrim,N_Obj) :-
        a5obj_r_nf_x(X,Obj,NF,N_NF,N_Obj),
        b5nf_add_neg_prims(NF,[NPrim],N_NF).

a5obj_add_conc(X,Obj,Key,N_Obj) :-
        a5obj_r_nf_x(X,Obj,NF,N_NF,N_Obj),
	b5nf_add_conc(NF,Key,N_NF).


a5obj_add_atleast(X,Obj,Min,Role,N_Obj) :- 
        a5obj_r_nf_x(X,Obj,NF,N_NF,N_Obj),
        b5nf_add_atleast(NF,Min,Role,N_NF).


a5obj_add_atmost(X,Obj,Max,Role,N_Obj) :- 
        a5obj_r_nf_x(X,Obj,NF,N_NF,N_Obj),
        b5nf_add_atmost(NF,Max,Role,N_NF).


a5obj_add_vr(X,Obj,VR,Role,N_Obj) :-
        a5obj_r_nf_x(X,Obj,NF,N_NF,N_Obj),
	b5nf_add_vr(NF,VR,Role,N_NF).

%% checks if there is an old entry, and if so deletes it. (cmk, 5.5.92)
a5obj_store(Obj) :-
        a5obj_id(Obj,Key),
	(a5odb_remove(Key), !; true),
        a5odb_store(Key,Obj).


a5obj_filled_roles_and_fillers(X,Obj,REL) :-
	a5obj_nf_x(X,Obj,NFX),
	b5nf_filled_roles_and_fillers(NFX,REL).


/*
a5obj_complete_locally(X,Obj,N_Obj) :-
	a5obj_r_x(X,Obj,NF,N_NF,N_Obj),
	b5nf_complete(NF,N_NF).
*/


a5obj_complete_locally(X,Obj,F_Obj) :-
	a5obj_r_x(X,Obj,NF,N_NF,N_Obj),
	b5nf_complete(NF,N_NF,REL),
	a5obj_r_new_fillers(N_Obj,OldNew_Fillers,NewNew_Fillers,F_Obj),
	b5typelist_unify(REL,OldNew_Fillers,NewNew_Fillers).

a5obj_fills(X,Obj,BREL) :-
	a5obj_nf_x(X,Obj,NF),
	b5nf_reslist(NF,RL), 
	b5brel_build_brel(RL,BREL).

a5obj_unify_nf(X,CPF,Obj,F_Obj) :-
	 a5obj_r_x(X,Obj,CPFOLD,CPFNEW,N_Obj),
	 a5obj_r_new_fillers(N_Obj,Old_NewFillers,New_NewFillers,F_Obj),
	 b5nf_unify(CPF,CPFOLD,CPFNEW),
	 b5nf_rl(CPF,Reslist),
	 b5rel_build_rel(Reslist,REL),  % told fillers
	 b5nf_reslist(CPFOLD,ReslistOLD), 
	 b5rel_build_rel(ReslistOLD,RELOLD), % old fillers
	 b5typelist_subtract(REL,RELOLD,DIFF), % told and not old
	 b5typelist_unify(Old_NewFillers,DIFF,New_NewFillers).
	  
	  
a5obj_some_role_closed_p(X,Obj,Roles) :-
	a5obj_nf_x(X,Obj,NFX),
	% evt. b5mark 
	b5nf_some_role_closed_p(NFX,Roles).


%% True if Obj has a non-empty entry for the new_fillers slot
a5obj_new_fillers_p(_-obj(_,_,_,_,NFs)) :- !, NFs \== [].


a5obj_reset_new_fillers(OBJ,NOBJ) :-
        b5rel_create(REL),
	a5obj_s_new_fillers(OBJ,REL,NOBJ).

a5obj_add_new_fillers(Obj,REL,N_Obj) :-
	a5obj_r_new_fillers(Obj,OldNew_Fillers,NewNew_Fillers,N_Obj),
	b5typelist_unify(REL,OldNew_Fillers,NewNew_Fillers).

a5obj_replace_nf(SEL,NEWNF,OBJ,NOBJ) :-
	a5obj_s_x(SEL,OBJ,NEWNF,NOBJ).


a5obj_fetch(ObjID, Obj0) :-
	a5odb_try_get(ObjID,Obj0,yes).


a5obj_nf(ONFselector, Obj, NF) :-
	a5obj_nf_x(ONFselector,Obj,NF).





%% a5obj_set_remote_backpointers(+NewFillers,+ObjID,+Scheduler,-NewScheduler)
%%  NewFillers is a list of RoleExtensions (i.e. pairs Role-re(Fillers)).
%% FFS: Maybe it is faster to build a list of backpointer jobs and
%%  add that list to agenda at once !?

a5obj_set_remote_backpointers([], _, S,S).
a5obj_set_remote_backpointers([R-re(Fs)|NewRFs], ObjID, S0,S) :-
	a5obj_set_remote_backpointers(Fs, R, ObjID, S0,S1),
	a5obj_set_remote_backpointers(NewRFs, ObjID, S1,S).

a5obj_set_remote_backpointers([], _, _, S,S).
a5obj_set_remote_backpointers([F|Fs], Role, ObjID, S0,S) :-
	a5sch_add_to_agenda(backpointer(F, ObjID, Role), S0,S1),
	a5obj_set_remote_backpointers(Fs, Role, ObjID, S1,S).




%% --------


a5obj_incoherent_p(ONFsel, Obj) :-
	a5obj_nf_x(ONFsel, Obj, ONF),
	a5obj_id(Obj, ObjId),
	(b5nf_incoherent_p(ONF) ->
	    t5out_error(a5obj_incoherent_p(ONFsel, ObjId))
	;   t5out_trace(a5obj_coherent_p(ONFsel, ObjId)),
	    !, fail ).


%% --------

%%  FFS: make it forward_prop only !!

a5obj_complete(ONFsel, Obj, Obj, Scheduler0, Scheduler) :-
	a5obj_nf(ONFsel, Obj, NF0),
	a5obj_new_fillers(Obj, NewFillers),
	a5obj_id(Obj, ObjID),
	a5rec_forward_propagation(NewFillers, ObjID,NF0,Scheduler0,Scheduler).


a5obj_ibox_init(OBJ,N_OBJ) :-
	a5obj_nf_s(OBJ,NFS),
	a5obj_s_i(OBJ,NFS,N_OBJ).


%%  FFS: clean it up; make it forward_prop only !!
/*
a5obj_complete(ONFsel, Obj0, Obj, Scheduler0, Scheduler) :-
	a5obj_nf(ONFsel, Obj0, NF0),	
	a5obj_new_fillers(Obj0, NewFillers0),
	a5obj_reset_new_fillers(Obj0,Obj1),
	%? a4rec_conc_ind_chk_consistent(+UC, +MSCs, +IndexConcs, +ResList)
	a5obj_id(Obj1, ObjID),
	a5rec_forward_propagation(NewFillers0, ObjID, NF0, Scheduler0, S1),
%%	a5rec_role_completion(NewFillers0, ObjID, NF0, NF, S1, S2),
	a5roco_role_completion(NewFillers0, ObjID, ONFsel, Obj1, Obj2, S1, S2),
	%% since every role has an inv role the backpointer mechanism
	%% is superfluous. FFS: roles with non-concept range !!
%%	a5obj_new_fillers(Obj1, NewFillers1),
%%	%% a5obj_reset_new_fillers(Obj2,Obj3), ??
%%	union(NewFillers0, NewFillers1, NewFillers),
%%	a5obj_set_remote_backpointers(NewFillers, ObjID, S2, Scheduler),
%%	a5obj_replace_nf(ONFsel, NF, Obj2, Obj).
        Obj = Obj2, Scheduler = S2.

*/

a5obj_explicit_supers(Obj,Sel,Supers) :-
	a5obj_nf_x(Sel,Obj,NF),
	b5nf_explicit_supers(NF,Supers).

a5obj_supers(Obj,Sel,Supers) :-
	a5obj_nf_x(Sel,Obj,NF),
	b5nf_supers(NF,Supers).

a5obj_direct_supers(Obj,Sel,DS) :-
	a5obj_nf_x(Sel,Obj,NF),
	b5nf_direct_supers(NF,DS).

a5obj_direct_supers(Obj,Sel,Filter,DS) :-
	a5obj_nf_x(Sel,Obj,NF),
	b5nf_direct_supers(NF,Filter,DS).

a5obj_include_role(Obj,Role,Supers,Subs,N_Obj) :-
	a5obj_r_nf(Obj,NF,N_NF,N_Obj),
	a5onfs_raw_create(NF_U,NF_S,NF_I,NF),
	a5onfs_raw_create(N_NF_U,N_NF_S,N_NF_I,N_NF),
	b5nf_include_role(NF_U,Role,Supers,Subs,N_NF_U),
	b5nf_include_role(NF_S,Role,Supers,Subs,N_NF_S),
	b5nf_include_role(NF_I,Role,Supers,Subs,N_NF_I).


a5obj_add_conc_yields_nothing_p(X,Obj,ConcID) :- 
	a5obj_nf_x(X,Obj,NF),
	b5nf_add_conc(NF,ConcID,N_NF),
	b5nf_complete(N_NF,CNF),                                 
	b5nf_incoherent_p(CNF).                                         

a5obj_add_conc_yields_nothing_p(X,Obj,ConcID1,ConcID2) :-
	a5obj_nf_x(X,Obj,NF),
	b5nf_add_conc(NF,ConcID1,N_NF1),
	b5nf_add_conc(N_NF1,ConcID2,N_NF2),
	b5nf_complete(N_NF2,CNF),
	b5nf_incoherent_p(CNF).

/*

a5objid_supers(C,Supers) :-
        t5tbox_conc_hc(HC),
        t5hc_supers(HC,C,Supers).
a5objid_subs(C,Subs) :-
        t5tbox_conc_hc(HC),
        t5hc_subs(HC,C,Subs).
a5objid_direct_supers(C,D_Supers) :-
        t5tbox_conc_hc(HC),
        t5hc_direct_supers(HC,C,D_Supers).
a5objid_direct_subs(C,D_Subs) :-
        t5tbox_conc_hc(HC),
        t5hc_direct_subs(HC,C,D_Subs).
a5objid_disjoints(C,Disj) :-
        t5tbox_conc_hc(HC),
        t5hc_disjoints(HC,C,Disj).
a5objid_non_disjoints(C,NonDisj) :-
        t5tbox_conc_hc(HC),
        t5hc_non_disjoints(HC,C,NonDisj).
a5objid_possibly_disjoints(C,PossDisj) :-
        t5tbox_conc_hc(HC),
        t5hc_possibly_disjoints(HC,C,PossDisj).
a5objid_sub_union(C,SubUnion) :-
        t5tbox_conc_hc(HC),
        t5hc_sub_union(HC,C,SubUnion).
a5objid_super_union(C,SuperUnion):-
        t5tbox_conc_hc(HC),
        t5hc_super_union(HC,C,SuperUnion).
a5objid_super_intersection(C,SuperInter) :-
        t5tbox_conc_hc(HC),
        t5hc_super_intersection(HC,C,SuperInter).
a5objid_sub_intersection(C,SubInter):-
        t5tbox_conc_hc(HC),
        t5hc_sub_intersection(HC,C,SubInter).
a5objid_subsumes_p(Key1,Key2) :-
        t5tbox_conc_hc(HC),
        t5hc_subsumes_p(HC,Key1,Key2).
*/

a5objid_explicit_supers(ID,Sel,Supers) :-
	a5odb_get(ID,Obj,_Filter),
	a5obj_explicit_supers(Obj,Sel,Supers).

a5objid_supers(ID,Sel,Supers) :-
	a5odb_get(ID,Obj,_Filter),
	a5obj_supers(Obj,Sel,Supers).

a5objid_direct_supers(ID,Sel,DS) :-
        a5odb_get(ID,Obj,_Filter),
	a5obj_direct_supers(Obj,Sel,DS).

a5objid_direct_supers(ID,Sel,Filter,DS) :-
        a5odb_get(ID,Obj,_Filter),
	a5obj_direct_supers(Obj,Sel,Filter,DS).

a5objid_obj(C,Filter,Obj) :-
        a5odb_get(C,Obj,Filter).

a5objid_nf_x(X,C,Filter,XNF) :-
        a5odb_nf_x(X,C,Filter,XNF).

a5objid_nf_u(C,Filter,UNF) :-
        a5objid_nf_x(nfu,C,Filter,UNF).

a5objid_nf_s(C,Filter,SNF) :-
        a5objid_nf_x(nfs,C,Filter,SNF).

a5objid_nf_i(C,Filter,INF) :-
        a5objid_nf_x(nfi,C,Filter,INF).


/* von nf */

a5objid_prims(X,Key,Prims) :-
        a5odb_get(Key,Obj,_Filter),
        a5obj_prims(X,Obj,Prims).

a5objid_neg_prims(X,Key,NegPrims) :-
        a5odb_get(Key,Obj,_Filter),
        a5obj_neg_prims(X,Obj,NegPrims).

a5objid_reslist(X,Key,ResList) :-
        a5odb_get(Key,Obj,_Filter),
        a5obj_reslist(X,Obj,ResList).

/* nf.rl */
a5objid_role_res(X,Key,Role,Res) :-
        a5odb_get(Key,Obj,_Filter),
        a5obj_role_res(X,Obj,Role,Res).

/*nf.rl.res */
a5objid_vr(X,Key,Role,VR) :-
        a5odb_get(Key,Obj,_Filter),
        a5obj_vr(X,Obj,Role,VR).

a5objid_nr(X,Key,Role,NR) :-
        a5odb_get(Key,Obj,_Filter),
        a5obj_nr(X,Obj,Role,NR).

/*nf.rl.res.nr */
a5objid_min(X,Key,Role,Min) :-
        a5odb_get(Key,Obj,_Filter),
        a5obj_min(X,Obj,Role,Min).

a5objid_max(X,Key,Role,Max) :-
        a5odb_get(Key,Obj,_Filter),
        a5obj_max(X,Obj,Role,Max).

a5objid_min_max(X,Key,Role,Min,Max) :-
        a5odb_get(Key,Obj,_Filter),
        a5obj_min(X,Obj,Role,Min),
        a5obj_max(X,Obj,Role,Max).

/* nf.rl.res */
a5objid_fillers(X,Key,Role,Fillers) :-
        a5odb_get(Key,Obj,_Filter),
        a5obj_fillers(X,Obj,Role,Fillers).

a5objid_rvm_equals(X,Key,Role,Eqs) :-
        a5odb_get(Key,Obj,_Filter),
        a5obj_rvm_equals(X,Obj,Role,Eqs).

a5objid_rvm_supers(X,Key,Role,Supers) :-
        a5odb_get(Key,Obj,_Filter),
        a5obj_rvm_supers(X,Obj,Role,Supers).

a5objid_rvm_subs(X,Key,Role,Subs) :-
        a5odb_get(Key,Obj,_Filter),
        a5obj_rvm_subs(X,Obj,Role,Subs).


/* nf */
a5objid_min_card(X,Key,MinC) :-         /* warning ! */
        a5odb_get(Key,Obj,_Filter),
        a5obj_min_card(X,Obj,MinC).        

a5objid_max_card(X,Key,MaxC) :-         /* warning */
        a5odb_get(Key,Obj,_Filter),
        a5obj_max_card(X,Obj,MaxC).

/* Neu */
a5objid_minmax(X,Key,NR) :-  /* warning ! */
        a5odb_get(Key,Obj,_Filter),
        a5obj_minmax(X,Obj,NR).       

a5objid_extension(X,Key,Ext) :- /* warning */
        a5odb_get(Key,Obj,_Filter),
        a5obj_extension(X,Obj,Ext).

a5objid_add_max(X,Key,Max,Role) :-
        a5odb_get(Key,Obj,_Filter),
        a5obj_add_max(X,Obj,Max,Role,N_Obj),
        a5odb_over_store(Key,N_Obj).

a5objid_add_min(X,Key,Min,Role) :-
        a5odb_get(Key,Obj,_Filter),
        a5obj_add_min(X,Obj,Min,Role,N_Obj),
        a5odb_over_store(Key,N_Obj).

a5objid_add_vr(X,Key,VR,Role) :-
        a5odb_get(Key,Obj,_Filter),
        a5obj_add_vr(X,Obj,VR,Role,N_Obj),
        a5odb_over_store(Key,N_Obj).

a5objid_add_backpointer(X,Key,Role,InvFiller) :-
        a5odb_get(Key,Obj,_Filter),
        a5obj_add_backpointer(X,Obj,Role,InvFiller,N_Obj),
        a5odb_over_store(Key,N_Obj).

a5objid_add_fillers(X,Key,Fs,Role) :-
        a5odb_get(Key,Obj,_Filter),
        a5obj_add_fillers(X,Obj,Fs,Role,N_Obj),
        a5odb_over_store(Key,N_Obj).

a5objid_some_role_closed_p(X,Key,Roles) :-
        a5odb_get(Key,Obj,_Filter),
	a5obj_some_role_closed_p(X,Obj,Roles).

a5objid_store(C,Obj) :-
        a5odb_store(C,Obj).


a5objid_tmp_copy(ID) :- 
        a5odb_get(ID,Obj,_),
	!, 
        a5odb_store_tmp(ID,Obj).
a5objid_tmp_copy(_). /* don't make a copy from nothing */ 


%% a5objid_new_vrs(ObjId, NFsel, CurrentNF, RoleVrList)

a5objid_new_vrs(ObjId, NFsel, NF1, L) :-
	a5objid_nf_x(NFsel, ObjId, _Filter, NF0),
	b5nf_delta_1(NF0,NF1,L).


/*
a5objid_new_vrs(ID,CPF,RVRl) :-
	a5odb_get_tmp(ID,Obj,Filter),!,
	a5obj_nf_s(Obj,NFS),
	%t5cpf_create(TmpNF,_,NFS),
	%t5cpf_create(NF,_,CPF),
	%b5nf_delta(TmpNF,NF,RVRl). 
	b5nf_delta(NFS,CPF,RVRl). 

a5objid_new_vrs(_,_,[]). 
*/
/* wenn es keine tempor"are version gibt, sind alle neu und damit nicht
spezieller */


a5objid_undo(Key) :-
	a5odb_undo(Key).


a5objid_reset_new_fillers(ID) :- 
        a5odb_get(ID,Obj,_),
	a5obj_reset_new_filler(Obj,NObj),
        a5odb_over_store(ID,NObj).
	

a5objid_filled_role_and_fillers(X,Key,REL) :-
        a5odb_get(Key,Obj,_),
	a5obj_filled_roles_and_fillers(X,Obj,REL).


a5objid_possible_instance_p(X,Obj_key,C_Key) :-
	a5objid_nf_x(X,Obj_key,_,XNF),
	t5concid_normal_form(C_Key,CNF),
	b5nf_unify(XNF,CNF,UNI),
	\+ b5nf_incoherent_p(UNI).

a5objid_tmp_nfu(Key,NFU) :-
	a5odb_nf_x_tmp(nfu,Key,NFU).


%% a5objid_add_conc_yields_nothing_p(+ObjID, +NFsel, +ConcID)
%%  True if when adding the NF of ConcID to the NFsel-NF of ObjID yields
%%  an incoherent NF.
%% N.B. This does not take into account incoherences that occur due to
%%  incompatibility of VRs of ConcID with the types of fillers of ObjID.


a5objid_add_conc_yields_nothing_p(ObjID, X, ConcID) :-
	a5odb_get(ObjID,Obj,_),
	a5obj_add_conc_yields_nothing_p(X,Obj,ConcID). 
		 
a5objid_add_conc_yields_nothing_p(ObjID, X, ConcID1,ConcID2) :-
	a5odb_get(ObjID,Obj,_),
	a5obj_add_conc_yields_nothing_p(X,Obj,ConcID1,ConcID2).
:- multifile b5dump_these_predicates/1.

b5dump_these_predicates([a5odb/2]).


% --------------------------------------------------------------------- %
% odb
% --------------------------------------------------------------------- %

a5odb_init :-
	retractall(a5odb(_,_)),
	!.

a5odb_get(Key,Obj) :-
	a5odb(Key,Obj).

a5odb_try_get(Key,Obj,yes) :-
	a5odb_get(Key,Obj),!.
	
a5odb_try_get(_Key,_Ob,no).

a5odb_remove(Key) :-
	retract(a5odb(Key,_)), !.

a5odb_remove(Key,Obj) :-
	retract(a5odb(Key,Obj)), !.

a5odb_store(Key,Obj) :-
        assert(a5odb(Key,Obj)).

a5odb_overstore(Key,N_Obj) :-
	a5odb_remove(Key),
	a5odb_store(Key,N_Obj).

a5odb_get_all_keys(Keys) :-
	findall(Key,a5odb(Key,_),Keys),!.


% --------------------------------------------------------------------- %
%odbtmp
% --------------------------------------------------------------------- %

a5odb_tmp_init :-
	retractall(a5odbtmp(_,_)).

a5odb_get_tmp(Key,Tmp_Obj) :-
	a5odbtmp(Key,Tmp_Obj).

a5odb_remove_tmp(Key) :-
	retract(a5odbtmp(Key,_)), !.

a5odb_remove_tmp(Key,Obj) :-
	retract(a5odbtmp(Key,Obj)), !.

a5odb_store_tmp(Key,Obj) :-
        assert(a5odbtmp(Key,Obj)).

a5odb_get_all_keys_tmp(Keys) :-
	bagof(Key,Obj^a5odbtmp(Key,Obj),Keys),!.


% --------------------------------------------------------------------- %



a5odb_abox_init(Objs) :-
	a5odb_init, 
	a5odb_abox_init2(Objs).

a5odb_abox_init2([]).
a5odb_abox_init2([Obj|Objs]) :-
	a5obj_create(Obj,OBJECT),
	a5odb_store(Obj,OBJECT),
	a5odb_abox_init2(Objs). 

a5odb_ibox_init :-
	a5odb_get_all_keys(Keys),
	a5odb_ibox_init(Keys).

a5odb_ibox_init.

a5odb_ibox_init([]).
a5odb_ibox_init([Key|Keys]) :-
	a5odb_remove(Key,Obj),
	a5obj_ibox_init(Obj,N_Obj),
	a5odb_store(Key,N_Obj),
	a5odb_ibox_init(Keys). 


/*
a5odb_ibox_init :-
        a5odb(Key,Obj),
	a5obj_nf_s(Obj,NFs),
	a5obj_s_i(Obj,NFs,N_Obj),
	a5odb_overstore(Key,N_Obj),
	fail.

a5odb_ibox_init.
*/



a5odb_get(Key,Conc,Filter) :-
	nonvar(Filter),!,
        a5odb_get(Key,Conc),
        a5obj_filter(Conc,Filter). %% cmk 21.5.92

a5odb_get(Key,Conc,Filter) :-
	var(Filter),!,
        a5odb_get(Key,Conc).


a5odb_nf(Key,NF) :-  /* 8-tung ohne filter !!!!! */
        a5odb_get(Key,Conc,_),
        t5obj_nf(Conc,NF).

a5odb_nf_x(X,Key,Filter,XNF) :-  
        a5odb_get(Key,Conc,_),
        a5obj_nf(Conc,NF),
        a5onfs_x(X,NF,XNF),
        a5obj_filter(Conc,Filter). 


a5odb_nf_x(X,Key,XNF) :-  /* 8-tung ohne filter !!!!! */
        a5odb_get(Key,Conc,_),
        a5obj_nf(Conc,NF),
        a5onfs_x(X,NF,XNF).




%% a5odb_remove_objects_without_tmp_copy(+KeyList)
%%  Removes the ODB entry for each Key in KeyList for which no
%%  temp copy exists; for Keys with a temp copy do nothing.

a5odb_remove_objects_without_tmp_copy([]).
a5odb_remove_objects_without_tmp_copy([Key|Keys]) :-
	a5odb_remove_obj_without_tmp_copy(Key),
	a5odb_remove_objects_without_tmp_copy(Keys).

a5odb_remove_obj_without_tmp_copy(Key) :-
	(a5odb_no_tmp_copy(Key) ->
	    a5odb_remove(Key)
	; true).

a5odb_no_tmp_copy(Key) :-
%	\+ a5odbtmp(Key,_).
	\+ a5odb_get_tmp(Key,_Tmp_Obj).



a5odb_get_tmp(Key,Obj,Filter) :-
        a5odb_get_tmp(Key,Obj),
        a5obj_filter(Obj,Filter).

a5odb_nf_tmp(Key,NF) :-  /* 8-tung ohne filter !!!!! */
        a5odb_get_tmp(Key,Obj,_),
        a5obj_nf(Obj,NF).

a5odb_nf_x_tmp(X,Key,XNF) :-  /* 8-tung ohne filter !!!!! */
        a5odb_get_tmp(Key,Conc,_),
        a5obj_nf(Conc,NF),
        a5onfs_x(X,NF,XNF).
/*
a5odb_undo(Key) :-
	a5odb_get_tmp(Key,Obj),
	a5odb_remove_tmp(Key),
	a5odb_overstor(Key,Obj).
 */       


a5odb_undo :-
	a5odb_get_all_keys_tmp(Keys),
	a5odb_undo(Keys).
a5odb_undo.

a5odb_undo([]).
a5odb_undo([Key|Keys]) :-
	a5odb_remove_tmp(Key,Obj),
	a5odb_overstore(Key,Obj),
	a5odb_undo(Keys).

	
a5odb_include_role(Role,Supers,Subs) :-
	t5rl_filter(Supers,LessSupers),
	t5rl_filter(Subs,LessSubs),
	a5odb_incl_role(Role,LessSupers,LessSubs).

a5odb_incl_role(Role,Supers,Subs) :-
	a5odb_get(Key,Obj),
	a5obj_include_role(Obj,Role,Supers,Subs,N_Obj),  %->? 
		(
			Obj == N_Obj -> true
			; a5odb_overstore(Key,N_Obj)
		),fail.
	
a5odb_incl_role(_,_,_).




a5onfs_create(OId,ONFs) :-
	t5tbox_anything_key(Anything),
	b5nf_create(Anything,Nf),
	b5nf_add_extension(Nf,[OId],Nf1),
	b5nf_add_min_card(Nf1,1,Nf2),
	b5nf_add_max_card(Nf2,1,ONf),
	a5onfs_raw_create(ONf,ONf,ONf,ONFs).

a5onfs_raw_create(US,SY,IM,onfs(US,SY,IM)).

a5onfs_nfu(onfs(U,_,_),U).
a5onfs_nfs(onfs(_,S,_),S).
a5onfs_nfi(onfs(_,_,I),I).

a5onfs_x(nfu,onfs(U,_,_),U).
a5onfs_x(nfs,onfs(_,S,_),S).
a5onfs_x(nfi,onfs(_,_,I),I).

a5onfs_nfs_u(onfs(_,S,I),U,onfs(U,S,I)).
a5onfs_nfs_s(onfs(U,_,I),S,onfs(U,S,I)).
a5onfs_nfs_i(onfs(U,S,_),I,onfs(U,S,I)).

a5onfs_r_nfu(onfs(U,S,I),U,NU,onfs(NU,S,I)).
a5onfs_r_nfs(onfs(U,S,I),S,NS,onfs(U,NS,I)).
a5onfs_r_nfi(onfs(U,S,I),I,NI,onfs(U,S,NI)).


a5onfs_nfs(nfu,onfs(_,S,I),U,onfs(U,S,I)).
a5onfs_nfs(nfs,onfs(U,_,I),S,onfs(U,S,I)).
a5onfs_nfs(nfi,onfs(U,S,_),I,onfs(U,S,I)).

a5onfs_r(nfu,onfs(U,S,I),U,NU,onfs(NU,S,I)).
a5onfs_r(nfs,onfs(U,S,I),S,NS,onfs(U,NS,I)).
a5onfs_r(nfi,onfs(U,S,I),I,NI,onfs(U,S,NI)).

a5onfs_modified_p(ONFS1,ONFS2 ) :- 
	a5onfs_raw_create(US1,SY1,IM1,ONFS1),
	a5onfs_raw_create(US2,SY2,IM2,ONFS2),
	b5nf_modified_p(US1,US2),
	b5nf_modified_p(SY1,SY2),
	b5nf_modified_p(IM1,IM2).


?- ensure_loaded(library(ordsets)).
?- ensure_loaded(library(basics)).

%% RECOGNITION MODULE (cmk)
%% ========================
%% 
%%  o  ABox specific completion
%%  o  Backward Propagation
%%  o  ABox classification

/* --- Exports ----------------------------------------

a5rec_vrs_abstractable_p(+RoleVrList, +ObjID, +ON, +NFselector)
a5rec_vrs_abstractable_p(+RoleVrList, +ObjID, +ONF, +Scheduler,-NewScheduler)
	Interface for RETR Module. True if for every pair Role-VR
	in RoleVrList, VR of Role is satisfied by ObjID acc. to ONF.

---------------------------------------------------- */


%%
%% Forward Propagation
%%

%% a5rec_forward_propagation(+NewFillers, +ObjID, +NF, +Scheduler0, -Scheduler)
%%  Propagates (i) to NewFillers the VR of the roles filled by them, and
%%  (ii) to all fillers vrs that are more special then before.

a5rec_forward_propagation(NewFillers, ObjID, NF, S0, S) :-
	a5rec_propagate_vr_to_new_fillers(NewFillers, ObjID, NF, S0, S1),
	a5rec_propagate_new_vrs_to_fillers(ObjID, NF, NewFillers, S1, S).


%% a5rec_propagate_vr_to_new_fillers(+NewFillers, +ObjID, +ONF, +S0, -S)
%%  Propagates the VRs of roles in ONF to NewFillers.

a5rec_propagate_vr_to_new_fillers([], _, _, S, S).
a5rec_propagate_vr_to_new_fillers([Role-re(RFs)|NewRFs], ObjID, ONF, S0, S) :-
	b5nf_vr(ONF, Role, VR),
	a5rec_propagate_vr_to_fillers(RFs, Role, VR, ObjID, S0, S1),
	a5rec_propagate_vr_to_new_fillers(NewRFs, ObjID, ONF, S1, S).


%% a5rec_propagate_new_vrs_to_fillers(+ObjID, +ONF, +NewFillers, 
%%                                    +Scheduler0, -Scheduler).
%%  Propagates new VRs as held in ONF to all Fillers held in ONF.
%%  Avoids propagating VR to new fillers which has been done already
%%  by a5rec_propagate_vr_to_new_fillers/5.
%%  (NewFillers is a list of RoleExtensions (i.e. pairs Role-re(Fillers), 
%%  Fillers being sorted).
%%  RoleVRs is a sorted (?) list of pairs Role-VR.)
%% 
%% NB: a5objid_new_vrs/3 is not able to deal with non-monotonicity.
%%  Initially we relied on the fact that whenever an object is revised, 
%%  all its filler relationships are treated as new. As a consequence 
%%  all VRs have been propagated by a5rec_propagate_vr_to_new_fillers/5.
%%  This, however, is only true for the NFs phase. During the NFi phase
%%  the following situation may occur in the retraction mode:
%%  - a y1 is a new filler wrt. NFs;
%%  - a y2 is a new filler wrt. NFi;
%%  - y1 is not any longer "new" wrt NFi !
%%  - thus not all VRs are propagated to y1
%%  - instead we have to find the NewVRs where we have to be cautious
%%    because a5objid_new_vrs/3 is not able to deal with non-monotonicity.
%%  - non-monotonicity wrt. NFi can only occur for one of the DepObjs;
%%    it will occur there because we did not store the re-initialized NFi
%%    to avoid an unnecessary assert!
%%  The solution is to compute NewVRs as the delta of NFs and NFi.
%%  Only disadvantage: on iteration k we again compare NFi^k against NFs
%%  instead of NFi^k-1 in the monotonic case.

a5rec_propagate_new_vrs_to_fillers(ObjID, NF, NewRFs, S0, S) :-
	a5sch_scheduler(nf_selector,S0,NFsel),
	((a5sch_scheduler(depending_objects,S0,DepObjs),
	  memberchk(ObjID,DepObjs)) ->
	       a5sch_previous_nf(NFsel,BasisNFsel),
	       a5objid_new_vrs(ObjID, BasisNFsel, NF, RoleVRs)
	;      a5objid_new_vrs(ObjID, NFsel,      NF, RoleVRs)),  
	a5rec_propagate_new_vrs_to_fillers(RoleVRs, ObjID, NF, NewRFs,S0,S).

a5rec_propagate_new_vrs_to_fillers([], _, _, _, S, S).
a5rec_propagate_new_vrs_to_fillers([Role-vr(VR)|Tail], ObjID,NF,NewRFs,S0,S) :-
	b5nf_fillers(NF, Role, Fillers),
	( selectchk(Role-re(NewFillers), NewRFs, NewRFsResidue) ->
	    ord_subtract(Fillers, NewFillers, DiffRFs)
	;   DiffRFs = Fillers, NewRFsResidue = NewRFs ),
	a5rec_propagate_vr_to_fillers(DiffRFs, Role, VR, ObjID, S0, S1),
	a5rec_propagate_new_vrs_to_fillers(Tail, ObjID,NF,NewRFsResidue, S1,S).


%% a5rec_propagate_vr_to_fillers(+Fillers, +Role, +VR, +ObjID, +S0, -S)
%% FFS: What if VR is number, attr, or string? Type-Checking ??

a5rec_propagate_vr_to_fillers(_, _, VR, _, S, S) :-
	t5tbox_anything_key(VR),
	!. %% RED %%
a5rec_propagate_vr_to_fillers(Fillers, Role, VR, ObjID, S0, S) :-
	%% tboxIF_conc_p(VR), 
	t5tbox_anything_key(AnythingKey),
	t5concid_subsumes_p(AnythingKey, VR),
	!, %% RED %%
	t5out_trace(forward_propagate_vr(VR, ObjID, Role, Fillers)),
	a5sch_scheduler(nf_selector,S0,NFsel),
	a5rec_propagate_vr(Fillers, Role, VR, ObjID, NFsel, S0, S).
a5rec_propagate_vr_to_fillers(_, _, _, _, S, S).

a5rec_propagate_vr([], _, _, _, _, S, S).
a5rec_propagate_vr([Filler|Fillers], Role, VR, ObjID, NFsel, S0, S) :-
	a5dep_dependency(NFsel, Filler, ObjID, cinst(ObjID,all(Role,VR)), Dep),
	a5sch_add_to_agenda(tell(Filler, ObjID, [VR], Dep), S0, S1),
	a5rec_propagate_vr(Fillers, Role, VR, ObjID, NFsel, S1, S).

%%
%% Role Completion
%%

%% a5rec_role_completion(+NFselector, +Obj0,-Obj, +Scheduler,-NewScheduler)
a5rec_role_completion(NFsel,Obj0,Obj, S0,S) :-
	%% a5obj_nf(NFsel, Obj0, NF0),	
	a5obj_new_fillers(Obj0, NewFillers0),
	a5obj_reset_new_fillers(Obj0,Obj1),
	a5obj_id(Obj1, ObjID),
	a5roco_role_completion(NewFillers0, ObjID, NFsel, Obj1, Obj, S0, S).


%% 
%% (B) Backward Propagation
%% 
 
/*

Wenn Obj selber Filler ist, 
und sich sein Typ veraendert hat (nur dann sollte das Flag ON sein ..)
dann den durch BackPointer referenzierten Objekten ein CLASSIFY request
schicken.

- nicht an Objekte, wenn neues C von dort als VR kam
- nur wenn RF set closed (dort behandeln ??)

In V4 wurde zwar angedacht, (A) fuer (B) gesondert zu behandeln;
durchgefuehrt wurde dies nicht.
Allerdings wurde auch kein BP an Trigger veranlasst.

BP von Y an X nur wenn closed(R): 
 (a) X ist vollstaendig bearbeitet -> closed(R) kann festgestellt werden
 (b) X ist nicht vollstaendig bearbeitet (zumindest classify fehlt!) 
     -> (b1) closed(R) gilt: BP
        (b2) nicht closed(R): sicherhaltshalber CLASSIFY request;
	     dieser mag ueberfluessig sein, wenn X zwar auf Agenda,
             aber nicht tatsaechlich veraendert, d.h. CLASSIFY mithin
	     unnoetig ist.
==> Als Entscheidung kann gelten:
        BP von Y an X nur wenn closed(R), oder wenn X nicht vollstaendig
	klassifiziert,

Weitere Optimierungen, wie auch in V4 nur angedacht, spaeter.

a5rec_backward_propagate(Obj, S0,S) :-
	get backpointer info
	   format Obj-RoleList um mehrfach req zu vermeiden
	for each x with filled roles Ri
	   if at least one Ri is not closed
	   then issue a CLASSIFY request

*/

%! a5rec_backward_propagate(+ObjID, +ONFselector, +Scheduler, -NewScheduler)
%%  Issues an CLASSIFY request for all those objects where ObjID is 
%%  a role filler in some closed filler set.
%%
%% FFS: Further optimizations are possible! Currently we do not exploit
%%  the fact that the classification searchspace is smaller in case
%%  it is invoked by backward propagation.
%%  Also a CLASSIFY request may be issued superfluously in case
%%  the filled object has not been recognized completely.

a5rec_backward_propagate(Obj, ONFsel, S0,S) :-
	a5obj_fills(ONFsel, Obj, BackwardRoleExtensionList),
	a5obj_id(Obj,ObjID),
	a5rec_backprop(BackwardRoleExtensionList, ObjID, ONFsel, S0,S).

a5rec_backprop([], _, _, S,S).
a5rec_backprop([InvObjID-bre(Roles)|Tail], ObjID, ONFsel, S0, S) :-
	( a5objid_some_role_closed_p(ONFsel, InvObjID, Roles) ->
	     a5sch_add_to_agenda(classify(InvObjID), S0, S1)
	; S0 = S1),
	a5rec_backprop(Tail, ObjID, ONFsel, S1, S).

%%
%% Classification 
%%

%% a5rec_classify(+ONFsel, +Obj, -NewObj, +Scheduler, -NewScheduler).
%% Bei classify ist darauf zu achten, dass nach erneuten Abstraktionen
%% wieder lokal kompletiert werden muss. In dem Fall sollte
%% die complete-locally Methode des MO onf direkt aufgerufen anstatt
%% ueber die Agenda zu gehen (dort wuerde danach naemlich wieder ein 
%% complete initiiert (oder nicht, weil das Flag geloescht ist ????).
%%
%% FFS: Classification fuer Backward Propagation
%% FFS: anstatt des Schedulers nur Dependencies durchreichen !?
%%
%% - determine searchspace/primsubsumers
%% - WHILE searchspace not empty 
%%   DO select an arbitrary candidate
%%      compare object against candidate
%%      update remaining searchspace.

a5rec_classify(ONFsel, Obj0,Obj, S0,S) :-
	a5obj_nf(ONFsel, Obj0, ONF0),
	a5obj_id(Obj0,ObjID),
	b5nf_compute_prim_subsumers(ONF0, CandConcs),
	a5rec_classify_nf(CandConcs, /* Misses */ [], ObjID,ONF0,ONF, S0,S),
	a5obj_replace_nf(ONFsel, ONF, Obj0, Obj).


%% a5rec_classify_nf(+CandConcs,+Misses, +ObjID,+ONF,-NewONF,+S,-NewS)
%%  the loop

a5rec_classify_nf([],_,_,ONF,ONF,S,S).
a5rec_classify_nf([C|Cs0], Ms0, ObjID, ONF0, ONF, S0,S) :-
	a5rec_abstract_conc(C, Ms0,Ms, ObjID,ONF0,ONF1, S0,S1, Res),
	a5rec_update_searchspace(Res, C, Cs0, Cs),
	a5rec_classify_nf(Cs, Ms, ObjID, ONF1, ONF, S1,S).


%% a5rec_abstract_conc(+Cand, +Misses0,-Misses, ObjID,+ONF0,-ONF, +S0,-S, -Res)
%%  Result is one of [hit,miss].
%%  Classify against a single concept
%% Note: NRs have to be tested also in case of backward propagation
%%  triggered abstraction. Otherwise NRs for roles not concerned
%%  by backward propagation would be abstracted without the required
%%  justification.
%% FFS: If Cand is abstracted it may be added to onf.supers !?
%% FFS: abstract NRs as in RETR module by eliminating VRs and using
%%  subsumption test on NF's !?
%%
%% cmk 26.1.91: In V5, fillers are part of concept normal-forms and defs.
%%  Consequently, they have to be checked during object classification
%%  just as NRs. 

a5rec_abstract_conc(C, M0,M, ObjID,ONF0,ONF, S0,S, R) :-
	%% a4info_trace(try_abstract_msc(UC, Cand)),
	a5rec_check_precomputed_facts(C, M0,M1, ObjID, ONF0, ONF1,S0,S1,R0),
	a5rec_abstract_conc(R0, C, M1,M, ObjID,ONF1,ONF2, S1,S, R),
	( R == hit ->
	    b5nf_add_super_conc(ONF2,C,ONF) 
	; ONF = ONF1).
	    

a5rec_abstract_conc(miss, _, M,M, _,ONF,ONF, S,S, miss).
a5rec_abstract_conc(hit, C, M0,M, ObjID,ONF0,ONF, S0,S, R) :-
	a5rec_vrs_abstraction(C, M0,M, ObjID,ONF0,ONF, S0,S, R).
	

%% a5rec_update_searchspace(+Result, +Conc, +CandConcs, -NewCandConcs)
%%  deletes redundant concs from CandConcs to yield NewCandConcs.
%% FFS: a4info_trace(could_not_abstract_msc(UC, C)) bzw.
%%  a4info_infos(abstracted_msc(UC, Cand)), ausserdem
%%  a4info_trace(remove_from_abstraction_searchspace(RemConcsAndCand)),

a5rec_update_searchspace(hit, C, Cs0, Cs) :-
	t5concid_supers(C, Supers),
	b5sort_difference(Cs0, Supers, Cs).
a5rec_update_searchspace(miss, C, Cs0, Cs) :-
	t5concid_subs(C, Subs),
	b5sort_difference(Cs0, Subs, Cs).


%% a5rec_check_precomputed_facts(+ConcId, +Misses0,-Misses,
%%				+ObjId, +ONF0,-ONF,+S0,-S, -Result)
%%  Checks if those restrictions in Conc which for objects are computed
%%  using forward-chained inferences are all satisfied by ONF.
%%  If so, Result is hit, miss otherwise.
%%
%% cmk 7.7.93: Introduces special case for atmost(0,R) restrictions that
%%  didn't pass the test on this pass. They will be processed after 
%%  the easy tests have been performed.

a5rec_check_precomputed_facts(C, M0,M, ObjId, ONF0,ONF,S0,S,  R) :-
	(a5rec_extension_memberchk(C, ObjId) ->
	    a5rec_nrs_and_fillers_abstraction(C, M0,M, ObjId, ONF0,ONF,S0,S, R)
	; R = miss, M0=M, ONF0=ONF, S0=S).


a5rec_extension_memberchk(C, ObjId) :-
	 (t5concid_closed_concept_p(C) ->
	     t5concid_extension_memberchk(C,ObjId)
	; true).
	

%% a5rec_nrs_and_fillers_abstraction(+Conc, +M0,-M, +ObjID,+ONF0,-ONF,+S0,-S, -Result)
%%  Result is one of [hit,miss].
%%  Result is hit if all NRs and FillerExpressions of Conc are satisfied
%%  by ONF; miss else.
%%
%% cmk 7.7.93: Abstraction of atmost(0,R) restrictions, short 0R, has
%%  to take into account also previous misses and the object-level;
%%  it updates Misses and the ONF (closed-status).

a5rec_nrs_and_fillers_abstraction(Conc, M0,M, ObjID,ONF0,ONF,S0,S, Result) :-
	a5rec_nrs_and_fillers_abstraction(Conc,ObjID,ONF0, NullNrList,Result0),
	a5rec_abstract_atmost_0_nrs(Result0, NullNrList, M0,M, ObjID, ONF0, ONF,S0,S, Result). 


%%  foreach cres(R) is CResList:
%%  	if ores(R) then 
%%  		if subsumes(cres,ores) then ok
%%  		else if cres a OR then ok else fail
%%  	else if cres a OR then ok else fail

a5rec_nrs_and_fillers_abstraction(Conc, _,ONF, NullNrList, Result) :-
	%% a4info_trace(try_abstract_nrs_and_fillers(UC, Conc)),
	t5concid_reslist(Conc, CResList),
	t5cpf_reslist(ONF, OResList), 
	a5rec_abstract_nrs_and_fillers(hit,CResList,OResList,[],NullNrList,Result).


%% a5rec_abstract_nrs_and_fillers(+ResultSoFar, +ConcResList, +ObjResList, 
%%				+NullNrList0,-NullNrList, -Result)

a5rec_abstract_nrs_and_fillers(miss, _, _, _, _, miss).
a5rec_abstract_nrs_and_fillers(hit, CResList0, OResList, NL0,NL, Result) :-
	( t5rl_done(CResList0) ->
	    Result = hit, NL = NL0
	; t5rl_next_1(CRes, CResList0, CResList),
	    a5rec_abstract_nr_and_fillers(CRes, OResList, NL0,NL1, Result0),
	    a5rec_abstract_nrs_and_fillers(Result0, CResList,OResList, NL1,NL, Result)
	).

%% a5rec_abstract_nr_and_fillers(+CRes, +OResList,
%%				 +NullNrList, -NewNullNrList, -Result)
%%  True if the NR and Filler in CRes is satisfied by the according ORes
%%  in OResList.  Result is one of [hit,miss].

%% FFS: a4info_trace(abstracted_nr(TNR, UcCard)),
%%  bzw. a4info_trace(could_not_abstract_nr(TNR, UcCard))
%%  acc. for Filler

a5rec_abstract_nr_and_fillers(CRes, OResList, NL0,NL, Result) :-
	t5res_role(CRes, R),
	t5rl_role_res(OResList, R, ORes), 
	(a5rec_fillers_abstractable_p(CRes, ORes) ->
	     a5rec_abstract_nr(CRes, ORes, NL0,NL, Result)
	; Result = miss).

a5rec_fillers_abstractable_p(CRes, ORes) :-
	t5res_fillers(CRes, CFillers),
	t5res_fillers(ORes, OFillers),
	b5sort_subset_p(CFillers,OFillers).


%% a5rec_abstract_nr(+CRes, +ORes, +NL0,-NL, -Result)
%%  Attempts to abstract the NR in CRes for ORes.
%%  If the NR is an atmost(0,R) restriction and cannot be abstracted here
%%  it is passed to the NL (NR-List) accu for later backward-chained
%%  processing.

a5rec_abstract_nr(CRes, ORes, NL0,NL, Result) :-
	t5res_nr(CRes, CNr),
	t5res_nr(ORes, ONr),
	(t5nr_subsumes_p(CNr, ONr) ->
	     NL0 = NL, Result = hit
	; ((t5nr_empty_p(CNr), t5nr_optional_p(ONr)) ->
	       t5res_role(CRes, R),
	       t5res_vr(CRes, VR), 
	       NL = [R-nullvr(VR)|NL0], %t5rl_add_role_res(NL0, CRes, NL),
	       Result = hit
	; Result = miss /* , NL0 = NL */ )).



%%% Backward-Chained Abstraction of atmost(0,R) NRs

%% a5rec_atmost_0_nrs_abstractable_p(+NullNrList,+ObjID,+ONF,+NFselector).
%% a5rec_atmost_0_nrs_abstractable_p(+NullNrList,+ObjID,+ONF,+Scheduler,-NewScheduler).
%%  Interface for RETR Module. True if for every pair Role-nullvr(VR)
%%  in NullNrList, atmost(0,Role) is satisfied by ObjID acc. to ONF.
%% 
%% FFS: there is a little overhead for filling the scheduler which
%%  for pure retrieval is just a dummy.

a5rec_atmost_0_nrs_abstractable_p(NullNrList, ObjID, NFsel, ONF0,ONF) :-
	a5sch_make_scheduler(NFsel, S), %% req. for REC code.
	a5rec_abstract_atmost_0_nrs(hit,NullNrList,[],_,ObjID,ONF0,ONF,S,_,hit).
a5rec_atmost_0_nrs_abstractable_p(NullNrList, ObjID,ONF0,ONF, S0,S) :-
	a5rec_abstract_atmost_0_nrs(hit,NullNrList,[],_,ObjID,ONF0,ONF,S0,S,hit).


%% a5rec_abstract_atmost_0_nrs(+Result0, +NullNrList, +M0,-M, +ObjID, +ONF0, -ONF,+S0,-S, -Result). 
%%  Attempts to abstract an atmost(0,R) restriction. Assumes that earlier
%%  the possibility of such an abstraction was checked.
%%  NullNrList is a list of elements RoleKey-nullnr(ConcVR)

a5rec_abstract_atmost_0_nrs(miss, _, M,M, _, ONF,ONF,S,S, miss).
a5rec_abstract_atmost_0_nrs(hit, NullNrList, M0,M, ObjID, ONF0,ONF,S0,S, Result) :-
	a5rec_abstract_atmost_0_nrs(NullNrList, M0,M, ObjID, ONF0,ONF,S0,S, Result).

a5rec_abstract_atmost_0_nrs([], M,M, _, ONF,ONF,S,S, hit).
a5rec_abstract_atmost_0_nrs([NullNr|List], M0,M, ObjID,ONF0,ONF,S0,S,Result) :-
	a5rec_abstract_atmost_0_nr(NullNr, M0,M1,ObjID,ONF0,ONF1,S0,S1,Result0),
	a5rec_abstract_atmost_0_nrs(Result0, List, M1,M, ObjID,ONF1,ONF,S1,S, Result).


%% a5rec_abstract_atmost_0_nr(+NullNrRes, +M0,-M, +ObjID,+ONF0,-ONF,+S0,-S,-Result)
%%  NullNrRes is a restriction (MO t5res)

a5rec_abstract_atmost_0_nr(NullNrRes, M0,M, ObjID,ONF0,ONF, S0,S, Result) :-
	(member(NullNrRes, M0) ->
	    Result == miss, M0 = M, ONF0 = ONF, S0 = S
	; (NullNrRes = R-nullvr(VR) -> %% format check
	    a5rec_atmost_0_nr_abstractable(R,VR, ObjID,ONF0,ONF1,S0,S,Result),
	    (Result == miss -> 
		ord_add_element(M0, NullNrRes, M), ONF1 = ONF
	    ;   b5nf_add_max(ONF1,0,R,ONF),
	        M0 = M)
	; t5out_panic(a5rec_abstract_atmost_0_nr(NullNrRes)), 
	    !, fail
	)).


%% a5rec_atmost_0_nr_abstractable(+R,+VrID, +ObjId,+ONF0,-ONF, +S0,-S, -Result)
a5rec_atmost_0_nr_abstractable(R, VR, ObjID, ONF0, ONF, S0,S, Result) :-
	b5nf_closed(ONF0, R, ClosedSuperRoles, ONF, Flag),
	(Flag == no ->
	    Result = miss, S0 = S
	;   b5nf_roles_fillers(ONF, ClosedSuperRoles, Fillers), %% <-- :- uk
	    (a5rec_atmost_0_nr_abstractable_on_object_level_p(ObjID,R,VR,Fillers,S0,S) ->
		Result = hit; Result = miss)
	).

%% a5rec_atmost_0_nr_abstractable_on_object_level_p(+ObjID +RoleID,+VrID,+Fillers,+S0,-S)
%% FFS: pass Scheduler as accu for dependencies

a5rec_atmost_0_nr_abstractable_on_object_level_p(ObjID, Role, CVr, Fillers, S0,S) :-
	a5sch_scheduler(nf_selector, S0, NFsel),
	a5objid_vr(NFsel, ObjID, Role, OVr),
	a5rec_objs_are_incoherent_with_vr_p(Fillers, NFsel, CVr, OVr),
	a5rec_vnr_abstraction_dependencies(Fillers, ObjID, NFsel,nr,S0,S).


%% a5rec_objs_are_incoherent_with_vr_p(+ObjIDList, +NFsel, +VR1, +VR2)
a5rec_objs_are_incoherent_with_vr_p([], _, _, _).
a5rec_objs_are_incoherent_with_vr_p([ObjID|ObjIDs], NFsel, VR1, VR2) :-
	a5objid_add_conc_yields_nothing_p(ObjID, NFsel, VR1, VR2),
	a5rec_objs_are_incoherent_with_vr_p(ObjIDs, NFsel, VR1, VR2).



%%% Backward-Chained Abstraction of VR

%% a5rec_vrs_abstractable_p(+RoleVrList, +ObjID, +ONF, +NFselector)
%% a5rec_vrs_abstractable_p(+RoleVrList, +ObjID,+ONF, +Scheduler,-NewScheduler)
%%  Interface for RETR Module. True if for every pair Role-VR
%%  in RoleVrList, VR of Role is satisfied by ObjID acc. to ONF.
%% 
%% FFS: there is a little overhead for filling the scheduler which
%%  for pure retrieval is just a dummy.

a5rec_vrs_abstractable_p(RoleVrTypeList, ObjID, ONF, NFsel) :-
	a5sch_make_scheduler(NFsel, S), %% req. for REC code.
	b5typelist_map(RoleVrTypeList,RoleVrList, a5rec_vr_unwrap),
	a5rec_vrs_abstraction_loop(RoleVrList, [],_, ObjID, ONF,_, S,_, hit).
a5rec_vrs_abstractable_p(RoleVrTypeList, ObjID, ONF, S0,S) :-
	b5typelist_map(RoleVrTypeList,RoleVrList, a5rec_vr_unwrap),
	a5rec_vrs_abstraction_loop(RoleVrList, [],_, ObjID, ONF,_, S0,S, hit).

a5rec_vr_unwrap(K-vr(X),K-X).


%% a5rec_vrs_abstraction(+Conc, +Misses0,-Misses, 
%%                       +ObjID,+ONF0,-ONF, +S0,-S, -Result)
%%  Result is one of [hit,miss].
%%  Processes the restrictions of Conc in two phases: (i) process
%%  directly closed filler sets, (ii) process indirectly closed
%%  filler sets.

%% FFS: ein Pred, das die Liste der Rollenfuellersets aufteilt in
%%  directly_closed und indirectly_closed ??
%%  !! die CResList aufteilen !!

a5rec_vrs_abstraction(Conc, M0,M, ObjID,ONF0,ONF, S0,S, Result) :-
	%% a4info_trace(try_abstract_vrs(UC, Conc)),
	t5concid_reslist(Conc, CResList),
	a5rec_reslist_to_role_vr_pairs(CResList, CRoleVRs),
	a5rec_vrs_abstraction_loop(CRoleVRs,M0,M,ObjID,ONF0,ONF,S0,S,Result).

a5rec_vrs_abstraction_loop([], M,M, _, ONF,ONF, S,S, hit).
a5rec_vrs_abstraction_loop([CRVR|CRoleVRs0], M0,M, ObjID,ONF0,ONF, S0,S,Res) :-
	a5rec_abstract_vrs(hit, d, [CRVR|CRoleVRs0],CRoleVRs1, 
	                   M0,M1, ObjID,ONF0,ONF1, S0,S1, Res1),
	a5rec_abstract_vrs(Res1, i, CRoleVRs1,[], 
	                   M1,M, ObjID,ONF1,ONF, S1,S, Res).


%% a5rec_abstract_vrs(+ResultSoFar, +Phase
%%                    +ConcRoleVRs,-ResidueConcRoleVRs, +Misses0,-Misses,
%%                    +ObjID,+ONF,-NewONF, +Scheduler,-NewScheduler, -Result)
%%  Result and ResultSoFar are oneof [hit,miss],
%%  Phase is one of [d /* directly closed */, i /* indirectly closed */].
%%
%% FFS: Unfold Phase earlier and ommit ResidueConcResList for 'i' phase

a5rec_abstract_vrs(miss, _, _,_, M,M, _,ONF,ONF, S,S, miss).
a5rec_abstract_vrs(hit,Phase, CRVRs0,CRVRs, M0,M, ObjID,ONF0,ONF, S0,S,Res) :-
	( CRVRs0 = [] ->
	    Res = hit, CRVRs0 = CRVRs, M0 = M, ONF0 = ONF, S0 = S
	; CRVRs0 = [ CRes | CRVRs1 ],
	    a5rec_abstract_vr(Phase,CRes,CRes1,M0,M1,ObjID,ONF0,ONF1,S0,S1,R1),
	    a5rec_add_non_consumed_res(CRes1, CRVRs2, CRVRs),
	    a5rec_abstract_vrs(R1,Phase, CRVRs1,CRVRs2, 
	                       M1,M, ObjID,ONF1,ONF, S1,S, Res)
	).



%% adds an unprocessed CRes back to CRoleVRs.
a5rec_add_non_consumed_res('*', CRoleVRs, CRoleVRs) :- !. %% var(CRes)
a5rec_add_non_consumed_res(CRes, CRoleVRs, [CRes|CRoleVRs]).


%% cmk 19.3.93: introduced trivialHit as a new result returned from 
%%  VR-abstraction preds. trivialHit represents abstraction of VRs
%%  which are already known in the ONF. 
%%  This yielded a speedup of a5rec_classify for cmktest approx. by factor 2
%%  (others ??).
%%  trivialHit is a value local to a5rec_abstract_vr!

a5rec_abstract_vr(P, Role-VR, RoleVR, M0,M, ObjID,ONF0,ONF, S0,S, Result) :-
	a5rec_abstract_vr(P,Role,VR,RoleVR,M0,M1,ObjID,ONF0,ONF1,S0,S,Result0),
	(Result0 == trivialHit -> Result = hit; Result = Result0),
	a5rec_vr_abstraction_update_accus(Result0,RoleVR,Role,VR,M1,M,ONF1,ONF).

a5rec_abstract_vr(d, Role,VR, RoleVR, M,M, ObjID,ONF0,ONF, S0,S, Result) :-
	a5rec_abstract_vr_d(Role,VR, RoleVR, M, ObjID,ONF0,ONF, S0,S, Result).
a5rec_abstract_vr(i, Role,VR, _, M0,M, ObjID,ONF0,ONF, S0,S, Result) :-
	a5rec_abstract_vr_i(Role,VR, M0,M, ObjID,ONF0,ONF, S0,S, Result).

%% a5rec_abstract_vr_d(+Role,+VR,-CRoleVR, +Misses, +ObjID,+ONF0,-ONF
%%                     +S0,-S,-Result)
%%  Tries to abstract VR in the 'directly closed' phase.
%%  CRoleVR is returned variable if we could not decide abstraction of VR 
%%  during the 'd' phase (Result=hit).
%%  - VR = range(Role) -> trivialHit (just a shortcut)
%%  - VR subsumes the according vr in ONF0 -> trivialHit
%%  - VR is in M0 -> miss
%%  - if according filler set is directly closed then
%%    - if all filler are instances of VR -> hit ; /* else */ miss
%%    else return Role & VR to be processed in the 'i' phase.
%%
%% FFS: a4info_trace(abstracted_vr(TNR, UcCard)),
%%  bzw. a4info_trace(could_not_abstract_vr(TNR, UcCard))

a5rec_abstract_vr_d(R, VR, _, _, _, NF,NF, S,S, trivialHit) :-
	t5role_range(R, VR),
	!. 
a5rec_abstract_vr_d(R, VR, _, _, _, NF,NF, S,S, trivialHit) :-
	a5rec_vr_abstractable_intensionally_p(R, VR, NF),
	!. 
a5rec_abstract_vr_d(R, VR, _, Misses, _, _NF,_NF, S,S, miss) :-
	member(R-VR,Misses),
	!.
a5rec_abstract_vr_d(R, VR, CRoleVR, _, ObjID, ONF0,ONF, S0,S, Result) :-
	b5nf_directly_closed(ONF0, R, ORes, ONF, Closed), 
	%% FFS: also returns default for ORes ?
	( Closed == yes ->
	    %% CRoleVR remains variable
	    ( a5rec_vr_abstractable_on_object_level_p(VR, ObjID, ORes, S0,S) ->
		Result = hit
	    ; S0 = S, Result = miss)
	; CRoleVR = R-VR, S0 = S, Result = hit
        ).


%% a5rec_abstract_vr_i(+Role,+VR, +M0,-M, +ObjID,+ONF0,-ONF, +S0,-S, -Result)
%%  Tries to abstract VR in the 'indirectly closed' phase.
%%  Cases where VR = range(Role) has been sorted out in 'd' phase.
%%  Also, there can be no new Misses, otherwise the 'i' phase would
%%  have terminated immediately. There may, however, exist new VRs
%%  caused by invoking completion during the 'd' phase (clause 1).

a5rec_abstract_vr_i(R, VR, M,M, _,ONF,ONF, S,S, trivialHit) :-
	a5rec_vr_abstractable_intensionally_p(R, VR, ONF),
	!.
a5rec_abstract_vr_i(R, VR, M0,M, ObjID,ONF0,ONF, S0,S, Res) :- 
	b5nf_indirectly_closed(ONF0, R, DirClosedSuperRoles, ONF1, Closed),
	( Closed == yes ->
	    a5rec_pairup_roles_vr(DirClosedSuperRoles, VR, RoleVRs),
	    a5rec_vrs_abstraction_loop(RoleVRs, M0,M, ObjID,ONF1,ONF, S0,S,Res)
	; Res = miss, S0 = S, M0 = M, ONF = ONF1).


%% a5rec_vr_abstractable_intensionally_p(+Role, +ConcVR, +ONF)
%%  True if ConcVR subsumes the according VR in ONF.

a5rec_vr_abstractable_intensionally_p(R, CVR, ONF) :-
	b5nf_vr(ONF, R, OVR), %% if CRole not present returns default
	t5concid_subsumes_p(CVR, OVR).


%% a5rec_vr_abstractable_on_object_level_p(+VR, +ObjID, +ORes, +S0,-S)
%%  get CPF(VR), pass it as BQ to RETR Module

a5rec_vr_abstractable_on_object_level_p(VRId,ObjId,ORes,S0,S) :-
	t5concid_concept_p(VRId), 
	!,
	a5sch_scheduler(nf_selector, S0, NFsel),
	t5res_fillers(ORes,Fillers),
	a5sch_blackboard_objects(S0,BBObjs),
	ord_subtract(Fillers,BBObjs,IndexedFillers),
	ord_intersect(Fillers,BBObjs,UnindexedFillers),
	a5ret_key_instantiate_p(VRId,NFsel,IndexedFillers,S0,S1), 
	a5ret_key_unindexed_instantiate_p(VRId,NFsel,UnindexedFillers,S1,S2),
	%% FFS: add tell(VR) an StillUnindexedFillers to S0 ??
	a5rec_vr_abstraction_dependencies(Fillers, ObjId, VRId, NFsel,S2,S).
a5rec_vr_abstractable_on_object_level_p(VRId,_,ORes,S,S) :-
	%% not t5concid_concpt_p(VRId),
	t5res_fillers(ORes,Fillers),
	b5inst_are_instances_p(Fillers,VRId).


%% a5rec_vnr_abstraction_dependencies(+Fillers, +ObjId, +NFsel,+S0,-S).
/*
PROBLEM:
- in die dep(x,y,C) muss das C eingehen, das zusammen mit VR NOTHING ergab.
- C=NF(y) -> NF(y) ist i.d.R. kein Conc mit Id
- entweder, das Dep Modul kann auch NFs statt keys handhaben
  oder es muss eine Approximation gewaehlt werden.
- moegliche Approximation: die direkten existierenden Subs von NF(y) und
  fuer jeden davon eine Dependenz
- dies macht Klassifikation der NF(y) notwendig; evtl. Subs = [NOTHING]
+ Loesung: Erweiterung des Dependenzmoduls um cinst(ObjID,onf(ONF)) 

*/
%%  NorVRes determines the kind of dependency to be created.
%%  NorVRes currently is oneof {nr, vr(+ConcKey)}
a5rec_vnr_abstraction_dependencies([],_,_,_,S,S).
a5rec_vnr_abstraction_dependencies([Y|Ys],X,NFsel,NorVRes,S0,S) :-
	a5rec_vnr_abstr_deps([Y|Ys],X,NFsel,NorVRes,Deps),
	a5sch_add_dependency(Deps, S0,S).

a5rec_vnr_abstr_deps([],_,_,_,[]).
a5rec_vnr_abstr_deps([Y|Ys], X, NFsel, NorVRes, [Dep|Ds]) :-
	a5rec_vnr_abstr_dep(NorVRes, NFsel, X, Y, Dep),
	a5rec_vnr_abstr_deps(Ys,X,NFsel, NorVRes,Ds).

a5rec_vnr_abstr_dep(nr, NFsel, X, Y, D) :-
	a5objid_nf_x(NFsel, Y, _ /* Filter */, ONF),
	a5dep_dependency(NFsel,X,Y,cinst(Y,onf(ONF)),D).
a5rec_vnr_abstr_dep(vr(C), NFsel, X, Y, D) :-
	a5dep_dependency(NFsel,X,Y,cinst(Y,C),D).

a5rec_vr_abstraction_dependencies(Ys,X,C,NFsel,S0,S) :-
	a5rec_vnr_abstraction_dependencies(Ys,X, NFsel, vr(C), S0,S).


%% a5rec_vr_abstraction_update_accus(+Result,CRes,+Role,+VR,+M0,-M,+ONF0,-ONF).
%%  Updates the Misses accu and ONF accu depending on Result.

a5rec_vr_abstraction_update_accus(miss,_, Role,VR, M0,M, ONF,ONF) :-
	ord_add_element(M0, Role-VR, M).
a5rec_vr_abstraction_update_accus(trivialHit, _, _, _, M,M, ONF,ONF).
a5rec_vr_abstraction_update_accus(hit,CRes, Role, VR, M,M, ONF0,ONF) :-
	( var(CRes) -> 
	    t5cpf_add_vr(ONF0,VR,Role,ONF1),
	    t5cpf_complete(ONF1,ONF) %% FFS: optimized completion ??
	; %% CRes hasn't been consumed and will be processed in 'i' phase
	    ONF0 = ONF).


%% a5rec_reslist_to_role_vr_pairs(+ResList, -RoleVrList)
%%  True if RoleVrList is the list of pairs Role-VR of the restrictions
%%  contained in ResList.

a5rec_reslist_to_role_vr_pairs(ResList0, RoleVrList0) :-
	( t5rl_done(ResList0) ->
	    RoleVrList0 = []
	; t5rl_next_1(Res, ResList0, ResList),
	    t5res_role(Res, Role),
	    t5res_vr(Res, VR),
	    RoleVrList0 = [ Role-VR | RoleVrList ],
	    a5rec_reslist_to_role_vr_pairs(ResList, RoleVrList)).


%% a5rec_pairup_roles_vr(+RoleList, +VR, -ListOfRoleVrPairs)

a5rec_pairup_roles_vr([],_,[]).
a5rec_pairup_roles_vr([R|Roles], VR, [R-VR|RoleVRs]) :-
	a5rec_pairup_roles_vr(Roles, VR, RoleVRs).


%%
%% Recollecting Broadcasted Data in Retraction Mode
%%

%% a5rec_recollect_broadcasted_msgs(+FactorizedBackPointer, +ONFsel, +ObjId,
%%                                  +AgendaEntry, -NewAgendaEntry)
%%  Collects information that previously had been broadcasted forward
%%  and was lost when the NFs was deleted. Up to now this is:
%%  i) VRs propagated to Obj0, ii) requests to add a inverse filler
%%  relationship, i.e., a tell(Obj, Trigger, fills(inv(R),[Trigger]), Dep)
%%  with Dep = dep(Obj,Trigger, cinst(Trigger,fills(R,[Obj]))).
%%  FactorizedBackPointer is a list of Backpointer where the object
%%  to which is pointed has been factorized ([ Id-RoleList | ...]).

a5rec_recollect_broadcasted_msgs([],_,_,AE,AE).
a5rec_recollect_broadcasted_msgs([F-Rs|BPs],ONFsel,ObjId,AE0,AE) :-
	a5obj_fetch(F,FObj),
	a5rec_recollect_broadcasted_msgs(Rs,F,FObj,ONFsel,ObjId,AE0,AE1),
	a5rec_recollect_broadcasted_msgs(BPs,ONFsel,ObjId,AE1,AE).

a5rec_recollect_broadcasted_msgs([],_,_,_,_,AE,AE).
a5rec_recollect_broadcasted_msgs([R|Rs],FId,FObj,ONFsel,ObjId,AE0,AE):-
	a5dep_dependency(ONFsel,ObjId,FId,cinst(FId,fills(R,[ObjId])),Dep1),
	t5role_inv_role(R,InvR),
	a5sch_modify_agenda_entry(tell(FId,[fills(InvR,[FId])],Dep1), AE0,AE1),
	a5obj_vr(ONFsel,FObj,R,C),
	a5dep_dependency(ONFsel,ObjId,FId,cinst(FId,C),Dep2),
	a5sch_modify_agenda_entry(tell(FId,[C],Dep2), AE1,AE2),
	a5rec_recollect_broadcasted_msgs(Rs,FId,FObj,ONFsel,ObjId,AE2,AE).
	

% end_of_file.


%% --------------------------------------------------
%
% Sicherstellen folgender Konsistenztests:
%
%  o  a4rec_conc_ind_chk_consistent(+UC, +MSCs, +IndexConcs, +ResList)
%     Aufruf bei a5obj_complete/5 !? -> cmk
%
%  o  a4rec_typecheck_role_fillers(+Role, +RangeType, +RoleFillers, +RRL)
%    Muss vom Conc/CNF Modul geleistet werden ! -> uk



% LAST EDIT: Sat Jan 23 20:44:52 1993 by Mirjam Kuehne (mitropa!mir) 
% LAST EDIT: Tue Nov 17 18:26:08 1992 by Mirjam Kuehne (madonna!mir) 
% LAST EDIT: Tue Aug  4 15:04:17 1992 by Mirjam Kuehne (madonna!mir) 

%%
%%  N E W   V E R S I O N
%%

%% This new version of the RET module will pass the scheduler as an argument.
%% FFS: To profit from abstractions (e.g., at a generative, query-time run)
%%  most of the '_p' preds will have to be rewritten to return a boolean
%%  result as a parameter rather then by success or failure.
%%  Since this isn't the case at the moment, no object should be changed
%%  by any of the calls of the following preds.

%% cmk, 28.7.93: Added a new data type bwchained/2 that contains those
%%  restrictions to be proven backward-chained. At the moment, these are
%%  VRs and atmost(0,R) restrictions (each passed as lists).
%%  Below, variable BwcRs denotes such a bwchained struct; it substitutes
%%  the VRs variables.

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                          ABOX-RETRIEVAL-MODULE                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              Mirjam Kuehne                             %
%                               August 1992                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% IMPORT-predicates: t5cpf_create/3.
%	             t5cls_conc/8.
%	             t5cls_obj/8.
%		     t5concid_normal_form/2.
%                    t5concid_filter/2.
%		     t5cpf_splitting/4.
%                    t5fil_holds_p/2.
%		     t5hc_direct_supers/4.
%		     t5hc_minimize_special/5.
%		     a5ind_get_all_instances/2.
%		     a5odb_nf_x/3.
%	             a5ve_make_simple_ve/3
%	             a5ve_ve_subsumedby/2.
%                    a5rec_vrs_abstractable_p/4.
%                    a5rec_vrs_abstractable_p/5.
%                    a5sch_make_scheduler/2.
%	             b5nf_nr/3.
%	             b5nf_complete/2.
%                    b5nf_subsumes_p/2.
%		     b5sort_unify/3.
%	             b5re_raw_create/3.


% EXPORT-predicates:

% a5ret_key_retrieve(+ConceptKey,+Selector,-Instances),
% a5ret_key_instantiates_p(+ConceptKey,+Selector,+ObjectKey),
% a5ret_instantiates_p(+BQ,+UnNormalizedRoleExprSeq,+Selector,+ObjectKey),
% a5ret_retrieve(+BQ,+UnNormalizedRoleExprSeq,+Selector,-Instances),
% a5ret_insert_conc(+ConceptKey,-Instances_S,-Instances_I)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a5ret_retrieve_key:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a5ret_key_retrieve(ConceptKey,Selector,Instances) :-
	t5concid_filter(ConceptKey,TrueFilter),
	t5fil_holds_p(indexing,TrueFilter),!,
	a5ind_get_all_instances(ConceptKey,Selector,Instances).

%a5ret_key_retrieve(ConceptKey,Selector,Instances) :-
%	t5hc_direct_supers(1,ConceptKey,indexing,IndConcs),
%	t5concid_normal_form(ConceptKey,CNF),
%	t5cpf_splitting(CNF,IndConcs,CPF2,VRs,Flag),
%	a5ret_bqr_apply_index(IndConcs,Selector,IndInstances),
%	a5ret_bqr_apply_restr(IndInstances,CPF2,VRs,Flag,Selector,Instances).


a5ret_key_retrieve(ConceptKey,Selector,Instances) :-
	a5ret_indexing_splitting(ConceptKey,CPF2,BwcRs,IndConcs,Flag),
	a5ret_bqr_apply_index_restr(IndConcs,Selector,CPF2,BwcRs,Flag,Instances).
	
%! mod
a5ret_indexing_splitting(ConceptKey,CPF2,BwcRs,IndConcs,Flag) :-
	BwcRs = bwchained(VRs,NullNRs), 
	t5hc_direct_supers(1,ConceptKey,indexing,IndConcs),
	t5concid_normal_form(ConceptKey,CNF),
	b5nf_splitting(CNF,IndConcs,CPF2,VRs,NullNRs,Flag).

a5ret_bqr_apply_index_restr(IndConcs,Selector,CPF2,BwcRs,Flag,Instances) :-
	a5ret_bqr_apply_index(IndConcs,Selector,IndInstances),
	a5ret_bqr_apply_restr(IndInstances,CPF2,BwcRs,Flag,Selector,Instances).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a5ret_key_instantiates_p:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% cmk

%% a5ret_key_instantiates_p(+ConceptKey,+Selector,+Object) 
%% a5ret_key_instantiate_p(+ConceptKey,+Selector,+Objects) 
%% a5ret_key_unindexed_instantiates_p(+ConceptKey,+Selector,+Object) 
%% a5ret_key_unindexed_instantiate_p(+ConceptKey,+Selector,+Objects) 
%%  True if the given Objects instantiate ConceptKey.
%% NB: All preds are also available with the Scheduler passed as an
%%  accumulator at the end of the parameter list.

%% without given Scheduler
a5ret_key_instantiates_p(ConceptKey,Selector,Object) :-
	a5sch_make_scheduler(Selector,S),
	a5ret_key_instantiate_p([Object],ConceptKey,Selector,true,S,_).
a5ret_key_instantiate_p(ConceptKey,Selector,Objects) :-
	a5sch_make_scheduler(Selector,S),
	a5ret_key_instantiate_p(Objects,ConceptKey,Selector,true,S,_).
a5ret_key_unindexed_instantiates_p(ConceptKey,Selector,Object) :-
	a5sch_make_scheduler(Selector,S),
	a5ret_key_instantiate_p([Object],ConceptKey,Selector,fail,S,_).
a5ret_key_unindexed_instantiate_p(ConceptKey,Selector,Objects) :-
	a5sch_make_scheduler(Selector,S),
	a5ret_key_instantiate_p(Objects,ConceptKey,Selector,fail,S,_).

%% with given Scheduler
a5ret_key_instantiates_p(ConceptKey,Selector,Object,S0,S) :-
	a5ret_key_instantiate_p([Object],ConceptKey,Selector,true,S0,S).
a5ret_key_instantiate_p(ConceptKey,Selector,Objects,S0,S) :-
	a5ret_key_instantiate_p(Objects,ConceptKey,Selector,true,S0,S).
a5ret_key_unindexed_instantiates_p(ConceptKey,Selector,Object,S0,S) :-
	a5ret_key_instantiate_p([Object],ConceptKey,Selector,fail,S0,S).
a5ret_key_unindexed_instantiate_p(ConceptKey,Selector,Objects,S0,S) :-
	a5ret_key_instantiate_p(Objects,ConceptKey,Selector,fail,S0,S).

%% --- the internal one:
%% NB: order of Args 1, 2 and 3 has been changed !
%% The scheduler is passed in order to be able to detect non indexed
%% instances from the blackboard in case this is called during recognition

%! mod
a5ret_key_instantiate_p([],_,_,_,S,S) :- !.
a5ret_key_instantiate_p(Objects,ConceptKey,Selector,UseIndex,S,S) :-
	t5concid_filter(ConceptKey,TrueFilter),
	t5fil_holds_p(indexing,TrueFilter),!,
	a5ret_are_instances_p(Objects,[ConceptKey],Selector,UseIndex).
a5ret_key_instantiate_p(Objects,ConceptKey,Selector,UseIndex,S0,S) :-
	t5hc_direct_supers(1,ConceptKey,indexing,IndConcs),
	t5concid_normal_form(ConceptKey,CNF),
	%% t5cpf_splitting(CNF,IndConcs,CPF2,VRs,NullNRs,Flag),
	b5nf_splitting(CNF,IndConcs,CPF2,VRs,NullNRs,Flag),
	!, %% remove when Uwe has made b5nf_splitting deterministic
	BwcRs = bwchained(VRs, NullNRs),
	a5ret_are_instances_p(Objects,IndConcs,Selector,UseIndex),
	a5ret_bqr_apply_restr_on_list_p(Objects,CPF2,BwcRs,Flag,Selector,S0,S).

%% end cmk


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a5ret_instantiates_p:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a5ret_instantiates_p(CPF,UnNormRoleExprSeq,Selector,Object) :-
	a5ret_bqr_splitting(CPF,IndConcs,BQWithoutBwcRs,BwcRs,Flag),
	a5ret_is_instance_p(IndConcs,Selector,Object),
	a5ret_build_r_expr_seq(UnNormRoleExprSeq,Selector,RoleExprSeq),
	a5ret_apply_constraints_p(Object,RoleExprSeq,Selector),
	a5ret_bqr_apply_restr_p(Object,BQWithoutBwcRs,BwcRs,Flag,Selector).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a5ret_retrieve:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a5ret_retrieve(Key,NFX,Objs) :-
	b5kif_extension(Key,NFX,Objs).       % -okp-

a5ret_retrieve(CPF,UnNormRoleExprSeq,Selector,Instances) :-
	a5ret_bqr_splitting(CPF,IndConcs,BQWithoutBwcRs,BwcRs,Flag),
	a5ret_bqr_apply_index(IndConcs,Selector,IndInstances),
	a5ret_build_r_expr_seq(UnNormRoleExprSeq,Selector,RoleExprSeq),
	a5ret_apply_constraints(IndInstances,RoleExprSeq,Selector,Instances1),
	a5ret_bqr_apply_restr(Instances1,BQWithoutBwcRs,BwcRs,Flag,Selector,Instances),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a5ret_bqr_splitting: splits Basic Query  (wich is after                %
%		       parsing a Concept Proto Form) into the list of    %
%                      direct indexing supers and the residue            %
%		       restrictions that make up the difference between  %
%		       BQ and the conjunction of all Concepts in the     %
%		       ListOfIndexingConcs.	                         %
% The Concept Proto Form Module splits the CPF into Concept Normal Form  %
% and the user-defined Supers. The CNF has to be completed, because the  %
% classifier needs the completed CNF and the user-defined Supers to      %
% compute AllSupers. 							 %
% Is the CNF already known in the classifier (Status = old), he gives    %
% also the right key of the CNF and the hierarchy cache gives 		 %
% all indexing superconcepts of the CNF. Is the CNF unknown the          %
% hierarchy cache computes the indexing superconcepts.                   %
% After this the cpf-module splits the Basic Query into Value-		 %
% Restrictions and the other restrictions.				 %
% the difference between the CPF and the list of the indexing concepts.  %
% The result is a list of restrictions (in the Basic Query contained     %
% Value Restictions etc.).    						 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! mod
%% a5ret_bqr_splitting(+BQinNF, -IndConcs, -NonBwcRLinNF, -BwcRs, -Flag)
%%  BQinNF is split into its most specific indexing supers IndConcs,
%%  its non-backward-chained restrictions NonBwcRLinNF as a NF, and 
%%  those restrictions to be tested backward chained BwcRs.
%%  The latter are those values restrictions VRs and those atmost(0,R) 
%%  restrictions which are more specific (or new) than the corresponding 
%%  restrictions in the union of the IndConcs,
%%  In NonBwcRLinNF, all VRs are generalized to the according role range,
%%  and all atmost(0,R) retrictiosn are generalized to atmost(in,R).
%%  Thus, NonBwcRLinNF is not necessarilly subsumed by union(IndConcs).
%%  Flag is 'no' iff NonBwcRLinNF subsumes union(IndConcs) (has no more
%%  specific restrictions); Flag is 'yes' if NonBwcRLinNF (and therefore
%%  BQinNF) has an non-vr restriction not expressed by union(IndConcs).

a5ret_bqr_splitting(CPF,IndConcs,CPF2,bwchained(VRs,NullNRs),Flag) :-
        b5nf_supers(CPF,AllSupers,Key,Status),
	(Status == new ->
	    t5hc_minimize_special(1,AllSupers,indexing,IndConcs)
	;(t5concid_filter_holds_p(Key,indexing) ->
	    IndConcs = [Key]
	;   t5tbox_hc(conc,HC),
	    t5hc_direct_supers(HC,Key,indexing,IndConcs))),
	b5nf_splitting(CPF,IndConcs,CPF2,VRs,NullNRs,Flag),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a5ret_bqr_apply_index: true if IndInstances is the intersection of the %
% 	                 set of instances of the single indexing concs   %
%			 in ListOfIndexingConcs ([Conc1|Tail]).		 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% mod cmk

a5ret_are_instances_p([],_,_,_).
a5ret_are_instances_p([O|Os],Cs,NFsel,UseIndex) :-
	(UseIndex == true ->
	    a5ret_is_instance_p(Cs,NFsel,O)
	;   a5ret_is_unindexed_instance_p(Cs,NFsel,O)),
	a5ret_are_instances_p(Os,Cs,NFsel,UseIndex),
	!.


a5ret_is_instance_p([],_,_).
a5ret_is_instance_p([Conc1|Tail],NFsel,Object) :-
	(NFsel == nfs ->
	    a5ind_get_s(Conc1,Object)
	; %% NFsel == nfi
	    a5ind_get_i(Conc1,Object)),
	!, %% remove choice point from IND modul preds
	a5ret_is_instance_p(Tail,NFsel,Object).	

%% a5ret_is_unindexed_instance_p(+Concs,+NFsel,+ObjId)
%%  True if ObjId is an instance of all C in Concs. 
%%  The test should NOT exploit the indexing structure.

a5ret_is_unindexed_instance_p(Cs,NFsel,ObjId) :-
	a5odb_nf_x(NFsel,ObjId,NF),	
	b5nf_instantiates_keys_p(NF,Cs).


%% end cmk 

%%%%%%%%%%%%%%%%%%%

a5ret_bqr_apply_index([],_,[]) :- !.

a5ret_bqr_apply_index([Conc1|Tail],Selector,IndInstances) :-
	a5ind_get_all_instances(Conc1,Selector,Objects1),
	a5ret_intersection(Tail,Objects1,Selector,IndInstances),
	!.

a5ret_intersection([],List,_,List) :- !.

a5ret_intersection([Conc2|Tail2],Objects1,Selector,IndInstances) :-
	a5ind_get_all_instances(Conc2,Selector,Objects2),
	b5sort_intersect(Objects1,Objects2,Intersection),!,
	a5ret_intersection(Tail2,Intersection,Selector,IndInstances).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a5ret_build_r_expr_seq:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a5ret_build_r_expr_seq([],_,[]) :- !. %% FFS: remove cut !
a5ret_build_r_expr_seq([R:UnNormVe|UnNormT],Selector,[R:Ve|T]) :-
	a5ve_ve(UnNormVe,Selector,Ve),
	a5ret_build_r_expr_seq(UnNormT,Selector,T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a5ret_apply_constraints: true if Instances1 is the subset of           %
% 			   IndInstances containing those elements of     %
%			   IndInstances that instantiate RoleExprSeq.    %
% An Object instantiates a RoleExprSeq if it instantiates every single   %
% RoleExpression. An Object instantiates a single RoleExpression if the  %
% ValueExpression of the RoleExpr subsumes the ValueExpression of the    %
% object for the give role.                                              %
%									 %
% a5ret_apply_constraints_p ist f"ur den Fall, da"s a5ret_retrieve schon %
% mit einem Objekt aufgerufen wurde und nur noch getestet wird, ob es    %
% die Bedingungen erf"ullt.					         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a5ret_apply_constraints_p(_,[],_) :- !.

a5ret_apply_constraints_p(Object,RoleExprSeq,ONFsel) :-
	a5odb_nf_x(ONFsel,Object,ONF1),
	a5ret_instantiated_by_p(RoleExprSeq,ONF1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

a5ret_apply_constraints(List,[],_,List) :- !.

a5ret_apply_constraints([],_,_,[]) :- !.

a5ret_apply_constraints(IndInstances,RoleExprSeq,ONFsel,Instances1) :-
	a5ret_apply_constraints(IndInstances,RoleExprSeq,ONFsel,[],
				Instances1),!.


a5ret_apply_constraints([],_,_,List,List) :- !.

a5ret_apply_constraints([IndInst1|Tail],RoleExprSeq,ONFsel,List,Instances1) :-
	a5odb_nf_x(ONFsel,IndInst1,ONF1),
	(a5ret_instantiated_by_p(RoleExprSeq,ONF1) ->
	 b5sort_unify(List,[IndInst1],StoreList),
	 a5ret_apply_constraints(Tail,RoleExprSeq,ONFsel,StoreList,Instances1);
	a5ret_apply_constraints(Tail,RoleExprSeq,ONFsel,List,Instances1)).
	
a5ret_instantiated_by_p([],_) :- !.

a5ret_instantiated_by_p([RoleExpr|Sequence],ONF1) :-
	b5rex_raw_create(Role,VExpr,RoleExpr),
	a5ve_make_simple_ve(ONF1,Role,VExpr1),
	a5ve_ve_subsumedby(VExpr1,VExpr),!,
	a5ret_instantiated_by_p(Sequence,ONF1).
	
	
%! mod
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% a5ret_bqr_apply_restr(+Instances1,+BQWithoutBwcRs,+BwChainedRestricitons,
%%			+Flag,+Selector,-Instances)
% a5ret_bqr_apply_restr: true if Instances is the subset of Instances1   %
%			 containing those elements of Instances1 that    %
%			 instantiate the part of restrictions of BQ      %
%			 without ValueRestrictions plus the Value-       %
%			 Restrictions itself.				 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ein Flag gibt an, ob beim splitten der Anfrage in indizierende Konzep- %
% te, Restrestriktionen und Valuerestriktionen ueberhaupt eine Differenz %
% zwischen den indizierenden Konzepten und den Restrestriktionen entstan-%
% den ist. Es kann sein, da"s die indizierenden Konzepte schon die ganze %
% Anfrage abdecken und keine weiteren Restriktionen existieren. Es gibt  %
% also vier M"oglichkeiten:						 %
% 1. Das Flagt steht auf "no" und die Liste der VRs ist leer:            %
%    Die Liste der Instanzen1 ist gleich der Liste aller Instanzen.      %
% 2. Das Flag steht auf "no", aber es gibt VRs:				 %
%    Es mu"s getestet werden, ob Instanzen1 die VRs erf"ullen.           %
% 3. Das Flag steht auf "yes" und die VRs sind leer:			 %
%    Es mu"s getestet werden, ob Instanzen1 von den Restrestriktionen    %
%    subsumiert werden.							 %
% 4. Das Flag steht auf "yes" und es gibt VRs:				 %
%    der zweite UND dritte Fall mu"s getestet werden.                    %
%									 %
% a5ret_apply_restr_p ist f"ur den Fall, da"s a5ret_retrieve schon mit   %
% einem Objekt aufgerufen wurde und nur noch getestet wird, ob es die    %
% Restriktionen erf"ullt.						 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% mod cmk

a5ret_bqr_apply_restr_on_list_p([], _,_,_,_,S,S).
a5ret_bqr_apply_restr_on_list_p([O|Os], CNF, BwcRs, Flag, NFsel,S0,S) :-
	a5ret_bqr_apply_restr_p(O, CNF, BwcRs, Flag, NFsel,S0,S1),
	a5ret_bqr_apply_restr_on_list_p(Os, CNF, BwcRs, Flag, NFsel,S1,S).

%% to map foreign calls to the new version that passes the Scheduler.
%% remove later
a5ret_bqr_apply_restr_p(O,CNF,BwcRs,Flag,NFsel) :-
	a5sch_make_scheduler(NFsel,S),
	a5ret_bqr_apply_restr_p(O,CNF,BwcRs,Flag,NFsel,S,_).


%! mod
a5ret_bqr_apply_restr_p(_,_,bwchained([],[]),no,_,S,S) :- !. %%  -okp- 26.7.
a5ret_bqr_apply_restr_p(Object,_,BwcRs,no,ONFsel,S0,S) :-
	!,
	a5ret_apply_bwcr_p(Object,BwcRs,ONFsel,S0,S).
a5ret_bqr_apply_restr_p(Object,RestRestrs,bwchained([],[]),yes,ONFsel,S,S) :-
	!,
	a5ret_apply_restr_p(Object,RestRestrs,ONFsel).
a5ret_bqr_apply_restr_p(Object,RestRestrs,BwcRs,yes,ONFsel,S0,S) :-
	a5ret_apply_restr_bwcr_p(Object,RestRestrs,BwcRs,ONFsel,S0,S).

%% end cmk

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a5ret_bqr_apply_restr([],_,_,_,_,[]) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1. Fall								 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a5ret_bqr_apply_restr(List,_,[],no,_,List) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2. Fall								 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a5ret_bqr_apply_restr(Instances1,_,BwcRs,no,ONFsel,Instances) :-
	!,
	a5ret_apply_bwcr(Instances1,BwcRs,ONFsel,[],Instances).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3. Fall								 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a5ret_bqr_apply_restr(Instances1,RestRestrs,[],yes,ONFsel,Instances) :-
	!,
	a5ret_apply_restr(Instances1,RestRestrs,ONFsel,[],Instances).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 4. Fall								 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a5ret_bqr_apply_restr(Instances1,RestRestrs,BwcRs,yes,ONFsel,Instances) :-
	a5ret_apply_restr_bwcr(Instances1,RestRestrs,BwcRs,ONFsel,[],Instances).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a5ret_apply_bwcr							 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! mod
%% mod cmk
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% wenn es nur ein Objekt gibt,
% das gleich beim Retrieve mit angegeben wurde:

a5ret_apply_bwcr_p(ObjID,bwchained(VRs,NRs),ONFsel,S0,S) :-
	a5odb_nf_x(ONFsel,ObjID,ONF0),	
	a5rec_atmost_0_nrs_abstractable_p(NRs, ObjID,ONF0,ONF1, S0,S1),
	a5rec_vrs_abstractable_p(VRs,ObjID,ONF1,S1,S).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

a5ret_apply_bwcr([],_,_,List,List).
a5ret_apply_bwcr([IndInst1|Tail],BwcRs,ONFsel,List,Instances) :-
	BwcRs = bwchained(VRs,NRs),
	a5odb_nf_x(ONFsel,IndInst1,ONF0),	
	((a5rec_atmost_0_nrs_abstractable_p(NRs,IndInst1,ONFsel, ONF0,ONF1),
	  a5rec_vrs_abstractable_p(VRs,IndInst1,ONF1,ONFsel)) ->  %% mf
	    b5sort_unify(List,[IndInst1],StoreList),
	    a5ret_apply_bwcr(Tail,BwcRs,ONFsel,StoreList,Instances)
	;   a5ret_apply_bwcr(Tail,BwcRs,ONFsel,List,Instances)).

%% end cmk


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a5ret_apply_restr							 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% wenn es nur ein Objekt gibt,
% das gleich beim Retrieve mit angegeben wurde:

a5ret_apply_restr_p(Object,RestRestrs,ONFsel) :-
	a5odb_nf_x(ONFsel,Object,ONF1),	
	b5nf_subsumes_p(RestRestrs,ONF1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

a5ret_apply_restr([],_,_,List,List).
a5ret_apply_restr([IndInst1|Tail],RestRestrs,ONFsel,List,Instances) :-
	a5odb_nf_x(ONFsel,IndInst1,ONF1),	
	(b5nf_subsumes_p(RestRestrs,ONF1) ->
	    b5sort_unify(List,[IndInst1],StoreList),
	    a5ret_apply_restr(Tail,RestRestrs,ONFsel,StoreList,Instances)
	;   a5ret_apply_restr(Tail,RestRestrs,ONFsel,List,Instances)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a5ret_apply_restr_bwcr							 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% mod cmk
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% wenn es nur ein Objekt gibt,
% das gleich beim Retrieve mit angegeben wurde:

a5ret_apply_restr_bwcr_p(ObjID,RestRestrs,BwcRs,ONFsel,S0,S) :-
	BwcRs = bwchained(VRs, NRs),
	a5odb_nf_x(ONFsel,ObjID,ONF0),	
	b5nf_subsumes_p(RestRestrs,ONF0),
	a5rec_atmost_0_nrs_abstractable_p(NRs, ObjID,ONF0,ONF1, S0,S1),
	a5rec_vrs_abstractable_p(VRs,ObjID,ONF1,S1,S).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% end cmk

a5ret_apply_restr_bwcr([],_,_,_,List,List).
a5ret_apply_restr_bwcr([IndInst1|Tail],RestRestrs,BwcRs,ONFsel,List,Instances) :-
	BwcRs = bwchained(VRs, NRs),
	a5odb_nf_x(ONFsel,IndInst1,ONF0),	
	((b5nf_subsumes_p(RestRestrs,ONF0),
	 a5rec_atmost_0_nrs_abstractable_p(NRs,IndInst1,ONFsel, ONF0,ONF1),
	 a5rec_vrs_abstractable_p(VRs,IndInst1,ONF1,ONFsel)) ->
	      b5sort_unify(List,[IndInst1],StoreList),
	      a5ret_apply_restr_bwcr(Tail,RestRestrs,BwcRs,ONFsel,
	                           StoreList,Instances)
	;     a5ret_apply_restr_bwcr(Tail,RestRestrs,BwcRs,ONFsel,List,Instances)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a5ret_insert_conc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Falls die IBox leer ist, sind die IBox Instanzen eine neuen Konzeptes
%% gleich den TBox Instanzen.

a5ret_insert_conc(ConceptKey,Instances_S,Instances_I) :-
	a5ret_indexing_splitting(ConceptKey,CPF2,BwcRs,IndConcs,Flag),
	a5ret_bqr_apply_index_restr(IndConcs,nfs,CPF2,BwcRs,Flag,Instances_S),
	t5out_info(indexing_instances_computed(conc(ConceptKey),nfs)),
	(b5sta_check_flag(iboxfilled,true),  % -okp-
	 !,
	 a5ret_bqr_apply_index_restr(IndConcs,nfi,CPF2,BwcRs,Flag,Instances_I),
	 t5out_info(indexing_instances_computed(conc(ConceptKey),nfi));
	    Instances_I = Instances_S).


%% -----------------------------------------------------------------------
%%   A5REV.PL
%%    
%%   ABox Revision
%%   ABox Consequences of TBox/IBox Revision
%% -----------------------------------------------------------------------

%%  EXPORT
%%     a5rev_redescribe_object(+ObjId, +Term)
%%            redescribe ObjId totally new by Term
%%     a5rev_forget_description(+ObjId, +RetrDesc)
%%            forget part RetrDesc of the description of ObjId;
%%            RetrDesc must have been told explicitly
%%     a5rev_forget_object(+ObjId)
%%            if ObjId is not introduced by the TBox or IBox, 
%%            remove it totally, otherwise it is revised to anything 
%%     a5rev_update_tbox(+RevisedClasses, +ObsoleteILinks, +NewILinks)
%%            Adapt the ABox to the change of the definition of some
%%            already existing role or concept; if any ilinks are concerned
%%            apply IBox changes as well (ILInks are given in form of 
%%            their LHSs
%%     a5rev_new_class(+Name, +Definition)
%%            Adapt the ABox to the insertion of a new concept or role
%%     a5rev_update_ibox(+Action, +LHS)
%%            Update the ABox to the removal or insertion of an ilink;
%%            Action must be one of {new,forget}

%%  IMPORT
%%     library(lists):  is_list/1, delete/3
%%     library(basics): member/2
%%     builtin:         sort/2, setof/3
%%
%%     b5par:    b5par_at_parse/3, b5par_update_entry/2, b5par_process/3, 
%%               b5par_object_description/3, b5par_aa_parse/6
%%               b5par_forward_introduction/2, b5par_keyterm_to_protoform/3
%%     b5sort:   b5sort_difference/3, b5sort_unify/3
%%     b5sta:    b5sta_check_flag/2, b5sta_set_flag/2
%%     b5desc:   b5desc_term_to_list/2, b5desc_list_to_term/2
%%     b5st:     b5st_object_entry/4, b5st_class_entry/4
%%     t5dm:     t5dm_obj_uses_delete/1, t5dm_used_by_objs/2
%%     t5concid: t5concid_direct_supers/3
%%     t5tbox:   t5tbox_anything_key/1
%%     a5sch:    a5sch_make_scheduler/2, a5sch_process_agenda/1, 
%%               a5sch_add_to_agenda/3, 
%%               a5sch_filled_objects_before_redescription/3
%%     a5ret:    a5ret_insert_conc/3, a5ret_retrieve/4, 
%%               a5ret_bqr_apply_index/3
%%     a5ind:    a5ind_get_all_instances/3
%%     a5objid:  a5objid_explicit_supers/3


%% -----------------------------------------------------------------------
%%    ABox Revision
%% -----------------------------------------------------------------------

a5rev_redescribe_object(ObjId, Term) :-
	t5out_trace(a5rev_redescribe_object(ObjId, Term)),
	a5jt_get_object_entry_chk(Name, Type, ObjId, OldDef),
	( b5par_at_parse(Name,Term,FI) ->
	      b5par_update_entry(object,(Name, Type, ObjId, ([],[])))
	; b5par_update_entry(object,(Name, Type, ObjId, OldDef)),
          !, fail ),	    
       	t5dm_obj_uses_delete(ObjId),
       	a5sch_make_scheduler(retraction,Scheduler0),
	b5par_process(FI,Scheduler0,Scheduler),
	( a5sch_process_agenda(Scheduler)
	; b5par_update_entry(object,(Name, Type, ObjId, OldDef)),
	  t5out_info(stored_old_userdef(Name)),
	  !, fail),
	!.

a5rev_forget_description(ObjId, RetrDesc) :-
	t5out_trace(a5rev_forget_description(ObjId, RetrDesc)),
        a5jt_get_object_entry_chk(Obj, (c,0), ObjId, (OldDef, _)),
	b5desc_term_to_list(RetrDesc, RetrList),
	sort(RetrList, SRetrList),
	b5sort_difference(OldDef, SRetrList, Residue),
	( OldDef = Residue ->
	      t5out_error(not_told(Obj, RetrDesc)),
	      !, fail
	; a5rev_forget_obj_description(Residue, ObjId)).

a5rev_forget_obj_description([], ObjId) :-
	!, a5rev_redescribe_object(ObjId, anything).
a5rev_forget_obj_description(ReducedDescList, ObjId) :-
	b5desc_list_to_term(ReducedDescList, NewTell),
	a5rev_redescribe_object(ObjId, NewTell). 

a5rev_forget_object(ObjId) :-
	t5out_trace(a5rev_forget_object(ObjId)),
           b5sta_check_flag(verbosity,Value),
	   b5sta_set_flag(verbosity,error),    %% tmp switch off verbosity
	   a5jt_get_object_entry_chk(Obj,(c,0),ObjId,_),
	   b5par_at_parse(Obj,anything,FI),  
	   b5sta_set_flag(verbosity,Value),
	a5sch_make_scheduler(retraction, Scheduler0),
	b5par_process(FI, Scheduler0, Scheduler1),
	a5sch_process_agenda(Scheduler1, Scheduler2),
	a5sch_filled_objects_before_redescription(ObjId,Scheduler2,InvFils),
	a5rev_update_inv_rfs(InvFils,Obj),
	t5dm_used_by(object/Obj,Classes),
	( Classes = [] ->
	      b5st_obj_delete_entry(Obj),
	      t5dm_obj_uses_delete(ObjId), !
	; b5par_update_entry(object,(Obj,(c,0),ObjId,([],[]))),
	  t5out_warning(object_not_totally_removed(Obj))).


%% a5rev_update_inv_rfs(+InvRFs, +Obj)
a5rev_update_inv_rfs([], _).
a5rev_update_inv_rfs([InvRFKey | InvRFs], Obj) :-
        a5jt_get_object_entry_chk(InvRF,_,InvRFKey, OldDef),
        a5rev_delete_rinsts(InvRF, OldDef, Obj, (NewDef,_)),
        ( NewDef = []
	        ->  NewTerm = anything
	; b5desc_list_to_term(NewDef, NewTerm) ),
        a5rev_redescribe_object(InvRFKey,NewTerm),
	a5rev_update_inv_rfs(InvRFs, Obj).

%% a5rev_delete_rinsts(+InvRF, +OldDef, Obj, NewDef).
a5rev_delete_rinsts(InvRF, OldDef, Obj, NewDef) :-
	b5par_update_description(del, InvRF, OldDef, [_:Obj], TmpNewDef),
	OldDef \== TmpNewDef,
        a5rev_delete_rinsts(InvRF, TmpNewDef, Obj, NewDef), !.
a5rev_delete_rinsts(_, Def, _, Def).



%% -----------------------------------------------------------------------
%%    ABox Consequences of TBox Revision
%% -----------------------------------------------------------------------

a5rev_update_tbox(RevisedClasses, ObsILinks, NewILinks) :-
	t5out_trace(a5rev_update_tbox(RevisedClasses, ObsILinks, NewILinks)),
	b5sta_set_flag(tarevision, true),
	a5rev_tbox_revision(RevisedClasses, [], JustProcessed),
	a5rev_update_ilinks(JustProcessed, ObsILinks, NewILinks),
	b5sta_set_flag(tarevision, false).

a5rev_tbox_revision([], Objs, Objs).
a5rev_tbox_revision([(Type,Name,OldKey,NewKey)|Rest], ObjsIn, ObjsOut) :-
	a5rev_tbox_revision(Type, Name, OldKey, NewKey, Processed),
	b5sort_unify(ObjsIn, Processed, TmpAccu),
	!, a5rev_tbox_revision(Rest, TmpAccu, ObjsOut).
	
%% a5rev_tbox_revision(#1 +Type, #2 +Name, #3 +OldKey, #4 +NewKey)
%%  Type is one of {conc, role}
%%  FFS: new,old,prim status ausnutzen in class 3!
a5rev_tbox_revision(_, _, Key, Key, _) :- !.
a5rev_tbox_revision(Type, Name, _, NewKey, Processed) :-
	t5out_trace(a5rev_tbox_revision(Type, Name, NewKey,_,_)),
	t5dm_used_by_objs(class/Name,Objs1),
        ( \+ Objs1 = [] ->
	      sort(Objs1, ClassA)
	; ClassA = []),
       	a5rev_update_class_1(Name, ClassA),
        a5rev_update_class_3(Type, Name, NewKey, ClassA, ClassC),
	( Type = role ->
	      b5sort_unify(ClassA, ClassC, Processed)
	; Processed = ClassA).



%% ----------------------------------------------------------------------
%%  Management of Class A Objects
%% ----------------------------------------------------------------------

%% a5rev_update_class_1(+ConcName, +Objs)		
a5rev_update_class_1(_, []) :- !.
a5rev_update_class_1(Name, Objs) :- 
	t5out_info(updating_objects(Objs)),
	a5rev_objs_upward(Objs, Name),
        a5rev_objs_downward(Objs).

%% a5rev_objs_upward(+Objs, +ConcName)
a5rev_objs_upward([], _).
a5rev_objs_upward([Obj |Objs], Name) :-
	a5jt_get_object_entry_chk(ObjName,(c,0),Obj, UserDef),
	b5par_update_description(up, ObjName, UserDef, Name, (ReducedDef,DelParts)),
        %% t5dm_uses richtig verwalten!!
	b5par_update_entry(object,(ObjName,(c,0),Obj,(ReducedDef,DelParts))),
	a5rev_obj_upward(Obj, ReducedDef),
        a5rev_objs_upward(Objs, Name).

%%  a5rev_obj_upward(+ObjID, +NewDefinition)
a5rev_obj_upward(ObjId, NewDef) :- 
	t5out_info(start_upward(ObjId)),
	( b5desc_list_to_term(NewDef, Redescription) ->
	      true
        ; Redescription = anything),
	a5rev_temporal_redescription(ObjId, Redescription),
	t5out_info(end_upward(ObjId)).

%%  a5rev_temporal_redescription(+ObjID, +Description)
a5rev_temporal_redescription(ObjId, Term) :-
	t5out_trace(a5rev_temporal_redescription(ObjId, Term)),
	b5par_object_description(O, (c,0), ObjId),
	b5par_at_parse(O,Term,FI),
       	a5sch_make_scheduler(retraction,Scheduler0),
	b5par_process(FI,Scheduler0,Scheduler),
	!, a5sch_process_agenda(Scheduler).
a5rev_temporal_redescription(ObjId, Term) :-
	t5out_panic(a5rev_temporal_redescription(ObjId, Term)), !,
	fail.

	
%% a5rev_objs_downward(+Objs).
a5rev_objs_downward([]).
a5rev_objs_downward([Obj|Objs]) :- 
	a5rev_obj_downward(Obj),
	!, a5rev_objs_downward(Objs).

%% a5rev_obj_downward(+ObjID)
a5rev_obj_downward(ObjId) :-
	t5out_info(a5rev_obj_downward(ObjId)),
       	a5jt_get_object_entry_chk(ObjName, _, ObjId, (ReducedUserDef,Undesc)),
	b5desc_list_to_term(Undesc, Unterm), 
	t5out_info(start_downward(ObjName)),
	!,  
        (  %% Case 1: downward migration succeeds
	   b5par_abox_tell(ObjName, Unterm),  %% 1st version
	   t5out_info(downward_succeeded(ObjName)),
           %% restore old userdef
	   b5par_update_description(down, ObjName, (ReducedUserDef,Undesc), _, NewDef),
           b5par_update_entry(object,(ObjName,(c,0),ObjId,NewDef)), !
           %% FFS : retell wie simples backtell? spezieller tbox-revise-mode?
        ;  %% Case 2: downward migration failed
	   ( b5sta_check_flag(tboxrevision, succeed) ->
		 t5out_warning(downward_failed(ObjName, Undesc))
	   ; fail )).


%% -----------  Class B Objects -----------------------------------------

%% a5rev_update_class_2 
%%  actually, nothing to do

%% ----------------------------------------------------------------------
%%  Management of Class C Objects
%% ----------------------------------------------------------------------


%% a5rev_new_class(+ClassName, +Definition)
a5rev_new_class(Name, Definition) :-
	t5out_trace(a5rev_new_class(Name, Definition)),
	a5jt_get_class_entry_chk(Name,Type,Key,Name := Definition),
	(  Type = (r,0) ->
	       t5out_trace(a5rev_update_roles(new,Definition,[],_)),
	       a5rev_update_roles(new, Definition, [], _)
	;  Type = (c,0) ->
	       t5out_trace(a5rev_update_conc_instances(Key,[],_)),
	       a5rev_update_conc_instances(Key, [], _)
	;  true ).


%% a5rev_update_class_3(+Type, +Name, +NewKey, +ClassA, -ClassC).
%%   Type is one of {conc,role}; ClassA is a list of object id's 
%%   which are already completely processed 
a5rev_update_class_3(Type, Name, NewKey, ClassA, ClassC) :-
	a5jt_get_class_entry(Name,_,NewKey,_:=Def),
	a5rev_update_class_3a(Type, NewKey, Def, ClassA, ClassC), !.
a5rev_update_class_3(_, _, _, _, []).
        %% implicit: Name is primitive

%% a5rev_update_class_3a(+Type,+NewKey,+Definition,+ClassAObjs,-ClassCObjs)
a5rev_update_class_3a(role, _NewKey, Def, ClassA, ClassC) :-
        !, a5rev_update_roles(old, Def, ClassA, ClassC).
a5rev_update_class_3a(conc, NewKey, _Def, ClassA, ClassC) :-
	!, a5rev_update_conc_instances(NewKey, ClassA, ClassC).
a5rev_update_class_3a(_, _, _, _, _).
        %% implicit: other types (aset,string,number). 
        %% FFS: does it suffice do to nothing?
       


%% --------- Concepts ----------------------------------------------------


%% a5rev_update_conc_instances(+ConceptKey, +ObjIDs, -MoreObjIDs).
a5rev_update_conc_instances(ConcKey, Objs1, Objs3) :-
	t5out_trace(a5rev_update_conc_instances(ConcKey, Objs1, Objs3)),
	a5ret_indexing_splitting(ConcKey,CPF2,VRs,IndConcs,_),
	a5ret_bqr_apply_index_restr(IndConcs,nfi,CPF2,VRs,no,Objs2),
	b5sort_difference(Objs2,Objs1,Objs3),
	t5out_info(recomplete(ConcKey, Objs3)),
	a5rev_scheduler_request(complete_locally_NFu,monotonic,Objs3).


%% ---------- Roles -----------------------------------------------------

%% a5rev_update_roles(+NewOrOld, +Definition, +ObjsIn, -ObjsOut)
%%  Updates role filler information after role revision
%%  ObjsIn is a list of the objects which have been already updated
%%  RoleID is the revised role
%%  if Definition is inv(Role) nothing to do
a5rev_update_roles(_, inv(_), _, _) :- !.
a5rev_update_roles(NewOrOld, RoleDef, UpdatedObjs, Triggers) :-
        a5rev_collect_triggers(RoleDef, UpdatedObjs, AnchorName, Triggers),
	t5out_trace(a5rev_trigger_role_completion(NewOrOld, AnchorName, Triggers)),
	a5rev_trigger_role_completion(Triggers, NewOrOld, AnchorName).

a5rev_collect_triggers(RDef, UpdatedObjs, Role, TriggerObjs) :-
	a5rev_get_role(RDef, Role),
        a5rev_getall_atleast_one_role(Role, nfi, Objs),
	b5sort_difference(Objs, UpdatedObjs, TriggerObjs).

a5rev_trigger_role_completion([],_,_) :- !.
a5rev_trigger_role_completion(Objs, new, _) :- 
	a5rev_scheduler_request(complete_locally_NFu, retraction, Objs),
	!.
a5rev_trigger_role_completion([Obj|Objs], old, RName) :- 
	a5jt_get_object_entry_chk(ObjName,(c,0),Obj,(Def1,_)), !, 
	( selectchk(RName:Filler, Def1, Def2) ->
	      b5par_update_entry(object,
                    (ObjName,(c,0),Obj,(Def2,[RName:Filler]))),    
	      a5rev_obj_upward(Obj, Def2),
	      a5rev_obj_downward(Obj)
	; b5par_update_entry(object,(ObjName,(c,0),Obj,([],Def1))),
	  a5rev_obj_upward(Obj, []),        %% implicit: up to anything
	  a5rev_obj_downward(Obj)),
	a5rev_trigger_role_completion(Objs, old, RName).
        

%% a5rev_getall_atleast_one_role(+RoleName, +NFx, -Objs).
a5rev_getall_atleast_one_role(Role, nfi, Objs) :-
	b5par_aa_parse(atleast(1,Role),(c,0),[],FI,BQ,RQ),
	b5par_forward_introduction(FI,old),
	b5par_keyterm_to_protoform(BQ,(c,0),CPF),
	a5ret_retrieve(CPF,RQ,nfi,Objs).


%% ----------------------------------------------------------------------
%%    Management of IBox Changes
%% ----------------------------------------------------------------------

%% forget/tell  ILink
a5rev_update_ibox(_,_) :-
	b5sta_check_flag(aboxfilled, Value),
	Value \== abox, !.
a5rev_update_ibox(Action, LHS) :-
	t5out_trace(a5rev_update_ibox(Action, LHS)),
        a5ret_insert_conc(LHS,InstsT,InstsI),
	b5sort_unify(InstsT, InstsI, AllInsts),
	AllInsts \== [],
        !, a5rev_update_nfis(Action, LHS, AllInsts).
a5rev_update_ibox(_, _).


a5rev_update_ilinks(ObjsIn, ObsILinks, NewILinks) :-
	t5out_trace(a5rev_update_ilinks(ObjsIn, ObsILinks, NewILinks)),
	a5rev_update_ilinks1(retraction, ObjsIn, ObsILinks, Processed),
	b5sort_unify(ObjsIn, Processed, AllProcessed),
	a5rev_update_ilinks1(monotonic, AllProcessed, NewILinks, _).

a5rev_update_ilinks1(_, Objs, [], Objs) :- !.
a5rev_update_ilinks1(Mode, ObjsIn, LHSs, Residue) :-
	a5rev_collect_nfi_objects(ObjsIn, LHSs, CollObjs),
	b5sort_difference(CollObjs, ObjsIn, Residue),
	a5rev_update_nfis(Mode, LHSs, Residue).

a5rev_collect_nfi_objects(Objs, [], Objs) :- !.
a5rev_collect_nfi_objects(ObjsIn, [LHS|LHSs], ObjsOut) :-
	a5ind_get_all_instances(LHS, nfi, Insts),
	b5sort_unify(ObjsIn, Insts, TmpObjs),
	!, a5rev_collect_nfi_objects(TmpObjs, LHSs, ObjsOut).
	      
a5rev_update_nfis(Action, LHS, Objs) :-
	t5out_trace(a5rev_update_nfis(Action, LHS, Objs)),
	( (Action = new ; Action = monotonic) ->
	       SchMode = monotonic
	;      SchMode = retraction ),
	a5rev_update_nfis1(SchMode, LHS, Objs).

%% a5rev_update_nfis1(Mode, LHS, Objs)
%%  Due to optimization reasons (no classification) the IBox expects
%%  a fully completed object normal form, i.e. it applies ilinks to 
%%  LHS only if it is explicitly mentioned in the object normal form;
%%  if so, the object can be sent to the scheduler with the 
%%  postpone_until_NFi request, otherwise it has to be ABox-classified
%%  first (which ensures Ilink application automatically) 	      
a5rev_update_nfis1(_, _, []) :- !. 
a5rev_update_nfis1(monotonic, LHS, Objs) :-
	t5out_info(updating_nfi_of(Objs)),
	a5rev_find_postponable_objects(LHS, Objs, PostObjs),
	b5sort_difference(Objs, PostObjs, ClassObjs),
	a5rev_scheduler_request(classify, monotonic, ClassObjs), 
	a5rev_scheduler_request(postpone_until_NFi, monotonic, PostObjs). 
a5rev_update_nfis1(retraction, _, Objs) :-
	%% implicit: Mode = forget or retraction
	t5out_info(updating_nfi_of(Objs)),
	a5rev_scheduler_request(postpone_until_NFi, retraction, Objs).

%% a5rev_find_postponable_objects(+ConcId,+Objs,-PostObjs)
a5rev_find_postponable_objects(_, [], []) :- !.
a5rev_find_postponable_objects(L, _, []) :-
	is_list(L), !.
a5rev_find_postponable_objects(LHS, [Obj1|Objs1], [Obj1|Objs2]) :-
	a5objid_explicit_supers(Obj1,nfi,Supers),
	member(LHS, Supers),!, 
	a5rev_find_postponable_objects(LHS, Objs1, Objs2), !.
a5rev_find_postponable_objects(LHS, [_|Objs1], Objs2) :-
	a5rev_find_postponable_objects(LHS, Objs1, Objs2).


%% ----------------------------------------------------------------------
%%   Subtasks
%% ----------------------------------------------------------------------
  

%% a5rev_scheduler_request(+Job, +SchMode, +Objs)
a5rev_scheduler_request(_,_,[]) :- !.
a5rev_scheduler_request(Job, Mode, Objs) :-
	t5out_trace(a5rev_scheduler_request(Job, Mode, Objs)),
	a5rev_prepare_agenda_entries(Job, Objs, ScheduledObjs),
	a5rev_init_scheduler(Mode, Sch1),
	a5sch_add_to_agenda(ScheduledObjs,Sch1,Sch2),
	a5sch_process_agenda(Sch2), !.
a5rev_scheduler_request(Job, Mode, Objs) :-
	t5out_warning(a5rev_sch_req_exception_handling(Job, Mode, Objs)),
	a5rev_sch_req_exception_handling(Job, Mode, Objs), !.
a5rev_scheduler_request(Job, Mode, Objs) :-
	t5out_error(a5rev_scheduler_request(Job, Mode, Objs)),
	!, fail.


%% Exception handler: if the collected mode fails, tries to pass 
%%  the objects to the scheduler one after another.
a5rev_sch_req_exception_handling(_, _, []) :- !.
a5rev_sch_req_exception_handling(Job, Mode, [Obj|Objs]) :-
	a5rev_prepare_agenda_entries(Job, Obj, ScheduledObj),
	a5rev_init_scheduler(Mode, Sch1),
	a5sch_add_to_agenda(ScheduledObj,Sch1,Sch2),
	a5sch_process_agenda(Sch2), 
	!, a5rev_sch_req_exception_handling(Job, Mode, Objs).

	
%% a5rev_prepare_agenda_entries(+Job, +Objs, -AgendaObjs)
%%  502: new (jt)
a5rev_prepare_agenda_entries(_,[],[]) :- !.
a5rev_prepare_agenda_entries(Job, [Obj|Objs], [AgEntry| ClassObjs]) :-
	AgEntry =.. [Job, Obj],
	!, a5rev_prepare_agenda_entries(Job, Objs, ClassObjs).

%% a5rev_init_scheduler(+Mode, -Scheduler).
%%  510: new (jt)
a5rev_init_scheduler(monotonic, Scheduler) :-
	!, a5sch_make_scheduler(Scheduler).
a5rev_init_scheduler(retraction, Scheduler) :-
        a5sch_make_scheduler(retraction, Scheduler).



%% ----------------------------------------------------------------------
%%   User descriptions
%% ----------------------------------------------------------------------

%% a5rev_update_description(+Op, +Obj, +OldDef, +NewDescription, -NewDef)
%%  NewDescription should be a list [Part1 | Parts], where Parti is one of
%%  {ConcTerm, Role:Filler}; Op must be one of {add,del,up,down}
%%    510: modified (jt)    
%%         no more history, but only two parts, the first one containing
%%         the unhistorized user-def, the second one the parts leading
%%         to conflicts due to TBox revision


b5par_update_description(add,B,C,Desc1,E) :- 
	%% downward compatibility
	\+ is_list(Desc1),               %% to catch unlisted Descs
	!, b5desc_term_to_list(Desc1, Desc2),
        b5par_update_description(add,B,C,Desc2,E).
b5par_update_description(add, _, [], Desc, (NewDesc,[])) :- 
	!, append([],Desc,NewDesc).
b5par_update_description(add, _, Def, [], Def) :- !.
b5par_update_description(add, _, Def, [anything], Def) :- !.
b5par_update_description(add, Obj, (OldDef,Undesc), Additions, 
                         (SNewDef, Undesc)) :- 
        ( atom(Obj), !; Obj = uc(_) ),
        !, append(OldDef, Additions, NewDef),
	sort(NewDef, SNewDef),
        t5out_trace(added_userdef(Obj, Additions)).
b5par_update_description(del, _, ([],Un), _, ([],Un)) :- !.
b5par_update_description(del, Obj, (OldDef,Undesc), Retractions, 
              (NewDef,Undesc)) :-
	( atom(Obj), !; Obj = uc(_) ),  
	sort(Retractions, SRetractions),
	b5sort_difference(OldDef, SRetractions, NewDef), !.
b5par_update_description(up, _, (Def1,Undesc1), Name, (Def2,Undesc2)) :-
	a5rev_delete_name(Name, Def1, (Def2,DelParts)),
	append(Undesc1, DelParts, Undesc2), !.
b5par_update_description(down, _, (Def1,Undesc1), _, (Def2, []))  :-
	append(Def1, Undesc1, Def2), !.
b5par_update_description(A, B, C, D, E) :-
        !, t5out_panic(b5par_update_description(A, B, C, D, E)),
        fail.


%% ----------------------------------------------------------------------
%%  Utilities
%% ----------------------------------------------------------------------

%% b5desc_delete_name(+Name, +UserDef, -ReplacedUserDef, -DeletedParts).
a5rev_delete_name(_, [], ([], [])) :- !.
a5rev_delete_name(Name, [Part|Parts1], (Parts2, [Part|DelParts])) :-
	a5rev_is_in_p(Name, Part),
	!, a5rev_delete_name(Name, Parts1, (Parts2, DelParts)).
a5rev_delete_name(Name, [Part|Parts1], ([Part|Parts2], DelParts)) :-
	!, a5rev_delete_name(Name, Parts1, (Parts2, DelParts)).


a5rev_is_in_p(Name, Name) :- !.
a5rev_is_in_p(Name, not(Name)) :- !.
a5rev_is_in_p(Name, T1 and T2) :-
	!, 
	( a5rev_is_in_p(Name, T1), !
	; a5rev_is_in_p(Name, T2)).
a5rev_is_in_p(Name, all(Role, Conc)) :-
	!, ( a5rev_is_in_p(Name, Role), !
	; a5rev_is_in_p(Name, Conc)).
a5rev_is_in_p(Name, Role:_) :-
	!, a5rev_is_in_p(Name, Role).
a5rev_is_in_p(Name, atleast(_, Role)) :-
	!, a5rev_is_in_p(Name, Role).
a5rev_is_in_p(Name, atmost(_, Role)) :-
	!, a5rev_is_in_p(Name, Role).
a5rev_is_in_p(Name, inv(Role)) :-
	!, a5rev_is_in_p(Name, Role).


%% a5rev_get_role(+RoleDefinition, -FirstRoleName)
%%  Extracts the FirstRoleName from RoleDefinition
a5rev_get_role(RoleExpr, Role2) :-
	RoleExpr =.. [_Op, Role1,Roles],
	( a5rev_get_role1(Role1, Role2) ->
	      true
	; a5rev_get_role1(Roles, Role2)), !.
a5rev_get_role(Role1, Role2) :-
	a5rev_get_role1(Role1, Role2), !.
a5rev_get_role(RoleExpr,_) :-
	!, t5out_panic(a5rev_get_role(RoleExpr,norole)),
	fail.

a5rev_get_role1(Role, Role) :-
        a5jt_get_class_entry(Role,(r,0),_,_), !.
a5rev_get_role1(trans(Role), Role) :-
	a5jt_get_class_entry(Role,(r,0),_,_), !.
a5rev_get_role1(inv(Role), inv(Role)) :- 
	a5jt_get_class_entry(Role,(r,0),_,_), !.
a5rev_get_role1(Role._, Role) :-
	a5jt_get_class_entry(Role,(r,0),_,_), !.
a5rev_get_role1(Role comp _, Role) :-
	a5jt_get_class_entry(Role,(r,0),_,_).


%% may fail
a5jt_get_object_entry(Name, Type, Key, Definition) :-
	!, b5st_object_entry(Name, Type, Key, Definition).
a5jt_get_class_entry(Name, Type, Key, Definition) :-
	!, b5st_class_entry(Name, Type, Key, Definition).

%% must succeed
a5jt_get_object_entry_chk(Name, Type, Key, Definition) :-
	( b5st_object_entry(Name, Type, Key, Definition) ->
	      true
	; t5out_panic(a5jt_get_object_entry(Name, Type, Key, Definition)),
	  !, fail).

a5jt_get_class_entry_chk(Name, Type, Key, Definition) :-
	( b5st_class_entry(Name, Type, Key, Definition) ->
	      true
	; t5out_panic(a5jt_get_class_entry(Name, Type, Key, Definition)),
	  !, fail).


%% ----------------------------------------------------------------------

%% some helpful information
%%  sequence of scheduler tasks:
%%        [{initialize_nfi/complete_locally_NFu}, tells, complete_locally, 
%%          complete_nfi, complete, role_completion, classify, 
%%          backward_propagate, done]

%% --------- end  a5rev.pl ----------------------------------------------
% *************************************************************************
% *                         ROleCOmpletion                                *
% *************************************************************************

%% EXPORT:

%% a5roco_role_completion(+ REList, + ObjId, + ObjNF, + NFType, + Scheduler,
%%                                                 - NewONF, - NewScheduler)

%% ODER

%% a5roco_new_local_tells(+ ObjId, + ObjNF, +NFType, + REList, - TellList)
%% a5roco_new_global_tells(+ REList, + ObjId, + NFType, - TellList)

%% IMPORT:


%% append( + List, + List, - List)

%% t5concid_normal_form( + CKey, - CNF)

%% a5dep_dependency( + NFType, + ObjId, + ObjId, + Term, - Dependency)
%% a5dep_empty_dependency( - Depenency)

%% b5inst_add_backpointers( + ValueKey, + RKey, + ObjId)

%% t5hc_sub_union( + CacheNr, + RKeyList, - RKeyList),
%% t5hc_supers( + CacheNr, + RKey, - RKeyList)

%% b5nf_filled_roles_and_fillers( + ONF, - RoleExtensionList)
%% b5nf_subsumes_p( + CNF, + CNF)

%% a5obj_add_fillers( + NFType, + Obj, + ObjList, + RKey, - Obj)
%% a5obj_complete_locally(+ NFType, + Obj, - NewObj)
%% a5obj_new_fillers( + Obj, - RoleExtensionList)
%% a5obj_nf(+ NFType, + Obj, - CPF)
%% a5obj_reset_new_fillers( + Obj, - Obj)

%% a5objid_fillers( + NFType, + ObjId, + RKey, -ObjList)

%% b5rel_add( + RoleExtensionList, + RoleExtension, - RoleExtensionList)

%% t5rdb_comps( + RKey, - CompList)
%% t5rdb_trans_p( + RKey)

%% t5rnf_subsumes_without_domain_range_p( + RNF, + RNF)

%% t5role_domain( + RKey, - CKey)
%% t5role_inv_role( + RKey, - RKey)
%% t5role_normal_form( + RKey, - RNF)
%% t5role_range( + RKey, - CKey)

%% a5sch_add_dependency( + Dependency, + Scheduler, - Scheduler)
%% a5sch_add_to_agenda( + TellList, + Scheduler, - Scheduler)

%% b5sort_difference( + KeyList, + KeyList, - KeyList)
%% b5sort_difference_minus_one( + KeyList, + KeyList, + Key, - KeyList)
%% b5sort_select( + Key, + KeyList, - KeyList)
%% b5sort_unify( + KeyList, + KeyList, - KeyList)

%% t5tbox_anyrole_key( - RKey)
%% t5tbox_nothing_key(Nothing)
%% t5tbox_role_hc( - CacheNr)

%% t5cpf_create(- ONF, -Supers , + CPF)

% *************************************************************************
% *************************************************************************

%% role_completion:
%% At first new local Tells (tells at the Object itself) were computed and
%% then added to the ObjNf. This is repeated  until no new tells were found.
%% Then the global new Tells (tells at other Objects) were computed and
%% added to the scheduler.

a5roco_role_completion(REList,OId,NFType,Obj,NewObj,Scheduler,NewScheduler) :-
	a5roco_roco(REList,REList,OId,Obj,NFType,Scheduler,
                                                      NewObj,NewScheduler).
%% Patch for role completion of compositions
%% 5.3.93

a5roco_roco(GlobalREList,LocalREList,OId,Obj,NFType,Scheduler,
                                                       NewObj,NewScheduler) :-
	a5obj_nf(NFType,Obj,ONF),
	a5roco_new_local_tells(OId,ONF,NFType,LocalREList,LocalTellList),
	a5roco_add_tells_to_obj(LocalTellList,NFType,Obj,Scheduler,
                                        Obj1,Scheduler1,NewLocalREList),
	(NewLocalREList \== [],
	 !,
	 a5roco_union_relist(NewLocalREList,GlobalREList,NewGlobalREList),
	 a5roco_roco(NewGlobalREList,NewLocalREList,OId,Obj1,NFType,
                                             Scheduler1,NewObj,NewScheduler);
	     a5obj_complete_locally(NFType,Obj,NewObj),
%% new
             a5obj_nf(NFType,NewObj,NewONF),
	     a5roco_new_global_tells(GlobalREList,OId,NewONF,NFType,
                                                            GlobalTellList),
%% endnew
%	     a5roco_new_global_tells(GlobalREList,OId,NFType,GlobalTellList),
	     a5sch_add_to_agenda(GlobalTellList,Scheduler1,NewScheduler)).

% ***************************************************************************


a5roco_add_tells_to_obj([],_,Obj,Scheduler,NewObj,Scheduler,REList) :-
	a5obj_new_fillers(Obj,REList),
	a5obj_reset_new_fillers(Obj,NewObj).

a5roco_add_tells_to_obj([tell(_,_,[fills(RKey,FList)],Dep)|TellList],
                       NFType,Obj,Scheduler,NewObj,NewScheduler,REList) :-
	a5obj_add_fillers(NFType,Obj,FList,RKey,Obj1),
	a5sch_add_dependency(Dep,Scheduler,Scheduler1),
	a5roco_add_tells_to_obj(TellList,NFType,Obj1,Scheduler1,
                                              NewObj,NewScheduler,REList).

% *************************************************************************

%% For existing role fillers the most special role tells were computed.
%% Additionally the local tells for transitive roles and for role compositions
%% were computed.

a5roco_new_local_tells(OId,ONF,NFType,REList,TellList) :-
	a5roco_most_special_tells(OId,ONF,NFType,TellList1),
	a5roco_trans_local(REList,OId,NFType,TellList2),
	a5roco_comp_local(REList,OId,NFType,TellList3),
	a5roco_union_tell_list(TellList1,TellList2,TellList4),
	a5roco_union_tell_list(TellList3,TellList4,TellList).

% *************************************************************************

%% new_global_tells computes the new tells for other objects for inverse
%% roles, transitive closure and role composition.
/*
a5roco_new_global_tells(REList,OId,NFType,TellList) :-
	a5roco_inv(REList,OId,NFType,InvTellList),
	a5roco_trans_global(REList,OId,NFType,TransTellList),
	a5roco_comp_global(REList,OId,NFType,CompTellList),
	a5roco_union_tell_list(InvTellList,TransTellList,TellList1),
	a5roco_union_tell_list(TellList1,CompTellList,TellList).
*/


a5roco_new_global_tells(_,_,_,_) :-
	write('XXX'),nl,nl,nl,nl,
        fail.

a5roco_new_global_tells(REList,OId,NewONF,NFType,TellList) :-
	a5roco_inv(REList,OId,NFType,InvTellList),
	a5roco_trans_global(REList,OId,NFType,TransTellList),
	a5roco_comp_global(REList,OId,NewONF,NFType,CompTellList),
	a5roco_union_tell_list(InvTellList,TransTellList,TellList1),
	a5roco_union_tell_list(TellList1,CompTellList,TellList).

% *************************************************************************
% *************************************************************************

%% For existing rolefillers we try to find more special roles, where they
%% are fillers.
%% (obj,o2) :: r1, (obj,o2) :: r2, r1 and r2 <<< s1  --> (obj,o2) :: s1
%% (obj,o2) :: r, obj :: c, r and domain(c) <<< s2   --> (obj,o2) :: s2
%% (obj,o2) :: r, o2 :: d, r and range(d) <<< s3    --> (obj,o2) :: s3

a5roco_most_special_tells(OId,ONF,NFType,MSTellList) :-
	b5nf_filled_roles_and_fillers(ONF,REList),
	a5roco_ms_tells(REList,OId,ONF,NFType,[],MSTellList).


a5roco_ms_tells([],_,_,_,MSTellList,MSTellList).

a5roco_ms_tells([_-re([])|REList],OId,ONF,NFType,Acc,MSTellList) :-
	!,
	a5roco_ms_tells(REList,OId,ONF,NFType,Acc,MSTellList).

a5roco_ms_tells([RKey-re([FId|FList])|REList],OId,ONF,NFType,Acc,
                                                             MSTellList) :-
	a5roco_select_relist(REList,FId,[RKey],REList1,RoleList),
	t5tbox_anyrole_key(Anyrole),
	b5sort_difference(RoleList,[Anyrole],RoleList1),
	a5roco_one_filler_ms_tells(RoleList1,OId,ONF,NFType,FId,TellList),
	a5roco_union_tell_list(TellList,Acc,NewAcc),
	!,
	a5roco_ms_tells([RKey-re(FList)|REList1],OId,ONF,NFType,NewAcc,
                                                                MSTellList).

% *************************************************************************

%% For each filler we try to compute more special role filler tells.
%% The search space for new roles are subroles of the roles in RoleList,
%% which are those filled by FId. From that we move away the RoleList and
%% 'nothing'. If the rest is not empty, we compute the infimum of the known
%%  roles with the object OId as domain and the filler FId as range. 
%% For each role R in the rest we test if the infimum is more special then R.
%% Then FId is also a filler for R.

a5roco_one_filler_ms_tells(RoleList,OId,ONF,NFType,FId,TellList) :-
	t5tbox_role_hc(HC),
	t5hc_sub_union(HC,RoleList,SubList),
	t5tbox_nothing_key(Nothing),
	b5sort_difference_minus_one(SubList,RoleList,Nothing,SubRoleList),
	(SubRoleList == [],
	 !,
	 TellList = [];
	    a5roco_infimum(RoleList,InfNf),
	    a5roco_o_f_ms_tells(SubRoleList,InfNf,OId,ONF,NFType,FId,
                                                             [],TellList)).

a5roco_o_f_ms_tells([],_,_,_,_,_,TellList,TellList).

a5roco_o_f_ms_tells([RKey|RoleList],RInfNF,OId,ONF,NFType,FId,Acc,TellList) :-
	t5role_normal_form(RKey,RNF),
	t5rnf_subsumes_without_domain_range_p(RNF,RInfNF),
	t5role_domain(RKey,RDomainKey),
	t5concid_normal_form(RDomainKey,RDomainNF),
	b5nf_subsumes_p(RDomainNF,ONF),
 	t5role_range(RKey,RRangeKey),
	(t5concid_concept_p(RRangeKey),
	 t5concid_normal_form(RRangeKey,RRangeNF),
	 a5objid_nf_x(NFType,FId,_,FillerNF),
	 b5nf_subsumes_p(RRangeNF,FillerNF);
	    b5inst_is_instance_p(FId,RRangeKey)),
	 !,

%% Gilt dann auch fuer alle Superrollen geschnitten RoleList.
	t5tbox_role_hc(HC),
	t5hc_supers(HC,RKey,Supers),
	b5sort_difference(RoleList,Supers,RoleList1),
	!,
	a5roco_insert_ms_tell_list(NFType,OId,FId,RKey,Acc,NewAcc),
	a5roco_o_f_ms_tells(RoleList1,RInfNF,OId,ONF,NFType,FId,
                                                            NewAcc,TellList).

a5roco_o_f_ms_tells([RKey|RoleList],RInfNF,OId,ONF,NFType,FId,Acc,TellList) :-

%%  Gilt dann auch fuer alle Subrollen nicht.
	t5tbox_role_hc(HC),
	t5hc_subs(HC,RKey,Subs),
	b5sort_difference(RoleList,Subs,RoleList1),
	a5roco_o_f_ms_tells(RoleList1,RInfNF,OId,ONF,NFType,FId,Acc,TellList).

% *************************************************************************

%% Infimum computes a quasi normalform of the conjunktion of the roles
%% in RoleList (InfNF) and the DomainNF, which is the Normalform of the 
%% object itself and RangeNf which is the normalform of the filler (if the
%% filler in not a real object but a value, then it is the Key of the value).

a5roco_infimum(RoleList,RoleInfimumNF) :-
	t5role_list_to_nf_without_domain_range(RoleList,RoleInfimumNF).

% *************************************************************************

%% a5roco_select_relist( + RoleExtensionList, + ObjId, RKeyListAcc, 
%%                                       - RoleExtensionList, - RKeyList)
%% computes the list of RKeys, for which ObjId is a fillers. Additionally
%% ObjId is deleted from the RoleExtensionList

a5roco_select_relist([],_,RoleList,[],RoleList).

a5roco_select_relist([RKey-re(OList)|REList],OId,
                                 Acc,[RKey-re(OList1)|REList1],RoleList) :-
	(b5sort_select(OId,OList,OList1),
	 !,
	 b5sort_unify([RKey],Acc,NewAcc);
	   OList1 = OList,
	   NewAcc = Acc),
	a5roco_select_relist(REList,OId,NewAcc,REList1,RoleList).
	       
% *************************************************************************
% *************************************************************************

%% New Tells for inverse roles
%% (o1,o2) :: r, s === inv(r)  --> (o2,o1) :: s

a5roco_inv(REList,OId,NFType,InvTellList) :-
	a5roco_inv(REList,OId,NFType,[],InvTellList).

a5roco_inv([],_,_,InvTellList,InvTellList).

a5roco_inv([RKey-re(OList)|REList],OId,NFType,AccOld,InvTellList) :-
	t5role_inv_role(RKey,InvKey),      %% InvKey is a key or 'notexists'
	(number(InvKey),
	 !,
	 a5roco_insert_inv_tell_list(OList,NFType,OId,RKey,InvKey,
                                                              AccOld,AccNew);
	    a5roco_add_backpointers(OList,NFType,RKey,OId),
	    AccNew = AccOld),
	a5roco_inv(REList,OId,NFType,AccNew,InvTellList).

%% If the role R has a value range and thus no inverse role than a backpointer
%% to the object is added to the values that fill R.

a5roco_add_backpointers([],_,_,_).

a5roco_add_backpointers([Value|ValueList],NFType,RKey,OId) :-
	b5inst_add_backpointer(Value,NFType,RKey,OId),
	a5roco_add_backpointers(ValueList,NFType,RKey,OId).

% *************************************************************************
% *************************************************************************

%% New tells for transitive roles (role which are defined as r := trans(r1)):
%% At first we compute all tells for transitive roles. That are tells for
%% transitive roles in REList and their for transitive superroles.
%% Than the local or the not local transitive closure of the role extensions
%% is computed.
%% local:
%% (obj,o2) :: r1, (o2,o3) :: r1, s4 := trans(r1)  --> (obj,o3) :: s4
%% global:
%% (obj,o2) :: r, (o4,obj) :: r, trans(r) <<< s  --> (o4,obj) :: s
%[(obj,o2) :: r, (o2,o3) :: r, (o4,obj) :: r, trans(r) <<< s --> (o4,o3) :: s]

a5roco_trans_local(REList,OId,NFType,LocalTransTells) :-
	a5roco_trans_relist(REList,[],TransREList),
	a5roco_trans_local(TransREList,OId,NFType,[],LocalTransTells).

a5roco_trans_local([],_,_,LocalTransTells,LocalTransTells).

a5roco_trans_local([_-re([])|REList],OId,NFType,Acc,LocalTransTells) :-
	!,
	a5roco_trans_local(REList,OId,NFType,Acc,LocalTransTells).

a5roco_trans_local([RKey-re([FId|FList])|REList],OId,NFType,Acc,
                                                        LocalTransTells) :-
	a5objid_fillers(NFType,FId,RKey,FFillerList),
	(FFillerList == [],
	 !,
	 NewAcc = Acc;
	    a5roco_insert_trans_tell_list([OId],NFType,FId,FFillerList,RKey,
                                                                 Acc,NewAcc)),
	a5roco_trans_local([RKey-re(FList)|REList],OId,NFType,NewAcc,
                                                        LocalTransTells).

% *************************************************************************

a5roco_trans_global(REList,OId,NFType,GlobalTransTells) :-
	a5roco_trans_relist(REList,[],TransREList),
	a5roco_trans_global(TransREList,OId,NFType,[],GlobalTransTells).

a5roco_trans_global([],_,_,GlobalTransTells,GlobalTransTells).

a5roco_trans_global([RKey-re(FList)|REList],OId,NFType,Acc,NewTransTells) :-
	t5role_inv_role(RKey,InvKey),
%% all transitive roles are invertable.
	a5objid_fillers(NFType,OId,InvKey,InvFillers),
	a5roco_insert_trans_tell_list(InvFillers,NFType,OId,FList,RKey,
                                                                Acc,NewAcc),
	a5roco_trans_global(REList,OId,NFType,NewAcc,NewTransTells).

% *************************************************************************
% *************************************************************************

%% New local tells for composed roles
%% At first we compute all tells for roles that are involved in a role comp-
%% osition at the first place. That are roles in REList and superroles of
%% them.
%% Then new tells at the object itself were computed:
%% (obj,o2) :: r1, (o2,o3) :: r2, s5 := r1 comp r2  --> (obj,o3) :: s5

a5roco_comp_local(REList,OId,NFType,NewTellList) :-
	a5roco_local_comp_relist(REList,[],CompREList),
	a5roco_comp_local(CompREList,OId,NFType,[],NewTellList).

a5roco_comp_local([],_,_,NewTellList,NewTellList).

a5roco_comp_local([c([],_,_)|CompREList],OId,NFType,
                                                         Acc,NewTellList) :-
	!,
	a5roco_comp_local(CompREList,OId,NFType,Acc,NewTellList).

a5roco_comp_local([c([Obj|OList],RList,CompKey)|CompREList],OId,NFType,
                                                         Acc,NewTellList) :-
	a5roco_comp_forward(RList,Obj,[],CompKey,OId,NFType,FwList),
	a5roco_comp_combine([bw(OId,[])],NFType,FwList,OId,[],TellList),
	a5roco_union_tell_list(TellList,Acc,NewAcc),
	a5roco_comp_local([c(OList,RList,CompKey)|CompREList],OId,NFType,
                                                      NewAcc,NewTellList).

% *************************************************************************

%% New global tells for role compositions:
%% At first we compute all tells for roles that are involved in a role comp-
%% osition not at the first place. That are roles in REList and superroles of
%% them.
%% (obj,o2) :: r2, (o4,obj) :: r3, r3 comp r2 <<< s2  --> (o4,o2) :: s2
%% (o4,obj) :: r1, (obj,o2) :: r2, (o2,o3) :: r3, 
%%                         r1 comp r2 comp r3 <<< s1  --> (o4,o3) :: s1
%%   .
%%   .
%%   .
%% This implementation is incomplete for compositions with more than two
%% roles.
/*
a5roco_comp_global(REList,OId,NFType,NewTellList) :-
	a5roco_global_comp_relist(REList,[],CompREList),
	a5roco_comp_global(CompREList,OId,NFType,[],NewTellList).

a5roco_comp_global([],_,_,NewTellList,NewTellList).

a5roco_comp_global([c(RKey,OList,RList1,RList2,CompKey)|CompREList],OId,
		                                    NFType,Acc,NewTellList) :-
	a5roco_c_g_forward(OList,RKey,RList2,CompKey,OId,NFType,[],FwList),
	(FwList == [],
	 !,
	 NewAcc = Acc;

	    a5roco_comp_backward(RList1,OId,NFType,[],BwList),
	    a5roco_comp_combine(BwList,NFType,FwList,OId,[],TellList),
	    a5roco_union_tell_list(TellList,Acc,NewAcc)),

	a5roco_comp_global(CompREList,OId,NFType,NewAcc,NewTellList).
*/


a5roco_comp_global(_,_,_,_) :-
	write('XXX'),nl,nl,nl,nl,
        fail.

a5roco_comp_global(REList,OId,NewONF,NFType,NewTellList) :-
	a5roco_global_comp_relist(REList,[],CompREList),
	a5roco_comp_global(CompREList,OId,NewONF,NFType,[],NewTellList).

a5roco_comp_global([],_,_,_,NewTellList,NewTellList).

a5roco_comp_global([c(RKey,OList,RList1,RList2,CompKey)|CompREList],OId,
		                             NewONF,NFType,Acc,NewTellList) :-
	a5roco_c_g_forward(OList,RKey,RList2,CompKey,OId,NFType,[],FwList),
	(FwList == [],
	 !,
	 NewAcc = Acc;

	    a5roco_comp_backward(RList1,OId,(OId,NewONF),NFType,
                                                                 [],BwList),
	    a5roco_comp_combine(BwList,NFType,FwList,OId,[],TellList),
	    a5roco_union_tell_list(TellList,Acc,NewAcc)),

	a5roco_comp_global(CompREList,OId,NewONF,NFType,NewAcc,NewTellList).

% ***************************************************************************


% *************************************************************************

                       %% Parameter OId is superflous
a5roco_comp_forward([],Obj,DepList,CompKey,_OId,_,FwList) :-
	FwList = [fw([fills(CompKey,[Obj])],DepList)].

a5roco_comp_forward([RKey|RList],Obj,DepList,CompKey,OId,NFType,FwList) :-
	a5objid_fillers(NFType,Obj,RKey,FList),
	a5roco_c_f_list(FList,RKey,RList,Obj,DepList,CompKey,OId,NFType,
			                                      [],FwList).

a5roco_c_f_list([],_,_,_,_,_,_,_,TellList,TellList).

a5roco_c_f_list([FId|FList],RKey,RList,Obj,DepList,CompKey,OId,NFType,
                                                             Acc,FwList) :-
	a5roco_comp_forward(RList,FId,[dep(Obj,fills(RKey,[FId]))|DepList],
                                             CompKey,OId,NFType,FwList1),
	append(FwList1,Acc,NewAcc),
	a5roco_c_f_list(FList,RKey,RList,Obj,DepList,CompKey,OId,NFType,
                                                          NewAcc,FwList).
	

a5roco_c_g_forward([],_,_,_,_,_,FwList,FwList).

a5roco_c_g_forward([Obj|OList],RKey,RList2,CompKey,OId,NFType,Acc,FwList) :-
	a5roco_comp_forward(RList2,Obj,[dep(OId,fills(RKey,[Obj]))],CompKey,
                                                          OId,NFType,FwList1),
	append(FwList1,Acc,NewAcc),
	a5roco_c_g_forward(OList,RKey,RList2,CompKey,OId,NFType,NewAcc,FwList).
	
% *************************************************************************

a5roco_comp_backward(_,_,_,_,_) :-
	write('XXX'),nl,nl,nl,nl,
        fail.
a5roco_c_b_list(_,_,_,_,_,_,_) :-
	write('XXX'),nl,nl,nl,nl,
        fail.

a5roco_comp_backward([RKey|RList1],InvFiller,(OId,NewONF),NFType,Dep,BwList) :-
	t5role_inv_role(RKey,InvKey),
%% all roles of a composition except the last, which is not in the rolelist
%% for comp_backward, are invertable.
%	a5objid_fillers(NFType,InvFiller,InvKey,InvFillerList),
        a5roco_fillers(NFType,InvFiller,(OId,NewONF),InvKey,InvFillerList),
	a5roco_c_b_list(InvFillerList,[RKey|RList1],InvFiller,
                                           (OId,NewONF),NFType,Dep,[],BwList).

a5roco_c_b_list([],_,_,_,_,_,BackwardList,BackwardList).

a5roco_c_b_list([F|FList],[RKey],InvFiller,
                                    (OId,NewONF),NFType,DepList,Acc,BwList) :-
	!,
	a5roco_c_b_list(FList,[RKey],InvFiller,
                      (OId,NewONF),NFType,DepList,[bw(F,DepList)|Acc],BwList).
	
a5roco_c_b_list([F|FList],[RKey|RList1],InvFiller,
                                    (OId,NewONF),NFType,DepList,Acc,BwList) :-
	a5roco_comp_backward(RList1,F,(OId,NewONF),NFType,
                            [dep(F,fills(RKey,[InvFiller]))|DepList],BwList1),
	append(BwList1,Acc,NewAcc),
	a5roco_c_b_list(FList,[RKey|RList1],InvFiller,
                                   (OId,NewONF),NFType,DepList,NewAcc,BwList).

% ***************************************************************************

% *************************************************************************

%% This predicate combines the 'forward-list' and the 'backward-list' to new
%% rolefiller tells

a5roco_comp_combine([],_,_,_,TellList,TellList).

a5roco_comp_combine([bw(Obj,Dep)|BwList],NFType,FwList,OId,Acc,TellList) :-
	a5roco_comp_c(FwList,NFType,Obj,Dep,OId,[],TellList1),
	a5roco_union_tell_list(TellList1,Acc,NewAcc),
	a5roco_comp_combine(BwList,NFType,FwList,OId,NewAcc,TellList).

a5roco_comp_c([],_,_,_,_,TellList,TellList).

a5roco_comp_c([fw(Tell,FDep)|FwList],NFType,Obj,BDep,OId,Acc,TellList) :-
	a5roco_make_deps(FDep,NFType,Obj,DepList1),
	a5roco_make_deps(BDep,NFType,Obj,DepList2),
	append(DepList1,DepList2,DepList),
	a5roco_comp_c(FwList,NFType,Obj,BDep,OId,
                                 [tell(Obj,OId,Tell,DepList)|Acc],TellList).

% *************************************************************************

a5roco_make_deps([],_,_,[]).

a5roco_make_deps([dep(Obj2,Tell)|IDepList],NFType,Obj1,[Dep|DepList]) :-
	a5dep_dependency(NFType,Obj1,Obj2,cinst(Obj2,Tell),Dep),
	a5roco_make_deps(IDepList,NFType,Obj1,DepList).

% *************************************************************************
% *************************************************************************

%% a5roco_trans_relist provides all new role extensions of transitive
%% roles.  

a5roco_trans_relist([],TransREList,TransREList).

a5roco_trans_relist([RKey-re(FList)|REList],Acc,TransREList) :-
	t5tbox_role_hc(HC),
	t5hc_supers(HC,RKey,Supers),
	a5roco_insert_trans_relist([RKey|Supers],FList,Acc,AccNew),
	a5roco_trans_relist(REList,AccNew,TransREList).
	

a5roco_insert_trans_relist([],_,REList,REList).

a5roco_insert_trans_relist([RKey|RList],FList,REList,NewREList) :-
	t5rdb_trans_p(RKey),
	!,
	b5rel_add(REList,RKey-re(FList),REList1),
	a5roco_insert_trans_relist(RList,FList,REList1,NewREList).
	
a5roco_insert_trans_relist([_|RList],FList,REList,NewREList) :-
	a5roco_insert_trans_relist(RList,FList,REList,NewREList).

% *************************************************************************

%% This predicates provides the relevant new tells for local completion of
%% role compositions

a5roco_local_comp_relist([],CompREList,CompREList).

a5roco_local_comp_relist([RKey-re(FList)|REList],Acc,CompREList) :-
	t5tbox_role_hc(HC),
	t5hc_supers(HC,RKey,Supers),
	a5roco_insert_local_comp_relist([RKey|Supers],FList,Acc,NewAcc),
	a5roco_local_comp_relist(REList,NewAcc,CompREList).

a5roco_insert_local_comp_relist([],_,CompREList,CompREList).

a5roco_insert_local_comp_relist([RKey|RList],FList,Acc,CompREList) :-
	t5rdb_comps(RKey,CompList),
	a5roco_insert_l_c_rel(CompList,FList,CompREList1),
	append(CompREList1,Acc,NewAcc),
	a5roco_insert_local_comp_relist(RList,FList,NewAcc,CompREList).

a5roco_insert_l_c_rel([],_,[]).

a5roco_insert_l_c_rel([comp([],RList,CompKey)|CompList],FList,
                                    [c(FList,RList,CompKey)|CompREList]) :-
	!,
	a5roco_insert_l_c_rel(CompList,FList,CompREList).

a5roco_insert_l_c_rel([_|CompList],FList,CompREList) :-
	a5roco_insert_l_c_rel(CompList,FList,CompREList).
	
% *************************************************************************

%% The relevant role extension for the non local completion of role-composi-
%% tions is computed. This were new RE or their superroles, which were part
%% of a composition not at the first place.

a5roco_global_comp_relist([],GlobalCompREList,GlobalCompREList).

a5roco_global_comp_relist([RKey-re(FList)|REList],Acc,GCompREList) :-
	t5tbox_role_hc(HC),
	t5hc_supers(HC,RKey,Supers),
	a5roco_insert_global_comp_relist([RKey|Supers],FList,Acc,NewAcc),
	a5roco_global_comp_relist(REList,NewAcc,GCompREList).

a5roco_insert_global_comp_relist([],_,CompREList,CompREList).

a5roco_insert_global_comp_relist([RKey|RList],FList,Acc,CompREList) :-
	t5rdb_comps(RKey,CompList),
	a5roco_insert_g_c_rel(CompList,RKey,FList,CompREList1),
	append(CompREList1,Acc,NewAcc),
	a5roco_insert_global_comp_relist(RList,FList,NewAcc,CompREList).

a5roco_insert_g_c_rel([],_,_,[]).

a5roco_insert_g_c_rel([comp([R1|RList1],RList2,CompKey)|CompList],RKey,FList,
                     [c(RKey,FList,[R1|RList1],RList2,CompKey)|CompREList]) :-
	!,
	a5roco_insert_g_c_rel(CompList,RKey,FList,CompREList).

a5roco_insert_g_c_rel([_|CompList],RKey,FList,CompREList) :-
	a5roco_insert_g_c_rel(CompList,RKey,FList,CompREList).

% *************************************************************************

% SORTIERT ???  BEI UWE SCHON VORHANDEN ??

a5roco_union_relist([],REList,REList).

a5roco_union_relist([RE|REList],[],[RE|REList]) :-
	!.

a5roco_union_relist([RKey-re(OList)|REList1],REList2,NewREList) :-
	b5rel_add(REList2,RKey-re(OList),REList22),
	a5roco_union_relist(REList1,REList22,NewREList).
	
% *************************************************************************

a5roco_union_tell_list(TellList1,TellList2,TellList3) :-
	append(TellList1,TellList2,TellList3).

a5roco_insert_tell_list(Tell,TellList,[Tell|TellList]).

/*
a5roco_insert_ms_tell_list(OId,FId,RKey,TellList,NewTellList) :-
	a5dep_empty_dependency(EmptyDep),
	a5roco_insert_tell_list(tell(OId,OId,[fills(RKey,[FId])],EmptyDep),
                                                        TellList,NewTellList).
*/
%% Dependencies has to be added because of:
%%   O1 :: R:O2 
%%   O2 :: C
%%     ==> O1 :: R and range(C)
%% which depends on O2

a5roco_insert_ms_tell_list(NFType,OId,FId,RKey,TellList,NewTellList) :-
	t5role_range(RKey,RangeKey),
	a5dep_dependency(NFType,OId,FId,cinst(FId,RangeKey),Dep),
	a5roco_insert_tell_list(tell(OId,OId,[fills(RKey,[FId])],Dep),
                                                        TellList,NewTellList).

a5roco_insert_inv_tell_list([],_,_,_,_,NewTellList,NewTellList).

a5roco_insert_inv_tell_list([FId|FList],NFType,OId,RKey,InvKey,
                                                      TellList,NewTellList) :-
	a5dep_dependency(NFType,FId,OId,cinst(OId,fills(RKey,[FId])),Dep),
	a5roco_insert_tell_list(tell(FId,OId,[fills(InvKey,[OId])],Dep),
                                                         TellList,TellList1),
	a5roco_insert_inv_tell_list(FList,NFType,OId,RKey,InvKey,
                                                      TellList1,NewTellList).


a5roco_insert_trans_tell_list([],_,_,_,_,NewTellList,NewTellList).

a5roco_insert_trans_tell_list([OId|OList],NFType,FId,FFillerList,TransKey,
                                                      TellList,NewTellList) :-
	a5roco_trans_dep_list(FFillerList,NFType,OId,FId,TransKey,Dep),
	a5roco_insert_tell_list(tell(OId,FId,[fills(TransKey,FFillerList)],Dep)
                                                       ,TellList,TellList1),
	a5roco_insert_trans_tell_list(OList,NFType,FId,FFillerList,TransKey,
                                                      TellList1,NewTellList).

a5roco_trans_dep_list([],_,_,_,_,[]).
a5roco_trans_dep_list([FFiller|FFillerList],NFType,OId,FId,TransKey,
                                                            [Dep|DepList]) :-
	a5dep_dependency(NFType,OId,FId,cinst(FId,fills(TransKey,[FFiller])),Dep),
	a5roco_trans_dep_list(FFillerList,NFType,OId,FId,TransKey,DepList).
	
% *************************************************************************



%% To get the fillers at inverse role of the object itself (OId) we have to
%% look in its actuell normal form (NewONF) and not in the stored one as for
%% other objects.

a5roco_fillers(NFType,InvFiller,(OId,NewONF),InvKey,InvFillerList) :-
	(InvFiller == OId,
	 !,
	 b5nf_fillers(NewONF,InvKey,InvFillerList);
	    a5objid_fillers(NFType,InvFiller,InvKey,InvFillerList)).
%%
%%  S C H E D U L E R 
%%

/* --- Exports ----------------------------------------

a5sch_make_scheduler(-NewScheduler)
a5sch_make_scheduler(+Mode,-NewScheduler)
	Returns an initialized scheduler with empty agenda,
	empty blackboard, an empty set of new Dependencies,
	and the default mode for handling assertions (monotonic
        additions).
	Additionally a required Mode may be specified,
	where Mode is one of [monotonic,retraction]. 

a5sch_process(+ONF, ?ObjID, +Scheduler, -NewScheduler)
	Passes ONF to the Scheduler for further processing.
	If called with ObjID variable a new object is created and its
	ID returned in ObjID. Otherwise ObjID must be instantiated 
	with the ID of an existing object that is going to be modified.
	Similar to pred STORE of MO conc.

a5sch_process_agenda(+Scheduler)
a5sch_process_agenda(+Scheduler,-FinalScheduler)
	Interface predicate for parser. Passes control to scheduler
	which will process all jobs stacked on its Agenda.

a5sch_add_to_agenda(+NewEntry, +Scheduler, -NewScheduler)
	Adds NewEntry to the agenda of Scheduler and returns the
	resulting NewScheduler. NewEntry may be one of the following:
	- complete_locally(+Obj)
	- complete(+Obj)
	- tell(+AffectedObj, +TriggerObj, +TermList, +Dependency)
	     where TermList is a list of Terms according to the 
	     Kernel Interface, and
	     Dependency is given according to Dependency Module specification.
	- backpointer(+FillerObj, +FilledObj, +Role)
	- backward_propagate(+Obj)
	- classify(+Obj)
        - process_until_NFi(+Obj)
        - a List of NewEntries
	where '...Obj' is an identifier, not the Object itself.

a5sch_add_dependency(+Dependency, +Scheduler, -NewScheduler)
	Adds a new Dependency to Scheduler and returns the resulting
	NewScheduler. Dependency may be a single dependency, or
        a list of dependencies.

---------------------------------------------------- */


?- ensure_loaded(library(ordsets)).
?- ensure_loaded(library(sets)).
?- ensure_loaded(library(bitsets)).

%% a5sch_make_scheduler(-NewScheduler)
%% a5sch_make_scheduler(+Mode,-NewScheduler) 
%%  Returns an initialized scheduler with empty agenda,
%%  empty blackboard, an empty set of new Dependencies,
%%  and the default mode for handling assertions (monotonic additions).
%%  Additionally a required Mode may be specified,
%%  where Mode is one of [monotonic,retraction,nfs,nfi]. 

a5sch_make_scheduler(scheduler(nfs,[],EmptyAgenda,Dependencies,monotonic) ) :-
	a5sch_make_agenda(EmptyAgenda),
	a5dep_dependencies_create(Dependencies).

a5sch_make_scheduler(monotonic,S) :- !, a5sch_make_scheduler(S).
a5sch_make_scheduler(nfs,S) :- !,
	a5sch_make_scheduler(S0),
	a5sch_scheduler_substitute(nf_selector,S0,nfs,S).
a5sch_make_scheduler(nfi,S) :- !,
	a5sch_make_scheduler(S0),
	a5sch_scheduler_substitute(nf_selector,S0,nfi,S).
a5sch_make_scheduler(retraction,scheduler(Sel,BB,A,D,retraction([],[],D))) :-
	!, a5sch_make_scheduler(scheduler(Sel,BB,A,D,_)).
a5sch_make_scheduler(UnknownMode,_) :-
	t5out_error(unknown_scheduler_mode(UnknownMode)),
	!, fail.

%% a5sch_scheduler(+Slot, +Scheduler, -SlotValue)
%% a5sch_scheduler_substitute(+Slot, +Scheduler, +NewSlotValue, -NewScheduler)
%% a5sch_scheduler_replace(+Slot, +-Scheduler, +-SlotValue,
%%                         -+NewScheduler, -+NewSlotValue)

a5sch_scheduler(nf_selector,scheduler(NFsel,_,_,_,_), NFsel).
a5sch_scheduler(blackboard,scheduler(_,BB,_,_,_),BB).
a5sch_scheduler(agenda,scheduler(_,_,A,_,_),A).
a5sch_scheduler(dependencies,scheduler(_,_,_,D,_),D).
a5sch_scheduler(depending_objects,scheduler(_,_,_,_,retraction(D,_,_)),D).
a5sch_scheduler(backpointers,scheduler(_,_,_,_,retraction(_,B,_)),B).
a5sch_scheduler(negative_dependencies,scheduler(_,_,_,_,retraction(_,_,N)),N).

a5sch_scheduler_substitute(nf_selector,scheduler(_,BB,A,D,M), NFsel,
	scheduler(NFsel,BB,A,D,M)).
a5sch_scheduler_substitute(blackboard,scheduler(N,_,A,D,M),BB,
	scheduler(N,BB,A,D,M)).
a5sch_scheduler_substitute(agenda,scheduler(N,B,_,D,M),A,
	scheduler(N,B,A,D,M)).
a5sch_scheduler_substitute(dependencies,scheduler(N,B,A,_,M),D,
	scheduler(N,B,A,D,M)).
a5sch_scheduler_substitute(depending_objects,
	scheduler(N,BB,A,D,retraction(_,BP,ND)),
	DepObjs, scheduler(N,BB,A,D,retraction(DepObjs,BP,ND))).
a5sch_scheduler_substitute(backpointers,
	scheduler(N,BB,A,D,retraction(DO,_,ND)),
	BP, scheduler(N,BB,A,D,retraction(DO,BP,ND))).
a5sch_scheduler_substitute(negative_dependencies,
	scheduler(N,BB,A,D,retraction(DO,BP,_)),
	NegDeps, scheduler(N,BB,A,D,retraction(DO,BP,NegDeps))).


a5sch_scheduler_replace(nf_selector, scheduler(NFsel0,BB,A,D,M), NFsel0,
	                             scheduler(NFsel, BB,A,D,M), NFsel ).
a5sch_scheduler_replace(blackboard, scheduler(N,BB0,A,D,M), BB0,
	                            scheduler(N,BB, A,D,M), BB ).
a5sch_scheduler_replace(agenda, scheduler(N,B,A0,D,M), A0,
	                        scheduler(N,B,A, D,M), A ).
a5sch_scheduler_replace(dependencies, scheduler(N,B,A,D0,M), D0,
	                              scheduler(N,B,A,D,M),  D ).
a5sch_scheduler_replace(depending_objects,
	scheduler(N,BB,A,D,retraction(DepObjs0,BP,ND)), DepObjs0, 
	scheduler(N,BB,A,D,retraction(DepObjs, BP,ND)), DepObjs ).
a5sch_scheduler_replace(backpointers,
	scheduler(N,BB,A,D,retraction(DO,BP0,ND)), BP0,
	scheduler(N,BB,A,D,retraction(DO,BP, ND)), BP ).
a5sch_scheduler_replace(negative_dependencies,
	scheduler(N,BB,A,D,retraction(DO,BP,NegDeps0)), NegDeps0, 
	scheduler(N,BB,A,D,retraction(DO,BP,NegDeps )), NegDeps ).


a5sch_retraction_mode_p(scheduler(_,_,_,_,retraction(_,_,_))).




%% a5sch_process(+ONF, ?ObjID, +Scheduler, -NewScheduler)
%%  Passes ONF to the Scheduler for further processing.
%%  If called with ObjID variable a new object is created and its
%%  ID returned in ObjID. Otherwise ObjID must be instantiated 
%%  with the ID of an existing object that is going to be modified.
%% 
%%  Similar to pred STORE of MO conc.

a5sch_process(ONF, ObjID, Scheduler, NewScheduler) :-
	(var(ObjID) ->
	    a5sch_process_new_object(ONF, ObjID, Obj)
	; a5sch_retraction_mode_p(Scheduler) ->
	       a5sch_redescribe_existing_object(ONF, ObjID, Obj)
	; a5sch_process_existing_object(ONF, ObjID, Obj)),
	a5sch_object_store(ObjID, Obj, Scheduler, S1),
	a5sch_add_to_agenda(complete_locally_NFu(ObjID), S1, NewScheduler).

a5sch_process_new_object(ONF, ObjID, Obj):-
	b5keys_gen_key(ObjID),  %% ???
	a5obj_create(ObjID, Obj0),
	a5obj_unify_nf(nfu, ONF, Obj0, Obj),
	t5out_info(a5sch_put_new_object_on_agenda(ObjID)).

a5sch_process_existing_object(ONF, ObjID, Obj) :-
	a5obj_fetch(ObjID, Obj0),
	a5obj_unify_nf(nfu, ONF, Obj0, Obj),
	t5out_info(a5sch_put_existing_object_on_agenda(ObjID)).	

a5sch_redescribe_existing_object(ONF, ObjId, Obj) :-
	a5obj_create(ObjId, DummyObj),
	a5obj_nf(nfu,DummyObj,DummyNFu),
	a5obj_fetch(ObjId, Obj0),
	a5obj_replace_nf(nfu, DummyNFu, Obj0, Obj1),
	a5obj_unify_nf(nfu, ONF, Obj1, Obj),
	t5out_info(a5sch_put_redescribed_object_on_agenda(ObjId)).


%% a5sch_process_agenda(+Scheduler)
%% a5sch_process_agenda(+Scheduler,-FinalScheduler)
%%  Interface predicate for parser. Passes control to scheduler
%%  which will process all jobs stacked on its Agenda.

%% cmk 07.01.93: added Scheduler as Arg to a5sch_reset_old_state

a5sch_process_agenda(Scheduler) :- a5sch_process_agenda(Scheduler,_).
a5sch_process_agenda(Scheduler,FinalScheduler) :-
	t5out_info(a5sch_start_processing_objects_on_agenda),
	( a5sch_process_agenda(/* start */ nfu, Scheduler,FinalScheduler) ->
	    a5sch_finalize_agenda_processing(FinalScheduler)
	;   a5sch_reset_old_state(Scheduler),
	    fail).


%% a5sch_process_agenda(+ONFselector, +Scheduler, -NewScheduler)
%%  - get agenda
%%  - get next job
%%  - process next job
%%  - process agenda
%% NB: It is ensured that all objects have a valid NFu, NFs, and esp. NFi.
%%  The NFi is initially set to the NFs of a new object, independently
%%  of whether the IBox is empty or filled.

/*
a5sch_process_agenda(nfu, S0,S) :-
	b5sta_check_flag(aboxfilled, true),
	!,
	a5sch_process_pure_TBox_objects(S0,S).
*/
a5sch_process_agenda(nfi /* final */,S,S) :- !.
a5sch_process_agenda(nfs, S,S) :-
	i5ibox_empty_p, 
	!,
	a5sch_replace_nfi(S).
a5sch_process_agenda(NFsel0, S0,S) :-
	a5sch_next_nf(NFsel0, NFsel),
	a5sch_scheduler_substitute(nf_selector,S0,NFsel,S1),
	t5out_info(a5sch_start_computing_object_normal_form(NFsel)),
	a5sch_prepare_agenda_processing(NFsel, S1, S2),
	a5sch_process_agenda1(NFsel, S2,S3),
	a5sch_process_agenda(NFsel,S3,S).

a5sch_process_agenda1(nfs, Scheduler, Scheduler) :-
	a5sch_scheduler(agenda,Scheduler,Agenda),
	a5sch_agenda_done(Agenda), 
	!.
a5sch_process_agenda1(nfi, S0,S) :-
	a5sch_scheduler(agenda,S0,Agenda),
	a5sch_agenda_done(Agenda), 
	!, 
	( a5sch_some_objects_have_been_truly_modified(S0,S1,Objs) -> %% 29.1.93
%%	( a5sch_some_objects_have_been_modified(S0,S1,Objs) ->
	    a5sch_scheduler_replace(agenda,S1,A1,S2,A2),
	    a5sch_add_objects_to_agenda(Objs,complete_nfi,A1,A2),
	    a5sch_process_agenda1(nfi,S2,S)
	; S0=S).	
a5sch_process_agenda1(ONFsel, S0, S) :-
	%% not a5sch_agenda_done(Agenda0), 
	a5sch_scheduler_replace(agenda,S0,Agenda0,S1,Agenda1),
	a5sch_next_agenda_job(Agenda0, Job, Agenda1),
	a5sch_process_job(Job, ONFsel, S1, S2),
	a5sch_process_agenda1(ONFsel, S2,S).


%% cmk 27.1.94
%% a5sch_prepare_agenda_processing(+NFsel, +S0, -S)
%% FFS: In the monotonic NFi mode, request initialize_nfi only 
%%  for objects that are marked as modified on the blackboard 
%%  (which is especially the case for new objects!).
%%  For postponed objects, which bypassed the NFs phase, request it
%%  explicitly (they will not be marked as modified)!
%%
%% FFS: Hier koennte spaeter ein Umsetzen der NF-Zeiger passieren,
%%  dh. der Uebergang von 
%%      LocalNF -> NFu, GlobalNF -> NFs
%%  auf LocalNF -> NFi/local, GlobalNF -> NFi/global

a5sch_prepare_agenda_processing(nfs, S0, S) :-
	(a5sch_retraction_mode_p(S0) ->
	    a5sch_prepare_retraction(nfs,S0,S)
	; S0=S).
a5sch_prepare_agenda_processing(nfi, S0, S) :-
	a5sch_activate_postponed_objects(S0,S1,PObjs),
	(a5sch_retraction_mode_p(S1) ->
	    a5sch_prepare_retraction(nfi,S1,S)
	; (a5sch_some_objects_have_been_modified(S1,S2,MObjs) ->  
	         true; S1=S2, MObjs=[]), 
	    a5sch_scheduler_replace(agenda,S2,A0,S,A),
	    ord_union(PObjs, MObjs, Objs),
	    a5sch_add_objects_to_agenda(Objs,initialize_nfi,A0,A)
	).
	   	


%% a5sch_some_objects_have_been_truly_modified(+Scheduler,-Scheduler,-Objs)
%%  True, if some Objs have been truly modified during last i-link
%%  application round, i.e., they had been marked as modified and the
%%  NFi =\= NFs.
%%  This prevents the 2nd attempt to apply i-links when already the 1st
%%  try didn't yield anything.

a5sch_some_objects_have_been_truly_modified(S0,S,Objs) :-
	a5sch_some_objects_have_been_modified(S0,S, Objs0),
	a5sch_objs_modified_by_ilink_application(Objs0,Objs).

a5sch_objs_modified_by_ilink_application([],[]).
a5sch_objs_modified_by_ilink_application([A|As], Zs) :-
	(a5sch_obj_modified_by_ilink_application_p(A) ->
	    Zs=[A|Zs0] ; Zs=Zs0),
	a5sch_objs_modified_by_ilink_application(As, Zs0).

a5sch_obj_modified_by_ilink_application_p(ObjId) :-
	a5obj_fetch(ObjId,Obj),
	a5obj_nf(nfs, Obj, NFs),
	a5obj_nf(nfi, Obj, NFi),
	b5nf_modified_p(NFs, NFi).
	

%% a5sch_replace_nfi(+Scheduler)
%%  Replaces for every O on the Blackboard NFi(O) by NFs(O).
%%  To be called for empty Ibox.

a5sch_replace_nfi(S) :-
	a5sch_blackboard_objects(S,Objs),
	a5sch_replace_nfi1(Objs).

a5sch_replace_nfi1([]).
a5sch_replace_nfi1([O|Os]) :-
	a5obj_fetch(O,Obj0),
	a5obj_nf(nfs, Obj0, NFs),
	a5obj_replace_nf(nfi, NFs, Obj0, Obj),
	a5obj_store(Obj),
	a5sch_replace_nfi1(Os).



%% a5sch_process_job(+Job, +ONFselector, +Scheduler, -NewScheduler)
%%  Job is ObjID-AgendaEntry.
%%  - get affected object
%%  - process everything in AgendaEntry
%%  - store object
%%  All Jobs collected for ObjID are processed at once to reduce
%%  calls to assert and retract. Assumes that local things that
%%  are abstracted etc. during this instance of a5sch_process_job/4
%%  are not passed to the scheduler but are
%%  handled immediately by the invoked reasoning procedures.
%%  Would still work if things were passed to Scheduler, but it 
%%  would take another call to a5sch_process_agenda/3 to actually
%%  process them.

a5sch_process_job(ObjID-Entry, ONFsel, S0, S) :-
	t5out_info(a5sch_process_object(ObjID)),
	a5obj_fetch(ObjID, Obj0),
	( a5sch_retraction_mode_p(S0) ->
	      InitialJob=recompute_NF; a5sch_initial_job(ONFsel,InitialJob)),
	a5sch_process_jobs(InitialJob,Entry,ONFsel,Obj0,Obj,S0,S1),
	not(a5obj_incoherent_p(ONFsel, Obj)),
	a5sch_object_store(Obj,S1,S).


%% a5sch_process_jobs(Phase, AgendaEntry, ONFsel, Obj0,Obj, S0,S) :-
%% FFS: Consider to unfold a5sch_next_phase/2 into a5sch_agenda_entry_next/4
%%  may eliminate ~|Flags| unnecessary calls of a5sch_process_jobs/7.

%% wann werden die einzelnen Jobs auf die Agenda getan, und von wem ??
%% Bsp. wird global dafuer gesorgt, dass nach complete_locally noch
%% ein complete gemacht wird, oder bestimmt complete_locally seinen
%% Nachfolger ??
%% Jeder Job sollte zumindest die Moeglichkeit haben,
%% nachfolgende Jobs zu canceln , z.B. wenn lokale Kompletierung
%% keine neue NF ergibt, kann der Rest gespart werden !?
%%
%% Z.Z. setzt a5sch_process_single_job(complete_locally_NFu, ...
%% die weiteren Flags.
%% Jeder weitere Job setzt ebenfalls seine Nachfolger.


a5sch_process_jobs(done, AgendaEntry, _, Obj0,Obj, S0,S) :-
	!, %% RED %
	a5sch_agenda_entry_done(AgendaEntry),
	Obj = Obj0, S = S0.
a5sch_process_jobs(Phase, AE0, ONFsel, Obj0,Obj, S0,S)  :-
	(a5sch_agenda_entry_next(Phase, AE0, Job, AE1) ->
	     a5sch_process_a_single_job(Job, ONFsel, Obj0,Obj1, AE1,AE, S0,S1),
	     NextPhase = Phase
	; a5sch_next_phase(Phase,NextPhase), 
	     AE = AE0, Obj1 = Obj0, S1 = S0 ),
	a5sch_process_jobs(NextPhase, AE, ONFsel, Obj1,Obj, S1,S).
		  

%% a5sch_initial_job(+NFselector, -FirstJobToDoForGivenNFselector)

a5sch_initial_job(nfs,complete_locally_NFu).
a5sch_initial_job(nfi,initialize_nfi).


%% a5sch_next_phase(+CurrentPhase, -NextPhase).
%%  Defines the state transition for processing the agenda.
%%  FFS: Role-Specialization Phase before backward_propagate Phase -> mf
%%   I.e.: if after classification the current obj is more special then
%%   before, set the role-specialization request bit !

a5sch_next_phase(initialize_nfi,tells).
a5sch_next_phase(complete_locally_NFu, tells).
a5sch_next_phase(recompute_NF, tells).
a5sch_next_phase(tells, complete_locally).
a5sch_next_phase(complete_locally, complete_nfi).
a5sch_next_phase(complete_nfi, complete).
a5sch_next_phase(complete, role_completion).
a5sch_next_phase(role_completion, classify).
%--- a5sch_next_phase(backpointer, classify).
a5sch_next_phase(classify, backward_propagate).
a5sch_next_phase(backward_propagate, done).

%% a5sch_next_nf(+CurrentNF, -NextNF)
%% a5sch_previous_nf(+CurrentNF, -PreviousNF)
%%  Fails if CurrentNF has no successor/predecessor.

a5sch_next_nf(nfu,nfs).
a5sch_next_nf(nfs,nfi).

a5sch_previous_nf(CurrentNF, PreviousNF) :-
	a5sch_next_nf(PreviousNF, CurrentNF), !.


%% FFS: a5sch_process_a_single_job(J, ONFsel, +Obj0,-Obj, +AE0,-AE, +S0,-S)
%%  Tells (clause 4) have to be passed one by one in order to determine
%%  dependency links.

a5sch_process_a_single_job(Job, ONFsel, Obj0,Obj, AE0,AE, S0,S) :-
	a5obj_id(Obj0,Id),
	t5out_trace(a5sch_process_single_job(Job,Id)),
	a5sch_process_single_job(Job, ONFsel, Obj0,Obj, AE0,AE, S0,S).

a5sch_process_single_job(complete_locally_NFu, _, Obj0,Obj, AE0,AE, S,S) :-
	a5sch_complete_locally_NFu(Obj0,Obj,AE0,AE).

%% FFS: im allgemeinen Fall: recompute_NF, ONFsel;
%%  dann muss auch die Backpointer Information fuer ONFsel vorbereitet sein...

a5sch_process_single_job(recompute_NF, NFsel, Obj0,Obj, AE0,AE, S,S) :-
	a5sch_recompute_NF(NFsel, Obj0,Obj, AE0,AE,S).

a5sch_process_single_job(complete_locally, ONFsel, Obj0,Obj, AE0,AE, S,S) :-
	a5obj_complete_locally(ONFsel, Obj0, Obj),
	%% FFS: modified?? 
	(fail -> %%a5obj_modified_p(ONFsel, Obj, Obj0) -> 
	    Jobs = [complete,classify,backward_propagate],
	    a5sch_modify_agenda_entry(Jobs, AE0, AE1) 
	; AE0 = AE1 ),
	a5sch_modify_agenda_entry(role_completion,AE1,AE).

a5sch_process_single_job(complete, ONFsel, Obj0,Obj, AE,AE, S0,S) :-
	a5obj_complete(ONFsel, Obj0, Obj, S0,S).

a5sch_process_single_job(role_completion, NFsel, Obj0,Obj, AE,AE, S0,S) :-
	a5rec_role_completion(NFsel, Obj0,Obj, S0,S).

a5sch_process_single_job(tell(_Trigger, Term, Dep), ONFsel, 
			 Obj0,Obj, AE0,AE, S0,S) :-
	a5sch_process_tell_job(Term, _Trigger, Dep, 
			       ONFsel, Obj0,Obj, AE0,AE, S0,S).

a5sch_process_single_job(backpointer(R,Objs), ONFsel, Obj0,Obj, AE,AE, S,S) :-
	a5obj_add_backpointer(ONFsel, Obj0, R, Objs, Obj).

a5sch_process_single_job(backward_propagate, ONFsel, Obj,Obj, AE,AE, S0,S) :-
	a5rec_backward_propagate(Obj, ONFsel, S0,S).

a5sch_process_single_job(classify, ONFsel, Obj0,Obj, AE0,AE, S0,S) :-
	a5rec_classify(ONFsel, Obj0,Obj, S0,S),
	(a5obj_modified_p(ONFsel, Obj, Obj0) -> 
	    Jobs = [backward_propagate],
	    a5sch_modify_agenda_entry(Jobs, AE0, AE) 
	; AE0 = AE ).

%% Once this is requested it must be performed! 
%%  If an object should bypass I-Link application (e.g. because the
%%  NFs wasn't changed), the calling environment must ensure this.

a5sch_process_single_job(initialize_nfi, _, Obj0,Obj, AE0,AE, S,S) :-
	a5obj_nf(nfs, Obj0, NFs),
	a5obj_unify_nf(nfi,NFs,Obj0,Obj),
	a5sch_modify_agenda_entry(complete_nfi, AE0, AE).


a5sch_process_single_job(complete_nfi, _, Obj0,Obj, AE0,AE, S,S) :-
	a5sch_complete_nfi(Obj0,Obj,AE0,AE).


%% complete_locally_NFu:
%%  IF Obj has been existing before, 
%%	but NFu as before the recent user-input has not been changed
%%  THEN skip next NFs steps
%%  ELSE continue with NFs processing

a5sch_complete_locally_NFu(Obj0,Obj, AE0,AE) :-
	a5obj_complete_locally(nfu, Obj0, Obj1),
	a5obj_id(Obj0, ObjId),
	a5obj_nf(nfu, Obj0, NFu1),
	( a5objid_tmp_nfu(ObjId, NFu0),
	 \+ b5nf_modified_p(NFu0,NFu1) ->
	    AE0=AE, Obj0=Obj
	;   %% new or user-modified object
	    a5obj_unify_nf(nfs, NFu1, Obj1, Obj),
	    Jobs = [complete,role_completion,classify,backward_propagate], 
	    a5sch_modify_agenda_entry(Jobs, AE0, AE)).


%% a5sch_process_tell_job(+Term, +TriggerObj, +Dep, 
%% 		          +ONFsel, +Obj0,-Obj, +AE0,-AE, +S0,-S)
%%  Adds Term to Obj0 to yield Obj. If Term does not subsume Obj0 for
%%  the selected ONF, Dep is added as a new dependency.
%%  Determines the next agenda entry AE.
%%
%%  Note, that for a FILLS term the value of Obj's slot newfiller
%%  is determined by the call of a5obj_unify_nf/4.
%%
%% FFS: TriggerObj ?
%% FFS: Is Term a single Term or a list ??
%% 	(t5par_send_conc_term(Term, NewNF0, NewNF))


a5sch_process_tell_job(Term, _Trigger, Dep, ONFsel, Obj0,Obj, AE0,AE, S0,S) :-
	b5nf_create_conc_nf(NewNF0),  %% means: t5conc_new_conc(NewNF0),
	b5par_tt_send_conc(Term, NewNF0, NewNF),
	a5obj_unify_nf(ONFsel, NewNF, Obj0, Obj),
	%t5out_trace(tell(Term,Dep,ONFsel,Obj0,Obj)),
	(a5obj_modified_p(ONFsel, Obj, Obj0) ->
	    Jobs = [complete_locally,complete,role_completion,
	            classify, backward_propagate],
	    a5sch_modify_agenda_entry(Jobs, AE0, AE1), 
	    a5sch_add_dependency(Dep, S0,S)
	; %% Obj not modified by caller
	AE0 = AE1, S0 = S),
	a5sch_modify_agenda_entry(role_completion,AE1,AE).


a5sch_recompute_NF(NFsel, Obj0,Obj, AE0,AE,S) :-
	a5sch_previous_nf(NFsel,BasisNFsel),
	a5obj_nf(BasisNFsel, Obj0, BasisNF),
	( NFsel == nfs ->
	    %% initially treat all old fillers again as new
	    b5nf_create_conc_nf(NewNF),
	    a5obj_replace_nf(NFsel, NewNF, Obj0, Obj1),
	    a5obj_unify_nf(NFsel, BasisNF, Obj1, Obj)
	;   a5obj_replace_nf(NFsel, BasisNF, Obj0, Obj)),
	a5obj_id(Obj,ObjId),
	a5sch_recollect_broadcasted_messages(NFsel, ObjId, S, AE0,AE1), 
	a5sch_recompute_NF_subsequent_jobs(NFsel, Jobs),
	%% FFS: if after completion the old NFs is confirmed, 
	%%      then classification may be ommitted !?
	a5sch_modify_agenda_entry(Jobs, AE1, AE).


%% cmk 28.1.93: Optimization that checks whether NFi has changed against NFs
%%  Prevents from a 2nd attempt to apply i-links when already the 1st
%%  application yielded nothing.

a5sch_complete_nfi(Obj0,Obj,AE0,AE) :-
	a5obj_nf(nfi, Obj0, NFi0),
	%% assertz(cmk_ilink(token)),
	b5nf_explicit_supers(NFi0,Supers), 
	i5ibox_complete_nf(NFi0,Supers,NFi), 
	a5obj_nf(nfs, Obj0, NFs),
	(b5nf_modified_p(NFi, NFs) -> 			%% 28.1.93
	    ((b5nf_modified_p(NFi, NFi0);a5obj_new_fillers_p(Obj0)) -> 
		a5obj_id(Obj0,ID),
		t5out_trace(ibox_result(ID,NFi0,NFi)),
		a5obj_unify_nf(nfi, NFi, Obj0, Obj),
		Jobs = [complete,classify,backward_propagate],
		a5sch_modify_agenda_entry(Jobs, AE0, AE1) 
	    ; AE1 = AE0, Obj = Obj0 ),
	    a5sch_modify_agenda_entry(role_completion,AE1,AE)
	;   AE = AE0, a5obj_reset_new_fillers(Obj0,Obj)).


%% a5sch_recompute_NF_subsequent_jobs(+NFsel,-SubsequentJobs)
%%  FFS: check if role_completion is req'ed in 2nd clause

a5sch_recompute_NF_subsequent_jobs(nfs,
	[complete,role_completion,classify,backward_propagate]).
a5sch_recompute_NF_subsequent_jobs(nfi,[complete_nfi,role_completion]).


a5sch_finalize_agenda_processing(S) :-
	t5out_info(a5sch_finalize_agenda_processing),
	a5sch_retract_invalid_dependencies(S),
	a5sch_scheduler(dependencies, S, Deps),
	a5dep_dependencies_store(Deps),
	a5sch_finalize_index_structure(S),
	a5odb_tmp_init.

a5sch_finalize_index_structure(S) :-
	a5sch_blackboard_objects(S, BBObjs),
	(a5sch_retraction_mode_p(S) ->
	    a5ind_modify(BBObjs)
	; a5ind_insert(BBObjs)).


a5sch_reset_old_state(Scheduler) :-
	a5sch_eliminate_new_objects(Scheduler),
	a5odb_undo.

a5sch_eliminate_new_objects(Scheduler) :-
	a5sch_blackboard_objects(Scheduler, Objs),
	a5odb_remove_objects_without_tmp_copy(Objs).



%%
%%  A G E N D A
%%

%% The Agenda holds the Jobs to be processed for particular objects.  The
%% structure is 'agenda(Agenda, PostponedStuff)' where Agenda is the main
%% agenda, and PostponedStuff contains things that are postponed for
%% processing in later phases. At the moment (Jan 93) PostponedStuff only
%% contains objects whose processing is postponed until the NFi phase.

%% a5sch_make_agenda(?Agenda)
%%  True if Agenda is a truly empty, new Agenda. 

a5sch_make_agenda(agenda([], [])).


%% a5sch_agenda_done(+Agenda).
%%  True if Agenda has been processed completely.
%%  There may be postponed Jobs for a next phase, though.

a5sch_agenda_done(agenda([],_)).


%% a5sch_add_to_agenda(+NewEntry, +Scheduler, -NewScheduler)
%% Adds NewEntry to the agenda of Scheduler and returns the
%% resulting NewScheduler. NewEntry may be one of the following:
%% - complete_locally(+Obj)
%% - complete(+Obj)
%% - tell(+AffectedObj, +TriggerObj, +Term, +Dependency)
%%	where Term is given according to Kernel Interface
%%	and Dependency is given according to Dependency Module specification.
%% - backpointer(+FillerObj, +FilledObj, +Role)
%% - backward_propagate(+Obj)
%% - classify(+Obj)
%% - postpone_until_NFi(+Obj)
%% - a List of NewEntries
%%
%% Chosen strategy: If Obj is already on the agenda the corresponding
%%  entry is removed from the agenda, is then modified, and finally 
%%  added again. Otherwise a new agenda entry is created and added.
%%  In any case, new entries are added and processed in LIFO strategy (stack).

a5sch_add_to_agenda([], S,S) :- !.
a5sch_add_to_agenda([Job|Jobs],S0,S) :- !,
	a5sch_add_to_agenda(Job, S0,S1),
	a5sch_add_to_agenda(Jobs, S1,S).
a5sch_add_to_agenda(Job, S0,S) :-
	a5sch_chk_job(Job, ObjID, InternalJob),
	a5sch_scheduler_replace(agenda,S0,Agenda,S,NewAgenda),
	a5sch_add_to_agenda(InternalJob, ObjID, Agenda, NewAgenda).

a5sch_add_to_agenda(postpone_until_NFi, ObjID, agenda(A,Later0),agenda(A,Later)) :-
	!, %% RED %%
	add_element(ObjID,Later0,Later).
a5sch_add_to_agenda(Job, ObjID, agenda(Agenda,Later), NewAgenda) :-
	select(ObjID-AgendaEntry, Agenda, Residue),
	!, %% RED %%
	NewAgenda = agenda([ObjID-NewAgendaEntry | Residue],Later),
	a5sch_modify_agenda_entry(Job, AgendaEntry, NewAgendaEntry).
a5sch_add_to_agenda(Job, ObjID, agenda(Agenda,Later), NewAgenda) :-
	%% not select(ObjID-AgendaEntry, Agenda, Residue),
	NewAgenda = agenda([ObjID-NewAgendaEntry | Agenda], Later),
	a5sch_make_agenda_entry(Job, NewAgendaEntry).

%% a5sch_add_objects_to_agenda(+ObjIdList,+InternalJob,+Agenda,-NewAgenda)

a5sch_add_objects_to_agenda([],_,A,A).
a5sch_add_objects_to_agenda([ObjID|ObjIDs],InternalJob,A0,A) :-
	    a5sch_add_to_agenda(InternalJob, ObjID, A0,A1),
	    a5sch_add_objects_to_agenda(ObjIDs,InternalJob,A1,A).


%% a5sch_chk_job(+Job, -Obj, InternalJob) 
%%  Succeeds if Job is a job as handled by Scheduler, if Obj
%%  is the object affected by Job, and if InternalJob is
%%  the internal representation of Job.
%%  Otherwise fails and raises an error.

a5sch_chk_job(Job, ObjID, InternalJob) :-
	a5sch_correct_job(Job, ObjID, InternalJob), 
	!. %% RED %%
a5sch_chk_job(Job, _, _) :-
	t5out_error(faulty_job(Job)), 
	fail.

a5sch_correct_job(complete_locally_NFu(O), O, complete_locally_NFu).
a5sch_correct_job(recompute_NF(O), O, recompute_NF).
a5sch_correct_job(complete_locally(O), O, complete_locally).
a5sch_correct_job(complete(O), O, complete).
a5sch_correct_job(tell(O,X,Y,Z), O, tell(X,Y,Z)).
a5sch_correct_job(backpointer(O,X,Y), O, backpointer(X,Y)).
a5sch_correct_job(backward_propagate(O), O, backward_propagate).
a5sch_correct_job(classify(O), O, classify).
a5sch_correct_job(role_completion(O),O,role_completion).
a5sch_correct_job(postpone_until_NFi(O),O,postpone_until_NFi).


%% a5sch_next_agenda_job(+Agenda, -Job, -NewAgenda)
%%  NewAgenda is Agenda without the next Job 
%%  where Job is ObjID-AgendaEntry.

a5sch_next_agenda_job(agenda([Job|Agenda],Later), Job, agenda(Agenda,Later)).

%% a5sch_objects_on_agenda(+Agenda,-ObjIds) 
%%  True if ObjIds is the list of IDs of objects on Agenda.

a5sch_objects_on_agenda(A0, ObjIds0) :-
	(a5sch_agenda_done(A0) ->
	    ObjIds0 = []
	; a5sch_next_agenda_job(A0, ObjId-_, A),
	    ObjIds0 = [ObjId|ObjIds],
	    a5sch_objects_on_agenda(A,ObjIds)).


%%
%% Agenda Entries
%%  Jobs are assumed to be in internal representation, i.e.,
%%  without the first arg (AffectedObj).
%%  An Agenda Entry is of the form
%%      agenda_entry(TellList, RoleFillersList, Flags)
%%  where TellList is a list of tell(TriggerObj, Term, Dependency),
%%  RoleFillersList is a keysorted list of pairs Role-FillerList,
%%  and Flags are Agenda Entry Flags as described below.
%%

%% a5sch_make_agenda_entry(Job, NewAgendaEntry).
%%  True if NewAgendaEntry is a new agenda entry containing job.

a5sch_make_agenda_entry(tell(Trigger, Term, Dep), Entry ) :- !, %% RED %%
	Entry = agenda_entry([tell(Trigger, Term, Dep)], [], Flags),
	a5sch_make_agenda_entry_flags(Flags).

a5sch_make_agenda_entry(backpointer(ToObj, Role), Entry ) :- !, %% RED %%
	Entry = agenda_entry([], [Role-[ToObj]], Flags),
	a5sch_make_agenda_entry_flags(Flags).

a5sch_make_agenda_entry(Job, agenda_entry([],[],Flags)) :-
	%% not Job in [tell, backpointer]
	a5sch_make_agenda_entry_flags(Flags0),
	a5sch_set_agenda_entry_flag(Job, Flags0, Flags).


%% a5sch_agenda_entry_done(+AgendaEntry)
%%  True if AgendaEntry has been processed entirely.

a5sch_agenda_entry_done(agenda_entry([],[],Flags) ) :-
	a5sch_agenda_entry_flags_done(Flags).


%% a5sch_modify_agenda_entry(+Job, +AgendaEntry, -NewAgendaEntry).
%%  True if NewAgendaEntry is AgendaEntry modified by adding Job.
%%  ATTENTION: Job may also be a list of jobs which have to be flags.

a5sch_modify_agenda_entry(tell(Trigger, Term, Dep), 			  
			  agenda_entry(T0,BP,F), agenda_entry(T,BP,F)) :-
	!,  %% RED %%
	add_element(tell(Trigger, Term, Dep),T0,T).
a5sch_modify_agenda_entry(backpointer(O,R), 
			  agenda_entry(T,BP0,F), agenda_entry(T,BP,F)) :-
	!,  %% RED %%
	(selectchk(R-Objs0, BP0, BP1) ->
	     ord_add_element(Objs0, O, Objs),
	     ord_add_element(BP1, R-Objs, BP)
	; ord_add_element(BP0, R-[O], BP)).
a5sch_modify_agenda_entry(Job, agenda_entry(T,BP,F0), agenda_entry(T,BP,F)) :-
	a5sch_set_agenda_entry_flag(Job, F0, F).


%% a5sch_agenda_entry_next(+Phase, +AgendaEntry, -Next, -AgendaEntryResidue)
%%  Determines the Next agenda entry element for the current Phase.
%%  Fails if there is none.
a5sch_agenda_entry_next(tells, agenda_entry(Ts,BP,F), Next, AE) :-
	!, %% RED %%
	(Ts = [] -> fail; Ts = [Next|Tail], AE = agenda_entry(Tail,BP,F)).
a5sch_agenda_entry_next(backpointer, agenda_entry(Ts,BP,F), Next, AE) :-
	!, %% RED %%
	(BP = [] -> fail
	; BP = [R-Objs|Tail],
	  AE = agenda_entry(Ts, Tail, F),
	  Next = backpointer(R,Objs) ).
a5sch_agenda_entry_next(Flag, agenda_entry(Ts, BP, Flags0), 
			Flag, agenda_entry(Ts, BP, Flags ) ) :-
	a5sch_agenda_entry_flags_set_p(Flag, Flags0),
	a5sch_reset_agenda_entry_flag(Flag, Flags0, Flags).


%%
%% Agenda Entry Flags
%%  Flags are represented using library(bitsets).
%% 

%% #a5sch_agenda_entry_flags_flag_no(?Flags,?No)
%%  The supported flags and the corresponding bit number.
%% ATTENTION: The cut in the clause for flag complete_locally_NFu
%%  ensures this to work as an enumerator that stops 
%%  after having enumerated the ordinary flags.

a5sch_agenda_entry_flags_flag_no(role_completion,      8).
a5sch_agenda_entry_flags_flag_no(complete_nfi,         7).
a5sch_agenda_entry_flags_flag_no(initialize_nfi,       6).
a5sch_agenda_entry_flags_flag_no(recompute_NF,         5).
a5sch_agenda_entry_flags_flag_no(classify,             4).
a5sch_agenda_entry_flags_flag_no(backward_propagate,   3).
a5sch_agenda_entry_flags_flag_no(complete,             2).
a5sch_agenda_entry_flags_flag_no(complete_locally,     1).
a5sch_agenda_entry_flags_flag_no(complete_locally_NFu, 0) :- !. %% RED %%
a5sch_agenda_entry_flags_flag_no([],[]).
a5sch_agenda_entry_flags_flag_no([F|Fs], [N|Ns]) :-
	a5sch_agenda_entry_flags_flag_no(F,N),
	a5sch_agenda_entry_flags_flag_no(Fs,Ns).


a5sch_make_agenda_entry_flags(Flags) :- list_to_bitset([],Flags).

a5sch_agenda_entry_flags_done(Flags) :- bitset_to_list(Flags,[]).

a5sch_set_agenda_entry_flag(Flag, Flags0, Flags) :-
						  % Flag may be a list too !
	a5sch_agenda_entry_flags_flag_no(Flag, FlagNo),
	(atom(Flag) ->
	     bit_add_element(Flags0, FlagNo, Flags)
	;    list_to_bitset(FlagNo,FlagNoBitset),
	     bit_union(FlagNoBitset, Flags0, Flags)).

%% if Flags is instantiated and Flag isn't, all set flags are enumerated
a5sch_agenda_entry_flags_set_p(Flag, Flags) :-
	a5sch_agenda_entry_flags_flag_no(Flag, FlagNo),
	bit_memberchk(FlagNo, Flags).

a5sch_reset_agenda_entry_flag(Flag, Flags0, Flags) :-
	a5sch_agenda_entry_flags_flag_no(Flag, FlagNo),
	bit_del_element(Flags0, FlagNo, Flags).


%%
%% B L A C K B O A R D
%%  Holds IDs of all objects currently processed
%%

%%  For each object, the blackboard holds its modification status.  This
%%  is esp. important for the NFi phase to decide which objects have been
%%  changed in the last round and have to be fed into another round of
%%  i-link application.
%%  The modification flag is set when for an unmodified object the normal
%%  form is determined as changed (compared to the one stored up to that
%%  point).  It is reset at the beginning of a new round by
%%  a5sch_some_objects_have_been_modified/3.

%% FFS: If the implementation is changed such that the blackboard holds
%%  entire objects (rather than their Ids), all calls of a5obj_fetch
%%  must be changed into something like a5sch_blackboard_fetch which
%%  then would get the desired object from the blackboard.

%% a5sch_object_store(+Object, +Scheduler, -NewScheduler)
%% a5sch_object_store(+ObjId, +Object, +Scheduler, -NewScheduler)
%%  Adds ObjId of Object onto the blackboard of Scheduler
%%  and returns the resulting NewScheduler.
%%  This involves also to make a temp. copy!
%% FFS: For a new Obj a superfluous tmp copy is made ...

a5sch_object_store(ObjId, Obj, S0,S) :-
	a5sch_scheduler_replace(blackboard,S0,BB0,S,BB),
	a5sch_scheduler(nf_selector,S0,NFsel),
	a5sch_blackboard_obj_store(ObjId,Obj,NFsel,BB0,BB).
a5sch_object_store(Obj, S0,S) :-
	a5sch_scheduler_replace(blackboard,S0,BB0,S,BB),
	a5obj_id(Obj, ObjId),
	a5sch_scheduler(nf_selector,S0,NFsel),
	a5sch_blackboard_obj_store(ObjId,Obj,NFsel,BB0,BB).

a5sch_blackboard_obj_store(ObjId,Obj,NFsel,BB0,BB) :-
	t5out_info(a5sch_suspend_object(ObjId)),
	a5sch_modified_status(Modified),
	(selectchk(ObjId-Status,BB0,BB1) ->
	    (Status == Modified ->
		BB0 = BB
	    ;%% Status \== Modified,
	        a5sch_blackboard_add_obj(ObjId,Obj,NFsel,BB1,BB))
	; a5sch_blackboard_add_obj(ObjId,Obj,NFsel,BB0,BB),
	    a5objid_tmp_copy(ObjId)),
	    %% FFS: a5objid_old_vr_patch(ObjId),
	a5obj_store(Obj).

%% add obj ObjId with correct status (m,u) onto BB.
a5sch_blackboard_add_obj(ObjId,Obj,NFsel,BB0,BB) :-
	(a5obj_fetch(ObjId,Obj0) ->
	    (a5obj_modified_p(NFsel,Obj0,Obj) ->
		a5sch_modified_status(Modified),
		ord_add_element(BB0,ObjId-Modified,BB)
	    ;   a5sch_unmodified_status(UnModified),
	        ord_add_element(BB0,ObjId-UnModified,BB))
	; a5sch_modified_status(Modified),
	    ord_add_element(BB0,ObjId-Modified,BB)).


%% a5sch_blackboard_objects(+Scheduler, -ObjIdList)

a5sch_blackboard_objects(Scheduler, ObjIdList) :-
	a5sch_scheduler(blackboard,Scheduler,BB),
	a5sch_blackboard_objs(BB,ObjIdList).

a5sch_blackboard_objs([],[]).
a5sch_blackboard_objs([O-_|BB], [O|L]) :-
	a5sch_blackboard_objs(BB,L).


%% a5sch_some_objects_have_been_modified(+Scheduler,-ResetScheduler,-ModObjs)
%%  ModObjs is a sorted list of ObjIds for objects that had been marked as
%%  modified. The Scheduler is reset, i.e., after this call all objects
%%  are again marked as unmodified.
%% FFS: who marks object as 'modified' on the Scheduler ?? 
%%  Answer: Currently a5sch_object_store

a5sch_some_objects_have_been_modified(S0,S,L) :-
	a5sch_scheduler_replace(blackboard,S0,BB0,S,BB),
	a5sch_blackboard_modified_objects(BB0, BB, L),
	L \== [].

a5sch_blackboard_modified_objects([],[],[]).
a5sch_blackboard_modified_objects([O-Status|BB0],[O-NewStatus|BB],Os1) :-
	a5sch_unmodified_status(NewStatus),
	(a5sch_modified_status(Status) ->
	    Os1 = [O|Os] ; Os1 = Os),
	a5sch_blackboard_modified_objects(BB0,BB,Os).

a5sch_modified_status(m).
a5sch_unmodified_status(u).


%% a5sch_activate_postponed_objects(+Scheduler, -NewScheduler, -PostponedObjs)
%%  Moves objects from the agenda's postponed_objects data structure
%%  to the blackboard.
%%  At the moment, this is a dedicated hack to support ABox Update
%%  after TBox/IBox changes. Objects are postponed to bypass the NFs
%%  processing. If a postponed object gets involved during NFs processing
%%  it will be NFs processed, though.
%%
%%  If postponing objects is required in a more general way, this
%%  code has to be worked over !!


a5sch_activate_postponed_objects(S0,S, PostponedObjs) :-
	a5sch_scheduler_replace(agenda,S0,A0,S1,A),
	a5sch_scheduler_replace(blackboard,S1,BB0,S,BB),
	A0 = agenda(Agenda,PostponedObjs),
	A  = agenda(Agenda,[]),
	a5sch_activate_postponed_objs(PostponedObjs, BB0, BB).

a5sch_activate_postponed_objs([],BB,BB).
a5sch_activate_postponed_objs([ObjId|ObjIds], BB0, BB) :-
	a5obj_fetch(ObjId,Obj),
	a5sch_blackboard_obj_store(ObjId,Obj,nfi,BB0,BB1),
	a5sch_activate_postponed_objs(ObjIds, BB1, BB).

	
%%
%% D E P E N D E N C I E S
%%

%% a5sch_add_dependency(+Dependency, +Scheduler, -NewScheduler)
/* %% acc. to Jan a5dep_dependencies_add_entry/3 can handle lists as well
a5sch_add_dependency([],S,S) :- !.
a5sch_add_dependency([D|Ds], S0,S) :- !,
	a5sch_scheduler(dependencies,S0,Deps0),
	a5sch_scheduler_substitute(dependencies,S0,Deps,S),
	a5dep_dependencies_add_entries(Deps0,[D|Ds],Deps).
*/
a5sch_add_dependency(Dep, S0,S) :-
	%% a5dep_dependency_p(Dep),
	a5sch_scheduler_replace(dependencies,S0,Deps0,S,Deps),
	t5out_trace(a5sch_add_dependency(Dep)),
	a5dep_dependencies_add_entry(Deps0,Dep,Deps).


%%
%% R E T R A C T I O N   M O D E
%%

%% a5sch_prepare_retraction(+NFsel, +Scheduler, -NewScheduler)
%%  Preparation of special mode for dealing with non-monotonic changes
%%  of the KB: 
%%   i) setup corresponding structure, 
%%  ii) foreach i in literally changed objects
%%      - complete_locally_NFu
%%      - determine i's depending objects
%%      - issue a recompute_NF request
%%  iii) add all depending objects to Agenda, issueing for each a
%%      recompute_NF request
%%  iv) foreach i on Agenda (= literally changed objects and depending objects)
%%      - determine backpointer information, add it to Scheduler
%%
%% Note: recompute_NF is only requested once before the acc. NFsel-phase
%%  is entered. In the scheduler, for each (i-link) round, recompute_NF
%%  is the Initial_Job, but isn't run again since it is not requested again!

a5sch_prepare_retraction(NFsel, S0,S) :-
	a5sch_preprocess_objects_already_on_blackboard(NFsel,S0,S1), 
	a5sch_scheduler(depending_objects,S1,DepObjs),
	a5sch_scheduler_replace(agenda,S1,A1,S2,A2),
	a5sch_add_objects_to_agenda(DepObjs,recompute_NF,A1,A2),
	a5sch_objects_on_agenda(A2,Objs),
	a5sch_scheduler_substitute(backpointers,S2,BPs,S3),
	a5sch_determine_reduced_backpointer(Objs,NFsel,DepObjs,BPs), %% FFS
	a5sch_substitute_invalid_nfs(NFsel, Objs, S3,S).


%% a5sch_preprocess_objects_already_on_blackboard(+NFsel,+S0,-S)

a5sch_preprocess_objects_already_on_blackboard(nfs, S0,S) :-
	a5sch_preprocess_literally_changed_objects(S0,S).
a5sch_preprocess_objects_already_on_blackboard(nfi, S0,S) :-
	a5sch_blackboard_objects(S0,Objs),
	a5sch_preprocess_objs_on_blackboard(Objs, nfi, S0,S).


%% FFS: use diff-lists to determine DepObjs; sofar I used append/3
%%  since as a Quintus built-in it is nearly as fast as a implementation
%%  using diff-lists.

a5sch_preprocess_literally_changed_objects(S0,S) :-
	a5sch_scheduler(agenda,S0,A0),
	(a5sch_agenda_done(A0) ->  S=S0
	; a5sch_next_agenda_job(A0,J0,A1),
	    a5sch_scheduler_substitute(agenda,S0,A1,S1),
	    a5sch_preprocess_literally_changed_object(J0,J1,S1,S2),
	    a5sch_scheduler_replace(agenda,S3,A2,S,A),
	    a5sch_next_agenda_job(A,J1,A2),
	    a5sch_preprocess_literally_changed_objects(S2,S3)
	).


%% a5sch_preprocess_literally_changed_object(+Job,-NewJob, +S0,-S)
%%  - complete_locally_NFu
%%  - determine depending objects and negative (obsolet) dependencies

a5sch_preprocess_literally_changed_object(ObjId-AE0,ObjId-AE,S0,S) :-
	a5obj_fetch(ObjId, Obj0),
	a5obj_complete_locally(nfu, Obj0, Obj),
	not(a5obj_incoherent_p(nfu, Obj)),
	a5sch_agenda_entry_next(complete_locally_NFu, AE0, _, AE1),
	a5sch_modify_agenda_entry(recompute_NF, AE1,AE),
	a5sch_determine_depending_objects(ObjId, nfs, S0,S).


%% a5sch_preprocess_objs_on_blackboard(+ObjsOnBB, +NFsel, +S0,-S).
%%  For all NFsel but 'nfu' preprocessing consists of determining
%%  the depending objects.
%%  Note: ObjsOnBB still have to be put on Agenda !

a5sch_preprocess_objs_on_blackboard([],_,S,S).
a5sch_preprocess_objs_on_blackboard([O|Os], NFsel, S0,S) :-
	a5sch_determine_depending_objects(O,NFsel, S0,S1),
	a5sch_preprocess_objs_on_blackboard(Os, NFsel, S1,S).


%% a5sch_determine_depending_objects(+ObjId, +NFsel, +S0,-S)
%%  Determines the Ids of objects depending on ObjId, 
%%  and the Negative-Dependencies that have to be deleted from
%%  the dependency graph at a successful end of the ongoing
%%  retraction transaction. Both is returned via the Scheduler.

a5sch_determine_depending_objects(ObjId, NFsel, S0,S) :-
	a5dep_object_depending_objects(NFsel,ObjId, DepObjs0, NegDeps0),
	a5sch_scheduler_replace(depending_objects,S0,DepObjs1,S1,DepObjs),
	a5sch_scheduler_replace(negative_dependencies,S1,NegDeps1,S,NegDeps),
	ord_union(DepObjs0,DepObjs1,DepObjs2),
	ord_add_element(DepObjs2,ObjId,DepObjs),
	ord_union(NegDeps0,NegDeps1,NegDeps).


%% a5sch_determine_reduced_backpointer(+Objs,+NFsel,+DepObjs,-RedBackPointer)
%%  foreach i in Objs:
%%   - get i's fillers
%%   - when transforming to backpointer datastructure eliminate
%%     i's literal fillers and any of DepObjs
%%  DepObjs are not relevant since revision will process them anyway,
%%  and thus they will re-broadcast whatever i had received by them;
%%  i's literal fillers are not relevant since when recomputing the NFs
%%  every filler relationship will be told as if new.
%%
%% cmk 5.2.93: The optimization for i's literal fillers is only true,
%%  if some k is a literal filler for i, but i is not also a literal
%%  filler for the same role at k.
%%  Example: cmkrev(4) :-
%%  	foo1 :: inv(r): foo2 and named: 'test',
%%  	foo2 :: c1 and r: foo1, %% c1 ?< all(r,d1),
%%  	foo1 ?: d1,
%%  	backtell(forget(foo1 :: named: 'test')),
%%  	foo1 ?: d1.
%%  foo1 re-tells its literal relation inv(r):foo2. The salient point
%%  is that this would not change foo2 (r:foo1 already holds in NFu(foo2)),
%%  and thus d1 would not be propagated anew to foo1.

a5sch_determine_reduced_backpointer([],_,_,[]).
a5sch_determine_reduced_backpointer([O|Os], NFsel,DepObjs, [O-BP|BPs]) :-
	a5obj_fetch(O, Obj),
	a5obj_nf(nfu,Obj,NFu),
	a5obj_nf(NFsel,Obj,NF), 
	a5sch_reduced_backpointer(O, NF,NFu,DepObjs,BP),
	a5sch_determine_reduced_backpointer(Os,NFsel,DepObjs,BPs).


%% a5sch_reduced_backpointer(+O,+NF,+NFu,+DepObjs,-BP) 
%% FFS: Module structure violated
%%  BP is sorted list of elements F-R where O is filler at F for R.

a5sch_reduced_backpointer(O,NF,NFu,DepObjs,BPs) :-
	b5nf_reslist(NF,RL), t5rl_ress(RL,ResList),
	b5nf_reslist(NFu,URL),
	a5sch_reduced_backpointer1(ResList,O,URL,DepObjs,[],BPs).

a5sch_reduced_backpointer1([],_,_,_,BPs,BPs).
a5sch_reduced_backpointer1([Res|ResList],ObjId,UResList,DepObjs,BPs0,BPs) :-
	t5res_role(Res,R), t5role_inv_role(R,InvR),
	( number(InvR) ->   %% FFS: not 'not_exists', prims, etc.
	    t5res_fillers(Res,Fs0),
	    ord_subtract(Fs0, DepObjs, Fs1), %% remove DependingObjects
	    t5rl_fillers(UResList,R,UFs),
	    a5sch_exclusive_literal_fillers(ObjId,InvR,R,UFs,XUFs),
	    ord_subtract(Fs1, XUFs, Fs2), %% remove literal fillers
	    a5sch_add_reduced_backpointer(Fs2,InvR,BPs0,BPs1)
	; BPs1 = BPs0),
	a5sch_reduced_backpointer1(ResList,ObjId,UResList,DepObjs,BPs1,BPs).


a5sch_add_reduced_backpointer([],_,BPs,BPs).
a5sch_add_reduced_backpointer([F|Fs],R,BPs0,BPs) :-
	ord_add_element(BPs0,F-R,BPs1),
	a5sch_add_reduced_backpointer(Fs,R,BPs1,BPs).

%% a5sch_exclusive_literal_fillers(+ObjId,+Role,+InvRole,+LiteralFillers,-ExplicitLFs)
%%  Returns those of ObjId's LiteralFillers for InvRole, where
%%  ObjId is not a literal filler for Role.
%%  Panics if Role is notexistant.

a5sch_exclusive_literal_fillers(O, R, InvR, Fs, XFs) :-
	(number(R) -> %% is R existant ?
	    a5sch_excl_literal_fillers(Fs,O,R,XFs)
	;t5out_panic(a5sch_exclusive_literal_fillers(
					non_existant_inv_role_for(InvR))),
	    !, fail).

a5sch_excl_literal_fillers([], _, _, []).
a5sch_excl_literal_fillers([F|Fs], O, R, XFs0) :-
	a5objid_fillers(nfu,F,R,LFs),
	(memberchk(O,LFs) ->   %% Is O literal filler of R at F ?
	    XFs0 = XFs 
	;   XFs0 = [F|XFs] ),
	a5sch_excl_literal_fillers(Fs,O,R,XFs).


%% a5sch_substitute_invalid_nfs(+NFsel,+Objs,+S0,-S)
%%  This is required because objects are re-recognized one after another.
%%  If not all NFs's are reset in an initial step, some object
%%  x may abstract information based on y's NFs that hadn't been reset
%%  properly.
%% FFS: Why isn't this done during the initial job of the NFsel phase?
%%  For 'nfi' this would be OK; but for 'nfs' for some reasons
%%  this pred seems to be required.
%%  For 'nfi' an error situation might occur if at some object an
%%  b5nf_delta is invoked and the new NFi is not more special then
%%  the old NFi which should have been overwritten here.

a5sch_substitute_invalid_nfs(NFsel,Objs,S0,S) :-
	a5sch_previous_nf(NFsel,BasisNFsel),
	a5sch_substitute_invalid_nfs(Objs,NFsel,BasisNFsel,S0,S).

a5sch_substitute_invalid_nfs([],_,_,S,S).
a5sch_substitute_invalid_nfs([O|Os], NFsel,BasisNFsel, S0,S) :-
	a5obj_fetch(O,Obj0),
	a5obj_nf(BasisNFsel, Obj0, BasisNF),
	a5obj_replace_nf(NFsel, BasisNF, Obj0, Obj),
	a5sch_object_store(Obj, S0,S1),   %% FFS: just an a5obj_store/1 ?
	a5sch_substitute_invalid_nfs(Os, NFsel,BasisNFsel, S1,S).

%% --- end of a5sch_prepare_retraction. ---	

%% a5sch_filled_objects_before_redescription(+ObjId,+Scheduler,-ObjIdList)
%%  ObjIdList is - acc. to Scheduler - the sorted lists of objects where
%%  ObjId is filler. 
%%  If not in retraction mode the result is [].
%%  Note: The reduced backpointers do not include all inv-fillers,
%%  therefore we added also the DepObjs. Some of them may not have
%%  had ObjId as a filler, so check again!!

a5sch_filled_objects_before_redescription(ObjId,S,L) :-
	(a5sch_scheduler(backpointers,S,BPs) ->
	    (memberchk(ObjId-BP,BPs) ->
		a5sch_objects_pointed_to(BP,L0); L0=[]),
	    a5sch_scheduler(depending_objects,S,DepObjs),
	    ord_union(DepObjs,L0,L1),
	    ord_del_element(L1,ObjId,L)
	;   %% not in retraction mode 
	    L=[]).

a5sch_objects_pointed_to([],[]).
a5sch_objects_pointed_to([O-_|T0],[O|T]) :-
	a5sch_objects_pointed_to(T0,T).


%% a5sch_object_backpointer(+ONFsel, +ObjId, +S, -BPs) 
%% FFS: in the future there will be multiple backpointer data
%%   for the different ONFs.

a5sch_object_backpointer(_, ObjId, S, BPs) :-
	a5sch_scheduler(backpointers,S,BPList),
	memberchk(ObjId-BPs,BPList).


%% a5sch_recollect_broadcasted_messages(+ONFsel, +ObjId, +S +AE0,-AE1),
%%  Collects information that previously had been broadcasted forward
%%  and was lost when the NFs was deleted. Up to now this is:
%%  i) VRs propagated to Obj0, ii) requests to add a inverse filler
%%  relationship, i.e., a tell(Obj, Trigger, fills(inv(R),[Trigger]), Dep)
%%  with Dep = dep(Obj,Trigger, cinst(Trigger,fills(R,[Obj]))).

a5sch_recollect_broadcasted_messages(ONFsel, ObjId, S, AE0, AE) :-
	a5sch_object_backpointer(ONFsel, ObjId, S, BPs),
	a5sch_factorize_backpointer(BPs,AdjBPs),
	a5rec_recollect_broadcasted_msgs(AdjBPs,ONFsel,ObjId,AE0,AE).

	
%% FFS: Generate BPs in the correct way in the first place.

a5sch_factorize_backpointer([],[]).
a5sch_factorize_backpointer([F-R|BPs],[F-[R|Rs]|AdjustedBPs]) :-
	a5sch_factorize_backpointer_residues(BPs,F,Rs,BPsWithoutF),
	a5sch_factorize_backpointer(BPsWithoutF,AdjustedBPs).
	
a5sch_factorize_backpointer_residues([],_,[],[]).
a5sch_factorize_backpointer_residues([G-R|BPs0],F,Rs0,BPs) :-
	( F == G ->
	    Rs0 = [R|Rs],
	    a5sch_factorize_backpointer_residues(BPs0,F,Rs,BPs)
	;   Rs0=[], BPs=[G-R|BPs0]).




%% a5sch_retract_invalid_dependencies(+Scheduler)
%%  takes from Scheduler list of dependencies to be
%%  retracted from dependency graph

a5sch_retract_invalid_dependencies(S) :- 
	a5sch_scheduler(negative_dependencies,S,NegDeps),
	!, a5dep_dependencies_delete(NegDeps).
a5sch_retract_invalid_dependencies(_).

/*
%%%% Ablauf bei Agenda-Vorbereitung NFi Phase Retraction Mode
%% --- a5sch_retraction_mode_p(S1) ->
a5sch_prepare_agenda_processing(nfi, S0, S)
  a5sch_prepare_retraction(nfi,S1,S)
    a5sch_preprocess_objects_already_on_blackboard(NFsel,S0,S1), 
      a5sch_preprocess_objs_on_blackboard(Objs, nfi, S0,S)
        a5sch_determine_depending_objects(O,NFsel, S0,S1)
    /a5sch_scheduler(depending_objects,S1,DepObjs),/
    /a5sch_scheduler_replace(agenda,S1,A1,S2,A2),/
    a5sch_add_objects_to_agenda(DepObjs,recompute_NF,A1,A2),
    /a5sch_objects_on_agenda(A2,Objs),/
    /a5sch_scheduler_substitute(backpointers,S2,BPs,S3),/
    a5sch_determine_reduced_backpointer(Objs,NFsel,DepObjs,BPs), %% FFS
    a5sch_substitute_invalid_nfs(NFsel, Objs, S3,S).


*/






% LAST EDIT: Thu Jul  2 15:07:24 1992 by Mirjam Kuehne (mitropa!mir) 
% LAST EDIT: Tue Jun 30 14:49:14 1992 by Mirjam Kuehne (madonna!mir) 

%% ********************************************************************
%%             Subject: ABox V5 - abox_ve.pro
%% ********************************************************************
 
 
%-cmk- :-op(700, xfy, ~=).
%-cmk- :-op(700, xfy, ~==).
%-cmk-  
%-cmk- 
%-cmk- ~=(X,Y) :- not(X = Y) .
%-cmk-  
%-cmk- ~==(X,Y) :- not(X == Y) .
 
%% ********************************************************************
%%
%%                   V a l u e     E x p r e s s i o n s
%%
%% ********************************************************************
%% Value expression are a boolean expressions of UC's, attributes, or
%% cardinalties.
%%
%% Note:  This implementation handles disjunctions in value
%%        expressions.  However, disjunctions are not handled
%%        by the code for roleinst, therefore disjunctions are not
%%        allowed to be asserted in the ABox.
%% ********************************************************************
%% ********************************************************************
 

%% *********************************************************************
%%                   Unnormalized Value Expressions
%% *********************************************************************
%% A unnormalized value expression must conform to the following syntax
%%  UnNormVe =  Value | 
%%              UnNormVe and UnNormVe |
%%              UnNormVe or UnNormVe  |
%%            * someknown(BQ,UnNormReSeq) |
%%            * allknown(BQ,UnNormReSeq) |
%%            * theknown(BQ,UnNormReSeq) |
%%              close(UnNormVe) 
%%  Value   =   PrologInteger (ObjId | Attr | IntegerInst | StringInst)
%% *********************************************************************


%% *********************************************************************
%%           N o r m a l i z e d   V a l u e   E x p r s
%% *********************************************************************
%% A normalized value expression is in reduced disjunctive form and is
%% consistent.  They are represented using the following domains.
%%
%% Domains:
%%     Vexpr   =   ve(Vterms, Card, VeType)
%%     Vterm   =   vt(Values, Card)
%%     VeType  =   closed | open
%% *********************************************************************
 
%% Basic Predicates
a5ve_ve(ve(_, _, _)).
a5ve_ve_vterms(ve(Vterms, _, _), Vterms).
a5ve_ve_card(ve(_, Card, _), Card).
a5ve_ve_type(ve(_, _, VeType), VeType).
 
a5ve_ve_numvterms(ve(Vterms, _, _), NumVterms) :-
    length(Vterms, NumVterms).
 
a5ve_ve_vterm(ve(Vterms, _, _), Vterm) :- member(Vterm, Vterms).
a5ve_ve_vterm_1(Ve, Vterm) :- a5ve_ve_vterm(Ve, Vterm), !.
 
%% a5ve_ve_definite_one(Ve).
%%  True if Ve is definite an has only 1 value.
 
a5ve_ve_definite_one(ve([vt([_], _)], card(1,1), closed)).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% a5ve_ve(+UnNormVe, -Ve).
%%  Unifies Ve with the value expression constructed from UnNormVe.
 
a5ve_ve(UnNormVe,Selector,Ve) :-
	a5ve_resolve_subqueries(UnNormVe,Selector,UnNormVe1),
	a5ve_dnfve(UnNormVe1,DnfVe),
	a5ve_ve_dnfve_conv(DnfVe, Ve),
	a5ve_ve_chk_consistent(Ve, UnNormVe),
	!.      %% This cut should not be needed, but it makes sure that
                %% all choice points are removed

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% a5ve_resolve_subqueries(UnNormVe, UnNormVe1).
%%  UnNormVe1 is UnNormVe with all subqueries resolved,
%%  i.e., someknown/2 -> someknown/1, etc.
%%  A theknown subquery is checked for uniqueness, 
%%  its result is returned as InstanceKey.

%% mod cmk
a5ve_resolve_subqueries(and(X0,Y0),Selector,and(X,Y)) :-
	!, 
	a5ve_resolve_subqueries(X0,Selector,X),
	a5ve_resolve_subqueries(Y0,Selector,Y).
a5ve_resolve_subqueries(or(X0,Y0),Selector,or(X,Y)) :-
	!, 
	a5ve_resolve_subqueries(X0,Selector,X),
	a5ve_resolve_subqueries(Y0,Selector,Y).
a5ve_resolve_subqueries(close(X0), Selector, close(X)) :-
	!, a5ve_resolve_subqueries(X0,Selector,X).
a5ve_resolve_subqueries(someknown(CPF,UnNRE),Selector,someknown(Instances)) :-
	!, a5ret_retrieve(CPF,UnNRE,Selector,Instances).
a5ve_resolve_subqueries(allknown(CPF,UnNRE),Selector,allknown(Instances)) :-
	!, a5ret_retrieve(CPF,UnNRE,Selector,Instances).
a5ve_resolve_subqueries(theknown(CPF,UnNRE),Selector,TheInstance) :-
	!, a5ret_retrieve(CPF,UnNRE,Selector,Instances),
	%% FFS: make this an extra ret predicate !
	(Instances = [TheInstance] -> 
	     true 
	;    t5out_error(cannot_resolve_uniquely(theknown(CPF,UnNRE))),
	     !, fail).
a5ve_resolve_subqueries(SQ,_,SQ).

%% end cmk


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
%% *********************************************************************
%%                            DnfVe
%% *********************************************************************
%%
%%  A DnfVe is used to represent a value expression in
%%  Disjunctive Normal Form (DNF).
%%
%%  Domains:
%%      DnfVe       =   DnfVterm | or(DnfVterm, DnfVe)
%%      DnfVterm    =   DnfVal | and(DnfVal, DnfVterm)
%%      DnfVal      =   v(Value) | Card
%%                         ^
%%                      Stattdessen: Keys
%%
%% *********************************************************************
 
 
%% a5ve_dnfve(+UnNormVe, -DnfVe).
%%  DnfVe is the disjunctive normal form for the value expresion
%%  represented by UnNormVe.
 
a5ve_dnfve(UnNormVe,DnfVe) :-
	a5ve_make_dnf(UnNormVe,UnNormDnfVe1),
	a5ve_dnfve_dist(UnNormDnfVe1,UnNormVe1),
	a5ve_dnfve_assoc(UnNormVe1, DnfVe).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% a5ve_make_dnf(+UnNormVeWithSubqueries,-UnNormVeWithoutSubqueries)
%%  Expands functors someknown, allknown, and close to simple 
%%  value expressions containing only and/or/card.
%% Patch 23.2.93: Special treatment for someknown/allknown in
%%  case of empty result; fix of close/1.

a5ve_make_dnf(and(Ex1,Ex2),and(X1,X2)) :-
	!,
	a5ve_make_dnf(Ex1,X1),
	a5ve_make_dnf(Ex2,X2).
a5ve_make_dnf(or(Ex1,Ex2),or(X1,X2)) :-
	!,
	a5ve_make_dnf(Ex1,X1),
	a5ve_make_dnf(Ex2,X2).
a5ve_make_dnf(close(X0), X) :-
	!, 
	a5ve_dnfve(X0,X1),
	a5ve_dnfve_make_closed(X1,X).
a5ve_make_dnf(someknown(List),OrExpr) :-
	!, 
	(List == [] ->
	    a5ve_empty_ve(OrExpr)
	;   a5ve_or_list_expr(List,OrExpr)).
a5ve_make_dnf(allknown(List),AndExpr) :-
	!,
	(List == [] ->
	    AndExpr = card(0,in)
	;   a5ve_and_list_expr(List,AndExpr)).
a5ve_make_dnf(DnfVe,DnfVe).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%! a5ve_or_list_expr(List, OrExpr).
%%  True if OrExpr is the or-expression corresponding to List.

a5ve_or_list_expr([A|L], OrExpr) :-
	a5ve_or_list_expr(L,A,OrExpr).

a5ve_or_list_expr([],A,A).

a5ve_or_list_expr([B|L],A,or(A,OrExpr)) :-
	a5ve_or_list_expr(L,B,OrExpr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%! a5ve_and_list_expr(List, AndExpr).
%%  True if AndExpr is the and-expression corresponding to List.

a5ve_and_list_expr([A|L], AndExpr) :-
	a5ve_and_list_expr(L,A,AndExpr).

a5ve_and_list_expr([],A,A).

a5ve_and_list_expr([B|L],A,and(A,AndExpr)) :-
	a5ve_and_list_expr(L,B,AndExpr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

%% mod cmk
%% a5ve_dnfve_dist(+UnNormVe, -Expr).
%%  Apply distributivity laws to UnNormVe1 to create Expr.
%%  Expr will be in disjunctive normal form but its
%%  associtivity may be wrong.
%%
%% cmk 21.04.89: Handling attributes as structured objects.

a5ve_dnfve_dist(and(A,B),R) :-
     !,
     a5ve_dnfve_dist(A,DA),
     a5ve_dnfve_dist(B,DB),
     a5ve_dnfve_and_dist(DA, DB, R).
a5ve_dnfve_dist(or(A, B),or(DA, DB)) :-
     !,
     a5ve_dnfve_dist(A,DA),
     a5ve_dnfve_dist(B,DB).
a5ve_dnfve_dist(UnNormVeKey,UnNormVeKey).

%% end cmk

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 
%% a5ve_dnfve_and_dist(A, B, R).
%% Distribute "and" over "or" in the expression "A and B"
%% to produce R.
 
a5ve_dnfve_and_dist(or(A, B), C, or(Dac, Dbc)) :-
    !,
    a5ve_dnfve_and_dist(A, C, Dac),
    a5ve_dnfve_and_dist(B, C, Dbc).
a5ve_dnfve_and_dist(A, or(B,C), or(Dab, Dac)) :-
    %% Implicit: A ~= or(_,_)
    !,
    a5ve_dnfve_and_dist(A, B, Dab),
    a5ve_dnfve_and_dist(A, C, Dac).
a5ve_dnfve_and_dist(A, B, and(A,B)) :-
    %% Implicit: B ~= or(B,C)
    !.
 
%% a5ve_dnfve_assoc(+Expr, -DnfVe).
%%  Apply associativity laws to Expr to form an expression that is
%%  right associative.
%%  Expr must already have all its "and" terms distributed
%%  over "or" terms.
%%
%% cmk 14.03.89: The associativity laws have to be propagated over the
%%  entire Expr. This is fixed by the 3rd and 4th clause.
%%  See Examples or_tell* and or_ask* in abcddom.mpro.
 
a5ve_dnfve_assoc(and(and(A, B), C), R) :-
     !,
     a5ve_dnfve_assoc(and(A, and(B, C)), R).

a5ve_dnfve_assoc(or(or(A, B), C), R) :-
     !,
     a5ve_dnfve_assoc(or(A,or(B,C)), R).

a5ve_dnfve_assoc(and(A, and(B, C)), and(A, R)) :-
    !,
    a5ve_dnfve_assoc(and(B, C), R).

a5ve_dnfve_assoc(or(A, or(B, C)), or(A, R)) :-
    !,
    a5ve_dnfve_assoc(or(B, C), R).

a5ve_dnfve_assoc(A, A) :-
    %% implicit: A ~= and(and(_,_),_) , A ~= or(or(_,_), _)
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% a5ve_ve_dnfve_conv(+DnfVe, -Ve).
%%  True when Ve is the Vexpr equivalent to DnfVe.

a5ve_ve_dnfve_conv(DnfVe, Ve) :-
	a5ve_ve_create(Ve0), 
	a5ve_ve_dnfve_conv(DnfVe, Ve0,Ve).


%% loop
a5ve_ve_dnfve_conv(or(DnfVterm, DnfVe), Ve0,Ve) :-
	!,
	a5ve_ve_dnfve_conv_unify(DnfVterm, Ve0, Ve1),
	a5ve_ve_dnfve_conv(DnfVe, Ve1, Ve).
a5ve_ve_dnfve_conv(DnfVterm, Ve0,Ve) :-
	a5ve_ve_dnfve_conv_unify(DnfVterm, Ve0, Ve).

%% a5ve_ve_dnfve_conv_unify(+DnfVterm, +Ve0, -Ve)
a5ve_ve_dnfve_conv_unify(DnfVterm, Ve0, Ve) :-
	a5ve_vterm_dnfvterm_conv(DnfVterm, Vterm),
	a5ve_vterm_card(Vterm, NR1),
	a5ve_vterm_type(Vterm, VeType1),
	a5ve_ve_vterms(Ve0, Vterms0),
	a5ve_ve_card(Ve0, NR0),
	a5ve_ve_type(Ve0, VeType0),
	t5nr_generalize(NR0, NR1, NR),
	a5ve_vetype_or(VeType0, VeType1, VeType),
	sort([Vterm | Vterms0], Vterms),
	a5ve_ve(Vterms, NR, VeType, Ve).





%% *********************************************************************
%% a5ve_vterm_dnfvterm_conv(+DnfVterm, -Vterm).
%%  True when Vterm is the equivalent to DnfVterm.
 
a5ve_vterm_dnfvterm_conv(DnfVterm, vt(Values,NR)) :-
	a5ve_dnfvterm_vals(DnfVterm, Values0),
	sort(Values0, Values),
	length(Values, NumValues),
	a5ve_dnfvterm_card(DnfVterm, NR0),
	t5nr_add_min(NR0, NumValues, NR).


%% a5ve_dnfvterm_card(+DnfVterm, -NR).
%%  True when NR is the "and" of all Card values in DnfVterm.
%%  NR is in the representation of t5nr.
 
a5ve_dnfvterm_card(DnfVterm, NR) :-
	t5nr_create(NR0),
	a5ve_dnfvterm_card(DnfVterm,NR0, NR).
 
a5ve_dnfvterm_card(and(X, Y), NR0, NR) :-
	!,
	a5ve_dnfvterm_card(X, NR0, NR1),
	a5ve_dnfvterm_card(Y, NR1, NR).
%% a5ve_dnfvterm_card(NR, NR0, NR) :- %% should never be called !?!
%% 	t5nr_p(NR1),
%% 	!,
%% 	t5nr_unify(NR1,NR0,NR).
a5ve_dnfvterm_card(card(Min,Max), NR0, NR) :-
	!,
	t5nr_add_min(NR0, Min, NR1),
	t5nr_add_max(NR1, Max, NR).
a5ve_dnfvterm_card(_, NR,NR).
 

%% a5ve_ve_create(-AccuVe)
%%  This one is fishy: we pass in-0 as an NR accumulator hoping that
%%  there will be at least one vterm that will make it consistent 8-}
a5ve_ve_create(ve([], NR, open)):-
	t5nr_create(NR0),
	t5nr_add_min(NR0, in, NR1),
	t5nr_add_max(NR1, 0, NR).


%% Basic Preds adds

a5ve_ve(VTerms, NR, Type, ve(VTerms, NR, Type)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% a5ve_dnfvterm_vals(+Dnfvterm, -Vals).
%%  True when Vals is a list of the values in DnfVterm.
%%  Note: Vals contains no duplicates.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a5ve_dnfvterm_vals(DnfVterm, Vals) :-
    a5ve_dnfvterm_vals(DnfVterm, [], Vals).
 
a5ve_dnfvterm_vals(and(X, Y), ValsIn, ValsOut) :-
    !,
    a5ve_dnfvterm_vals(X, ValsIn, Vals1),
    a5ve_dnfvterm_vals(Y, Vals1, ValsOut).

a5ve_dnfvterm_vals(card(_,_), Vals, Vals) :- !.
a5ve_dnfvterm_vals(Val, Vals, Vals) :-
    member(Val, Vals),
    !.

a5ve_dnfvterm_vals(Val,ValsIn,ValsOut) :-
	b5sort_unify([Val],ValsIn,ValsOut).

%%a5ve_dnfvterm_vals(Val, ValsIn, [Val | ValsIn]) :- !. %%mir 2.7.92
    %% Implicit: not( member(Val, ValsIn) ),

 
%% *********************************************************************
%% a5ve_ve_chk_consistent(Ve, UnNormVe).
%%  Raise an error if Ve is not consistent.
 
a5ve_ve_chk_consistent(ve(_, _, open), _) :- !.
a5ve_ve_chk_consistent(ve(_, _, closed), _) :- !.
a5ve_ve_chk_consistent(_, UnNormVe) :-
   t5out_error(ve_inconsistent(UnNormVe)),
   !, fail.
 
%% *********************************************************************

a5ve_vterm(vt(_, _)).
a5ve_vterm_card(vt(_, Card), Card).
a5ve_vterm_values(vt(Values, _), Values).
 
a5ve_vterm_value(vt(Values, _), Value) :- member(Value, Values).
a5ve_vterm_value_1(vt(Values, _), Value) :-
    a5ve_vterm_value(Value, Values), !.
 
a5ve_vterm_numvalues(vt(Values, _), NumValues) :-
    length(Values, NumValues).
 
%% if [[ve]] = [] then [[ve]] = [[card(in,-23)]]
a5ve_empty_ve(card(in,-23)) :- !.
 

a5ve_empty_vterm_card(NR) :-
	t5nr_min(NR, in),
	t5nr_max(NR, -23).


%% a5ve_vterm_type(+Vterm, -VeType).
%%  if [[ve]] = [] then [[ve]] = [[in-23]]

a5ve_vterm_type(vt(Values, Card), VeType) :-
    length(Values, NumValues),
    a5ve_vterm_type(Card, NumValues, VeType).


a5ve_vterm_type(NR, _, open) :- 
	a5ve_empty_vterm_card(NR), !.
a5ve_vterm_type(NR, NumValues, Type) :-
	t5nr_minmax(NR, Min, Max),
	((Min \== Max, t5nr_g_max(Min, Max, Min)) ->
	    Type = inconsistent
	;
	(Max == NumValues ->
	    Type = closed
	;
	(t5nr_g_max(NumValues, Max, NumValues) ->
	    Type = inconsistent
	;   
	Type = open))).
	
%% *********************************************************************
%%                              VeType
%% *********************************************************************
%%  A VeType represents the type of value expression.
%%  (See Anatomy of the BACK system for a description).
%%
%%  Domains:
%%      VeType  =   "open" | "closed" | "inconsistent"
%% *********************************************************************
 
%% a5ve_vetype_or(VeType1, VeType2, VeType).
%%  True when VeType is the VeType resulting from "or"ing
%%  Vexpr1 of type VeType1 and Vexpr2 of type VeType2.
 
a5ve_vetype_or(open, open, open) :- !.
a5ve_vetype_or(open, closed, open) :- !.
a5ve_vetype_or(closed, closed, closed) :- !.
a5ve_vetype_or(closed, open, open) :- !.
a5ve_vetype_or(inconsistent, _, inconsistent) :- !.
a5ve_vetype_or(_, inconsistent, inconsistent) :- !.
 
%% a5ve_dnfve_make_closed(+DnfVe1, -DnfVe2).
%%  Make DnfVe1 "closed" by determining the "sure values", determining
%%  how many there are, replacing the DnfVe1 by the conjunction 
%%  of the sure values and atmost('how many are there...'). This is
%%  what follows from the AQL semantics (cf. KIT R78).
%%  If [[ DnfVe1 ]] is {} (e.g., [[someknown(nothing)]]) then
%%  also [[close(DnfVe1)]] is {}; the semantics in KIT R78 was 
%%  not precise enough at this point!
%% N.B. DnfVe1 is in DNF (assoc and distr already applied) !!!
 
a5ve_dnfve_make_closed(DnfVe, SureValsAndExpr) :-
	(a5ve_empty_ve(DnfVe) ->
	    SureValsAndExpr = DnfVe
	;   a5ve_dnfve_value_sets(DnfVe, VSets),
	    ord_intersection(VSets, SureVals),
	    length(SureVals, MinMax),
	    a5ve_and_list_expr([card(MinMax,MinMax)|SureVals],SureValsAndExpr)).


 
 
%% a5ve_dnfve_make_open(+DnfVe1, -DnfVe2).
%% True when DnfVe2 is DnfVe1 but with the max of all Cards
%% set to "in".
 
a5ve_dnfve_make_open(card(MIN,_),card(MIN,in)) :- !.
a5ve_dnfve_make_open(and(A,B),and(RA,RB)) :-
     !,
     a5ve_dnfve_make_open(A,RA),
     a5ve_dnfve_make_open(B,RB).
a5ve_dnfve_make_open(or(A,B),or(RA,RB)) :-
     !,
     a5ve_dnfve_make_open(A,RA),
     a5ve_dnfve_make_open(B,RB).
a5ve_dnfve_make_open(Key1, Key1) :- !.
 

%% a5ve_dnfve_value_sets(+DEnfVe, -SetsOfValues).
%%  Extracts the values in the elements of DnfVe.
%%  DnfVe must not be the empty_ve.

a5ve_dnfve_value_sets(or(X,Y), [VSet0|VSet]) :-
	!,
	a5ve_dnfve_value_set(X, [], VSet0),
	a5ve_dnfve_value_sets(Y, VSet).
a5ve_dnfve_value_sets(X, [VSet]) :-
	a5ve_dnfve_value_set(X, [], VSet).

a5ve_dnfve_value_set(card(_,_), VSet, VSet) :- !.
a5ve_dnfve_value_set(and(X,Y), VSet0, [X|VSet]) :- 
	!, 
	a5ve_dnfve_value_set(Y, VSet0, VSet).
a5ve_dnfve_value_set(Key, VSet, [Key|VSet]).





 
%% *********************************************************************
%% a5ve_dnfvterm_card(+DnfVterm, -Card).
%%  True when Card is the "and" of all Card values in DnfVterm.
 
a5ve_dnfvterm_card(DnfVterm, Card) :-
	t5nr_create(NR),
	a5ve_dnfvterm_card(DnfVterm,NR, Card).
 
a5ve_dnfvterm_card(and(X, Y), CardIn, CardOut) :-
    !,
    a5ve_dnfvterm_card(X, CardIn, Card1),
    a5ve_dnfvterm_card(Y, Card1, CardOut).

a5ve_dnfvterm_card(NR, CardIn, Card) :-
	t5nr_p(NR),
	!,
	t5nr_unify(NR,CardIn,Card).
%%	a5ve_card_and(NR, CardIn, Card).

a5ve_dnfvterm_card(_, Card, Card) :- !.
 
 
 
%% *********************************************************************
%%                       CARD  (cardinalities)
%% *********************************************************************
%% A Card is used to represent an interval over the natural numbers.
%%     E.g. [0..10]
%% The atom 'in' is used to represent infinity thereby allowing
%% for open intervals.  E.g. [1,in]
%%
%% Domains:
%%   Card       = card(CardInt, CardInt)
%% *********************************************************************
 
%% Basic Access functions
a5ve_card(card(_,_)).
a5ve_card_max(card(_, Max), Max).
a5ve_card_min(card(Min, _), Min).
 
%% a5ve_card(+M, +N, Card).
%%  True if Card is a Card with min M and max N.
 
a5ve_card(M, N, card(M, N)).
 
%% a5ve_card_consistent_p(+Card).
%%  True when Card is consistent.  I.e.  Card.max >= Card.min.
 
a5ve_card_consistent_p(card(M,N)) :- a5ve_cardint_max(M, N, N).
 
%% a5ve_card_in(+Card, +CardInt).
%%  True if CardInt is in the range specified by Card.
 
a5ve_card_in(card(Min, Max), I) :-
    a5ve_cardint_max(I, Max, Max),
    a5ve_cardint_min(I, Min, Min).
 
%% a5ve_card_strictly_in(+Card, +CardInt).
%%  True if CardInt is in the range specified by Card and CardInt is
%%  not one of the boundaries of Card.
 
a5ve_card_strictly_in(card(Min, Max), I) :-
    a5ve_cardint_max(I, Max, Max),
    I \== Max,
    a5ve_cardint_min(I, Min, Min),
    I \== Min.
 
 
%% a5ve_card_subsumedby(+Card1, +Card2).
%%  True if Card1 is subsumed by Card2.
 
a5ve_card_subsumedby(card(Min1, Max1), card(Min2, Max2)) :-
    a5ve_cardint_min(Min1, Min2, Min2),
    a5ve_cardint_max(Max1, Max2, Max2).
 
%% a5ve_card_and(+Card1, +Card2, -Card).
%%   Unify Card with the cardinality of Card1 "and" Card2.
 
a5ve_card_and(card(Min1, Max1), card(Min2, Max2), card(Min, Max)) :-
    a5ve_cardint_max(Min1, Min2, Min),
    a5ve_cardint_min(Max1, Max2, Max).
 
%% a5ve_card_or(+Card1, +Card2, -Card).
%%   Unify Card with the cardinality of Card1 "or" Card2.
 
a5ve_card_or(Min1-Max1, Min2-Max2, Min-Max) :-
    a5ve_cardint_min(Min1, Min2, Min),	
    a5ve_cardint_max(Max1, Max2, Max).	
 
 
%% *********************************************************************
%%                             CardInt
%% *********************************************************************
%% A CardInt is used to represent the boundary of a Card.
%%
%% Domains:
%%      CardInt =   0 | 1 | 2 | 3 .... | 'in'
%% *********************************************************************
 
%% a5ve_cardint_min(+N1, +N2, -N).
%%   True when N is the minimum of N1 and N2.
 
a5ve_cardint_min(I,J,I) :- integer(I), integer(J), I < J, !.
a5ve_cardint_min(I,J,J) :- integer(I), integer(J), J =< I, !.
a5ve_cardint_min(I,in, I) :- !.
a5ve_cardint_min(in,I, I) :- !.
 
%% a5ve_cardint_max(+N1, +N2, -N).
%%   True when N is the minimum of N1 and N2.
 
a5ve_cardint_max(_,in,X) :- !, X=in .
a5ve_cardint_max(in,_,X) :- !, X=in .
a5ve_cardint_max(I,J,I) :- integer(I), integer(J), I > J, !.
a5ve_cardint_max(I,J,J) :- integer(I), integer(J), J >= I, !.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
a5ve_make_simple_ve(ONF,Role,VExpr) :-
	b5nf_fillers(ONF,Role,Fillers),
	b5nf_nr(ONF,Role,NR),
	VExpr = ve([vt(Fillers,NR)],NR,open).	%% Status ist nicht immer 
						%% open !! Nur zum Test

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% a5ve_ve_subsumedby(+Ve1, +Ve2).
%%  True if Ve1 is subsumed by Ve2.
 
a5ve_ve_subsumedby(ve(Vterms1, Card1, _), ve(Vterms2, Card2, _)) :-
%%    a5ve_card_subsumedby(Card1, Card2), %% not sufficient, see below.
	t5nr_subsumes_p(Card2,Card1),
    a5ve_vterms_subsumedby(Vterms1, Vterms2).
 
%% a5ve_vterms_subsumedby(Vterms1, Vterms2).
%%  True if each Vterm in Vterms1 is subsumed by some vterm in Vterms2.
 
a5ve_vterms_subsumedby([],_).

a5ve_vterms_subsumedby([Vterm1|Vterms1], Vterms2) :-
	member(Vterm2, Vterms2),
	a5ve_vterm_subsumedby(Vterm1, Vterm2),
	!,   %% Don't retry member if the next call fails
	a5ve_vterms_subsumedby(Vterms1, Vterms2).
 
%% a5ve_vterm_subsumedby(+Vterm1, +Vterm2)
%%  True, if Vterm1 is subsumed by Vterm2.
%%
%% cmk 14.03.89: It is not sufficient to test subset relationship between the
%%  values of the Vterms. Card1 must be subsumed by Card2 as well.
%%  This is not handled by the subsumption check on the entire ve,
%%  which at most causes a "quicker" fail.
%%  Atleast in case of disjunctions a local check is required.
 
a5ve_vterm_subsumedby(vt(Values1, Card1), vt(Values2, Card2)) :-
	t5nr_subsumes_p(Card2,Card1),
	b5sort_subset_p(Values2,Values1).
%%a5ve_card_subsumedby(Card1,Card2),	%% mir 1.7.92
%%set_subset(Values2, Values1).		%% mir 1.7.92
