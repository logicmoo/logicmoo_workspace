
% ***************************************************************************
% *                 ibox DataBase                                           *
% ***************************************************************************

:- multifile b5dump_these_predicates/1.

b5dump_these_predicates([i5db_key/1,i5db_ilink/3,i5db_conc/5,b5_ilink_def/1]).

i5db_init :-
	retractall(i5db_key(_)),
	assert(i5db_key(0)),
	retractall(i5db_ilink(_,_,_)),   % FFS jjq
	retractall(i5db_conc(_,_,_,_,_)),
	retractall(i5db_revision_stamp(_)),
	assert(i5db_revision_stamp(-1)),
%	b5sta_set_flag(iboxfilled,false),   % -okp-
	!.

i5db_get_key(Key) :-
	i5db_key(Key),
	!.

i5db_incr_key :-
	retract(i5db_key(OldKey)),
	NewKey is OldKey + 1,
	assert(i5db_key(NewKey)),
	!.

i5db_set_revision_stamp(Stamp) :-
	retractall(i5db_revision_stamp(_)),
	assert(i5db_revision_stamp(Stamp)),
	!.

i5db_set_conc(TConc,Stamp,Status,IConc) :-
	(retract(i5db_conc(TConc,_,_,_,ILinks)),
	assert(i5db_conc(TConc,Stamp,Status,IConc,ILinks));
	   assert(i5db_conc(TConc,Stamp,Status,IConc,[]))),
	!.
i5db_set_conc(TConc,Stamp,Status,IConc,ILinks) :-
	retractall(i5db_conc(TConc,_,_,_,_)),
	assert(i5db_conc(TConc,Stamp,Status,IConc,ILinks)),
	!.

i5db_get_conc(TConc,Stamp,Status,IConc,ILinks) :-
	i5db_conc(TConc,Stamp,Status,IConc,ILinks),
	!.

i5db_get_link(Key,Conc1,Conc2) :-
	i5db_ilink(Key,Conc1,Conc2),
	!.

i5db_set_link(Conc1,Conc2,Key) :-
	i5db_get_key(Key),         
	assert(i5db_ilink(Key,Conc1,Conc2)),
	!.

i5db_del_link(ILink) :-
	retract(i5db_ilink(ILink,Conc1,_)),
	retract(i5db_conc(Conc1,Stamp,Status,IConc,ILinks)),
	b5sort_difference(ILinks,[ILink],NewILinks),
	assert(i5db_conc(Conc1,Stamp,Status,IConc,NewILinks)),
	!.
i5db_del_link(ILink,Conc1) :-
	retract(i5db_ilink(ILink,Conc1,_)),
	retract(i5db_conc(Conc1,Stamp,Status,IConc,ILinks)),
	b5sort_difference(ILinks,[ILink],NewILinks),
	assert(i5db_conc(Conc1,Stamp,Status,IConc,NewILinks)),
	!.


% *************************************************************************
% *                         IBOX                                          *
% *************************************************************************

%%% The IBox does not behave correctly for cyclic ILinks:
%%% for c -> all(r,d), d -> all(r,c)
%%% it yields c ?< all(r,all(r,c)) but *not* d ?< all(r,all(r,d))!!
%%% These answers depend on the order in which the queries are asked:
%%% the first one succeeds, the second one fails. The reason is that
%%% if c is asked first, expansion of d stops early to prevent a cycle.
%%% When a new link is added, d is again processed correctly, however.


iboxinit :-
	i5ibox_init,
	!.

i5ibox_init :-
	i5db_init,
	!.

i5ibox_empty_p :-
	i5db_get_key(0),
	!.

i5ibox_filled_p :-
	not(i5ibox_empty_p),
	!.

i5ibox_time_stamp(Stamp) :-
	i5db_get_key(Stamp),
	!.

i5ibox_valid_time_stamp_p(Stamp) :-
	i5ibox_time_stamp(Stamp),
	!.

i5ibox_valid_status_p(Status,Status) :- !.
i5ibox_valid_status_p(complete,_) :- !.
i5ibox_valid_status_p(semi,naive) :- !.
i5ibox_valid_status_p(_,no_vr) :- !.


%%%%% ADD LINK
%%%%% The link counter is increased and the link is stored
%%%%% in the database. If there is already an entry for
%%%%% Conc1 in the IBox Table Conc2 is added to this entry.
%%%%% Otherwise the union of Conc1 and Conc2 is entered in the table.
%%%%% Both operations set the time stamp of the table entry to 0,
%%%%% since no further consequences are taken into account. Since
%%%%% time stamp 0 leads in any case to an update the Completeness
%%%%% Status can be set to _.

i5ibox_add_link(Conc1,Conc2,ILink) :-
	i5db_incr_key,                 
	t5fil_create(EmptyFilter),
	t5fil_add(ibox_gen,EmptyFilter,Filter), % uk
	i5db_set_link(Conc1,Conc2,ILink),
	(i5db_get_conc(Conc1,_,_,IConc,ILinks),
	t5concid_del_filter(IConc,Filter), % right ??? %uk
	t5concid_unify(IConc,Conc2,Filter,NewIConc), %uk
	b5sort_unify(ILinks,[ILink],NewILinks),
	i5db_set_conc(Conc1,0,_,NewIConc,NewILinks);
	   t5concid_unify(Conc1,Conc2,Filter,IConc), %uk
	   i5db_set_conc(Conc1,0,_,IConc,[ILink])),
	!.

i5ibox_del_link(ILink) :-
	i5db_del_link(ILink),
	i5ibox_time_stamp(Stamp),
	i5db_set_revision_stamp(Stamp),
	i5db_incr_key,
	!.
i5ibox_del_link(ILink,LHS) :-
	i5db_del_link(ILink,LHS),
	i5ibox_time_stamp(Stamp),
	i5db_set_revision_stamp(Stamp),
	i5db_incr_key,
	!.



%%%%% COMPUTE
i5ibox_get_ibox_conc(TConc,TConc) :-             %% -mf- 3.8.92
	i5ibox_empty_p,
	!.

i5ibox_get_ibox_conc(TConc,IConc) :-
	i5ibox_compute_ibox_conc(TConc,[TConc],no_vr,IConc), 
	!.

i5ibox_get_ibox_conc(TConc,ReqStat,IConc) :-
	i5ibox_compute_ibox_conc(TConc,[TConc],ReqStat,IConc),
	!.

i5ibox_get_ibox_conc(TConc,In,ReqStat,IConc) :-
	(b5sort_member_p(TConc,In),
	IConc = TConc;
	   b5sort_unify(In,[TConc],NewIn),
	   i5ibox_compute_ibox_conc(TConc,NewIn,ReqStat,IConc)),
	!.

%%%%% COMPUTE IBOX CONC
%%%%% If the table entry retrieved by GET START is valid it
%%%%% is returned.
%%%%% Otherwise the normal form of Start is processed
%%%%% by including the IConcs of the Supers of TConc.
%%%%% The resulting normal form StoreNF is then further completed
%%%%% by NORMAL FORM, tbox completion, and ibox transformation
%%%%% (replacement of value restrictions by their ibox counterparts)
%%%%% to NewNF. This NewNF is then classified and stored by COMPUTE.
i5ibox_compute_ibox_conc(TConc,In,no_vr,IConc) :-
	i5ibox_get_start(TConc,Stamp,CompStat,Start),
	(i5ibox_valid_time_stamp_p(Stamp),
	i5ibox_valid_status_p(CompStat,no_vr),
	IConc = Start;
	   i5ibox_additional_implications(Start,Stamp,Concs),
	   (Concs == [],
	   IConc = Start;
	      t5concid_normal_form(Start,NF),
	      i5ibox_normal_form(Concs,NF,In,no_vr,StoreNF),
	      b5nf_complete(StoreNF,NewNF),
	      b5sort_unify(Concs,[Start],StoreSupers),
	      t5tbox_conc_hc(HC),
	      t5hc_minimize_special(HC,StoreSupers,MinSups),
	      i5ibox_compute(NewNF,MinSups,In,no_vr,INF,ISups,Subs,ClStat,Key),
	      i5ibox_store(ClStat,INF,ISups,Subs,Key,IConc)),
	   i5ibox_time_stamp(ActStamp),
	   i5db_set_conc(TConc,ActStamp,no_vrs,IConc)),
	!.
i5ibox_compute_ibox_conc(TConc,In,ReqStat,IConc) :-
	i5ibox_get_start(TConc,Stamp,CompStat,Start),
	(i5ibox_valid_time_stamp_p(Stamp),
	i5ibox_valid_status_p(CompStat,ReqStat),
	IConc = Start;
	   t5concid_direct_supers(TConc,TSups),
	   b5sort_unify(In,[TConc],NewIn),
	   t5concid_normal_form(Start,NF),
	   i5ibox_normal_form(TSups,NF,NewIn,ReqStat,StoreNF),
	   b5nf_complete(StoreNF,NextNF),
	   b5nf_ibox_transform(NextNF,NewIn,ReqStat,NewNF),
	   i5ibox_compute(NewNF,TSups,NewIn,ReqStat,INF,ISups,Subs,ClStat,Key),
	   i5ibox_store(ClStat,INF,ISups,Subs,Key,IConc),
	   i5ibox_time_stamp(ActStamp),
	   i5db_set_conc(TConc,ActStamp,ReqStat,IConc)),
	!.

i5ibox_additional_implications(Start,Stamp,Concs) :-
	t5fil_filter(ibox_lhs,Filter),
	t5concid_supers(Start,Filter,Supers),
	i5ibox_extract_new_right_hand_sides(Supers,Start,Stamp,Concs),
	!.

i5ibox_extract_new_right_hand_sides([],_,_,[]).
i5ibox_extract_new_right_hand_sides([Super|Supers],Start,Stamp,FinConcs) :-
	i5db_get_conc(Super,_,_,_,ILinks),
	i5ibox_get_relevant_right_hand_sides(ILinks,Start,Stamp,Concs),
	!,i5ibox_extract_new_right_hand_sides(Supers,Start,Stamp,StoreConcs),
	b5sort_unify(Concs,StoreConcs,FinConcs),
	!.

i5ibox_get_relevant_right_hand_sides([],_,_,[]).
i5ibox_get_relevant_right_hand_sides([Link|Links],Start,Stamp,FinConcs) :-
	Link > Stamp,
	i5db_get_link(Link,_,Conc),
	\+ t5concid_subsumes_p(Conc,Start),
	t5out_trace(ilink(Link)),
	!,i5ibox_get_relevant_right_hand_sides(Links,Start,Stamp,Concs),
	b5sort_unify(Concs,[Conc],FinConcs),
	!.
i5ibox_get_relevant_right_hand_sides([_|Links],Start,Stamp,Concs) :-
	!,i5ibox_get_relevant_right_hand_sides(Links,Start,Stamp,Concs),
	!.

%%%%% GET START 
%%%%% Returns the current entry in the IBox Table as Start
%%%%% and the associated Validity Stamp and the Completeness Status.
%%%%% For primitive components it is assumed that the IBox Table
%%%%% is the identity relation!
i5ibox_get_start(Prim,Stamp,complete,Prim) :-
%	t5concid_primitive_p(Prim),   % FFS: uk
	t5concid_prims(Prim,[Prim]),
	i5ibox_time_stamp(Stamp),
	!.
i5ibox_get_start(TConc,Stamp,Status,Start) :-
	i5db_get_conc(TConc,Stamp,Status,IConc,ILinks),
	i5db_revision_stamp(RevStamp),
	(Stamp > RevStamp,
	Start = IConc;
	          i5ibox_compute_revision_start(ILinks,TConc,Start)),
	!.
i5ibox_get_start(TConc,0,_,TConc).


i5ibox_compute_revision_start([],TConc,TConc) :- !.
i5ibox_compute_revision_start([ILink|Links],Conc,FinConc) :-
	i5db_get_link(ILink,_,RHS),
	t5concid_unify(Conc,RHS,StoreConc),
	i5ibox_compute_revision_start(Links,StoreConc,FinConc),
	!.
	
%%%%% COMPUTE
%%%%% A normal form is classified by CLASSIFY. If new supers are
%%%%% detected these are added by NORMAL FORM, the result is 
%%%%% tbox completed and ibox transformed (replacement of value 
%%%%% restrictions by their ibox counterparts), and COMPUTE is
%%%%% called recursively.

i5ibox_compute(NF,DirSupers,In,ReqStat,INF,Supers,Subs,Status,IConc) :-
	i5ibox_classify(NF,DirSupers,AllSupers,MinSups,Eq,Stat,Result),
	(Result == unfinished,!,
	b5sort_difference(MinSups,DirSupers,NewSupers),
	i5ibox_normal_form(NewSupers,NF,In,ReqStat,StoreNF),
	b5nf_complete(StoreNF,NextNF),
	(ReqStat == no_vr,!, 
	NewNF = NextNF;
	      b5nf_ibox_transform(NextNF,In,ReqStat,NewNF)),
	i5ibox_compute(NewNF,MinSups,In,ReqStat,INF,Supers,Subs,Status,IConc);
	   % Result == finished
	   INF = NF,
	   Supers = AllSupers,
	   Status = Stat,
	   IConc = Eq,
	   (Stat == old,!,
	   t5concid_subs(Eq,Subs);
	      t5cls_sub_concs(NF,AllSupers,Subs))).
	   


/* old version
i5ibox_compute(NF,Supers,In,Status,INF,DSups,Subs,Stat,IConc) :-
	i5ibox_classify(NF,Supers,AllSups,NewSupers,NFSubs,Key,Res,Result),
	(Result = finished,
	INF = NF,
	DSups = NewSupers,
	Subs = NFSubs,
	Stat = Res,
	IConc = Key;
	    i5ibox_normal_form(NewSupers,NF,In,Status,StoreNF),
	    b5nf_complete(StoreNF,NextNF),
	    (Status == no_vr,
	    NewNF = NextNF; 
	       b5nf_ibox_transform(NextNF,In,Status,NewNF)), 
	    i5ibox_compute(NewNF,AllSups,In,Status,INF,DSups,Subs,Stat,IConc)),
	!.
*/

%%%%% CLASSIFY
%%%%% A normal form NF is classified (Supers are the direct
%%%%% supers of this NF).
%%%%% The StoreSupers detected by classification are
%%%%% minimized to the direct supers DirSupers. When DirSupers
%%%%% are equal to Supers,  classification is finished and the
%%%%% normal form is stored.
%%%%% Otherwise the NewSupers are determined and returned.

i5ibox_classify(NF,DirSupers,AllSupers,MinSupers,IConc,Status,Result) :-
	t5cls_super_concs(NF,DirSupers,AllSupers,Eq,Status),
	(Status == old,!,
	i5ibox_get_ibox_conc(Eq,IConc),
	Result = finished;
	   t5tbox_conc_hc(HC),
	   t5hc_minimize_special(HC,AllSupers,MinSupers),
	   (MinSupers == DirSupers,
	   Result = finished;
	        Result = unfinished)),
	!.

/* old version
i5ibox_classify(NF,Supers,DirSupers,NewSupers,Subs,IConc,Status,Result) :-
	t5cls_conc(NF,Supers,[],StoreSupers,Subs,_,IConc,Status),
	t5tbox_conc_hc(HC),
	t5hc_minimize_special(HC,StoreSupers,DirSupers),
	(DirSupers == Supers,
	NewSupers = DirSupers,
	 Result = finished;
	    b5sort_difference(DirSupers,Supers,NewSupers),
	    Result = unfinished),
	 !.
*/

i5ibox_store(old,_,_,_,IConc,IConc) :- !.
i5ibox_store(_,NF,Supers,Subs,_,IConc) :-
	t5fil_filter(ibox,Filter), 
	b5nf_raw_store(NF,Filter,Supers,Subs,IConc),
	!.

%%%%% NORMAL FORM
%%%%% computes the ibox normal form for a set of concepts.
%%%%% Entries is used to prevent cycles: if an element of Concs
%%%%% is also an element of Entries, its normal form is not
%%%%% added (this is correct, because it is already part of
%%%%% the relevant normal form).
%%%%% Otherwise the normal form of the IBox concept of Conc is
%%%%% added. 
i5ibox_normal_form([],NF,_,_,NF).
i5ibox_normal_form([Entry|Concs],NF,Entries,Status,NewNF) :-
	b5sort_member_p(Entry,Entries),
	!,i5ibox_normal_form(Concs,NF,Entries,Status,NewNF),
	!.

i5ibox_normal_form([Conc|Concs],NF,Entries,Status,NewNF) :-
	i5ibox_compute_ibox_conc(Conc,Entries,Status,IConc),
	t5concid_normal_form(IConc,AddNF),
	b5nf_unify(NF,AddNF,StoreNF), 
	!,i5ibox_normal_form(Concs,StoreNF,Entries,Status,NewNF),
	!.

%%% ABox


i5ibox_complete_nf(TNF,TNF) :-                  %% -mf- 16.7.92
	i5ibox_empty_p,
	!.
i5ibox_complete_nf(TNF,INF) :-
	b5nf_xs(TNF,Supers),
	i5ibox_complete_nf(TNF,Supers,INF),
	!.

i5ibox_complete_nf(TNF,_,TNF) :-                  %% -mf- 16.7.92
	i5ibox_empty_p,
	!.
i5ibox_complete_nf(TNF,Supers,INF) :-
	t5cls_super_concs(TNF,Supers,AllSupers,Eq,Status),
	(Status == old,
	i5ibox_get_ibox_conc(Eq,IConc),
	t5concid_normal_form(IConc,INF),
	!;
	   t5tbox_conc_hc(HC),
	   t5hc_minimize_special(HC,AllSupers,DirSupers),
	   i5ibox_nf_add_ibox_supers(DirSupers,TNF,no_vr,StoreNF),
	   (StoreNF == TNF,
	   INF = TNF,
	   !;
	      b5nf_complete(StoreNF,NextNF),
	      i5ibox_compute(NextNF,DirSupers,[],no_vr,INF,_,_,_,_))).


i5ibox_nf_add_ibox_supers([],NF,_,NF).
i5ibox_nf_add_ibox_supers([Conc|Concs],NF,Status,NewNF) :-
	i5ibox_compute_ibox_conc(Conc,[],Status,IConc),
	(IConc == Conc,
	!,
	StoreNF = NF;
	   t5out_trace(ibox_info(Conc)),
	   t5concid_normal_form(IConc,AddNF),
	   b5nf_unify(NF,AddNF,StoreNF)), 
	!,i5ibox_nf_add_ibox_supers(Concs,StoreNF,Status,NewNF),
	!.

/* old version
i5ibox_complete_nf(TNF,INF) :- 
	t5cls_conc(TNF,[],[],Supers,_,_,Key,Status),
	(Status = new,
	 !,
	 t5tbox_conc_hc(HC),
	 t5hc_minimize_special(HC,Supers,DirSupers);
	    Status = old,
	    DirSupers = [Key]),
	i5ibox_normal_form(DirSupers,TNF,[],no_vr,StoreNF),
	b5nf_complete(StoreNF,NextNF),
%	b5nf_ibox_transform(NextNF,[],naive,NewNF),
	i5ibox_compute(NextNF,DirSupers,[],no_vr,INF,_,_,_,_),
	!.
*/



%%% Subsumption/Disjointness Information
i5ibox_subsumes_p(conc,Key1,Key2) :-
	i5ibox_get_ibox_conc(Key2,IKey2),
	t5concid_subsumes_p(Key1,IKey2),
	!.
i5ibox_subsumes_p(role,Key1,Key2) :-
	t5role_ibox_subsumes_p(Key1,Key2),   
	!.

i5ibox_disjoint_p(conc,Key1,Key2) :-
	i5ibox_get_ibox_conc(Key1,IKey1),
	i5ibox_get_ibox_conc(Key2,IKey2),
	t5concid_disjoint_p(IKey1,IKey2),
	!.
i5ibox_disjoint_p(role,Key1,Key2) :-
	t5role_ibox_disjoint_p(Key1,Key2), 
	!.

i5ibox_equivalent_p(conc,Key1,Key2) :-
	i5ibox_get_ibox_conc(Key1,IKey1),
	i5ibox_get_ibox_conc(Key2,IKey1),
	!.

i5ibox_equivalent_p(role,Key,Key).   %% FFS fischli
	

i5ibox_incoherent_p(conc,Key) :-
	i5ibox_get_ibox_conc(Key,Nothing),
	t5tbox_bottom_p(conc,Nothing).

i5ibox_incoherent_p(role,Key) :-
	t5tbox_bottom_p(role,Key),   %% FFS fischli
	!.



% *************************************************************************
% *             role COMPosition                                          *
% *************************************************************************
% *************************************************************************
% *             Normalization of a Role Composition                       *
% *************************************************************************

t5comp_normalize(DomainIn,RangeIn,RNF1,RNF2,DomainOut,RangeOut,COMP) :-
	t5rnf_raw_create(D1,R1,_,_,_,_,_,COMP1,RNF1),
	t5rnf_raw_create(D2,R2,_,_,_,_,_,COMP2,RNF2),
	t5concid_unify(DomainIn,D1,DomainOut),
	t5concid_unify(RangeIn,R2,RangeOut),
	t5concid_unify(R1,D2,Conc),
	t5tbox_bottom_key(conc,Bottom),
	Conc \== Bottom,
	!,
	(t5comp_is_comp(RNF1),
	 !,
	 (t5comp_is_comp(RNF2),
	  !,
	     t5comp_both_comp(DomainOut,Conc,RangeOut,COMP1,COMP2,COMP);
	         t5comp_first_comp(DomainOut,Conc,RangeOut,COMP1,RNF2,COMP));
	(t5comp_is_comp(RNF2),
	 !,
	     t5comp_second_comp(DomainOut,Conc,RangeOut,RNF1,COMP2,COMP);
	         t5comp_no_comp(DomainOut,Conc,RangeOut,RNF1,RNF2,COMP))).

t5comp_normalize(_,_,RNF1,RNF2,Bottom,Bottom,[]) :-
	t5rnf_raw_create(_,R1,_,_,_,_,_,_,RNF1),
	t5rnf_raw_create(D2,_,_,_,_,_,_,_,RNF2),
	t5out_warning(incoherent_comp_range_domain(R1,D2)),
	t5tbox_bottom_key(conc,Bottom).


% If both roles are compositions, then the new domain is added to the first 
% role of the first composition (clause 1).
% Conc is added to the range of the last role of the first composition and to
% the domain of the first role of the second composition (clause 2).
% The new range is added to the last role of the second composition. (clause 4)

t5comp_both_comp(d,Conc,Range,[[R]],[[R1|Comp2]],[RNew,R1New|COMP]) :-
	!,
	t5role_normal_form(R,Rnf),
	t5rnf_add_range(Rnf,Conc,RnfNew),
	t5role_store_nf(RnfNew,RNew),
	t5role_normal_form(R1,R1nf),
	t5rnf_add_domain(R1nf,Conc,R1nfNew),
	t5role_store_nf(R1nfNew,R1New),
	t5comp_both_comp(d,c,Range,[],[Comp2],COMP).

t5comp_both_comp(d,Conc,Range,[[R|Comp1]],[Comp2],[R|COMP]) :-
	!,
	t5comp_both_comp(d,Conc,Range,[Comp1],[Comp2],COMP).
	
t5comp_both_comp(d,c,Range,[],[[R]],[RNew]) :-
	!,
	t5role_normal_form(R,Rnf),
	t5rnf_add_range(Rnf,Range,RnfNew),
	t5role_store_nf(RnfNew,RNew).

t5comp_both_comp(d,c,Range,[],[[R|Comp2]],[R|COMP]) :-
	!,
	t5comp_both_comp(d,c,Range,[],[Comp2],COMP).

t5comp_both_comp(Domain,Conc,Range,[[R1|Comp1]],[Comp2],[R1New|COMP]) :-
	t5role_normal_form(R1,R1nf),
	t5rnf_add_domain(R1nf,Domain,R1nfNew),
	t5role_store_nf(R1nfNew,R1New),
	t5comp_both_comp(d,Conc,Range,[Comp1],[Comp2],COMP).


t5comp_first_comp(d,Conc,Range,[[R1]],R2nf,[R1New,R2New]) :-
	!,
	t5role_normal_form(R1,R1nf),
	t5rnf_add_range(R1nf,Conc,R1nfNew),
	t5role_store_nf(R1nfNew,R1New),
	t5rnf_add_domain(R2nf,Conc,R2nf1),
	t5rnf_add_range(R2nf1,Range,R2nfNew),
	t5role_store_nf(R2nfNew,R2New).

t5comp_first_comp(d,Conc,Range,[[R|Comp1]],R2nf,[R|COMP]) :-
	!,
	t5comp_first_comp(d,Conc,Range,[Comp1],R2nf,COMP).

t5comp_first_comp(Domain,Conc,Range,[[R1|Comp1]],R2nf,[R1New|COMP]) :-
	t5role_normal_form(R1,R1nf),
	t5rnf_add_domain(R1nf,Domain,R1nfNew),
	t5role_store_nf(R1nfNew,R1New),
	t5comp_first_comp(d,Conc,Range,[Comp1],R2nf,COMP).
	

t5comp_second_comp(Domain,Conc,Range,Rnf,[[R2|Comp2]],[RNew,R2New|COMP]) :-
	!,
	t5rnf_add_domain(Rnf,Domain,Rnf1),
	t5rnf_add_range(Rnf1,Conc,RnfNew),
	t5role_store_nf(RnfNew,RNew),
	t5role_normal_form(R2,R2nf),
	t5rnf_add_domain(R2nf,Conc,R2nfNew),
	t5role_store_nf(R2nfNew,R2New),
	t5comp_both_comp(d,c,Range,[],[Comp2],COMP).


t5comp_no_comp(Domain,Conc,Range,R1nf,R2nf,[R1New,R2New]) :-
	!,
	t5rnf_add_domain(R1nf,Domain,R1nf1),
	t5rnf_add_range(R1nf1,Conc,R1nfNew),
	t5role_store_nf(R1nfNew,R1New),
	t5rnf_add_domain(R2nf,Conc,R2nf1),
	t5rnf_add_range(R2nf1,Range,R2nfNew),
	t5role_store_nf(R2nfNew,R2New).

% *********************************************************************

t5comp_is_comp(RNF) :-
	t5rnf_raw_create(_,_,[Anyrole],[],[Anyrole],[],[],[_],RNF),
	!,
	t5tbox_top_key(role,Anyrole).


t5comp_is_comp(RNF) :-
	t5rnf_raw_create(_,_,Prims,[],InvPrims,[],[],[Comp],RNF),
	t5comp_check_feature(Comp,Yes_Inv_Both),
	t5tbox_feature_prim_key(FeaturePrim),
	t5tbox_anyrole_key(Anyrole),
	t5comp_check_prims(Yes_Inv_Both,[Anyrole,FeaturePrim],Prims,InvPrims).

t5comp_check_prims(yes,AnyFea,Prims,_) :-
	b5sort_subset_p(Prims,AnyFea).

t5comp_check_prims(inv,AnyFea,_,InvPrims) :-
	b5sort_subset_p(InvPrims,AnyFea).

t5comp_check_prims(both,AnyFea,Prims,InvPrims) :-
	b5sort_subset_p(Prims,AnyFea),
	b5sort_subset_p(InvPrims,AnyFea).

t5comp_check_feature(Comp,Yes_Inv_Both) :-
	(t5comp_comp_feature_p(Comp),
	 !,
	 (t5comp_comp_inv_feature_p(Comp),
	  !,
	  Yes_Inv_Both = both;
	     Yes_Inv_Both = yes);
	    t5comp_comp_inv_feature_p(Comp),
	    Yes_Inv_Both = inv).

% **************************************************************************

t5comp_feature_p([Comp]) :-
	!,
	t5comp_comp_feature_p(Comp).

t5comp_feature_p([Comp|Comps]) :-
	(t5comp_comp_feature_p(Comp),
	 !;
	    t5comp_feature_p(Comps)).

t5comp_comp_feature_p([RKey]) :-
	!,
	t5role_feature_p(RKey).

t5comp_comp_feature_p([RKey|RKeys]) :-
	t5role_feature_p(RKey),
	t5comp_comp_feature_p(RKeys).

t5comp_inv_feature_p([Comp]) :-
	!,
	t5comp_comp_inv_feature_p(Comp).

t5comp_inv_feature_p([Comp|Comps]) :-
	(t5comp_comp_inv_feature_p(Comp),
	 !;
	    t5comp_inv_feature_p(Comps)).

t5comp_comp_inv_feature_p([RKey]) :-
	!,
	t5role_inv_role(RKey,InvKey),
	t5role_feature_p(InvKey).

t5comp_comp_inv_feature_p([RKey|RKeys]) :-
	t5role_inv_role(RKey,InvKey),
	t5role_feature_p(InvKey),
	t5comp_comp_inv_feature_p(RKeys).

% ***********************************************************************

t5comp_inv_comp(COMP,InvCOMP) :-
	t5comp_inv_comp(COMP,[],InvCOMP).

t5comp_inv_comp([Rn],InvCOMP,[InvRn|InvCOMP]) :-
	!,
	t5role_inv_role(Rn,InvRn).

t5comp_inv_comp([R1|COMP],InvCOMP1,InvCOMP) :-
	t5role_inv_role(R1,InvR1),
	t5comp_inv_comp(COMP,[InvR1|InvCOMP1],InvCOMP).


t5comp_add_domain(OldRNF,Domain,NewRNF) :-
	t5rnf_r_comps(OldRNF,OldComps,NewComps,NewRNF),
	t5comp_comps_add_domain(OldComps,Domain,NewComps).

t5comp_comps_add_domain([],_,[]).

t5comp_comps_add_domain([[R|RList]|CompsIn],Domain,[COMP|CompsOut]) :-
	t5comp_comp_add_domain([R|RList],Domain,COMP),
	t5comp_comps_add_domain(CompsIn,Domain,CompsOut).

t5comp_comp_add_domain([R1|Rest],Domain,[R2|Rest]) :-
	t5role_normal_form(R1,R1nf),
	t5rnf_add_domain(R1nf,Domain,R2nf),
	t5role_store_nf(R2nf,R2).


t5comp_add_range(OldRNF,Range,NewRNF) :-
	t5rnf_r_comps(OldRNF,OldComps,NewComps,NewRNF),
	t5comp_comps_add_range(OldComps,Range,NewComps).

t5comp_comps_add_range([],_,[]).

t5comp_comps_add_range([[R|RList]|CompsIn],Range,[COMP|CompsOut]) :-
	t5comp_comp_add_range([R|RList],Range,COMP),
	t5comp_comps_add_range(CompsIn,Range,CompsOut).

t5comp_comp_add_range([R1],Range,[R2]) :-
	!,
	t5role_normal_form(R1,R1nf),
	t5rnf_add_range(R1nf,Range,R2nf),
	t5role_store_nf(R2nf,R2).

t5comp_comp_add_range([R1|RList1],Range,[R1|RList2]) :-
	t5comp_comp_add_range(RList1,Range,RList2).

% *************************************************************************
% *             Subsumption                                               *
% *************************************************************************

% Subsumtion betwen a transitive role and a role composition:
% A transitive role 'trans(r)' subsumes a role composition 'r1 comp r2  comp
% ... comp rn', if the trans(r) subsumes each ri.

t5comp_trans_comps_subsumes_p(_,[]) :- !, fail.

t5comp_trans_comps_subsumes_p(Trans,[COMP|_]) :-
	t5comp_trans_comp_subsumes_p(COMP,Trans), !.

t5comp_trans_comps_subsumes_p(Trans,[_|Comps]) :-
	t5comp_trans_comps_subsumes_p(Trans,Comps).

t5comp_trans_comp_subsumes_p([],_).

t5comp_trans_comp_subsumes_p([R|Rest],Trans) :-
	t5role_subsumes_p(Trans,R),
	!,
	t5comp_trans_comp_subsumes_p(Rest,Trans).
	
% *************************************************************************

% Subsumtion between role compositions

t5comp_subsumes_p([],_).

t5comp_subsumes_p([COMP|Comps1],Comps2) :-
	t5comp_subs_p(Comps2,COMP),
	t5comp_subsumes_p(Comps1,Comps2).
	
t5comp_subs_p([],_) :- !, fail.

t5comp_subs_p([COMP2|_],COMP1) :-
	t5comp_comp_subs_p(COMP1,COMP2,[],[]),
	!.

t5comp_subs_p([_|Comps],COMP1) :-
	t5comp_subs_p(Comps,COMP1),
	!.

t5comp_comp_subs_p([R1|COMP1],[R2|COMP2],COMP11,COMP22) :-
	t5comp_comp_subs_p(COMP1,COMP2,[R1|COMP11],[R2|COMP22]).

t5comp_comp_subs_p([R1|COMP1],[R2],[R11|COMP11],[R22|COMP22]) :-
	t5comp_c_s_p([R1|COMP1],[R2],R1Rest),
	(R1Rest == [],
	 !,                                                     %% -mf- ???
	t5comp_comp_subs_p([R11],[R22],COMP11,COMP22);
	t5comp_comp_subs_p([R11|R1Rest],[R22],COMP11,COMP22)).

t5comp_comp_subs_p([R1|COMP1],[R2],[],[]) :-
	t5comp_c_s_p([R1|COMP1],[R2],[]).

t5comp_c_s_p([R1],[R2],[]) :-
	!,
	t5role_subsumes_p(R1,R2).

t5comp_c_s_p([R1|COMP1],[R2],[R1|Rest]) :-
	t5comp_c_s_p(COMP1,[R2],Rest).

t5comp_c_s_p(COMP1,[R2],[]) :-
	t5role_comps(R2,Comps2),
	t5comp_subs_p(Comps2,COMP1).

% *************************************************************************

t5comp_handle_comps(RNF,_) :-
	t5rnf_comps(RNF,[]),
	!.

t5comp_handle_comps(RNF,Key) :-
	t5comp_is_comp(RNF),
	!,
	t5rnf_comps(RNF,[Comp]),
	t5comp_insert_comp_infos(Comp,Key).

t5comp_handle_comps(RNF,_) :-
	t5rnf_comps(RNF,CompList),
	t5comp_store_complist(CompList).


t5comp_store_complist([]).

t5comp_store_complist([Comp|CompList]) :-
	t5comp_store_comp(Comp),
	t5comp_store_complist(CompList).

t5comp_store_comp(Comp) :-
	t5tbox_anyrole_key(Anyrole),
	t5role_normal_form(Anyrole,AnyroleNF),
	t5comp_domain(Comp,Domain),
	t5rnf_add_domain(AnyroleNF,Domain,RNF1),
	t5comp_range(Comp,Range),
	t5rnf_add_range(RNF1,Range,RNF2),
	t5rnf_insert_comps(RNF2,[Comp],RNF),
	t5role_store_nf(RNF,Key),
	t5comp_insert_comp_infos(Comp,Key).

t5comp_domain([R1|_],Domain) :-
	t5role_domain(R1,Domain).

t5comp_range([Rn],Range) :-
	!,
	t5role_domain(Rn,Range).

t5comp_range([_,R|Comps],Range) :-
	t5comp_range([R|Comps],Range).

t5comp_insert_comp_infos(Comp,Key) :-
	t5comp_insert_comp_infos(Comp,[],Key).

t5comp_insert_comp_infos([],_,_).

t5comp_insert_comp_infos([R|RList1],RList2,Key) :-
	t5rdb_add_comp_info(R,comp(RList2,RList1,Key)),
	!,
	t5comp_insert_comp_infos(RList1,[R|RList2],Key).

% *************************************************************************


% **************************************************************************
%                  Role Data Base                                          *
% **************************************************************************

%EXPORT-predicates:
% ask               t5rdb_attribute_p(+ RKey)
% ask               t5rdb_comps(+ RKey, - Comps)
% ask               t5rdb_defined_p(+ RKey)
% ask               t5rdb_feature_p(+ RKey)
% ask               t5rdb_filter(+ RKey, - Filter)
% ask               t5rdb_inherent_feature_p(+ RKey)
% tell              t5rdb_init
% ask               t5rdb_inv(+ RKey, - RKey)
% tell              t5rdb_new_filter(+ RKey, + Filter)
% tell              t5rdb_new_inv(+ RKey, + RKey)
% tell              t5rdb_new_comps(+ RKey, + Comps)
% ask               t5rdb_primitive_p(+ RKey)
% tell              t5rdb_remove(+ RKey)
% ask               t5rdb_rnf(+ RKey, - RNF)
% ask               t5rdb_rnf(+ RKey, - RNF, - Filter)
% tell              t5rdb_store(+ Role)
% tell              t5rdb_store(+ Role, + Key, + yes/no)

%IMPORT-predicates:
%                   t5role_filter(Role,Filter)
%                   t5role_flag(Role,Flag)
%                   t5role_key(Role,RKey)
%                   t5role_range(+RKey, - CKey)
%                   t5role_rnf(Role,RNF)
%                   t5role_subsumes_p(+ RKey, + RKey)

%                   t5tbox_anything_key(- CKey)

% **************************************************************************

%% TELL predicates:

% **************************************************************************

t5rdb_init :-
	retractall(t5rdb(_,_,_,_,_,_,_)),
	!.

% **************************************************************************

t5rdb_store(Role) :-
	t5role_key(Role,Key),
	t5role_flag(Role,Flag),
	t5role_rnf(Role,RNF),
	t5role_filter(Role,Filter),
	assert(t5rdb(Key,Flag,RNF,Filter,_,_,_)).


%% If the inverse role is known than it's Key is stored.
%% If the range of the role is an abstract concept than the inverse role is
%% 'unknown' else an inverse role does 'notexist'.
%% If the role is a transitive closure than TransFlag is 'yes' else 'no'.

t5rdb_store(Role,Inv,TransFlag) :-
	t5role_key(Role,Key),
	t5role_flag(Role,Flag),
	t5role_rnf(Role,RNF),
	t5role_filter(Role,Filter),
        assert(t5rdb(Key,Flag,RNF,Filter,Inv,[],TransFlag)).

% **************************************************************************

t5rdb_new_filter(Key,Filter) :-
	(t5rdb_filter(Key,Filter),
	 !;
	    retract(t5rdb(Key,Flag,NF,_,Inv,Comps,TransFlag)), !,
	    assert(t5rdb(Key,Flag,NF,Filter,Inv,Comps,TransFlag))).


t5rdb_new_inv(Key,Inv) :-
	t5rdb(Key,_,_,_,unknown,_,_),
	!,
	retract(t5rdb(Key,Flag,NF,Filter,_,Comps,TransFlag)), !, 
	assert(t5rdb(Key,Flag,NF,Filter,Inv,Comps,TransFlag)).

t5rdb_new_inv(Key,Inv) :-
	(t5rdb(Key,_,_,_,Inv,_,_),
	 !;
	     t5out_error(irregular_rdb_new_inv),
	     fail).


t5rdb_new_comps(Key,Comps) :-
	retract(t5rdb(Key,Flag,NF,Filter,Inv,_,TransFlag)), !,
	assert(t5rdb(Key,Flag,NF,Filter,Inv,Comps,TransFlag)).

t5rdb_add_comp_info(Key,CompInfo) :-
	retract(t5rdb(Key,Flag,NF,Filter,Inv,CompOld,TransFlag)), !,
	b5sort_unify([CompInfo],CompOld,CompNew),
	assert(t5rdb(Key,Flag,NF,Filter,Inv,CompNew,TransFlag)).
	

% **************************************************************************

t5rdb_remove(Key) :-
	retract(t5rdb(Key,_,_,_,_,_,_)), !.

% **************************************************************************

%% ASK predicates:

% **************************************************************************

t5rdb_rnf(Key,RNF,Filter) :-
        t5rdb(Key,_,RNF,Filter,_,_,_).


t5rdb_rnf(Key,RNF) :-
        t5rdb(Key,_,RNF,_,_,_,_).

t5rdb_filter(Key,Filter) :-
	t5rdb(Key,_,_,Filter,_,_,_).

t5rdb_primitive_p(Key) :-
	t5rdb(Key,p,_,_,_,_,_).

t5rdb_defined_p(Key) :-
	t5rdb(Key,d,_,_,_,_,_).

t5rdb_attribute_p(Key) :-
	t5rdb(Key,a,_,_,_,_,_).

t5rdb_feature_p(Key) :-
	t5rdb(Key,f,_,_,_,_,_).

t5rdb_inherent_feature_p(Key) :-
	t5rdb(Key,i,_,_,_,_,_).


t5rdb_inv(Key,Inv) :-
	t5rdb(Key,_,_,_,Inv,_,_).

t5rdb_comps(Key,Comps) :-
	t5rdb(Key,_,_,_,_,Comps,_).

t5rdb_trans_p(Key) :-
	t5rdb(Key,_,_,_,_,_,yes).

% **************************************************************************

%% backdump

:- multifile b5dump_these_predicates/1.

b5dump_these_predicates([t5rdb/7]).

% **************************************************************************

% ************************************************************************
% *                RoleNormalForm                                         *
% ************************************************************************

%EXPORT-predicates:
% tell              t5rnf_add_comp(+ RNF, + RKey, + RKey, - RNF)
% tell              t5rnf_add_domain(+ RNF, + CKey, - RNF)
% tell              t5rnf_add_inv(+ RNF, + RKey, - RNF)
% tell              t5rnf_add_neg_invs(+ RNF, + RKey, - RNF)
% tell              t5rnf_add_neg_prims(+ RNF, + RKey, - RNF)
% tell              t5rnf_add_prim(+ RNF, + RKey, - RNF)
% tell              t5rnf_add_range(+ RNF, + CKey, - RNF)
% tell              t5rnf_add_trans(+ RNF,R + Key, - RNF)
% ask               t5rnf_compare(+ Sub, + RNF, + RNF, - Sub)
% ask               t5rnf_compare_without_prims(+ Sub, + RNF, + RNF, - Sub)
% ask               t5rnf_comps(+ RNF, - Complist)
%                   t5rnf_create(- RNF)
% ask               t5rnf_domain(+ RNF, - CKey)
% ask               t5rnf_incoherent_p(+ RKey)
% ask               t5rnf_invs(+ RNF, - Invlist)
% ask               t5rnf_neg_invs(+ RNF, - Neg_Invlist)
% ask               t5rnf_neg_prims(+ RNF, - Neg_Primlist)
% ask               t5rnf_prims(+ RNF, - Primlist)
% ask               t5rnf_range(+ RNF, - CKey)
% ask               t5rnf_subsumes_p(+ RKey, + RKey)
% ask               t5rnf_subsumes_without_domain_range_p(+ RKey, + RKey)
% ask               t5rnf_transs(+ RNF, - Translist)
% ask               t5rnf_trans_p(+ RNF)
%                   t5rnf_unify(+ RNF, + RNF, - RNF)

%IMPORT-predicates:
%                   t5concid_compare(Sub,CKey,CKey,Sub)
%                   t5concid_incoherent_p(CKey)
%                   t5concid_subsumes_p(CKey,Ckey)
%                   t5concid_unify(CKey,CKey,CKey)

%                   b5sort_common_member_p(List,List)
%                   b5sort_compare(Sub,List,List,Sub)
%                   b5sort_subset_p(List,List)
%                   b5sort_unify(List,List,List)




%% Role normal forms:

%% The role normal form is an 8-tuple:

%%    rnf(domain,range,prims,negprims,invs,neginvs,transs,comps)

%% domain is the conceptkey of the domain of the role
%% range is the conceptkey of the range of the role
%% prims is a list of primitive role components
%% negprims a the list of  primitive role components which are negated
%% inv is a list of primitive role components which are inverse in the role
%% neginv is a list of primitive role components which are negated and inverse
%% transs is a list of rolekeys. For each rolekey R in transs trans(R) is
%% a superrole of the role
%% comps is a list of lists of rolekeys. If [R1, ... Rn] is a member of comps
%% then R1 comp ... comp Rn is a superrole of the role.

t5rnf_create(rnf(Anything,All,[],[],[],[],[],[])) :- 
	t5tbox_all_key(All),
	t5tbox_anything_key(Anything).

% ************************************************************************
% ************************************************************************

t5rnf_domain(rnf(DomainKey,_,_,_,_,_,_,_),DomainKey) :- !.
t5rnf_range(rnf(_,RangeKey,_,_,_,_,_,_),RangeKey) :- !.
t5rnf_prims(rnf(_,_,Prims,_,_,_,_,_),Prims) :- !.
t5rnf_neg_prims(rnf(_,_,_,NegPrims,_,_,_,_),NegPrims) :- ! .
t5rnf_invs(rnf(_,_,_,_,Invs,_,_,_),Invs) :- !.
t5rnf_neg_invs(rnf(_,_,_,_,_,NegInvs,_,_),NegInvs) :- !.
t5rnf_transs(rnf(_,_,_,_,_,_,Transs,_),Transs) :- !.
t5rnf_comps(rnf(_,_,_,_,_,_,_,Comps),Comps) :- !.

% ************************************************************************

t5rnf_trans_p(rnf(DKey,RKey,[RoleTop],[],[RoleTop],[],[TransKey],[])) :-
	t5tbox_top_key(role,RoleTop),
	t5role_domain(TransKey,DKey),
	t5role_range(TransKey,RKey), !.

t5rnf_compute_inv(rnf(_,RangeKey,_,_,_,_,_,_),Inv) :-
	t5tbox_anything_key(Anything),
	(t5concid_subsumes_p(Anything,RangeKey),
	 !,
	 Inv = unknown;
	   Inv = notexists).

% ************************************************************************

%% Adding new information to the rnf

t5rnf_add_domain(RNF,Domain,NewRNF) :-
	t5rnf_r_domain(RNF,OldDomain,NewDomain,RNF1),
        t5concid_unify(OldDomain,Domain,NewDomain),
	t5comp_add_domain(RNF1,NewDomain,NewRNF).

t5rnf_add_range(RNF,Range,NewRNF) :-
	t5rnf_r_range(RNF,OldRange,NewRange,RNF1),
        t5concid_unify(OldRange,Range,NewRange),
	t5comp_add_range(RNF1,NewRange,NewRNF).

t5rnf_add_prim(RNF,Prim,NewRNF) :- 
	t5rnf_insert_prims(RNF,[Prim],NewRNF).

t5rnf_add_neg_prim(RNF,NegPrim,NewRNF) :- 
	t5rnf_insert_neg_prims(RNF,[NegPrim],NewRNF).


t5rnf_add_inv(RNF,Inv,NewRNF) :-                           %????? 
	(t5tbox_top_key(role,Inv),
	 !;
	t5tbox_bottom_key(role,Inv)),
	!,
	t5rnf_insert_invs(RNF,[Inv],NewRNF).

t5rnf_add_inv(RNF,Inv,NewRNF) :- 
	t5rnf_normalize_inv(Inv,InvNF),
	((InvNF == disjoint_anything_range,
	  !;
	  InvNF == unspecified_range),
         !,
	NewRNF = InvNF;
	t5rnf_unify(RNF,InvNF,NewRNF)).

t5rnf_add_trans(RNF,TransNF,TransKey,NewRNF) :-
	t5rnf_domain(TransNF,DomainKey),
	t5rnf_range(TransNF,RangeKey),
	(t5rnf_check_trans(TransNF,DomainKey,RangeKey),
	 !,
	 t5rnf_unify(RNF,TransNF,NewRNF);
	    t5rnf_add_domain(RNF,DomainKey,RNF1),
	    t5rnf_add_range(RNF1,RangeKey,RNF2),
	    t5rnf_insert_transs(RNF2,[TransKey],NewRNF)).


t5rnf_check_trans(_,DomainKey,RangeKey) :-
	t5concid_disjoint_p(DomainKey,RangeKey),
	!.

t5rnf_check_trans(rnf(DomainKey,RangeKey,[RoleTop],[],[RoleTop],[],
                                         [TransKey],[]),DomainKey,RangeKey) :-
	t5tbox_top_key(role,RoleTop),
	t5role_domain(TransKey,DomainKey),
	t5role_range(TransKey,RangeKey), !.
/*
t5rnf_add_comp(RNF,Comp1,Comp2,NewRNF) :-
	t5tbox_anyrole_key(Anyrole),
	t5role_normal_form(Anyrole,AnyroleNF),
	(t5rnf_subsumes_without_domain_range_p(Comp1,AnyroleNF),
	 !;
	    t5rnf_subsumes_without_domain_range_p(Comp2,AnyroleNF)),
	t5rnf_domain(Comp1,Domain),
	t5rnf_range(Comp2,Range),
	t5rnf_add_domain(RNF,Domain,RNF1),
	t5rnf_add_range(RNF1,Range,NewRNF).
*/
t5rnf_add_comp(RNF,Comp1,Comp2,NewRNF) :-
	t5rnf_r_domain(RNF,DomainIn,DomainOut,RNF1),
	t5rnf_r_range(RNF1,RangeIn,RangeOut,RNF2),
	t5comp_normalize(DomainIn,RangeIn,Comp1,Comp2
                                                   ,DomainOut,RangeOut,COMP),
	t5rnf_insert_comps(RNF2,[COMP],NewRNF).

% ***********************************************************************

t5rnf_add_role_without_domain_range(RNF1,RNF2,NewRNF) :-
	t5rnf_unify(RNF1,RNF2,RNF3),
	t5rnf_raw_create(_,_,P3,NP3,I3,NI3,T3,C3,RNF3),
	t5tbox_all_key(All),
	t5tbox_anything_key(Anything),
	t5rnf_raw_create(Anything,All,P3,NP3,I3,NI3,T3,C3,NewRNF).
	
% ***********************************************************************

t5rnf_insert_prims(RNF,Prims,NewRNF) :- 
	t5rnf_r_prims(RNF,OldPrims,NewPrims,NewRNF),
        b5sort_unify(OldPrims,Prims,NewPrims), !.

t5rnf_insert_neg_prims(RNF,NegPrims,NewRNF) :- 
	t5rnf_r_neg_prims(RNF,OldNegPrims,NewNegPrims,NewRNF),
        b5sort_unify(OldNegPrims,NegPrims,NewNegPrims), !.

t5rnf_insert_invs(RNF,Invs,NewRNF) :- 
	t5rnf_r_invs(RNF,OldInvs,NewInvs,NewRNF),
        b5sort_unify(OldInvs,Invs,NewInvs), !.

t5rnf_insert_neg_invs(RNF,NegInvs,NewRNF) :- 
	t5rnf_r_neg_invs(RNF,OldNegInvs,NewNegInvs,NewRNF),
        b5sort_unify(OldNegInvs,NegInvs,NewNegInvs), !.

t5rnf_insert_transs(RNF,Transs,RNFNew) :- 
	t5rnf_r_transs(RNF,TransOld,TransNew,RNFNew),
	b5sort_unify(TransOld,Transs,TransNew), !.

t5rnf_insert_comps(RNF,Comps,RNFNew) :-
	t5rnf_r_comps(RNF,Comps1,Comps2,RNFNew),
	append(Comps,Comps1,Comps2), !.



/* normalize */
/*
t5rnf_normalize_inv(rnf(DomainKey,RangeKey,_,_,_,_,_,_),ErrorTyp) :-
	(t5tbox_all_key(RangeKey),
	RangeKey \== DomainKey,
	ErrorTyp = unspecified_range;
	t5tbox_anything_key(Anything),
	t5concid_disjoint_p(Anything,RangeKey),
	ErrorTyp = disjoint_anything_range),
	!.
*/
t5rnf_normalize_inv(rnf(DK,RK,P,NP,I,NI,T,C),rnf(RK,DK,I,NI,P,NP,InvT,InvC)) :-
	t5rnf_inv_trans(T,[],InvT),
	t5rnf_inv_comp(C,[],InvC).

%% It is checked if the role is invertable before t5rnf_inv_trans is called.

t5rnf_inv_trans([],Tlist,Tlist).

t5rnf_inv_trans([T1|Tlist],Accold,NewTlist) :-
	t5role_inv_role(T1,InvT1),
	b5sort_unify([InvT1],Accold,Accnew),
	t5rnf_inv_trans(Tlist,Accnew,NewTlist).

%% It is checked if the role is invertable before t5rnf_inv_comp is called.

t5rnf_inv_comp([],Comps,Comps).

t5rnf_inv_comp([COMP|Comps],Accold,InvComps) :-
	t5comp_inv_comp(COMP,InvCOMP),
	t5rnf_inv_comp(Comps,[InvCOMP|Accold],InvComps).

% ************************************************************************
% ************************************************************************
/*
t5rnf_unify(nf(D1,R1,[Anyrole],[],[Anyrole],[],[],[]),Rnf2,RNF) :-
	t5tbox_anyrole_key(Anyrole),
	!,
	t5rnf_add_domain(Rnf2,D1,Rnf21),
	t5rnf_add_range(Rnf21,R1,RNF).
*/
t5rnf_unify(Rnf1,Rnf2,RNF) :-
        t5rnf_raw_create(DK1,RK1,P1,NP1,I1,NI1,T1,C1,Rnf1),
        t5rnf_raw_create(DK2,RK2,P2,NP2,I2,NI2,T2,C2,Rnf2),
        t5concid_unify(DK1,DK2,DK3),
        t5concid_unify(RK1,RK2,RK3),
        b5sort_unify(P1,P2,P3),
        b5sort_unify(NP1,NP2,NP3),
        b5sort_unify(I1,I2,I3),
        b5sort_unify(NI1,NI2,NI3),
        t5rnf_raw_create(DK3,RK3,P3,NP3,I3,NI3,T1,C1,Rnf4),
        t5rnf_insert_transs(Rnf4,T2,Rnf5),
        t5rnf_insert_comps(Rnf5,C2,Rnf6),
	t5comp_add_domain(Rnf6,DK3,Rnf7),
	t5comp_add_range(Rnf7,RK3,RNF).
	
% ************************************************************************
% ************************************************************************

t5rnf_compare(Sub,BottomNF,NF2,NewSub) :-
	t5tbox_bottom_key(conc,Bottom),
	t5rnf_domain(BottomNF,Bottom),
	!,
	(t5rnf_domain(NF2,Bottom),
	 !,
	 t5sub_unify(Sub,equal,NewSub);
	   t5sub_unify(Sub,first,NewSub)).

t5rnf_compare(Sub,_,BottomNF,NewSub) :-
	t5rnf_domain(BottomNF,Domain),
	t5tbox_bottom_key(conc,Domain),
	!,
	t5sub_unify(Sub,second,NewSub).

t5rnf_compare(Sub,RNF1,RNF2,NewSub) :-
	Sub \== none,
        t5rnf_raw_create(DK1,RK1,P1,NP1,I1,NI1,_,C1,RNF1),
        t5rnf_raw_create(DK2,RK2,P2,NP2,I2,NI2,_,C2,RNF2),
	t5concid_compare(Sub,DK1,DK2,Sub1),
	Sub1 \== none,
	t5concid_compare(Sub1,RK1,RK2,Sub2),
	Sub2 \== none,
        b5sort_compare(Sub2,P1,P2,Sub3),
	Sub3 \== none,
        b5sort_compare(Sub3,NP1,NP2,Sub4),
	Sub4 \== none,
        b5sort_compare(Sub4,I1,I2,Sub5),
	Sub5 \== none,
        b5sort_compare(Sub5,NI1,NI2,Sub6),
	Sub6 \== none,
	t5rnf_trans_compare(Sub6,RNF1,RNF2,Sub7),
	Sub7 \== none,
	t5rnf_comp_compare(Sub7,C1,C2,NewSub), !.

t5rnf_compare(_,_,_,none).

% ************************************************************************

t5rnf_compare_without_prims(Sub,RNF1,RNF2,NewSub) :- 
	Sub \== none,
        t5rnf_raw_create(DK1,RK1,_,NP1,I1,NI1,_,C1,RNF1),
        t5rnf_raw_create(DK2,RK2,_,NP2,I2,NI2,_,C2,RNF2),
	t5concid_compare(Sub,DK1,DK2,Sub1),
	Sub1 \== none,
	t5concid_compare(Sub1,RK1,RK2,Sub3),
	Sub3 \== none,
        b5sort_compare(Sub3,NP1,NP2,Sub4),
	Sub4 \== none,
        b5sort_compare(Sub4,I1,I2,Sub5),
	Sub5 \== none,
        b5sort_compare(Sub5,NI1,NI2,Sub6),
	Sub6 \== none,
	t5rnf_trans_compare(Sub6,RNF1,RNF2,Sub7),
	Sub7 \== none,
	t5rnf_comp_compare(Sub7,C1,C2,NewSub), !.

t5rnf_compare_without_prims(_,_,_,none).

% ************************************************************************

t5rnf_trans_compare(second,RNF1,RNF2,second) :-
	t5rnf_transs(RNF1,Transs1),
	t5rnf_trans_subsumes_p(Transs1,RNF2),
	!.

t5rnf_trans_compare(first,RNF1,RNF2,first) :-
	t5rnf_transs(RNF2,Transs2),
	t5rnf_trans_subsumes_p(Transs2,RNF1),
	!.

t5rnf_trans_compare(equal,RNF1,RNF2,Subneu) :-
	t5rnf_transs(RNF1,Transs1),
	t5rnf_trans_subsumes_p(Transs1,RNF2),
	!,
	(t5rnf_transs(RNF2,Transs2),
	t5rnf_trans_subsumes_p(Transs2,RNF1),
	 !,
	 Subneu = equal;
	    Subneu = second).

t5rnf_trans_compare(equal,RNF1,RNF2,first) :-
	t5rnf_transs(RNF2,Transs2),
	t5rnf_trans_subsumes_p(Transs2,RNF1),
	!.

t5rnf_trans_compare(_,_,_,none).

t5rnf_comp_compare(second,C1,C2,second) :-
	t5comp_subsumes_p(C1,C2),
	!.

t5rnf_comp_compare(first,C1,C2,first) :-
	t5comp_subsumes_p(C2,C1),
	!.

t5rnf_comp_compare(equal,C1,C2,Subneu) :-
	t5comp_subsumes_p(C1,C2),
	!,
	(t5comp_subsumes_p(C2,C1),
	 !,
	 Subneu = equal;
	    Subneu = second).

t5rnf_comp_compare(equal,C1,C2,first) :-
	t5comp_subsumes_p(C2,C1),
	!.

t5rnf_comp_compare(_,_,_,none).

% ************************************************************************

t5rnf_subsumes_p(RNF1,RNF2) :-
        t5rnf_raw_create(DK1,RK1,P1,NP1,I1,NI1,T1,C1,RNF1),
        t5rnf_raw_create(DK2,RK2,P2,NP2,I2,NI2,_,C2,RNF2),
	t5concid_subsumes_p(DK1,DK2),
	t5concid_subsumes_p(RK1,RK2),
        b5sort_subset_p(P1,P2),
        b5sort_subset_p(NP1,NP2),
        b5sort_subset_p(I1,I2),
        b5sort_subset_p(NI1,NI2),
	t5rnf_trans_subsumes_p(T1,RNF2),
	t5comp_subsumes_p(C1,C2),
	!.

t5rnf_subsumes_p(_,BottomNF) :-
	t5rnf_domain(BottomNF,Bottom),
	t5tbox_bottom_key(conc,Bottom),
	!.

t5rnf_subsumes_p(_,BottomNF) :-
	t5rnf_range(BottomNF,Bottom),
	t5tbox_bottom_key(conc,Bottom),
	!.

t5rnf_subsumes_p(_,RNF2) :-
	t5tbox_nothing_key(Nothing),
	t5rnf_prims(RNF2,P2),
	b5sort_member_p(Nothing,P2),
        !.

t5rnf_subsumes_without_domain_range_p(RNF1,RNF2) :-
        t5rnf_raw_create(_,_,P1,NP1,I1,NI1,T1,C1,RNF1),
        t5rnf_raw_create(_,_,P2,NP2,I2,NI2,_,C2,RNF2),
	t5tbox_nothing_key(Nothing),
	((b5sort_member_p(Nothing,P2),
	  !;
	  b5sort_member_p(Nothing,P2)),
	 !;
	 b5sort_subset_p(P1,P2),
         b5sort_subset_p(NP1,NP2),
	 b5sort_subset_p(I1,I2),
         b5sort_subset_p(NI1,NI2),
	 t5rnf_trans_subsumes_p(T1,RNF2),
	 t5comp_subsumes_p(C1,C2),     %%% EIGENTLICH without_domain_range
	 !).

t5rnf_trans_subsumes_p([],_).

t5rnf_trans_subsumes_p([Trans|Tlist],RNF) :-
	t5rnf_transs(RNF,Transs),
	t5rnf_sub_member(Trans,Transs),
	!,
	t5rnf_trans_subsumes_p(Tlist,RNF).

t5rnf_trans_subsumes_p([Trans|Tlist],RNF) :-
	t5role_normal_form(Trans,TransNF),
	t5rnf_subsumes_p(TransNF,RNF),
	!,
	t5rnf_trans_subsumes_p(Tlist,RNF).

t5rnf_trans_subsumes_p([Trans|Tlist],RNF) :-
	t5rnf_comps(RNF,Comps),
	t5comp_trans_comps_subsumes_p(Trans,Comps),
	!,
	t5rnf_trans_subsumes_p(Tlist,RNF).

t5rnf_sub_member(_,[]) :- !, fail.

t5rnf_sub_member(Trans1,[Trans2|Tlist]) :-
	t5tbox_role_hc(HC),
	(t5hc_subsumes_p(HC,Trans1,Trans2),
	 !;
	t5rnf_sub_member(Trans1,Tlist)).

% ************************************************************************

t5rnf_incoherent_p(Rnf) :-
        t5rnf_raw_create(DK,RK,P,NP,I,NI,_,_,Rnf),
	t5tbox_nothing_key(Nothing),
	(b5sort_member_p(Nothing,P);
	 b5sort_member_p(Nothing,I);
	 t5concid_incoherent_p(RK);
	 t5concid_incoherent_p(DK);
	 b5sort_common_member_p(P,NP);
	 b5sort_common_member_p(I,NI)),
	 !.     /* ??trans,comp?? */

% ************************************************************************

t5rnf_disjoint_p(Rnf1,Rnf2) :-
	t5rnf_raw_create(DK1,RK1,P1,NP1,I1,NI1,_,_,Rnf1),
	t5rnf_raw_create(DK2,RK2,P2,NP2,I2,NI2,_,_,Rnf2),
	(t5concid_disjoint_p(DK1,DK2);
	t5concid_disjoint_p(RK1,RK2);
	b5sort_common_member_p(P1,NP2); %uk 2-12-92
	b5sort_common_member_p(P2,NP1); %uk
	b5sort_common_member_p(I1,NI2); %uk
	b5sort_common_member_p(I2,NI1)),
	!.

% ************************************************************************
% ************************************************************************

t5rnf_raw_create(Domain,Range,Prims,NegPrims,Invs,NegInvs,Transs,Comps,
                rnf(Domain,Range,Prims,NegPrims,Invs,NegInvs,Transs,Comps)).

% ************************************************************************
    
%% r * componente * componente -> rnf
%% replace one old component with a new one

t5rnf_r_domain(rnf(DomainKey,RK,P,NP,I,NI,T,C),DomainKey,NewDomainKey,
                     rnf(NewDomainKey,RK,P,NP,I,NI,T,C)) :- !.
t5rnf_r_range(rnf(DK,RangeKey,P,NP,I,NI,T,C),RangeKey,NewRangeKey,
                     rnf(DK,NewRangeKey,P,NP,I,NI,T,C)) :- !.
t5rnf_r_prims(rnf(DK,RK,Prims,NP,I,NI,T,C),Prims,NewPrims,
                     rnf(DK,RK,NewPrims,NP,I,NI,T,C)) :- !.
t5rnf_r_neg_prims(rnf(DK,RK,P,NegPrims,I,NI,T,C),NegPrims,NewNegPrims,
                     rnf(DK,RK,P,NewNegPrims,I,NI,T,C)) :- ! .
t5rnf_r_invs(rnf(DK,RK,P,NP,Invs,NI,T,C),Invs,NewInvs,
                     rnf(DK,RK,P,NP,NewInvs,NI,T,C)) :- !.
t5rnf_r_neg_invs(rnf(DK,RK,P,NP,I,NegInvs,T,C),NegInvs,NewNegInvs,
                     rnf(DK,RK,P,NP,I,NewNegInvs,T,C)) :- !.
t5rnf_r_transs(rnf(DK,RK,P,NP,I,NI,Transs,C),Transs,NewTranss,
                     rnf(DK,RK,P,NP,I,NI,NewTranss,C)) :- !.
t5rnf_r_comps(rnf(DK,RK,P,NP,I,NI,T,Comps),Comps,NewComps,
                     rnf(DK,RK,P,NP,I,NI,T,NewComps)) :- !.

% ************************************************************************

t5rnf_bottom_nf(Norole) :-
	t5tbox_bottom_key(role,Bottomrole),
	t5tbox_bottom_key(conc,Bottomconc),
	t5rnf_create(RNF),
	t5rnf_add_prim(RNF,Bottomrole,RNF1),
	t5rnf_add_domain(RNF1,Bottomconc,RNF2),
	t5rnf_add_range(RNF2,Bottomconc,Norole).

% ***********************************************************************
t5rnf_feature_p(RNF) :-
	(t5rnf_feature_prim_p(RNF),
	 !;
	    t5rnf_feature_comp_p(RNF)).

t5rnf_feature_prim_p(RNF) :-
	t5rnf_prims(RNF,Prims),
	t5tbox_feature_prim_key(FeaturePrim),
	b5sort_member_p(FeaturePrim,Prims).

t5rnf_feature_comp_p(RNF) :-
	t5rnf_comps(RNF,Comps),
	t5comp_feature_p(Comps).

t5rnf_add_feature(RNF,NewRNF,Supers,NewSupers,Flag,NewFlag) :-
	t5rnf_check_feature(Flag,RNF,No_Yes_Inv_Both),
	t5rnf_feature_rnf(No_Yes_Inv_Both,RNF,NewRNF,Flag,NewFlag),
	t5rnf_feature_supers(No_Yes_Inv_Both,Supers,NewSupers).

t5rnf_feature_supers(yes,Supers,NewSupers) :-
	t5tbox_feature_prim_key(FeaturePrim),
	b5sort_unify([FeaturePrim],Supers,NewSupers).

t5rnf_feature_supers(both,Supers,NewSupers) :-
	t5tbox_feature_prim_key(FeaturePrim),
	b5sort_unify([FeaturePrim],Supers,NewSupers).

t5rnf_feature_supers(inv,Supers,Supers).

t5rnf_feature_supers(no,Supers,Supers).


t5rnf_feature_rnf(no,RNF,RNF,d,d).

t5rnf_feature_rnf(yes,RNF,NewRNF,_,f) :-
	t5tbox_feature_prim_key(FeaturePrim),
	t5rnf_add_prim(RNF,FeaturePrim,NewRNF).

t5rnf_feature_rnf(inv,RNF,NewRNF,_,d) :-
	t5tbox_feature_prim_key(FeaturePrim),
	t5rnf_insert_invs(RNF,[FeaturePrim],NewRNF).

t5rnf_feature_rnf(both,RNF,NewRNF,_,f) :-
	t5tbox_feature_prim_key(FeaturePrim),
	t5rnf_add_prim(RNF,FeaturePrim,RNF1),
	t5rnf_insert_invs(RNF1,[FeaturePrim],NewRNF).


t5rnf_check_feature(f,rnf(_,_,_,_,Invs,_,_,Comps),No_Yes_Inv_Both) :-
	 (t5rnf_check_feature_inv(Invs,Comps),
	  !,
	  No_Yes_Inv_Both = both;
	      No_Yes_Inv_Both = yes).

t5rnf_check_feature(d,rnf(_,_,Prims,_,Invs,_,_,Comps),No_Yes_Inv_Both) :-
	(t5rnf_check_feature_yes(Prims,Comps),
	 !,
	 (t5rnf_check_feature_inv(Invs,Comps),
	  !,
	  No_Yes_Inv_Both = both;
	      No_Yes_Inv_Both = yes);
	     t5rnf_check_feature_inv(Invs,Comps),
	     !,
	     No_Yes_Inv_Both = inv;
	        No_Yes_Inv_Both = no).
	     

t5rnf_check_feature_yes(Prims,Comps) :-
	t5tbox_feature_prim_key(FeaturePrim),
	(b5sort_member_p(FeaturePrim,Prims),
	 !;
	    t5comp_feature_p(Comps)).

t5rnf_check_feature_inv(Invs,Comps) :-
	t5tbox_feature_prim_key(FeaturePrim),
	(b5sort_member_p(FeaturePrim,Invs),
	 !;
	    t5comp_inv_feature_p(Comps)).

% ***********************************************************************

% LAST EDIT: Fri Jan 29 14:46:04 1993 by Mirjam Kuehne (kit!mir) 
% LAST EDIT: Fri Jan 22 18:52:08 1993 by Mirjam Kuehne (anfall!mir) 
% ************************************************************************
% *                       ROLE                                           *
% ************************************************************************

% EXPORT-predicates: 
% tell              t5role_add_comp( +RPF, + RKey, + RKey, - RPF)
% tell              t5role_add_domain( + RPF, + CKey, - RPF)
% tell              t5role_add_inv( + RPF, +RKey, -RPF)
% tell              t5role_add_neg_inv( + RPF, +RKey, -RPF)
% tell              t5role_add_neg_prim( + RPF, +RKey, -RPF)
% tell              t5role_add_prim( + RPF, +RKey, -RPF)
% tell              t5role_add_range( + RPF, + CKey, -RPF)
% tell              t5role_add_role( + RPF, +RKey, -RPF)
% tell              t5role_add_trans( + RPF, +RKey, -RPF)
% tell              t5role_add_filter( + RKey, + Filter)             ?? uwe
% ask               t5role_attribute_p( + RKey)
% tell              t5role_bottom( + Key)
% tell              t5role_changed_status( + RKey, + RKey)           ?? uwe
% ask               t5role_compare( + Sub, + RKey, + RKey, - Sub)
% ask               t5role_compare_without_prims( + Sub, + RKey, + RKey, - Sub)
% ask               t5role_comps( + RKey, - Complist)
% ask               t5role_defined_p( + RKey)
% ask hc            t5role_direct_subs( + RKey, + Filter, - D_Sublist)
% ask hc            t5role_direct_subs( + RKey, + Filter, - D_Sublist)
% ask hc            t5role_direct_supers( + RKey, - D_Superlist)
% ask hc            t5role_direct_supers( + RKey, - D_Superlist)
% ask               t5role_disjoint_p( + RKey, + RKey)
% ask hc&rdb        t5role_disjoints( + RKey, - Disjointlist)
% ask               t5role_domain( + RKey, - DomainKey)
%                   t5role_downward_hereditary_p(+RKey, +RKeyList, +CNF, +CKey)
% ask               t5role_feature_p( + RKey)
% rdb only          t5role_filter( + Role, - Filter)
% ask               t5role_filter_holds_p( + RKey, + Filter)
% rdb only          t5role_flag( + Role, - Flag)
% ask               t5role_ibox_disjoint_p( + RKey, + RKey)
% ask               t5role_ibox_subsumes_p( + RKey, + RKey)
% ask               t5role_incoherent_p( + RKey)
% tell intern       t5role_inv_role( + RKey, - RKey)
% ask               t5role_invs( + RKey, - Inv_Primlist)
% rdb only          t5role_key( + Role, - RKey)
% ask hc            t5role_known_disjoints( + RKey, - Disjointlist)
% ask               t5role_neg_invs( + RKey, - NegInv_Primlist)
% ask               t5role_neg_prims( + RKey, - Neg_Primlist)
% tell              t5role_new_prim( - RKey)
% tell              t5role_new_defined_role( - RPF)
% tell              t5role_new_primitive_role( - RPF)
% tell              t5role_new_role( - RPF)
% ask hc            t5role_non_disjoints( + RKey, - Non_Disjointlist)
% ask hc            t5role_possible_disjoints( + RKey, - P_Disjointlist)
% ask               t5role_primitive_p( + RKey)
% ask               t5role_prims( + RKey, - Primlist)
% ask               t5role_range( + RKey, - RangeKey)
% ask               t5role_range_type_key( + RKey, - CKey)
% ask               t5role_remove( + RKey)
% rdb only          t5role_rnf( + Role, - RNF)
% ask               t5role_normal_form( + RKey, - RNF)
% tell              t5role_store( + RPF, + Filter, - RKey, - old/new)
% tell intern       t5role_store_nf( + RNF, - RKey)
% ask hc            t5role_subs( + RKey, - Sublist)
% ask               t5role_subsumes_p( + RKey, + RKey)
% ask hc            t5role_sub_intersection( + Roles, - Subs)
% ask hc            t5role_sub_union( + Roles, - Subs)
% ask hc            t5role_supers( + RKey, - Superlist)
% tell              t5role_top( + Key)
%                   t5role_topmost_def_p( + RKey, + RKey)
% ask               t5role_transs( + RKey, - Translist)
% ask               t5role_unify( + RKey, + RKey, - RKey)

% IMPORT-predicates:

%                   b5nf_subsumes_p(CNF,CNF)

%                   b5sort_subset_p(List,List)


%                   i5ibox_get_ibox_conc( + CKey, - CKey)


%                   t5cls_class(role,RNF,Filter,List,List,List,
%                                                   List,List,Key,old/new)

%                   t5concid_normal_form(CKey,CNF)
%                   t5concid_subsumes_p(CKey,CKey)
%                   t5concid_type_key(CKey,CKey)

%                   t5fil_add(FilterName,Filter,Filter)
%                   t5fil_create(Filter)
%                   t5fil_filter(FilterName,Filter)
%                   t5fil_holds_p(Filter,Filter)
%                   t5fil_unify(Filter,Filter,Filter)

%                   t5hc_direct_subs(HC,RKey,D_Sublist)
%                   t5hc_direct_subs(HC,RKey,Filter,D_Sublist)
%                   t5hc_direct_supers(HC,RKey,D_Superlist)
%                   t5hc_direct_supers(HC,RKey,Filter,D_Superlist)
%                   t5hc_disjoints(HC,RKey,Disjointlist)
%                   t5hc_infimum(RKey,Rkey,RKey)
%                   t5hc_insert(HC,Key,Superlist,Sublist,Disjointlist)
%                   t5hc_insert_infimum(RKey,Rkey,Rkey)
%                   t5hc_non_disjoints(HC,RKey,Non_Disjointlist)
%                   t5hc_possible_disjoints(HC,RKey,P_Disjointlist)
%                   t5hc_subs(HC,RKey,Sublist)
%                   t5hc_sub_intersection(RKey,Subs)
%                   t5hc_sub_union(Roles,Subs)
%                   t5hc_supers(HC,RKey,Superlist)

%                   t5rdb_attribute_p(+ RKey)
%                   t5rdb_comps(+ RKey, - Comps)
%                   t5rdb_defined_p(+ RKey)
%                   t5rdb_feature_p(+ RKey)
%                   t5rdb_filter(+ RKey, - Filter)
%                   t5rdb_inherent_feature_p(+ RKey)
%                   t5rdb_init
%                   t5rdb_inv(+ RKey, - RKey)
%                   t5rdb_new_filter(+ RKey, + Filter)
%                   t5rdb_new_inv(+ RKey, + RKey)
%                   t5rdb_new_comps(+ RKey, + Comps)
%                   t5rdb_primitive_p(+ RKey)
%                   t5rdb_remove(+ RKey)
%                   t5rdb_rnf(+ RKey, - RNF)
%                   t5rdb_rnf(+ RKey, - RNF, - Filter)
%                   t5rdb_store(+ Role)
%                   t5rdb_store(+ Role, + Key, + yes/no)

%                   t5rnf_add_comp(RNF,RKey,RKey,RNF)
%                   t5rnf_add_domain(RNF,CKey,RNF)
%                   t5rnf_add_inv(RNF,RKey,RNF)
%                   t5rnf_add_neg_invs(RNF,RKey,RNF)
%                   t5rnf_add_neg_prims(RNF,RKey,RNF)
%                   t5rnf_add_prim(RNF,RKey,RNF)
%                   t5rnf_add_range(RNF,CKey,RNF)
%                   t5rnf_add_trans(RNF,RKey,RNF)
%                   t5rnf_compare(Sub,RNF,RNF,Sub)
%                   t5rnf_compare_without_prims(Sub,RNF,RNF,Sub)
%                   t5rnf_comps(RNF,Complist)
%                   t5rnf_compute_inv(RNF,unknown/notexists)
%                   t5rnf_create(RNF)
%                   t5rnf_domain(RNF,CKey)
%                   t5rnf_incoherent_p(RKey)
%                   t5rnf_invs(RNF,Invlist)
%                   t5rnf_neg_invs(RNF,Neg_Invlist)
%                   t5rnf_neg_prims(RNF,Neg_Primlist)
%                   t5rnf_prims(RNF,Primlist)
%                   t5rnf_range(RNF,CKey)
%                   t5rnf_subsumes_p(RKey,RKey)
%                   t5rnf_subsumes_without_domain_range_p(RNF,RNF)
%                   t5rnf_transs(RNF,Translist)
%                   t5rnf_unify(RNF,RNF,RNF)

%                   t5tbox_anyrole_key(Key)
%                   t5tbox_bottom_key(Key)
%                   t5tbox_nothing_key(Key)
%                   t5tbox_next_key(Key)
%                   t5tbox_role_hc(HC)

% ***********************************************************************
% ***********************************************************************

% Hierarchy informations are taken from the role hierarchy cache.

t5role_supers(RKey,Supers) :-
	t5tbox_role_hc(HC),
	t5hc_supers(HC,RKey,Supers).

t5role_subs(RKey,Subs) :-
	t5tbox_role_hc(HC),
	t5hc_subs(HC,RKey,Subs).

t5role_direct_supers(RKey,D_Supers) :-
	t5tbox_role_hc(HC),
	t5hc_direct_supers(HC,RKey,D_Supers).

t5role_direct_supers(RoleKey,Filter,Supers) :-
	t5tbox_role_hc(HC),
	t5hc_direct_supers(HC,RoleKey,Filter,Supers).

t5role_direct_subs(RKey,D_Subs) :-
	t5tbox_role_hc(HC),
	t5hc_direct_subs(HC,RKey,D_Subs).

t5role_direct_subs(RoleKey,Filter,Subs) :-
	t5tbox_role_hc(HC),
	t5hc_direct_subs(HC,RoleKey,Filter,Subs).

t5role_known_disjoints(RKey,Disj) :-
	t5tbox_role_hc(HC),
	t5hc_known_disjoints(HC,RKey,Disj).

t5role_disjoints(C,AllDisj) :-
        t5tbox_role_hc(HC),
        t5hc_possibly_disjoints(HC,C,PossDisj),
        t5hc_known_disjoints(HC,C,KnownDisj),
	t5hc_sub_union_id(HC,KnownDisj,MoreDis),
        %t5hc_non_disjoints(HC,C,NonDisj),
	t5role_test_disj(PossDisj,C,HC,RealDisj),
	b5sort_unify(MoreDis,RealDisj,AllDisj).
	
t5role_test_disj([],_C,_HC,[]).
t5role_test_disj([Test|Tests],C,HC,RealDisj) :-
	(
	t5role_disjoint_p(unknown,HC,Test,C) ->
		%t5hc_subs(HC,Test,Subs),
		b5kif_subsx(Test,Subs),
		b5sort_difference(Tests,Subs,LessTests),
		RealDisj = [Test|TailReal]
	;
		t5hc_supers(HC,Test,Supers),
		b5sort_difference(Tests,Supers,LessTests),
		RealDisj = TailReal
	),
	t5role_test_disj(LessTests,C,HC,TailReal).

t5role_non_disjoints(RKey,NonDisj) :-
	t5tbox_role_hc(HC),
	t5hc_non_disjoints(HC,RKey,NonDisj).

t5role_possibly_disjoints(RKey,PossDisj) :-
	t5tbox_role_hc(HC),
	t5hc_possibly_disjoints(HC,RKey,PossDisj).

% ***********************************************************************
% ***********************************************************************

%% Stored information from t5rdb. Parts of the normal form of a role can be
%% accessed with these predicates.

% Normalform

t5role_domain(RKey,DomainKey) :-
	t5rdb_rnf(RKey,RNF,_),
	t5rnf_domain(RNF,DomainKey).
t5role_range(RKey,RangeKey) :-
	t5rdb_rnf(RKey,RNF,_),
	t5rnf_range(RNF,RangeKey).
t5role_prims(RKey,Prims) :-
	t5rdb_rnf(RKey,RNF,_),
	t5rnf_prims(RNF,Prims).
t5role_neg_prims(RKey,NegPrims) :-
	t5rdb_rnf(RKey,RNF,_),
	t5rnf_neg_prims(RNF,NegPrims).
t5role_invs(RKey,Invs) :-
	t5rdb_rnf(RKey,RNF,_),
	t5rnf_invs(RNF,Invs).
t5role_neg_invs(RKey,NegInvs) :-
	t5rdb_rnf(RKey,RNF,_),
	t5rnf_neg_invs(RNF,NegInvs).
t5role_transs(RKey,Transs) :-
	t5rdb_rnf(RKey,RNF,_),
	t5rnf_transs(RNF,Transs).
t5role_comps(RKey,Comps) :-
	t5rdb_rnf(RKey,RNF,_),
	t5rnf_comps(RNF,Comps).

%% The type (concept, number, aset, ...) of th e range of a role is given.
t5role_range_type_key(RKey,TypeKey) :-
	t5rdb_rnf(RKey,RNF,_),
	t5rnf_range(RNF,RangeKey),
	t5concid_type_key(RangeKey,TypeKey).

%% These Predicates checks the role flag (primitive, defined, feature,
%% inherent feature). Note that no additional inferences for features are
%% drawn within the TBox in the current implementation.

t5role_primitive_p(Key) :-
	t5rdb_primitive_p(Key).
t5role_defined_p(Key) :-
	t5rdb_defined_p(Key).
t5role_attribute_p(Key) :-                      % loeschen
	t5rdb_attribute_p(Key).
t5role_inherent_feature_p(Key) :-
	t5rdb_inherent_feature_p(Key).
t5role_feature_p(Key) :-
	t5rdb_feature_p(Key).

% filter

%t5role_filter(RKey,RoleFilter) :-
%       t5rdb_filter(RKey,RoleFilter).

%% t5role_filter_holds_p(RKey,RoleFilter) checks whether filter RoleFilter
%% holds for the role with key RKey.
t5role_filter_holds_p(RKey,Filter) :-
        t5rdb_filter(RKey,RoleFilter),
	t5fil_holds_p(Filter,RoleFilter),
	!.

%% t5role_add_filter adds a new filter to a role. It is relevant e.g. when a
%% user tries to indruduce a role that has been introduced internal before.g
t5role_add_filter(RKey,Filter) :-	% uk
        t5rdb_filter(RKey,RoleFilter),
	t5fil_unify(Filter,RoleFilter,NewRoleFilter),
	t5rdb_new_filter(RKey,NewRoleFilter).
	

t5role_changed_status(RKey,RKey) :- !.
t5role_changed_status(RKey,_) :- 
	t5fil_filter(revised,Filter),
	t5role_add_filter(RKey,Filter).


% *********************************************************************
% *********************************************************************

% Introductions

%% t5role_new_prim( - Key) introduces a new primitive role component and
%% returns its key.

t5role_new_prim(Key) :-
	t5tbox_next_key(Key),
	t5out_trace(storing_new_prim_role(Key)),
	t5tbox_role_hc(HC),
	t5tbox_anyrole_key(Anyrole),
	t5tbox_nothing_key(Nothing),
	t5hc_insert(HC,Key,[Anyrole],[Nothing],[]),
	t5rnf_create(RNF),
	t5rnf_add_prim(RNF,Key,NewRNF),                     % !!!
	t5fil_filter(internal,Filter),
	t5role_raw_create(Key,p,NewRNF,Filter,Role),
	t5rdb_store(Role,prim,no).

%% t5role_top( + Key) introduces the top role with key Key.
t5role_top(Key) :-
	t5rnf_create(RNF),
	t5rnf_add_prim(RNF,Key,RNF1),
	t5rnf_add_inv(RNF1,Key,NewRNF),                  % ???
	t5fil_create(EmptyFilter),
	t5fil_add(predef,EmptyFilter,Filter),		%uk
	t5role_raw_create(Key,p,NewRNF,Filter,Role),
	t5rdb_store(Role,Key,no).

%% t5role_bottom( + Key) introduces the bottom role with key Key.
t5role_bottom(Key) :-
	t5rnf_create(RNF),
	t5rnf_add_prim(RNF,Key,RNF1),
	t5rnf_add_inv(RNF1,Key,RNF2),
	t5tbox_bottom_key(conc,ConcBottom),
	t5rnf_add_domain(RNF2,ConcBottom,RNF3),
	t5rnf_add_range(RNF3,ConcBottom,RNF4),
	t5fil_create(EmptyFilter),
	t5role_raw_create(Key,p,RNF4,EmptyFilter,Role),
	t5rdb_store(Role,Key,no).

% ***********************************************************************

%% Storage of a role:
%% (1) If the range of the role is unknown, anything is added as the default.
%% (2) Then the role is classified. If the role exists (old), nothing is done, 
%% (3) For each role is checked, whether its inverse exists. 
%% (4) If the role exists (old), only the filter is updated.
%% (5) Else the role is inserted into the hierarchy cache,
%% Additional information is computed and stored into the role data base
%% (6) It is checked whether the role is transitive (R == trans(R1).
%% (7) If the role is a composition (R1 comp ... comp Rn), the comp information
%%     is computed and inserted at the involved roles.
%% (8) If the role is new it is stored stored in the role data base.
%%     We store rdb(Key,r/f/i,RNF,inv info, trans info, comp info)
%%     inv info is unknown or notexists, if it is unknown it is updated when
%%     the inverse role is introduced.
%%     trans info is yes or no
%%     comp info is empty. If the role is involved in a composition this slot
%%     is filled,  when the composition itself is stored.
%% (9) For each role its inverse is introduced, if it exists.
%% (10 After the introduction of a new role concepts and objects has to be
%%     recompleted.

%% roles, features or inherent_features are marked with d, f, or i.
%% now inherent_features same as features 
t5role_store(RPF,Filter,Key,New_or_Old) :-
	t5role_store(RPF,Filter,d,Key,New_or_Old).

t5role_store_role(RPF,Filter,Key,New_or_Old) :-
	t5role_store(RPF,Filter,d,Key,New_or_Old).

t5role_store_feature(RPF,Filter,Key,New_or_Old) :-
	t5role_store(RPF,Filter,f,Key,New_or_Old).

t5role_store_inherent_feature(RPF,Filter,Key,New_or_Old) :-
	t5role_store(RPF,Filter,f,Key,New_or_Old).
%	t5role_store(RPF,Filter,i,Key,New_or_Old).


% ************************************************************************

t5role_store(RPF,Filter,Flag,Key,New_or_Old) :-
	t5out_trace(storing_role),
	t5role_create_rpf(RNF,Supers,RPF),
	t5rnf_add_feature(RNF,RNF0,Supers,Supers0,Flag,Flag0),
	t5rnf_range(RNF0,Range),
	t5tbox_anything_key(Anything),
                                                      %% (1)
	(t5tbox_all_key(Range),
	 !,
	 t5rnf_add_range(RNF0,Anything,RNF1);
	    RNF1 = RNF0),
                                                      %% (2)
	t5cls_class(role,RNF1,Filter,Supers0,[],
                                   AllSupers,AllSubs,Disj,Key,New_or_Old),
                                                      %% (3)
	t5rnf_compute_inv(RNF1,Inv),
                                                      %% (4) -- (10)
	t5role_store_internal(New_or_Old,Key,Filter,RNF1,Inv,Flag0,
                                                 AllSupers,AllSubs,Disj),
                                                      %% range info
	(t5tbox_all_key(Range),
	 !,
	 t5out_warning(default_range(Anything,Key));
	    t5out_info(range(Range,Key))).

                                                      %% (4)
t5role_store_internal(old,Key,NewFilter,_,_,_,_,_,_) :-
	t5out_trace(role(old,Key)),
	(t5tbox_bottom_key(role,Key),
	 t5out_warning(incoherent_role),
	 !;
	    t5rdb_filter(Key,OldFilter),
	    t5fil_unify(NewFilter,OldFilter,Filter),
	    t5rdb_new_filter(Key,Filter)).

t5role_store_internal(new,Key,Filter,RNF,Inv,Flag,AllSupers,AllSubs,Disj) :-
                                                      %% (5)
	t5tbox_next_key(Key),
	t5out_trace(role(new,Key)),
	(Inv == notexists,
	 !,
	 InvKey = Inv;
	    t5tbox_next_key(InvKey)),
	t5role_store_new(Key,Filter,RNF,InvKey,Flag,AllSupers,AllSubs,Disj),
                                                      %% (11)
	(t5fil_holds_p(user_primitive,Filter),
	 !,
	 t5role_new_inv_role(prim,Key,InvKey);
	    t5role_new_inv_role(def,Key,InvKey)),
                                                      %% (8)
	t5comp_handle_comps(RNF,Key).


t5role_store_new(Key,Filter,RNF,Inv,Flag,AllSupers,AllSubs,Disj) :-
	t5tbox_role_hc(HC),
	t5hc_insert(HC,Key,AllSupers,AllSubs,Disj),
	t5role_raw_create(Key,Flag,RNF,Filter,Role),
                                                      %% (6)
	(t5rnf_trans_p(RNF),
	 !,
                                                      %% (7)
	 t5rdb_store(Role,Inv,yes);
	    t5rdb_store(Role,Inv,no)),
       t5tbox_update_pure_defs(role,Key),            %% wg jjq
                                                      %% (9)
       t5cdb_include_role(Key,AllSupers,AllSubs),
                                                      %% (10)
       a5odb_include_role(Key,AllSupers,AllSubs).

% ***********************************************************************
% ***********************************************************************

%% Storage of new roles, which needs no classification.

%% t5role_store_domain_range(Flag,PrimKey,Domain,Range,Filter,-NewKey)
%% stores primitive roles with domain and range restrictions.


t5role_store_domain_range_role(PrimKey,Domain,Range,Filter,NewKey) :-
	t5role_store_domain_range(d,PrimKey,Domain,Range,Filter,NewKey).

t5role_store_domain_range_feature(PrimKey,Domain,Range,Filter,NewKey) :-
	t5role_store_domain_range(f,PrimKey,Domain,Range,Filter,NewKey).


t5role_store_domain_range_inherent_feature(PrimKey,Domain,
                                                    Range,Filter,NewKey) :-
	t5role_store_domain_range(f,PrimKey,Domain,Range,Filter,NewKey).

t5role_store_domain_range(_,PrimKey,Domain,Range,_,Key) :-
	t5tbox_nothing_key(Nothing),
	(PrimKey == Nothing,
	 !;
	    (Domain == Nothing,
	     !;
	        Range == Nothing,
		!)),
	t5out_warning(incoherent_role),
	Key = Nothing.

t5role_store_domain_range(Flag,PrimKey,Domain,Range,Filter,Key) :-
	t5tbox_next_key(Key),
	t5out_trace(storing_domain_range_only(Key)),
	t5role_handle_range(Range,Key,RangeKey),
	t5tbox_anyrole_key(Anyrole),
	t5rdb_rnf(Anyrole,RNF1),
	t5rnf_add_prim(RNF1,PrimKey,RNF2),
	(Domain == none,
	 !,
	 RNF3 = RNF2;
	    t5rnf_add_domain(RNF2,Domain,RNF3)),
	t5rnf_add_range(RNF3,RangeKey,RNF4),
	b5sort_unify([Anyrole],[PrimKey],Supers0),
	t5rnf_add_feature(RNF4,RNF5,Supers0,Supers,Flag,NewFlag),
	(t5rnf_compute_inv(RNF4,notexists),
	 !,
	 InvKey = notexists;
	    t5tbox_next_key(InvKey)),
	t5role_raw_create(Key,NewFlag,RNF5,Filter,Role),
	t5rdb_store(Role,InvKey,no),
	t5tbox_role_hc(HC),
	t5tbox_nothing_key(Nothing),
	t5hc_insert(HC,Key,Supers,[Nothing],[]),
	t5tbox_update_pure_defs(role,Key),
	t5role_new_inv_role(prim,Key,InvKey).


t5role_handle_range(none,Key,Anything) :-
	!,
	t5tbox_anything_key(Anything),
	t5out_warning(default_range(Anything,Key)).

t5role_handle_range(Range,_,Range).
	    


%% t5role_store_atomic(Flag,Superkey,PrimKey,Filter, - Key) stores a role
%% defined as R :< S, where the key of S is SuperKey, PrimKey is the key of
%% the new primitive component of R. Key is the resulting key for R.

t5role_store_atomic_role(PrimKey,SuperKey,Filter,Key) :-
	t5role_store_atomic(d,SuperKey,PrimKey,Filter,Key).

t5role_store_atomic_feature(PrimKey,SuperKey,Filter,Key) :-
	t5role_store_atomic(f,SuperKey,PrimKey,Filter,Key).

t5role_store_atomic_inherent_feature(PrimKey,SuperKey,Filter,Key) :-
	t5role_store_atomic(f,SuperKey,PrimKey,Filter,Key).


t5role_store_atomic(_,SuperKey,_,_,Key) :-
	t5tbox_nothing_key(SuperKey),
	!,
	Key = SuperKey,
	t5out_warning(incoherent_role).

t5role_store_atomic(Flag,SuperKey,PrimKey,Filter,Key) :-
	t5tbox_next_key(Key),
	t5out_trace(storing_atomic_primitive_role(Key)),
	(t5rdb_inv(SuperKey,notexists),
	 !,
	 InvKey = notexists;
	    t5tbox_next_key(InvKey)),
	t5role_atomic_to_rdb(Flag,PrimKey,Key,InvKey,SuperKey,Filter),
	t5role_new_inv_role(prim,Key,InvKey).



t5role_atomic_to_rdb(Flag,PrimKey,Key,InvKey,SuperKey,Filter) :-
	t5rdb_rnf(SuperKey,RNF1),
	t5rnf_range(RNF1,Range),
	(t5tbox_all_key(Range),
	 !,
	 t5tbox_anything_key(Anything),
	 t5rnf_add_range(RNF1,Anything,RNF2),
	 t5out_warning(default_range(Anything,Key));
	    RNF2 = RNF1),
	t5rnf_add_prim(RNF2,PrimKey,RNF3),
	t5rnf_add_feature(RNF3,RNF4,[],_,Flag,Flag0),
	t5role_raw_create(Key,Flag0,RNF4,Filter,Role),
	t5rdb_store(Role,InvKey,no),
	t5role_atomic_to_hc(Flag0,Key,PrimKey,SuperKey),
	t5tbox_update_pure_defs(role,Key).                %%  ???

t5role_atomic_to_hc(d,Key,PrimKey,SuperKey) :-
	t5tbox_role_hc(HC),
	t5hc_supers(HC,SuperKey,Supers),
	b5sort_unify([SuperKey],[PrimKey],PrimKeySuperKey),
	b5sort_unify(PrimKeySuperKey,Supers,AllSupers),
	t5hc_known_disjoints(HC,SuperKey,Disjoints),
	t5tbox_nothing_key(Nothing),
	t5hc_insert(HC,Key,AllSupers,[Nothing],Disjoints),
        t5cdb_include_role(Key,AllSupers,[Nothing]),
                                                      %% (10)
        a5odb_include_role(Key,AllSupers,[Nothing]).

t5role_atomic_to_hc(f,Key,PrimKey,SuperKey) :-
	t5tbox_role_hc(HC),
	t5hc_supers(HC,SuperKey,Supers),
	t5tbox_feature_prim_key(FeaturePrim),
	b5sort_unify([SuperKey],[PrimKey],Supers1),
	b5sort_unify([FeaturePrim],Supers1,Supers2),
	b5sort_unify(Supers2,Supers,AllSupers),
	t5hc_known_disjoints(HC,SuperKey,Disjoints),
	t5tbox_nothing_key(Nothing),
	t5hc_insert(HC,Key,AllSupers,[Nothing],Disjoints),
        t5cdb_include_role(Key,AllSupers,[Nothing]),
                                                      %% (10)
        a5odb_include_role(Key,AllSupers,[Nothing]).

% ************************************************************************

t5role_new_inv_role(_,Key,notexists) :- !,
	t5out_trace(no_inv_role(Key)).

t5role_new_inv_role(prim,Key,InvKey) :-
	t5out_trace(storing_inv_role(Key,InvKey)),
	t5rdb_rnf(Key,RNF),
	t5rnf_normalize_inv(RNF,InvRNF),
	t5fil_filter(inv_primitiv,Filter),
	(t5rnf_feature_p(InvRNF),
	 !,
	 t5role_raw_create(InvKey,f,InvRNF,Filter,InvRole);
	    t5role_raw_create(InvKey,d,InvRNF,Filter,InvRole)),
	t5rdb_store(InvRole,Key,no),
	t5tbox_role_hc(HC),
	t5hc_supers(HC,Key,Supers),
	t5role_inv_list(Supers,InvSupers1),
	t5rnf_prims(InvRNF,Prims),
	b5sort_unify(InvSupers1,Prims,InvSupers),
	t5rnf_neg_prims(InvRNF,NegPrims),
	t5tbox_nothing_key(Nothing),
	t5hc_insert(HC,InvKey,InvSupers,[Nothing],NegPrims),
	t5tbox_update_pure_defs(role,InvKey),
        t5cdb_include_role(Key,InvSupers,[Nothing]),
                                                      %% (10)
        a5odb_include_role(Key,InvSupers,[Nothing]).

t5role_new_inv_role(def,Key,InvKey) :-
	t5out_trace(storing_inv_role(Key,InvKey)),
	t5fil_filter(internal,Filter),
	t5rdb_rnf(Key,RNF),
	t5rnf_normalize_inv(RNF,InvRNF),
	t5tbox_role_hc(HC),
	t5hc_supers(HC,Key,Supers),
	t5role_inv_list(Supers,InvSupers1),
	t5rnf_prims(InvRNF,Prims),
	b5sort_unify(InvSupers1,Prims,InvSupers),
	t5hc_subs(HC,Key,Subs),
	t5role_inv_list(Subs,InvSubs1),
	t5tbox_nothing_key(Nothing),
	b5sort_unify([Nothing],InvSubs1,InvSubs),
	t5rnf_neg_prims(InvRNF,Disj),
	(t5rnf_feature_p(InvRNF),
	 !,
	 t5role_store_new(InvKey,Filter,InvRNF,Key,f,InvSupers,InvSubs,Disj);
	  t5role_store_new(InvKey,Filter,InvRNF,Key,d,InvSupers,InvSubs,Disj)),
                                                      %% (8)
	t5comp_handle_comps(InvRNF,InvKey),
	t5tbox_update_pure_defs(role,InvKey).


t5role_inv_list([],[]).

t5role_inv_list([R|RList],InvList) :-
	(t5role_primitive_p(R),
	 !,
	 InvList = List;
	    t5role_inv_role(R,InvR),
	    InvList = [InvR|List]),
	t5role_inv_list(RList,List).


% ***********************************************************************
% The result of t5role_store_nf is the key of a given normalform. If the
% role doesn't exist, it is introduced.

t5role_store_nf(RNF0,Key) :-
	t5rnf_add_feature(RNF0,RNF,[],Supers,d,Flag),
                                                      %% (2)
	t5fil_filter(internal,Filter),
	t5cls_class(role,RNF,Filter,Supers,[],
                             AllSupers,AllSubs,Disj,Key,New_or_Old),
	(New_or_Old == old,
	 !,
	 (t5tbox_bottom_key(role,Key),
	  !,
	  t5out_warning(incoherent_role);
	     true);
                                                      %% (3)
	    t5out_trace(internal_role(New_or_Old)),
	    t5rnf_compute_inv(RNF,Inv),
                                                      %% (4) -- (10)
	    t5role_store_internal(New_or_Old,Key,Filter,RNF,Inv,Flag,
                                                AllSupers,AllSubs,Disj)).

% ***********************************************************************

% ***********************************************************************

%% With t5role_inv_role( + RoleKey, - InvKey/notexists/prim) you get the key
%% of the inverse role, if the role has an inverse and "notexists" else.
t5role_inv_role(RoleKey,InvKey_or_Info) :-
	t5rdb_inv(RoleKey,InvKey_or_Info).


%% wahrscheinlich Muell
t5role_not_inv_error_p(unspecified_range,RoleKey) :-
	t5out_error(inverse_role(unspecified_range,RoleKey)),
	!, fail.

t5role_not_inv_error_p(disjoint_anything_range,RoleKey) :-
	t5out_error(inverse_role(disjoint_anything_range,RoleKey)),
	!, fail.

t5role_not_inv_error_p(_,_).

% *********************************************************************
%% wahscheinlich Muell
t5role_compute_inv_trans(RNF,Inv,TransFlag) :-
	t5rnf_compute_inv(RNF,Inv),
	(t5rnf_trans_p(RNF),
	 !,
	 TransFlag = yes;
	    TransFlag = no).

% *********************************************************************

%% Removing a role from role-data-base
t5role_remove(Key) :-
	t5rdb_remove(Key).

% ***********************************************************************
% ***********************************************************************

%% Creates a role for storage.
t5role_raw_create(Key,Flag,RNF,Filter,[Key,Flag,RNF,Filter]).

% ***********************************************************************

%% These predicates are used by t5rdb to select information from the role
%% created above.
t5role_key([Key|_],Key) :- !.
t5role_flag([_,Flag|_],Flag) :- !.
t5role_rnf([_,_,RNF|_],RNF) :- !.
t5role_filter([_,_,_,F],F) :- !.

% *********************************************************************
% *********************************************************************

%% t5role_unify( + RKey, + RKey, - Rkey) computes the unification ( the con-
%% junktion of two roles given by their keys and returns a key.
%% There are two cases:
%% (a) If the infimum is yet stored within the hierarchy cache we get it from
%%     there.
%% (b) Else the unification of the normal forms is computed, and a knew role
%%     is stored and inserted as the infimum into the cache.

t5role_unify(Role1,Role2,Role2) :-
	var(Role1),!.

t5role_unify(Role1,Role2,Role1) :-
	var(Role2),!.

t5role_unify(Role1,Role2,Role3) :-
                                                      %% (a)
	t5tbox_role_hc(HC),
	(t5hc_infimum(HC,Role1,Role2,Role3), 
	 Role3 \== unknown,
	 !;
                                                      %% (b)
	    t5rdb_rnf(Role1,RNF1,_),
	    t5rdb_rnf(Role2,RNF2,_),
	    t5rnf_unify(RNF1,RNF2,RNF3),
	    t5role_store([RNF3,[Role1,Role2]],[],Role3,_), % sortiert?
            t5hc_insert_infimum(HC,[Role1,Role2],Role3)).

% ***********************************************************************

%% For the comparison of two role keys we ask the hierarchy cache.

t5role_compare(first,RKey1,RKey2,first) :-
	t5tbox_role_hc(HC),
	t5hc_subsumes_p(HC,RKey2,RKey1),
	!.

t5role_compare(second,RKey1,RKey2,second) :-
	t5tbox_role_hc(HC),
	t5hc_subsumes_p(HC,RKey1,RKey2),
	!.

t5role_compare(equal,RKey1,RKey2,NewSub) :-
	t5tbox_role_hc(HC),
	t5hc_subsumption(HC,RKey1,RKey2,NewSub),
	!.

t5role_compare(_,_,_,none).

% ***********************************************************************

%% Comparison without primitive components is computed on RNFs by t5rnf.
t5role_compare_without_prims(Sub,RKey1,RKey2,NewSub) :-
	t5rdb_rnf(RKey1,RNF1,_),
	t5rdb_rnf(RKey2,RNF2,_),
	t5rnf_compare_without_prims(Sub,RNF1,RNF2,NewSub), !.

% ***********************************************************************

%% For subsumption the hierarchy cache is asked.
t5role_subsumes_p(RKey1,RKey2) :-
	t5tbox_role_hc(HC),
	t5hc_subsumes_p(HC,RKey1,RKey2),
	!.

% ***********************************************************************

%% A role given by its Key is incoherent if the key is role bottom key.
t5role_incoherent_p(RKey) :-
	t5tbox_bottom_key(role,RKey).

% ***********************************************************************

%% To check disjointness of two roles given by there keys, we two cases:
%% (a) We ask first the hierarchy cache, if it answers "yes" we succeed if "no"
%%     we fail. If the answer is "unknown"
%% (b) we compute disjointness of the RNFs and insert the result into the
%%     hierarchy cache.
t5role_disjoint_p(Key1,Key2) :-
                                                       %% (a)
	t5tbox_role_hc(HC),
	t5hc_disjoint(HC,Key1,Key2,X),
	(X = yes,
	 !;
                                                       %% (b)
	    X = unknown,
	    t5rdb_rnf(Key1,Rnf1,_),
	    t5rdb_rnf(Key2,Rnf2,_),
	    t5tbox_role_hc(HC),
	    (t5rnf_disjoint_p(Rnf1,Rnf2),
             !,
	     t5hc_disjoint_info(HC,Key1,Key2,yes);
	        t5hc_disjoint_info(HC,Key1,Key2,no),
                !,
                fail)).

/*
t5role_disjoint_p(Key,Key) :- !,fail.
t5role_disjoint_p(Key1,Key2) :- 
	t5tbox_role_hc(HC),
	t5hc_disjoint(HC,Key1,Key2,X),
	t5role_disjoint_p(X,HC,Key1,Key2).
*/

t5role_disjoint_p(yes,_,_,_).
t5role_disjoint_p(unknown,HC,Key1,Key2) :-
	    t5rdb_rnf(Key1,Rnf1,_),
	    t5rdb_rnf(Key2,Rnf2,_),
	    (t5rnf_disjoint_p(Rnf1,Rnf2) ->
	     	t5hc_disjoint_info(HC,Key1,Key2,yes)
            ;
	        t5hc_disjoint_info(HC,Key1,Key2,no),!,fail
            ).
		

% ***********************************************************************

%% Because I-links for roles are not allowed, we has to check the
%% I-Box predicates only for the domain and range of the role.
%% For simplcity we do not lock for I-Box subsumption, incoherence etc. 
%% within nested trans and comp terms. This is  leeding to some incompleteness.
t5role_ibox_subsumes_p(RKey1,RKey2) :-
	t5role_domain(RKey1,Domain1),
	t5role_domain(RKey2,Domain2),
	i5ibox_get_ibox_conc(Domain2,IBox_Domain2),
	t5concid_subsumes_p(Domain1,IBox_Domain2),
	t5role_range(RKey1,Range1),
	t5role_range(RKey2,Range2),
	i5ibox_get_ibox_conc(Range2,IBox_Range2),
	t5concid_subsumes_p(Range1,IBox_Range2),
	t5rdb_rnf(RKey1,RNF1,_),
	t5rdb_rnf(RKey2,RNF2,_),
	t5rnf_subsumes_without_domain_range_p(RNF1,RNF2).
	
% ***********************************************************************

t5role_ibox_equivalent_p(RKey1,RKey2) :-
	t5role_ibox_subsumes_p(RKey1,RKey2),
	t5role_ibox_subsumes_p(RKey2,RKey1).

% ***********************************************************************

t5role_ibox_incoherent_p(RKey) :-
	(t5role_incoherent_p(RKey),
	 !;
	    (t5role_domain(RKey,D),
	     i5ibox_incoherent_p(conc,D),
	     !;
	        t5role_range(RKey,R),  % okp 20.11 statt t5rdb_range
		i5ibox_incoherent_p(conc,R))).

%% INCOMPLETE

% ***********************************************************************

t5role_ibox_disjoint_p(RKey1,RKey2) :-
	(t5role_disjoint_p(RKey1,RKey2),
	 !;
	    (t5role_domain(RKey1,D1),
	     t5role_domain(RKey2,D2),
	     i5ibox_disjoint_p(conc,D1,D2),
	     !;
	         t5role_domain(RKey1,D1),
	         t5role_domain(RKey2,D2),
	         i5ibox_disjoint_p(conc,D1,D2))).

%% Incomplete because of incomplete incoherent check.

% ***********************************************************************

%% t5role_sub_union (t5role_sub_intersection) is computed by the hierarchy
%% cache. The result is the union (intersection) of the subroles of the roles
%% from the List.
t5role_sub_union(Roles,Subs) :-
	t5tbox_role_hc(HC),
	t5hc_sub_union(HC,Roles,Subs),
	!.

t5role_sub_intersection(Roles,Subs) :-
	t5tbox_role_hc(HC),
	t5hc_sub_intersection(HC,Roles,Subs),
	!.

% *********************************************************************

%% gives the normal form of a role key
t5role_normal_form(Role,RNF) :-
	t5rdb_rnf(Role,RNF,_),
	!.

% *********************************************************************
% *********************************************************************

%% When introducing a new role the parser works on role protoforms. They
%% contain a role normal form and a list of super roles.
%% The following Predicates provide the initial role proto form with the
%% RNF of anyrole.
t5role_new_defined_role(RPF) :-
	t5role_new_role(RPF).
t5role_new_primitive_role(RPF) :-
	t5role_new_role(RPF).

t5role_new_role([RNF,[]]) :-
	t5tbox_anyrole_key(Anyrole),
	t5rdb_rnf(Anyrole,RNF).

%% t5role_bottom_role computes the proto form of an incoherent role.
t5role_bottom_role([BottomRNF,[]]) :-
	t5tbox_bottom_key(role,BottomKey),
	t5rdb_rnf(BottomKey,BottomRNF,_).

% *********************************************************************

%% With the following predicates new information is added to a role proto form.
%% A protoform is constructed by succesivly adding terms with at most one
%% operator and keys as arguments.
t5role_add_domain(_,Domain,BottomRPF) :-
	t5tbox_bottom_key(conc,Domain), !,
	t5role_bottom_role(BottomRPF).

t5role_add_domain(RPF,Domain,NewRPF) :-
	t5role_rpf_r_rnf(RPF,RNF,NewRNF,NewRPF),
	t5rnf_add_domain(RNF,Domain,NewRNF).

t5role_add_range(_,Range,BottomRPF) :-
	t5tbox_bottom_key(conc,Range), !,
	t5role_bottom_role(BottomRPF).

t5role_add_range(RPF,Range,NewRPF) :-
	t5role_rpf_r_rnf(RPF,RNF,NewRNF,NewRPF),
	t5rnf_add_range(RNF,Range,NewRNF).
/*
t5role_add_prim(_,Prim,BottomRPF) :-
	t5tbox_bottom_key(conc,Prim), !,
	t5role_bottom_role(BottomRPF).

t5role_add_prim(RPF,Prim,NewRPF):-
	t5role_rpf_r_rnf(RPF,RNF,NewRNF,NewRPF),
	t5rnf_add_prim(RNF,Prim,NewRNF).
*/

t5role_add_prim(_,Prim,BottomRPF) :-
	t5tbox_bottom_key(role,Prim), !,
	t5role_bottom_role(BottomRPF).

t5role_add_prim(RPF,Prim,NewRPF):-
	t5role_create_rpf(RNF,Supers,RPF),
	t5rnf_add_prim(RNF,Prim,NewRNF),
	b5sort_unify([Prim],Supers,NewSupers),
	t5role_create_rpf(NewRNF,NewSupers,NewRPF).

t5role_add_neg_prim(RPF,NegPrim,NewRPF):-
	t5role_rpf_r_rnf(RPF,RNF,NewRNF,NewRPF),
	t5rnf_add_neg_prim(RNF,NegPrim,NewRNF).


t5role_add_inv(_,Key,BottomRPF) :-
	t5tbox_bottom_key(role,Key), !,
	t5role_bottom_role(BottomRPF),
	t5out_warning(incoherent_inv).

t5role_add_inv(RPF,Key,NewRPF) :-
	t5role_inv_role(Key,InvKey_or_Info),
	(number(InvKey_or_Info),
	 !,
	 t5role_add_role(RPF,InvKey_or_Info,NewRPF);
	    t5out_error(inverse_role(InvKey_or_Info,Key)),
	    fail).

t5role_add_trans(_,Trans,BottomRPF) :-
	t5tbox_bottom_key(role,Trans), !,
	t5role_bottom_role(BottomRPF),
	t5out_warning(incoherent_trans).

t5role_add_trans(RPF,Trans,NewRPF):-                 /*???*/
	t5rdb_rnf(Trans,TransNF,_),
	t5role_rpf_r_rnf(RPF,RNF,NewRNF,NewRPF),
	t5rnf_add_trans(RNF,TransNF,Trans,NewRNF).

t5role_add_comp(_,Comp1,Comp2,BottomRNF):-                  /*?!?!?!?!*/
	(t5tbox_bottom_key(role,Comp1),
	t5out_warning(incoherent_comp_first);
	   t5tbox_bottom_key(role,Comp2),
	   t5out_warning(incoherent_comp_second)), !,
	   t5role_bottom_role(BottomRNF).

t5role_add_comp(_,Comp1,Comp2,BottomRNF) :-
	t5role_range(Comp1,Range),
	t5role_domain(Comp2,Domain),
	t5concid_disjoint_p(Range,Domain),
	!,
	t5out_warning(incoherent_comp_domain_range(Comp1,Comp2)),
	t5role_bottom_role(BottomRNF).


t5role_add_comp(RPF,Comp1,Comp2,NewRPF):-                  /*?!?!?!?!*/
	t5rdb_rnf(Comp1,Comp1NF,_),
	t5rdb_rnf(Comp2,Comp2NF,_),
	t5role_rpf_r_rnf(RPF,RNF,NewRNF,NewRPF),
	t5rnf_add_comp(RNF,Comp1NF,Comp2NF,NewRNF).

/*
t5role_add_role(_,Role,BottomRPF) :-
	t5tbox_bottom_key(role,Role), !,
	t5role_bottom_role(BottomRPF),
	t5out_warning(incoherent_androle).

t5role_add_role(RPF,Role,NewRPF) :-
	t5rdb_rnf(Role,RoleNF,_),
	t5role_rpf_r_rnf(RPF,RNF,NewRNF,NewRPF),
	t5rnf_unify(RNF,RoleNF,NewRNF).
*/

t5role_add_role(_,Role,BottomRPF) :-
	t5tbox_bottom_key(role,Role), !,
	t5role_bottom_role(BottomRPF),
	t5out_warning(incoherent_androle).

t5role_add_role(RPF,Role,NewRPF) :-
	t5rdb_rnf(Role,RoleNF,_),
	t5role_create_rpf(RNF,Supers,RPF),
	t5rnf_unify(RNF,RoleNF,NewRNF),
	b5sort_unify([Role],Supers,NewSupers),
	t5role_create_rpf(NewRNF,NewSupers,NewRPF).

% ***********************************************************************

t5role_rpf_rnf([RNF,_],RNF).

t5role_rpf_supers([_,Supers],Supers).


t5role_rpf_r_rnf([RNF,Supers],RNF,NewRNF,[NewRNF,Supers]).
t5role_rpf_r_supers([RNF,Supers],Supers,NewSupers,[RNF,NewSupers]).

t5role_create_rpf(RNF,Supers,[RNF,Supers]).

% ***********************************************************************

%% This predicate is needed to compute subroles which inherits the Min-NRs
%% and rolefillers in the TBox and ABox.

t5role_list_to_nf_without_domain_range([Role],RNF) :-
	!,
	t5rdb_rnf(Role,RNF).

t5role_list_to_nf_without_domain_range(RoleList,InfNF) :-
	t5tbox_anyrole_key(Anyrole),
	t5rdb_rnf(Anyrole,AnyroleNF),
	t5role_l_t_nf_w_d_r(RoleList,AnyroleNF,InfNF).

t5role_l_t_nf_w_d_r([],InfNf,InfNf).

t5role_l_t_nf_w_d_r([RKey|RoleList],Acc,InfNf) :-
	t5rdb_rnf(RKey,RNF),
	t5rnf_add_role_without_domain_range(RNF,Acc,NewAcc),
	t5role_l_t_nf_w_d_r(RoleList,NewAcc,InfNf).

% ***********************************************************************

%% c := atleast(1,r) and all(r,c) --> c ?< atleast(1,r and range(c))
%% c := atleast(1,r) and d --> c ?< atleast(1,r and domain(d))

t5role_downward_hereditary_p(R1Key,RoleList,ConcNF,VRKey) :-
	t5role_range(R1Key,Range1Key),
	t5concid_subsumes_p(Range1Key,VRKey),
	t5role_domain(R1Key,Domain1Key),
	t5concid_normal_form(Domain1Key,Domain1NF),
	b5nf_subsumes_p(Domain1NF,ConcNF),
	t5role_list_to_nf_without_domain_range(RoleList,InfNF),
	t5role_normal_form(R1Key,RNF1),
	t5rnf_subsumes_without_domain_range_p(RNF1,InfNF).
	
% ***********************************************************************

%% is true for the defined direct subconcepts of anyrole
t5role_topmost_def_p(Top,RKey) :-
	t5role_supers(RKey,[Top]).



%% Initializing the prim component for features

t5role_feature_prim(FeaturePrim) :-
	t5tbox_role_hc(HC),
	t5tbox_anyrole_key(Anyrole),
	t5tbox_nothing_key(Nothing),
	t5hc_insert(HC,FeaturePrim,[Anyrole],[Nothing],[]),
	t5rnf_create(RNF),
	t5rnf_add_prim(RNF,FeaturePrim,NewRNF),                     % !!!
	t5fil_filter(internal,Filter),
	t5role_raw_create(FeaturePrim,p,NewRNF,Filter,PrimRole),
	t5rdb_store(PrimRole,prim,no).
	
% **************************************************************************

/*------------------------------------------------------------ */ 
/*		Rolevaluemaps				       */
/*------------------------------------------------------------ */ 

/* dummy module : role value maps not implementet yet */

t5rvm_raw_create(Equals,Supers,Subs,rvm(Equals,Supers,Subs)).
t5rvm_generate(Eq,Su,Subs,norvm) :- var(Eq),var(Su),var(Subs),!.
t5rvm_generate(Eq,Su,Subs,rvm(Eqg,Sug,Subsg)) :-
	t5rvm_gen(Eq,Eqg),
	t5rvm_gen(Su,Sug),
	t5rvm_gen(Subs,Subsg).

t5rvm_gen(X,X) :- nonvar(X),!.
t5rvm_gen(_X,[]).

t5rvm_create(norvm).
t5rvm_empty(rvm([],[],[])).
t5rvm_empty_p(rvm([],[],[])).
t5rvm_empty_p(norvm).

t5rvm_equals(rvm(E,_,_),E).
t5rvm_equals(norvm,[]).
t5rvm_supers(rvm(_,S,_),S).
t5rvm_supers(norvm,[]).
t5rvm_subs(rvm(_,_,S),S).
t5rvm_subs(norvm,[]).

t5rvm_s_equals(rvm(_Eq,Sup,Sub),Eq,rvm(Eq,Sup,Sub)).
t5rvm_s_equals(norvm,[],norvm) :- !.
t5rvm_s_equals(norvm,Eq,rvm(Eq,[],[])).
t5rvm_s_supers(rvm(Eq,_Sup,Sub),Sup,rvm(Eq,Sup,Sub)).
t5rvm_s_supers(norvm,[],norvm) :- !.
t5rvm_s_supers(norvm,Sup,rvm([],Sup,[])).
t5rvm_s_subs(rvm(Eq,Sup,_Sub),Sub,rvm(Eq,Sup,Sub)).
t5rvm_s_subs(norvm,[],norvm) :- !.
t5rvm_s_subs(norvm,Sub,rvm([],[],Sub)).

/* auf syntaktische gleichheit pr"ufen, insbesonderer keine var! */
t5rvm_r_equals(rvm(Eq,Sup,Sub),Eq,N_Eq,rvm(N_Eq,Sup,Sub)).
t5rvm_r_equals(norvm,[],X,norvm) :- X == [],!.
t5rvm_r_equals(norvm,[],Eq,rvm(Eq,[],[])).
t5rvm_r_supers(rvm(Eq,Sup,Sub),Sup,N_Sup,rvm(Eq,N_Sup,Sub)).
t5rvm_r_supers(norvm,[],[],norvm) :- !.
t5rvm_r_supers(norvm,[],Su,rvm([],Su,[])).
t5rvm_r_subs(rvm(Eq,Sup,Sub),Sub,N_Sub,rvm(Eq,Sup,N_Sub)).
t5rvm_r_subs(norvm,[],[],norvm).


t5rvm_unify(norvm,RVM,RVM) :- !.
t5rvm_unify(RVM,norvm,RVM) :- !.
t5rvm_unify(RVM1,RVM2,RVM3) :-
        t5rvm_raw_create(Eq1,Sup1,Sub1,RVM1),
        t5rvm_raw_create(Eq2,Sup2,Sub2,RVM2),
        b5sort_unify(Eq1,Eq2,Eq3),
        b5sort_unify(Sup1,Sup2,Sup3),
        b5sort_unify(Sub1,Sub2,Sub3),
        t5rvm_raw_create(Eq3,Sup3,Sub3,RVM3).

t5rvm_compare(St0,norvm,norvm,St0) :- !.
t5rvm_compare(St0,norvm,_RVM,St1) :-  t5sub_unify(St0,second,St1),!.
t5rvm_compare(St0,_RVM,norvm,St2) :-  t5sub_unify(St0,first,St2),!.
t5rvm_compare(St0,RVM1,RVM2,St3) :-
        t5rvm_raw_create(Eq1,Sup1,Sub1,RVM1),
        t5rvm_raw_create(Eq2,Sup2,Sub2,RVM2),
        b5sort_compare(St0,Eq1,Eq2,St1),!,
        St1 \== none,
        b5sort_compare(St1,Sup1,Sup2,St2),!,
        St2 \== none,
        b5sort_compare(St2,Sub1,Sub2,St3),!.

t5rvm_compare(_,_,_,none).

t5rvm_subsumes_p(norvm,norvm) :- !.
t5rvm_subsumes_p(_RVM,norvm) :- !.
t5rvm_subsumes_p(norvm,_RVM) :- !,fail.
t5rvm_subsumes_p(RVM1,RVM2) :-
        t5rvm_raw_create(Eq1,Sup1,Sub1,RVM1),
        t5rvm_raw_craete(Eq2,Sup2,Sub2,RVM2),
        b5sort_subset_p(Eq2,Eq1),
        b5sort_subset_p(Sup2,Sup1),
        b5sort_subset_p(Sub2,Sub1).


t5eq_create(Roles,Ress,Var,eq(Roles,Ress,Var)).
t5eq_roles(eq(R,_Rs,_Var),R).
t5eq_ress(eq(_R,Rs,_Var),Rs).
t5eq_var(eq(_R,_Rs,Var),Var).

t5eq_s_roles(eq(_R,Rs,Var),R,eq(R,Rs,Var)).
t5eq_s_ress(eq(R,_Rs,Var),Rs,eq(R,Rs,Var)).
t5eq_s_var(eq(R,Rs,_Var),Var,eq(R,Rs,Var)).

t5eq_r_roles(eq(R,Rs,Var),R,NR,eq(NR,Rs,Var)).
t5eq_r_ress(eq(R,Rs,Var),Rs,NRs,eq(R,NRs,Var)).
t5eq_r_var(eq(R,Rs,Var),Var,NVar,eq(R,Rs,NVar)).

t5eq_include(EQ,R-Es,Varbind,NEQ) :-
	t5res_equals(R-Es,Eq),
	b5sort_unify(Eq,[R],Roles),
	t5eq_include(EQ,Roles,R-Es,Varbind,NEQ).

t5eq_include([],Roles,Res,Var,[EQ]) :-
	t5eq_create(Roles,[Res],Var,EQ).

t5eq_include([H|T],Roles,Res,Varbind,[NH|NT]) :-
	t5eq_create(H_Roles,Ress,Var,H),
	t5eq_create(NH_Roles,NRess,Var,NH),
	(b5sort_common_member_p(Roles,H_Roles) ->
		b5sort_unify(Roles,H_Roles,NH_Roles),
		b5typelist_add_ele(Ress,Res,NRess),
		NT = T,Varbind = Var
		;
		NH_Roles=H_Roles,NRess = Ress,
		t5eq_include(T,Roles,Res,Varbind,NT)   
	).


t5eq_eval([],[]) :- !.
t5eq_eval([H|T],[H|T]) :-
	t5eq_create(_Roles,Ress,Var,H),
	t5res_unify_list_without_roles(Ress,_Role-Var),
	t5eq_eval(T,T).



% new 27.02

t5eq_include_new(EQ,R-Es,NEQ) :-
	t5res_equals(R-Es,Eq),
	b5sort_unify(Eq,[R],Roles),
	t5eq_include_new2(EQ,Roles,NEQ).


t5eq_include_new2([],[_Role],[]) :- !.
t5eq_include_new2([],Roles,[Roles]). 

t5eq_include_new2([H_Roles|T],Roles,[NH_Roles|NT]) :-
	(b5sort_common_member_p(Roles,H_Roles) ->
		b5sort_unify(Roles,H_Roles,NH_Roles),
		NT = T
		;
		NH_Roles=H_Roles,
		t5eq_include_new2(T,Roles,NT)   
	).


t5eq_rvms2eq([],[]).
t5eq_rvms2eq([H|T],[NH|NT]) :-
	t5eq_create(H,[],_,NH),
	t5eq_rvms2eq(T,NT).



t5eq_process_res([],Res,Res,[]). % not found, not changed

t5eq_process_res([EQ|EQs],R-Es,NRES,[NEQ|NEQs]) :- 
	t5eq_create(H_Roles,Ress,Var,EQ),
	t5eq_create(H_Roles,NRess,Var,NEQ),
	%
	(b5sort_common_member_p(H_Roles,[R]) ->
		b5typelist_add_ele(Ress,R-Es,NRess),
		NRES = R-Var,
		NEQs = EQs
	;
		NRess = Ress,
		t5eq_process_res(EQs,R-Es,NRES,NEQs)
	).


t5eq_eval_new([]) :- !.
t5eq_eval_new([H|T]) :-
	t5eq_create(_Roles,Ress,Var,H),
	t5res_unify_list_without_roles(Ress,_Role-Var),
	t5eq_eval_new(T).

t5eq2rvarl(EQ,List) :-
	t5eq_create(Roles,_Ress,Var,EQ),
	t5eq_tolist(Roles,Var,List).

t5eq_tolist([],_,[]).

t5eq_tolist([Role|Roles],Var,[RVAR|T]) :-
	b5rvar_create(Role,Var,RVAR),
	t5eq_tolist(Roles,Var,T).




% LAST EDIT: Fri Aug  6 14:10:58 1993 by Mirjam Kuehne (madonna!mir) 
% LAST EDIT: Fri Jan 29 16:53:16 1993 by Mirjam Kuehne (kit!mir) 
% LAST EDIT: Tue Jan 26 16:15:55 1993 by Mirjam Kuehne (anfall!mir) 
% LAST EDIT: Fri Jan 22 18:02:41 1993 by Mirjam Kuehne (anfall!mir) 
% LAST EDIT: Sat Jan 16 15:56:12 1993 by Mirjam Kuehne (madonna!mir) 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 	                     DEPENDENCY-MODULE                           % 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              Mirjam Kuehne                             %
%                               Januar 1993                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% EXPORT-predicates: TELL-predicates: 
%		     t5dm_uses(+Name,+[Names]),
%		     t5dm_remove(+Name),
%		     t5dm_substitute(+Name,+[Names]), %%(gibt's noch nicht)
%		     t5dm_defined(+ExternalName,+ExternalTerm),
%                    t5dm_cleanup.
%                    t5dm_reset.
% for ABox-Revision: t5dm_obj_uses_add(+ObjID,+Names),
%                    t5dm_obj_uses_delete(+ObjID)
%		     t5dm_obj_uses_delete(+ObjID,+Names),
% for aboxinit       t5dm_abox_init( + Objs)

%		     ASK-predicates:
%		     t5dm_uses_p(+Name,+Name),
%		     t5dm_used_by(+Name,-[Names]),
%		     t5dm_definition(+ExternalName,-ExternalTerm) 
						       %%(gibts noch nicht)
% for ABox-Revision: t5dm_used_by_objs(+Name,-Objs)
%                    t5dm_obj_uses(+ObjID, -Names)


% IMPORT-predicates: b5sort_unify/3.
%		     b5sort_member_p/2.
%		     b5sort_intersect/3.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-multifile(b5dump_these_predicates/1).

b5dump_these_predicates([
t5dm_uses_relation/2,
t5dm_used_by_relation/2,
t5dm_trans_uses_relation/2,
t5dm_obj_used_by_relation/2,
t5dm_obj_uses_relation/2]).

%:- dynamic t5dm_uses_relation/3.          %% stehen alle in b5dyn.pl
%:- dynamic t5dm_used_by_relation/3.
%:- dynamic t5dm_trans_uses_relation/3.
%:- dynamic t5dm_obj_used_by_relation/3.
%:- dynamic t5dm_obj_uses_relation/2.
%:- dynamic t5dm_obj_uses_relation_copy/2.
%:- dynamic t5dm_obj_used_by_relation_copy/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 				TELL					 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% init									 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5dm_init :-
	retractall(t5dm_uses_relation(_,_)),
	retractall(t5dm_used_by_relation(_,_)),
	retractall(t5dm_obj_used_by_relation(_,_)),
	retractall(t5dm_obj_uses_relation(_,_)),
	retractall(t5dm_obj_used_by_relation_copy(_,_)),
	retractall(t5dm_obj_uses_relation_copy(_,_)),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% uses(Name,Names): inserts that Name uses the list of Names,            %
%                   inserts that the elements of Names are used by Name  %
%                   and the transitive closure trans_uses and            %
%                   trans_used_by.                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5dm_uses(Name,Names) :-
	t5dm_set(uses(Name,Names)),
	t5dm_used_by_handling(Names,Name),
	!.

t5dm_used_by_handling([],_) :- !.

t5dm_used_by_handling([NamesHead|Tail],Name) :-
	(t5dm_retract(used_by(NamesHead,NameList)),
	 b5sort_unify(NameList,[Name],NewNames),
	 t5dm_set(used_by(NamesHead,NewNames));
	t5dm_set(used_by(NamesHead,[Name]))),
	t5dm_used_by_handling(Tail,Name),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% remove(Name):                                                          %
% 1. uses_removal: removes uses(Name,DirectNames) and 			 %
%                  used_by(DirectNames,Name)                             %
% 3. trans_removal: removes trans_uses(Name,TransNames) 		 %
% 4. trans_by_removal: removes trans_used_by(Name,TransUsedByNames) and  %
%		        trans_uses(TransUsedByNames,Name)		 %
% It is not necassary to remove used_by(Name,Names) and                  %
% trans_used_by(Name,TransNames), because the definition of Name is only %
% changed but not completely removed. So this doesn't influence the      %
% Names used by Name. Thats also the reason, why it's not necassary      %
% to remove uses(UsedByNames,Name) and trans_uses(UsedByNames,Name).     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5dm_remove(Name) :-
	t5dm_uses_removal(Name),
	t5dm_used_removal(Name).

t5dm_uses_removal(Name) :-
	(t5dm_retract(uses(Name,Names)),
	 !,
	 t5dm_get_used_by(Names,Name); true).

t5dm_get_used_by([],_) :- !.

t5dm_get_used_by([NamesHead|NamesTail],Name) :-
	t5dm_retract(used_by(NamesHead,List)),
	t5dm_remove_name_from_list(List,NewList,Name),
	t5dm_set_used_by(NamesHead,NewList),
	t5dm_get_used_by(NamesTail,Name),
	!.

t5dm_set_used_by(_,[]) :- !.

t5dm_set_used_by(NamesHead,NewList) :-
	t5dm_set(used_by(NamesHead,NewList)).

%%%%%%%%%%%%%%%%%
t5dm_used_removal(Name) :-
	(t5dm_retract(used_by(Name,UsedByNames)),
	 !,
	 t5dm_get_uses(UsedByNames,Name);
	true).

t5dm_get_uses([],_) :- !.

t5dm_get_uses([UsedByNamesHead|Tail],Name) :-
	t5dm_retract(uses(UsedByNamesHead,List)),
	t5dm_remove_name_from_list(List,NewList,Name),
	t5dm_set_uses(UsedByNamesHead,NewList),
	t5dm_get_uses(Tail,Name),
	!.

t5dm_set_uses(_,[]) :- !.

t5dm_set_uses(UsedByNamesHead,NewList) :-
	t5dm_set(uses(UsedByNamesHead,NewList)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		             ASK					 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% used_by(Name,Names): Gives all names used by Name.                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5dm_used_by(Name,Names) :-
	t5dm_get(used_by(Name,Names)),
	!.

%%t5dm_used_by(_,[]) :- !.           % Juli 93

t5dm_trans_used_by(Name,OrdNames) :-
	t5dm_compute_ordered_list([Name],[],OrdNames),
	!.

t5dm_compute_ordered_list([],List,List) :-
	!.
	
t5dm_compute_ordered_list(List,List2,OrdNames) :-
	t5dm_get_directs(List,Directs),
	t5dm_remove_list_from_list(Directs,List2,NewList),
	append(NewList,Directs,OrdList),
	!,
	t5dm_compute_ordered_list(Directs,OrdList,OrdNames).

t5dm_get_directs([],_) :- !.

t5dm_get_directs([L|List1],Directs) :-
	!,
	t5dm_get_directs([L|List1],[],Directs).

t5dm_get_directs(_,[]) :- !.

t5dm_get_directs([],List,List) :- !.

t5dm_get_directs([Head|Tail],List2,Directs) :-
	t5dm_get(used_by(Head,Names)),
	b5sort_unify(Names,List2,StoreDirects),
	!,
	t5dm_get_directs(Tail,StoreDirects,Directs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% predicates ABox-revision                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% t5dm_obj_uses_add(ObjID,Names): the same as uses, but without          %
%                                 transitive closure, because it is      %
%                                 already computed at the time uses was  %
%                                 inserted.                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5dm_obj_uses_add(_, [class/anything]) :- !.           %% no entry
t5dm_obj_uses_add(ObjID,Names) :-
	(t5dm_retract(obj_uses(ObjID,OldNames)),
	 t5dm_set(obj_uses_copy(ObjID,OldNames)),      %% reset
	 b5sort_unify(OldNames,Names,NewNames),
	 t5dm_set(obj_uses(ObjID,NewNames));
	t5dm_set(obj_uses(ObjID,Names))),
	t5dm_set_obj_used_by(Names,ObjID),
	!.

t5dm_set_obj_used_by([],_).
t5dm_set_obj_used_by([NamesHead|Tail],ObjID) :-
	( t5dm_retract(obj_used_by(NamesHead,OldObjs)) ->
	       t5dm_set(obj_used_by_copy(NamesHead,OldObjs)),   %% reset
	       b5sort_unify(OldObjs,[ObjID],NewObjs),
	       UsedByObjs = NewObjs
	; UsedByObjs = [ObjID] ),              %% ObjID is the first one
	t5dm_set(obj_used_by(NamesHead,UsedByObjs)),
	t5dm_set_obj_used_by(Tail,ObjID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% t5dm_cleanup: The new Object-Tell succeeded and all copies were        %
%                removed.                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5dm_cleanup :-
	retractall(t5dm_obj_uses_relation_copy(_,_)),
	retractall(t5dm_obj_used_by_relation_copy(_,_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% t5dm_reset: The new Object-Tell failed and the database is  reset.     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5dm_reset :-
	(t5dm_retract(obj_uses_copy(Obj,OldNames)),
	 t5dm_retract(obj_uses(Obj,Names)),
	 t5dm_set(obj_uses(Obj,OldNames)),
	 b5sort_difference(Names,OldNames,DiffNames),
	 t5dm_retract_used_by(DiffNames),
	 fail ; true).

t5dm_retract_used_by([]).

t5dm_retract_used_by([NamesHead|Tail]) :-
	(t5dm_retract(obj_used_by(NamesHead,_)) ; true),!,
	(t5dm_retract(obj_used_by_copy(NamesHead,OldObjs)) ; true),!,
	t5dm_set(obj_used_by(NamesHead,OldObjs)),
	t5dm_retract_used_by(Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% t5dm_obj_uses_delete(ObjID,Names): removes uses-relation between ObjId %
%                                    and Names but without respect to    %
%                                    the relations between the names.    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% jt:
%% t5dm_obj_uses_delete(+ObjID)
%% t5dm_obj_uses_delete(+ObjID, +Names).
%%   If called without Names, ObjID is deleted from all occurrences of
%%   obj_uses and obj_used_by relations; 

t5dm_obj_uses_delete(ObjID) :-
	t5out_trace(t5dm_obj_uses_delete(ObjID)),
	t5dm_obj_uses_relation(ObjID, NamesList1), !, 
	retract(t5dm_obj_uses_relation(ObjID,_)), !,
	sort(NamesList1, NamesList2),   %% jt 29/01/93
             %% to remove doubles, since NamesList1 is a bag; 
	     %% FFS: make use of orderedness of NamesList1
	t5dm_obj_used_by_removal(NamesList2, ObjID), !.
t5dm_obj_uses_delete(_).   %% if no t5dm_obj_uses_relation

t5dm_obj_uses_delete(ObjID,Names) :-
	t5dm_obj_uses_removal(ObjID,Names),
	t5dm_obj_used_by_removal(Names,ObjID).

%% t5dm_obj_uses_removal(+ObjID, +Names).
%%   All Names are removed from the according t5dm_obj_uses_relation.

t5dm_obj_uses_removal(ObjID,Names) :-
	t5dm_retract(obj_uses(ObjID,NamesList)),
	b5sort_difference(NamesList,Names,DiffNamesList),
	t5dm_set(obj_uses(ObjID,DiffNamesList)).
	
%% t5dm_obj_used_by_removal(+ObjId, +UsedNames)
%%   For every Name in UsedNames ObjId is removed from
%%   the according t5dm_obj_used_by_relation.

t5dm_obj_used_by_removal([], _) :- !.

t5dm_obj_used_by_removal([NamesHead | Tail], ObjID) :-
	t5dm_retract(obj_used_by(NamesHead,ObjIDList)),
	( select(ObjID, ObjIDList, Residue) ->
	      t5dm_set(obj_used_by(NamesHead,Residue))
	; true),
	!,
	t5dm_obj_used_by_removal(Tail, ObjID).	

% end jt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% t5dm_used_by_objs(Name,Objs): gives all objects used by Name           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5dm_used_by_objs(Name,Objs) :-
	t5dm_get(obj_used_by(Name,Objs)).

t5dm_obj_uses(ObjId, Names) :-
	t5dm_get(obj_uses(ObjId, Names)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set (assert)								 %
% get									 %
% remove (retract)							 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5dm_set(uses(_,[])) :- !.

t5dm_set(uses(Name,Names)) :-	
	assert(t5dm_uses_relation(Name,Names)),
	!.

t5dm_set(used_by(_,[])) :- !.

t5dm_set(used_by(Name,Names)) :-
	assert(t5dm_used_by_relation(Name,Names)),
	!.

t5dm_set(trans_uses(_,[])) :- !.

t5dm_set(trans_uses(Name,Names)) :-
	assert(t5dm_trans_uses_relation(Name,Names)),
	!.

t5dm_set(trans_used_by(_,[])) :- !.

t5dm_set(trans_used_by(Name,Names)) :-
	assert(t5dm_trans_used_by_relation(Name,Names)),
	!.

t5dm_set(obj_uses(_,[])) :- !.

t5dm_set(obj_uses(ObjID,Names)) :-
	assert(t5dm_obj_uses_relation(ObjID,Names)),
	!.

t5dm_set(obj_used_by(_,[])) :- !.

t5dm_set(obj_used_by(Name,Objs)) :-
	assert(t5dm_obj_used_by_relation(Name,Objs)),
	!.

t5dm_set(obj_uses_copy(Obj,Names)) :-
	assert(t5dm_obj_uses_relation_copy(Obj,Names)).

t5dm_set(obj_used_by_copy(Name,Objs)) :-
	assert(t5dm_obj_used_by_relation_copy(Name,Objs)).

%%%%%%%%%%%%%%%%%%%%

t5dm_get(uses(Name,Names)) :-
	(t5dm_uses_relation(Name,Names);
	 true),
	!.

t5dm_get(used_by(Name,Names)) :-
	t5dm_used_by_relation(Name,Names),
	!.

t5dm_get(used_by(_,[])).            % Juli 93

t5dm_get(trans_uses(Name,Names)) :-
	(t5dm_trans_uses_relation(Name,Names);
	 true),
	!.

t5dm_get(trans_used_by(Name,Names)) :-
	(t5dm_trans_used_by_relation(Name,Names);
	 true),
	!.

%%%%%%%%%%%%%%%
t5dm_get(obj_uses(Obj,Names)) :-
	(t5dm_obj_uses_relation(Obj,Names);
	 true),
	!.

t5dm_get(obj_used_by(Name,Objs)) :-
	(t5dm_obj_used_by_relation(Name,Objs);
	 true),
	!.

%%%%%%%%%%%%%%%%%%%%%%%

t5dm_retract(uses(Name,Names)) :-	
	retract(t5dm_uses_relation(Name,Names)),
	!.

t5dm_retract(used_by(Name,Names)) :-
	retract(t5dm_used_by_relation(Name,Names)),
	!.

t5dm_retract(trans_uses(Name,Names)) :-
	retract(t5dm_trans_uses_relation(Name,Names)),
	!.

t5dm_retract(trans_used_by(Name,Names)) :-
	retract(t5dm_trans_used_by_relation(Name,Names)),
	!.

t5dm_retract(obj_uses(Obj,Names)) :-
	retract(t5dm_obj_uses_relation(Obj,Names)),
	!.

t5dm_retract(obj_used_by(Name,Objs)) :-
	retract(t5dm_obj_used_by_relation(Name,Objs)),
	!.

t5dm_retract(obj_uses_copy(Obj,Names)) :-
	retract(t5dm_obj_uses_relation_copy(Obj,Names)),
	!.

t5dm_retract(obj_used_by_copy(Name,Objs)) :-
	retract(t5dm_obj_used_by_relation_copy(Name,Objs)),
	!.

%%%%%%%%%%%%%%%%%%%%%

t5dm_remove_list_from_list([],List,List) :- !.

t5dm_remove_list_from_list([NewNamesHead|NewNamesTail],List,NewList) :-
	t5dm_remove_name_from_list(List,StoreList,NewNamesHead),
	t5dm_remove_list_from_list(NewNamesTail,StoreList,NewList),
	!.

t5dm_remove_name_from_list([],[],_) :- !.

t5dm_remove_name_from_list([Name|Tail],Tail,Name) :- !.

t5dm_remove_name_from_list([X|Tail],[X|List],Name) :- 
	t5dm_remove_name_from_list(Tail,List,Name),
	!.

%%%%%%%%%%

t5dm_abox_init(Objects) :-
	retractall(t5dm_obj_uses_relation(_,_)),
	retractall(t5dm_obj_used_by_relation(_,_)),
	t5dm_assert_tbox_objects(Objects).

t5dm_assert_tbox_objects([]).

t5dm_assert_tbox_objects([O1|Objects]) :-
	t5dm_obj_uses_add(O1,[class/anything]),
	t5dm_assert_tbox_objects(Objects).

% LAST EDIT: Wed Feb  3 16:06:27 1993 by Mirjam Kuehne (jenseits!mir) 
% LAST EDIT: Mon Feb  1 12:42:40 1993 by Mirjam Kuehne (madonna!mir) 
% LAST EDIT: Sun Jan 31 17:18:08 1993 by Mirjam Kuehne (anfall!mir) 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            HIERARCHY-CACHE			         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              Mirjam Kuehne                             %
%                               Januar 1993                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% IMPORT-predicates : b5sort_unify(List1,List2,NewList),
%		      b5sort_member_p(Term,List),
%		      b5sort_difference(List1,List2,DifferenceList),
%		      b5sort_common_member_p(List1,List2),
%		      t5sub_reverse(Key1,Key2),
%		      t5concid_filter(Key,Filter)


% EXPORT-predicates:
%	TELL-predicates:
%		    t5hc_init(+Top,+Bottom,-HC_Number),
%		    t5hc_insert(+HC_Number,+Term,+Supers,+Subs,+Disjoints),
%		    t5hc_insert_disjoint(+HC_Number,+Term1,+Term2),
%		    t5hc_disjoint_info(+HC_Number,+Term1,+Term2,+yes/no),
%		    t5hc_insert_infimum(+HC_Number,+(Term1,Term2),+Infimum),
%		    t5hc_remove(+HC_Number,+KeyList)
%		    
%	ASK-predicates:
%		    t5hc_supers(+HC_Number,+Term,-SuperList),
% 	            t5hc_supers(+HC_Number,+Term,+Filter,-SuperList),
%
%		    t5hc_subs(+HC_Number,+Term,-SubList),
%	            t5hc_subs(+HC_Number,+Term,+Filter,-SubList),
%
%		    t5hc_direct_supers(+HC_Number,+Term,-DirectSuperList),
%	            t5hc_direct_supers(+HC_Nr,+Term,+Filter,-SuperList),
%
%		    t5hc_direct_subs(+HC_Number,+Term,-DirectSubList),
%	            t5hc_direct_subs(+HC_Nr,+Term,+Filter,-DirectSublist),
%
%	    	    t5hc_minimize_special(+N,+Supers,-SpecialSupers),
%	            t5hc_minimize_special(+N,+Supers,+Filter,-FilSpecSupers),
%
%	            t5hc_minimize_general(+N,+Subs,-GeneralSubs),
%	            t5hc_minimize_general(+N,+Subs,+Filter,-FilGenSubs),
%		    t5hc_minimize(+N,+List,+Filter,-FilteredList),
%
%		    t5hc_known_disjoints(+HC_Number,+Term,-DisjointList),
%		    t5hc_known_disjoints(+HC_Number,+Term,Filter,-DisjointList),
%		    t5hc_non_disjoints(+HC_Number,+Term,-NonDisjointList),
%		    t5hc_possibly_disjoints(+HC_Number,+Term,-PossDisjList),
%		    t5hc_disjoint(+HC_Number,+Term1,+Term2,-3_Bool),
%
%		    t5hc_subsumes_p(+HC_Number,+Term1,+Term2),
%		    t5hc_subsumption(+HC_Number,+Term1,+Term2,-Subsumption),
%
%		    t5hc_infimum(+HC_Number,+Term1,+Term2,-Infimum),
%
%		    t5hc_super_intersection(+HC_Nr,+Terme,-SuperIntersection),
%	            t5hc_super_intersection(+N,+Term,+Filter,-FilSuperInters),
%
%		    t5hc_sub_intersection(+HC_Number,+Terme,-SubIntersection),
%	            t5hc_sub_intersection(+N,+Term,Filter,FilterSubIntersec),
%
%		    t5hc_super_union(+HC_Number,+Terme,-SuperUnion),
%	            t5hc_super_union(+N,+Term,+Filter,-FilterSuperUnion),
%
%		    t5hc_sub_union(+HC_Number,+Terme,-SubUnion),
%	            t5hc_sub_union(+N,+Term,+Filter,-FilterSubUnion),
%
%		    t5hc_retract(+HC_Number,+Term,+Supers,+Subs,+Disjoints)



:-multifile(b5dump_these_predicates/1).

b5dump_these_predicates([
t5hc_conc_hierarchy/4,        
t5hc_role_hierarchy/4,
t5hc_term_hierarchy/4,
t5hc_infimum_hierarchy/3,
t5hc_subsumption_hierarchy/4,
t5hc_key/1,
t5hc_bottom_p/2,
t5hc_top_p/2]).


%:- dynamic t5hc_conc_hierarchy/4.        
%:- dynamic t5hc_role_hierarchy/4.
%:- dynamic t5hc_term_hierarchy/4.
%:- dynamic t5hc_infimum_hierarchy/3.
%:- dynamic t5hc_subsumption_hierarchy/4.
%:- dynamic t5hc_key/1.
%:- dynamic t5hc_bottom_p/2.
%:- dynamic t5hc_top_p/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%									 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  TELL  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%									 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%									 %
%				 INIT					 %
%									 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
% Initializing a Hierarchy Cache: 					 %
% 	There are n Caches: Number 1 denotes the concept-hierarchy.	 %
%			    Number 2 denotes the role-hierachy.		 %
%	You can define more hierarchies with other numbers.              %
% The hierachies have the following format:				 %
% 	hierarchy(Term,Subs,Supers,Disjoints).                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_init(Top,Bottom,N) :-
	t5hc_next_key(N),
        t5hc_set(Top,[],[Bottom],[],N),
        t5hc_set(Bottom,[Top],[],[],N),
	retractall(t5hc_top_p(N,Top)),
	retractall(t5hc_bottom_p(N,Bottom)),
	assert(t5hc_top_p(N,Top)),
	assert(t5hc_bottom_p(N,Bottom)),
        !.

t5hc_next_key(Key) :-
        retract(t5hc_key(Key)), !,
        NewKey is Key +1,
        assert(t5hc_key(NewKey)),
        !.

t5hc_init_module :-
	retractall(t5hc_key(_)),
	retractall(t5hc_conc_hierarchy(_,_,_,_)),
	retractall(t5hc_role_hierarchy(_,_,_,_)),
	retractall(t5hc_term_hierarchy(_,_,_,_)),
	retractall(t5hc_subsumption_hierarchy(_,_,_,_)),
	retractall(t5hc_infimum_hierarchy([_,_],_,_)),
	assert(t5hc_key(1)),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                  					 %
%			     INSERT	 				 %
%									 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Insert a new concept or role:						 %
% 			That contains handling the supers, subs and      %
%			disjoints of the new concept (role)		 %
%			( but doesn't compute the transitive closure)    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   

t5hc_insert(N,Term,Supers,Subs,Disjoints) :-
	t5hc_insert_term_in_sub_lists(Supers,Term,N),
	t5hc_insert_term_in_super_lists(Subs,Term,N),
	t5hc_disjoint_handling(Disjoints,NewDisjoints,Term,N),
	t5hc_set(Term,Supers,Subs,NewDisjoints,N),
	!.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% insert_term_in_sub_lists puts the new concept or role into all         %
%			   sublists of the elements of its own           %
%			   superlist.				         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_insert_term_in_sub_lists([],_,_) :- !.

t5hc_insert_term_in_sub_lists([Head|Tail],Term,N) :-
	t5hc_retract(Head,Supers,Subs,Disjoints,N),
	b5sort_unify([Term],Subs,NewSubs),
        t5hc_set(Head,Supers,NewSubs,Disjoints,N),	
	t5hc_insert_term_in_sub_lists(Tail,Term,N),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% insert_term_in_super_lists puts the new concept or role into all       %
%			     superlists of the elements of its own       %
%			     sublist.					 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_insert_term_in_super_lists([],_,_) :- !.

t5hc_insert_term_in_super_lists([Head|Tail],Term,N) :-
	t5hc_retract(Head,Supers,Subs,Disjoints,N),
	b5sort_unify([Term],Supers,NewSupers),
	t5hc_set(Head,NewSupers,Subs,Disjoints,N),
	t5hc_insert_term_in_super_lists(Tail,Term,N),
	!.


/*
t5hc_insert(N,Term,Supers,Subs,Disjoints) :-
	t5hc_insert_internal(Term,Supers,Subs,Disjoints,N).

t5hc_insert_internal(Term,Supers,Subs,Disjoints,N) :-
	t5hc_super_handling(Term,Supers,NewSupers,N),
	t5hc_sub_handling(Term,Subs,NewSubs,N),
	t5hc_disjoint_handling(Disjoints,NewDisjoints,Term,N),
	t5hc_set(Term,NewSupers,NewSubs,NewDisjoints,N),
	!.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% super_handling							 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% collect_super_lists collects all superlists of the elements of the 	 %
%		     superlist of the new concept or role and put them   %
%		     together into the list SuperList.		         %
% unify unifies the superlist of the new concept or role with all in     %
%	SuperList collected supers.					 %
% insert_term_in _sub_lists puts the new concept or role into all        %
%			      sublists of the elements of its own        %
%			      superlist.				 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_super_handling(Term,Supers,NewSupers,N) :-
	t5hc_collect_super_lists(Supers,SuperList,N),
	b5sort_unify(Supers,SuperList,NewSupers),
	t5hc_insert_term_in_sub_lists(NewSupers,Term,N),
	!.

t5hc_insert_term_in_sub_lists([],_,_) :- !.

t5hc_insert_term_in_sub_lists([Head|Tail],Term,N) :-
	t5hc_retract(Head,Supers,Subs,Disjoints,N),
	b5sort_unify([Term],Subs,NewSubs),
        t5hc_set(Head,Supers,NewSubs,Disjoints,N),	
	t5hc_insert_term_in_sub_lists(Tail,Term,N),
	!.
*/
	
t5hc_collect_super_lists([],[],_) :- !.

t5hc_collect_super_lists([Head|Tail],SuperList,N) :-
 	t5hc_get(Head,HeadSupers,_,_,N),
	t5hc_collect_super_lists(Tail,TailList,N),
	b5sort_unify(HeadSupers,TailList,SuperList),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sub_handling  						         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% collect_sub_lists collects all sublists of the elements of the 	 %
%		     sublist of the new concept or role and put them     %
%		     together into the list SubList.		         %
% unify unifies the sublist of the new concept or role with all in       %
%	SubList collected subs.						 %
% insert_term_in _super_lists puts the new concept or role into all      %
%			      superlists of the elements of its own      %
%			      sublist.					 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
t5hc_sub_handling(Term,Subs,NewSubs,N) :-
	t5hc_collect_sub_lists(Subs,SubList,N),
	b5sort_unify(Subs,SubList,NewSubs),
	t5hc_insert_term_in_super_lists(NewSubs,Term,N),
	!.

t5hc_insert_term_in_super_lists([],_,_) :- !.

t5hc_insert_term_in_super_lists([Head|Tail],Term,N) :-
	t5hc_retract(Head,Supers,Subs,Disjoints,N),
	b5sort_unify([Term],Supers,NewSupers),
	t5hc_set(Head,NewSupers,Subs,Disjoints,N),
	t5hc_insert_term_in_super_lists(Tail,Term,N),
	!.
*/

t5hc_collect_sub_lists([],[],_) :- !.

t5hc_collect_sub_lists([Head|Tail],SubList,N) :-
	t5hc_get(Head,_,HeadSubs,_,N),
	t5hc_collect_sub_lists(Tail,TailList,N),
	b5sort_unify(HeadSubs,TailList,SubList),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Disjoint_handling                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% insert_term_in_disjoint_lists: The new concept or role will be         %
%                                inserted in  disjointlists of the 	 %
%			         elements of its own disjointlist.	 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_disjoint_handling([],[],_,_) :- !.              

t5hc_disjoint_handling(Disjoints,NewDisjoints,Term,N) :-
	t5hc_minimize_general(N,Disjoints,NewDisjoints),
	t5hc_insert_term_in_disjoint_lists(NewDisjoints,Term,N),
	!.

t5hc_insert_term_in_disjoint_lists([],_,_).

t5hc_insert_term_in_disjoint_lists([Ele1|Rest],Term,N) :-
	t5hc_get(Ele1,_,_,Ele1Disjoints,N),
	(t5hc_term_non_sub_test(Ele1Disjoints,Term,N) ->
	    t5hc_retract(Ele1,Supers,Subs,Ele1Disjoints,N),
	    b5sort_unify([Term],Ele1Disjoints,NewEle1Disjoints),
	    t5hc_set(Ele1,Supers,Subs,NewEle1Disjoints,N),
	    t5hc_insert_term_in_disjoint_lists(Rest,Term,N)
	;
	t5hc_insert_term_in_disjoint_lists(Rest,Term,N)).


t5hc_term_non_sub_test([],_,_).

t5hc_term_non_sub_test([Ele1DisjointsHead|Ele1DisjointsTail],Term,N) :-
	t5hc_get(Ele1DisjointsHead,_,Subs,_,N),
	\+ b5sort_member_p(Term,Subs),
	t5hc_term_non_sub_test(Ele1DisjointsTail,Term,N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			DISJOINTS					 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% There are two possibilities to insert a new disjoint-relation:	 %
% 1. insert_disjoint(N,Conc1,Conc2):					 %
%		     You know that Conc1 and Con2 are disjoint.	         %
% 2. disjoint_info(N,Con1,Conc2,yes/no): 				 %
%		   You know that two concepts (or roles) are disjoint    %
%                  or you know they are not disjoint.			 %
% If the same disjoint-relation is already asserted for the super-       %
% concepts, it will not be inserted for Conc1 and Conc2.		 %
% Otherwise Conc1 will be inserted into the disjointlist of Conc2 the    %
% other way round.							 %
% It can be that the disjoint-relation influences the subsumption-       %
% hierarchy: If two concepts or roles are disjoint and there is a        %
% subsumption-relation subsumption_hierarchy(Conc1,Conc2,none) it will   %
% be changed in subsumption_hierarchy(Conc1,Conc2,dis). That means they  %
% don't only subsume each other, but are also disjoint.			 %
% If disjoint_info says the two concepts or roles are not disjoint,      %
% a subsumption-relation subsumption_hierarchy(Conc1,Conc2,non_dis) will %
% be set. That means, I don't know whether they subsume each other, it's %
% only known they are not disjoint.					 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_insert_disjoint(N,Term1,Term2) :-
	t5hc_disjoint_info(N,Term1,Term2,yes).

t5hc_disjoint_info(N,Term1,Term2,yes) :-
	t5hc_insert_and_minimize(Term1,Term2,N),
	t5hc_insert_and_minimize(Term2,Term1,N),
	(b5sort_ordered_p(Term1,Term2) ->
	    t5hc_super_subsumption_update(Term1,Term2,N)
	;
	t5hc_super_subsumption_update(Term2,Term1,N)),
	!.

t5hc_disjoint_info(N,Term1,Term2,no) :-
	(b5sort_ordered_p(Term1,Term2) ->
	    t5hc_sub_subsumption_update(Term1,Term2,N)
	;
	t5hc_sub_subsumption_update(Term2,Term1,N)),
	!.

t5hc_insert_and_minimize(Key1,Key2,N) :-
	t5hc_retract(Key1,Supers,Subs,Disjoints,N),
	b5sort_unify(Disjoints,[Key2],UnifiedDisjoints),
	t5hc_minimize_general(N,UnifiedDisjoints,NewDisjoints),
	t5hc_set(Key1,Supers,Subs,NewDisjoints,N),
	!.

t5hc_super_subsumption_update(Term1,Term2,N) :-
	 findall(X,t5hc_subsumption_hierarchy(X,Term1,dis,N),List1),
	 findall(X,t5hc_subsumption_hierarchy(Term1,X,dis,N),List2),
	 b5sort_unify(List1,List2,SubsumptionList),
	 t5hc_get(Term2,Supers,_,_,N),
	 (b5sort_common_member_p(Supers,SubsumptionList) -> true
         ;
	 retractall(t5hc_subsumption_hierarchy(Term1,Term2,_,N)), 
	 assert(t5hc_subsumption_hierarchy(Term1,Term2,dis,N))).

t5hc_sub_subsumption_update(Term1,Term2,N) :-
	 findall(X,t5hc_subsumption_hierarchy(Term1,X,non_dis,N),List1),
	 findall(X,t5hc_subsumption_hierarchy(X,Term1,non_dis,N),List2),
	 b5sort_unify(List1,List2,SubsumptionList),
	 t5hc_get(Term2,_,Subs,_,N),
	 (b5sort_common_member_p(Subs,SubsumptionList) -> true
         ;
	 retractall(t5hc_subsumption_hierarchy(Term1,Term2,_,N)), 
	 assert(t5hc_subsumption_hierarchy(Term1,Term2,non_dis,N))).

%%%%%%%%%%%%%%%%%%%
t5hc_collect_disjoint_lists([],[],_) :- !.

t5hc_collect_disjoint_lists([Head|Tail],SuperDisjointList,N) :-
	t5hc_collect([Head|Tail],[],SuperDisjointList,N),
	!.

t5hc_collect([],List,List,_) :- !.
 
t5hc_collect([Head|Tail],Disjoints,SuperDisjointList,N) :-
	t5hc_get(Head,_,_,HeadDisjoints,N),
	b5sort_unify(HeadDisjoints,Disjoints,ResultList),
        t5hc_collect(Tail,ResultList,SuperDisjointList,N),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			    INFIMUM					 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% insert_infimum: a new infimum-relation will be set.			 %
%		  The concepts (or roles) are sorted, so the infimum     %
%		  has to be inserted only one time.			 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_insert_infimum(N,[Term1,Term2],Infimum) :-
	t5hc_insert_infimum_internal([Term1,Term2],Infimum,N).

t5hc_insert_infimum_internal([Term1,Term2],Infimum,N) :-
	(b5sort_ordered_p(Term1,Term2),
	 !,
	 assert(t5hc_infimum_hierarchy([Term1,Term2],Infimum,N));
	 assert(t5hc_infimum_hierarchy([Term2,Term1],Infimum,N))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%				REMOVE					 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% remove: at TBox-Revision the respective keys are removed from the 	 %
%	  hierarchy cache.						 %
%	  1. removes the concept or role itself				 %
%	  2. removes the concept or role from the entry of the elements  %
%	     of its own sub-, super- and disjointlist.                   %
%	  3. if given, removes the term from the subsumption_hierarchy   %
%	  4. if given, removes the term from the infimum_hierarchy       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_remove(_,[]) :- !.

t5hc_remove(N,[Term|Tail]) :- 
	t5hc_remove_internal([Term|Tail],N).

t5hc_remove_internal([],_) :- !.

t5hc_remove_internal([Term|Tail],N) :- 
	t5hc_retract(Term,Supers,Subs,Disjoints,N),
	t5hc_list_removal(Term,Supers,Subs,Disjoints),
	(retract(t5hc_subsumption_hierarchy(Term,_,_,N)),!;
	 retract(t5hc_subsumption_hierarchy(_,Term,_,N)),!;
	 true),
	(retract(t5hc_infimum_hierarchy([Term,_],_,N)),!;
	 retract(t5hc_infimum_hierarchy([_,Term],_,N)),!;
	 true),
	t5hc_remove_internal(Tail,N),
	!.

t5hc_list_removal(Term,Supers,Subs,Disjoints) :-
	t5hc_super_removal(Supers,Term),
	t5hc_sub_removal(Subs,Term),
	t5hc_disjoint_removal(Disjoints,Term),
	!.

t5hc_super_removal([],_) :- !.

t5hc_super_removal([SuperHead|SuperTail],Term) :-
	t5hc_retract(SuperHead,Supers,Subs,Disjoints,N),
	t5hc_remove_term_from_list(Term,Subs,NewSubs),
	t5hc_set(SuperHead,Supers,NewSubs,Disjoints,N),
	t5hc_super_removal(SuperTail,Term),
	!.

t5hc_sub_removal([],_) :- !.

t5hc_sub_removal([SubHead|SubTail],Term) :-
	t5hc_retract(SubHead,Supers,Subs,Disjoints,N),
	t5hc_remove_term_from_list(Term,Supers,NewSupers),
	t5hc_set(SubHead,NewSupers,Subs,Disjoints,N),
	t5hc_sub_removal(SubTail,Term),
	!.

t5hc_disjoint_removal([],_) :- !.

t5hc_disjoint_removal([DisjointHead|DisjointTail],Term) :-
	t5hc_retract(DisjointHead,Supers,Subs,Disjoints,N),
	t5hc_remove_term_from_list(Term,Disjoints,NewDisjoints),
	t5hc_set(DisjointHead,Supers,Subs,NewDisjoints,N),
	t5hc_disjoint_removal(DisjointTail,Term),
	!.

t5hc_remove_term_from_list(_,[],[]) :- !.

t5hc_remove_term_from_list(Term,[Term|Tail],Tail) :- !.

t5hc_remove_term_from_list(Term,[X|Tail],[X|List]) :-
	t5hc_remove_term_from_list(Term,Tail,List),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  ASK %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% supers/3.: give all elements of the concept's superlist                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_supers(N,Term,SuperList) :-
	t5hc_supers_internal(Term,SuperList,N).

t5hc_supers_internal(Term,SuperList,N) :-
	t5hc_get(Term,SuperList,_,_,N),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% supers/4.: give all elements of the concept's superlist, which         %
%            satisfy a special filter.		                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_supers(N,Term,Filter,FilterSuperList) :-
	t5hc_supers_internal(Term,Filter,FilterSuperList,N).

t5hc_supers_internal(Term,Filter,FilterSuperList,N) :-
	t5hc_get(Term,SuperList,_,_,N),
	t5hc_filter_test(N,SuperList,Filter,FilterSuperList),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% subs/3.: give all elements of the concept's sublist                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_subs(N,Term,SubList) :-
	t5hc_subs_internal(Term,SubList,N).

t5hc_subs_internal(Term,SubList,N) :-
	t5hc_get(Term,_,SubList,_,N),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% subs/4.: give all elements of the concept's sublist, which satisfy     %
%          a special filter.		                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_subs(N,Term,Filter,FilterSubList) :-
	t5hc_subs_internal(Term,Filter,FilterSubList,N).

t5hc_subs_internal(Term,Filter,FilterSubList,N) :-
	t5hc_get(Term,_,SubList,_,N),
	t5hc_filter_test(N,SubList,Filter,FilterSubList),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% direct_supers/3.: give all elements of the concept's superlist, which  %
%                   are direct supers.					 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_direct_supers(N,Term,Directs) :-
	t5hc_direct_supers_internal(Term,Directs,N).

t5hc_direct_supers_internal(Term,Directs,N) :-
        t5hc_supers_internal(Term,Supers,N),
        t5hc_filter_direct_supers(Supers,[],Directs),
	!.

t5hc_filter_direct_supers([],Directs,Directs) :- !.

t5hc_filter_direct_supers([Head|Tail],Directs1,Directs) :-
	t5hc_supers_internal(Head,HeadSupers,_),
        b5sort_difference(Tail,HeadSupers,DifferenceSupers),
        b5sort_difference(Directs1,HeadSupers,DifferenceDirects),
	b5sort_unify([Head],DifferenceDirects,DirectSupers),
        t5hc_filter_direct_supers(DifferenceSupers,DirectSupers,Directs),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% direct_supers/4.: give all elements of the concept's superlist, which  %
%                   are direct supers and satisfy a special filter       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_direct_supers(N,Term,Filter,FilterDirects) :-
	t5hc_direct_supers_internal(Term,Filter,FilterDirects,N).

t5hc_direct_supers_internal(Term,Filter,FilterDirects,N) :-
        t5hc_supers_internal(Term,Filter,Supers,N),
        t5hc_collect_direct_supers(N,Supers,Filter,[],FilterDirects),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% t5hc_minimize_special/3: the same as direct_supers/3, but with a list  % 
%                          instead of a single concept.  		 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_minimize_special(N,Supers,SpecialSupers) :-
	t5hc_collect_direct_supers(N,Supers,[],SpecialSupers).
%%%%%%%%%%%%%%%%%%

t5hc_collect_direct_supers(_,[],Directs,Directs) :- !.

t5hc_collect_direct_supers(N,[Head|Tail],Directs1,Directs) :-
	t5hc_collect_dir_supers_int([Head|Tail],Directs1,Directs,N).

t5hc_collect_dir_supers_int([],Directs,Directs,_) :- !.

t5hc_collect_dir_supers_int([Head|Tail],Directs1,Directs,N) :-
	 t5hc_supers_internal(Head,HeadSupers,N),
         b5sort_difference(Tail,HeadSupers,DiffSupers),
         b5sort_difference(Directs1,HeadSupers,DiffDirects),
	 b5sort_unify([Head],DiffDirects,DirectSupers),
         t5hc_collect_dir_supers_int(DiffSupers,DirectSupers,Directs,N),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% t5hc_minimize_special/4.: the same as direct_supers/4, but with a list %
%                           instead as a single concept.		 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_minimize_special(N,Supers,Filter,FilterSpecialSupers) :-
	t5hc_collect_direct_supers(N,Supers,Filter,[],FilterSpecialSupers).

t5hc_collect_direct_supers(_,[],_,Directs,Directs) :- !.

t5hc_collect_direct_supers(N,[Head|Tail],Filter,Directs1,Directs) :-
	t5hc_collect_dir_supers_int([Head|Tail],Filter,Directs1,Directs,N).

t5hc_collect_dir_supers_int([],_,Directs,Directs,_) :- !.

t5hc_collect_dir_supers_int([Head|Tail],Filter,Directs1,Directs,N) :-
	(t5hc_filter_test_p(N,Head,Filter),
	 t5hc_supers_internal(Head,HeadSupers,N),
         b5sort_difference(Tail,HeadSupers,DiffSupers),
         b5sort_difference(Directs1,HeadSupers,DiffDirects),
	 b5sort_unify([Head],DiffDirects,DirectSupers),
         t5hc_collect_dir_supers_int(DiffSupers,Filter,DirectSupers,Directs,N);
	t5hc_collect_dir_supers_int(Tail,Filter,Directs1,Directs,N)),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% direct_subs/3.: give all elements of the concept's sublist, which are  %
%                 direct subs.					         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_direct_subs(N,Term,Directs) :-
	t5hc_direct_subs_internal(Term,Directs,N).

t5hc_direct_subs_internal(Term,Directs,N) :-
        t5hc_subs_internal(Term,Subs,N),
        t5hc_collect_direct_subs(Subs,[],Directs),
	!.

t5hc_collect_direct_subs([],Directs,Directs) :- !.

t5hc_collect_direct_subs([Head|Tail],Directs1,Directs) :-
	t5hc_subs_internal(Head,HeadSubs,_),
        b5sort_difference(Tail,HeadSubs,DifferenceSubs),
        b5sort_difference(Directs1,HeadSubs,DifferenceDirects),
	b5sort_unify([Head],DifferenceDirects,DirectSubs),
        t5hc_collect_direct_subs(DifferenceSubs,DirectSubs,Directs),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% direct_subs/4.: give all elements of the concept's sublist, which are  %
%                 direct subs and satisfy a special filter               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_direct_subs(N,Term,Filter,FilterDirects) :-
	t5hc_direct_subs_internal(Term,Filter,FilterDirects,N).

t5hc_direct_subs_internal(Term,Filter,FilterDirects,N) :-
        t5hc_subs_internal(Term,Filter,Subs,N),
        t5hc_collect_direct_subs(N,Subs,Filter,[],FilterDirects),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% t5hc_minimize_general/3.: the same as direct_subs/3.			 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_minimize_general(N,Subs,GeneralSubs) :-
	t5hc_collect_direct_subs(N,Subs,[],GeneralSubs).

t5hc_collect_direct_subs(_,[],Directs,Directs) :- !.

t5hc_collect_direct_subs(N,[Head|Tail],Directs1,Directs) :-
	t5hc_collect_dir_subs_int([Head|Tail],Directs1,Directs,N).

t5hc_collect_dir_subs_int([],Directs,Directs,_) :- !.

t5hc_collect_dir_subs_int([Head|Tail],Directs1,Directs,N) :-
	 t5hc_subs_internal(Head,HeadSubs,N),
         b5sort_difference(Tail,HeadSubs,DiffSubs),
         b5sort_difference(Directs1,HeadSubs,DiffDirects),
	 b5sort_unify([Head],DiffDirects,DirectSubs),
         t5hc_collect_dir_subs_int(DiffSubs,DirectSubs,Directs,N),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% t5hc_minimize_general/4.: the same as direct_subs/4.			 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_minimize_general(N,Subs,Filter,FilterGeneralSubs) :-
	t5hc_collect_direct_subs(N,Subs,Filter,[],FilterGeneralSubs).

t5hc_collect_direct_subs(_,[],_,Directs,Directs) :- !.

t5hc_collect_direct_subs(N,[Head|Tail],Filter,Directs1,Directs) :-
	t5hc_collect_dir_subs_int([Head|Tail],Filter,Directs1,Directs,N).

t5hc_collect_dir_subs_int([],_,Directs,Directs,_) :- !.

t5hc_collect_dir_subs_int([Head|Tail],Filter,Directs1,Directs,N) :-
	(t5hc_filter_test_p(N,Head,Filter),
	 t5hc_subs_internal(Head,HeadSubs,N),
         b5sort_difference(Tail,HeadSubs,DiffSubs),
         b5sort_difference(Directs1,HeadSubs,DiffDirects),
	 b5sort_unify([Head],DiffDirects,DirectSubs),
         t5hc_collect_dir_subs_int(DiffSubs,DirectSubs,Directs,N);
        t5hc_collect_dir_subs_int(Tail,Directs1,Directs,N)),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% minimize/4: proves for a list of concepts or roles whether the         %
%             elements satisfy a filter and collect these in             %
%             FilteredList.						 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_minimize(N,List,Filter,FilteredList) :-
	t5hc_filter_test_internal(List,Filter,FilteredList,N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% filter_test_p: proves whether a concept or role satisfies a filter.	 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% for concepts 			   				         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_filter_test_p(1,Term,Filter) :-
	!,
	t5concid_filter(Term,TrueFilter),
	t5fil_holds_p(Filter,TrueFilter).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% for roles					                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_filter_test_p(2,Term,Filter) :-
	!,
	t5rdb_filter(Term,TrueFilter),
	t5fil_holds_p(Filter,TrueFilter).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% filter_test: the same as minimize/4.					 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_filter_test(_,[],_,[]) :- !.

t5hc_filter_test(N,List,Filter,FilterList) :-
	t5hc_filter_test_internal(List,Filter,FilterList,N).

%% cmk, 6.4.93: Patch hc filter_test Nondeterminismus

t5hc_filter_test_internal([],_,[],_).
t5hc_filter_test_internal([Head|Tail], Filter, FilteredList0, HC) :-
	(t5hc_test_filter_holds_p(HC, Head, Filter) ->
	     FilteredList0 = [Head|FilteredList]
	;    FilteredList0 = FilteredList),
	t5hc_filter_test_internal(Tail, Filter, FilteredList, HC).

t5hc_test_filter_holds_p(1, Conc, Filter) :-
	%% the conc case
	t5concid_filter(Conc,TrueFilter),
	t5fil_holds_p(Filter,TrueFilter).
t5hc_test_filter_holds_p(2, Role, Filter) :-
	%% the role case
	t5rdb_filter(Role,TrueFilter),
	t5fil_holds_p(Filter,TrueFilter).


%-cmk- t5hc_filter_test_internal([],_,[],_).
%-cmk- 
%-cmk- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-cmk- % for concepts 							 	 %
%-cmk- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-cmk- 
%-cmk- t5hc_filter_test_internal([Head|Tail],Filter,FilterList,1) :-
%-cmk- 	t5concid_filter(Head,TrueFilter),
%-cmk- 	(t5fil_holds_p(Filter,TrueFilter) ->
%-cmk- 	     FilterList = [Head|FilterList1]
%-cmk-         ;    FilterList = FilterList1),
%-cmk- 	t5hc_filter_test_internal(Tail,Filter,FilterList1,1).
%-cmk- 
%-cmk- /*
%-cmk- t5hc_filter_test_internal([],_,List,List,_) :- !.
%-cmk- t5hc_filter_test_internal([Head|Tail],Filter,List,FilterList,1) :-
%-cmk- 	(t5concid_filter(Head,TrueFilter),
%-cmk- 	 t5fil_holds_p(Filter,TrueFilter),
%-cmk- 	 b5sort_unify([Head],List,StoreList),
%-cmk- 	 t5hc_filter_test_internal(Tail,Filter,StoreList,FilterList,1);
%-cmk- 	t5hc_filter_test_internal(Tail,Filter,List,FilterList,1)),
%-cmk- 	!.
%-cmk- */
%-cmk- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-cmk- % for roles						 		 %
%-cmk- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-cmk- 
%-cmk- t5hc_filter_test_internal([Head|Tail],Filter,FilterList,2) :-
%-cmk- 	t5rdb_filter(Head,TrueFilter),
%-cmk- 	(t5fil_holds_p(Filter,TrueFilter) ->
%-cmk- 	     FilterList = [Head|FilterList1]
%-cmk-         ;    FilterList = FilterList1),
%-cmk- 	t5hc_filter_test_internal(Tail,Filter,FilterList1,2).
%-cmk- 
%-cmk- /*
%-cmk- t5hc_filter_test_internal([Head|Tail],Filter,List,FilterList,2) :-
%-cmk- 	(t5rdb_filter(Head,TrueFilter),
%-cmk- 	 t5fil_holds_p(Filter,TrueFilter),
%-cmk- 	 b5sort_unify([Head],List,StoreList),
%-cmk- 	 t5hc_filter_test_internal(Tail,Filter,StoreList,FilterList,2);
%-cmk- 	t5hc_filter_test_internal(Tail,Filter,List,FilterList,2)),
%-cmk- 	!.
%-cmk- */


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		          Subsumption					 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% subsumes_p: Is Term2 subsumed by Term1?				 %
%	      Is the result "second" Term2 is more special than Term1,   %
%	      that means Term2 is subsumed by Term1.			 %
%	      The answer is "yes".					 %
%	      Is the result "first" Term1 is more special than Term2,    %
%	      that means Term2 is not subsumed by Term1.		 %
%	      The answer is "no".					 %
%	      Is the result "none" there is no subsumption-relation      %
%	      between Term1 and Term2.					 %
%	      The answer is also "no".					 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_subsumes_p(_,Term,Term) :- !.

t5hc_subsumes_p(N,Term1,Term2) :-           
	t5hc_subsumption(N,Term1,Term2,second),
%	Result == second,
	!.

%t5hc_subsumes_p(N,Term1,Term2) :-
%	t5hc_subsumption(N,Term1,Term2,Result),
%	(Result == first ; Result == none),
%	!,fail.

/*
t5hc_subsumes_p(N,Term1,Term2) :-           
	t5hc_subsumes_p_internal(Term1,Term2,N).

t5hc_subsumes_p_internal(Term,Term,_) :- !.

t5hc_subsumes_p_internal(Term1,Term2,N) :-           
	t5hc_subsumption_internal(Term1,Term2,Result,N),
	Result == second,
	!.

t5hc_subsumes_p_internal(Term1,Term2,N) :-
	t5hc_subsumption_internal(Term1,Term2,Result,N),
	(Result == first ; Result == none),
	!,fail.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% subsumption: Is there any subsumption-relation between Term1 and       %
%	       Term2?							 %
% 1. Equal terms don't subsume eachother.			 	 %
% 2. Are T1 and T2 sorted?						 %
%    Subsumption will only be asserted for one direction. So in some     %
%    casees the result has to be changed: "first" to "second" and        %
%    "second" to "first".						 %
% There are tree Variations of none subsumption:			 %
% 1. "none": no subsumption, but it is nothing known about disjointnes.  %
% 2. "dis" : no subsumption and also disjointnes between T1 and T2 	 %
% 3. "non_dis" : no subsumption, but also no disjointnes.		 %
% For the question about subsumption the disjointnes isn't interesting,  %
% so for all three cases the answer is "none".				 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_subsumption(_,Term,Term,equal) :- !.

t5hc_subsumption(N,Term1,Term2,first) :-
	t5hc_get(Term1,Supers,_,_,N),
	b5sort_member_p(Term2,Supers),
	!.

t5hc_subsumption(N,Term1,Term2,second) :-
	t5hc_get(Term2,Supers,_,_,N),
	b5sort_member_p(Term1,Supers),
	!.

t5hc_subsumption(_,_,_,none).

/*
t5hc_subsumption(N,Term1,Term2,Subsumption) :-
	t5hc_subsumption_internal(Term1,Term2,Subsumption,N).

t5hc_subsumption_internal(Term,Term,equal,_) :- !.

t5hc_subsumption_internal(Term1,Term2,Subsumption,N) :-
	(b5sort_ordered_p(Term1,Term2),
	 (t5hc_subsumption_hierarchy(Term1,Term2,Result,N),
	 t5hc_dis_test(Result,Subsumption);
	  t5hc_compute_subsumption(Term1,Term2,Subsumption,N));
	 (t5hc_subsumption_hierarchy(Term2,Term1,RevSubsumption,N),
	 t5hc_dis_test(Result,RevSubsumption),
	 t5sub_reverse(RevSubsumption,Subsumption);
	  t5hc_compute_subsumption(Term2,Term1,RevSubsumption,N),
	  t5sub_reverse(RevSubsumption,Subsumption))),
	!.

t5hc_dis_test(Result,Subsumption) :-
	((Result == dis; Result == non_dis),
	  Subsumption = none;
	Subsumption = Result),
       !.
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The computation of subsumption:					 %
% Is T2 contained in T1's superlist then T1 is more special than T2, so  %
% it is subsumed by T2 and the answer is "first" (the first one is more  %
% special than the second one).						 %
% Is T1 in the superlist of T2 then T2 is more special than T1, the      %
% answer is "second" (because the second one is more special than the    %
% first one).								 %
% Is none of the terms in the other's superlist, the answer is "none".   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fuer Konzepte
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
t5hc_compute_subsumption(Term1,Term2,first,N) :-
	t5hc_get(Term1,Supers,_,_,N),
	b5sort_member_p(Term2,Supers),
	assert(t5hc_subsumption_hierarchy(Term1,Term2,first,N)),
	!.

t5hc_compute_subsumption(Term1,Term2,second,N) :-
	t5hc_get(Term2,Supers,_,_,N),
	b5sort_member_p(Term1,Supers),
	assert(t5hc_subsumption_hierarchy(Term1,Term2,second,N)),
	!.

t5hc_compute_subsumption(Term1,Term2,none,N) :-
	assert(t5hc_subsumption_hierarchy(Term1,Term2,none,N)),
	!.
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			DISJOINTS					 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% known_disjoints: give all elements of the term's disjointlist          %
%                  (without supers of the disjoints)                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_known_disjoints(N,Term,DisjointList) :-
	t5hc_get(Term,_,_,Disjoints1,N),
%%	t5hc_supers_internal(Term,Supers,N),
%%	t5hc_collect_disjoint_lists(Supers,SuperDisjointList,N),
%%	b5sort_unify(Disjoints,SuperDisjointList,DisjointList1),
	findall(X,t5hc_subsumption_hierarchy(X,Term,dis,N),List1),
	findall(X,t5hc_subsumption_hierarchy(Term,X,dis,N),List2),
	b5sort_unify(List1,List2,Disjoints2),
	b5sort_unify(Disjoints1,Disjoints2,DisjointList),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% disjoints with filters						 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_known_disjoints(N,Term,Filter,FilterDisjointList) :-
	t5hc_get(Term,_,_,DisjointList,N),
%%	t5hc_supers(N,Term,Supers),
%%	t5hc_collect_disjoint_lists(Supers,SuperDisjointList,N),
%%	b5sort_unify(Disjoints,SuperDisjointList,DisjointList),
	t5hc_filter_test(N,DisjointList,Filter,FilterDisjointList),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% non_disjoints: give all known disjoints of the term (all elements of   %
%                the term's sub- and superlists).                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_non_disjoints(N,Term,NonDisjointList) :-
	t5hc_get(Term,SuperList,SubList,_,N),
	b5sort_unify(SuperList,SubList,NonDisjointList1),
	findall(X,t5hc_subsumption_hierarchy(X,Term,non_dis,N),List1),
	findall(X,t5hc_subsumption_hierarchy(Term,X,non_dis,N),List2),
	b5sort_unify(List1,List2,NonDisjointList2),
	b5sort_unify(NonDisjointList1,NonDisjointList2,NonDisjointList3),
	t5tbox_nothing_key(NothingKey),
	b5sort_difference(NonDisjointList3,[NothingKey],NonDisjointList).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% possibly_disjoints: returns a list of keys that are neither known to   %
%                     be disjoint nor known to be not disjoint from Term %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_possibly_disjoints(N,Term,PossiblyDisjoints) :-
	t5hc_top_p(N,Top),
	t5hc_subs(N,Top,TopSubs),
	t5hc_known_disjoints(N,Term,KnownDisjoints),
	b5sort_difference(TopSubs,KnownDisjoints,PossDisjoints1),
	t5hc_collect_sub_lists(KnownDisjoints,DisjointSubs,N),
	b5sort_difference(PossDisjoints1,DisjointSubs,PossDisjoints2),
	t5hc_non_disjoints(N,Term,NonDisjoints),
	b5sort_difference(PossDisjoints2,NonDisjoints,PossDisjoints3),
	t5hc_collect_super_lists(NonDisjoints,NonDisjointSupers,N),
	b5sort_difference(PossDisjoints3,NonDisjointSupers,PossDisjoints4),
	t5hc_get(Term,Supers,Subs,_,N),
	b5sort_difference(PossDisjoints4,Supers,PossDisjoints5),
	b5sort_difference(PossDisjoints5,Subs,PossiblyDisjoints).

/*
t5hc_possibly_disjoints(N,Term,PossiblyDisjoints) :-
	t5hc_top_p(N,Top),
	t5hc_subs(N,Top,TopSubs),
	t5hc_known_disjoints(N,Term,Disjoints),
	t5hc_non_disjoints(N,Term,NonDisjoints),
	b5sort_unify(Disjoints,NonDisjoints,AllDisjonts),
	t5hc_get(Term,Supers,Subs,_,N),
	b5sort_unify(Supers,Subs,SupersSubs),
	b5sort_unify(SupersSubs,AllDisjonts,AllNotPossiblyDisjoints),
	b5sort_difference(TopSubs,AllNotPossiblyDisjoints,PossiblyDisjoints).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% disjoint: Are T1 and T2 disjoint?					 %
% 1. Equal terms are not disjoint.					 %
% 2. Is one term contained in the other's disjointlist they are disjoint.%
% 3. Are the supers of the terms disjoint the terms themselves are also  %
%    disjoint.								 %
% 4. The terms are disjoint if one of the terms is the bottom concept or %
%    role.		 						 %
% 5. The terms are not disjoint if one of the terms is the top concept   %
%    or role.								 %
% 6. The terms are not disjoint if they subsume eachother.		 %
% 7. The terms are disjoint if the infimum is bottom (nothing).		 %
% 8. The terms are not disjoint if the infimum is something else than    %
%    bottom.								 %
% 9. In all other cases the disjoint-relation between T1 and T2 is       %
%    unknown.								 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_disjoint(N,Term1,Term2,Result) :-
	t5hc_disjoint_internal(Term1,Term2,_,Disjoint,N),
	(var(Result),
	 Result = Disjoint;
	Disjoint == Result),
	!.

t5hc_disjoint_internal(Term,Term,_,no,_) :- !.

t5hc_disjoint_internal(Term1,Term2,_,yes,N) :-
	(b5sort_ordered_p(Term1,Term2) ->
	 t5hc_subsumption_hierarchy(Term1,Term2,dis,N);
	t5hc_subsumption_hierarchy(Term2,Term1,dis,N)),
	!.                                               

t5hc_disjoint_internal(Term1,Term2,_,yes,N) :-
	t5hc_get(Term1,_,_,Disjoints,N),
	b5sort_member_p(Term2,Disjoints),
	assert(t5hc_subsumption_hierarchy(Term1,Term2,dis,N)),
	!.

t5hc_disjoint_internal(Term1,Term2,_,yes,N) :-
	t5hc_supers_internal(Term1,Supers1,N),
	t5hc_supers_internal(Term2,Supers2,N),
	b5sort_unify([Term1],Supers1,List1),
	b5sort_unify([Term2],Supers2,List2),
	t5hc_supers_contain_disjoint_concs_p(List1,List2,N),
	assert(t5hc_subsumption_hierarchy(Term1,Term2,dis,N)),
	!.

t5hc_disjoint_internal(Term1,Term2,_,yes,N) :-
	(t5hc_bottom_p(N,Term1); t5hc_bottom_p(N,Term2)),
	t5hc_insert_disjoint(N,Term1,Term2),
	assert(t5hc_subsumption_hierarchy(Term1,Term2,dis,N)),
	!.

t5hc_disjoint_internal(Term1,Term2,_,no,N) :-
	(t5hc_top_p(N,Term1); t5hc_top_p(N,Term2)),
	!.

t5hc_disjoint_internal(Term1,Term2,_,no,N) :-
	t5hc_subsumption(N,Term1,Term2,Result),
        (Result == first; Result == second),
	!.

t5hc_disjoint_internal(Term1,Term2,_,yes,N) :-
	t5hc_infimum_hierarchy([Term1,Term2],bottom,N),
	t5hc_insert_disjoint(N,Term1,Term2),
	assert(t5hc_subsumption_hierarchy(Term1,Term2,dis,N)),
	!.

t5hc_disjoint_internal(Term1,Term2,dis,Disjoint,N) :-
	t5hc_infimum_internal(Term1,Term2,dis,Infimum,N),
	(Infimum == unknown, Disjoint = unknown;
	 Disjoint = no),
	!.

t5hc_disjoint_internal(_,_,inf,unknown,_) :- !.

t5hc_supers_contain_disjoint_concs_p([],_,_) :-
	!,fail.

t5hc_supers_contain_disjoint_concs_p([Prim|_],Supers2,N) :-
	t5hc_known_disjoints(N,Prim,DisConcs),
	b5sort_common_member_p(DisConcs,Supers2),
	!.

t5hc_supers_contain_disjoint_concs_p([_|Supers1],Supers2,N) :-
	!,t5hc_supers_contain_disjoint_concs_p(Supers1,Supers2,N),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                       INFIMUM						 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1. Is there an respective entry in the infimum_hierarchy?		 %
% 2. The infimumis bottom if one of the terms is bottom.		 %
% 3. If one term is top and the other one is not bottom then the infimum %
% 4. is the other term.							 %
% 5. Is one term more special than the other one, the infimum is the     %
%    more special one.							 %
% 6. Are the terms disjoint they have no infimum.			 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_infimum(N,Term1,Term2,Result) :-
	t5hc_infimum_internal(Term1,Term2,_,Infimum,N),
	(var(Result),
	 Result = Infimum;
	Infimum == Result),
	!.

t5hc_infimum_internal(Term,Term,_,Term,_) :- !.

t5hc_infimum_internal(Term1,Term2,_,Infimum,N) :-
	(t5hc_infimum_hierarchy([Term1,Term2],Infimum,N),
         !;
	 t5hc_infimum_hierarchy([Term2,Term1],Infimum,N)),
	 !.

t5hc_infimum_internal(Term1,Term2,_,Bottom,N) :-
	(t5hc_bottom_p(N,Term1), Bottom = Term1,
	 !; 
	 t5hc_bottom_p(N,Term2), Bottom = Term2),
	 t5hc_insert_infimum_internal([Term1,Term2],Bottom,N),
	!.
	
t5hc_infimum_internal(Term1,Term2,_,Infimum,N) :-
	(t5hc_top_p(N,Term1),Infimum = Term2),
	 !;
	(t5hc_top_p(N,Term2),Infimum = Term1),
        t5hc_insert_infimum([Term1,Term2],Infimum,N),
	!.

t5hc_infimum_internal(Term1,Term2,_,Infimum,N) :-
	t5hc_subsumption(N,Term1,Term2,Result),
	(Result == first, Infimum = Term1,
	 !;
	 Result == second,Infimum = Term2),
	t5hc_insert_infimum_internal([Term1,Term2],Infimum,N),
	!.

t5hc_infimum_internal(Term1,Term2,inf,Infimum,N) :-
 	t5hc_disjoint_internal(Term1,Term2,inf,Disjoint,N),
	(Disjoint == unknown, Infimum = unknown,
	 !;
	t5hc_bottom_p(N,Bottom), Infimum = Bottom,
	t5hc_insert_infimum_internal([Term1,Term2],Bottom,N)),
	!.

t5hc_infimum_internal(_,_,dis,unknown,_) :- !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 			   INTERSECTION					 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		         SUPER_INTERSECTION				 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% super_intersection: intersection of the supersets of the given         %
%                     concepts or roles					 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_super_intersection(_,[],[]) :- !.

t5hc_super_intersection(N,[Term1|Tail],Intersection) :-
	t5hc_super_intersection_internal([Term1|Tail],Intersection,N).

t5hc_super_intersection_internal([],[],_) :- !.

t5hc_super_intersection_internal([Term1|Tail],Intersection,N) :-
	t5hc_get(Term1,Supers1,_,_,N),
	t5hc_super_intersect(Tail,Supers1,Intersection,N),
	!.

t5hc_super_intersect([],List,List,_) :- !.

t5hc_super_intersect([Term2|Tail2],Supers1,Intersection,N) :-
	t5hc_get(Term2,Supers2,_,_,N),
	b5sort_intersect(Supers1,Supers2,IntersectResult),
	t5hc_super_intersect(Tail2,IntersectResult,Intersection,N),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% super_intersection with filters: interscetion of the elements of the   %
%				   supersets, which satisfy a special    %
%				   filter				 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_super_intersection(_,[],_,[]) :- !.

t5hc_super_intersection(N,[Term1|Tail],Filter,Intersection) :-
	t5hc_super_inters_int([Term1|Tail],Filter,Intersection,N).

t5hc_super_inters_int([],_,[],_) :- !.

t5hc_super_inters_int([Term1|Tail],Filter,Intersection,N) :-
	t5hc_get(Term1,Supers1,_,_,N),
	t5hc_filter_test(N,Supers1,Filter,FilterList1),
	t5hc_super_intersect(Tail,Filter,FilterList1,Intersection,N), 
	!.  

t5hc_super_intersect([],_,List,List,_) :- !.

t5hc_super_intersect([Term2|Tail2],Filter,FilterList1,Intersection,N) :-
	t5hc_get(Term2,Supers2,_,_,N),
	t5hc_filter_test(N,Supers2,Filter,FilterList2),
	b5sort_intersect(FilterList1,FilterList2,IntersectResult),
	t5hc_super_intersect(Tail2,Filter,IntersectResult,Intersection,N),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		           SUB_INTERSECTION				 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sub_intersection: intersection of the subsets of the given concepts or %
%		    roles						 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_sub_intersection(_,[],[]) :- !.

t5hc_sub_intersection(N,[Term1|Tail],Intersection) :-
	t5hc_sub_intersection_internal([Term1|Tail],Intersection,N).

t5hc_sub_intersection_internal([],[],_) :- !.

t5hc_sub_intersection_internal([Term1|Tail],Intersection,N) :-
	t5hc_get(Term1,_,Subs1,_,N),
	!,
	t5hc_sub_intersect(Tail,Subs1,Intersection,N).

t5hc_sub_intersect([],List,List,_) :- !.

t5hc_sub_intersect([Term2|Tail2],Subs1,Intersection,N) :-
	t5hc_get(Term2,_,Subs2,_,N),
	b5sort_intersect(Subs1,Subs2,IntersectResult),
	!,
	t5hc_sub_intersect(Tail2,IntersectResult,Intersection,N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sub_intersection with filters: interscetion of the elements of the     %
%				 subsets, which satify a special filter  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_sub_intersection(_,[],_,[]) :- !.

t5hc_sub_intersection(N,[Term1|Tail],Filter,Intersection) :-
	t5hc_sub_intersection_int([Term1|Tail],Filter,[],Intersection,N).

t5hc_sub_intersection_int([],_,[],_) :- !.

t5hc_sub_intersection_int([Term1|Tail],Filter,Intersection,N) :-
	t5hc_get(Term1,_,Subs1,_,N),
	t5hc_filter_test(N,Subs1,Filter,FilterList1),
	t5hc_sub_intersect(Tail,Filter,FilterList1,Intersection,N),    
	!.

t5hc_sub_intersect([],_,_,[],_) :- !.

t5hc_sub_intersect([Term2|Tail2],Filter,FilterList1,Intersection,N) :-
	t5hc_get(Term2,_,Subs2,_,N),
	t5hc_filter_test(N,Subs2,Filter,FilterList2),
	b5sort_intersect(FilterList1,FilterList2,IntersectResult),
	t5hc_sub_intersect(Tail2,Filter,IntersectResult,Intersection,N),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 			     UNION					 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			  SUPER_UNION					 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% super_union_id: union of the supersets of the given concepts or roles  %
%		  plus the concepts (or roles) themselves		 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_super_union_id(_,[],[]) :- !.

t5hc_super_union_id(N,TermList,IDUnion) :-
	t5hc_super_union_internal(TermList,Union,N),
	b5sort_unify(Union,TermList,IDUnion),
	!. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% super_union: union of the supersets of the given concepts or roles	 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_super_union(_,[],[]) :- !.

t5hc_super_union(N,[Term1|Tail],Union) :-
	t5hc_super_union_internal([Term1|Tail],Union,N).

t5hc_super_union_internal([],[],_) :- !.

t5hc_super_union_internal([Term1|Tail],Union,N) :-
	t5hc_get(Term1,Supers,_,_,N),
	t5hc_super_unify(Tail,Supers,Union,N),
	!.

t5hc_super_unify([],List,List,_).

t5hc_super_unify([Term2|Tail2],Supers1,Union,N) :-
	t5hc_get(Term2,Supers,_,_,N),
	b5sort_unify(Supers1,Supers,UnifyResult),
	t5hc_super_unify(Tail2,UnifyResult,Union,N),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% super_union_id with filters						 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_super_union(_,[],_,[]) :- !.

t5hc_super_union(N,[Term1|Tail],Filter,Union) :-
	t5hc_super_union_internal([Term1|Tail],Filter,[],[],Union,N).

t5hc_super_union_internal([],_,List,List,_) :- !.

t5hc_super_union_internal([Term1|Tail],Filter,List,Union,N) :-
	(t5hc_get(Term1,Supers,_,_,N),
	 t5hc_filter_test(N,Supers,Filter,FilterList) ->
	 b5sort_unify(FilterList,List,StoreUnion),
	 t5hc_super_union_internal(Tail,Filter,StoreUnion,Union,N);
	t5hc_super_union_internal(Tail,Filter,List,Union,N)),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			    SUB_UNION					 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sub_union_id: union of the subsets of the given concepts or roles plus %
%		the concepts (or roles) themselves		         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_sub_union_id(_,[],[]) :- !.

t5hc_sub_union_id(N,TermList,IDUnion) :-
	t5hc_sub_union_internal(TermList,Union,N),
	b5sort_unify(Union,TermList,IDUnion),
	!. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sub_union: union of the supersets of the given concepts or roles	 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_sub_union(_,[],[]) :- !.

t5hc_sub_union(N,[Term1|Tail],Union) :-
	t5hc_sub_union_internal([Term1|Tail],Union,N).

t5hc_sub_union_internal([],[],_) :- !.

t5hc_sub_union_internal([Term1|Tail],Union,N) :-
	t5hc_get(Term1,_,Subs,_,N),
	t5hc_sub_unify(Tail,Subs,Union,N),
	!.

t5hc_sub_unify([],List,List,_).

t5hc_sub_unify([Term2|Tail2],Subs1,Union,N) :-
	t5hc_get(Term2,_,Subs,_,N),
	b5sort_unify(Subs1,Subs,UnifyResult),
	t5hc_sub_unify(Tail2,UnifyResult,Union,N),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sub_union_id with filters						 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_sub_union_id(_,[],_,[]) :- !.

t5hc_sub_union_id(N,TermList,Filter,IDUnion) :-
	t5hc_sub_union_id_internal(TermList,Filter,[],IDUnion,N),
	!. 

t5hc_sub_union_id_internal([],_,List,List,_) :- !.

t5hc_sub_union_id_internal([Term1|Tail],Filter,List,IDUnion,N) :-
	t5hc_get(Term1,_,Subs,_,N),
	t5hc_filter_test(N,Subs,Filter,FilterList),
	(t5hc_filter_test_p(N,Term1,Filter) ->
	 b5sort_unify([Term1],FilterList,StoreUnion);
	b5sort_unify(List,FilterList,StoreUnion)),
	t5hc_sub_union_id_internal(Tail,Filter,StoreUnion,IDUnion,N),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sub_union with filters						 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_sub_union(_,[],_,[]) :- !.

t5hc_sub_union(N,[Term1|Tail],Filter,Union) :-
	t5hc_sub_union_internal([Term1|Tail],Filter,[],Union,N).

t5hc_sub_union_internal([],_,List,List,_) :- !.

t5hc_sub_union_internal([Term1|Tail],Filter,List,Union,N) :-
	(t5hc_get(Term1,_,Subs,_,N),
	 t5hc_filter_test(N,Subs,Filter,FilterList) ->
	 b5sort_unify(FilterList,List,StoreUnion),
	 t5hc_sub_union_internal(Tail,Filter,StoreUnion,Union,N);
	t5hc_sub_union_internal(Tail,Filter,List,Union,N)),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% prediactes for frequent use:						 %
% set :- assert                                                          %
% remove :- retract                                                      %
% get                                                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t5hc_set(Term,NewSupers,NewSubs,NewDisjoints,1) :-
	assert(t5hc_conc_hierarchy(Term,NewSupers,NewSubs,NewDisjoints)),
	!.
	
t5hc_set(Term,NewSupers,NewSubs,NewDisjoints,2) :-
	assert(t5hc_role_hierarchy(Term,NewSupers,NewSubs,NewDisjoints)),
	!.

t5hc_set(Term,NewSupers,NewSubs,NewDisjoints,_) :-
	assert(t5hc_term_hierarchy(Term,NewSupers,NewSubs,NewDisjoints)),
	!.
%%%%%%%%%%%%%%%%%%%

t5hc_retract(Term,Supers,Subs,Disjoints,1) :-
	retract(t5hc_conc_hierarchy(Term,Supers,Subs,Disjoints)),
	!.

t5hc_retract(Term,Supers,Subs,Disjoints,2) :-
	retract(t5hc_role_hierarchy(Term,Supers,Subs,Disjoints)),
	!.

t5hc_retract(Term,Supers,Subs,Disjoints,_) :-
	retract(t5hc_term_hierarchy(Term,Supers,Subs,Disjoints)),
	!.
%%%%%%%%%%%%%%%%%%%%

t5hc_get(Term,Supers,Subs,Disjoints,1) :-
	t5hc_conc_hierarchy(Term,Supers,Subs,Disjoints),
	!.

t5hc_get(Term,Supers,Subs,Disjoints,2) :-
	t5hc_role_hierarchy(Term,Supers,Subs,Disjoints),
	!.

t5hc_get(Term,Supers,Subs,Disjoints,_) :-
	t5hc_term_hierarchy(Term,Supers,Subs,Disjoints),
	!.


