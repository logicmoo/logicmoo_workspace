
/*

modes of compile for (H:-B) are 
 cwc,  = prolog's default (static or dynamic depending on built_in status)
 fwc,  = pfc(forward)chain  B==>-
 bwc,  = pfc(backwards)memoization 
 awc,  = asserta
 zwc,  = assertz
 pttp, = pttp iterative deepening
 dra,  = dynamic reording of alternatives (Solid tabling)
 other(_) Extendable

File mode interpretation (LHS=>RHS) are 
 pl    = Prolog (thus no re-interpretation)
 pfc   = Prolog Forward Chaining
 clif  = Common Logic Interchange Format
 cycl  = CycL 
 kif   = Knowledge Interchange Format
 chr   = CHR mode
 
additional features that may be added/manipulated in the body
 ctx 
 lin = argument linearization  via  linearize(lin,(H:-(G,B)),(HH:-BB),[],_,true,G).
 no_repeats/2 (on head vars)
 no_repeats/2 (on body vars)
 body reording
 proof recording
 variable lifespans are "depth minimized"
 unbound argument type constaining typing on
     entry
     exit

Per-Litteral features
 t(Pred,Arg1,Arg2,Arg3,...)
 t(Pred,Arg1,Arg2,Arg3,...,+Ctx)
 i_c(isa,inst,col)
 c_c(genls,col1,col2)
 p_p(genlPreds,inst,col)
 p_c_c(relationalAllExists...)
 p_c_u(relationalAllInstance...)
 p_c_c(relationalAllAll...)  

 u = any
 i = instances like MTs and people
 s = strings
 g = formulas
 l = lists
 n = numbers
 z = specials like 
 p = preds
 c = collections

 asserted_
 impossible_
 beliefs_

*/

% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_boxlog.pl
%:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
:- module(common_logic_boxlog,
          [ avoidHeadLoop/2,
            body_for_mpred_1/5,
            body_for_mpred_2/5,
            body_for_pfc/5,
           boxlog_to_pfc/2,
           boxlog_to_pfc_pass_2/3,
           boxlog_to_pfc_pass_3/4,
           boxlog_to_pfc_pass_4/2,           
            can_use_hack/1,
            % conjoin_body/3,
            conjoin_maybe/3,
            correct_mode/3,
            did_use_hack/1,
            get_op_alias_compile/2,
            get_reln/2,
            head_for_skolem/3,
            is_unit/2,
            is_unit/0,
            is_unit/3,
            is_unit/4,
            is_unit/5,
            is_unit/6,
            is_unit/7,
            is_unit/8,
            is_unit/9,
            is_unit/10,
            is_units_b/1,
            is_units_h/1,
            make_must_ground/3,
            make_vg/4,
            overlaping/3,
            isk/2,
            isk_bind/3,
            overlapingFunctors/2,
            reduce_literal/2,
            set_clause_compile/1,
            vg/1,
            vg/3]).


:- include(library('logicmoo/common_logic/common_header.pi')).

%:- endif.
:- reexport(baseKB:library('logicmoo/common_logic/common_logic_compiler.pl')). 

:- user:use_module(library(dialect/hprolog),[]).
:- common_logic_boxlog:use_module(library(dialect/hprolog),[]).

%:- use_module(library(lockable_vars)).
%:- use_module(library(util_varnames)).

:- set_how_virtualize_file(bodies).

:-  system:((
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-'))).

%= 


%% is_units_h( ?A) is semidet.
%
% If Is A Units Head.
%
is_units_h(A):-maplist(is_unit,A).

%= 	 	 

%% is_units_b( ?A) is semidet.
%
% If Is A Units Backtackable.
%
is_units_b(A):-maplist(is_unit,A).
% is_unit(A):-is_unit(A).

%= 	 	 

%% is_unit is semidet.
%
% If Is A Unit.
%
is_unit :- dmsg(is_unit).

%= 	 	 

%% is_unit( ?A, ?B) is semidet.
%
% If Is A Unit.
%
is_unit(A,B):-is_unit(A),is_unit(B).

%= 	 	 

%% is_unit( ?A, ?B, ?C) is semidet.
%
% If Is A Unit.
%
is_unit(A,B,C):-is_unit(A),is_unit(B),is_unit(C).

%= 	 	 

%% is_unit( ?A, ?B, ?C, ?D) is semidet.
%
% If Is A Unit.
%
is_unit(A,B,C,D):-is_unit(A),is_unit(B),is_unit(C),is_unit(D).
is_unit(A,B,C,D,E):-is_unit(A,B,C,D),is_unit(E).
is_unit(A,B,C,D,E,F):-is_unit(A,B,C,D),is_unit(E,F).
is_unit(A,B,C,D,E,F,G):-is_unit(A,B,C,D),is_unit(E,F,G).
is_unit(A,B,C,D,E,F,G,H):-is_unit(A,B,C,D),is_unit(E,F,G,H).
is_unit(A,B,C,D,E,F,G,H,I):-is_unit(A,B,C,D),is_unit(E,F,G,H,I).
is_unit(A,B,C,D,E,F,G,H,I,J):-is_unit(A,B,C,D),is_unit(E,F,G,H,I,J).

% might dtrace down when it is ~
% vg(G):-var(G),!,fail.

%= 	 	 

%% vg( ?G) is semidet.
%
% Vg.
%
vg(G):- (ground(G); ( compound(G), \+ (arg(_,G,E),var(E)))),!.

%= 	 	 

%% vg( ?VALUE1, ?B, ?C) is semidet.
%
% Vg.
%
vg(_,B,C):-vg(B),vg(C).

%= 	 	 

%% make_must_ground( ?H, ?BB, ?VG) is semidet.
%
% Make Must Be Successfull Ground.
%

make_must_ground(H,BB,VG):-
   term_slots(H,HVs)->
   term_slots(BB,BBVs)->
   hprolog:intersect_eq(HVs,BBVs,Shared),
   hprolog:list_difference_eq(HVs,BBVs,UHVs),
   hprolog:list_difference_eq(BBVs,HVs,UBBVs),
   make_vg(UBBVs,Shared,UHVs,VG),!.


%= 	 	 

%% make_vg( ?VALUE1, ?Shared, ?VALUE3, :TermS) is semidet.
%
% Make Vg.
%
make_vg([],[],[],true):-!.
make_vg([],[],_,true):-!.
make_vg(_,[],[],true):-!.
make_vg([],Shared,[],{(S)}):-  S=..[is_unit|Shared],!.
make_vg(_,Shared,_,{(S)}):-  S=..[is_unit|Shared],!.
make_vg(B,S,H,{(CB,CS,CH)}):- CB=..[is_units_b,B],CS=..[is_unit|S],CH=..[is_units_h,H].


%= 	 	 

%% set_clause_compile( ?TYPE) is semidet.
%
% Set Clause Compile.
%
set_clause_compile(TYPE):-op_alias((:-),TYPE).


%= 	 	 

%% get_op_alias_compile( ?I, ?O) is semidet.
%
% Get Oper. Alias Compile.
%
get_op_alias_compile(I,O):-get_op_alias(I,O),( I== (:-)),( O\== (:-)),!.
get_op_alias_compile(_,fwc).



%% boxlog_to_pfc( :TermPFCM, ?PFC) is semidet.
%
% Datalog Converted To Prolog Forward Chaining.
%
boxlog_to_pfc(H0,H0):- \+ compound(H0),!.
boxlog_to_pfc(H0,H0):- is_ftVar(H0),!.
boxlog_to_pfc([H|T],[HH|TT]):- !,boxlog_to_pfc(H,HH),boxlog_to_pfc(T,TT).
boxlog_to_pfc(kb_why_flags_assert(_,_,_Flags,B),PFC):- boxlog_to_pfc(B,PFC).
boxlog_to_pfc((H&T),(HH,TT)):- !,boxlog_to_pfc(H,HH),boxlog_to_pfc(T,TT).
boxlog_to_pfc((H v T),(HH;TT)):- !,boxlog_to_pfc(H,HH),boxlog_to_pfc(T,TT).
boxlog_to_pfc('$unused'(P),'$unused'(P)):-!.
boxlog_to_pfc( ('$unused'(H) :- B), ('$unused'(H) :- B)):-!.
boxlog_to_pfc(H0,PFCO):-
  sumo_to_pdkb(H0,H00),
  subst(H00,('not'),('~'),H),
  get_op_alias_compile((:-),TYPE),!,  
  with_vars_locked(throw,H,((maybe_notrace((boxlog_to_pfc_pass_1(TYPE,H,OUTPUTM))),!,
    OUTPUTM=OUTPUT))),
  subst(OUTPUT,('not'),('~'),PFCO).


boxlog_to_pfc_pass_1(TYPE,HB,OUTPUT):-
  expand_to_hb(HB,H,B),
  boxlog_to_pfc_pass_2(TYPE,(H:-B),OUTPUTM),!,
  boxlog_to_pfc_pass_4(OUTPUTM,OUTPUT).


%% boxlog_to_pfc_pass_2( ?TYPE, :TermH, ?OUTPUT) is semidet.
%
% Datalog Converted To Compile.
%

boxlog_to_pfc_pass_2(Why,I,O):-nonvar(O),!,boxlog_to_pfc_pass_2(Why,I,M),!,mustvv(M=O).
boxlog_to_pfc_pass_2(_,(H:-(Cwc,B)),(H:-(Cwc,B))):- Cwc == cwc,!.
boxlog_to_pfc_pass_2(Mode,(H:-(Cwc,B)),(H:-(Cwc,B))):- Mode == Cwc,!.
boxlog_to_pfc_pass_2(cwc,H,OUTPUT):-!, boxlog_to_pfc_pass_2((:-),H,OUTPUT).
boxlog_to_pfc_pass_2(==>,H,OUTPUT):-!, boxlog_to_pfc_pass_2(fwc,H,OUTPUT).
boxlog_to_pfc_pass_2(=>,H,OUTPUT):-!, boxlog_to_pfc_pass_2(fwc,H,OUTPUT).
boxlog_to_pfc_pass_2(<=,H,OUTPUT):-!, boxlog_to_pfc_pass_2(fwc,H,OUTPUT).
boxlog_to_pfc_pass_2(<-,H,OUTPUT):-!, boxlog_to_pfc_pass_2(bwc,H,OUTPUT).
boxlog_to_pfc_pass_2(rev(==>),H,OUTPUT):-!, boxlog_to_pfc_pass_2(fwc,H,OUTPUT).
boxlog_to_pfc_pass_2(rev(=>),H,OUTPUT):-!, boxlog_to_pfc_pass_2(fwc,H,OUTPUT).
boxlog_to_pfc_pass_2(~(WHAT),(~(H):-B),OUTPUT):-!, boxlog_to_pfc_pass_2(WHAT,(~(H):-B),OUTPUT).
boxlog_to_pfc_pass_2(~(WHAT),~(H),OUTPUT):-!, boxlog_to_pfc_pass_2(WHAT,~(H),OUTPUT).

boxlog_to_pfc_pass_2((:-),(~(H):-B),unused_true((~(H):-B))):- nonvar(H),a(prologBuiltin,H),!.
boxlog_to_pfc_pass_2((:-),(~(H):-B),(HH:-(cwc,BBB))):-body_for_pfc((:-),~(H),HH,B,BB),make_must_ground(HH,BB,MMG),conjoin_body(BB,MMG,BBB).
boxlog_to_pfc_pass_2((:-),(H:-B),OUT):- a(pfcControlled,H),boxlog_to_pfc_pass_2((bwc),(H:-B),OUT),!.
boxlog_to_pfc_pass_2((:-),(H:-B),(HH:-(cwc,BBB))):- body_for_pfc((:-),H,HH,B,BB),make_must_ground(HH,BB,MMG),conjoin_body(BB,MMG,BBB).
boxlog_to_pfc_pass_2((:-),~(H),~(H)):-  !.
boxlog_to_pfc_pass_2((:-),H,H):-  !.


boxlog_to_pfc_pass_2(fwc,(~(H):-B),unused_true((~(H):-B))):- nonvar(H),H = skolem(_,_),!.
boxlog_to_pfc_pass_2(fwc,(~(H):-B),OUT):- term_slots(H,HV),term_slots(B,BV), HV\==BV,!,boxlog_to_pfc_pass_2(bwc,(~(H):-B),OUT).
boxlog_to_pfc_pass_2(fwc,(~(H):-B),(BBBHH)):- body_for_pfc(fwc,~(H),HH,B,BB),make_must_ground(HH,BB,MMG),conjoin_body(BB,MMG,BBB),body_head_pfc(BBB,HH,BBBHH).
boxlog_to_pfc_pass_2(fwc,(H:-B),(BBBHH)):- body_for_pfc(fwc,H,HH,B,BB),make_must_ground(HH,BB,MMG),conjoin_body(BB,MMG,BBB),body_head_pfc(BBB,HH,BBBHH).
boxlog_to_pfc_pass_2(fwc,~(H),~(H)):-  !.
boxlog_to_pfc_pass_2(fwc,H,H):-  !.

boxlog_to_pfc_pass_2(bwc,(~(H):-B),unused_true((~(H):-B))):- nonvar(H),H = skolem(_,_),!.
boxlog_to_pfc_pass_2(bwc,(~(H):-B),(HH<-BBB)):-body_for_pfc(<-,~(H),HH,B,BB),make_must_ground(HH,BB,MMG),conjoin_body(BB,MMG,BBB).
boxlog_to_pfc_pass_2(bwc,(H:-B),OUT):- a(pfcRHS,H),term_slots(H,HV),term_slots(B,BV),HV==BV,boxlog_to_pfc_pass_2((fwc),(H:-B),OUT),!.
boxlog_to_pfc_pass_2(bwc,(H:-B),(HH<-BBB)):- body_for_pfc(<-,H,HH,B,BB),make_must_ground(HH,BB,MMG),conjoin_body(BB,MMG,BBB).
boxlog_to_pfc_pass_2(bwc,~(H),~(H)):-  !.
boxlog_to_pfc_pass_2(bwc,H,H):-  !.

boxlog_to_pfc_pass_2(TYPE,(H:-BB),OUTPUT):- !,boxlog_to_pfc_pass_3(TYPE,H,BB,OUTPUT).
boxlog_to_pfc_pass_2(TYPE,~(H),OUTPUT):-  !,boxlog_to_pfc_pass_3(TYPE,~(H),true,OUTPUT).
boxlog_to_pfc_pass_2(TYPE,H,OUTPUT):-     !,boxlog_to_pfc_pass_3(TYPE,H,true,OUTPUT).

body_head_pfc(BBB,HH,HH):-is_src_true(BBB),!.
body_head_pfc(BBB,HH,(BBB==>HH)).

%= 	 	 

%% boxlog_to_pfc_pass_3( ?TYPE, ?H, ?BB, :TermH) is semidet.
%
% Datalog Converted To Compile Extended Helper.
%
boxlog_to_pfc_pass_3(TYPE,~(H),BB,(~(H):-OUTPUT)):-!,conjoin_maybe(TYPE,BB,OUTPUT).
boxlog_to_pfc_pass_3(TYPE,H,BB,(H:-OUTPUT)):- conjoin_maybe(TYPE,BB,OUTPUT).
boxlog_to_pfc_pass_3(TYPE,H,BB,(H:-OUTPUT)):- conjoin_maybe(TYPE,BB,OUTPUT).



%= 	 	 

%% conjoin_maybe( ?TYPE, ?BB, ?OUTPUT) is semidet.
%
% Conjoin Maybe.
%
conjoin_maybe(_X,true,true):-!.
conjoin_maybe(TYPE,BB,OUTPUT):-conjoin(TYPE,BB,OUTPUT).


%= 	 	 

%% correct_mode( ?VALUE1, ?O, ?O) is semidet.
%
% Correct Pred Mode.
%
correct_mode(_,O,O):-var(O),!.
correct_mode((:-),{M},O):-!,correct_mode((:-),M,O).
correct_mode(Mode,(A,B),O):-!,correct_mode(Mode,A,AA),correct_mode(Mode,B,BB),conjoin_body(AA,BB,O).
correct_mode(_,O,O).


%= 	 	 

%% body_for_pfc( ?Mode, ?Head, ?NewNewHead, ?I, ?O) is semidet.
%
% Body For Prolog Forward Chaining.
%
body_for_pfc(Mode,Head,NewNewHead,I,O):-reduce_literal(Head,NewHead),!,body_for_mpred_1(Mode,NewHead,NewNewHead,I,O),!.
body_for_pfc(Mode,Head,NewHead,B,BB):- body_for_mpred_1(Mode,Head,NewHead,B,BB),!.


%= 	 	 

%% body_for_mpred_1( ?Mode, ?Head, ?HeadO, ?C, ?CO) is semidet.
%
% body for Managed Predicate  Secondary Helper.
%
body_for_mpred_1(Mode,Head,HeadO,C,CO):- (Mode ==(:-);Mode==(cwc);Mode==(<-)),
    overlaping(C,Head,Avoid),
    body_for_mpred_1(Mode,Head,HeadM,zzAvoidHeadLoop,AA),
    body_for_mpred_2(Mode,HeadM,HeadO,C,BB),!,
    subst(AA,zzAvoidHeadLoop,{Avoid},AAA),
       conjoin_body(AAA,BB,CM),correct_mode(Mode,CM,CO).
body_for_mpred_1(Mode,Head,NewNewHead,I,O):-body_for_mpred_2(Mode,Head,NewNewHead,I,M),correct_mode(Mode,M,O).



%= 	 	 

%% overlaping( ?C, ?Head, ?Avoid) is semidet.
%
% Overlaping.
%
overlaping(C,Head,Avoid):- (is_ftVar(C);is_ftVar(Head)),!,Avoid=avoidHeadLoop(C,Head).
overlaping(~(C),Head,Avoid):-is_ftNonvar(C),!,overlaping(C,Head,Avoid).
overlaping(C,~(Head),Avoid):-is_ftNonvar(Head),!,overlaping(C,Head,Avoid).
overlaping(C,Head,Avoid):-is_ftNonvar(Head),is_ftNonvar(C), compound(C),compound(Head),once((get_reln(C,FC),get_reln(Head,HC))),!,overlapingFunctors(FC,HC),!,Avoid=avoidHeadLoop(C,Head).


%= 	 	 

%% overlapingFunctors( ?FC, ?HC) is semidet.
%
% Overlaping Functors.
%
overlapingFunctors(FC,HC):- (\+ \+ FC=HC),!.
overlapingFunctors(t,_):-!.
overlapingFunctors(_,t):-!.


%= 	 	 

%% get_reln( ?C, ?F) is semidet.
%
% Get Reln.
%
get_reln(C,F):-var(C),!,F=_.
get_reln(C,F):-is_ftVar(C),!,F=_.
get_reln(~(C),RO):-nonvar(C),!,get_reln(C,RO).
get_reln(~(C),RO):-nonvar(C),!,get_reln(C,RO).
get_reln(\+(C),RO):-nonvar(C),!,get_reln(C,RO).
get_reln('{}'(C),RO):-nonvar(C),!,get_reln(C,RO).
get_reln(C,RO):-get_functor(C,F),
  (F==t->
     (arg(1,C,R),(is_ftVar(R)->RO=t;RO=R));
     RO=F),!.


%= 	 	 

%% avoidHeadLoop( ?C, ?Head) is semidet.
%
% Avoid Head Loop.
%
avoidHeadLoop(C,Head):- stack_check, ground(C),(C\=Head),\+ is_loop_checked(C).



%= 	 	 

%% isk( ?Var, ?SK) is semidet.
%
% Isk.
%
isk(Var,SK):- ignore((=(Var,SK))),!.
isk(Var,SK):- when('?='(Var,Val),isk_bind(Var,Val,SK)).

%= 	 	 

%% isk_bind( ?Var, ?Val, ?SK) is semidet.
%
% Isk Bind.
%
isk_bind(Var,Val,SK):-show_call(add_cond(Var,[Val,SK])).

% Like this one better but it breaks things
% head_for_skolem(H,if_missing(H,pfclog((HH:-isk(NewOut,SK)))),skolem(In,SK)):-contains_var(In,H),subst(H,In,NewOut,HH),!.
% 
% ugly but works

%= 	 	 

%% head_for_skolem( ?H, :TermH, :TermIn) is semidet.
%
% Head For Skolem.
%
head_for_skolem(H,if_missing(H,HH),skolem(In,NewOut)):- contains_var(In,H),subst(H,In,NewOut,HH),!.

is_skolem_arg(Var):- callable(Var),functor(Var,F,_),atom_concat('sk',_,F),atom_concat(_,'Fn',F).


%% body_for_mpred_2( +Mode, +Head, -NewHead, +BodyIn, -NewBody) is semidet.
%
%  Make a NewHead and a NewBody for +Mode using Head+Body
%

%body_for_mpred_2(_Mode,Head,Head,A,A):- must(\+ is_ftVar(A)),fail.
body_for_mpred_2(_Mode,Head,Head,A,A):-is_ftVar(A),!,sanity(\+ is_ftVar(Head)).

% body_for_mpred_2(_Mode,if_missing(A,B),if_missing(A,B),Body,Body):-!.

body_for_mpred_2(Mode,Head,HeadO,(A,B), C):-!,body_for_mpred_1(Mode,Head,HeadM,A,AA),body_for_pfc(Mode,HeadM,HeadO,B,BB),conjoin_body(AA,BB,C).
body_for_mpred_2(Mode,Head,HeadO,(A;B),(AA;BB)):-!,body_for_mpred_1(Mode,Head,HeadM,A,AA),body_for_pfc(Mode,HeadM,HeadO,B,BB).
body_for_mpred_2((:-),Head,HeadO,(A/B),(AA,BB)):-!,body_for_mpred_1(Mode,Head,HeadM,A,AA),body_for_pfc(Mode,HeadM,HeadO,B,BB).
body_for_mpred_2(Mode,Head,HeadO,(A/B),(AA/BB)):-!,body_for_mpred_1(Mode,Head,HeadM,A,AA),body_for_pfc(Mode,HeadM,HeadO,B,BB).

body_for_mpred_2((fwc),H,HEAD,I,O):- H\=if_missing(_,_), sub_term(Var,H),attvar(Var),get_attr(Var,sk,Sk),
   subst(H,Var,NewVar,NewH),head_for_skolem(NewH,SKHEAD,skolem(NewVar,Sk)), !,
   body_for_mpred_2((fwc),SKHEAD,HEAD,I,O).

body_for_mpred_2((fwc),HwSk,HEAD,I,O):- HwSk\=if_missing(_,_), sub_term(Sk,HwSk),is_skolem_arg(Sk),subst(HwSk,Sk,Var,H),
   subst(H,Var,NewVar,NewH),head_for_skolem(NewH,SKHEAD,skolem(NewVar,Sk)), !,
   body_for_mpred_2((fwc),SKHEAD,HEAD,I,O).

body_for_mpred_2((fwc),H,HEAD,{skolem(In,NewOut)},true):- head_for_skolem(H,HEAD,skolem(In,NewOut)),!.
body_for_mpred_2((fwc),H,HEAD,skolem(In,NewOut),true):- head_for_skolem(H,HEAD,skolem(In,NewOut)).
body_for_mpred_2(_Mode,~(Head),~(Head),skolem(_,_),true).
%body_for_mpred_2(Mode,H,H,skolem(_,_),true).
body_for_mpred_2(_Mode,Head,Head,skolem(In,Out),{ignore(In=Out)}).

body_for_mpred_2(Mode,Head,HeadO,(X & Y),XY):-
   body_for_mpred_2(Mode,Head,HeadM,X,XX),body_for_mpred_2(Mode,HeadM,HeadO,Y,YY),
   conjoin_maybe(XX,YY,XY),!.
  
body_for_mpred_2(Mode,Head,HeadO,(X , Y),XY):-
   body_for_mpred_2(Mode,Head,HeadM,X,XX),body_for_mpred_2(Mode,HeadM,HeadO,Y,YY),
   conjoin_maybe(XX,YY,XY),!.

body_for_mpred_2(Mode,Head,HeadO,(X ; Y),(XX ; YY)):-
   body_for_mpred_2(Mode,Head,HeadM,X,XX),
   body_for_mpred_2(Mode,HeadM,HeadO,Y,YY),!.
   

body_for_mpred_2(_Mode,Head,Head,poss(X),poss(X)).
body_for_mpred_2(_Mode,Head,Head,poss(X),{loop_check(\+ ~(X),true)}).
% body_for_mpred_2(Mode,Head,Head,skolem(In,Out),{(In=Out;when('nonvar'(In),ignore((In=Out))))}).
% body_for_mpred_2(Mode,Head,Head,skolem(In,Out),{when((?=(In,_);nonvar(In)),ignore(Out=In))}).
body_for_mpred_2(Mode,Head,NewHead,B,BBB):- once(reduce_literal(B,BB)),B\=@=BB,!,body_for_mpred_1(Mode,Head,NewHead,BB,BBB).
body_for_mpred_2(Mode,Head,HeadO,proven_tru(H),(HH)):-  !,body_for_mpred_2(Mode,Head,HeadO,H,HH).
body_for_mpred_2(Mode,Head,HeadO,once(H),(HH)):-  !,body_for_mpred_2(Mode,Head,HeadO,H,HH).
body_for_mpred_2(Mode,Head,HeadO,proven_neg(H),(HH)):-  !,body_for_mpred_2(Mode,Head,HeadO,~H,HH).
body_for_mpred_2(_Mode,Head,Head,different(A,B),{dif:dif(A,B)}).
body_for_mpred_2(_Mode,Head,Head,A,{A}):-a(prologBuiltin,A),!.
body_for_mpred_2(_Mode,Head,Head,A,A).


%= 	 	 

%% reduce_literal( ?A, ?A) is semidet.
%
% Reduce Literal.
%
reduce_literal(A,A):-is_ftVar(A),!.
reduce_literal(~(A),~(A)):-is_ftVar(A),!.
reduce_literal(~(different(P3, R3)),not_different(P3, R3)).
reduce_literal(~(mudEquals(P3, R3)),different(P3, R3)).
% reduce_literal(~(skolem(P3, R3)),different(P3, R3)).
reduce_literal(~(termOfUnit(P3, R3)),different(P3, R3)).
reduce_literal(~(equals(P3, R3)),different(P3, R3)).
reduce_literal(termOfUnit(P3, R3),skolem(P3, R3)).
reduce_literal(~({A}),AA):- reduce_literal(~(A),AA), AA \=@= ~(A),!.
reduce_literal(~(A),~(A)):-!.
reduce_literal(A,A).


%= 	 	 

%% can_use_hack( ?VALUE1) is semidet.
%
% Can Use Hack.
%
can_use_hack(two_implications):-!,fail.
can_use_hack(X):- did_use_hack(X).

%= 	 	 

%% did_use_hack( ?X) is semidet.
%
% Did Use Hack.
%
did_use_hack(X):-dmsg(did_use_hack(X)).



%= 	 	 

%% boxlog_to_pfc_pass_4( :TermIN, :TermOUT) is semidet.
%
% Datalog Converted To Prolog.
%
boxlog_to_pfc_pass_4(IN,OUT):-quietly(leave_as_is(IN)),!,IN=OUT.
boxlog_to_pfc_pass_4(H, HH):-is_list(H),!,must_maplist(boxlog_to_pfc_pass_4,H,HH).
%boxlog_to_pfc_pass_4(IN,OUT):-once(demodal_sents('$VAR'('KB'),IN,MID)),IN\=@=MID,!,boxlog_to_pfc_pass_4(MID,OUT).
boxlog_to_pfc_pass_4(IN,OUT):-once(subst_except(IN,not,~,MID)),IN\=@=MID,!,boxlog_to_pfc_pass_4(MID,OUT).
%boxlog_to_pfc_pass_4(IN,OUT):-once(subst_except(IN,poss,possible_t,MID)),IN\=@=MID,!,boxlog_to_pfc_pass_4(MID,OUT).

boxlog_to_pfc_pass_4((V:- TRUE),VE):- is_src_true(TRUE),boxlog_to_pfc_pass_4(V,VE),!.
boxlog_to_pfc_pass_4((H:- B),(HH:- BB)):- !,boxlog_to_pfc_pass_4(H,HH),boxlog_to_pfc_pass_4(B,BB).
boxlog_to_pfc_pass_4((H & B),(HH , BB)):- !,boxlog_to_pfc_pass_4(H,HH),boxlog_to_pfc_pass_4(B,BB).
boxlog_to_pfc_pass_4((H v B),(HH ; BB)):- !,boxlog_to_pfc_pass_4(H,HH),boxlog_to_pfc_pass_4(B,BB).
boxlog_to_pfc_pass_4((H , B),(HH , BB)):- !,boxlog_to_pfc_pass_4(H,HH),boxlog_to_pfc_pass_4(B,BB).
boxlog_to_pfc_pass_4((H ; B),(HH ; BB)):- !,boxlog_to_pfc_pass_4(H,HH),boxlog_to_pfc_pass_4(B,BB).
boxlog_to_pfc_pass_4(H,O):- H=..[N,nesc(F)],kb_nlit(_,N),nonvar(F),!,HH=..[N,F],boxlog_to_pfc_pass_4(HH,O).

% boxlog_to_pfc_pass_4(IN,OUT):-demodal_sents(_KB,IN,M),IN\=@=M,!,boxlog_to_pfc_pass_4(M,OUT).

boxlog_to_pfc_pass_4( H, HH):- H=..[F|ARGS],!,boxlog_to_pfc_pass_4(ARGS,ARGSO),!,HH=..[F|ARGSO].
boxlog_to_pfc_pass_4(BL,PTTP):- baseKB:as_prolog_hook(BL,PTTP).

:- fixup_exports.

