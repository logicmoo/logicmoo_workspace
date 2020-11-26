:- module(swi_apeal,[% widget/2,
    assertin/1,gui_expansion/2,widget_decl/3,
   op(800, xfx, (widget)),
   op(1100, xfy, '<->'),
   op(1100, xfy, '<='),   
   op(600, xfy, (form)),
   op(600, fy, (unmanaged)),
   
   op(1000, xf, (cuTbl)),
   op(600, xfx, (wgetl)),op(600, xfx, (wget)),op(600, xfx, (wset)),op(600, xfx, (wproc)), 
   op(600, xfx, (wsproc))]).

:- dynamic(widget_decl/3).
:- discontiguous(widget_decl/3).
:- multifile(widget_decl/3).
head_expansion((Type widget Head <-> Decl), (widget_decl(Type,Head,Decl))):- nonvar(Head),!.
head_expansion((Type widget (Head <-> Decl)), (widget_decl(Type,Head,Decl))):- nonvar(Head),!.
head_expansion(((Type widget Head) <-> Decl), (widget_decl(Type,Head,Decl))):- nonvar(Head),!.
head_expansion(('<->'(Head,Decl):- Type), widget_decl(Type,Head,Decl)):- ignore(nonvar(Type)),!.
head_expansion(('widget'(Type,Head):- Decl), widget_decl(Type,Head,Decl)):- nonvar(Head),!.
head_expansion('<->'(Head,Type), widget_decl(Type,Head,'<->')):- nonvar(Head),!.
head_expansion('widget'(Type,Head), widget_decl(Type,Head,widget)):- ignore(nonvar(Head)),!.

gui_expansion(NC,_):- \+ compound(NC),!,fail.
gui_expansion(HeadIn,(:- assertin(Head))):- head_expansion(HeadIn,Head),!.
gui_expansion((HeadIn:-BodyIn),(Out)):- !, compound(HeadIn), head_expansion(HeadIn,Head),expand_term(Head:-BodyIn,Out).

called_swi_apeal(P):- throw(called_swi_apeal(P)).

assertin(G):- copy_term(G,C),clause(G,_),G=@=C.
assertin(G):- asserta(G),wdmsg(assertin(G)),!.
% assertin(G):- cgt:asserta(G).

def_swi_appeal(F/A):- functor(P,F,A),def_swi_appeal(P,F,A).
def_swi_appeal(P,_,_):- predicate_property(P,_),!.
def_swi_appeal(P,F,A):- 
  export(swi_apeal:F/A),
  assert(((swi_apeal:P) :- called_swi_apeal(P))),
  compile_predicates([swi_apeal:P]).

% widget(A,B):- wdmsg(call_widget(A,B)).

:- maplist(def_swi_appeal,[
 next_event/1, 
% nth/3, 
 % replace_text/2, 
 wget/2, 
 wproc/2, 
 wset/2, 
 xt_app_pending/2, 
 xt_context/2, 
 xt_convert/4, 
 xt_parse/2, 
 xt_parse_accelerator_table/2, 
 xt_translate/3]).

% :- dynamic(swi_apeal:term_expansion/4).



:- module_transparent(( assertin/1,gui_expansion/2,widget_decl/3, term_expansion/4)).
:- multifile(( assertin/1,gui_expansion/2,widget_decl/3)).
