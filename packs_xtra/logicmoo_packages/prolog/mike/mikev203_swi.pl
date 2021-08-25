/* File: mikev203.pl (incorporates old *.PL files, which can be
   searched for by name embedded within this master file)

                            *************
                             M I K E - 2
                            *************
               Micro Interpreter for Knowledge Engineering
                  {written in Edinburgh-syntax Prolog}

                           VERSION = 2.03s (SICSTUS)


MIKE: Copyright (c) 1989, 1990, 1991 The Open University (U.K.)

MIKE is intended for educational purposes, and may not
be sold as or incorporated in a commercial product without
written permission from: The Copyrights Officer, Open University,
Milton Keynes MK7 6AA,  U.K.

The Open University accepts no responsibility for any legal or other
consequences which may arise directly or indirectly as a result of the
use of all or parts of the contents of this program.


SICSTUS Prolog amendments courtesy Peter Ross, University of Edinburgh,
1990/91

*/

/*
README preface notes from Peter Ross (edited slightly from the original
email message to Marc Eisenstadt):

Please note that the University of Edinburgh file mike_start_up is *NOT*
included!  That file simply has a big welcome message and a few tools eg to
invoke external editor and reload the result - but nothing integral to MIKE's
functioning.

The file all_of_mike2.pl is a version which will compile under
SICSTUS; The files mike.xref, mike.impexp were created
by our own dear old XREF program, and are mainly for those who
want to find their way around the sources.

all_of_mike2.pl is NOT just a concatenation of the original OU  *.pl 
files, however. There are some compiler directives at the start,
and a few name changes necessary to make the source compatible
with SICSTUS eg not/1 -> \+/1, findall/3 -> 'pd624 findall'/3.

The SICSTUS compiled version runs the demos correctly.

Peter Ross
14 Nov 1990
*/


/*
----- all_of_mike2.pl ------------------------------
*/

 /* Dynamic declarations for the benefit of SICSTUS.

   Note that very few of these are about actual procedures
   defined at run-time, the vast majority are just about
   terms which are asserted and/or retracted. But most such
   `data' is called somewhere, eg
      somewhere:   ..., assert(rete(on)), ...
      elsewhere:   ..., rete(on), ...
   Although the compiler will cope if it meets the assertion
   before the call, by noting that the term is dynamic, it
   may not cope (ie I dont know) if the call happens to appear
   before the assertion in the source code. Quintus documentation
   includes a warning about this. So I have just added a dynamic/1
   declaration for all such cases.

   For future reference: how to find them:
     (a) cat all the sources together into one big file (you are
         reading such a product) for ease of processing.
     (b) use grep to find all asserts and retracts
     (c) edit that result, either directly to create the dynamic/1
         assertions, or - to save you the time of editing and
         eliminating duplicates - use your editor to turn the grep
         output into a whole series of Prolog unit clauses with a
         standard uncontentious functor, such as xxx/1.
     (d) In which case, you can write a trivial program to write out
         the dynamic/1 declarations, then use UNIX sort and uniq to
         eliminate duplicates. That's what I did, it's quick.
   Peter Ross, 14 Jan 1991
*/


:- dynamic 'pd624 current cycle is'/1.
:- dynamic 'pd624 fc_history'/3.
:- dynamic 'pd624 wme'/1.
:- dynamic 'pd624 symbol root'/2.
:- dynamic 'wm counter'/1.
:- dynamic addrete/1.
:- dynamic addtms/1.
:- dynamic allowable_back_door_initialise/1.
:- dynamic allowable_prolog_lhs/1.
:- dynamic allowable_prolog_rhs/2.
:- dynamic already_did/2.
:- dynamic bank/1.
:- dynamic bi/4.
:- dynamic conflict_set/2.
:- dynamic current_conflict_resolution_strategy/1.
:- dynamic currentb/2.
:- dynamic currentdb/2.
:- dynamic delrete/1.
:- dynamic deltms/1.
:- dynamic enabled/3.
:- dynamic fc_do_one_cycle/1.
:- dynamic fc_undo_rhs/2.
:- dynamic go_with/1.
:- dynamic how/1.
:- dynamic instance_of/2.
:- dynamic is_child_world_of/2.
:- dynamic jhistory/1.
:- dynamic justification/3.
:- dynamic justify/1.
:- dynamic kb/1.
:- dynamic lhsthreshold/1.
:- dynamic m/2.
:- dynamic mem/2.
:- dynamic memkb/1.
:- dynamic method/1.
:- dynamic nid/1.
:- dynamic pd624_flag/1.
:- dynamic receives_answer/2.
:- dynamic rete/1.
:- dynamic rete_compile/0.
:- dynamic root/3.
:- dynamic rul/4.
:- dynamic rule/1.
:- dynamic seek_rete_p_on_p_go_back/0.
:- dynamic seek_rete_p_on_p_go_fwd/0.
:- dynamic special_case/1.
:- dynamic subclass_of/2.
:- dynamic stack/2.
:- dynamic temp/1.
:- dynamic tms/1.
:- dynamic true_rule/1.
:- dynamic unique_pd624_symbol/1.
:- dynamic varg/1.
:- dynamic with/2.
:- dynamic worldcount/1.

/* file: LOADOPS.PL {directives with operator declarations} */

/*
                            *************
                             M I K E - 2
                            *************
               Micro Interpreter for Knowledge Engineering
                  {written in Edinburgh-syntax Prolog}

MIKE: Copyright (c) 1989, 1990, 1991 The Open University (U.K.)

MIKE is intended for educational purposes, and may not
be sold as or incorporated in a commercial product without
written permission from: The Copyrights Officer, Open University,
Milton Keynes MK7 6AA, U.K.

The Open University accepts no responsibility for any legal or other
consequences which may arise directly or indirectly as a result of the
use of all or parts of the contents of this program.
*/

/* ================ O P E R A T O R   D E C L A R A T I O N S ========= */

:- op(1200,fx,rule).
:- op(1199,xfx,with).
:- op(1199,xfx,forward).
:- op(1199,xfx,backward).
:- op(1100,fx,if).
:- op(1000,xfx,then).
:- op(1000,xfx,from).
:- op(999, fx, [make_value,add_value]).
:- op(955,xfy, or).
:- op(954, xfy, '&').
:- op(953,fx, '--').
:- op(953,fx, query).
:- op(952,xfy, receives_answer).
:- op(950,fx, establish).
:- op(950,fx, deduce).
:- op(950,fx, say).
:- op(950,fx, remove).
:- op(950,fx, note).
:- op(950,fx, add).
:- op(950,fx, announce).
:- op(950,xfy,explained_by).
:- op(950,fx,why).
:- op(950,fx,how).
:- op(950,fx,justify). /* to display TMS proof tree */
:- op(950,fx,describe).
:- op(950,fx,show).
:- op(950,fx,strategy).
:- op(939,xfx, cf).
:- op(900,fx,'~').
:- op(899,fx,the).
:- op(899,fx,all).
:- op(898,xfx,of).
:- op(897,xfx,to).
:- op(876,xfx,for).
:- op(850,xfx,are).
:- op(800,xfx,instance_of).
:- op(800,xfx,subclass_of).
:- op(799,xfx,':').
:- op(700,fx,kb).
:- op(200,xfx,'<--').
:- op(10,fx,'?').
/* crucial operators from XTEND.PL */
:- op(740,xfx,':=').
/*:- op(900,fx, ed).*/
/*:- op(900,fx, qed).*/
/*:- op(900,fx, ked).*/
/* file: ENGINE1.PL {main code for MIKE rule/frame engine} */
/*       see also ENGINE2.PL for more!                     */
/*
                            *************
                             M I K E - 2
                            *************
               Micro Interpreter for Knowledge Engineering
                  {written in Edinburgh-syntax Prolog}

MIKE: Copyright (c) 1989, 1990, 1991 The Open University (U.K.)

MIKE is intended for educational purposes, and may not
be sold as or incorporated in a commercial product without
written permission from: The Copyrights Officer, Open University,
Milton Keynes MK7 6AA, U.K.

The Open University accepts no responsibility for any legal or other
consequences which may arise directly or indirectly as a result of the
use of all or parts of the contents of this program.
*/
/* ENGINE1.PL & ENGINE2.PL contain the essential innards of MIKE.
   Some auxilliary code is contained in the files UTIL.PL and IO.PL,
   and the kernel of the forward chaining executive loop is in FC_EXEC.PL
   ENGINE1.PL & ENGINE2.PL are subdivided into six main parts, as follows:
   1.  Backward chaining
   2.  Frame manipulation
   3.  Demon processing
   (N.B. the last three parts are in file ENGINE2.PL)
   4.  Top level
   5.  Forward chaining (left hand side conditions)
   6.  Forward chaining (right hand side actions)
*/
/* ================ (1) B A C K W A R D  C H A I N I N G =============== */

/* prove1 invokes the workhorse prove, but wraps it inside
   some MIKE tracing information, settable by user request.
   (Tracing is set up by a call to ?- tracing.  It is defined in UTIL.PL ) */

prove1(Goal,Depth) :-
  when_enabled('show backward chaining' for Goal/Depth),
  prove(Goal,Depth),
  when_enabled('show outcome of backward chaining' for Goal/Depth).

prove1(Goal,Depth) :-
  enabled('show outcome of backward chaining',' enabled ',_),
  write('<- '),tab(Depth),write('- '),conj_write(Goal),nl,
  !,
  fail.

/* Top level invocation of prove (usually done via  deduce <pattern> ) */
prove((A&B)) :-    /* Conjunction of goals */
  !,
  prove1((A&B),0).  /* set initial depth to zero */

prove(X) :-        /* Singleton goal */
  prove1(X,0).     /* set initial depth to zero */

/* prove/2 is the main meta-level interpreter */
/* case 1: conjunction of goals... tackle head, then rest */
prove((Head & Rest),D) :-
    prove1(Head,D),  /* use prove1 to invoke optional tracing info... */
 (Rest = (_ & _) , prove(Rest,D);  /* Rest is conjunct? no trace */
  \+(Rest = (_ & _)), prove1(Rest,D)). /* Rest is singleton? prove1 trace OK */

prove((A&B),_):- !,fail. /* only arrive here if above calls fail */

/* case 2: A disjunction of goals */
prove((A or B),D) :-
    prove1(A,D), !.
prove((_ or B),D) :-
    prove1(B,D).

/* case 3, e.g. prove(the Slot of Object is Filler) is handled below
  by the call to in_wm (now covered by case 6) */

/* cases 4a-4b: special traps for relational operators > and <.
   The code for >= or =< is equally trivial */
prove(the Slot of Object > Value,D) :-
    !,
    fetch(Object, Slot, Filler, [Object], _),
    Filler > Value.
prove(the Slot of Object < Value,D) :-
    !,
    fetch(Object, Slot, Filler, [Object], _),
    Filler < Value.
prove(the Slot of Object >= Value,D) :-
    !,
    fetch(Object, Slot, Filler, [Object], _),
    Filler >= Value.
prove(the Slot of Object =< Value,D) :-
    !,
    fetch(Object, Slot, Filler, [Object], _),
    Filler =< Value.

/* case 5a: instead of 'Foo is in class Bar' we say 'Foo instance_of Bar' */
prove(X instance_of Y,D) :-
    !,
    X instance_of Y with _. /* simple pattern-match in database, ignore _ */
/* case 5b: analogous to case 5a, but this time for subclass_of */
prove(X subclass_of Y,D) :-
    !,
    X subclass_of Y with _. /* simple pattern-match in database, ignore _ */

/* case 6: allow the system to ask the user for a value.  This may also
be used to check a value, if you specify one.  If the user
specifies values and the value doesn't match with the one
the user gives at query-time, the goal will fail. */
prove((query X),D):-
    query X.

/* case 7: check that the thing is not already in working memory.  This
    would be the case of, say, an unstructured fact eg. [kettle,on] */
prove(WME,_):-
    in_wm(WME).  /* N.B. the code for in_wm(WME) now handles case 3, too,
                 namely frame access of the form: the X of Y is Z */

/* case 8: retrieve backward chaining rule from database, and prove premises.
 N.B. only a single conclusion is allowed in rule (i.e. Horn clause form) */
prove(Conc,D) :-
    (rule Name backward if Premise then Conc),  /* single conclusion only!!! */
 D1 is D + 1,                        /* increase depth for recursive proof */
 when_enabled('show chosen rule' for  /* optional trace display */
               [(rule Name backward if Premise then Conc)]),
    prove1(Premise,D1).                 /* recursive proof of premise(s) */

/* =============== (2) F R A M E    M A N I P U L A T I O N ============= */

/* fetch is the main frame-traversal utility.
 Fourth argument is a 'history list' to perform cycle detection
 Fifth argument tells us where recursive lookup terminates,
 i.e. from which ultimate object the property has been inherited, which could
 be useful for fancier implementation, to reveal that, say,
 the filler of slot FOO was inherited from class BAR.
 Most of the work is done by contains, described below */

fetch(Object, Attribute, Value, History, TerminalObject) :-
    (Object instance_of Class with Body), /* get from DB */
    traverse_body(Object,Body,Attribute,
               slot(value(V),inheritance(I),cardinality(C))),
                                            /* arg 3 is OUTPUT here */
    ((I=[supersede];I=supersede),\+(V=[]),!,'pd624 member'(Value,V) ;
         'pd624 member'(Value,V)). /* fall through to next clause on btrack */

fetch(Object, Attribute, Value, History, TerminalObject) :-
    (Object subclass_of Class with Body), /* alternative way to get it */
    traverse_body(Object,Body,Attribute,
               slot(value(V),inheritance(I),cardinality(C))),
                                            /* arg 3 OUTPUT here */
    ((I=[supersede];I=supersede),\+(V=[]),!,'pd624 member'(Value,V) ;
         'pd624 member'(Value,V)). /* fall through to next clause on btrack */

fetch(Object,Attribute,Value,CycleList,TerminalNode):-
    isa_linked(Object,Super), /* search up class links */
    \+ 'pd624 member'(Super, CycleList), /* loop detector */
    fetch(Super,Attribute,Value,[Super|CycleList],TerminalNode).

fetch1(Object,Attribute,Value,_,Object):-
    (Object instance_of Class with Body),
    contains(Body,Attribute:Value).

fetch1(Object,Attribute,Value,_,Object):-
    (Object subclass_of Class with Body),
    contains(Body,Attribute:Value).

fetch1(Object,Attribute,Value,CycleList,TerminalNode):-
    isa_linked(Object,Super),
    \+ 'pd624 member'(Super,CycleList),
    fetch1(Super,Attribute,Value,[Super|CycleList],TerminalNode).

/* store/3 adds new slot-filler info at run time
 COMPATIBILITY NOTE: Be sure that the object definitions are INTERPRETED in
 MacPROLOG, or DYNAMIC in Quintus Prolog, so that the database can be updated
 correctly */
store(Object, Attribute, NewValue) :-
 var(NewValue),
 'pd624 write'(['ERROR: Illegal use of frames.  You are not allowed to'
 ,nl,'have a variable as a frame slot filler.',nl,
 'You tried to note the ',Attribute,' of ',Object,' is a variable. ',nl,
 '*** FRAME ',Object,' NOT UPDATED ***',nl]),!.

store(Object, Attribute, NewValue) :-
    (Object instance_of Class with Body),
    !,
    subst(Object,(Attribute:Old),
    (Attribute:NewValue), Body, NewBody),
    retract((Object instance_of Class with Body1)),
    delrete(the Attribute of Object is Old),
    assert((Object instance_of Class with NewBody)),
    addrete(the Attribute of Object is NewValue),
    if_added_demon_check(NewBody,Object,Attribute,NewValue,Class).

store(Object, Attribute, NewValue) :-
    (Object subclass_of Class with Body),
    !,
    subst(Object,(Attribute:Old),
    (Attribute:NewValue), Body, NewBody),
    retract((Object subclass_of Class with Body1)),
    delrete(the Attribute of Object is Old),
    assert((Object subclass_of Class with NewBody)),
    addrete(the Attribute of Object is NewValue),
    if_added_demon_check(NewBody,Object,Attribute,NewValue,Class).

store(Object, Attribute, NewValue) :-
    assert((Object instance_of 'Newly Created Object' with Attribute:NewValue)),
    addrete(Object instance_of 'Newly Created Object'),
    addrete(the Attribute of Object is NewValue).

if_added_demon_check(NewBody,Object,A,V,C):-
  'pd624 cmember'(A:Body,NewBody),
  if_added_demon(Body,Object,A,V,C).
if_added_demon_check(NewBody,Object,A,V,C):-
  if_added_demon([],Object,A,V,C).

/* subst: slot-filler substitution
Args are as follows:
1: (input) object for ?self substitution
2: (input) old slot:filler combo we want over-written
3: (input) new slot:filler combo we want in place of oldie
4: (input) 'Body' of the frame, typically a long conjunct ...,...,...,...
5: (ouput) 'NewBody', i.e. the old Body with new stuff replacing old
*/
subst(Object,Attribute:_, Attribute:New, (Attribute:[H|T],Rest),
                                         (Attribute:NewList,Rest)):-
        subst_facet(Attribute:New,[H|T],[H|T],NewList,Object).
subst(Object,Attribute:_, Attribute:New, Attribute:[H|T],
                                         Attribute:NewList):-
        subst_facet(Attribute:New,[H|T],[H|T],NewList,Object).


subst(Object,Attribute:Old, Attribute:New, Attribute:Old, Attribute:New):-
    type_check([],Object,Attribute,New),
    cardinality_check([],Object,Attribute,New).

subst(Object,Attribute1:Old1, Attribute:New, Attribute2:Old2,
(Attribute2:Old2,Attribute:New)):-
    type_check([],Object,Attribute,New),
    cardinality_check([],Object,Attribute,New).

subst(Object,Attribute:Old, Attribute:New, (Attribute:Old,Rest),
(Attribute:New,Rest)):-
    type_check([],Object,Attribute,New),
    cardinality_check([],Object,Attribute,New).

subst(Object,X,Y, (First,Rest), (First,NewRest)) :-
    subst(Object,X,Y,Rest,NewRest).

/* substitute facet assumes that you can only change the value of an individual
facet, not other facets like cardinality or type, which CANNOT be changed
dynamically.  The substitue does a change as opposed to an add, i.e.
existing value(s) are destructively replaced.  There is as yet no
additional flag to AUGMENT an existing set of values, although
section 4.2.2 of the MIKE reference manual shows how to accomplish this. */

/* variables A:V typically refer to Attribute:Value, which we tend
to use synomynously with Slot:Filler */

subst_facet(A:V,Body,[value:V|Rest],[value:V|Rest],Object):-
    type_check(Body,Object,A,V),
    cardinality_check(Body,Object,A,V).
subst_facet(A:V,Body,[value:V1|Rest],[value:V|Rest],Object):-
    type_check(Body,Object,A,V),
    cardinality_check(Body,Object,A,V).
subst_facet(A:V,B,[H|Rest],[H|Output],Object):-
    subst_facet(A:V,B,Rest,Output,Object).

/* searching up 'isa' chain can either involve 'instance_of' or
     alternatively 'subclass_of' */
isa_linked(X, Y) :-
    X instance_of Y with _ .
isa_linked(X, Y) :-
    X subclass_of Y with _ .

/* ---------- c o n t a i n s ---------------------------
this searches down the (usually compound) 'Body', looking for a
slot:filler combination.  For example, suppose that we're looking for
age:34 in some particular frame.  By the time contains/2 is invoked
(by fetch), we don't care what particular frame we're examining,
but instead are looking in detail at the 'Body' of that frame, e.g.
      contains((has: fleas, eats: meat, age: 34),  age:34)
We therefore have to do 2 things...
    a) 'traverse' the body, CDR'ing down the line until we're at
the right slot,
    b) see whether we can reconcile the goods we actually found with the
slot:filler combination we set out to find.
The magic is that when we have found the relevant slot, we ALSO
want to convert it to what we call "NormalFacetForm" to avoid
the problem arising from three different ways of specifying
frames (e.g. simple, compound, complex).  Therefore, when we find
the relevant slot we invoke a workhorse called 'compose' which
takes in a particular term and transforms it into our so-called
NormalFacetForm.
Thus there are only two calls to invoke:
*/
contains(Body, Slot:Filler) :- /* Body & Slot normally input, Filler output */
    traverse_body(Body,Slot,NormalFacetForm),
    reconcilable(NormalFacetForm,Filler).

/* --------- t r a v e r s e - b o d y -------------------------
This 'CDRs' down the compound body looking for a match of slot names,
and (more importantly) invokes compose/2, which
takes in a particular term and transforms it into our so-called
NormalFacetForm, so that we dont have to sweat about which way
the user happened to specify the contents of a
frame (e.g. simple, compound, complex).
*/

/* case 1a: just a single slot, and it is the one we want */
traverse_body(Object,Slot:Term,Slot,NormalFacetForm) :-
    !,
    compose(Object,Term, NormalFacetForm).
traverse_body(Object,Slot:Term1,Slot1,NormalFacetForm) :-
    if_needed(Object,Object,Slot1,Term),
    compose(Object,Term,NormalFacetForm), !.
/* case 2: a conjunction of slots, and first one is the one we want */
traverse_body(Object,(Slot:Term, Rest), Slot, NormalFacetForm) :-
    compose(Object,Term, NormalFacetForm),
    !.
/* case 3: a conjunction of slots, so we CDR on down the line */
traverse_body(Object,(_, Rest), Slot, NormalFacetForm) :-
    traverse_body(Object,Rest, Slot, NormalFacetForm).

/* -------------- c o m p o s e ------------------------------
This is where we suffer for letting the user specify frames
in any of three different ways.  compose/2 takes its first frame's argument
(a typical filler) as input, and converts into a 'normalised' form.
For consistency, unitary values are always converted to a 1-element
list, so that the value facet is ALWAYS a list.
Here are some input/ouput examples, wherein IN is always the
first argument to compose, and OUT is always the second
argument:

IN: meat
OUT: slot(value([meat]), inheritance(supersede), cardinality(any))

IN: [meat, bread]
OUT: slot(value([meat, bread]), inheritance(supersede),
cardinality(any))

IN: [value: cheese, inheritance: merge, cardinality: 4]
OUT: slot(value([cheese]), inheritance(merge), cardinality(4))

Now, for the code...

case 1: 'simple'
this is an atomic filler, so just whack it into the value facet
*/

compose(Object,X, slot(value([X]), inheritance(supersede),
                       cardinality(any))) :-
    atomic(X),
    !.

/* case 2: 'compound'
this is a list of atomic fillers, e.g. [meat, potatoes], so do the
same */
compose(Object,[A|B], slot(value([A|B]),
                           inheritance(supersede),
                           cardinality(any))) :-
    atomic(A),
    !.

/* case 3: 'complex'
to get this far, the fillers must be in the complex form
of [<facetX.Y>:<filler>, etc.], so we work through the list separately
for each of our important facets, using a workhorse utility called
force_membership which either finds the relevant item, or shoves
the default value (e.g. 'supersede' for inheritance) in the right place
*/
compose(Object,FacetFillerList, slot(value(V), inheritance(I),
                                     cardinality(C))) :-
    force_membership(Object,value : V, FacetFillerList),
    force_membership(Object,inheritance : I, FacetFillerList),
    force_membership(Object,cardinality : C, FacetFillerList).

/* --------- f o r c e - m e m b e r s h i p ------------------
arg1 is OUTPUT, arg2 INPUT
*/
/* first check the value --- if the value is absent or unknown
   then look to see if there is an if_needed (access rule) demon */
force_membership(Object,value: V,List):-
    \+ 'pd624 member'(value:V,List),
    if_needed1(Object,value:V,List).
force_membership(Object,value: V,List):-
    ( 'pd624 member'(value: unknown,List); 'pd624 member'(value : [], List)),
    if_needed1(Object,value:V,List).
/* compund (set of) fillers? then put them all in the facet */
force_membership(Object,Facet : [Filler|Fillers], List) :-
    'pd624 member'(Facet : [Filler|Fillers], List),
    !.
/* unitary filler? then put it into a one-element list [Filler] */
force_membership(Object,Facet : [Filler], List) :-
    'pd624 member'(Facet : Filler, List),
    !.

/* to get here, we must not have found anything, so we impose
('force') a default filler on the relevant facet, according to the
name of the facet.  e.g. if it is 'inheritance', we force 'supersede',
if it is 'type', we force 'any', if it is 'value', we force '[]' */

force_membership(Object,inheritance: [supersede], List) :- !.
force_membership(Object,type: any, List) :- !.
force_membership(Object,cardinality: any, List) :- !.
/* all other facets, such as 'value', default to an empty list */
force_membership(Object,Facet : [], List).

/* ---------------- r e c o n c i l a b l e ----------------------
having found our actual frame contents, now we really need to know
whether the given Filler is consistent with the current value
(remember that since we have converted to NormalFacetForm,
the variable V below will always be a list.
For now, we just use a membership test, but fancier options are
possible, such as checking for consistency, looking for counter-
examples, etc.
Notice 4th argument (output) is used as a flag to pass back the
type of inheritance mechanism */

reconcilable(slot(value(V), inheritance(I), cardinality(C)),
                    Filler  ) :-
    'pd624 member'(Filler,V).


/* ----------------- Type and Cardinality checking --------------------
  (a) cardinality must be defined by number, or a range in the form
      LowerBound-HigherBound.  The arguments ARE ORDER SENSITIVE!
  (b) type checking is only done for a particular slot in
      which the value is located. */

type_check(Body,Object,Attribute,Value):-
   'pd624 member'(type : T,Body),!,
    type_consistency(T,Object,Attribute,Value).
type_check(_,Object,Attribute,Value):-
    isa_tc_check(Object,Object,type,Attribute,Value).
type_check(_,_,_,_).
cardinality_check(Body,Object,Attribute,Value):-
   'pd624 member'(cardinality : C,Body),  !,
   cardinality_consistency(C,Object,Attribute,Value).
cardinality_check(_,Object,Attribute,Value):-
   isa_tc_check(Object,Object,cardinality,Attribute,Value).
cardinality_check(_,_,_,_).

/* recursive checking up isa hierarchy */
isa_tc_check(Thing,Original,Flag,Attribute,Value):-
   isa_linked(Thing,Parent),
   (Parent subclass_of _ with Body),
   'pd624 cmember'(Attribute:List,Body),
   'pd624 member'(Flag:Check,List),
   choose_test(Flag,Check,Parent,Attribute,Value),!.
isa_tc_check(Thing,Original,Flag,Attribute,Value):-
   isa_linked(Thing,Parent),
   (Parent instance_of _ with Body),
   'pd624 cmember'(Attribute:List,Body),
   'pd624 member'(Flag:Check,List),
   choose_test(Flag,Check,Parent,Attribute,Value),!.

isa_tc_check(Thing,Original,Flag,Attribute,Value):-
   isa_linked(Thing,Parent),
   isa_tc_check(Parent,Original,Flag,Attribute,Value).
isa_tc_check(_,_,_,_,_). /* so it always wins */

choose_test(type,Check,Object,Attribute,Value):-
   type_consistency(Check,Object,Attribute,Value).
choose_test(cardinality,Check,Object,Attribute,Value):-
   cardinality_consistency(Check,Object,Attribute,Value).

/* any is the default case so for efficiency let's check for it first */
type_consistency(any,_,_,_):- !.
type_consistency(integer,_,_,V):-
    integer(V),!.
type_consistency(nonvar,_,_,V):-
    nonvar(V),!.
type_consistency(atom,_,_,V):-
    atomic(V),!.
type_consistency(list,_,_,[H|T]) :- !.
type_consistency(list,_,_,[]) :- !.
type_consistency(Alternatives,_,_,V):-
   'pd624 member'(V,Alternatives),!.
type_consistency(A,_,_,V):-
    isa_linked(V,A), !.
type_consistency(A,_,_,V):-
    isa_linked(V,Something),
    type_consistency(A,_,_,Something), !.
type_consistency(T,Object,Attribute,Value):-
    'pd624 write'(['Warning: "',Value,
    '" violates the "type" facet of "',Object,'" for slot "',Attribute,
    '" ',
    nl,'which specifies type : ',T,'. ',
    nl,'(but proceeding anyway)',nl]),!.

/* the default cardinality is 'any'.  As this will probably occur more
   often than any other case, for efficiency check for it first */
cardinality_consistency(any,_,_,_).
cardinality_consistency(1,Object,Attribute,Value):- /* cardinality one,
   then check to see if the slot filler is atomic */
   atomic(Value).
cardinality_consistency(A-B,Object,Attribute,Value):-
    'pd624 list length'(Value,Length),
    Length >= A,
    Length =< B.
cardinality_consistency(Num,Object,Attribute,Value):-
   'pd624 list length'(Value,Num). /* this will also cater for list of
      length one should they exist for some perverse reason */
cardinality_consistency(Number,Object,Attribute,Value):-
   'pd624 write'(['Warning: "',Value,'" violates the "cardinality" facet of "',
    Object,'" for slot "',Attribute,'"',nl,
    'which specifies cardinality : ',Number,'. ',nl,
    '(but proceeding anyway)',nl]).


/* =================== (3) D E M O N   P R O C E S S I N G ============ */
/* if_added demons are 'change_rules' in the text
   if_needed are called 'access_rules' */

if_added_demon(Body,Object,A,V,Parent):-
    'pd624 member'(change_rule : What_to_do,Body),
    process_if_added(What_to_do,Object,A).

if_added_demon(Body,Object,Attr,Val,Parent):-
    find_the_superior_body(Parent,New_body),
    'pd624 cmember'(Attr:ABody,New_body),
    'pd624 member'(change_rule : Method,ABody),!,
    unify_value(Val,ABody),
    process_if_added(Method,Object,Attr).
if_added_demon(Body,Object,Attr,Val,Parent):-
    isa_linked(Parent,Super_parent),
 if_added_demon(Body,Object,Attr,Val,Super_parent).
if_added_demon(B,O,A,V,P).

unify_value(Value,Body):-
    'pd624 member'(value:Value,Body).
unify_value(A,B). /* when the two will not unify */

find_the_superior_body(Object,Body):-
    (Object instance_of _ with Body).
find_the_superior_body(Object,Body):-
 (Object subclass_of _whoever with Body).

'pd624 cmember'(A,(A,_)).
'pd624 cmember'(A,(_,Rest)):-
     'pd624 cmember'(A,Rest).
'pd624 cmember'(A,A).

process_if_added(Method,Object,Attr):-
    process_method(Method,Object,Attr).
process_if_added(Method,Object,Attr):-
    write('WARNING: The following method failed '),write(Method),nl,
    write('from the object frame '),write(Object),nl.


if_in_wm(A or B,Obj):-
    if_in_wm(A,Obj), !.
if_in_wm(_ or B,Obj):-
    if_in_wm(B,Obj), !.
if_in_wm(Pattern1 & Rest,Obj) :-
    !,
    if_in_wm(Pattern1,Obj),
    if_in_wm(Rest,Obj).
if_in_wm(the Attr of ?self is What,Object):-
 in_wm(the Attr of Object is What).
if_in_wm(true,_).
if_in_wm(Pattern,_) :- /*singleton*/
    in_wm(Pattern).

process_method((if Ifs then Thens),Object,Attr):-
   'pd624 replace'(?self,Object,Ifs,NewIfs),
   if_in_wm(NewIfs,Object),
   parse_first(Thens,Object,Attr).

parse_first(A & B,Object,Attribute):-
   parse_first(A,Object,Attribute),!,
   parse_first(B,Object,Attribute).

/* PATCH 7th. September 1990.
the following makes the consequences of if added demons consistent
with that of forward chaining rules.  Instead of the previous limitation
regarding the use of ?self which was limited to the pattern
the X of ?self is Y, ?self can now be used arbitrarily in a pattern
e.g. [likes, ?self, beer] or loves(?self,mary).  */
/* PATCH 7th December 1990.
bug introduced at v1.40 fixed, make_value was being passed on to
perform1 instead of getting the specific demon treatment.  Behaviour
modified below to be in line with the manual -- Mike */
parse_first(make_value Term,Object,Attr):-
   (Object instance_of Class with Body),
   !,
   'pd624 replace'(?self,Object,Term,NTerm),
   subst(Object,(Attr:Old),(Attr:NTerm),Body,NBody),
   retract((Object instance_of Class with _)),
   assert((Object instance_of Class with NBody)).
parse_first(make_value Term,Object,Attr):-
   (Object subclass_of Class with Body),
   !,
   'pd624 replace'(?self,Object,Term,NTerm),
   subst(Object,(Attr:Old),(Attr:NTerm),Body,NBody),
   retract((Object subclass_of Class with _)),
   assert((Object subclass_of Class with NBody)).


parse_first(Term,Object,A):-
   'pd624 replace'(?self,Object,Term,NTerm),
   perform1(NTerm,_,_,_).

/* ---------------- COMPATIBILITY NOTE -------------------------
In the next two predicate definitions, we test for
   real(Term)
If your dialect of Prolog does not cater for real numbers,
you can comment out those tests
---------------------------------------------------------------- */

'pd624 replace'(O,N,O1,O1):- var(O1),!.
'pd624 replace'(O,N,O,N).
'pd624 replace'(Old,New,Term,Term) :-
  (/* integer(Term) ; real(Term) */ number(Term) ; atom(Term)),
  \+(Term=Old).
'pd624 replace'(Old,New,Term,Term1) :-
  'pd624 compound'(Term),
  functor(Term,F,N),
  functor(Term1,F,N),
  'pd624 replace'(N,Old,New,Term,Term1).
'pd624 replace'(N,Old,New,Term,Term1) :-
  N>0,
  arg(N,Term,Arg),
  'pd624 replace'(Old,New,Arg,Arg1),
  arg(N,Term1,Arg1),
  N1 is N-1,
  'pd624 replace'(N1,Old,New,Term,Term1).
'pd624 replace'(0,Old,New,Term,Term1).

'pd624 compound'(X) :-
   \+(atom(X)),
   /* \+(real(X)),*/     /* see above compatibility notice */
   /* \+(integer(X)). */
   \+(number(X)).

decide_what_to_store(the Attribute of Object is Value):-
    store(Object,Attribute,Value).
decide_what_to_store(_).  /* to cater for unstructured facts */

/* the output is always a list */
find_value((if Ifs then Thens),Object,Attribute,Value):-
  demon_prove(Ifs,Object,Attribute),
  demon_rhs(Thens,Object,Attribute,Value).

demon_prove(X or Y,Object,Attribute):-
  demon_prove(X,Object,Attribute);
  demon_prove(Y,Object,Attribute),!.
demon_prove(X & Y,Object,Attribute):-
  demon_prove(X,Object,Attribute),
  demon_prove(Y,Object,Attribute).
demon_prove(true,_,_). /* for lhs of the form if true then Then */
demon_prove(the Attribute of Object is What,Object,Attribute):-
  'pd624 write'(['Error: the ',Attribute,' of ',Object,
   ' is being re-invoked in the very demon that is trying to find',nl,
   'its value.  This request will cause the method to fail',nl]).
demon_prove(the Attribute of ?self is What,Object,Attribute):-
  'pd624 write'(['Error: the ',Attribute,' of ',Object,
   ' is being re-invoked in the very demon that is trying to find',nl,
   'its value.  This request will be cause the method to fail',nl]).

demon_prove(the A of ?self is What,Object,_):-
   prove(the A of Object is What).
demon_prove(X,_,_) :- prove(X).

demon_rhs(the A of B is C,_,_,C):- /* PATCH 7/9/90 */
   !.  /* i.e. do nothing, because C is simply returned un-cached */
demon_rhs(make_value X,Object,Attribute,X):-
   !,
   note the Attribute of Object is X.

demon_rhs(A,B,C,_):-
'pd624 write'(['Warning: the access_rule method for ',B,' of  ',C,
  'contains a right-hand-side which does not supply a value.',nl,
  'Right-hand-sides that do must be of the form the A of B is C',
  'or they must contain the key-word "make_value" followed by a value.',
  nl]),!.

list_check([P|P1],[P|P1]).
list_check(P,[P]).

/* this only gets called when we know we have a demon in the current
   facet filler, in which case we don't go chasing up the isa chain */
if_needed1(Object,value:Value,Body):-
    'pd624 member'((access_rule : What_to_do),Body),
    find_value(What_to_do,Object,' this is a dummy value to pass ',Value).

/* the standard case.  You don't find a value so you try going up the
   isa chain in order to find an appropriate demon at some stage in
   your ancestry */
if_needed(BaseObject,Object,Slot,Term):-
       (Object instance_of Super with _),
    find_the_superior_body(Super,NewBody),
    if_needed_process(BaseObject,Slot,Term,Super,NewBody).
if_needed(BaseObject,Object,Slot,Term):-
    (Object subclass_of Super with _),
    find_the_superior_body(Super,NewBody),
    if_needed_process(BaseObject,Slot,Term,Super,NewBody).
if_needed(BaseObject,Object,Slot,Term):-
    (Object subclass_of Super with _),
    if_needed(BaseObject,Super,Slot,Term).
if_needed(BaseObject,Object,Slot,Term):-
    (Object instance_of Super with _),
    if_needed(BaseObject,Super,Slot,Term).

if_needed_process(BaseObject,Attr,Val,Super,Body):-
    'pd624 cmember'(Attr:ABody,Body),
    'pd624 member'(access_rule:Method,ABody),
    !,
    (find_value(Method,BaseObject,Attr,Val);
     write('WARNING: The following access_rule demon failed '),nl,
    write(Method),nl,write('In the frame '),write(Super),nl,!,fail).

/* more forward chaining code is in file ENGINE2.PL */
/* file: ENGINE2.PL {2nd part of main code for MIKE rule/frame engine} */
/*
                            *************
                             M I K E - 2
                            *************
               Micro Interpreter for Knowledge Engineering
                  {written in Edinburgh-syntax Prolog}

MIKE: Copyright (c) 1989, 1990, 1991 The Open University (U.K.)

MIKE is intended for educational purposes, and may not
be sold as or incorporated in a commercial product without
written permission from: The Copyrights Officer, Open University,
Milton Keynes MK7 6AA, U.K.

The Open University accepts no responsibility for any legal or other
consequences which may arise directly or indirectly as a result of the
use of all or parts of the contents of this program.
*/
/* ENGINE1.PL & ENGINE2.PL contain the essential innards of MIKE.
   Some auxilliary code is contained in the files UTIL.PL and IO.PL,
   and the kernel of the forward chaining executive loop is in FC_EXEC.PL
   ENGINE1.PL & ENGINE2.PL are subdivided into six main parts, as follows:
   (N.B. the first three parts are in file ENGINE1.PL)
   1.  Backward chaining
   2.  Frame manipulation
   3.  Demon processing
   (N.B. the last three parts are in file ENGINE2.PL)
   4.  Top level
   5.  Forward chaining (left hand side conditions)
   6.  Forward chaining (right hand side actions)
*/

/* ===================== (4) T O P   L E V E L ========================== */
A & B :-
   and(A & B).

(X instance_of Y):-
   (X instance_of Y with _).
(X subclass_of Y):-
   (X subclass_of Y with _).

and(initialise):- initialise.
and(go):- !, go.
and(X & Y) :- and(X),and(Y).
and(X):- perform1(X,New,'top level','You told me so'),
    retract('pd624 wme'(Whatever)),
    assert('pd624 wme'([New|Whatever])).

/* The first few forward chaining predicates are required to stop the forward
   chainer being invoked while the TMS is on, to avoid tricky situations */

nocando:-
    write('The forward chainer may not be invoked while the TMS is enabled.'),
    nl.

/* */

fc:-
   tms(on),
   !,
   nocando.

fc(_):-
   tms(on),
   !,
   nocando.

fc:-
    initialise,
    add start,
    go.

/* It would be faster to use 'continue' instead of 'go' in the line above,
because 'go' now invokes part_initialise, which is actually redundant in
this precise context.  However, the above definition is published in the
course text, so we stick with it.
*/

fc(X):-
  initialise,
  add X,
  go.
/* See preceding comment about using 'continue' instead of 'go' */

add X :- /* fc triggers off the forward chainer */
    perform1(add X,_,'top level','You told me so'),
    !.

remove X:-
   tms(on),
   just_check(X,'top level'),
   justification(X,'top level','You told me so'),
   'pd624 retract'(justification(X,'top level','You told me so')),
   ((check_justification_links(X),  /*If there is a well-founded justification*/
   !,                               /*inform user, otherwise... */
   'pd624 write'(['The premise ',X,' has been removed, but ',X,' still exists',
   nl,
   'in working memory as it has been concluded from elsewhere (type',nl,
   'how ',X,' for details).']));
    perform1(remove X,_,_,_)). /*remove wm element*/

remove X:-
   tms(on),
   just_check(X,'top level'),
   !,
   'pd624 write'(['Sorry : ',X,' is a conclusion, only',nl,
   'base level premises can be removed from the top level',nl]),
   'pd624 retract'(justification(X,'top level','You told me so')).

remove X :-
   tms(on),
   justification(X,'top level',_),
   perform1(remove X,_,'top level','You told me so'),
   !.

remove X :-   /* old definition of remove, for MIKE <2.0 compatibility */
   tms(off),
   retract(currentdb(X,Truth)),
   !,
   'pd624 retract'(justification(X,_,_)). /* justification update added*/

remove X cf Y:-  /*Just in case user tries to remove cf with wm!*/
   remove X.

remove X :-
   'pd624 write'(['Sorry : ',X,' is not in working memory',nl,
   'and thus cannot be removed',nl]).

note X:-
    perform1(note X,New,'top level','You told me so'),
    (retract('pd624 wme'(Whatever));Whatever = []),
    assert('pd624 wme'([New|Whatever])),
    addrete(X),
    !.

deduce X :-
  prove(X).

initialise:-
 fc_reset_history,      /* reset history counters (see fc_exec.pl) */
    reset_rete,     /* resets RETE memory for forward chaining */
    abolish(currentdb,2),   /* the relation 'currentdb/2' stores all WM items */
    kill(currentdb),  /* just for portability */
    abolish(already_did,2), /* used for quick refractoriness test */
    kill(already_did),
    assert(already_did(nil,nil)),
                           /* need some assertion to avoid run-time complaint*/
abolish('pd624 wme',1),    /* otherwise we end up in a curious state?!!! */
    kill('pd624 wme'),
    abolish('pd624 symbol root', 2),
    kill('pd624 symbol rot'),
    abolish(receives_answer,2),
    abolish(justification,3),
    assert('pd624 wme'([])),
 (retract(pd624_flag(_)) ; true), /* Used for single-step trace. See UTIL.PL */
 initialise_back_door,  /* in case of later extensions ! */
 !.

/*
   part_initialise is like initialise, but leaves WM alone, and
   also leaves justifications arising from top level use of ?- add ...
*/
part_initialise :-
 fc_reset_history,      /* reset history counters (see fc_exec.pl) */
    abolish(already_did,2), /* used for quick refractoriness test */
    kill(already_did),
    assert(already_did(nil,nil)),
                         /* need some assertion to avoid run-time complaint */
abolish('pd624 wme',1),    /* otherwise we end up in a curious state?!!! */
    kill('pd624 wme'),
    abolish(receives_answer,2),
 obliterate_all_non_top_level_justifications,
    assert('pd624 wme'([])),
 (retract(pd624_flag(_)) ; true), /* Used for single-step trace. See UTIL.PL */
 initialise_back_door,  /* in case of later extensions ! */
 !.

obliterate_all_non_top_level_justifications :-
  justification(A, B, C),
  \+( (B='top level', C='You told me so') ),
  retract(justification(A,B,C)),
  fail.

obliterate_all_non_top_level_justifications.




initialise_back_door :-
  allowable_back_door_initialise(X),   /* back door utility defined? */
  do_just_once(call(X)),               /* then invoke it once */
  fail.                                /* backtrack for others */

initialise_back_door.                  /* default success */

allow_back_door_initialise(Pred) :-           /* to be used as a directive */
   allowable_back_door_initialise(Pred);      /* already there? do nothing */
   assertz(allowable_back_door_initialise(Pred)). /* else add flag */


announce P :-
         'pd624 write'(P),!.    /* simple output of list of items */

/* PATCH 19-SEP-90: We now distinguish between 'continue' and 'go'.
   The former really leaves ALL internal state information alone
   (e.g. what rules have recently fired), and carries on forward
   chaining, if possible.  The latter ('go') leaves working memory
   alone, as promised, but clears up various internal flags, so
   that a brand new run of forward chaining can be invoked with the
   current working memory (this is what most users expect anyway) */



continue:-
   tms(on),
   !,
   nocando.
continue :- 'pd624 wme'(A),!,forward_chain.
continue :- assert('pd624 wme'([])),forward_chain.

go:-
   tms(on),
   !,
   nocando.

go :-
  part_initialise, /* get rid of hidden flags like 'already_did'...*/
  forward_chain.

the X of Y is Z:-
    prove(the X of Y is Z).
the X of Y > Z:-
    prove(the X of Y > Z).
the X of Y < Z:-
    prove(the X of Y < Z).
the X of Y >= Z:-
    prove(the X of Y >= Z).
the X of Y =< Z:-
    prove(the X of Y =< Z).

all X of Y are Z:-
    prove(all X of Y are Z).

cf:-
    'pd624 write'(['The current contents of working memory are the following:',
    nl,nl]),
    assert('wm counter'(0)),
    currentdb(X,CF),
    do_just_once((tab(5),write(X),write(' cf '),write(CF),nl,
                  retract('wm counter'(P)),
                  New is P + 1,
                  assert('wm counter'(New)) )),
    fail.

cf:-
    retract('wm counter'(Number)),
    'pd624 write'([nl,'A total of ',Number,
     ' current working memory elements were found.',nl]).
wm:-
    'pd624 write'([
        'The current contents of working memory are the following: ',nl]),
    assert('wm counter'(0)),
    currentdb(X,Y),
    do_just_once((tab(5),write_db(X,Y),nl,
                  retract('wm counter'(P)),
                  New is P + 1,
                  assert('wm counter'(New)) )),
    fail.
wm:-
    retract('wm counter'(Number)),
    'pd624 write'([nl,'A total of ',Number,
     ' current working memory elements were found.',nl]).

write_db(X,false):-
    write(X),write(' is known to be false'),!.
write_db(X,_):-
    write(X).

/* this defines the de facto conflict resolution strategy, namely
   refractoriness
   recency
   specificity
   lhsthreshold
   cfstrength

       - - - applied in that order */
current_conflict_resolution_strategy(
    [refractoriness,recency,specificity,lhsthreshold,cfstrength]).

lhsthreshold(0.2).  /* set default threshold for LHS certainty factor */

/* ==================== (5) F O R W A R D  C H A I N I N G =========== */
/* ====================    Left-hand-side conditions       =========== */

/* N.B. The forward chaining executive loop is stored separately in the file
FC_EXEC.PL. It has been separated in order to keep this file (ENGINE.PL) a
manageable size.   */

/* ----- all_in_wm (sees whether all of its args are present in WM) ---- */
all_in_wm(A or B):-
    all_in_wm(A), !.
all_in_wm(_ or B):-
    all_in_wm(B), !.
all_in_wm(Pattern1 & Rest) :-
    !,
 when_enabled('show individual LHS in' for Pattern1),
    in_wm(Pattern1),
 when_enabled('show individual LHS out' for Pattern1),
    all_in_wm(Rest).

all_in_wm(Pattern) :- /*singleton*/
 when_enabled('show individual LHS in' for Pattern),
    in_wm(Pattern),
 when_enabled('show individual LHS out' for Pattern).

/* ------------------------ Conflict resolution ------------------------- */

resolve_conflicts(List,Item,_,[]):-
                              /* when you've exhausted conflict resolution */
    first_filter(List,Item),!.               /* choose the first */
    /* first filter just takes the first item in the list.  This can
    be achieved more efficiently, but is not for the sake of tracing.
    If tracing is deemed not to be important make the clause head of the
    first clause resolve_conflicts([H|_],Item,_,[]) instead.  A second clause
    resolve_conflicts([],_,_,[]) will also be necessary to cater for an
    empty conflict set */
resolve_conflicts(Set,H,WME,[Strategy|Rest]):-
    DO_It =.. [Strategy,Set,WME,Newset],
    DO_It,
    resolve_conflicts(Newset,H,WME,Rest).

first_filter([],(rule 'didnt find a winner' forward if 'no ifs' then
                     'no thens')):- !.
first_filter([H|_],H).     /* choose the first item */

/* conflict resolution strategies ---- user-modifiable */

/* if you design your own conflict resolution rules they must be of the form
<name>(Input_set,Working_memory_elements,Output_set).

The types of conflict resolution are
refractoriness: a particular rule with a given set of instantiations
  is precluded from firing again
recency: a weighting is done and only those rules whose pre conditions
  corespond most closely to the latest items in working memory are chosen
specificity: the rules whose preconditions are most clearly specified
  (i.e. most left-hand-side conditions) are fired next
lhsthreshold: rules whose left-hand-sides have a combined certainty factor
  below the given threshold are precluded from firing
cfstrength: the higher the certainty factor of the whole rule, the higher
  chance it has of being chosen for firing

*/

lhsthreshold([],_,[]).

lhsthreshold([(rule Rule forward if Conds then Actions)|Rest],_,Output):-
    lhsthreshold(Threshold),
    lhslist(Conds,[],CFs),
    method(Whatever),
    combine(Whatever,CFs,1.0,NewCF),
    compare(<,NewCF,Threshold),
    lhsthreshold(Rest,_,Output).

lhsthreshold([H|Rest],_,[H|Output]):-
    lhsthreshold(Rest,_,Output).

cfstrength([],_,[]).

cfstrength(Input,_,Output):-
    makelist(Input,[],List), /* compile rules + CFs into a list */
    sortcfs(List,Output).    /* and sort them */

makelist([(rule Rule forward if Conds then Actions)|Rest],ListIn,Output):-
    lhslist(Conds,[],CFs),
    method(Whatever),
    combine(Whatever,CFs,1.0,NewCF),
    append(ListIn,
           [ruleandcf(NewCF,(rule Rule forward if Conds then Actions))],
           NewList),
    makelist(Rest,NewList,Output).

makelist([],ListIn,ListIn).

sortcfs(Unsorted,Sorted):-
    sortcfs(Unsorted,Sorted,[]).

sortcfs([],AuxiliaryList,AuxiliaryList).

sortcfs([Element|Rest],Sorted,AuxiliaryList):-
    break_down(Element,Rest,ListA,ListB),
    sortcfs(ListB,ListC,AuxiliaryList),
    Element=ruleandcf(CF,SmartElement),
    sortcfs(ListA,Sorted,[SmartElement|ListC]).

break_down(_,[],[],[]):- !.

break_down(Element,[Head|Rest],[Head|RestA],RestB):-
    more_certain_than(Head,Element),
    !,
    break_down(Element,Rest,RestA,RestB).

break_down(Element,[Head|Rest],RestA,[Head|RestB]):-
    break_down(Element,Rest,RestA,RestB).

more_certain_than(X,Y):-
    X=ruleandcf(IDX,_),
    Y=ruleandcf(IDY,_),
    compare(>,IDX,IDY).

refractoriness([],_,[]).
refractoriness([(rule Rule forward if COND then Actions)|Rest],_,Output):-
    already_did(Rule,COND),!,
    when_enabled('show refractoriness' for Rule),
    refractoriness(Rest,_,Output).
refractoriness([H|Rest],_,[H|Output]):-
    refractoriness(Rest,_,Output).

recency([],_,[]).
recency(Set,Wme,NewSet):-
    rank_candidates(Set,Wme,RankedSet),
    choose_most_likely_set(RankedSet,0,[],NewSet),
    when_enabled('show recency' for NewSet).

rank_candidates([],_,[]).
rank_candidates([(rule Rule forward if Cond then Actions)|Rest],Wme,
                [(Rank,(rule Rule forward if Cond then Actions))|NewRest]):-
    make_rank(Cond,Wme,0,Rank),
    rank_candidates(Rest,Wme,NewRest).

make_rank(H or T,Wme,A,Rank):-
    make_rank(H,Wme,A,T1),
    make_rank(T,Wme,A,T2),
    Rank is T1 + T2.
make_rank(H &T,Wme,A,B):-
    'pd624 member'(H,Wme),
    A1 is A + 1,
    make_rank(T,Wme,A1,B).
make_rank(_ & T,Wme,A,B):-
    make_rank(T,Wme,A,B).
make_rank(A,Wme,B,C):-
    'pd624 member'(A,Wme),
    C is B + 1 .
make_rank(L,_,A,A).

choose_most_likely_set([],_,A,A).
choose_most_likely_set([(A,H)|Tail],Crit,Result,Set):-
    Crit > A,
    choose_most_likely_set(Tail,Crit,Result,Set).
choose_most_likely_set([(A,H)|Tail],Crit,Result,Set):-
    Crit = A,
    choose_most_likely_set(Tail,Crit,[H|Result],Set).
choose_most_likely_set([(A,H)|Tail],Crit,Result,Set):-
    A > Crit,
    choose_most_likely_set(Tail,A,[H],Set).

specificity([],_,[]).   /* when there are no applicable rules */
specificity(Set,Wme,Output):-
    specificity1(Set,Wme,Ranked_set),
    choose_most_likely_set(Ranked_set,0,[],Output),
    when_enabled('show specificity' for Output).

specificity1([],_,[]).
specificity1([(rule Rule forward if Cond then Actions)|Rest],_,
             [(Length,(rule Rule forward if Cond then Actions))|Set]):-
     'pd624 length with disjunct check'(Cond,Length),  /* see UTIL.PL */
     specificity1(Rest,_,Set).

/* if a rule has a disjunction on the LHS and both elements of that disjunction
are true then it will appear multiple times in the conflict set e.g.
  rule eg forward if a(P) or b(P) then c(P) given
  a(1) and b(2)
  will result in both instantiations (i.e. c(1) and c(2)) appearing in the
  conflict set.  HOWEVER if the rule is instead 'a or b then c', this will
  lead to the same rule in the conflict set twice, but via different routes.
  c'est la guerre */

/* ----------------------------- in_wm -------------------------------- */
in_wm(A or B):-
  in_wm(A).
in_wm(A or B):-
  in_wm(B).

in_wm(-- X) :-
    !,
    \+(in_wm(X)).

in_wm(deduce X) :-
    !,
    do_just_once(prove(X)). /* runs backward rules for that pattern! */
/* N.B. change above line to simply
   prove(X)
if you disagree with the large comment below, i.e. if you want
there to be multiple solutions whenever 'deduce' is used on the
left hand side of a rule */
/* Notice that arbitrary backtracking is NOT allowed in consecutive
   calls to deduce which occur on the left hand side of a
   forward-chaining rule!!!!! -- the call to
   do_just_once above prevents this.  Arbitrary backtracking is allowed
   within sequences of backward-chaining rules, however.
   In other words, suppose we had two rules such as the following:

   rule init forward
     if
       start
     then
       remove start &
       add [fred, is, happy] &
       add [mary, is, happy] &
       add [mary, likes, potatoes].

   rule temp forward
     if
       -- start &
       deduce [X, is, happy] &
       deduce [X, likes, potatoes]
     then
       add [X, isa, happy_potato_eater].

Rule temp will never find a happy_potato_eater, because the first call
to deduce will succeed with X = fred, but deduce [fred, likes, potatoes]
will fail, and the first call will not be redone!!  However, either of
the next two temp rules would do the trick (along with the backward
chaining rule 'potato_eater':

    rule temp2 forward
      if
        -- start &
        [X, is, happy] &
        [X, likes, potatoes]
      then
        add [X, isa, happy_potato_eater].

    rule temp3 forward
      if
        -- start &
        deduce [X, isa, happy_potato_eater]
      then
        announce ['Hooray, I have discoverd a happy potato eater: ', X].

    rule potato_eater backward
      if                     (because this is backward chaining..)
        [X, is, happy] &     (arbitrary calls to deduce would be OK, also)
        [X, likes, potatoes] (arbitrary calls to deduce OK, also)
      then
        [X, isa, happy_pototo_eater].
*/

/* execute prolog goal */
in_wm(prolog(X)):-
    !,
    X.

/* If we look for, say, (the father of enrico is X), then we will not really
find it in working memory, but instead invoke fetch/5 to do the
real work inside the frame representation, just as we do in the
case of backward chaining.  Notice that fetch/5 on its own does
pure frame accessing (possibly looking up the class hierarchy),
but does NOT itself invoke the backward chainer */

in_wm(the Slot of Object is Filler) :- /* the basic frame form */
    fetch(Object, Slot, Filler, [Object], _).
in_wm(A receives_answer B):-
    A receives_answer B.
in_wm(all X of Y are What) :-
    pd624_own_findall(Out,fetch(Y,X,Out,[Y],_),What). /* What is order 
sensitive!!! */
in_wm(the A of B > C):-
          do_just_once(prove(the A of B > C)). /* no backtracking!! */
in_wm(the A of B < C):-
          do_just_once(prove(the A of B < C)).
in_wm(the A of B >= C):-
          do_just_once(prove(the A of B >= C)). /* no backtracking!! */
in_wm(the A of B =< C):-
          do_just_once(prove(the A of B =< C)).

/* N.B. change above line to simply
   prove(the A of B < C)
(and similarly for 3 lines above!!!)
if you disagree with the huge comment about a page earlier, i.e. if you want
there to be multiple solutions whenever 'deduce' is used on the
left hand side of a rule */

in_wm(A instance_of B):-
  A instance_of B with _whatever.
in_wm(A subclass_of B):-
  A subclass_of B with _some_body.

in_wm(Pattern) :-
 currentdb(Pattern,Whatever).  /* this is the basic WM assertion form */

/* Back door case, for extensions to MIKE */
in_wm(X):-allowable_prolog_lhs(X), !, call(X).

/* 'Back door' enables us to extend the syntax of MIKE with calls to
    arbitrary lumps of Prolog:
    Note that the following two predicates are intended to be used as
    DIRECTIVES (analogous to ?-op(A,B,C)).  The 'allow...' directive
    makes an assertion of the form 'allowable...' for testing by MIKE.
*/
/* If database assertion is present then ignore, else make assertion */
allow_prolog_lhs(Pattern) :-
   allowable_prolog_lhs(Pattern);
   (assertz(allowable_prolog_lhs(Pattern)),assert(special_case(Pattern))).

allow_prolog_rhs(Pattern,WM) :-
   allowable_prolog_rhs(Pattern,WM);assertz(allowable_prolog_rhs(Pattern,WM)).

allow_prolog_rhs(Pattern) :-
   allowable_prolog_rhs(Pattern,_);assertz(allowable_prolog_rhs(Pattern,[])).


/* ================= (6) F O R W A R D  C H A I N I N G =============== */
/* =================       Right-hand-side actions      =============== */

perform(Action1 & Rest,List,Rule,Conds) :-
    !,
    do_just_once(perform1(Action1,A,Rule,Conds)), /* PATCH 11/1/90 */
    perform(Rest,R,Rule,Conds),
    append(A,R,List).

perform(Action,A,Rule,Conds) :-  /* singleton case */
    perform1(Action,A,Rule,Conds).

/* lhslist/3 creates a list of the CFs of all the lhs conditions */
lhslist(Var,_,[]):-
    var(Var),
    'pd624 write'(['WARNING: lhslist is called with upbound arguments',
    nl,'this is an error but will be ignored']),!.
lhslist(H&T,OldCFs,AntecedentCFs):-
    !,
    ((currentdb(H,CF),
    append(OldCFs,[CF],NewCFs),
    lhslist(T,NewCFs,AntecedentCFs));
    lhslist(T,OldCFs,AntecedentCFs)).

lhslist(H,OldCFs,AntecedentCFs):-
    currentdb(H,CF),
    append(OldCFs,[CF],AntecedentCFs).

lhslist(H,OldCFs,OldCFs).

perform1(prolog(Action),[],Rule,Conds) :-
    !,
   call(Action).

perform1(remove Pattern,[],Rule,Conds) :-
    !,
    retract(currentdb(Pattern,Whatever)),
    'pd624 retract'(justification(Pattern,_,_)), /*PATCH 30NOV90*/
    tms_trace(Pattern,Rule,'removed from'),
    delrete(Pattern).

perform1(strategy List,[],Rule,Conds):-
    strategy List.

/* Note the second argument to perform1 in the next three cases, which is
   the output of the new working memory elements.
   This is redundant storage because the user could have later referenced the
   answer to this question in one of two ways, either in the standard facet
   form, i.e. the A of B is C, or they could have checked the question
   answer specifically, in the form the A of B receives_answer C.  Since all
   New Working Memory is used for is summation, redundancy will not effect
   the final outcome.  For this reason, both forms can be added back to
   the conflict resolution component with safety. */
perform1((query the A of B receives_answer C),
         [the A of B receives_answer C,the A of B is C],Rule,Conds):-
 answer_vetting(C),
 (query the A of B receives_answer C),
 assert(justification((the A of B is C),Rule,'You told me so')).
perform1((query the A of B is C receives_answer yes),
         [the A of B is C receives_answer yes, the A of B is C],Rule,Conds):-
 answer_vetting(C),
 (query the A of B is C receives_answer yes),
 assert(justification((the A of B is C),Rule,'You told me so')).
perform1((query Quest receives_answer Ans),
         [Quest receives_answer Ans],Rule,Conds):-
 answer_vetting(C),
 (query Quest receives_answer Ans),
 assert(justification(Quest,Rule,'You told me so')).
perform1(note (A instance_of B with C),[],Rules,Conds):-
  retract((A instance_of B with Body)),
  'pd624 write'(['Warning : overwriting previous definition of ',A,
    nl,' instance of ',B,' with body ',Body,nl,' with the new body ',
    C,'. ',nl]),
  assert((A instance_of B with C)),
  assert(justification((A instance_of B with C),Rules,Conds)),!.
perform1(note (A instance_of B with C),[A instance_of B],Rule,Conds):-
 assert((A instance_of B with C)),
 assert(justification((A instance_of B with C),Rule,Conds)),
 !. /* cut needed to stop overinstantiation
 in the following clauses in cases of failure */
perform1(note (A subclass_of B with C),[],Rules,Conds):-
  retract((A subclass_of B with Body)),
  'pd624 write'(['Warning : overwriting previous definition of ',A,
    nl,' subclass of ',B,' with body ',Body,nl,' with the new body ',
    C,'. ',nl]),
  assert((A subclass_of B with C)),
  assert(justification((A subclass_of B with C),Rules,Conds)).
perform1(note (A subclass_of B with C),[A subclass_of B],Rule,Conds):-
 assert((A subclass_of B with C)), !. /* cut needed to stop overinstantiation
                       in the following clauses in cases of failure */

perform1(note the A of O is V,[the A of O is V],Rule,Conds):-
    store(O,A,V),
    assert(justification((the A of O is V),Rule,Conds)).

perform1( (note X), _,Context,_) :-  /* PATCH NEW ERROR MSG 20-SEP-90 */
  \+( X = ( the _ of _ is _ ) ),         /* IF PATTERN IS NOT THIS ONE */
  \+( X = ( _ subclass_of _ with _ ) ),  /* NOR THIS ONE... */
  \+( X = ( _ instance_of _ with _ ) ),  /* NOR THIS ONE... */
  nl,                                     /* THEN IT IS A MISTAKE! */
  write('ERROR... you have attempted the following'),
  'pd624 tell me context'(Context),write(':'),nl,
  write('     note '),write(X),nl,
  write('HOWEVER, note can only be used with one of these 3 formats:'),nl,
  write('  a) note the X of Y is Z.'),nl,
  write(
'  b) note (Obj1 instance_of Obj2 with Slot1:Filler1, Slot2:Filler2, ...).'),
  nl,
  write(
'  c) note (Obj1 subclass_of Obj2 with Slot1:Filler1, Slot2:Filler2, ...).'),
  nl,
  write('(Most frames can be developed/saved in a file using a text editor.)'),
  nl,
  !,
  fail.

perform1(add the A of O is V,[],Rule,Conds):- /* PATCH NEW ERROR MSG 11/1/90 */
  write('ERROR: add can only be used for working memory patterns (use note)'),
  nl.

perform1(add Pattern cf CF, [Pattern cf CF],'top level',Conds) :-
    !,
    update_wm(Pattern,CF),
    (justification(Pattern,'top level',Conds);
    (assert(justification(Pattern,'top level',Conds)),
    tms_trace(Pattern,'top level','added to'))),
    addrete(Pattern).

perform1(add Pattern, [Pattern],'top level',Conds) :-
                         /* identical to next case with keyword */
    !,
    update_wm(Pattern,1.0),    /*If cf not specified, make it 1.0 */
    (justification(Pattern,'top level',Conds);
    (assert(justification(Pattern,'top level',Conds)),
    tms_trace(Pattern,'top level','added to'))),
    addrete(Pattern).

perform1(add Pattern cf CF, [Pattern cf NewCF],Rule,Conds):-
                                                       /* PATCH: 8-11-90 */
    !,
    lhslist(Conds,[],AntecedentsCFs),
    method(Whatever),
    combine(Whatever,AntecedentsCFs,CF,NewCF),
    update_wm(Pattern,NewCF),
    (justification(Pattern,Rule,Conds);
    (assert(justification(Pattern,Rule,Conds)),
    tms_trace(Pattern,Rule,'added to'))),
    addrete(Pattern).

perform1(add Pattern, [Pattern],Rule,Conds) :-
                           /* identical to next case with keyword */
    !,
    lhslist(Conds,[],AntecedentsCFs),
    method(Whatever),
    combine(Whatever,AntecedentsCFs,1.0,NewCF),
                           /*If cf not specified, make it 1.0 */
    update_wm(Pattern,NewCF),
    (justification(Pattern,Rule,Conds);
    (assert(justification(Pattern,Rule,Conds)),
    tms_trace(Pattern,Rule,'added to'))),
    addrete(Pattern).

perform1(announce Pattern,[],Rule,Conds):-
    'pd624 write'(Pattern),nl.

perform1(the X of Y is Z,[],R,C) :- /* PATCH 14/6/90 */
    'pd624 write'([
    'ERROR: the ',X,' of ',Y,' is ',Z,nl,
    'appeared on the right hand side of a rule.  Use note if you want to',
    nl, 'change a frame, or prolog(the',X,' of ',Y,' is <VAR>)',
    nl, 'to retrieve a slot filler in this context.',nl]),
    !.

/* 'Back door' case... see code for allow_prolog_rhs above */
perform1(Action,WM,Rule,Conds) :-
 allowable_prolog_rhs(Action,WM),
 !,
 call(Action).

perform1(Pattern cf CF,[Pattern cf CF],Rule,C):-  /* default is add to wm */
    !,
    lhslist(C,[],AntecedentsCFs),
    method(Whatever),
    combine(Whatever,AntecedentsCFs,CF,NewCF),
    update_wm(Pattern,NewCF),
    (justification(Pattern,Rule,C);
    (assert(justification(Pattern,Rule,C)),
    tms_trace(Pattern,Rule,'added to'))),
    addrete(Pattern).

perform1(Pattern,[Pattern],Rule,C):-  /* default is add to wm */
    !,
    lhslist(C,[],AntecedentsCFs),
    method(Whatever),
    combine(Whatever,AntecedentsCFs,1.0,NewCF),
                           /*if cf not specified, make it 1.0 */
    update_wm(Pattern,NewCF),
    (justification(Pattern,Rule,C);
    (assert(justification(Pattern,Rule,C)),
    tms_trace(Pattern,Rule,'added to'))),
    addrete(Pattern).

perform1(P,[],Rule,C):-
    writel(['ERROR: the following Right-hand side of a rule failed',P]).

update_wm(the Attribute of Object is Value,CF) :- /* frame syntax? */
    store(Object, Attribute, Value).
                /* utility to update frame representation */
update_wm(all Attributes of Object are [Value1 | Values],CF) :-
    store(Object, Attributes, [Value1|Values]). /* must unify with list! */

update_wm(OTHER,CF) :-
    retract(currentdb(OTHER,OldCF)), /* already there? */
    !,
    method(Method),                   /* fetch combination method type */
    combine(Method,[OldCF,CF],NewCF), /* combine old and new CFs */
    assert(currentdb(OTHER,NewCF)).   /* Replace wm element, with new cf */

update_wm(OTHER,CF) :- /* must not have been there before, so add afresh */
    assert(currentdb(OTHER,CF)).

'pd624 tell me context'('top level') :-
   !.

'pd624 tell me context'(Name) :-
   ((rule Name forward if _ then _) ;
    (rule Name backward if _ then _)),
   write(' within rule '),write(Name),
   !.

'pd624 tell me context'(Name) :-
   write(' from '),write(Name).
/* file: FC_EXEC.PL {main forward chainer, RETE, TMS, justification tree} */
/*
                            *************
                             M I K E - 2
                            *************
               Micro Interpreter for Knowledge Engineering
                  {written in Edinburgh-syntax Prolog}

MIKE: Copyright (c) 1989, 1990, 1991 The Open University (U.K.)

MIKE is intended for educational purposes, and may not
be sold as or incorporated in a commercial product without
written permission from: The Copyrights Officer, Open University,
Milton Keynes MK7 6AA, U.K.

The Open University accepts no responsibility for any legal or other
consequences which may arise directly or indirectly as a result of the
use of all or parts of the contents of this program.
*/

/* This file contains most of the new material in MIKE 2.0, the new features
   are:

   (1) An implementation of the RETE algorithm to speed up forward chaining
   (2) A truth maintainance system (TMS)
   (3) A justification tree generator accessed via the 'justify' command
   (4) Provision for wm elements with certainty factors    */

/*===================== FORWARD CHAINING EXECUTIVE ====================== */

/* The main interpreter runs in a loop inside forward_chain. Rules who's
left-hand-side conditions are comprised of working memory and frame
accessing are added to RETE's conflict set as the required wm or frame
data is added by the user. During the forward chaining loop, executed by
typing fc, all rules stored in the RETE conflict set are gathered up using
pd624_own_findall/3. This set of rules is then 'whittled down' using
resolve_conflicts/4.  The unique winner then has its left hand side tested
for special case conditions (i.e. non-wm conditions, such as prolog(X<Y)
and deduce X). If any special conditions are found they are tested, and if
they are all true then the rule's  right-hand-side actions are executed
using perform/4, otherwise the rule is deleted from the forward chainer's
conflict set and a new rule is selected for firing from the ones remaining
in the conflict set. */

/* The essential auxilliary procedures all_in_wm, resolve_conflicts,
and perform are defined in ENGINE2.PL. */

forward_chain:-
        repeat,                      /* cycle until halt is encountered... */
 fc_do_one_cycle(NewWME),     /* workhorse: computes new WM each cycle */
 fc_halt_else_loop(NewWME).   /* bails out only if 'halt' is in WM */

/* cleanup below is invoked by initialise and part_initialise */
fc_reset_history :-
 abolish('pd624 fc_history',3),        /* tidy up stuff for ?- show history. */
 abolish('pd624 current cycle is',1),  /* ditto */
 asserta('pd624 current cycle is'(0)). /* ditto */

fc_do_one_cycle(NewWME) :-
                         /* NewWME is OUTPUT (New Working Memory Elements) */
   fc_update_counters(WME),  /* fetch current WM, update cycle counter */
   findall1(conflict_set(ID,(rule Rule forward if Conds then Actions)),
        (conflict_set(ID,(rule Rule forward if Conds then Actions)),
         fc_check_lhs(Conds),
         when_enabled('show history on request' for [Rule,'+'])),
        ConfSet),               /*Extract conflict set components... */
   quicksortof(ConfSet,SortedSet),   /*sort them into kb appearance order.. */
   fc_pick_one_and_do_it(SortedSet,WME,NewWME), /*...and choose a winner. */
   !.

fc_update_counters(WME) :- /* cycle counter gets globally incremented */
   retract('pd624 wme'(WME)), /* this retrieves latest working memory */
   retract('pd624 current cycle is'(CURRCYCLENUM)),
   NEWCYCLENUM is CURRCYCLENUM + 1,
   asserta('pd624 current cycle is'(NEWCYCLENUM)).

fc_pick_one_and_do_it(ConfSet,WME,NewWME) :-  /* third arg is OUTPUT */
   current_conflict_resolution_strategy(Strategy),
   when_enabled('show conflict set' for ConfSet),
   resolve_conflicts(ConfSet, /* whole set goes in, only next one comes out */
                     (rule Rule forward if Cond then Actions), /* This one */
                     WME,
                     Strategy),
   when_enabled('show conflict winner' for Rule), /* shorthand winner */
   when_enabled('show history on request' for [Rule,'*']),
   when_enabled('show chosen rule'
                 for (rule Rule forward if Cond then Actions)),
     /* PATCH 10 SEP 90  assert --> asserta for possible TMS in future */
   asserta(already_did(Rule,Cond)), /* tell me you just did it */
   fc_do_rhs(WME,NewWME,Rule,Cond,Actions), /* execute RHS actions */
   (true_rule((rule Rule forward if Cond then Actions));
   assert(true_rule((rule Rule forward if Cond then Actions)))),
   when_enabled('show new working memory elements or frame changes'
                 for NewWME).

/*fc_good_potential is retained for use in non-RETE mode */

fc_good_potential(RULE,COND,ACTIONS) :-  /* a rule enters conflict set if... */
   (rule RULE forward if COND then ACTIONS), /* given this form in database...*/
   when_enabled('show single stepping in' for RULE),
   all_in_wm(COND), /* all of its left-hand-side conditions are satisfied */
   when_enabled('show single stepping out' for RULE),
   when_enabled('show history on request' for [RULE,'+']).

fc_do_rhs(WME,NewWME,Rule,Cond,Actions) :- /* case 1: nothing to do */
  Actions = 'no thens',
  NewWME = [halt], /* no applicable rule so halt */
  !.

fc_do_rhs(WME,NewWME,Rule,Cond,Actions) :- /* case 2: normal execution */
  perform(Actions,NewWME,Rule,Cond), /* execute RHS actions */
  !.

fc_do_rhs(WME,NewWME,Rule,Cond,Actions) :- /* case 3:perform/4 failed somehow*/
  write('WARNING: '),write(Rule),
  write(' right-hand-side action failed. '),
  tab11_write(Actions),
  nl,
  NewWME = WME.

fc_halt_else_loop(NewWME) :-       /* halt encountered, so finish off */
   ( 'pd624 member'(halt,NewWME) ; currentdb(halt,true) ) ,
   !. /* end of processing, nothing more to do */

fc_halt_else_loop(NewWME) :- /* no halt, so cause failure-driven loop */
   assert('pd624 wme'(NewWME)),
   fail.      /* fail straight back to repeat */

fc_check_lhs('no ifs').   /*Rule 'didn't find a winner' is always true...*/

fc_check_lhs([]).           /*...as is an empty list??*/

fc_check_lhs(Cond&Rest):- /*A left hand side is true if*/
   special_case(Cond),
   !,                       /*it has special case conditions, */
   in_wm(Cond),             /*all of which are true, or .. */
   fc_check_lhs(Rest).

fc_check_lhs(Cond&Rest):- /* ...if it has no special lhs conditions */
   !,
   fc_check_lhs(Rest).

fc_check_lhs(Cond):-        /* As above, but for singlton*/
   special_case(Cond),
   !,
   in_wm(Cond).

fc_check_lhs(Cond):-        /* As above, but for singleton.*/
   !.

/* ================ Flags For Special Case Left-Hand-Sides ============= */

/*All the patterns below are special (non-wm) lhs conditions,
  these include queries and things that cannot be relied upon
  to be contants. Note that references to access demons are
  also treated as special cases, but they are trapped using a
  different method.

  User defined special case left-hand-sides are set up in MIKE.INI
  using 'allowable prolog' (see elsewhere for details). This means that
  the definitions below need not be edited */

special_case(prolog(_)).            /*Satisfy prolog goal  */
special_case(allow_prolog_lhs(_)).  /*ditto (new version) */
special_case(deduce _).             /*Call to backward chainer*/
special_case(_ receives_answer _).  /*Queries to user*/
special_case(all _ of _ are _).     /*Etceteras...*/
special_case(the _ of _ is _).
special_case(the _ of _ > _).
special_case(the _ of _ < _).
special_case(the _ of _ >= _).
special_case(the _ of _ =< _).
special_case(--_).                  /*Negated Patterns */

/* ========================= RETE ALGORITHM ============================

   This section of the file forms the RETE algorithm used to speed up
   forward chaining by compiling all the rules into a network structure
   and storing relevant facts in the network. These facts are updated when
   either the user of some other process such as loading a kb, calling
   forward or backward chaining causes them to change.

   Much of the RETE code is based on that given in Dennis Merritt's book,
   "Building Expert Systems in Prolog", published by Springer-Verlag.
   (and used with permission, which we gratefully acknowledge).

   == Network updating ==

   addrete/1 and delrete/1 propagate tokens though the rule network.

   All special case left-hand-side elements are sent through the
   network when the knowledge base is loaded, as these are treated as
   true until proven false (by the main forward chaining loop). */

addrete(Z):-
    root(ID,Z,NextList),
    ffsend(Z,NextList),
    fail.

addrete(Z):-
    addtms(Z).

ffsend(Z,NextList):-
    send(tok(ad,(Z)),NextList),
    !.

delrete(Z):-
    root(ID,Z,NextList),
    delr(Z),
    fail.

delrete(Z):-
    deltms(Z).

delr(Z):-
    !,
    send(tok(del,(Z)),NextList).

delr(Z).

/* And finally the workhorses that actually 'deliver' the tokens
   to the write places in the network... */

send(_,[]).

send(Token,[Node|Rest]):-
    sen(Node,Token),
    send(Token,Rest).

sen((rule)-N,tok(AD,Token)):-
    rul(N,ID,Token,Actions),
    (AD=ad,add_conflict_set(N,ID,Token,Actions);
    AD = del,del_conflict_set(N,ID,Token,Actions)),
    !.

sen(Node-l,tok(AD,Token)):-
    bi(Node,Token,Right,NextList),
    (AD=ad,(\+(retreive_mem(Node-l,Token)),add_to_mem(Node-l,Token));
    AD=del,del_from_mem(Node-l,Token)),
    !,
    matchRight(Node,AD,Token,Right,NextList).

sen(Node-r,tok(AD,Token)):-
    bi(Node,Left,Token,NextList),
    (AD=ad,(\+(retreive_mem(Node-r,Token)),add_to_mem(Node-r,Token));
    AD=del,del_from_mem(Node-r,Token)),
    !,
    matchLeft(Node,AD,Token,Left,NextList).

matchRight(Node,AD,Token,Right,NextList):-
    retreive_mem(Node-r,Right),
    'pd624 & append'(Token,Right,NewTok),
    send(tok(AD,NewTok),NextList),
    fail.

matchRight(_,_,_,_,_).

matchLeft(Node,AD,Token,Left,NextList):-
    retreive_mem(Node-l,Left),
    'pd624 & append'(Left,Token,NewTok),
    send(tok(AD,NewTok),NextList),
    fail.

matchLeft(_,_,_,_,_).

/* == Network Creation == */

/* reset_rete/0 clears all run-time working memory elements out of
   the RETE memory network. */

reset_rete:-
    retract(currentdb(Pattern,Truth)),
    delrete(Pattern),
    fail.

reset_rete.

/* The rest of the RETE predicates search through the knowledge base
   and compile all the rules found into a network structure.
   rete_compile/0 resets the relevant memory, starts the building of
   the network and then tries to send all the frame instances it can
   find through the network using the RETE utilities find_frames/0 and
   find_wm_elements/0. It is automatically used to build a
   new network whenever a newknowledge base is loaded using kb. */

rete_compile:-
    write('Compiling RETE network...'),nl,
    rete_init,
    note_access_demons,
    asserta(nid(0)),
    rete_compil,
    send_tests,
    find_frames,
    find_wm_elements.

rete_compile.

rete_init:-   /*Destroys all remains of old network, as it may interfere*/
    abolish(root,3),                                 /*with the new one*/
    abolish(bi,4),
    abolish(rul,4),
    abolish(varg,1),
    abolish(nid,1),
    abolish(temp,1),
    abolish(m,2),
    abolish(mem,2),
    abolish(conflict_set,2),
    abolish(rule_true,1).

rete_compil:-   /*Collects up rules and adds them to the network*/
    (rule N forward if LHS then RHS),
    add_rule((rule N forward if LHS then RHS)),
    fail.

rete_compil.

/* The following are workhorses used by rete_compile and rete_compil to
   build the network */

add_rule((rule N forward if (CONDS1 or CONDS2) then RHS)):-
    add_rule((rule N forward if CONDS1 then RHS)),
    add_rule((rule N forward if CONDS2 then RHS)).

add_rule((rule N forward if LHS then RHS)):-
    \+(LHS=(_ or _)),
    rete_comp(N,LHS,RHS).

rete_comp(N,H&T,RHS):-
    check_root(RN,H,HList),
    retcom(root(RN),H,HList,T,N,RHS),
    !.

rete_comp(N,H,RHS):-
    check_root(RN,H,HList),
    retcom(root(RN),H,HList,[],N,RHS),
    !.

rete_comp(N,_,_).

retcom(PNID,OutPat,PrevList,[],N,RHS):-
    build_rule(OutPat,PrevList,N,RHS),
    update_node(PNID,PrevList,(rule)-N),
    !.

retcom(PNID,PrevNode,PrevList,H&T,N,RHS):-
    check_root(RN,H,HList),
    check_node(PrevNode,PrevList,H,HList,NID,OutPat,NList),
    update_node(PNID,PrevList,NID-l),
    update_root(RN,HList,NID-r),
    !,
    retcom(NID,OutPat,NList,T,N,RHS).

retcom(PNID,PrevNode,PrevList,H,N,RHS):-
    check_root(RN,H,HList),
    check_node(PrevNode,PrevList,H,HList,NID,OutPat,NList),
    update_node(PNID,PrevList,NID-l),
    update_root(RN,HList,NID-r),
    !,
    retcom(NID,OutPat,NList,[],N,RHS).

build_rule(OutPat,PrevList,N,RHS):-
    gen_nid(ID),
    assertz(rul(N,ID,OutPat,RHS)).

check_root(NID,Pattern,[]):-
    gen_nid(NID),
    assertz(root(NID,Pattern,[])),
    test_store(Pattern).

check_root(N,Pattern,List):-
    asserta(temp(Pattern)),
    retract(temp(T1)),
    root(N,Pattern,List),
    root(N,T2,_),
    comp_devar(T1,T2),
    !.

check_root(NID,Pattern,[]):-
    gen_nid(NID),
    assertz(root(NID,Pattern,[])),
    test_store(Pattern).

check_node(PNode,PList,H,HList,NID,OutPat,[]):-
    \+(bi(_,PNode,H,_)),
    'pd624 & append'(PNode,H,OutPat),
    gen_nid(NID),
    assertz(bi(NID,PNode,H,[])),
    !.

check_node(PNode,PList,H,HList,NID,OutPat,NList):-
    'pd624 &append'(PNode,H,OutPat),
    asserta(temp(OutPat)),
    retract(temp(Tot1)),
    bi(NID,PNode,H,NList),
    bi(NID,T2,T3,_),
    'pd624 & append'(T2,T3,Tot2),
    comp_devar(Tot1,Tot2),
    !.

check_node(PNode,PList,H,HList,NID,OutPat,[]):-
    'pd624 & append'(PNode,H,OutPat),
    gen_nid(NID),
    assertz(bi(NID,PNode,H,[])).

update_root(RN,HList,NID):-
    'pd624 member'(NID,HList),
    !.

update_root(RN,HList,NID):-
    retract(root(RN,H,HList)),
    asserta(root(RN,H,[NID|HList])).

update_node(root(RN),HList,NID):-
    update_root(RN,HList,NID),
    !.

update_node(X,PrevList,NID):-
    'pd624 member'(NID,PrevList),
    !.

update_node(PNID,PrevList,NID):-
    retract(bi(PNID,L,R,_)),
    asserta(bi(PNID,L,R,[NID|PrevList])).

comp_devar(T1,T2):-
    init_vargen,
    del_variables(T1),
    del_variables(T2),
    T1=T2.

del_variables(T):-
    de_vari(T).

de_vari([]).

de_vari([H|T]):-
   de_var(H),
   de_vari(T).

de_vari(X):-
    de_var(X).

de_var(X/_):-
    de_var(X).

de_var(the X of Y is Z):-
   de_v(the X of Y),
   de_vl(Z),
   !.

de_vl([]).

de_vl([H|T]):-
    de_v(H),
    de_v(T).

de_vl(X):-
    d_v(X).

d_v(V):-
    var(V),
    var_gen(V),
    !.

de_v(the X of Y):-
    d_v(X),
    d_v(Y).

d_v(_).

init_vargen:-
    abolish(varg,1),
    asserta(varg(1)).

var_gen(V):-
    retract(varg(N)),
    NN is N+1,
    asserta(varg(NN)),
    int2string(N,NS),
    stringconcat('#VAR_',NS,X),
    name(V,X).

/* == RETE-Specific Utilities == */

/* NOTE: The first two are written specificaly for the supplied Prolog  */

/* COMPATABILITY Note: The string concatenation clause shown immediately
   below is specific to the supplied Prolog, if you are using a different
   type of (Edinburgh syntax) Prolog then adjust the comment markers so
   the other clause is used instead. */

/* stringconcat(Name1,Name2,Concatenation):-
    Concatenation is_string (Name1 & Name2). */

stringconcat(Name1,Name2,Concatenation):-
    name(Name1,String1),
    name(Name2,String2),
    append(String1,String2,Concatenation).
/*    name(Concatenation,ResultString). */

/* COMPATABILITY Note: The string manipulation below is also Prolog2
specific, a CProlog version might look like the following
*/
/*int2string(Integer,String):-
   name(Integer,IntList),
   append(IntList,[39],List),
   name(String,[39|List]). */

int2string(X,X).  /* adequate for SICSTUS. PMR */



/* int2string(Integer,String):-       /* Converts from Integer to string */
/*    number(Integer,String).        /****Prolog-2 specific****/

gen_nid(NID):-
    retract(nid(N)),
    NID is N+1,
    asserta(nid(NID)).

/* find_frames/0 gathers up all currently stored frames and sends their
   constituant parts through the network. Unfortunately this is rather
   time consuming for knowledge bases which contain a multiplicity of
   rules and/or a multitude of objects (e.g. KIVA.KB). */

find_frames:-
   root(_,Cond,_),
   \+(special_case(Cond)),
   check_out(Cond),
   fail.

find_frames.

check_out(the X of Y is Z):-
   ((Y instance_of _ with _);
   (Y subclass_of _ with _)),
   fetch(Y,X,Z,[Y],_),
   \+(Z=unknown),
   addrete(the X of Y is Z),
   fail.

check_out(X instance_of Z):-
   (X instance_of Z),
   addrete(X instance_of Z),
   fail.

check_out(X subclass_of Z):-
   (X subclass_of Z),
   addrete(X subclass_of Z),
   fail.

check_out(_).

/* note_access_demons/0 searches through all currently loaded frames and
   sets up a special_case clause for all slot:filler combinations which
   use access demons as slot fillers. This must be done as such fillers
   are not necessarily constant, and changes to them are not automatically
   trapped by the RETE network. */

note_access_demons:-
    (Object instance_of _ with Body),
    hunt_demons(Body,Object),
    fail.

note_access_demons:-
    (Object subclass_of _ with Body),
    hunt_demons(Body,Object),
    fail.

note_access_demons.

hunt_demons(((Attribute:H),T),Object):-
    (H=[_|_],
    ('pd624 member'((access_rule:_),H);
     'pd624 member'((change_rule:_),H)),
    !,
    assert(special_case(the Attribute of _ is _)),
    hunt_demons(T,Object));
    hunt_demons(T,Object).

hunt_demons((Attribute:H),Object):-
    H=[_|_],
    ('pd624 member'((access_rule:_),H);
     'pd624 member'((change_rule:_),H)),
    assert(special_case(the Attribute of _ is _)).

/* add_conflict_set/4 and del_conflict_set/4 add and delete rules from
   RETE's conflict set. */

add_conflict_set(Rule,ID,Token,Actions):-
    \+(conflict_set(ID,(rule Rule forward if Token then Actions))),
    assertz(conflict_set(ID,(rule Rule forward if Token then Actions))),
    go_with((rule Rule forward if Token then Actions)).

del_conflict_set(Rule,ID,Token,Actions):-
    conflict_set(ID,(rule Rule forward if Token then Actions)),
    retract(conflict_set(ID,(rule Rule forward if Token then Actions))),
    fc_undo_rhs(Actions,Rule).

/* test_store/1 stores 'special case' lhs conditions in memory
   during compilation, see send_tests/0 below for the reason.  */

test_store(Cond):-
    special_case(Cond),
    assert(bank(Cond)).

test_store(_).

/* send_tests/0 retreives special lhs conditions and sends them though the
   RETE network. It does this as such conditions are always assumed true
   until proven false by fc_good_potential. The conditions cannot be sent
   through the network during compilation, as the network is not complete
   at that time, hence the need for test_store/1.*/

send_tests:-
    retract(bank(Test)),
    addrete(Test),
    fail.

send_tests.

/* find_wm_elements sends all current working memory elements through
   the RETE network. */

find_wm_elements:-
    currentdb(X,true),
    addrete(X),
    fail.

find_wm_elements.

find_virtual_wm_elements :-  /* 4/1/91 similar to currentdb, but finds... */
    justification(X,_,_),    /* derived wm elements as well as top level */
    addrete(X),
    fail.

find_virtual_wm_elements.


add_to_mem(Node-Side,Token):-    /*ASSERT WAS ASSERTA 21/9/90*/
    (m(ID,Token),                 /* Either a token already exists, in   */
     assert(mem(Node-Side,ID))); /* which case add a pointer to the     */
    (gen_nid(NID),                /* token, or generate a new ID number  */
     assert(m(NID,Token)),       /* and add it's pointer to the memory. */
     assert(mem(Node-Side,NID))).

del_from_mem(Node-Side,Token):-   /* To delete a token from RETE memory  */
    m(ID,Token),                  /* find it's ID first, and then retract*/
    retract(mem(Node-Side,ID)).   /* the memory entry with this pointer. */

retreive_mem(Node-Side,Token):-   /* To retreive tokens the token's ID  */
    m(ID,Token),                  /* is found, and then a check is made */
    mem(Node-Side,ID).            /* at the relevant node.              */

/*quicksortof/2 is used to sort the conflict set into the order in which
  the rules appear in the knowledge base, this makes it fully compatable
  with the previous version of MIKE. The sort is task specific, so cannot
  be used to sort any other types of information (hence quicksortof)! */

quicksortof(Unsorted,Sorted):-
    quicksortof(Unsorted,Sorted,[]).

quicksortof([],AuxiliaryList,AuxiliaryList).

quicksortof([Element|Rest],Sorted,AuxiliaryList):-
    split_up(Element,Rest,ListA,ListB),
    quicksortof(ListB,ListC,AuxiliaryList),
    Element=conflict_set(ID,SmartElement),  /*ruined 18/9/90 @ 3:41*/
    quicksortof(ListA,Sorted,[SmartElement|ListC]).

split_up(_,[],[],[]):- !.

split_up(Element,[Head|Rest],[Head|RestA],RestB):-
    precedes(Head,Element),
    !,
    split_up(Element,Rest,RestA,RestB).

split_up(Element,[Head|Rest],RestA,[Head|RestB]):-
    split_up(Element,Rest,RestA,RestB).

precedes(X,Y):-
    X=conflict_set(IDX,_),
    Y=conflict_set(IDY,_),
    compare(<,IDX,IDY).

/*'pd624 & append'/3 has just the same use as append, but it is for use
  lists of the form _&_, not [_|_] */

'pd624 & append'(H&L1,L2,H&L3):-
    'pd624 & append'(L1,L2,L3).

'pd624 & append'(H,L,H&L).

/* == RETE Status == */

/* The following predicates are used to turn RETE optimisation on and off*/

rete(on). /*RETE is on by default*/

rete_off:-
    rete(off),
    !,
    write('RETE optimisation is already off.'),nl.

rete_off:-
    rete(on),
    tms(on),
    !,
    writel([
 'Disabling RETE processing upsets the TMS, hence you are not allowed',
 'to turn RETE off without manually turning the tms off first (with tms_off).',
  nl]).

/* COMPATIBILITY NOTE: This next clause makes a run-time assertion of
   some code (new definition of fc_do_one_cycle), which may require a
   'dynamic' declaration in some Prolog dialects.
*/
rete_off:-
    rete(on),
    rete_init, /*Free all memory previously used to store network*/
    retract(rete(on)),
    assert(rete(off)),
    asserta(rete_compile),    /*Make rete_compile/0 inactive*/
    asserta(addrete(_)),      /*ditto for addrete/1 */
    asserta(delrete(_)),      /*ditto for delrete/1 */
    abolish(fc_do_one_cycle,1),  /*Replace main fc predicate...*/
    assert((
   fc_do_one_cycle(NewWME) :-
                             /* NewWME is OUTPUT (New Working Memory Elements)*/
   fc_update_counters(WME),  /* fetch current WM, update cycle counter */
   findall1((rule RULE forward if COND then ACTIONS), /* given this pattern */
            fc_good_potential(RULE,COND,ACTIONS), /* round up candidates */
            ConfSet),                             /* put into conflict set */
   fc_pick_one_and_do_it(ConfSet,WME,NewWME),     /* choose one winner */
   !)),
   write('RETE optimisation is now disabled.'),nl.

rete_on:-
    rete(on),
    write('RETE optimisation is already on.'),nl.

/* COMPATIBILITY NOTE: This next clause makes a run-time assertion of
   some code (new definition of fc_do_one_cycle), which may require a
   'dynamic' declaration in some Prolog dialects.
*/
rete_on:-
    rete(off),
    retract(rete(off)),
    assert(rete(on)),
    retract(rete_compile),
    retract(addrete(_)),
    retract(delrete(_)),
    abolish(fc_do_one_cycle,1),
    assert((
   fc_do_one_cycle(NewWME) :-
                             /* NewWME is OUTPUT (New Working Memory Elements)*/
   fc_update_counters(WME),  /* fetch current WM, update cycle counter */
   findall1(conflict_set(ID,(rule Rule forward if Conds then Actions)),
        (conflict_set(ID,(rule Rule forward if Conds then Actions)),
         fc_check_lhs(Conds),
         when_enabled('show history on request' for [Rule,'+'])),
        ConfSet),               /*Extract conflict set components... */
   quicksortof(ConfSet,SortedSet),   /*sort them into kb appearance order..*/
   fc_pick_one_and_do_it(SortedSet,WME,NewWME), /*...and choose a winner.*/
   !)),
   rete_compile,   /*Compile rules (back?) into network form */
   write('RETE optimisation is now enabled.'),nl.

/* rete/0 can be used to find out whether RETE optimisation is on of off*/

rete:-
    nl,writel([
'This version of MIKE allows optional forward chaining optimisation based on',
'the RETE algorithm. Three new commands are available:-',
nl,
'rete_on     Enables optimisation (forward chaining around 7 times faster)',
'rete_off    Disables optimisation (uses less memory)',
'rete        Displays this message and shows current RETE status.',
nl,
'For further inforamtion such as setting the default RETE status see READ.ME.'
      ]),
      rete(State),
      nl,write('Rete optimisation is currently '),write(State),write('.'),nl.

/* ===================== TRUTH MAINTAINANCE SYSTEM ===================== */

/* addtms/1 and deltms/1 are called via addrete/1 and delrete/1 whenever
   a working memory element is added or removed from working memory, they
   do special stuff like enhanced run-time negation verification and
   checking for conflicts, such as foo and ~foo being in wm simultaniously.*/

deltms(Pattern):-
    root(_,--Pattern,_),
    addrete(--Pattern).

deltms(_).

addtms(Pattern):-
    check_justification_links(Pattern),  /*Make sure reason for adding is*/
    !,                                                            /*valid*/
    addtms2(Pattern).

addtms(Pattern):-                         /*If it isn't then remove it again*/
    retract(currentdb(Pattern,_)),
    retract(justification(Pattern,_,_)),
    ((enabled('show new working memory elements or frame changes',
            ' enabled ',5),  /* and inform user IF tracing(5) is enabled */
      'pd624 write'(['Inconsistent support for WM pattern ',
      Pattern,', so MIKE has removed it.',nl])) ; true).

addtms(_). /*PATCH 30NOV90*/

addtms2(~Pattern):-
    in_wm(Pattern),
    resolve_contradiction(~Pattern),
    addtms3(Pattern).

addtms2(Pattern):-
    \+(Pattern=(~(_))),
    in_wm(~Pattern),
    resolve_contradiction(Pattern),
    addtms3(Pattern).

addtms2(Pattern):-
    addtms3(Pattern).

addtms2(_).

addtms3(Pattern):-
    root(_,--Pattern,_),
    delrete(--Pattern).

addtms3(_).

fc_undo_rhs(H&T,ThisRule):-
    ((not_the_only_reason(H),      /* Either there is another reason why */
    delete_justification(H,ThisRule));
    undo_action(H,ThisRule)),      /* or there isn't !?!? */
    fc_undo_rhs(T,ThisRule).

fc_undo_rhs(H,ThisRule):-
    ((not_the_only_reason(H,ThisRule),
    delete_justification(H,ThisRule));
    undo_action(H,ThisRule)).

undo_action(H,ThisRule):-
    (H=(remove Pattern),
    perform1(add Pattern,_,'tms','a rule became false'));
    ((H=(add Pattern cf _);H=(add Pattern)),
    perform1(remove Pattern,_,ThisRule,'tms_hacking_it_up!'));
    last_resort(H,ThisRule).

undo_action(_,_).

last_resort(Pattern,R):-  /*If Pattern doesn't match any of the following*/
    Pattern=(prolog(_));  /*then add Pattern to wm*/
    Pattern=(strategy _);
    Pattern=(query _ receives_answer _);
    Pattern=(note _);
    Pattern=(announce _);
    perform1(remove Pattern,_,R,'tms_hacking_it_up!').

delete_justification(add Pattern,ThisRule):-
    'pd624 retract'(justification(Pattern,ThisRule,_)).

delete_justification(remove Pattern,ThisRule):-
    'pd624 retract'(justification(Pattern,ThisRule,_)).

delete_justification(Pattern,ThisRule):-   /*Simple add case*/
    'pd624 retract'(justification(Pattern,ThisRule,_)).

go_with((rule A forward if B then C)):-
    fc_check_lhs(B),
    perform(C,_,A,B).

go_with(_).

not_the_only_reason(add Pattern,ThisRule):-
    just_check(Pattern,ThisRule).

not_the_only_reason(remove Pattern,ThisRule):-
    just_check(Pattern,ThisRule).

not_the_only_reason(Pattern,ThisRule):-   /*Second 'add' type pattern*/
    just_check(Pattern,ThisRule).

just_check(Pattern,ThisRule):-
    justification(Pattern,Rule,Conds), /* If there is link to another rule   */
    \+(Rule=ThisRule),          /* i.e. one that isn't this one then  */
    (Rule='top level';           /* there is another reason if origin  */
    fc_check_lhs(Conds)).        /* is top level or lhs conds are true.*/

'pd624 retract'(Pattern):-       /*'pd624 retract' is just the same as  */
    retract(Pattern),            /*retract except that gets all matches */
    fail.                        /*out, and it succeeds.                */

'pd624 retract'(_).

/* check_justification_links/1 checks to see it the justifications are
   well founded */

check_justification_links(Fact):-
    check_justification(Fact,Fact).

check_justification(--Fact,Fact2) :- !.
                        /*--wm elements have no justification*/

check_justification(~Fact,Fact2):-
    \+(justification(Fact,_,_)),
    !.

check_justification(Fact,Fact2):-
    justification(Fact,'top level','You told me so'),
    !.

check_justification(Fact,Fact2):-
    justification(Fact,Rule,Conds),
    \+('pd624 & member'(Fact2,Conds)),
    \+('pd624 & member'(--Fact2,Conds)),
    \+('pd624 & member'(~Fact2,Conds)),
    !,
    check_conds_justification(Conds,Fact2).

check_justification(Fact,Fact2):-
    justification(Fact,Rule,Conds),
    'pd624 write'(['Inconsistency detected: ',nl,' ',
                  Fact2, ', which was apparently supported by', nl]),
    justify(Fact2),
    'pd624 write'(['is not tenable given premise(s) ',
                  Conds,'   of rule ',Rule,nl]),
    !,fail.  /* cut just in case formating statement  resatisfiable */

check_conds_justification(H&T,Fact2):-
    !,
    check_justification(H,Fact2),
    check_conds_justification(T,Fact2).

check_conds_justification(H,Fact2):-
    check_justification(H,Fact2).

/* tms_on and tms_off (hopefully) do as their names suggest! */

tms(on).    /*TMS is on by default*/

tms_off:-
    retract(tms(on)),
    asserta(go_with(_)),
    asserta(fc_undo_rhs(_,_)),
    asserta(addtms(_)),
    asserta(deltms(_)),
    assert(tms(off)),
    write('TMS mode is now disabled.').

tms_off:-
    write('TMS mode is already off.'),nl.

tms_on:-
    rete(off),
    !,
    write('TMS will not function unless RETE is enabled (using rete_on).'),nl,
    write('TMS status remains unchanged.'),nl.

tms_on:-
    retract(tms(off)),
    retract(go_with(_)),
    retract(fc_undo_rhs(_,_)),
    retract(addtms(_)),
    retract(deltms(_)),
    assert(tms(on)),
    write('TMS processing is now enabled.'),nl.

tms_on:-
    write('TMS mode is already on.'),nl.

/*tms/0 is like rete/0 in that it gives help/status info */

tms:-
    nl,writel([
'This version of MIKE allows an optional truth maintenance system to operate',
'in the background. Three new commands are available to support this:-',
nl,
'tms_on     Enables TMS',
'tms_off    Disables TMS',
'tms        Displays this message and shows current TMS status.',
nl]),
      tms(State),
      write('TMS processing is currently '),write(State),write('.'),nl.

/*
resolve_contradiction/1 resolves contradications such as e and ~e being in
working memory at the same time.  To do this, it focusses on the belief which
is a derived conclusion (let's suppose that it is e, and that the belief ~e is
a raw premise asserted by the user rather than a derived conclusion), and sees
whether the 'foundations' of e (the 'bottom line' premises from which e was
derived) can be 'undermined' in a sensible way.  The foundations of e are the
terminal nodes as seen in the 'justification' proof tree.  Beliefs can be
'undermined' if there is no basis for believing them in the first place.  For
example, terminal nodes which were asserted directly by the user CANNOT be
undermined by the TMS.  Terminal nodes of the proof tree which specify the
ABSENCE of a pattern, however, (e.g. --d) depict a 'state of affairs' which
holds at the moment, but has no firm foundation of its own...  and can
therefore be undermined by changing its state from absent to present! This is
precisely what the TMS does.  More discussion & examples are provided
in the file MIKE2REF.DOC.
*/

resolve_contradiction(Pattern):-
    resolve_using_defaults(Pattern),
    find_wm_elements,  /*just to keep things up-to-date*/
    !.

resolve_contradiction(~Pattern):-
    !,
    'pd624 write'([
'A contradiction has occured, both ',Pattern,' and ',~Pattern,nl,
'are currently in working memory. This could not be resolved internally',nl,
'so you must now remove either or both of these items from wm.'
]).

resolve_contradiction(Pattern):-
    !,
    'pd624 write'([
'A contradiction has occured, both ',Pattern,' and ',~Pattern,nl,
'are currently in working memory. This could not be resolved internally',nl,
'so you must now remove either or both of these items from wm.'
]).

resolve_using_defaults(~Pattern):-
    !,
    justification(Pattern,Rule,Conds),
    pick_a_default(Pattern,Default),
    build_a_contradiction_rule(~Pattern,Conds,Default,RuleName,Justification).

build_a_contradiction_rule(TroubleMaker,Conds,Result,Name,LHS):-
    gen_nid(ID),
    int2string(ID,StringID),
    stringconcat('contradiction_resolver_',StringID,StName),
    name(Name,StName),
    build_lhs(TroubleMaker,LHS),
    assert((rule Name forward if LHS then add Result)),
    add_rule((rule Name forward if LHS then add Result)),
    fc_check_lhs(LHS), /* 4/1/91.. this SHOULD do TMS work, but doesn't */
    perform(add Result,_,Name,LHS),
    find_virtual_wm_elements.  /* 4/1/91 trigger TMS assertions m(...) */


build_lhs(~Pattern,X):-
    !,
    find_foundations(Pattern,~Pattern,X).

build_lhs(Pattern,X):-
    find_foundations(~Pattern,Pattern,X).

find_foundations(X,List,Result):-
    justification(X,_,Conds),
    find_foundations_in_conds(Conds,List,Result).

find_foundations_in_conds(H&T,List,Result):-
    !,
    ((justification(H,'top level','You told me so'),
    'pd624 & append'(H,List,NewList),
    find_foundations_in_conds(T,NewList,Result));
    (\+(justification(H,_,_)),
    find_foundations_in_conds(T,List,Result));
    (find_foundations(H,List,NewList),
    find_foundations_in_conds(T,NewList,Result))).

find_foundations_in_conds(H,List,Result):-
    (justification(H,'top level','You told me so'),
    'pd624 & append'(H,List,Result));
    (\+(justification(H,_,_)),
    Result=List);
    find_foundations(H,List,Result).

pick_a_default(Pattern,Default):-
    justification(Pattern,Rule,Conds),
    get_a_default(Conds,Default).

pick_a_default(Pattern,Default):-
    justification(Pattern,Rule,Conds),
    pick_a_default_from_conds(Conds,Default).

pick_a_default_from_conds(H&T,Default):-
    !,
    (pick_a_default(H,Default);
    pick_a_default_from_conds(T,Default)).

pick_a_default_from_conds(H,Default):-
    pick_a_default(H,Default).

get_a_default(H&T,D):-
    !,
    ((H=(--X),
    D=X);
    get_a_default(T,D)).

get_a_default(H,D):-
    H=(--X),
    D=X.

tms_trace(Pattern,'top level',Action):-
    tms(on),    /* Only trace if TMS is ON & tracing option 5 is ON */
    enabled('show new working memory elements or frame changes',' enabled ',5),
    write('The premise '),write(Pattern),write(' has been '),
    (((Action='added to'),write('added'));write('removed')),
    nl.

tms_trace(Pattern,Rule,Action):-
    tms(on),    /* Only trace if TMS is ON & tracing option 5 is ON */
    enabled('show new working memory elements or frame changes',
            ' enabled ',5),
    write(Pattern),write(' has just been '),write(Action),
    write(' working memory by rule '),write(Rule),nl.

tms_trace(_,_,_).

/* ================== JUSTIFICATION TREE GENERATOR ===================== */
/* Version 1.0 */
/* PC version of MIKE only */

justify Pattern:-
    abolish(jhistory,1),
    assert(jhistory([Pattern])),
    write(Pattern),nl,
    justification(Pattern,_,Conds),
    drawtree(Pattern,'').

justify Pattern:-
    write(' is not in working memory'),nl.

drawtree(Pattern,IndStr):-
    justification(Pattern,_,Conds),
    do_just_once(draw_conds(Conds,IndStr)),
    fail.

drawtree(_,_).

draw_conds(H&T,IndStr):-
    write(IndStr),
/*    put(195),put(196), */
    put(124), put(95), /* PMR */
    ((H='You told me so',put(64));jwrite(H)),nl,
    go_deeper(H,IndStr,[/* 179 */ 124,32]),
    draw_conds(T,IndStr).

draw_conds(H,IndStr):-
    write(IndStr),
/*    put(192),put(196), */
    put(124), put(95), /* PMR */
    ((H='You told me so',put(64));jwrite(H)),nl,
    go_deeper(H,IndStr,[32,32]).

go_deeper(Pattern,IndStr,CharList):-
    jhistory(List),
    \+('pd624 member'(Pattern,List)),
    retract(jhistory(List)),
    append(List,[Pattern],NewList),
    assert(jhistory(NewList)),
    justification(Pattern,_,Conds),
/*    list(CharList,CharString), */
    name(CharString,CharList),
    stringconcat(IndStr,CharString,NewIndStr),
    name(NewInd,NewIndStr), /* PMR */
    drawtree(Pattern,NewInd).

go_deeper(_,_,_).

jwrite(Pattern):-
    jhistory(List),
    (('pd624 member'(Pattern,List),    /* to avoid endless display */
    write('{'),write(Pattern),write('}')); /* changed to '{' 31/12/90 */
    write(Pattern)).

/* ======================= Uncertainty Utilities ======================= */
/*                          (user modifiable)                            */

method(standard).  /* Default combination method is 'standard' */

/* combine/4 is used to combine the CFs of the antecedents
for the conclusion */

combine(standard,AntecedentCFs,RuleCF,New):-
    min(AntecedentCFs,1.0,Min),
    New is RuleCF*Min.

combine(fuzzy,AntecedentCFs,RuleCF,RuleCF).

combine(bayes,AntecedentCFs,RuleCF,New):-
    mult(AntecedentCFs,1.0,Product),
    New is RuleCF*Product.

/* combine/3 is used to combine the CFs of different entries of an
   element in working memory, so that the element can be re-added
   with the compound CF */

combine(standard,ElementCFs,New):-
    max(ElementCFs,0,New).

combine(fuzzy,ElementCFs,New):-
    max(ElementCFs,0,New).

combine(bayes,[H,T],New):-
    New is (1-H)*T+H.

mult([H|T],In,Out):-
    Newin is H*In,
    mult(T,Newin,Out).

mult([],In,In).

min([H|T],In,Out):-
    (compare(<,H,In),
    min(T,H,Out));
    min(T,In,Out).

min([],In,In).

max([H|T],In,Out):-
    (compare(>,H,In),
    max(T,H,Out));
    max(T,In,Out).

max([],In,In).

/* pcm/2 stores Possible Certainty Methods */

pcm(1,standard).
pcm(2,fuzzy).
pcm(3,bayes).

/* certainty/1 is used to change the certainty calculation method */

certainty(Method):-
     integer(Method),          /* if a number is given */
     ((pcm(Method,String),     /* find relevant string */
     retract(method(_)),       /* remove old setting   */
     assert(method(String)),  /* and assert new one   */
     'pd624 write'(['The default certainty combination method is now: ',
                    String,nl])
     );
     (nl,
     write(
       'Invalid option - you must specify 1, 2, 3, standard, fuzzy or bayes'),
     nl,write('Setting remains unchanged.'),nl)).
certainty(Method):-  /* allow look up using a VAR */
     var(Method),!,
     method(Method).
certainty(Method):-
     \+(integer(Method)),
     ((pcm(_,Method),          /* check the method is valid */
     retract(method(_)),       /* remove old setting   */
     assert(method(Method)),   /* assert new one       */
     'pd624 write'(['The default certainty combination method is now: ',
                    Method,nl])
     );
     (nl,
     write(
      'Invalid option - you must specify 1, 2, 3, standard, fuzzy or bayes'),
     nl,write('Setting remains unchanged.'),nl)).

/* Certainty/0 provides a nice(!?) way of using cetainty/1 */

certainty:-
    method(Old),
    'pd624 write'(['*****  MIKE Uncertainty Control Menu *****',nl,
    'The currently selected method is: ',Old,nl,
    'Options available are as follows:',nl]),
    write('     1 - Standard'),nl,
    write('     2 - Fuzzy   '),nl,
    write('     3 - Bayes(ian)'),nl,nl,
    'pd624 write'(['Type the number of the option you would like to use, eg. 1',
    ', and then a FULL STOP',nl,'==> ']),
    read(Option),
    certainty(Option).    /* set the user chosen option */

/* threshold/1 is used to set the certainty threshold for rule left-hand-sides*/

threshold(Value):-
    retract(lhsthreshold(_)),
    assert(lhsthreshold(Value)).

/* threshold/0 provides a user interface to threshold/0 */

threshold:-
 'pd624 write'(['Type the required threshold value (0.0 to 1.0) to use during',
 nl,'conflict resolution, and then a FULL STOP',nl,'==> ']),
 read(Value), threshold(Value).



/* ===================== SHOW STATUS UTILITY ==================7-11-90=== */

status:-
 current_conflict_resolution_strategy(CCRS),
 lhsthreshold(LHST),
 method(M),
 rete(R),
 tms(T),
 memkb(KBName),
 pd624_own_findall(Obj, (Obj with _), AllObjs),
 pd624_own_findall(RP, (rule RP), AllRules),
 pd624_own_findall(W, currentdb(W,_), AllWM),
 'pd624 list length'(AllObjs, NFrames),
 'pd624 list length'(AllRules, NRules),
 'pd624 list length'(AllWM, NWMElements),
 'banner line',
 'pd624 write'([
 nl,
 'MIKE OPTIONS                     USEFUL COMMANDS            STATUS/COUNT',
 nl,nl,
 'TMS............................. tms_on.     tms_off.       ',T,nl,
 'RETE............................ rete_on.    rete_off.      ',R,nl,
 'Rules........................... show rules. describe R.    ',NRules,nl,
 'Frames.......................... browse.     describe F.    ',NFrames,nl,
 'Working memory elements......... wm.         cf.            ',NWMElements,
 nl,
 'Uncertainty paradigm............ certainty.  threshold.     ',M,nl,
 'Current knowledge base.......... kb ''<filename>''.           ',KBName,
 nl,nl,
 'Current conflict resolution strategy (in order) is:',nl,
  CCRS,nl,
 'with a left-hand-side certainty threshold of ',LHST,'.']),
 'banner line',
 nl.

/* file: IO.PL  {query, how, and why} */
/*
                            *************
                             M I K E - 2
                            *************
               Micro Interpreter for Knowledge Engineering
                  {written in Edinburgh-syntax Prolog}

MIKE: Copyright (c) 1989, 1990, 1991 The Open University (U.K.)

MIKE is intended for educational purposes, and may not
be sold as or incorporated in a commercial product without
written permission from: The Copyrights Officer, Open University,
Milton Keynes MK7 6AA, U.K.

The Open University accepts no responsibility for any legal or other
consequences which may arise directly or indirectly as a result of the
use of all or parts of the contents of this program.
*/
/* This file handles Input/output from/to user via query, how, and why */

/*query Q receives_answer yes :-
  Q receives_answer no,!,fail.
query Q receives_answer no :-
  Q receives_answer yes,!,fail.    PMR changed this */

query Question receives_answer Answer :-
    Question receives_answer AnAnswer,!,  /* don't ask same question twice*/
    Answer = AnAnswer.
query the A of B receives_answer Answer :-
    the A of B is AnAnswer receives_answer yes,!,
    Answer = AnAnswer.

/* it is an implicit assumption that the answer is either yes or no */
query the Attr of Obj is Value receives_answer Answer:-
    var(Value),
    'pd624 write'(['Error: the Value facet must be bound in a question',nl,
        'of the form "the attribute of object is value".  If you ',nl,
        'actually want to find out the value then you must use the',
        nl,'question form "the attribute of object receives_answer value"',
        nl]),!,fail.
query the Attr of Obj is Value receives_answer Answer :-!,
    write('Is it the case that the '),write(Attr),write(' of '),
    write(Obj),write(' is '),write(Value),write('?'),nl,

write('Answer yes/no ==> '),
        'pd624 read'(the Attr of Obj is Value,Answer,X),nl,
    (standardise_answers(X,Answer); /* this will fail is answer is not
                                          yes or no */
    update_answers(the Attr of Obj is Value,X),fail),
    consistency_check(Obj,Attr,Value,Answer),
    update_answers(the Attr of Obj is Value, Answer).
query the Attr of Obj receives_answer Value :- !,
    write('What is the '),write(Attr),write(' of '),
        write(Obj),nl,write('==> '),'pd624 read'(the Attr of Obj,Value,Value1),
    Value = Value1,
           store(Obj,Attr,Value),
    update_answers(the Attr of Obj, Value).

query  [H|T] receives_answer Whatever :- !,
    'pd624 pretty list'([H|T]),nl,
    yes_no_check([H|T],Whatever).
query Thing receives_answer Whatever :-
    write(Thing),nl,
    yes_no_check(Thing,Whatever).

yes_no_check(Thing,Answer):- (var(Answer); \+(Answer=yes),
        \+(Answer=no)),!, /* next two clauses thus disallowed */
    write(' ==> '), ttyflush,
    'pd624 read'(Thing,Answer,AnAnswer),nl,!, /* PMR added An.. and cut */
    update_answers(Thing,AnAnswer),           /* ditto */
    Answer = AnAnswer.                        /* PMR added this */

yes_no_check(Thing,yes):- !,
    write('Answer yes/no ==> '), ttyflush, 'pd624 read'(Thing,yes,X),nl,
    (standardise_answers(X,yes);
    update_answers(Thing,X),fail),
    chuck_it_out(Thing),
    assert(currentdb(Thing,true)),
    update_answers(Thing, yes).

yes_no_check(Thing,no):- !,
    write('Answer yes./no. ==> '),'pd624 read'(Thing,no,X),nl,
    (standardise_answers(X,no);
    update_answers(Thing,X),fail),
    assert(currentdb(Thing,false)),
    chuck_it_out(Thing),
    update_answers(Thing,no).

'pd624 read'(Thing,Input,Answer):-

        repeat_and_prompt,
        read(X),
    completion(Thing,Input,Answer,X),!.

repeat_and_prompt.
repeat_and_prompt:- nl, write('==> '), repeat_and_prompt.

completion(Thing,Input,Answer,why):-
    Thing explained_by Text,
    'pd624 write'(Text),nl,!,fail.
completion(Thing,Input,Answer,why):-
    write('Currently MIKE has no explanation.'),nl,!,fail.
completion(Thing,Input,Answer,how(Item)):-
 nonvar(Item),
    how Item,nl,!,fail.
completion(Thing,Input,Answer,how(_)):-
    write('MIKE is unable to justify the statement.'),nl,!,fail.
completion(Thing,Input,Answer,halt):- !,
    assert(currentdb(halt,true)).
completion(Thing,Input,Input,Input).
/* yes/no handling is special... user MUST type one of: [yes,y,no,n]
  or the system will complain.  query does the smart thing, i.e. if
  you answer any of the 4 legal choices, your response is asserted in
  the database (under 'receives_answer/2') to avoid asking dumb question
  more than once
*/

completion(Thing,yes,no,no).
completion(Thing,no,yes,yes).
completion(Thing,yes,yes,y).
completion(Thing,no,no,n).
/* next line means: 'For query Thing, you expected a 'yes', but
   you treat it as a 'no' because the user typed in 'n'. */
completion(Thing,yes,no,n).
completion(Thing,no,yes,y).
completion(Thing,Input,X,Y):-
 (Input = yes;Input = no), 'pd624 write'(
   ['ERROR: ',Y,' is an illegal response.',nl,
   'Answer yes. or no. ',nl]),!,fail.
completion(Thing,Input,X,X):- !.


update_answers(the A of B,Answer):- \+(B = (C is D)),  /* loop detector */
    update_answers(the A of B is Answer,yes).
update_answers(Thing,Answer):-
    Thing receives_answer Answer1,
    \+ Answer1 = Answer, !,
    write('Warning overwriting existing answer'),nl,write(Thing),
    write(' receives_answer '),write(Answer1),write(' with '),
    nl,write(Thing),write(' receives_answer '),write(Answer),nl,
    retract(Thing receives_answer Answer1),
    assert(Thing receives_answer Answer).
update_answers(T,A):- T receives_answer A,!.
update_answers(T,A):-
    assert(T receives_answer A), !.

how Thing:-
    justification(Thing,Rule,Conditions),
    write(Thing),write(' was concluded from '),write(Rule),nl,
    write('with the following premises '),nl,tab11_write(Conditions),nl,nl,
    fail.

how Thing:-
    \+(in_wm(Thing)),
    write(Thing),write(' is not in working memory.'),nl.

how Thing.

why X:-
        X explained_by Text,
        'pd624 write'(Text).

chuck_it_out(Thing):-
    retract(currentdb(Thing,Truth)).
chuck_it_out(_). /* or rather not in this case! */

consistency_check(Obj,Attr,Value,no). /* don't record something that's not
                    the case */
consistency_check(Obj,Attr,Value,Answer):-
    fetch(Obj,Attr,Value1,[Obj],_),
    (Value1 = Value;
    writel(['Warning: in the knowledge-base is the information that',
        Attr,' of ',Obj,' is ',Value1,nl,
        'However this is going to be overwritten by the following',nl,
                Attr,' of ',Obj,' is ',Value,nl]),
        store(Obj,Attr,Value)).
consistency_check(A,B,C,_):-store(A,B,C).

/* positive answers and translation */
standardise_answers(yes,yes).
standardise_answers(y,yes).
standardise_answers(ok,yes).
standardise_answers(true,yes).
standardise_answers(ye,yes).  /* fake escape recognition!!! */

/* negative answers and their translation */
standardise_answers(no,no).
standardise_answers(n,no).
standardise_answers(false,no).


/* enforce unbound answers to right-hand-side queries */
answer_vetting(C):-
                   var(C),!.
answer_vetting(_):-
  'pd624 write'(['Warning : you should not specify answers to forward',nl,
   'chaining queries.',nl]),!.  /* the cut protects 'pd624 write'!!!! */
/* file: UTIL.PL {writel etc., tracing, history, describe, strategy, show} */
/*
                            *************
                             M I K E - 2
                            *************
               Micro Interpreter for Knowledge Engineering
                  {written in Edinburgh-syntax Prolog}

MIKE: Copyright (c) 1989, 1990, 1991 The Open University (U.K.)

MIKE is intended for educational purposes, and may not
be sold as or incorporated in a commercial product without
written permission from: The Copyrights Officer, Open University,
Milton Keynes MK7 6AA, U.K.

The Open University accepts no responsibility for any legal or other
consequences which may arise directly or indirectly as a result of the
use of all or parts of the contents of this program.
*/
/* Utilities for MIKE */
/* mainly tracing and formatting information */

/* If you are porting MIKE to another PROLOG then pay particular attention
   to this file.  If the PROLOG to which you are porting has the built in
   primitive 'append' then you may well have to comment the definition of
   it in this file.  It is assumed that the PROLOG you are using has a
   built-in definition of 'abolish'.  If you are porting to LPA MacProlog
   then also remove the definition of 'kill' below.  */
/* This file is divided into three main sections:
     1.  simple utilities, such as 'pd624 member'
     2.  tracing facilities
     3.  the utilities 'describe', 'strategy', and 'show'
*/
/* =================== (1) SIMPLE UTILITIES ============================= */

kill(_). /* for compatibility with LPA MacPROLOG, dummy definition needed */

/* ---  'pd624 member'(X, [X|_]). is to avoid any name clashes with existing
         definitions of member */

'pd624 member'(X,[X|_]).
'pd624 member'(X, [_|Xs]) :-
    'pd624 member'(X, Xs).

do_just_once(X) :-
    call(X),
    !.

append([],L,L).
append([H|L1],L2,[H|L3]):-
    append(L1,L2,L3).

/* definition of union and intersection (commented out), just in case...

union([],Ys,Ys).
union([X|Xs], Ys, Zs) :-
   member(X,Ys),
   !,
   union(Xs,Ys,Zs).
union([X|Xs], Ys, [X|Zs]) :-
   union(Xs,Ys,Zs).

intersection([],Ys,[]).
intersection([X|Xs], Ys, [X|Zs]) :-
   member(X,Ys),
   !,
   intersection(Xs,Ys,Zs).
intersection([X|Xs],Ys,Zs) :-
   intersection(Xs,Ys,Zs).

*/


'pd624 subset'([],X):- nonvar(X).
'pd624 subset'([H|T],Target):-
    'pd624 member'(H,Target),
    'pd624 subset'(T,Target).

'pd624 & member'(A,A).
'pd624 & member'(A,A & _).
'pd624 & member'(A,_ & B):-
    'pd624 & member'(A,B).

'pd624 list length'([],0).
'pd624 list length'([A|List],Length):-
   'pd624 list length'(List,Length1),
   Length is Length1 + 1.

/* 'pd624 length with disjunct check' sees if there is a disjunct in the
   pattern.  If there is it will take the first disjunct (in left to right
   sequence) and it will compute the specificity in terms of the specificity
   of either side of the disjunct and then choose the highest.  All other
   disjuncts will be ignored */
'pd624 length with disjunct check'(A or B,L):-
       'pd624 length'(A,L1),
       'pd624 length'(B,L2),
       (L1 >= L2,L1 = L;L2 = L),!.
'pd624 length with disjunct check'(A,B):-
       'pd624 length'(A,B).

'pd624 length'(A or B,N):-
   'pd624 length'(A,N1),
   'pd624 length'(B,N2),
   N is N1 + N2.
'pd624 length'(_ & T,N):-
    'pd624 length'(T,N1),
    N is 1 + N1.
'pd624 length'(A,1).

/* A tailor-made 'quicksort' for triples of the form (S,I,T),
   (these are the three arguments of enabled, used elsewhere in
   this file).
   S (Switch) is an integer, and triples need to be sorted
   into ascending numerical order.  */
'pd624 sort'([],[]).
'pd624 sort'([(S,I,T)|SITs],Sorted) :-
       'pd624 split'(SITs,S,Los,His),
       'pd624 sort'(Los, SortedLos),
       'pd624 sort'(His, SortedHis),
       append(SortedLos, [(S,I,T)|SortedHis], Sorted).

'pd624 split'([],_,[],[]).
'pd624 split'([(S,I,T)|SITs],Crit,[(S,I,T)|Los],His) :-
       S < Crit,
       'pd624 split'(SITs,Crit,Los,His).
'pd624 split'([(S,I,T)|SITs],Crit,Los,[(S,I,T)|His]) :-
       S >= Crit,
       'pd624 split'(SITs,Crit,Los,His).

/* ---- writel and other output routines -------------- */
writel([]).
writel([(rule Name forward if Ifs then Thens)|Rest]):-
    write('rule '),write(Name),write(' forward '),nl,tab(6), write(' if '),
    nl,tab11_write(Ifs),nl,tab(6),write(' then '),nl,
    tab11_write(Thens),write('. '),nl,
    writel(Rest),!.
writel([(rule Name backward if Ifs then Thens)|Rest]):-
    write('rule '),write(Name),write(' backward '),nl,tab(6), write(' if '),
    nl,tab11_write(Ifs),nl,tab(6),write(' then '),nl,
    tab11_write(Thens),write('. '),nl,
    writel(Rest),!.
writel([nl|R]):-
    nl, writel(R).
writel([t/Tab|Rest]):-
    tab(Tab),
    writel(Rest).
writel([&|Rest]):-
      write(' & '),
        writel(Rest).
writel(A:[H|[]]):-
 tab(6),write(A),write(' : '),write(' ['),
 write(H),write(']').
writel(A:[H|T]):-
    tab(6),write(A),write(' : '),write(' ['),
 write(H),write(','),nl,
    write1(T), write(']').
writel(A:B):-
    tab(6),write(A),write(' : '),write(B).
writel([H|T]):-
    write(H),nl,
    writel(T).
writel((A,B)):- /* conjunct, but for MIKE this means 'with' Body */
    writel(A), write(','),nl,
    writel(B).
writel(A):-
    tab(12),write(A),nl.

write1([X]) :-
    tab(20),write(X).
write1([]).
write1([X|[]]):-
 tab(20),write(X).
write1([H|T]):-
    tab(20),write(H),write(','),
    nl,write1(T).

conj_write((A&B)) :- write(A),write(' & '),conj_write(B), !.
conj_write(X) :- write(X).

tab11_write((A or B)):-
    !,
    tab11_write(A),     nl,
    tab(6),write('or '), nl,
    tab11_write(B).
tab11_write((H & T)):-
    !,
    tab(11),write(H),write(' & '),nl,tab11_write(T).
tab11_write(H):-
    tab(11),write(H).

'pd624 write'([]).
'pd624 write'([nl|B]):-!, nl,'pd624 write'(B).
'pd624 write'([tab(A)|B]):- !,tab(A),'pd624 write'(B).
'pd624 write'([t/A|B]):- !,tab(A),'pd624 write'(B).
'pd624 write'([A|B]):-!, write(A),'pd624 write'(B).
'pd624 write'([nl]):-nl.
'pd624 write'([tab(L)]):-tab(L), ttyflush.
'pd624 write'([t/L]):-tab(L), ttyflush.
'pd624 write'([A]):- write(A), ttyflush.

'pd624 pretty list'([]).
'pd624 pretty list'([nl|B]):- !,nl,'pd624 pretty list'(B).
'pd624 pretty list'([tab(A)|B]):- !,tab(A),'pd624 pretty list'(B).
'pd624 pretty list'([t/A|B]):- !,tab(A),'pd624 pretty list'(B).
'pd624 pretty list'([A|B]):- !,tab(1),write(A),'pd624 pretty list'(B).
'pd624 pretty list'([nl]):-nl.
'pd624 pretty list'([tab(L)]):-tab(L), ttyflush.
'pd624 pretty list'([t/L]):-tab(L), ttyflush.
'pd624 pretty list'([A]):- tab(1),write(A), ttyflush.

/* ========================== (2)  T R A C I N G ======================= */
/* tracing is normally called with no arguments, in which case it prompts
   the user with a menu of choices.  With a single integer argument,
   just that option is 'toggled' (i.e. turned from off to on, or on to off).
   Optional syntax is: tracing([N1,N2,N3,...]), where N1 etc. are integers
   from 1 to 10 specifying the number of the tracing option you wish
   to 'toggle' */

tracing([]).
tracing([X|Xs]) :-  /* list of integers expected */
  tracing(X),
  tracing(Xs).
tracing(X):- change_options(X),   /* single integer expected */
 enabled(I,T,X),
 write_options([(X,I,T)]).

tracing:-            /* this is the normal usage */
   display_tracing_options,
      !,
   'pd624 write'(['Type the numbers of the option you wish to change',nl,
      ' eg. 1,2,3,4. and then a FULL STOP',nl,
      'Or quit. to exit without altering the settings',nl]),
    write('==> '),
    read(Input),
    change_options(Input),
    display_tracing_options,
    'pd624 write'(['Type',nl,'  ?- show symbols.',nl,
                  'for a reminder of what the tracing symbols mean.',nl]).


display_tracing_options :-
    pd624_own_findall((S,I,T),(enabled(I,T,S), \+ 'pd624 
member'(S,[11,12,13,14,15]))
               ,Newoptions_unsorted),
    'pd624 sort'(Newoptions_unsorted,Newoptions),
    write_options(Newoptions),nl.

change_options((A,B)):- /* conjunction of options? deal with head then tail */
     !,
  change_options(A),
        change_options(B).
change_options(9):-   /* options 13,14,15 are 'yoked' with number 9 */
    enabled(A,' disabled ',9),
    reverse_option(A,enable),
  change_options((13,14,15)), !.
change_options(9):-
 enabled(A,' enabled ',9),
    reverse_option(A,disable),
 change_options((13,14,15)), !.
change_options(A):-           /* normal case: singleton option */
    enabled(Name,' disabled ',A),
    reverse_option(Name,enable),
 !.
change_options(A):-
    enabled(Name,' enabled ',A),
    reverse_option(Name,disable),
 !.
change_options(quit):- !. /* normal way to bail out of tracing options... */
change_options(q):- !.    /* but we also allow 'q', 'exit', 'e', 'ok', 'halt'*/
change_options(exit):- !. /* as un-documented alternatives */
change_options(e):- !.
change_options(ok):- !.
change_options(halt):- !.
change_options(A) :-
    writel([A,' is an illegal option',nl,'legal options are numbers that',
                'appear in the tracing menu. ']),!.


reverse_option(Name,disable):-
    retract(enabled(Name,_,A)),
    assert(enabled(Name,' disabled ',A)),
    !.
reverse_option(Name,enable):-
    retract(enabled(Name,_,A)),
    assert(enabled(Name,' enabled ',A)),
    !.
reverse_option(A,_):-
    writel([A,' is an illegal option',nl,'legal options are numbers that',
                'appear in the tracing menu. ']),!.

turn_off_option(A):-  /* like change, but unconditionally turns off or
                         else leaves it alone if it was already off */
    enabled(Name,' enabled ',A),
    reverse_option(Name,disable).
turn_off_option(_).

write_options([]).
write_options([(Index,Item,S)|T]):-
    write_plus_or_minus(S,Index),write(Index),write(': '),write(Item),
    write(' is currently'),write(S),write('. '),nl,
    write_options(T).
write_options([A|B]):-
    write('ERROR: from write options'),
    write(A),nl,
    write_options(B).

write_plus_or_minus(' disabled ', N) :-
   write('-'), (N < 10, write(' ') ; true), !.

write_plus_or_minus(_, N) :-
   write('+'), (N < 10, write(' '); true), !.

when_enabled(P for List):-
    enabled(P,' enabled ',_ignore_the_index), /* flag enabled? */
    !,
    P for List.    /* then call P (e.g. 'show outcome for backward chain..') */
                   /* if P can be re-satisfied on backtracking, this is fine */
when_enabled(P for List) :-
    enabled(P,' disabled ',_), !. /* i.e. do nothing if flag disabled */
when_enabled(X) :-
   writel(['Warning: when_enabled/1 has been passed an unexpected argument:',
          X,nl,'  Only the authorised tracing flags are allowed!',
   'These are the ones displayed when you type:  ?- tracing.',
   '[Succeeding anyway, which may cause extra solutions to be found!]']).

/* enabled/3 is just a database of flags, using the following 3 arguments:
   1  The name of the option
   2  Its current state, either ' enabled ' or ' disabled '
   3  An integer indicating its position in the tracing multiple-choice menu.
*/
enabled('show conflict set',' disabled ',1).
enabled('show refractoriness',' disabled ',2).
enabled('show specificity',' disabled ',3).
enabled('show recency',' disabled ',4).
enabled('show new working memory elements or frame changes',' disabled ',5).
enabled('show chosen rule',' disabled ',6).
enabled('show backward chaining',' disabled ',7).
enabled('show outcome of backward chaining',' disabled ',8).
enabled('show single stepping',' disabled ',9).
enabled('show history on request',' enabled ',10).

/* the next five options are set internally at run-time, and are not
   meant to be settable by the user!! */

enabled('show individual LHS in', ' disabled ',11).
enabled('show individual LHS out', ' disabled ',12).
enabled('show single stepping in', ' disabled ',13).
enabled('show single stepping out', ' disabled ',14).
enabled('show conflict winner', ' disabled ',15).

'show outcome of backward chaining' for P/Depth:-  /* success */
      enabled('show backward chaining',' enabled ',7),
         write('<- '),tab(Depth),write('+ '),conj_write(P),nl.
'show outcome of backward chaining' for P/Depth:-  /* only come here on retry */
         write('<- '),tab(Depth), write('^ '),conj_write(P),nl,
      !,
      fail.  /* because we need to propagate failure back to older sibling */
'show backward chaining' for P/Depth:-  /* goal invocation */
  do_just_once((Depth = 0, nl ; true)), /* extra newline only for first call */
  write('<- '),tab(Depth),write('? '),conj_write(P),nl.
'show conflict set' for P:-
    nl,write('Conflict Set is: '),nl,
    writel(P).
'show refractoriness' for P :-
    nl,write('Refractoriness filter threw out the following rule: '),nl,
    writel(P).
'show specificity' for P:-
    nl,write('Conflict set AFTER specificity filter is: '), nl,
    writel(P).
'show new working memory elements or frame changes' for P :-
    nl,write('New working memory elements or frame changes are: '), nl,
    writel(P).
'show recency' for P:-
    nl,write('Conflict set AFTER recency filter is: '), nl,
    writel(P).
'show chosen rule' for P:-
    nl,write('Chosen rule is: '), nl,
    writel(P).

'show individual LHS in' for X :-
              write('-LHS-> '),write('? '),write(X),
              pd624_read_loop,
              !.

'show individual LHS out' for X :-
              write('-LHS-> '),write('+ '), write(X),nl,
              !.

'show single stepping in' for X :-
              turn_off_option(11),   /* kill creeping in, regardless */
              turn_off_option(12),  /* kill creeping out, regardless */
              nl,write('-> ? '),write(X),ulnl,
              pd624_read_loop,
              !.      /* because embedded within a findall */

'show single stepping out' for X :-
              write('   +'),   /* many instantiations may win here! */
              !.

'show conflict winner' for X:-
              nl,write('-> * '),write(X),write(' ************'),
              ulnl,
              pd624_read_loop,
              !.

'show history on request' for [RuleName,Symbol] :-
      'pd624 current cycle is'(CycleNum),
       assertz('pd624 fc_history'(RuleName,CycleNum,Symbol)),
       !.

/* ------------ Forward chaining history display ------------------------ */
history(_,_) :-
  enabled('show history on request',' disabled ',_),
  !,
  writel([
  'To see the history of execution in tabular form, you must FIRST',
  'ensure that the tracing option called',
  '    ''show history on request''      ',
  'has been enabled (this is necessary because the history has to be',
  'stored during execution).',
  'To enable the relevant option, you can either type',
  '    ?- tracing(10).',
  'or else',
  '    ?- tracing.',
  'and then respond appropriately to the menu of choices.',nl,
  'When you have set your options correctly, you can then reinvoke',
  '    ?- fc.',
  'and then type',
  '    ?- show history.',
  'when execution has completed.']).

history(beginning, end) :-  /* default case passed in from ?- show history. */
   'pd624 current cycle is'(SoFar),
   SoFar > 45,
   'pd624 write'([
     'The highest cycle number reached on the preceding run was: ',SoFar,nl,
 'You can display the history for any contiguous group of up to 45 cycles',nl,
 'by entering a modified version of the show history command.',nl,
 'For example, to see cycles 30 to 55, say, you would type in', nl,
 '  ?- show history/30-55.',nl]).

history(beginning, end) :-   /* constants are deliberate 'flags' */
   'pd624 current cycle is'(SoFar),
   SoFar < 46,
   SoFar > 0,
   history(1,SoFar),  /* this invokes the 'legitimate' output below */
   !.

history(beginning, end) :-   /* constants are deliberate 'flags' */
   'pd624 current cycle is'(0),
   nl,
   write('No history to show you yet!  Try: ?- fc.'),
   nl,
   !.

history(Lo,Hi) :-
   integer(Lo),
   integer(Hi),
   Diff is Hi - Lo,
   Diff > 44,
   'pd624 current cycle is'(SoFar),
   'pd624 write'(['Sorry, you can only display 45 cycles at a time.',nl,
 'Try   ?- show history.  to see the first 45 cycles,',nl,
 'or    ?- show history/40-85. to see cycles 40 to 85, etc.',nl,
 'The highest cycle number reached on the preceding run was: ',SoFar,nl]),
   !,
   fail.

history(Lo, Hi) :-
   integer(Lo),
   integer(Hi),
   !,
   'pd624 write'([nl,'  RULE NAME                           CYCLE NUMBER(',
                 Lo,'-',Hi,')',nl]),
   'pd624 generate listofnums'(Lo,Hi,WholeList),
   tab(30),posh_dots(WholeList),nl,
   pd624_own_findall(Name,((rule Name forward if X then Y)),AllNames),
   'pd624 fc_history display'(AllNames,WholeList).

history(A,B) :-
   'pd624 write'(['Sorry, only integer values are allowed, e.g.', nl,
                  '  ?- show history/40-85. ', nl]),
   !,
   fail.

posh_dots([]).
posh_dots([N|Ns]) :-
   posh_symbol(N,Sym),
   write(Sym),
   posh_dots(Ns).

posh_symbol(N, Int) :-
  0 is N mod 10,  /* multiple of 10?  then use integer from 1 to 9  */
  !,
  posh_truncate(N, Int).

posh_symbol(N, ':') :-  /* use : for multiples of 5, e.g.  ....:....1....: */
  0 is N mod 5,
  !.

posh_symbol(N, '.').  /* default case... just use a dot */

posh_truncate(N, Int) :-
    Temp is N//10,
    Int is Temp mod 10.

'pd624 fc_history display'([Rule|Rules],NumList) :-
    'pd624 show name'(Rule),
    'pd624 fc_history gimme one line'(Rule, NumList),
    'pd624 fc_history display'(Rules,NumList).

'pd624 fc_history display'([],_) :- nl.  /* termination */

'pd624 show name'(RuleName) :-
       'pd624 string length'(RuleName,Len),
       'pd624 maybe truncate name'(RuleName,Len).

'pd624 maybe truncate name'(Name,Len) :-
       Len > 30,                /* very long name? */
       write(Name),nl,tab(30).  /* then insert <CR>, tab across */

'pd624 maybe truncate name'(Name,Len) :-
       Len =< 30,              /* name length < 30 chars?  OK... */
       Remainder is 30 - Len,  /* cause fc_history stuff starts at column 30 */
       write(Name), tab(Remainder). /* write it out, tab the rest */

/* COMPATIBILITY NOTE: THE THREE CLAUSES WHICH FOLLOW INCLUDE A (WASTEFUL)
DIALECT-SPECIFIC SUBGOAL IN CLAUSE 2 (exporter...), TO DECIDE WHICH
VERSION OF 'name/2' TO USE: ESL PROLOG-2 vs. C-PROLOG.  IF YOU KNOW
WHICH DIALECT YOU ARE GOING TO STICK WITH, THEN YOU CAN SIMPLIFY
THINGS BY DELETING THE CLAUSE OF 'pd624 string length' THAT YOU
DON'T NEED.  IF YOU ARE STICKING WITH ESL Prolog-2 (on PC, Sun,
VAX, etc.), THEN DELETE CLAUSE 3 OF 'pd624 string length', AND ALSO
DELETE THE LINE IN CLAUSE 2 CONTAINING exporter.  IF YOU ARE USING MOST OTHER
DIALECTS OF PROLOG, THEN DELETE CLAUSE 2 OF 'pd624 string length'.
FOR ALL DIALECTS, LEAVE CLAUSE 1 ALONE!
*/

/* clause 1 ... useful for all dialects */
'pd624 string length'(Atom, Length) :-
       integer(Atom),       /* can only happen if you use rule name like 1 */
       !,
       'pd624 power_of_10'(Atom,Power),  /* e.g. 124 is 3 (3 digits) */
       Length is Power + 1 .

/* clause 2 ... the one with the wasteful test */
/*'pd624 string length'(Atom,Length):-     /* ESL PROLOG-2 VERSION */
/*        exporter(prolog_start_goal/0, _),/* DIALECT-SPECIFIC (WASTEFUL) TEST*/
/*        !,                               /* if we get this far... */
/*        name(Atom,String),               /* then use ESL PROLOG-2 'name' */
/*        list(List,String),              /* which can be unpacked into list */
/*        'pd624 list length'(List,Length). */

/* clause 3 ... relevant for non-ESL-PROLOG2 dialects */
'pd624 string length'(Atom, Length) :-  /* C-Prolog, Quintus, etc. version */
       name(Atom, List),
       'pd624 list length'(List, Length).

'pd624 power_of_10'(X,0) :- X < 10, !.
'pd624 power_of_10'(X,1) :- X < 100, !.
'pd624 power_of_10'(X,2) :- X < 1000, !.
'pd624 power_of_10'(X,3) :- X < 10000, !.
'pd624 power_of_10'(X,4) :- X < 100000, !.
'pd624 power_of_10'(X,5). /* This means that a rule named 123456789 will
                             have a fixed string length of 6.  This
                             will cause ?- show history. to print a
                             slightly messed up chart.  The solution
                             is to use numbers < 999999 for rule names! */


'pd624 fc_history gimme one line'(Rule,[]) :- nl.
'pd624 fc_history gimme one line'(Rule,[Num|Rest]) :-
        /* if you have a symbol stored, write it out, else write ' ' */
        'pd624 get best symbol'(Rule,Num,Sym),
        write(Sym),
        'pd624 fc_history gimme one line'(Rule,Rest).

'pd624 get best symbol'(Rule,Num,'*') :-   /* strict priority sequence */
        'pd624 fc_history'(Rule,Num,'*'),
        !.

'pd624 get best symbol'(Rule,Num,'+') :-
       'pd624 fc_history'(Rule,Num,'+'),
       !.

'pd624 get best symbol'(Rule,Num,' '). /* no symbol, use blank */

'pd624 generate listofnums'(X,Hi,[0]) :-
       X > Hi,
       write('Sorry, can only work with an ascending sequence of integers.'),
       nl,
       !,
       fail.

'pd624 generate listofnums'(Hi,Hi,[Hi]) :- !.
'pd624 generate listofnums'(Lo,Hi,[Lo|Rest]) :-
       Next is Lo + 1,
       'pd624 generate listofnums'(Next,Hi,Rest).


/* ------------ handler for user input to single-stepper -------------- */

pd624_flag(dummy).  /* for MacProlog-like dialects & POPLOG, requiring 1 */

pd624_read_loop :-
               \+(pd624_flag(unleashed)),
               get0(Char),
               ((\+(Char = 13), get0(NextChar)) ; true),
               pd624_deal_with(Char), !.

pd624_read_loop :-
               pd624_flag(unleashed).

ulnl :-  /* unleashed new line */
     pd624_flag(unleashed),
     nl.

ulnl.

pd624_deal_with(97) :-  /* a for abort (does not really... ) */
                   change_options(9),
                   add halt. /* i.e. switch off stepper */
pd624_deal_with(110) :- /* n for no-tracing */
                   change_options(9). /* i.e. switch off stepper */
pd624_deal_with(13).   /* <CR> */
pd624_deal_with(98) :-  /* b for break */
                   repeat,
                   nl,write('MIKE ?- '),
                   read(INPUT),
                   (INPUT = quit;
                    (do_just_once(call(INPUT)),
                     do_just_once((write(INPUT);write(no))),
                     fail) ).

pd624_deal_with(99) :- /* c for creep through left hand side conditions */
         change_options(11),
         change_options(12). /* toggles LHS creeping */

pd624_deal_with(117) :-  /* u for unleash */
                    assert(pd624_flag(unleashed)).


pd624_deal_with(HelpChar) :-
                    (HelpChar = 63 ; HelpChar = 104),
                    nl,nl,
                    write('a(bort at end of the current interpreter cycle)'),nl,
                    write('b(reak until quit)'),nl,
                    write('c(reep through left-hand-side conditions)'),nl,
                    write('h(elp)'),nl,
                    write('n(o more tracing)'),nl,
                    write('u(nleash)'),nl,
                    write('<CR> = step'),nl,
                    pd624_read_loop.

pd624_deal_with(_).



/* ======= (3)  D E S C R I B E ,  S T R A T E G Y , and  S H O W ====== */
describe A:-
    (A  instance_of Object with Body),
    write(A instance_of Object),
    write(' with '),
    nl,writel(Body), write('.'), nl.
describe A:-
    (A  subclass_of Object with Body),
    write(A subclass_of Object),
    write(' with '),
    nl,writel(Body), write('.'), nl.
describe A:-
 (rule A forward if Ifs then Thens),
 writel([(rule A forward if Ifs then Thens)]).
describe A:-
 (rule A backward if Ifs then Thens),
 writel([(rule A backward if Ifs then Thens)]).

strategy menu:-
 current_conflict_resolution_strategy(List),
 writel(['The current conflict resolution strategy is ',List,
  nl,'To change it, type the numbers that correspond to',
  'the ordering that you want from the following menu ',
  ' e.g. if you want the ordering to be specificity, recency, refractoriness',
  'then type 3,2,1.  <remember the FULL STOP!>',
  '1 - refractoriness  (same instantiation cannot fire twice)',
  '2 - recency         (prefers rules whose LHS patterns were just added)',
  '3 - specificity     (prefers rules with more conditions on LHS)',
  '4 - lhsthreshold    (WM patterns weaker than threshold get overlooked)',
  '5 - cfstrength      (prefers rules with high cf value)'
  ]),
  write('==>'),read(P),sort_out_options(P,List1),
  retract(current_conflict_resolution_strategy(_)),
  assert(current_conflict_resolution_strategy(List1)),
  writel(['Ok, the new strategy is now ',List1,nl]).

/* WARNING: THE NEXT CLAUSE DOES NOT INSPECT List TO MAKE SURE THAT
   THE USER HAS PUT IN SOMETHING SENSIBLE!!! */
strategy List:-
    retract(current_conflict_resolution_strategy(P)),
    assert(current_conflict_resolution_strategy(List)).

sort_out_options((A,B),[H|T]):-  !,
    'pd624 member'((A,H),[(1,refractoriness),(2,recency),(3,specificity),
(4,lhsthreshold),(5,cfstrength)]),
    sort_out_options(B,T).

sort_out_options(A,[H]):-
    'pd624 member'((A,H),[(1,refractoriness),(2,recency),(3,specificity),
(4,lhsthreshold),(5,cfstrength)]).

/* ---------------- the show facility ------------------------- */

show history/Lo-Hi :-
     history(Lo,Hi).

show history:-
     history(beginning, end). /* this provides default output (see above) */

show wm :-
     wm.
show rules:-
    'pd624 write'(['The currently loaded ruleset is ',
    nl,'the following : ',nl]),
    assert('wm counter'(0)),
    ((rule X forward if _ then _);
     (rule X backward if _ then _)),
    do_just_once(('pd624 write'([t/5,X,nl]),
                 retract('wm counter'(P)),
                 New is P + 1,
                 assert('wm counter'(New)))),
    fail.
show rules:-
    retract('wm counter'(Number)),
    'pd624 write'([nl,'A total of ',Number,
     ' rules were found.',nl]).

show frames:-
    'pd624 write'(['The currently loaded frames are ',
    nl,'the following : ',nl]),
    assert('wm counter'(0)),
    ((A subclass_of _ with _X);
     (A instance_of _ with _)),
    do_just_once(('pd624 write'([t/5,A,nl]),
                  retract('wm counter'(P)),
                  New is P + 1,
                  assert('wm counter'(New)))),
    fail.
show frames:-
    retract('wm counter'(Number)),
    'pd624 write'([nl,'A total of ',Number,
     ' frames were found.',nl]).

show symbols :-
   writel([
'SINGLE-STEP SYMBOL  |     MEANING (ASSUMES TRACING OPTION 9 IS SELECTED)',
'->                     Forward chaining taking place',
'-> ? <rule-name>       Considering this rule on forward chaining cycle',
'   +                   The above (just-considered) rule enters conflict set',
'   +   +   +   +       Four instantiations of above rule enter conflict set',
'-> * <rule-name>       This rule alone has been selected for firing',
'-LHS-> ? <pattern>     Considering this Left-Hand-Side pattern',
'-LHS-> + <pattern>     This Left-Hand-Side pattern matched successfully',
'<-                     Backward chaining taking place',
'<- ? <pattern>         Trying to deduce this pattern',
'<- + <pattern>         Pattern deduced successfully',
'<- - <pattern>         Failed to deduce this pattern',
'<- ^ <pattern>         Backtrack to find alternative proof for pattern',
'When the single-step tracer pauses you can type one of the following letters:',
' a(bort at end of the current interpreter cycle) - soon bails out',
' b(reak until quit) - invokes Prolog interpreter until you type ?- quit.',
' c(reep through left-hand-side conditions) - fine-grained -LHS-> trace',
' h(elp) - reminder of these symbols.   ?  has the same effect.',
' n(o more tracing) - suppresses extensive printout.',
' u(nleash) - no more pausing at each step, let loose extensive printout',
' <CR> = step through each rule (or LHS condition) as encountered.' ]).
/* file: FINDALL.PL {hand-coded findall} */
/*
                            *************
                             M I K E - 2
                            *************
               Micro Interpreter for Knowledge Engineering
                  {written in Edinburgh-syntax Prolog}

MIKE: Copyright (c) 1989, 1990, 1991 The Open University (U.K.)

MIKE is intended for educational purposes, and may not
be sold as or incorporated in a commercial product without
written permission from: The Copyrights Officer, Open University,
Milton Keynes MK7 6AA, U.K.

The Open University accepts no responsibility for any legal or other
consequences which may arise directly or indirectly as a result of the
use of all or parts of the contents of this program.
*/
/* pd624_own_findall/3 is not provided as a system primitive in some dialects,
   so this file contains a hand-coded definition.

   findall1/3 is the analogous, except duplicates are removed
*/

/* unique stack-markers are needed in case you nest findall within findall */

unique_pd624_symbol(sym1).
unique_pd624_symbol(sym2).
unique_pd624_symbol(sym3).
unique_pd624_symbol(sym4).
unique_pd624_symbol(sym5).
unique_pd624_symbol(sym6).
unique_pd624_symbol(sym7).
unique_pd624_symbol(sym8).
unique_pd624_symbol(sym9).
unique_pd624_symbol(sym10).
unique_pd624_symbol(sym11).
unique_pd624_symbol(sym12).
unique_pd624_symbol(sym13).
unique_pd624_symbol(sym14).
unique_pd624_symbol(sym15).
unique_pd624_symbol(sym16).
unique_pd624_symbol(sym17).
unique_pd624_symbol(sym18).
unique_pd624_symbol(sym19).
unique_pd624_symbol(sym20).



pd624_own_findall(X, Goal, Xlist) :-
    retract(unique_pd624_symbol(Sym)),
    !,
    (call(Goal),
    assertz(stack(Sym,X)),
    fail;
    assertz(stack(Sym,bottom))),
    collect(Sym,Xlist),
    assert(unique_pd624_symbol(Sym)).

findall1(X, Goal, Xlist) :-
    retract(unique_pd624_symbol(Sym)),
    !,
    (call(Goal),
    do_just_once((stack(Sym,X);assertz(stack(Sym,X)))),
    fail;
    assertz(stack(Sym,bottom))),
    collect(Sym,Xlist),
    assert(unique_pd624_symbol(Sym)).

collect(Sym,L) :-
    retract(stack(Sym,X)), !,
    (X == bottom, !, L = [];
    L = [X | Rest], collect(Sym,Rest)).
/* file: BROWSE.PL {hierarchical frame browsing utility} */
/*
                            *************
                             M I K E - 2
                            *************
               Micro Interpreter for Knowledge Engineering
                  {written in Edinburgh-syntax Prolog}

MIKE: Copyright (c) 1989, 1990, 1991 The Open University (U.K.)

MIKE is intended for educational purposes, and may not
be sold as or incorporated in a commercial product without
written permission from: The Copyrights Officer, Open University,
Milton Keynes MK7 6AA, U.K.

The Open University accepts no responsibility for any legal or other
consequences which may arise directly or indirectly as a result of the
use of all or parts of the contents of this program.
*/
/*
A hierarchical frame browsing utility, that shows graphically your frame
hierarchy.  It is more fully documented in the accompanying READ.ME file */

browse :-   /* PATCH 19 JAN: Z^ & Z1^ critical (_ nogood in some Prologs) */
 'pd624 setof1'(X, Y^Z^(Y subclass_of X, \+(X subclass_of Z)), RootNodes1),
 'pd624 setof1'(X1, Y1^Z1^(Y1 instance_of X1,         /* PATCH 16 JAN '90 */
                        \+(X1 subclass_of Z1),
                        \+('pd624 member'(X1,RootNodes1))
                       ),
               RootNodes2),
/*  pd624_own_findall(Obj, Obj instance_of 'Newly Created Object', Loners), */
  append(RootNodes1,RootNodes2,RootNodes),
/*  append(Loners, RootNodes, AllRootNodes), */
  treewrite(RootNodes, 0).    /* PATCH 20 JAN '90: 2nd setof finds NCO */

browse(N) :-
  treewrite([N], 0).

treewrite([], _).
treewrite([X|Xs], Indentation) :-
   nl, tab(Indentation), write(X),
   get_children(X, Kids),
   NewIndent is Indentation + 2,
   treewrite(Kids, NewIndent),
   treewrite(Xs, Indentation).

get_children(Node, Kids) :-
        pd624_own_findall(X, X subclass_of Node, Group1),
        pd624_own_findall(Y, Y instance_of Node, Group2),
        append(Group1, Group2, Kids).

'pd624 setof1'(Var, Goals, Ans) :-  /* PATCH 16 JAN 90 */
         setof(Var, Goals, Ans),
         !.
'pd624 setof1'(_,_,[]).

/* KB LOADING UTILITY */
/* New with version 1.3 */
/* Custom initialise pasted in to speed up RETE killing 5-NOV-90 */

'banner line' :-    /* Patch 12-DEC-90: now prints only 77 chars */
  nl,
  write(
'----------------------------------------------------------------------------'
        ).

kb X :-  /* load new knowledge base and print out summary information */
    assert(kb(X)),
    fc_reset_history,      /* reset history counters (see fc_exec.pl) */
    abolish(currentdb,2),  /* the relation 'currentdb/2' stores all WM items */
    kill(currentdb),  /* just for portability */
    abolish(already_did,2), /* used for quick refractoriness test */
    kill(already_did),
    assert(already_did(nil,nil)),
                          /* need some assertion to avoid run-time complaint */
    abolish('pd624 wme',1),    /* otherwise we end up in a curious state?!!! */
    kill('pd624 wme'),
    abolish(receives_answer,2),
    abolish(justification,3),
    assert('pd624 wme'([])),
    (retract(pd624_flag(_)) ; true),
                      /* Used for single-step trace. See UTIL.PL */
    initialise_back_door,  /* in case of later extensions ! */
    fc_reset_history, /* clear up forward-chaining history storage */
    abolish((rule),1),  /* get rid of any current rules */
    abolish((with), 2), /* get rid of any current frames */
    reconsult(X),     /* if file/pathname is wrong, reconsult complains */
    /* the above line overwrites old frames and rules, but the preceding
       calls of 'abolish' are necessary in case the new file happens
       not to have one or the other (i.e. frames or rules) within it */
    nl,
    write('New knowledge base loaded.'),
/* PATCH 9-OCT-90: Only show NUMBER of frames, rules, WM elements,
rather than listing them all
PATCH 7-NOV-90: Now does this by calling status/0 (in FC_EXEC.PL!) */
    retract(memkb(_)),
    assert(memkb(X)),
    status,
    rete_compile.

memkb('none').   /* Default currently loaded kb is 'None' */
/* WORLDS.PL:  Basic hypothetical worlds machinery.
   Works with any working memory pattern,
   but NOT with MIKE frames!!!!   Examples are in the KBs called
   RNWORLD.KB and STRWORLD.KB
*/
/*
                            *************
                             M I K E - 2
                            *************
               Micro Interpreter for Knowledge Engineering
                  {written in Edinburgh-syntax Prolog}

MIKE: Copyright (c) 1989, 1990, 1991 The Open University (U.K.)

MIKE is intended for educational purposes, and may not
be sold as or incorporated in a commercial product without
written permission from: The Copyrights Officer, Open University,
Milton Keynes MK7 6AA, U.K.

The Open University accepts no responsibility for any legal or other
consequences which may arise directly or indirectly as a result of the
use of all or parts of the contents of this program.
*/

/* Pure Prolog operator declarations */
:- op(1199,xfy, special).
:- op(950,fx, suppose).
:- op(950,fx, unsuppose).
:- op(945,xfx, in_world).
:- op(945,xfx, in_worlds).
:- op(945,xfx, in_merged_worlds).
:- op(940,fx, new_world).
:- op(940,xfx, is_new_child_world_of).
:- op(940,xfx, is_child_world_of).
:- op(940,xfx, in_new_child_world_of).
:- op(950,fx, wdescribe).

/* Back-door flag-setting to allow some of the above into MIKE */
:- allow_prolog_rhs(suppose Pattern, [Pattern]). /* 2nd arg->list 12/12/90 */
:- allow_prolog_rhs(unsuppose _).
:- allow_prolog_rhs(new_world _).
:- allow_prolog_rhs(X is_new_child_world_of _).
:- allow_prolog_rhs(clearworlds).
:- allow_prolog_lhs(_ in_world _).
:- allow_prolog_rhs(wdescribe _).

/* Next line ensures that 'clearworlds' is automatically invoked
  by the generic 'initialise' code within 'fc' */
:- allow_back_door_initialise(clearworlds).

/* Definitions */

worldcount(0).

clearworlds :-
   abolish(is_child_world_of, 2),
   abolish(worldcount,1),
   assert(worldcount(0)),
   retractall(currentdb(W:Pat,_)).

(X&Y) in_worlds Ws :-  /* find all the worlds in which conjunction holds */
   !,
   X in_worlds W1s,
   Y in_worlds W2s,
   pd624intersection(W1s,W2s,Ws).

(-- X) in_worlds Ws :-
   !,
   pd624_own_findall(W, ((-- X) in_world W), Ws).

X in_worlds Ws :-
   pd624_own_findall(W, currentdb(W:X, _), Ws).

(X & Y) in_world W :-  /* find HIGHEST world in which conjunction holds */
   !,
   (X & Y) in_worlds Ws,
   max_world(Ws,W).

X in_world global :-
   currentdb(X, _).

(-- X) in_world W :-   /* X is NOT the case in some world W */
   nonvar(W),
   !,
   \+(currentdb(W:X, _)).

(-- X) in_world W :-
   !,
   all_worlds(AllWorlds),  /* use this to generate some binding for W */
   'pd624 member'(W, AllWorlds),  /* find some world */
   \+(currentdb(W:X, _)).     /* in which the pattern isn't there! */

X in_world W :-        /* finds X in LOWEST world W */
   currentdb(W:X, _),
   \+(found_in_higher_world(X,W,Higher)). /* prevents extra instantiations */

found_in_higher_world(Fact,World,Higher) :-
   currentdb(Higher:Fact, _),
   ancestor_world(Higher,World).

all_worlds([global|Ws]) :-
   pd624_own_findall(W, W is_child_world_of _, Ws).

ancestor_world(Old, Young) :-
   Young is_child_world_of Old.

ancestor_world(Old, Young) :-
   Young is_child_world_of Temp,
   ancestor_world(Old, Temp).

pd624intersection([],Ys,[]).
pd624intersection([X|Xs], Ys, [X|Zs]) :-
   'pd624 member'(X,Ys),
   !,
   pd624intersection(Xs,Ys,Zs).
pd624intersection([X|Xs],Ys,Zs) :-
   pd624intersection(Xs,Ys,Zs).

max_world([W],W).
max_world([W1|Ws], Result) :-
  max_world(Ws, Temp),
  ((ancestor_world(W1,Temp), Result=W1) ; Result=Temp),
  !.


new_world X :-
   X is_new_child_world_of global.

world-N1 is_new_child_world_of SomeWorld :-
   retract(worldcount(N)),
   N1 is N + 1,
   assert(worldcount(N1)),
   assert(world-N1 is_child_world_of SomeWorld),
   copy_wm(SomeWorld,world-N1).


copy_wm(global,Child) :- /* special handling for global... */
   !,
   pd624_own_findall(Pattern/CF,
                   /* any old 'top level' patterns WITHOUT a World: label */
            ( ( currentdb(Pattern, CF), \+(Pattern=(A:B)) );
              currentdb(global:Pattern, CF) /* or ones with global: label */
            ),
           Assertions),
   assert_all_patterns(Assertions, Child).

copy_wm(Parent,Child) :-
   pd624_own_findall(Pattern/CF, currentdb(Parent:Pattern, CF), Assertions),
   assert_all_patterns(Assertions, Child).

assert_all_patterns([],_).
assert_all_patterns([Pat/CF|Pats], World) :-
  /* if it is already present, leave it alone... */
  currentdb(World:Pat, _),
  !,
  assert_all_patterns(Pats,World).

assert_all_patterns([Pat/CF|Pats], World) :-
  /* ... else assert afresh */
  assertz(currentdb(World:Pat, CF)),
  assert_all_patterns(Pats,World).

combine_worlds((X&Y), New) :-
  New is_new_child_world_of X,
  duplicate_worlds(Y,New).

duplicate_worlds((Parent1&Parents), New) :-
  !,
  assert(New is_child_world_of Parent1),
  copy_wm(Parent1, New),
  duplicate_worlds(Parents, New).

duplicate_worlds(SingleParent, New) :-
  assert(New is_child_world_of SingleParent),
  copy_wm(SingleParent, New).

suppose X in_merged_worlds (A&B) :-
  !,
  combine_worlds((A&B), New),
  suppose X in_world New.

suppose (A&B) in_world W :-
  !,
  suppose A in_world W,
  suppose B in_world W.

suppose Pattern in_world W cf CF:-
  !,
  update_wm(W:Pattern,CF),
  already_did(Rule, Conds),
  assert(justification(W:Pattern, Rule, Conds)),
  tms_trace(W:Pattern,Rule,'added to'),   /* notify RETE and TMS */
  addrete(W:Pattern),
  impossibility_analysis(W),
  world_contradiction_test(Pattern,W).

suppose Pattern cf CF in_world W:-
  !,
  update_wm(W:Pattern,CF),
  already_did(Rule, Conds),
  assert(justification(W:Pattern, Rule, Conds)),
  tms_trace(W:Pattern,Rule,'added to'),   /* notify RETE and TMS */
  addrete(W:Pattern),
  impossibility_analysis(W),
  world_contradiction_test(Pattern,W).

suppose Pattern in_world W :-
  !,
  already_did(Rule, Conds),  /* db has just stored ?self */
  perform1(add W:Pattern, _, Rule, Conds),
  impossibility_analysis(W),
  world_contradiction_test(Pattern,W).

suppose X in_new_child_world_of W :-
   !,
   NewWorld is_new_child_world_of W,
   suppose X in_world NewWorld.

suppose X :- suppose X in_world global.

unsuppose (A&B) in_world W :-
  !,
  unsuppose A in_world W,
  unsuppose B in_world W.

unsuppose X in_world W :-
  !,
  remove W:X,
  impossibility_analysis(W).

unsuppose X in_new_child_world_of W :-
   !,
   NewWorld is_new_child_world_of W,
   unsuppose X in_world NewWorld.

unsuppose X :- unsuppose X in_world global.

reverse_wm_pattern(-- X, X) :- !.
reverse_wm_pattern(X, --X).

world_contradiction_test(Singleton,World) :-
   /* simple case.. direct clash */
   currentdb(W:(-- Singleton), true),  /* is  Singleton known to be 'out' */
   !,
   resolve_clash(Singleton,(-- Singleton),World,direct).

world_contradiction_test(Singleton,World) :-
   reverse_wm_pattern(Singleton, Rev),
   ( (already_did(Rule,Conds in_world World), RelevantWorld = World) ;
        (already_did(Rule,Conds in_world AnyWorld),
         ancestor_world(AnyWorld, World),
         RelevantWorld = AnyWorld)
   ),
   'pd624 & member'(Rev, Conds),
   (rule Rule forward if (Conds in_world RelevantWorld) then OldConcs),
   !,
   resolve_clash(Singleton,(-- Singleton),World,
     (rule Rule forward if (Conds in_world RelevantWorld) then OldConcs)
    ).

world_contradiction_test(_,_).  /* must have passed inconsistency test */


resolve_clash(Pat,Rev,World,direct) :-
   nl, write('*** FOUND A DIRECT CLASH IN WORLD '),write(World),nl,
   write(Pat),nl,write(Rev),nl.

resolve_clash(Singleton,(-- Singleton),World,
     (rule Rule forward if (Conds in_world RelevantWorld) then OldConcs)) :-
   nl, write('*** FOUND A CLASH IN WORLD '),write(World),
   nl,write(' emanating from rule '),write(Rule),write(' in world '),
   write(RelevantWorld), nl,
   write(Singleton), write(' vs. '), write(-- Singleton), nl,
   writel([(rule Rule forward if (Conds in_world RelevantWorld)
                              then OldConcs)]).


/* 'Worlds Browser': pretty prints spawned world hierarchy */
/* assumes RHS of rules use one of these two forms:
        suppose <pat> in_world W
        suppose <pat> in_new_child_world_of W */

impossibility_analysis(World) :-
  (rule IRule special if (Pats in_world World) then impossible),
  all_in_world(Pats, World),
  !,
  warn_user_of_impossibility(IRule, Pats, World).
impossibility_analysis(_).  /* if not impossible, then let it succeed! */

all_in_world((Pat & Pats),World) :-
  !,
  currentdb(World:Pat, _),
  all_in_world(Pats,World).

all_in_world(Pat, World) :- /* singleton case */
  currentdb(World:Pat, _).

warn_user_of_impossibility(IRule,Pats,World) :-
   nl,
   write('******************** WARNING **************************'),nl,
   write('An impossibility has been detected in world '),write(World),
   nl,write('by the following rule instantiation:'),nl,
   writel([(rule IRule special if Pats then impossible)]),
   nl, write('Killing impossible world '), write(World), nl,
   write('*******************************************************'),nl,
   retractall(currentdb(World:Assertion, _)), /* kill assertions */
   assertz(currentb(World:impossible, true)).  /* leave reminder */

worlds :-
  worlds(global).

worlds(N) :-
  wtreewrite([N], 0).

wtreewrite([], _).
wtreewrite([X|Xs], Indentation) :-
   nl, tab(Indentation), write(X),
   pd624_own_findall(Kid, Kid is_child_world_of X, Kids),
   NewIndent is Indentation + 2,
   wtreewrite(Kids, NewIndent),
   wtreewrite(Xs, Indentation).

wdescribe(World) :-
   pd624_own_findall(X, currentdb(World:X, _), Pats),
   nl, write('The following assertions hold in world '), write(World),
   write(':'),nl,
   writel(Pats).

/* COMPATIBILITY NOTE ... some dialects may object to the following
  run-time addition of clauses to the definitions of justify/1 and how/1,
  and may require them to be declared 'dynamic'
*/

/* The following two load-time assertions are needed for
  how and justify to work with worlds */

:- asserta((   justify Pattern:-
      justification(W:Pattern,Rule,Conds),
      justify W:Pattern)).

:- asserta((   how Pattern:-
      justification(W:Pattern,Rule,Conds),
      how W:Pattern)).
/* XTEND.PL --- User Defined Customisations & Extensions to MIKE */
/* N.B. THIS FILE SUPERSEDES BOTH MIKE.INI AND EXTRAS.PL (USED IN v1.50)
   Also, definitions of ed, qed, and ked to invoke editor are in AUTO.PRO */

/* Contains:
   a) Extensions to MIKE to allow Prolog operators >, <, etc.
   b) definitions of benchmark/timing utility called 'bm'
   c) definitions of ask_menu and forall (used to be in extras.pl)
*/
/*
                            *************
                             M I K E - 2
                            *************
               Micro Interpreter for Knowledge Engineering
                  {written in Edinburgh-syntax Prolog}

MIKE: Copyright (c) 1989, 1990, 1991 The Open University (U.K.)

MIKE is intended for educational purposes, and may not
be sold as or incorporated in a commercial product without
written permission from: The Copyrights Officer, Open University,
Milton Keynes MK7 6AA, U.K.

The Open University accepts no responsibility for any legal or other
consequences which may arise directly or indirectly as a result of the
use of all or parts of the contents of this program.
*/


/*
   The file MIKE.PL always loads this file last, so this is the place
   to add 'customizations'. A few sample customizations are included to
   give you the idea.
*/

/* Customization 1: some extra syntactic sugar for enhancing rule syntax
   with ordinary arithmetic operators.  If you want to add more of your
   own in some other file, be sure to use the 'directive' form, e.g.
         ?- allow_prolog_lhs(_ < _).
         ?- allow_prolog_rhs(foo(_,_)).
   The first directive allows '<' to be used on the left hand side
   (IF-PART) of a MIKE rule.
   The second allows some user-defined predicate 'foo' to be used
   on the right hand side (THEN-PART) of a MIKE rule.
  (These directives are defined in the file ENGINE2.PL)
*/
                      /* We want to use a new assignment operator ':=' */
/* ?- op(740,xfx,':=').  Declared in LOADOPS.PL */

Var := Term :-   /* Then we define its meaning in Prolog ... */
   Var is Term.  /* Simply calls the built-in Prolog 'is' for assignment */

:- allow_prolog_rhs(_ := _). /* Then we tell MIKE to allow it as a right-hand
                                side action */

/* The next four operators are already known to Prolog, so we just need
   to tell MIKE that they should be allowed on the left hand side of rules */

:- allow_prolog_lhs(_ > _).
:- allow_prolog_lhs(_ < _).
:- allow_prolog_lhs(_ >= _).
:- allow_prolog_lhs(_ =< _).

/*
with the above declarations, you could now write a rule like this:

   rule foo forward
    if
      [annual_earnings, Person, AE] &
      AE > 25000
    then
      MonthlySalary := AE/12 &
      MonthlyTax := MonthlySalary*0.40 &
      announce [Person, ' pays this higher rate montly tax: ', MonthlyTax].

additional comments about these operators are in the file READ.ME
*/


/* benchmarks for MIKE-2.  There are two predicates, the first bm/0
invokes the forward chaining interpreter and produces the timing
information of this run.  The second times takes a single argument
and times the execution of this.  */
bm:-
/*    time(Hrs,Mins,Sec), */
         /* COMPATIBILITY NOTE: hours needed in case you start the benchmark
            at 11.59.59 etc.. ESL Prolog-2 specific  */
    statistics(runtime,[_,_]),
    fc,
    statistics(runtime,[_,Time]),
/*    time(NHrs,NMins,NSec),*/
    /* convert everything into seconds and then take the difference */
/*    First is (Hrs * 3600) + (Mins * 60) + Sec,
    Second is (NHrs * 3600) + (NMins * 60) + NSec, 
    Time is Second - First,  */
    'pd624 write'([nl,'***** Time Taken was (in milliseconds) ---> ',Time,nl]).
bm(ProveMe):-
/* analogous to the above, only takes an argument and proves it, or
   records the time taken for it to fail */
/*    time(Hrs,Mins,Sec),*/   /* COMPATIBILITY NOTE: ESL Prololg-2 specific */
    statistics(runtime,[_,_]),
    (ProveMe;true),   /* NOTE we only worry about the time taken in the
    benchmarks, we do not concern ourselves with whether the goal
    actually succeeds or not.  So be aware that you might be finding the
    time taken for the program to fail, not necessarily the time it takes
    to succeed */
    statistics(runtime,[_,Time]),
/*    time(NHrs,NMins,NSec),*/
    /* convert everything into seconds and then take the difference */
/*    First is (Hrs * 3600) + (Mins * 60) + Sec,
    Second is (NHrs * 3600) + (NMins * 60) + NSec, 
    Time is Second - First, */
    'pd624 write'([nl,'***** Time Taken to execute ',ProveMe,
    ' was (in milliseconds) ---> ',Time,nl]).

/* What follows USED TO be in the file 'extras.pl', which was reconsulted
   at this point.  However, this led to too many indirect reconsults, which
   was a bit confusing and unnecessary, and therefore the body of extras.pl
   is simply pasted in directly below.  It comprises the definitions of
   ask_menu and forall.

   See above for an explanation of the
   ?- allow_prolog... declarations in this file.

The following facilities are defined in this file:
1. Simple multiple-choice menus
2. A 'forall' facility
*/

/* special menu handling code */
/* next line makes 'ask_menu' legal on right hand side of MIKE rules */

:- allow_prolog_rhs(ask_menu(_,_,_)).

/*
'ask_menu' facility (right hand side of MIKE rules only)
---------------------------------------------------------
ask_menu(Object, Relation, List)  displays a numbered menu of
all the items in List and prompts the user to type in number(s).

   For example:
     rule a forward
      if
        diagnosing &
        [current_patient, P]
      then
        ask_menu(P,exhibits_symptom,[sneezing,coughing,headache,spots]).

   The end-user types in the numbers (1,2,3,4, or some combination, in
   this case) and the corresponding list elements are then stored
   in working memory as a triple: [Object, Relation, Choice]
   for each numbered Choice which was presented in the List.
   e.g.         [john, exhibits_symptom, sneezing]
   An example of its use may be found in the file FLU2.KB

*/

ask_menu(_,_,[]):- !,
    'pd624 write'(['Error: ask menu must be called with a list of menu',nl,
                           'elements, not with an empty list',nl]).
ask_menu(Object,Relation,List):-
    'pd624 write'(['**********************************************',nl]),
    draw_and_read_menu(1,List,Menu),
    'pd624 write'(['Choose the items from the menu by typing the',nl,
           'corresponding number(s). Separate numbers with commas e.g.',nl,
           '1,3,5.',nl,
           'REMEMBER to use a FULL STOP (''.'') at the end',nl,'==> ']),
    read(Selections),
    add_selections_to_wm(Object,Menu,Selections,Relation),
    nl.

draw_and_read_menu(_,[],[]).
draw_and_read_menu(N,[H|T],[N-H|Rest]):-
    'pd624 write'([t/6,N,'   -   ',H,nl]),
    N1 is N + 1,
    draw_and_read_menu(N1,T,Rest).

add_selections_to_wm(Thing,Menu,(A,B),Relation):-
    (A=(Num/CF);(CF is 1.0,Num is A)),  /* fetch, or invent,certainty factor */
    add_singleton_to_wm(Thing,Menu,Num,Relation,CF),
    add_selections_to_wm(Thing,Menu,B,Relation).

add_selections_to_wm(Thing,Menu,A,Relation):-
    (A=(Num/CF);(CF is 1.0,Num is A)),
    add_singleton_to_wm(Thing,Menu,Num,Relation,CF).

add_singleton_to_wm(Thing,Menu,Num,Relation,CF):-
   'pd624 member'(Num-Item,Menu),
   add [Thing,Relation,Item] cf CF.
add_singleton_to_wm(_,_,A,_,_):-
    'pd624 write'(['Error : ',A,' is not a legal menu entry and will',nl,
    'be ignored.',nl]).

/* FORALL */

/*
'forall' facility (for left hand side of MIKE rules only)
-----------------------------------------------------------
forall(Pattern1, Pattern2) tests whether all of the variables
which match working memory within Pattern1 also match working memory
within Pattern2.  For instance, suppose working memory contains
the following patterns:
   [john, likes, mary]
   [john, likes, sue]
   [john, likes, betty]
   [bill, likes, mary]
   [bill, likes, betty]
   [bill, likes, sue]

The following rule succeeds with the above working memory:

  rule b forward
   if
    forall([john, likes, X], [bill, likes, X])
   then
    announce ['bill likes all the people that john likes'].

If we typed in ?- remove [bill, likes, sue].
Then the above rule b would fail
*/

:- allow_prolog_lhs(forall(_,_)).

forall(Pattern1, Pattern2) :-
  pd624_own_findall(in_wm(Pattern2), in_wm(Pattern1), Cases),
  forall_tryall(Cases).

forall_tryall([Case|Cases]) :-
  call(Case),
  forall_tryall(Cases).

forall_tryall([]).


:- allow_prolog_rhs(new_symbol(_,_)).

new_symbol(Root,NewSymbol) :-
  nonvar(Root), var(NewSymbol),
  ( retract('pd624 symbol root'(Root, Number))
  ; Number = 0
  ),
  NewNumber is Number + 1,
  assert('pd624 symbol root'(Root, NewNumber)),
  name(Root, List),
  name(Number,NumberList),
  append(List,NumberList,NameList),
  name(NewSymbol,NameList),
  !.
new_symbol(_,_) :-
 'pd624 write'(['ERROR: Illegal use of new_symbol(Root,Symbol)',
 nl,            '       The first argument must be an atom, the',
 nl,            '       second must be uninstantiated.', nl]), !, fail.


