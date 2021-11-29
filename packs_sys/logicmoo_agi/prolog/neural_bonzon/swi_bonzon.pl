/* compiler_cleaner */
/* BNF syntax
<tree> ::= [] || <sequence> || [<alternative>]
<sequence> ::= [<instruction>|<tree>]
<alternative> ::= <branch> ||(<branch>;<alternative>)
<branch> ::=(<guard>|<tree>)
*/
:- use_module(library(apply),[maplist/3]).
:- use_module(library(logicmoo_utils)).
:- cls.
:-op(600,fx,'@').
:-op(800,xfy,'>>>').
:-op(700,xfy,'o').
:-op(901,xfy,':::').
:-op(904,xfy,'=>').

bonzon_bi(lpa_call_sense/1).
bonzon_bi(lpa_call/1). bonzon_bi(apply/2). bonzon_bi(wdmsg/1).

bonzon_bi((->)/2). bonzon_bi(!/1). bonzon_bi(('[|]')/2). bonzon_bi(('|')/2). bonzon_bi(('$VAR')/1). bonzon_bi((,)/2).
bonzon_bi((:::)/2). bonzon_bi((;)/2). bonzon_bi((=)/2).
bonzon_bi((>>>)/2). bonzon_bi((\+)/1). bonzon_bi(assert/1).
bonzon_bi(atom/1). bonzon_bi(call/1). bonzon_bi(compile/2). bonzon_bi(compileAlternative/4). bonzon_bi(compileBranch/4). 
bonzon_bi(compileInstruction/4). bonzon_bi(compileSequence/4). bonzon_bi(compileTree/4).  bonzon_bi(compound_name_arguments/3).
bonzon_bi(else/1). bonzon_bi(findall/3). bonzon_bi(for_each/3). bonzon_bi(forall/2). bonzon_bi(functor/3).  bonzon_bi(get_code/1).
bonzon_bi(if/2). bonzon_bi(if/3). bonzon_bi(if_not/2). bonzon_bi(insert/2). bonzon_bi(instance/2). bonzon_bi(instruction/1).
bonzon_bi(interrupt/1). bonzon_bi(is/2). bonzon_bi(is_list/1). bonzon_bi(is_list/2). bonzon_bi(ist/2).
bonzon_bi(length/2). bonzon_bi(append/3). bonzon_bi(lpa_functor/1). bonzon_bi(load/1).
bonzon_bi(loop/1). bonzon_bi(maplist/3). bonzon_bi(member/2). bonzon_bi(('+')/2). bonzon_bi(('=>')/2).
bonzon_bi(from/1). bonzon_bi(do/1). bonzon_bi(new/1). bonzon_bi(nth0/3). bonzon_bi(random/1). bonzon_bi(random/2).
bonzon_bi(random_e/2). bonzon_bi(react/1). bonzon_bi(read/1). bonzon_bi(reflect/1). bonzon_bi(remove/2). 
bonzon_bi(retractall/1). bonzon_bi(run/1). bonzon_bi(such_that/1). bonzon_bi(set/2).
bonzon_bi(then/1). bonzon_bi(write/1). 

bonzon_builtin(F/A):- nonvar(F),bonzon_bi(F/A),!.
bonzon_builtin(F/A):- current_predicate(system:F/A), format('%~~ ~N~q.~n',[bonzon_bi(F/A)]).

%lpa_zave_varname(N,V):- debug_var(N,V),!.
lpa_zave_varname(N,V):- V = '$VAR'(N).


lpa_implode_varnames(Vs):- (var(Vs) ; Vs==[]),!.
lpa_implode_varnames([NV|Vs]) :- lpa_implode_varnames(Vs),
    (var(NV) -> ignore((get_var_name(NV,Name),lpa_zave_varname(Name,NV))); 
  ignore((NV=(N=V),lpa_zave_varname(N,V)))).


lpa_expansion(I,O):- \+ compound(I) ; I=(:- _),!,I=O.
%lpa_expansion(I,O):- sub_term(E,I),is_list(E), !,I=O.
lpa_expansion(I,O):- 
  prolog_load_context(variable_names,Vs),copy_term(I:Vs,II:VVs),
  lpa_implode_varnames(VVs),
  term_variables(II,TVs),
  maplist(=('$VAR'('_')),TVs),
  %prolog_load_context(variable_names,Vs),
  %pterm_to_sterm(I,O),
  %I=O,
  pterm_to_sterm(II,OO),
  unnumbervars(OO,O),
  nop(II\==OO-> wdmsg(II); true), format('~N'),pc(OO),!.

%pc(O):- print(O),writeln('.'),!.
pc(O):- prolog_listing:portray_clause(O).
pc(O):- prolog_listing:portray_body(O, 0, indent, 1199, current_output, [portray(true),quoted(true), output(current_output)]).
pc(O):- print_tree(O).

pterm_to_sterm(In,In):-  \+ compound(In),!.
pterm_to_sterm('$VAR'(In),'$VAR'(In)):-!.
pterm_to_sterm(In,Out):-  is_list(In), !, maplist(pterm_to_sterm,In,Out).
%pterm_to_sterm(In,Out):- sub_term(E,In),is_list(E), !, In=Out.
pterm_to_sterm(In,Out):- 
 compound_name_arguments(In,N,A),
 functor(In,N,Ar),
 maplist(pterm_to_sterm,A,AA),
 lis_to_pterm(Ar,[N|AA],Mid),
 maybe_fix_list(Mid,Out).

maybe_fix_list(Mid,Out):- maybe_fix_list_0(Mid,Out),ignore((Mid=[A|_],atom(A),assert_if_new(bonzon_op(A)))).
maybe_fix_list_0(Mid,Out):- \+ compound(Mid), Out= Mid.
maybe_fix_list_0('@'(T),T):- !.
maybe_fix_list_0(Mid,Out):- \+ is_list(Mid), Out= Mid.
maybe_fix_list_0(['@',T],T):- !.
maybe_fix_list_0([H|T],[H|TT]):- maybe_fix_list_0(T,TT).

maybe_var(A,_):- \+ atom(A),!,fail.
maybe_var(A,AA):- downcase_atom(A,A),!,AA=A.
maybe_var(A,AA):- AA='$VAR'(A).
lis_to_pterm(_,X,X):- \+ compound(X),!.
lis_to_pterm(_,X,X):- \+ is_list(X),!.
%lis_to_pterm(_,[N|A],Y):- maplist(pterm_to_sterm,A,AA),A\==AA,!,lis_to_pterm(_,[N|AA],Y).
lis_to_pterm(_,[N|A],Y):- atom(N),upcase_atom(N,N),downcase_atom(N,N),is_list(A),compound_name_arguments(Y,N,A).
lis_to_pterm(_,[A,@(B)],[AA|B]):-!,maybe_var(A,AA).
lis_to_pterm(_,[A,@,B],[A|B]):-!.
lis_to_pterm(_,['$VAR',A],['$VAR'(A)]):-!.
lis_to_pterm(_,[N|A],[N|A]):- atom(N),instruction(N),!.
lis_to_pterm(_,[o,A,B],NN):- !, append(A,B,NN).
lis_to_pterm(Ar,[N|A],Y):- atom(N),bonzon_builtin(N/Ar),!,assertion((atom(N),is_list(A))),compound_name_arguments(Y,N,A).
lis_to_pterm(_,[N|A],[NN|A]):- maybe_var(N,NN),!.
lis_to_pterm(_,[N|A],[N|A]):-!.
%lis_to_pterm(_,[N|A],Y):- atom(N),compound_name_arguments(Y,N,A).
%lis_to_pterm(_,NA,t(NA)).



term_expansion(I,P,O,P):- 
  notrace(current_prolog_flag(allow_variable_name_as_functor,true)),
  compound(I),
  nonvar(P),
  prolog_load_context(term,T),T==I,
  lpa_expansion(I,O),!.

:- style_check(-singleton).


:- dynamic(bonzon_op/1).


bonzon_op(at). 
bonzon_op(check).
bonzon_op(choice).
bonzon_op(cleaner).
bonzon_op(clear).
bonzon_op(clock).
bonzon_op(decrement).
bonzon_op(detect).
bonzon_op(effector).
bonzon_op(excite).
bonzon_op(fetch).
bonzon_op(fire).
bonzon_op(increment).
bonzon_op(inhibit).
bonzon_op(initial).
bonzon_op(join).
bonzon_op(learn).
bonzon_op(left).
bonzon_op(ltd).
bonzon_op(ltp).
bonzon_op(merge).
bonzon_op(move).
bonzon_op(on).
bonzon_op(recall).
bonzon_op(receive).
bonzon_op(resume).
bonzon_op(right).
bonzon_op(robot).
bonzon_op(scan).
bonzon_op(send).
bonzon_op(sense).
bonzon_op(sensor).
bonzon_op(seq).
bonzon_op(signal).
bonzon_op(step).
bonzon_op(stop).
bonzon_op(sync).
bonzon_op(synchro).
bonzon_op(thread).
bonzon_op(threads).
bonzon_op(weight).
bonzon_op(weights).

compile(Tree,Code) :-
  compileTree(Tree,true,1,Code).

compileTree([],Guard,T,[Guard => T:::end]).
compileTree(Sequence,Guard,T,Code) :-
  compileSequence(Sequence,Guard,T,Code).
compileTree([Alternative],Guard,T,Code) :-
  compileAlternative(Alternative,Guard,T,Code).

compileSequence([Instruction|Tree],Guard,T,Code) :-
  T1 is T+1,
  compileInstruction(Instruction,Guard,T,P1),
  compileTree(Tree,Guard,T1,P2),
  append(P1,P2,Code).

compileAlternative(Branch,Guard,T,Code) :-
  compileBranch(Branch,Guard,T,Code).
 
compileAlternative((Branch;Alternative),Guard,T,Code) :-
  compileBranch(Branch,Guard,T,P1),
  compileAlternative(Alternative,Guard,T,P2),
  append(P1,P2,Code).

compileBranch((Guard1| Tree), Guard2, T, Code) :-
    ( Guard2=true ->  Guard=Guard1 ;   Guard=(Guard1, Guard2)),
    compileTree(Tree, Guard, T, Code).


lpa_functor(PX,F,A):- is_list(PX),PX=[F|Len],length(Len,A).
lpa_functor(PX,F,A):- functor(PX,F,A).

% compileInstruction(P(@X), Guard, T, [Guard => T ::: P(@X)] ) :- \+ atom(PX), lpa_functor(PX,F), instruction(F).
compileInstruction(PX, Guard, T, [Guard => T ::: PX] ) :- \+ atom(PX), lpa_functor(PX,F,_), instruction(F).


			/*
*/

instruction(fire).
instruction(resume).
instruction(end).
instruction(exit).
instruction(send).
instruction(receive).
instruction(join).
instruction(merge).
instruction(increment).
instruction(decrement).
instruction(step).
instruction(on).
instruction(choice).
instruction(check).
instruction(scan).
instruction(effector).





			/* machine_cleaner */

:- set_prolog_flag(allow_variable_name_as_functor,true).

%:-consult(compiler_cleaner).

load(Model) :- new(_), 			/* clear machine */
   for_each(thread(Thread,Tree), 			/* for all threads */
     from(threads(Model(Fiber))), 		/* from model fibers */
     do((compile(Tree,Code), 			    /* compile tree */
       forall(member(P,Code),
       insert(Model(Fiber)o(Thread),P))))), 			/* load code */

   for_each(thread(Thread,Tree), 			/* for all threads */
     from(threads(Model)), 			      /* from model */
     do((compile(Tree,Code), 			    /* compile kernel */
     forall(member(P,Code),
       insert(Model(_)o(Thread),P))))),	/* load kernel */

   for_each(Weight(Thread1,Thread2)o(@(X)), 
       from(weights(Model(Fiber))), 			/* load weights */
       do(insert(Model(Fiber),
   Weight(Thread1,Thread2)o(@(X))))).

run(Model) :- loop((sense(Model), 		/* loop sense */
                react(Model), 			  /* react */
                 reflect(Model))). 		/* reflect */

lpa_call_sense(Model) :- if(interrupt(Stream(Interrupt)), 			/* if interrupt */
    then((remove(Model(Stream)o(_),clock(@(_))), 			/*   then clear stream */
  remove(Model(Stream)o(_),excite(@_)), 
  remove(Model(Stream)o(_),inhibit(@_)), 
  remove(Model(Stream)o(_),signal(@_)),   
  for_each(sensor(@X), 			                    /* for each */
  such_that(member(sensor(@X),Interrupt)), 			/* instruction */
  do((set(Model(Stream)o(sense(@X)),clock(1)), 	/* fire sensor */
  write(0:::sense(@X):::sensor(@X)),nl))), 
  set(Model(Stream),seq(1)), 			              /* reset stream */
  remove(Model(Stream),_:::_:::_)))). 			    /* clear synchro */

react(Model) :- for_each((Stream(Thread),T:::Instruction), 			/* for each */
 such_that(ist(Model(Stream)o(Thread), 			                    /* instruction */
(clock(T), T:::Instruction))), 			                            /* retrieve */
 do(Model(Stream)o(Thread)>>>(T:::Instruction))). 			        /* interpret */

reflect(Model):-for_each((Stream,I:::Thread:::Stimulus), 			/* for each stimulus */
 such_that(ist(Model(Stream),I:::Thread:::Stimulus)), 			  /* retrieve synchro */
 do(Model(Stream)>>>(I:::Thread:::Stimulus))). 			          /* report synchro */

/* virtual machine stream reports */ 

Model(Stream)>>>(I:::Thread:::Stimulus) :-
 write(I:::Thread:::Stimulus),nl,
 remove(Model(Stream),I:::Thread:::Stimulus).

/* virtual machine thread instructions */

[Model, Stream]>>>(I:::Thread:::Stimulus) :-
    write(I:::Thread:::Stimulus),
    nl,
    remove([Model, Stream],
           I:::Thread:::Stimulus).
[Model, Stream]>>>(I:::Thread:::Stimulus) :-
    write(I:::Thread:::Stimulus),
    nl,
    remove([Model, Stream],
           I:::Thread:::Stimulus).
[Model, Stream, P, X]>>>(T:::[fire, [Q, Y]]) :-
    T1 is T+1,
    set([Model, Stream, Q, Y], [clock, 1]),
    set([Model, Stream, P, X], [clock, T1]).
[Model, Stream, P, X]>>>(_T:::[resume, [P, X]]) :-
    set([Model, Stream, P, X], [clock, 1]).
[Model, Stream, P, X]>>>(T:::end) :-
    remove([Model, Stream, P, X], [clock, T]).
[Model, Stream, P, X]>>>(T:::[send, [Q, Y]]) :-
    T1 is T+1,
    if_not(ist([Model, Stream, Q, Y], [clock, _]),
           then(set([Model, Stream, Q, Y], [clock, 1]))),
    if_not(ist([Model, Stream],
               [weight, [P, X], [Q, Y]|W]),
           then(if(ist([Model, Stream], [initial, [P, X], [Q, Y]|W]),
                   then(set([Model, Stream],
                            [ weight, [P, X], [Q, Y] | W ])),
                   else(set([Model, Stream], [weight, [P, X], [Q, Y]|0]))))),
    if_not(ist([Model, Stream, P, X],
               [signal, [send, [Q, Y]]]),
           then(insert([Model, Stream, P, X],
                       [signal, [send, [Q, Y]]]))),
    set([Model, Stream, P, X], [clock, T1]).
[Model, Stream, P, X]>>>(T:::[receive, [Q, Y]]) :-
    T1 is T+1,
    if(ist([Model, Stream, Q, Y],
           [signal, [send, [P, X]]]),
       then(if((ist([Model, Stream], [weight, [Q, Y], [P, X]|K]), K>0),
               then(set([Model, Stream, P, X],
                        [clock, T1]))))).
[Model, Stream, P, X]>>>(T:::[merge, [Q, Y]]) :-
    T1 is T+1,
    if_not(ist([Model, Stream, P, X], [signal, [merge, [Q, Y]]]),
           then(insert([Model, Stream, P, X], [signal, [merge, [Q, Y]]]))),
    set([Model, Stream, P, X], [clock, T1]).
[Model, Stream, P, X]>>>(T:::[join, [Q, Y]]) :-
    T1 is T+1,
    if(ist([Model, Stream, Q, Y], [signal, [merge, [P, X]]]),
       then(set([Model, Stream, P, X], [clock, T1]))).
[Model, Stream|Thread]>>>(T:::[increment, [weight, [P, [F, X]], [Q, Y]]]) :-
    T1 is T+1,
    ist([Model, Stream], [seq, I]),
    if_not(ist([Model, Stream], [weight, [P, [F, X]], [Q, Y]|W]),
           then(if(ist([Model, Stream], [ initial, [P, [F, X]], [Q, Y] | W ]),
                   then(set([Model, Stream], [ weight, [P, [F, X]], [Q, Y] | W ])),
                   else(set([Model, Stream], [ weight, [P, [F, X]], [Q, Y] | 0 ]))))),
    if((ist([Model, Stream], [weight, [P, [F, X]], [Q, Y]|W]), W<1),
       then((W1 is W+1, insert([Model, Stream], I:::Thread:::[weight, [P, [F, _]], [Q, Y]|W1]), 
                        set([Model, Stream], [weight, [P, [F, _]], [Q, Y]|W1])))),
    set([Model, Stream|Thread], [clock, T1]).
[Model, Stream|Thread]>>>(T:::[decrement, [weight, [P, [F, X]], [Q, Y]]]) :-
    T1 is T+1,
    ist([Model, Stream], [seq, I]),
    if_not(ist([Model, Stream],
               [weight, [P, [F, X]], [Q, Y]|W]),
           then(if(ist([Model, Stream], [ initial, [P, [F, X]], [Q, Y] | W ]),
                   then(set([Model, Stream], [ weight, [P, [F, X]], [Q, Y] | W ])),
                   else(set([Model, Stream],
                            [ weight, [P, [F, X]], [Q, Y] | 0 ]))))),
    if((ist([Model, Stream], [weight, [P, [F, X]], [Q, Y]|W]), W>0),
       then((W1 is W-1, 
             insert([Model, Stream], I:::Thread:::[weight, [P, [F, _]], [Q, Y]|W1]), 
             set([Model, Stream], [weight, [P, [F, _]], [Q, Y]|W1])))),
    set([Model, Stream|Thread], [clock, T1]).
[Model, Stream|Thread]>>>(T:::[step, Y]) :-
    T1 is T+1,
    ist([Model, Stream], [seq, I]),
    if(Y=forward, then(F=right), else(F=left)),
    if(ist([Model, _], [at, [F, X]]),
       then(X1 is X+1),
       else(X1 is 1)),
    if(X1<8,
       then(set([Model, _], [at, [F, X1]])),
       else(remove([Model, _], [at, _]))),
    I1 is I+1,
    set([Model, Stream], [seq, I1]),
    set([Model, Stream|Thread], [clock, T1]).
[Model, Stream|Thread]>>>(T:::[on, X]) :-
    T1 is T+1,
    if(ist([Model, _], [at, X]),
       then(set([Model, Stream], [on, X])),
       else(remove([Model, Stream], [on, _]))),
    set([Model, Stream|Thread], [clock, T1]).
[Model, Stream|Thread]>>>(T:::[choice, X]) :-
    T1 is T+1,
    ist([Model, Stream], [seq, I]),
    random_e(R, X),
    set([Model, Stream|Thread], [fetch, R]),
    insert([Model, Stream],
           I:::Thread:::[fetch, R]),
    set([Model, Stream|Thread], [clock, T1]).
[Model, Stream|Thread]>>>(T:::[check, [P, X]]) :-
    T1 is T+1,
    ist([Model, Stream], [seq, I]),
    remove([Model, Stream|Thread], [excite, [P, X]]),
    remove([Model, Stream|Thread],
           [inhibit, [P, X]]),
    if(ist([Model, Stream], [P, X]),
       then((set([Model, Stream|Thread], [excite, [P, X]]), 
             insert(Model, [Stream, I]:::Thread:::[excite, [P, X]]), 
             insert([Model, Stream], I:::Thread:::[excite, [P, X]]))),
       else((set([Model, Stream|Thread], [inhibit, [P, X]]), 
             insert([Model, Stream], I:::Thread:::[inhibit, [P, X]]), 
             insert(Model, [Stream, I]:::Thread:::[inhibit, [P, X]])))),
    set([Model, Stream|Thread], [clock, T1]).
[Model, Stream|Thread]>>>(T:::[scan, [detect, [F, X]], [move, Y]]) :-
    T1 is T+1,
    ist([Model, Stream], [seq, I]),
    remove([Model, Stream|Thread],
           [sync, [detect, [F, X]], [move, Y]]),
    if((ist(Model, [Stream, I1]:::[detect, [F, X]]:::[excite, [on, [F, X]]]), 
        ist(Model, [Stream, I2]:::[move, Y]:::[excite, [at, [F, X]]]), I1=I2),
       then((set([Model, Stream|Thread], [sync, [detect, [F, X]], [move, Y]]),
        insert([Model, Stream], I:::Thread:::[sync, [detect, [F, X]], [move, Y]])))),
    set([Model, Stream|Thread], [clock, T1]).
[Model, Stream|Thread]>>>(T:::[effector, P]) :-
    T1 is T+1,
    ist([Model, Stream], [seq, I]),
    insert([Model, Stream],
           I:::Thread:::[effector, P]),
    set([Model, Stream|Thread], [clock, T1]).

			/* data types and macro instructions */

:- dynamic(instance/2). 

new(C) :- retractall(instance(C,_)).

insert(C,P) :- assert(instance(C,P)).

remove(C,P) :- retractall(instance(C,P)).

set(C,F(@(X))) :- remove(C,F(@ _)), insert(C,F(@X)).

ist(_C,true).
ist(C,P) :- instance(C,P); instance(C,Q=>P),ist(C,Q).
ist(C,(P,Q)) :- ist(C,P), ist(C,Q).

loop(P) :- repeat, call((lpa_call(P),!)),fail.

interrupt(P):-get_code(C),(C=13->nl,read(P);false).

lpa_call(G):- wdmsg(lpa_call(G)),
  (is_list(G)->apply(call,G);call(G)).

if(P,then(Q)):-lpa_call(P)->lpa_call(Q);true.

if_not(P,then(Q)):-lpa_call(P)->true;lpa_call(Q).

if(P,then(Q),else(R)):-lpa_call(P)->lpa_call(Q);lpa_call(R).


for_each(X, such_that(F), do(P)) :-
    findall(X, lpa_call(F), L),
    forall(member(X, L), P).
for_each(X, from(F), do(P)) :-
    forall(lpa_call(F:::L), forall(member(X, L), lpa_call(P))).

random_e(X,L):-length(L,N),K is random(N),nth0(K,L,X).
                                                   





/* cleaner  
 
                                          |inhibit|resume(detect(F(X))) 
             -detect(F(X))-check(on(F(X)))| 
            |                             |excite|clear(F(X))-synchro(detect(F(X)),move(A))-  
            |                                                                                         |             |   ---<----------------------------------------------------------------------------------  
            |  |  
            | LTP 
            | \|/                                      |excite|resume(move(A)) 
            +--*>=>-recall(A)-+-move(A)-check(at(F(X)))| 
            |                 |                        |inhibit|stop(A) 
            |   ---<---------- 
            |  |                                                           |excite|resume(move(A)) 
            | LTD                         |fetch(A)|move(A)-check(at(F(_)))| 
            | \|/                         |                                |inhibit|stop(A) sense(F(X))-+--*>=>-learn(F)-choice([A,B])| 
            | /|\                         |                                |excite|resume(move(B)) 
            | LTD                         |fetch(B)|move(B)-check(at(F(_)))|  
            |  |                                                           |inhibit|stop(A) 
            |   ---<----------  
            |                 |                        |excite|resume(move(A)) 
            +--*>=>-recall(B)-+-move(B)-check(at(F(X)))| 
            | /|\                                      |inhibit|stop(A) 
            | LTP  
            |  | 
            |   ---<----------------------------------------------------------------------------------  
            |                                                                                         | 
            |                              |          
            |                             |excite|clear(F(X))-synchro(detect(F(X)),move(B))- 
             -detect(F(X))-check(on(F(X)))| 
                                          |inhibit|resume(detect(F(X))) 
*/
  


:::([threads, [robot, [cleaner, A, A]]],
    [ [ thread,
        [sense, [B, C]],
        [ [merge, [ltd, [sense, [B, C]], [learn, B]]],
          [merge, [ltp, [sense, [B, C]], [recall, D]]],
          [send, [learn, B]],
          [fire, [detect, [B, C]]],
          [send, [recall, D]]
        ]
      ],
      [ thread,
        [detect, [B, C]],
        [ [on, [B, C]],
          [check, [on, [B, A]]],
          (([excite, [on, [B, A]]]| [[effector, [clear, [B, C]]], 
            [fire, [synchro, [detect, [B, A]], [move, A]]]])
            ;[inhibit, [on, [B, A]]]| [[resume, [detect, [B, A]]]])
        ]
      ],
      [ thread,
        [learn, B],
        [ [receive, [sense, [B, C]]],
          [choice, [A, A]],
          (([fetch, A]| [[fire, [move, A]]]);([fetch, A]| [[fire, [move, A]]]))
        ]
      ],
      [ thread,
        [synchro, [detect, [B, C]], [move, D]],
        [ [scan, [detect, [B, C]], [move, D]],
          [sync, [detect, [B, A]], [move, A]],
          [fire, [ltp, [sense, [B, A]], [recall, A]]]
        ]
      ],
      [ thread,
        [recall, D],
        [ [receive, [sense, [B, C]]],
          [fire, [ltd, [sense, [B, A]], [learn, A]]],
          [fire, [move, A]]
        ]
      ],
      [ thread,
        [move, D],
        [ [step, D],
          [check, [at, [B, A]]],
          ([excite, [at, [B, A]]]| [[resume, [move, A]]]
           ;[inhibit, [at, [B, A]]]| [[effector, [stop, A]]])
        ]
      ]
    ]).

:::([threads, robot],
    [ [ thread,
        [ltp, A, B],
        [[join, A], [increment, [weight, A, B]]]
      ],
      [ thread,
        [ltd, C, C],
        [[join, C], [decrement, [weight, C, C]]]
      ]
    ]).

:::([weights, [robot, [cleaner, A, A]]],
    [ [initial, [sense, A], [learn, A]|1],
      [initial, [recall, A], do(A)|1]
    ]).



			/* example run
consult(cleaner).
load(robot).
run(robot).

cleaner(forward,backward)o([sensor(right(3))]).

0 : sense(right(3)) : sensor(right(3))
1 : learn(right) : fetch(backward)
1 : detect(right(3)) : inhibit(on(right(3)))
2 : move(backward) : excite(at(left(1)))
2 : detect(right(3)) : inhibit(on(right(3)))
3 : move(backward) : excite(at(left(2)))
3 : detect(right(3)) : inhibit(on(right(3)))
4 : move(backward) : excite(at(left(3)))
4 : detect(right(3)) : inhibit(on(right(3)))
5 : move(backward) : excite(at(left(4)))
5 : detect(right(3)) : inhibit(on(right(3)))
6 : move(backward) : excite(at(left(5)))
6 : detect(right(3)) : inhibit(on(right(3)))
7 : move(backward) : excite(at(left(6)))
7 : detect(right(3)) : inhibit(on(right(3)))
8 : move(backward) : excite(at(left(7)))
8 : detect(right(3)) : inhibit(on(right(3)))
9 : move(backward) : inhibit(at(_44192(_44198)))
9 : move(backward) : effector(stop(backward))
9 : detect(right(3)) : inhibit(on(right(3)))
9 : detect(right(3)) : inhibit(on(right(3)))
9 : detect(right(3)) : inhibit(on(right(3)))
...
cleaner(forward,backward)o([sensor(right(3))]).
0 : sense(right(3)) : sensor(right(3))
1 : learn(right) : fetch(forward)
1 : detect(right(3)) : inhibit(on(right(3)))
2 : move(forward) : excite(at(right(1)))
2 : detect(right(3)) : inhibit(on(right(3)))
3 : move(forward) : excite(at(right(2)))
3 : detect(right(3)) : inhibit(on(right(3)))
4 : move(forward) : excite(at(right(3)))
4 : detect(right(3)) : excite(on(right(3)))
5 : detect(right(3)) : effector(clear(right(3)))
5 : move(forward) : excite(at(right(4)))
5 : synchro(detect(right(3)),move(forward)) : sync(detect(right(3)),move(forward))
6 : move(forward) : excite(at(right(5)))
6 : ltp(sense(right(3)),recall(forward)) : weight(sense(right(_14222)),recall(forward))o(1)
7 : move(forward) : excite(at(right(6)))
7 : ltd(sense(right(3)),learn(right)) : weight(sense(right(_38444)),learn(right))o(0)
8 : move(forward) : excite(at(right(7)))
9 : move(forward) : inhibit(at(_42122(_42128)))
9 : move(forward) : effector(stop(forward))
cleaner(forward,backward)o([sensor(right(3))]).
   
0 : sense(right(3)) : sensor(right(3))
1 : detect(right(3)) : inhibit(on(right(3)))
1 : detect(right(3)) : inhibit(on(right(3)))
2 : move(forward) : excite(at(right(1)))
2 : detect(right(3)) : inhibit(on(right(3)))
3 : move(forward) : excite(at(right(2)))
3 : detect(right(3)) : inhibit(on(right(3)))
4 : move(forward) : excite(at(right(3)))
4 : detect(right(3)) : excite(on(right(3)))
5 : detect(right(3)) : effector(clear(right(3)))
5 : move(forward) : excite(at(right(4)))
5 : synchro(detect(right(3)),move(forward)) : sync(detect(right(3)),move(forward))
6 : move(forward) : excite(at(right(5)))
7 : move(forward) : excite(at(right(6)))
8 : move(forward) : excite(at(right(7)))
9 : move(forward) : inhibit(at(_22894(_22900)))
9 : move(forward) : effector(stop(forward))
cleaner(forward,backward)o([sensor(right(2))]).
  
0 : sense(right(2)) : sensor(right(2))
1 : detect(right(2)) : inhibit(on(right(2)))
1 : detect(right(2)) : inhibit(on(right(2)))
2 : move(forward) : excite(at(right(1)))
2 : detect(right(2)) : inhibit(on(right(2)))
3 : move(forward) : excite(at(right(2)))
3 : detect(right(2)) : excite(on(right(2)))
4 : detect(right(2)) : effector(clear(right(2)))
4 : move(forward) : excite(at(right(3)))
4 : synchro(detect(right(2)),move(forward)) : sync(detect(right(2)),move(forward))
5 : move(forward) : excite(at(right(4)))
6 : move(forward) : excite(at(right(5)))
7 : move(forward) : excite(at(right(6)))
8 : move(forward) : excite(at(right(7)))
9 : move(forward) : inhibit(at(_41294(_41300)))
9 : move(forward) : effector(stop(forward))
cleaner(forward,backward)o([sensor(left(2))]).
0 : sense(left(2)) : sensor(left(2))
1 : learn(left) : fetch(forward)
1 : detect(left(2)) : inhibit(on(left(2)))
2 : move(forward) : excite(at(right(1)))
2 : detect(left(2)) : inhibit(on(left(2)))
3 : move(forward) : excite(at(right(2)))
3 : detect(left(2)) : inhibit(on(left(2)))
4 : move(forward) : excite(at(right(3)))
4 : detect(left(2)) : inhibit(on(left(2)))
5 : move(forward) : excite(at(right(4)))
5 : detect(left(2)) : inhibit(on(left(2)))
6 : move(forward) : excite(at(right(5)))
6 : detect(left(2)) : inhibit(on(left(2)))
7 : move(forward) : excite(at(right(6)))
7 : detect(left(2)) : inhibit(on(left(2)))
8 : move(forward) : excite(at(right(7)))
8 : detect(left(2)) : inhibit(on(left(2)))
9 : move(forward) : inhibit(at(_43862(_43868)))
9 : move(forward) : effector(stop(forward))
9 : detect(left(2)) : inhibit(on(left(2)))
9 : detect(left(2)) : inhibit(on(left(2)))
9 : detect(left(2)) : inhibit(on(left(2)))
cleaner(forward,backward)o([sensor(left(1))]).
0 : sense(left(1)) : sensor(left(1))
1 : learn(left) : fetch(backward)
1 : detect(left(1)) : inhibit(on(left(1)))
2 : move(backward) : excite(at(left(1)))
2 : detect(left(1)) : excite(on(left(1)))
3 : detect(left(1)) : effector(clear(left(1)))
3 : move(backward) : excite(at(left(2)))
3 : synchro(detect(left(1)),move(backward)) : sync(detect(left(1)),move(backward))
4 : move(backward) : excite(at(left(3)))
4 : ltp(sense(left(1)),recall(backward)) : weight(sense(left(_37664)),recall(backward))o(1)
5 : move(backward) : excite(at(left(4)))
5 : ltd(sense(left(1)),learn(left)) : weight(sense(left(_14100)),learn(left))o(0)
6 : move(backward) : excite(at(left(5)))
7 : move(backward) : excite(at(left(6)))
8 : move(backward) : excite(at(left(7)))
9 : move(backward) : inhibit(at(_37904(_37910)))
9 : move(backward) : effector(stop(backward))
*/

:- set_prolog_flag(allow_variable_name_as_functor,false).
%:- listing(bonzon_op/1).
