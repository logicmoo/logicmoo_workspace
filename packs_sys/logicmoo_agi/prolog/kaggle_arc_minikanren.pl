#!/usr/bin/env clif
/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (xxxxx.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(kanren,[krepl/0]).
:- use_module(library(logicmoo_utils)).
:- use_module(library(wam_cl/sreader)).
:- create_prolog_flag(lisp_repl_goal,krepl,[keep(false),type(term)]).
:- wots(_,weto(ensure_loaded(library(wam_cl/repl)))).
:- wots(_,weto(ensure_loaded(library(wamclrt)))).

krepl:- 
 in_md(cl,(
   nop(lisp_banner),   
   set_prolog_flag(lisp_primordial,false), % requires  "PACKAGE:SYM" to already externally exists
   with_prompt_str('> ',
   ((	repeat,
        catch(read_k_eval_print(Result),'$aborted',fail),
   	quietly(Result == end_of_file)))))),!.

read_k_eval_print(Result):-
 cl:((
        ignore(catch(lquietly(set_prompt_from_package),_,true)),
        set_md_lang(cl),
        get_prompt_from_package('> ',Prompt),prompt1(Prompt),
        lquietly(show_uncaught_or_fail(read_no_parse(Expression))),!,       
        lquietly(show_uncaught_or_fail(lisp_add_history(Expression))),!,
        nb_linkval('$mv_return',[Result]),
        set_md_lang(prolog),
        show_uncaught_or_fail(kanren:eval_at_krepl(Expression,Result)),!,
        lquietly(show_uncaught_or_fail(write_results(Result))))),!.


k_eval_only:- true.

:- export(k_eval/2).
k_eval(Exp, R) :-
  prologue(P),!,
  eval_exp([let, P, Exp], e{}, R),!.


% basic EVAL statements for built-in procedures
eval_at_krepl(Var,  R):- quietly(var(Var)),!, R=Var.
% hook to his
eval_at_krepl(Expression, Result):- kanren:k_eval(Expression,Result),!.
eval_at_krepl(_Expression, _Result):- k_eval_only, !, fail.

eval_at_krepl(Expression, Result):-  eval_krepl_hooks(Expression,Result),!.

eval_at_krepl(Expression,Result):- 
 notrace((tracing, \+ t_l:rtracing)),
  call_cleanup(eval_at_krepl_tracing(Expression,Result),trace).
eval_at_krepl(Expression,Result):-
 cl:((
  lquietly(as_sexp(Expression,SExpression)),
  (reader_intern_symbols(SExpression,LExpression)),
  quietly(dbginfo(:- lisp_compiled_eval(LExpression))),
  quietly(debug_var('ReplEnv',Env)),
  timel('COMPILER',always_catch(maybe_ltrace(lisp_compile(Env,Result,LExpression,Code)))),
  quietly(dbginfo(:-Code)),
  (notrace(tracing)-> (user:Code) ; 
   timel('EXEC',always_catch(ignore(always(maybe_ltrace(call(user:Code))))))))),!.

eval_at_krepl_tracing(Expression,Result):-
 cl:((
  lquietly(as_sexp(Expression,SExpression)),
  (reader_intern_symbols(SExpression,LExpression)),
  writeq((reader_intern_symbols(SExpression,LExpression))),nl,
  quietly(debug_var('ReplEnv',Env)),
  %quietly(cls),
   draw_cline,draw_cline,draw_cline,
   userout(:- lisp_compiled_eval(LExpression)),
   draw_cline,draw_cline,draw_cline,
  timel('COMPILE',(offer_rtrace(lisp_compile(Env,Result,LExpression,Code)))),
  % quietly(cls),
   draw_cline,draw_cline,draw_cline,
   userout(:-Code),
   draw_cline,timel('PREEXEC',(offer_rtrace((user:Code)))),
   draw_cline,draw_cline,draw_cline,
  timel('EXEC',(offer_rtrace((user:Code)))))).


:- abolish(cl:eval/2).
:- assert(cl:eval(Expr,R):- kanren:eval_k(Expr,R)).
:- abolish(cl:eval/3).
:- assert(cl:eval(Expr,Env,R):- kanren:eval_k(Expr,Env,R)).

% hook to his
eval_k(Expression, Result):- kanren:k_eval(Expression,Result),!.
eval_k(_Expression, _Result):- k_eval_only,!, fail.
eval_k(Expression, Result):- current_env(Env), eval_k(Expression, Env, Result),!.

/* Like eval_exp, but unwraps type boxes. This is where we ground out for
 * prolog interop */
eval_uexp(Exp, Env, R) :-
  eval_exp(Exp, Env, Typed),
  untyped(Typed, R),!.

eval_k(Expression, Env, Result):-
  cl:((
   always_catch(maybe_ltrace(lisp_compile(Env,Result,Expression,Code))), 
   always_catch(ignore(always(maybe_ltrace(call(user:Code))))))),!.
/*

eval_krepl_hooks(V,_):-var(V),!,fail.
eval_krepl_hooks(nil,  []):-!.
eval_krepl_hooks(KW, Ret):- atom(KW),atom_concat(':',UC,KW),
  downcase_atom(UC,DC),atom_concat('kw_',DC,US),!,eval_krepl_hooks(US,Ret).
% :cd t
eval_krepl_hooks(KW, Ret):- 
 cl:((
 is_keywordp(KW),to_prolog_string(KW,PStr),name(UC,PStr),downcase_atom(UC,Atom),
  user:((current_predicate(Atom/2)-> (read_prolog_object(PrologArg),call(Atom,PrologArg,Ret));
  (current_predicate(Atom/1)-> (read_prolog_object(PrologArg),!,t_or_nil(call(Atom,PrologArg),Ret));
  (current_predicate(Atom/0)-> (call(Atom)))))))).

% make.  ls.  pwd. 
eval_krepl_hooks(Atom, R):- atom(Atom),atom_concat_or_rtrace(_,'.',Atom),
 cl:((
  quietly(catch(read_term_from_atom(Atom,Term,[variable_names(Vs),syntax_errors(true)]),_,fail)),
  callable(Term),current_predicate(_,Term),b_setval('$variable_names',Vs),
  t_or_nil((user:call(Term)*->userout(yes(Term));(userout(no(Term)),fail)),R))).

eval_krepl_hooks([debug,A], t):- !,debug(lisp(A)).
eval_krepl_hooks([nodebug,A], t):- !, nodebug(lisp(A)).

eval_krepl_hooks([UC|A], R):- atom(UC),downcase_atom(UC,DC),DC\==UC,!,eval_krepl_hooks([DC|A], R).

eval_krepl_hooks([X], R):-!, cl:eval_repl_atom( X, R),!.
eval_krepl_hooks( X , R):- cl:eval_repl_atom( X, R),!.
*/

%:- fixup_exports.

% :- set_prolog_flag(verbose_autoload,false).

:- use_module(library(shell)).

:- initialization((in1t:lisp_repl),main).

:- if(false).
:- if(getuid(1006)).
:- use_module(library(eggdrop)).
:- initialization((do_wamcl_inits,egg_go_fg),main).

eggdrop:lisp_call([S|TERM],_Vs,R):- lisp_compiled_eval([S|TERM],R).
:- endif. % getuid(1006)
:- endif. % false
%:- process_si.
%:- cddd.


%:- use_module(library(reif)).
:- export(
    (if_/3,
     cond_t/3,
     (=)/3,
     dif/3,
     (',')/3,
     (;)/3,
     memberd_t/3,
     tmember/2,
     tmember_t/3,
     tfilter/3,
     tpartition/4
    )).

/** <module> Reified if, reification library

@author Ulrich Neumerkel
*/


:- meta_predicate
    if_(1, 0, 0),
    cond_t(1, 0, ?),
    tfilter(2, ?, ?),
    tpartition(2, ?, ?, ?),
    ','(1, 1, ?),
    ;(1, 1, ?),
    tmember(2, ?),
    tmember_t(2, ?, ?).

:- op(900, fy, [$]).

% uwnportray(T) :- write_term(T,[quoted(true)]),nl.

uwnportray(T) :- portray_clause(T).  % Item#539

$(X) :- uwnportray(call-X),X,uwnportray(exit-X).
$(C,V1) :-
   $call(C,V1).
$(C,V1,V2) :-
   $call(C,V1,V2).
$(C,V1,V2,V3) :-
   $call(C,V1,V2,V3).
$(C,V1,V2,V3,V4) :-
   $call(C,V1,V2,V3,V4).
$(C,V1,V2,V3,V4,V5) :-
   $call(C,V1,V2,V3,V4,V5).
$(C,V1,V2,V3,V4,V5,V6) :-
   $call(C,V1,V2,V3,V4,V5,V6).
$(C,V1,V2,V3,V4,V5,V6,V7) :-
   $call(C,V1,V2,V3,V4,V5,V6,V7).

goal_expanded(MG_0, MGx_0) :-
   var(MG_0),
   !,
   MG_0 = MGx_0.
goal_expanded(call(MG_1, X), MGx_0) :-
   MG_1 = M:G_1, atom(M), callable(G_1), G_1 \= (_:_),
   functor_(G_1, G_0, X),
   \+ predicate_property(M:G_0, (meta_predicate _)),
   !,
   MGx_0 = M:G_0.
goal_expanded(call(G_0), Gx_0) :-
   acyclic_term(G_0),
   nonvar(G_0),
   % more conditions
   !,
   G_0 = Gx_0.
goal_expanded(MG_0, MG_0).


functor_(T, TA, A) :-
   functor(T, F, N0),
   N1 is N0+1,
   functor(TA, F, N1),
   arg(N1, TA, A),
   sameargs(N0, T, TA).

sameargs(N0, S, T) :-
   N0 > 0,
   N1 is N0-1,
   arg(N0, S, A),
   arg(N0, T, A),
   sameargs(N1, S, T).
sameargs(0, _, _).


/*
  no !s that cut outside.
  no variables in place of goals
  no malformed goals like integers
*/


/* 2do: unqualified If_1: error
*/

:- multifile
        system:goal_expansion/2.
:- dynamic
        system:goal_expansion/2.

system:goal_expansion(if_(If_1, Then_0, Else_0), G_0) :-
   ugoal_expansion(if_(If_1, Then_0, Else_0), G_0).

ugoal_expansion(if_(If_1, Then_0, Else_0), Goal) :-
   nonvar(If_1), If_1 = (X = Y),
   goal_expanded(call(Then_0), Thenx_0),
   goal_expanded(call(Else_0), Elsex_0),
   !,
   Goal =
      ( X \= Y -> Elsex_0
      ; X == Y -> Thenx_0
      ; X = Y,    Thenx_0
      ; dif(X,Y), Elsex_0
      ).
ugoal_expansion(if_(If_1, Then_0, Else_0), Goal) :-
   subsumes_term((A_1;B_1), If_1),
   (A_1;B_1) = If_1,
   !,
   Goal = if_(A_1, Then_0, if_(B_1, Then_0, Else_0)).
ugoal_expansion(if_(If_1, Then_0, Else_0), Goal) :-
   subsumes_term((A_1,B_1), If_1),
   (A_1,B_1) = If_1,
   !,
   Goal = if_(A_1, if_(B_1, Then_0, Else_0), Else_0).
ugoal_expansion(if_(If_1, Then_0, Else_0), Goal) :-
   goal_expanded(call(If_1, T), Ifx_0),
   goal_expanded(call(Then_0), Thenx_0),
   goal_expanded(call(Else_0), Elsex_0),
   Goal =
      (  Ifx_0,
         (  T == true -> Thenx_0
         ;  T == false -> Elsex_0
         ;  nonvar(T) -> throw(error(type_error(boolean,T),
                               type_error(call(If_1,T),2,boolean,T)))
         ;  throw(error(instantiation_error,
                               instantiation_error(call(If_1,T),2)))
         )
      ).

if_(If_1, Then_0, Else_0) :-
   call(If_1, T),
   (  T == true -> Then_0
   ;  T == false -> Else_0
   ;  nonvar(T) -> throw(error(type_error(boolean,T),
                               type_error(call(If_1,T),2,boolean,T)))
   ;  throw(error(instantiation_error,instantiation_error(call(If_1,T),2)))
   ).


tfilter(C_2, Es, Fs) :-
   i_tfilter(Es, C_2, Fs).

i_tfilter([], _, []).
i_tfilter([E|Es], C_2, Fs0) :-
   if_(call(C_2, E), Fs0 = [E|Fs], Fs0 = Fs),
   i_tfilter(Es, C_2, Fs).

tpartition(P_2, Xs, Ts, Fs) :-
   i_tpartition(Xs, P_2, Ts, Fs).

i_tpartition([], _P_2, [], []).
i_tpartition([X|Xs], P_2, Ts0, Fs0) :-
   if_( call(P_2, X)
      , ( Ts0 = [X|Ts], Fs0 = Fs )
      , ( Fs0 = [X|Fs], Ts0 = Ts ) ),
   i_tpartition(Xs, P_2, Ts, Fs).

=(X, Y, T) :-
   (  X == Y -> T = true
   ;  X \= Y -> T = false
   ;  T = true, X = Y
   ;  T = false,
      dif(X, Y)
   ).

dif(X, Y, T) :-
  =(X, Y, NT),
  non(NT, T).

non(true, false).
non(false, true).

','(A_1, B_1, T) :-
   if_(A_1, call(B_1, T), T = false).

;(A_1, B_1, T) :-
   if_(A_1, T = true, call(B_1, T)).

cond_t(If_1, Then_0, T) :-
   if_(If_1, ( Then_0, T = true ), T = false ).

memberd_t(E, Xs, T) :-
   i_memberd_t(Xs, E, T).

i_memberd_t([], _, false).
i_memberd_t([X|Xs], E, T) :-
   if_( X = E, T = true, i_memberd_t(Xs, E, T) ).

tmember(P_2, [X|Xs]) :-
   if_( call(P_2, X), true, tmember(P_2, Xs) ).

tmember_t(P_2, [X|Xs], T) :-
   if_( call(P_2, X), T = true, tmember_t(P_2, Xs, T) ).

:- use_module(library(pairs)).

not_in_list(K, L) :-
  if_((L = []),
    true,
    ([X | More] = L,
     dif(K, X),
     not_in_list(K, More))).

not_in_env(K, Env) :-
  /* \+ get_dict(K, Env, _) */
  dict_pairs(Env, _, Pairs),
  pairs_keys(Pairs, Keys),
  not_in_list(K, Keys).

/* Binds a single binding form to a value. Symbols are assigned directly. */
bind(K, V, Env, Env2) :-
  atom(K),
  Env2 = Env.put(K, V).

/* Vectors are recursively destructured. &(V) denotes varargs. */
bind([], _, Env, Env).
bind([K|Ks], Vs, Env, Env3) :-
  /* Varargs */
  (K = &(K2),
   bind(K2, Vs, Env, Env3), !) ;
  /* Out of values */
  ([] = Vs,
   bind(K, nil, Env, Env2),
   bind(Ks, [], Env2, Env3));
  /* Standard vector of bindings: bind first and recur */
  ([V1|V1s] = Vs,
   bind(K,  V1, Env, Env2),
   bind(Ks, V1s, Env2, Env3)).

/* Self-evaluating literals */
eval_lit(Exp, _Env, Exp) :-
  Exp = [] ;
  Exp = nil ;
  Exp = true ;
  string(Exp) ;
  number(Exp).

/* a bare variable x which is in scope */
eval_variable(Var, Env, R) :-
  atom(Var),
  get_dict(Var, Env, R).

/* Evaluates all elements of list */
eval_each([], _, []).
eval_each(Exp, Env, R) :-
  [X | More] = Exp,
  [XR | MoreR] = R,
  eval_exp(X, Env, XR),
  eval_each(More, Env, MoreR).

/* Binds vars to args in env, evaluates body, yielding R. */
call_closure(Vars, Args, Body, Env, R) :-
  bind(Vars, Args, Env, Env2),
  eval_exp(Body, Env2, R).

/* [f, a, b] */
eval_application([Fun | Args], Env, R) :-
  dif(Fun, quote),
  /* format("Trying to apply ~w to ~w~n", [Fun, Args]), */
  /* Evaluate operator in env, yielding a closure */
  eval_uexp(Fun, Env, ?(Type, Vars, Body, Env2)),
  /* format("Evaluated fun.~n", []), */
  /* Depending on the type of the closure... */
  ((Type = lambda,
    /* Lambdas evaluate their arguments */
    eval_each(Args, Env, ArgsR),
    /* format("args evaluated~n", []), */
    /* And invoke their body with those evaluated args. */
    call_closure(Vars, ArgsR, Body, Env2, R), !) ;
   (Type = macro,
    /* Macros receive unevaluated arguments, and invoke their body to generate
    an expression */
    call_closure(Vars, Args, Body, Env2, Code),
    /* Which is then evaluated */
    eval_exp(Code, Env, R))).

/* [lambda [a b] body1 body2] or [macro ...]*/
eval_lambda([Type, Args | Body], Env, R) :-
  /* Must be either macro or lambda, and not shadowed */
  ((Type = lambda, not_in_env(lambda, Env)) ;
   (Type = macro,  not_in_env(macro, Env))),
  /* Construct a closure with the type, argument, body, and env. The
  environment includes a `recur` target which is bound to the closure itself.
  HOLY SHIT, you can just DO this and Prolog will LET you */
  R = ?(Type, Args, [do | Body], Env.put(recur, R)).

eval_do([do], _Env, nil).
eval_do([do, X], Env, R) :-
  eval_exp(X, Env, R).
eval_do([do, X1, X2 | More], Env, R) :-
  eval_exp(X1, Env, _),
  eval_do([do, X2 | More], Env, R).

/* [let [a val b val] body1 body2 ...] */
eval_let([let, [] | Body], Env, R) :-
  eval_do([do | Body], Env, R).
eval_let([let, [K, V | More] | Body], Env, R) :-
  eval_exp(V, Env, VR),
  bind(K, VR, Env, Env2),
  eval_let([let, More | Body], Env2, R).

/* [cond test branch test branch default] */
eval_cond([cond], _Env, nil).
eval_cond([cond, Default], Env, R) :-
  eval_exp(Default, Env, R).
eval_cond([cond, Test, Branch | More], Env, R) :-
  eval_uexp(Test, Env, TestR),
  if_((nil = TestR),
    eval_cond([cond | More], Env, R),
    eval_exp(Branch, Env, R)).

/* [quote x] */
eval_quote([quote, R], Env, R) :-
  /* Ensure quote isn't shadowed */
  not_in_env(quote, Env).

/* M-M-M-METACIRCULARRRRR */
eval_eval([eval, X], Env, R) :-
  eval_exp(X, Env, XR),
  eval_exp(XR, Env, R).

/* Matches a typebox */
is_typed(t(_,_)).

/* Every compound other than a lambda or typebox is a struct. */
is_struct(X) :-
  compound(X),
  compound_name_arity(X, T, _),
  dif(T, ?),
  dif(T, t),
  dif(T, '[|]').

/* Relational version */
is_typed_or_struct(X, R) :-
  (is_typed(X) ; is_struct(X)), R = true , ! ;
  R = false.

type(X, R) :-
  (atomic(X),
    ([] = X ,                         R = list) ;
    (atom(X),                         R = atom) ;
    (number(X),                       R = number) ;
    (string(X),                       R = string)), ! ;
  (compound(X),
    ((functor(X, '[|]', _),                     R = list), ! ;
     (compound_name_arity(X, ?, _),             R = function), ! ;
     (compound_name_arguments(X, t, [Type, _]), R = Type), ! ;
     (compound_name_arity(X, Type, _),          R = Type))).

/* True if X is of type Type; explodes with an error otherwise. */
typecheck(Type, X) :-
  type(X, Type), ! ;
  (format("Type error: expected ~p, received ~p~n",
    [Type, X]),
   false).

/* Unwraps type boxes */
untyped(X, R) :-
  (X = t(_, Value),
   R = Value) ;
  (dif(X, t(_, _)),
    R = X).

/* With one arg, type gets the type of an expression. */
eval_type([type, X], Env, R) :-
  not_in_env(type, Env),
  eval_exp(X, Env, XR),
  type(XR, R).

/* This is the worst hack. (type cat x) serves both as a type-returning
 * introspection, and also as a type assertion, both at runtime. If x is an
 * untyped object, the type call succeeds and constructs a transparent type
 * wrapper typed(name, X). This wrapper is transparent in that it is unwrapped
 * within eval_exp--except, of course, for calls to eval_type!--most language
 * features don't see the type-box's existence.
 *
 * When calling (type cat x) with a type-boxed x, this serves as a type
 * *assertion*: x is returned unchanged iff its type is cat; otherwise, this
 * explodes with a type error. */
eval_type([type, Name, X], Env, R) :-
  not_in_env(type, Env),
  eval_exp(X, Env, XR),
  /* Type boxes and structs must match type Name; otherwise, wrap them in a
   * type box. */
  is_typed_or_struct(XR, Typed),
  if_(Typed = true,
    (typecheck(Name, XR),
     R = XR),
    /* Construct type box around untyped value. */
    (R = t(Name, XR))).

/* An explicit typecheck: like type, but does not promote untyped values to
 * typed ones. Note that you can [check list []] transparently, but [type list []] introduces a type wrapper. */
eval_type([check, Name, X], Env, R) :-
  not_in_env(check, Env),
  eval_exp(X, Env, R),
  typecheck(Name, R).

/* Used to unbox typed objects, allowing you to write map, filter, etc. lmao,
 * not even 2 hours and already I need type polymorphism. */
eval_type([untype, X], Env, R) :-
  not_in_env(untype, Env),
  eval_exp(X, Env, XR),
  untyped(XR, R).

/* [list x y] */
eval_list([list | Args], Env, R) :-
  /* Ensure it's not shadowed */
  not_in_env(list, Env),
  /* And evaluate */
  eval_each(Args, Env, R).

list_sense(X, R) :-
  if_((nil = X), R = [], R = X).

eval_first([first, List], Env, R) :-
  not_in_env(first, Env),
  eval_uexp(List, Env, ListR),
  if_(([] = ListR),
    R = nil,
    [R|_] = ListR).

eval_rest([rest, List], Env, R) :-
  not_in_env(rest, Env),
  eval_uexp(List, Env, ListR),
  ((ListR = [], R = nil) ;
   (ListR = [_|R])).

eval_cons([cons, Head, Tail], Env, R) :-
  not_in_env(cons, Env),
  eval_exp(Head, Env, HeadR),
  eval_exp(Tail, Env, TailR),
  list_sense(TailR, TailL),
  R = [HeadR | TailL].

eval_eq([eq, A, B], Env, R) :-
  not_in_env(eq, Env),
  eval_exp(A, Env, ARes),
  eval_exp(B, Env, BRes),
  if_((ARes = BRes), R = true, R = nil).

eval_plus([plus, A, B], Env, R) :-
  eval_uexp(A, Env, AR),
  eval_uexp(B, Env, BR),
  R is AR + BR.

prn_helper([]) :-
  format("~n").
prn_helper([X | Xs]) :-
  format("~p ", [X]),
  prn_helper(Xs).
eval_prn([prn | Args], Env, nil) :-
  not_in_env(prn, Env),
  eval_each(Args, Env, ArgsR),
  prn_helper(ArgsR).

/* We use functors for structs */
eval_struct([struct, Type | Fields], Env, R) :-
  not_in_env(struct, Env),
  eval_each(Fields, Env, FieldsR),
  compound_name_arguments(R, Type, FieldsR).

eval_struct([destruct, Type, Struct], Env, R) :-
  not_in_env(destruct, Env),
  eval_uexp(Struct, Env, StructR),
  ((atomic(StructR),
   format("Can't destruct atom ~w~n", [StructR]),
   false) ;
  (compound(StructR),
   compound_name_arguments(StructR, Type, R))).

eval_gensym([gensym, Prefix], _Env, R) :-
  atom_concat(Prefix, '__auto__', Sym),
  gensym(Sym, R).

/* Symbol concatenation */
eval_symcat([symcat, A, B], Env, R) :-
  eval_exp(A, Env, AR),
  eval_exp(B, Env, BR),
  atom_concat(AR, BR, R).

eval_env(env, Env, Env) :-
  not_in_env(env, Env).

eval_exp(Exp, Env, R) :-
  /* format('eval ~w~n', [Exp]), */
  eval_quote(Exp, Env, R), ! ;
  eval_lambda(Exp, Env, R), ! ;
  eval_do(Exp, Env, R), ! ;
  eval_let(Exp, Env, R), ! ;
  eval_cond(Exp, Env, R), ! ;
  eval_eq(Exp, Env, R), ! ;
  eval_list(Exp, Env, R), ! ;
  eval_cons(Exp, Env, R), ! ;
  eval_first(Exp, Env, R), ! ;
  eval_rest(Exp, Env, R), ! ;
  eval_plus(Exp, Env, R), ! ;
  eval_struct(Exp, Env, R), ! ;
  eval_prn(Exp, Env, R), ! ;
  eval_env(Exp, Env, R), ! ;
  eval_gensym(Exp, Env, R), ! ;
  eval_symcat(Exp, Env, R), ! ;
  eval_type(Exp, Env, R), ! ;
  eval_eval(Exp, Env, R), ! ;
  eval_lit(Exp, Env, R), ! ;
  eval_variable(Exp, Env, R), ! ;
  eval_application(Exp, Env, R), ! ;
  (format('unable to eval ~p~n', [Exp]),
   false).

prologue([
  /* Booleans ************************************************************/
  and, [macro, [a, b],
    [list, [quote, cond], a, b]],

  or, [macro, [a, b],
    [let, [a_, [gensym, a]],
      [list, [quote, let],
             [list, a_, a],
             [cond, a_, a_, b]]]],

  not, [lambda, [x], [cond, x, nil, true]],


  /* Predicates ********************************************************/
  is_null, [lambda, [x],
    [let, [x, [untype, x]],
      [or, [eq, x, []],
           [eq, x, nil]]]],

  is_empty, [lambda, [coll], [eq, [], [untype, coll]]],

  is_list, [lambda, [x],
    [eq, [type, [untype, x]], [quote, list]]],

  is_pair, [lambda, [x],
    [and, [not, [is_empty, x]],
          [is_list, x]]],

  is_fn, [lambda, [x],
    [eq, [type, [untype, x]], [quote, function]]],

  /* Basic sequence operations, polymorphic over all types. */
  second, [lambda, [coll], [first, [rest, coll]]],
  third,  [lambda, [coll], [first, [rest, [rest, coll]]]],

  count, [lambda, [coll],
    [cond, [is_empty, coll],
      0,
      [plus, 1, [recur, [rest, coll]]]]],

  map, [lambda, [f, coll],
    [cond, [is_empty, coll],
      [],
      [cons, [f, [first, coll]],
             [recur, f, [rest, coll]]]]],

  filter, [lambda, [f, coll],
    [cond, [is_empty, coll],
      [],
      [let, [x,  [first, coll],
             xs, [rest, coll]],
        [cond, [f, x],
          [cons, x, [recur, f, xs]],
          [recur, f, xs]]]]],

  fold, [lambda, [f, init, coll],
    [cond, [is_empty, coll], init,
           [recur, f, [f, init, [first, coll]], [rest, coll]]]],

  rev, [lambda, [coll],
    [fold, [lambda, [list, elem], [cons, elem, list]], [], coll]],

  concat, [lambda, [as, bs],
    [fold, [lambda, [res, a],
             [cons, a, res]],
          bs,
          [rev, as]]],

  /* Control flow *****************************************************/

  /* Passes through nils, typechecks otherwise. */
  check_option, [macro, [type, expr],
    [let, [res_, [gensym, res]],
      [list, [quote, let], [list, res_, expr],
        [list, [quote, cond], [list, [quote, eq], nil, res_],
               nil,
               [list, [quote, check], type, res_]]]]],

  assert, [macro, [expr, message],
    [list, [quote, cond],
           expr,
           true,
           [list, [quote, do],
             [list, [quote, prn], "Assert failed:",
                    [list, [quote, quote], expr],
                    message],
             [quote, throw]]]],

  /* Apply a function to every node of a tree (list), preorder application,
   * recursively */
  treemap, [lambda, [f, x],
    [let, [treemap, recur,
           x2,      [f, x]],
      [cond, [is_pair, x2],
        [cons, [f, [first, x2]], [recur, f, [rest, x2]]],
        x2]]],

  /* Utilities ***********************************************************/

  /* Syntax-quote takes an expression and quotes it, except for [unquote, x]
   * forms, which unquotes a single term. */
  syntax_quote_fn, [lambda, [expr],
    [let, [q, [quote, quote],
           sq, recur],
      [cond, [is_empty, expr], expr,

             [is_list, expr],
             [let, [f, [first, expr]],
               [cond, /* Unquote means *don't* descend into this term. */
                      [eq, f, [quote, unquote]],
                      [second, expr],

                      /* Otherwise, recur into term and build up a list expr. */
                      [cons, [quote, list],
                             [map, sq, expr]]]],

             /* Non-list terminals are all quoted. */
             [list, q, expr]]]],

  syntax_quote, [macro, [expr], [syntax_quote_fn, expr]],

  syntax_quote_test, [macro, [x],
    [syntax_quote, [prn, [list, 1], [unquote, x]]]],

  /* Rewrites closures to be more readable. */
  unfn, [lambda, [x],
          [first,
            [treemap, [lambda, [x],
                       /* Print functions without envs */
                       [cond, [is_fn, x],
                              [let, [[type, args, [do, &(body)]],
                                     [destruct, ?, x]],
                                [struct, ?, args, body]],
                             x]],
                     [list, x]]]],

  /* MICROKANREN ************************************************************/

  /* Logic vars are their own struct type. We do this for safety--it's too easy
   * to accidentally destructure/cons/etc lvars otherwise. */
  lvar,     [lambda, [num], [struct, lvar, num]],
  is_lvar,  [lambda, [x], [eq, [quote, lvar], [type, x]]],
  lvar_num, [lambda, [x], [first, [destruct, foo, x]]],

  /* We represent a binding as a struct as well. */
  binding,    [lambda, [var, val],
                [struct, binding, var, val]],
  is_binding, [lambda, [x], [eq, [type, x], [quote, binding]]],
  bvar,       [lambda, [b], [first, [destruct, binding, b]]],
  bval,       [lambda, [b], [second, [destruct, binding, b]]],

  /* A substitution map is a list of bindings */
  empty_subs, [type, subs, []],

  /* Returns the binding of lvar num in substitution subs */
  lvar_binding, [lambda, [num, subs],
    [first, [filter, [lambda, [x],
                       [eq, num, [first, x]]],
                     subs]]],

  /* A state is a substitution paired with a fresh variable counter. */
  state,        [lambda, [subs, counter],
                  [check, subs, subs],
                  [struct, state, subs, counter]],
  is_state,     [lambda, [x], [eq, [type, x], [quote, state]]],
  empty_state,  [state, empty_subs, 0],
  st_subs,      [lambda, [s], [check, subs, [first, [destruct, state, s]]]],
  st_counter,   [lambda, [s], [second, [destruct, state, s]]],

  /* Search for the value of u in a given substitution. */
  walk, [lambda, [u, subs],
    [check, subs, subs],
    [let, [pr, [and, [is_lvar, u],
                     [first, [filter, [lambda, [v],
                                        /* u is equal to the binding's lvar */
                                        [eq, u, [bvar, v]]],
                                      subs]]],
           res, [cond, pr,
                  [recur, [bval, pr], subs],
                  u]],
      /* [prn, "walk", u, [quote, '->'], res, "in", subs], */
      res]],

  /* Adds a binding of x to v to the given substitution. */
  ext_s, [lambda, [x, v, subs],
    [check, subs, subs],
    [type, subs, [cons, [binding, x, v], subs]]],

  /* Unifies u with v in a substitution */
  unify, [lambda, [u, v, subs],
    [check, subs, subs],
    /* Look up u and v in the substitution map */
    [check_option, subs,
      [let, [u, [walk, u, subs],
             v, [walk, v, subs]],
        /* If both are equal, return subs unchanged. This covers both normal
         * values and lvars. */
        [cond, [eq, u, v], subs,

               /* If we have one lvar, and something else (either a value or a
                * different lvar) extend the substitution with that
                * association. */
               [is_lvar, u], [ext_s, u, v, subs],
               [is_lvar, v], [ext_s, v, u, subs],

               /* If both are bindings, unify their vars and vals. The Scheme
                * implementation gets around this by encoding bindings as
                * pairs. TODO: when we're done being type-safe, go back to
                * that. */
               [and, [is_binding, u], [is_binding, v]],
               [let, [subs, [recur, [bvar, u], [bvar, v], subs]],
                 [and, subs, [recur, [bval, u], [bval, v]], subs]],

               /* We unify lists recursively. Note that this works for improper
                * lists too! */
               [and, [is_pair, u], [is_pair, v]],
               [let, [subs, [recur, [first, u], [first, v], subs]],
                 [and, subs, [recur, [rest, u], [rest, v], subs]]]]]]],

  /* M'zero, the empty stream */
  mzero, [type, stream, []],

  unit, [lambda, [state],
    [assert, [is_state, state], "unit"],
    [type, stream, [cons, state, mzero]]],

  /* A goal constructor which asserts u and v are equal */
  eql, [lambda, [u, v],
    [type, goal,
      [lambda, [st],
        [assert, [is_state, st], "eql"],
        [check, stream,
          [let, [subs, [unify, u, v, [st_subs, st]]],
            [cond, subs,
              [unit, [state, subs, [st_counter, st]]],
              mzero]]]]]],
  =, eql,

  /* A goal which introduces a new logical variable. Takes a function [f, lvar]
  which yields a goal, and constructs a goal using it. The constructed goal
  takes a state sc, derives a goal with a fresh logic variable in that state,
  and passes the (evolved) state to the goal that f yields. */
  call_fresh, [lambda, [f],
    [type, goal,
      [lambda, [st],
        [assert, [is_state, st], "call_fresh"],
        /* Construct a new logic variable and call f with it, yielding a goal
         * function.  That goal function gets called with our state, evolved to
         * account for the new variable. */
        [check, stream,
          [let, [c, [st_counter, st]],
            [[check, goal, [f, [lvar, c]]],
             [state, [st_subs, st], [plus, 1, c]]]]]]]],

  /* Merges two alternative streams (i.e. representing the two forks of a
   * disjunction) together. */
  mplus, [lambda, [a, b],
    [check, stream, a],
    [check, stream, b],
    [check, stream,
      [let, [mplus, recur],
        [cond, /* When one stream is exhausted, yield the other. */
               [is_null, a], b,
               /* A fn is a lazy stream; if given one, we yield our own lazy
                * stream which, when evaluated, tries the opposite order: b,
                * then the concrete stream [a]. Flipping a and b is what lets
                * us trade off between streams, ensuring no single fork
                * dominates the search. */
               [is_fn, a], [type, stream, [lambda, [], [mplus, b, [a]]]],
               /* a must be a concrete non-empty list. We yield its first
                * state, followed by the disjunction of later states of a with
                * b. */
               [type, stream,
                 [cons, [first, a], [mplus, [rest, a], b]]]]]]],

  /* Bind applies a goal (a function of states to streams) over a stream. */
  bind, [lambda, [stream, goal],
    [check, stream, stream],
    [check, goal, goal],
    [check, stream,
      [let, [bind, recur],
        [cond, /* If the stream is empty, we return the empty stream. */
               [is_null, stream], mzero,
               /* If the stream is lazy, we construct a lazy stream. */
               [is_fn, stream], [type, stream,
                                  [lambda, [], [bind, [stream], goal]]],
               /* Otherwise, the stream is a concrete, non-empty stream. We
                * have two choices now: either apply the goal to the first
                * thing in the stream, or mapping the goal over the rest of the
                * stream. We express these two options via mplus! */
               [mplus, [goal, [first, stream]],
                       [bind, [type, stream, [rest, stream]], goal]]]]]],

  /* Logical disjunction of two goals. */
  disj, [lambda, [g1, g2],
    [check, goal, g1],
    [check, goal, g2],
    [type, goal,
      [lambda, [state],
        [assert, [is_state, state], "disj"],
        [check, stream,
          [mplus, [g1, state], [g2, state]]]]]],

  /* Logical conjunction of two goals */
  conj, [lambda, [g1, g2],
    [check, goal, g1],
    [check, goal, g2],
    [type, goal,
      [lambda, [state],
        [assert, [is_state, state], "conj"],
        /* We apply the first goal to the state, yielding a stream. We then map
         * the second goal over that stream, ensuring both goals hold. */
        [check, stream,
          [bind, [g1, state], g2]]]]],

  /* Basic uKanren examples */
  fives,  [call_fresh, [lambda, [x], [=, x, 5]]],
  sixes,  [call_fresh, [lambda, [x], [=, x, 6]]],
  either, [call_fresh, [lambda, [x], [disj, [=, x, 3],
                                            [=, x, 4]]]],

  alts, [conj, [call_fresh, [lambda, [x], [=, x, 3]]],
               /* This is a different logic variable! */
               [call_fresh, [lambda, [x], [disj, [=, x, 5], [=, x, 6]]]]],

  /* Evals until realized */
  pull, [lambda, [stream],
    [check, stream, stream],
    [cond, [is_fn, stream],
      [recur, [stream]],
      stream]],

  /* Limited take */
  take, [lambda, [n, stream],
    [cond, [eq, 0, n],
      [],
      [let, [stream, [pull, stream],
             state,  [first, stream]],
        [cond, [is_fn, state],
               [recur, n, state],

               state,
               [do, [assert, [is_state, state], "take"],
                    [cons, state, [recur, [plus, n, -1], [rest, stream]]]],

               /* Out of elements */
               []]]]],

  /* A basic function which takes n solutions from the goal on the empty state
   * */
  run_raw, [lambda, [n, goal],
    [take, n, [goal, empty_state]]],


  /* Reifier **************************************************************/

  /* walkr reifies a term by replacing logic variables in it with their
   * corresponding values in some substitution. */
  walkr, [lambda, [v, subs],
    /* Look up v in subs */
    [let, [v,     [walk, v, subs],
           /* A little mutually-recursive helper to simplify traversal */
           walkr, recur,
           w,     [lambda, [v2], [walkr, v2, subs]]],
      [cond, /* If we have a logic variable, that's all we can do. */
             [is_lvar, v], v,

             /* If we have a binding, walk its var and val, and construct a new
              * binding from that. TODO: if we use lists for bindings, this
              * goes away too. */
             [is_binding, v], [binding, [w, [bvar, v]], [w, [bval, v]]],

             /* Walk lists recursively. */
             [is_pair, v], [cons, [w, [first, v]],
                                  [w, [rest, v]]],

             /* A ground term */
             v]]],

  /* This is how we construct names for logic variables */
  reify_name, [lambda, [n],
    [symcat, [quote, '_'], n]],

  /* Reifies a substitution map, by extending subs with extra entries for any
   * unknowns in v. */
  reify_s, [lambda, [v, subs],
    /* Look up v in subs */
    [let, [v, [walk, v, subs]],
      [cond, /* If we have a logic variable, extend the substitution by binding
             v to a new logic var named after the size of the substitution. */
             [is_lvar, v],
             [let, [n, [reify_name, [count, subs]]],
               [ext_s, v, n, subs]],

             /* If we have a binding, extend the substitution (recursively)
              * with this variable name and value. */
             [is_binding, v],
             [recur, [bval, v], [recur, [bvar, v], subs]],

             /* Recur through pairs */
             [is_pair, v],
             [recur, [rest, v], [recur, [first, v], subs]],

             /* Ground out */
             subs]]],

  reify_state_first_var, [lambda, [state],
    /* [prn],
    [prn, "state", state], */
    /* Look up the first logic variable in this state's substitution. */
    [let, [v, [walkr, [lvar, 0], [st_subs, state]]],
      /* Then reify the substitution for v, starting with an empty
       * substitution, and walk v in *that*. Why? No clue. */
      [prn, "v", v, "subs", [reify_s, v, empty_subs]],
      [walkr, v, [reify_s, v, empty_subs]]]],

  /* Reifies a state with respect to the first n vars */
  reify_state_vars, [lambda, [num_vars, state],
    /* [prn],
    [prn, "state", state], */
    [let, [subs, [st_subs, state]],
      [[lambda, [n, r],
         [cond, [eq, -1, n],
           /* Done */
           r,
           /* Resolve the nth variable */
           [let, [v, [walkr, [lvar, n], subs]],
             /* And resolve that again... */
             [recur, [plus, n, -1],
                     [cons, [walkr, v, [reify_s, v, empty_subs]],
                            r]]]]],
       /* The first variable to resolve, and the list of our resolutions */
       [plus, num_vars, -1],
       []]]],

  /* Reifies states with respect to the first n vars. */
  mk_reify, [lambda, [num_vars, states],
    [prn],
    [prn, "REIFY =============="],
    [map, [lambda, [state],
            [reify_state_vars, num_vars, state]],
          states]],

  /* Shortcut for running goals. Takes a number of solutions to find, a list of
   * variable names which will be bound in the given goal, and reified when
   * returned. */
  run, [macro, [n, vars, &(goals)],
    [syntax_quote,
      [mk_reify, [unquote, [count, vars]],
                 [run_raw, [unquote, n],
                           [unquote,
                            [cons, [quote, fresh],
                                   [cons, vars, goals]]]]]]],

  /* Syntax! **************************************************************/

  /* This defers evaluation of a goal via inverse-eta-delay */
  zzz, [macro, [g],
    [let, [state_, [gensym, state]],
      [syntax_quote,
        [type, goal,
          [lambda, [[unquote, state_]],
            [assert, [is_state, [unquote, state_]],
                     [list, "zzz", [unquote, state_]]],
              [type, stream,
                [lambda, [],
                  [check, stream,
                    [[unquote, g], [unquote, state_]]]]]]]]]],

  /* conj or disj of multiple goals */
  conjall, [macro, [&(gs)],
    [let, [[g, &(gs)], [rev, gs],
            code, [fold, [lambda, [form, goal],
                           [list, [quote, conj],
                                  [list, [quote, zzz], goal],
                                  form]],
                         /* Infinite loops? */
                         [list, [quote, zzz], g],
                         gs]],
          code]],

  disjall, [macro, [&(gs)],
    [let, [[g, &(gs)], [rev, gs],
            code, [fold, [lambda, [form, goal],
                           [list, [quote, disj],
                                  [list, [quote, zzz], goal],
                                  form]],
                         g,
                         gs]],
          code]],

   /* A disjunction of conjunctions */
   conde, [macro, [&(clauses)],
     [let, [code, [cons, [quote, disjall],
                    [map, [lambda, [goals],
                            [cons, [quote, conjall], goals]],
                          clauses]]],
       code]],

   fresh, [macro, [vars, &(goals)],
     [let, [code, [fold, [lambda, [form, var],
                            /* Wrap in [call_fresh, [lambda, [x], ...]] */
                            [list, [quote, call_fresh],
                                   [list, [quote, lambda],
                                          [list, var],
                                          [list, [quote, type],
                                                 [quote, goal],
                                                 form]]]],
                          /* Start with conjunction of all goals */
                          [cons, [quote, conjall], goals],
                          [rev, vars]]],
       code]],

  /* Sequence goals  ****************************************************
   *
   * Adapted from https://github.com/mullr/micrologic/blob/master/src/micro_logic/sequence.clj */

  /* Conso provides an abstraction for (cons head tail) = list. */
  conso, [lambda, [hd, tl, lst],
      [eql, [cons, hd, tl], lst]],

  /* We can assert things about the first and rest of a list now */
  firsto, [lambda, [head, list],
    [fresh, [tail],
      [conso, head, tail, list]]],

  resto, [lambda, [tail, list],
    [fresh, [head],
      [conso, head, tail, list]]],

  /* Here's sequence append */
  /* From microkanren A Lucid Little Logic Language with a Simple Complete
   * Search */
  appendo, [lambda, [as, bs, combined],
    [let, [appendo, recur],
      [conde, [[eql, [], as],
               [eql, bs, combined]],
              [[fresh, [head, tail, rec],
                 /* Pull apart as */
                 [conso, head, tail, as],
                 /* Pull apart combined */
                 [conso, head, rec, combined],
                 /* And we can recur from there */
                 [appendo, tail, bs, rec]]]]]],

  /* Peano numbers */
  succ, [lambda, [n], [cons, [quote, 'S'], n]],
  n0, [list, 0],
  n1, [succ, n0],
  n2, [succ, n1],
  n3, [succ, n2],

  /* Coercions to and from normal numbers */
  peano, [lambda, [n],
    [cond, [eq, 0, n],
      n0,
      [succ, [recur, [plus, -1, n]]]]],
  unpeano1, [lambda, [n],
    [cond, [eq, n, [list, 0]], 0,

           [and, [is_pair, n], [eq, [quote, 'S'], [first, n]]],
           [let, [m, [recur, [rest, n]]],
             [cond, [eq, [type, m], [quote, number]],
               [plus, 1, m],
               [struct, 'S', m]]],

           n]],
  unpeano, [lambda, [x], [treemap, unpeano1, x]],

  /* Successor and addition relations */
  succo, [lambda, [n, next], [eql, [succ, n], next]],
  pluso, [lambda, [a, b, sum],
    [let, [pluso, recur],
      [conde, [[eql, a, n0], [eql, b, sum]],
              [[fresh, [a_, sum_],
                 [succo, a_, a],
                 [succo, sum_, sum],
                 [pluso, a_, b, sum_]]]]]],

  lesso, [lambda, [lesser, greater],
    [fresh, [n], [pluso, lesser, [succ, n], greater]]],

  /* Maximum */
  maxo, [lambda, [a, b, max],
    [conde, [[eql, a, b],   [eql, max, a]],
            [[lesso, a, b], [eql, max, b]],
            [[lesso, b, a], [eql, max, a]]]],


  /* Ensures a and b are within 1 of each other. */
  approxo, [lambda, [a, b],
    [conde, [[eql, a, b]],
            [[succo, a, b]],
            [[succo, b, a]]]],

  /* Construct a leaf node */
  leaf, [lambda, [x],
    [list, [quote, l], x]],

  /* Construct a branch node with left and right parts */
  branch, [lambda, [left, right],
    [list, [quote, b], left, right]],

  /* Converts a compact notation [l,r] to explicit leaf/branch */
  tree, [lambda, [t],
    [cond, [is_list, t],
           [let, [[left, right], t],
             [branch, [recur, left], [recur, right]]],

           [leaf, t]]],

  /* Maps explicit leaf/branch back to compact lists. */
  untree, [lambda, [[type, a, b]],
    [cond, [eq, type, [quote, b]], [list, [recur, a], [recur, b]],
           [eq, type, [quote, l]], a]],

  /* Rotates between two trees: [a, [b, c]] <-> [[a, b], c]*/
  rot_lefto, [lambda, [t1, t2],
    [fresh, [a, b, c],
      [eql, t1, [branch, a, [branch, b, c]]],
      [eql, t2, [branch, [branch, a, b], c]]]],

  /* Left and right rotations */
  rot_botho, [lambda, [t1, t2],
    [conde, [[rot_lefto, t1, t2]],
            [[rot_lefto, t2, t1]]]],

  /* All possible rotations of a node at any depth between two trees. */
  roto, [lambda, [t1, t2],
    [let, [roto, recur],
      /* Rotate this node */
      [conde, [[rot_botho, t1, t2]],
              /* Break apart this tree as a branch */
              [[fresh, [t1l, t1r, t2l, t2r],
                 [eql, t1, [branch, t1l, t1r]],
                 [eql, t2, [branch, t2l, t2r]],
                 [conde,
                   /* And rotate left branch */
                   [[eql, t1r, t2r], [roto, t1l, t2l]],
                   /* Or right branch*/
                   [[eql, t1l, t2l], [roto, t1r, t2r]]]]]]]],

  /* Any number of rotations at any level */
  rotso, [lambda, [t1, t2],
    [let, [rotso, recur],
      [conde, [[eql, t1, t2]],
              [[fresh, [t],
                 [roto, t1, t],
                 [rotso, t, t2]]]]]],

  /* Height of a tree */
  heighto, [lambda, [t, h],
    [let, [heighto, recur],
      [conde, /* Leaf */
              [[fresh, [x],
                 [eql, t, [leaf, x]],
                 [eql, h, n0]]],
              /* Branch */
              [[fresh, [left, right, left_height, right_height, max_height],
                 [eql, t, [branch, left, right]],
                 [heighto, left, left_height],
                 [heighto, right, right_height],
                 [maxo, left_height, right_height, max_height],
                 [succo, max_height, h]]]]]],

  /* Balanced tress have approximately equal heights at every level. Oh god,
   * this is SO expensive. */
  balancedo, [lambda, [t, h],
    [let, [balancedo, recur],
      [conde, /* Leaf nodes are trivially balanced */
              [[fresh, [x], [eql, t, [leaf, x]]]],
              /* Branch nodes are balanced if their left and right heights are
               * approximately equal, and both left and right nodes are
               * themselves balanced. */
              [[fresh, [left, right, left_height, right_height],
                 [eql, t, [branch, left, right]],
                 [balancedo, left],
                 [balancedo, right],
                 [heighto, left, left_height],
                 [heighto, right, right_height],
                 [approxo, left_height, right_height]]]]]],

  /* Takes any tree and finds an equivalent (via rotation) balanced tree. */
  balanceo, [lambda, [tree, balanced],
    [conde, [[rotso, tree, balanced],
             [balancedo, balanced]]]],
             
  /* Look up a variable in an association list */
  lookupo, [lambda, [var, env, r],
    [let, [lookupo, recur],
      [fresh, [k, v, more],
        [eql, env, [cons, [cons, k, v], more]],
        /* Found it! */
        [conde, [[eql, k, var],
                 [eql, v, r]],
                /* Keep looking */
                [[lookupo, var, more, r]]]]]],

  /* How many symbols do you really need */
  symbolo, [lambda, [var],
    [conde, [[eql, var, [quote, f]]],
            [[eql, var, [quote, x]]]]],

  evalo, [lambda, [exp, env, r],
    [let, [evalo, recur],
      [conde,
        /* Application */
        [[fresh, [f, arg, var, body, env2, argr],
           /* Break up [f arg] */
           [eql, exp, [list, f, arg]],
           /* Evaluate f to a closure */
           [evalo, f, env, [list, [quote, ?], var, body, env2]],
           /* And eval arg */
           [evalo, arg, env, argr],
           /* Then evaluate body with arg bound */
           [evalo, body, [cons, [cons, var, argr], env2], r]]],
        /* Lambda */
        [[fresh, [var, body],
           [eql, exp, [list, [quote, lambda], [list, var], body]],
           [eql, r, [list, [quote, ?], var, body, env]]]],
        /* A symbol */
        [[symbolo, exp],
         [lookupo, exp, env, r]]]]]
]).

demos(conde, [run, 2, [q,w,x,y],
               [conde,
                 [[=, [list, x, w, x], q],
                  [=, y, w]],
                 [[=, [list, w, x, w], q],
                  [=, y, w]]]]).

demos(conde_anon, [run, 2, [q],
                    [fresh, [w, x, y],
                      [conde,
                        [[=, [list, x, w, x], q],
                         [=, y, w]],
                        [[=, [list, w, x, w], q],
                         [=, y, w]]]]]).

/* Define a relation which reverses a list. */
demos(nrev, [let, [nrev, [lambda, [l1, l2],
                           [lambda, [sc],
                             /* Either both lists are empty, or... */
                             [[conde, [[=, l1, []],
                                       [=, l2, []]],
                                      /* Or we can pull apart l2 */
                                      [fresh, [h, t],
                                        [=, h, [first, l2]],
                                        [=, tl, [rest, l2]],
                                        /* And reverse t to r */
                                        [fresh, [r],
                                          [nrev, t, r],
                                          [append, r, [list, h], l2]]]],
                              sc]]]],
              [run, 5, [q],
                [nrev, [list], q]]]).


/* Should this work? */
demos(cons, [run, 5, [l, hd, tl],
              [=, [cons, hd, tl], l],
              [=, hd, 1],
              [=, tl, [list,2,3]]]).

/* Balancing trees */
/* ?- demos(balance, Code), k_eval(Code, R). */
demos(balance,
[map, [lambda, [[x]], [untree, x]], [run, 1, [a], [balanceo, [tree, [quote, [[[1,2],3],4]]], a]]]
).


