%%
%% Auxilliary verbs
%%

%% aux(+Gap, ?Polarity, ?Agreement, ?Tense, ?Aspect, ?Form, ?ModalLF)
%  An optional string of auxilliaries and/or negative particle.

:- randomizable aux/9, aux_without_do_support/9.
aux(nogap, affirmative, _Agreement, present, simple, simple, X^X) --> [ ].
aux(np_chat(NP, Case, Agreement), affirmative, Agreement, Tense, simple, base, X^X) -->
   aux_do(Tense, Agreement),
   np_chat(NP, Case, Agreement, nogap, nogap).
aux(Gap, negative, Agreement, Tense, simple, base, X^X) -->  
  aux_do(Tense, Agreement), theTextM1(not), aux_gap(Gap).
aux(Gap, Polarity, Agreement, Tense, Aspect, Form, ModalLF) -->
   aux_without_do_support(Gap, Polarity, Agreement, Tense, Aspect, Form, ModalLF).

:- register_lexical_item(not).



%=autodoc
%% aux_without_do_support( ?ARG1, ?ARG2, ?ARG3, ?ARG4, ?ARG5, ?ARG6, ?ARG7) is semidet.
%
% Aux Without Do Support.
%
aux_without_do_support( Gap, 
  Polarity, Agreement, future, Aspect, 
  Form, X^X) -->  
  ( theTextM1(will)  ,
    opt_not(Polarity), 
    aux_gap(Gap), 
    aux_aspect_future(Aspect, Agreement, Form)).
aux_without_do_support( Gap, 
  Polarity, _Agreement, present, simple, base, 
  P^LF) -->  
  ( theTextM1(ModalAux)  ,
    { modal_aux(ModalAux, ModalAuxLF), 
      apply_modal(ModalAuxLF, P, LF) }, 
    opt_not(Polarity), 
    aux_gap(Gap)).
aux_without_do_support(Gap, Polarity, Agreement, Tense, Aspect, Form, X^X) -->
   aux_aspect(Gap, Polarity, Tense, Aspect, Agreement, Form).

:- register_lexical_item(will).

apply_modal([], P, LF):- !, LF=P.
apply_modal([Modal|AuxLF], P, LF):-  !, apply_modal(Modal, P, M), apply_modal(AuxLF, M, LF).
apply_modal(ModalAuxLF, P, LF):- LF=..[ModalAuxLF, P].

%% aux_aspect_future(?Aspect, ?Agreement, ?Form)
%  An optional string of auxilliaries devoted to aspect (be, have), and/or negative particle.
%  Special case for after the auxilliary will.  Don't have to check for not particle.

:- randomizable aux_aspect_future//3.
aux_aspect_future(simple, _, simple) --> [ ].
aux_aspect_future(progressive, Agreement, present_participle) -->
   aux_be(future, Agreement).
aux_aspect_future(Aspect, Agreement, Form) -->
   aux_have(future, Agreement),
   aux_perfect(Aspect, Agreement, Form).

%% aux_aspect(+Gap, ?Polarity, ?Tense, ?Aspect, ?Agreement, ?Form)
%  An optional string of auxilliaries devoted to aspect (be, have), and/or negative particle.
%  Case where there has been no prior will auxilliary.

:- randomizable aux_aspect//5.
aux_aspect(nogap, affirmative, _, simple, _, simple) --> [ ].
aux_aspect(Gap, Polarity, Tense, progressive, Agreement, present_participle) -->
   aux_be(Tense, Agreement),
   opt_not(Polarity),
   aux_gap(Gap).
aux_aspect(Gap, Polarity, Tense, Aspect, Agreement, Form) -->
   aux_have(Tense, Agreement),
   opt_not(Polarity),
   aux_gap(Gap),
   aux_perfect(Aspect, Agreement, Form).

%% aux_gap(+Gap)
%  Inserts NP for Gap here if need be
aux_gap(nogap) --> [ ].
aux_gap(np_chat(LF, Case, Agreement)) -->
   np_chat(LF, Case, Agreement, nogap, nogap).

:- randomizable aux_perfect//3.


%=autodoc
%% aux_perfect( ?ARG1, ?ARG2, ?ARG3) is semidet.
%
% Aux Perfect.
%
aux_perfect(perfect, _Agreement, past_participle) -->
   [ ].
aux_perfect(perfect_progressive, Agreement, present_participle) -->
   aux_be(past, Agreement).

:- randomizable opt_not//1.


%=autodoc
%% opt_not( ?ARG1) is semidet.
%
% Opt Not.
%
opt_not(affirmative) --> [ ].
opt_not(negative)-->theTextM1(not).

%%%
%%% Lexical entries
%%%

:- randomizable(modal_aux/2).


%=autodoc
%% modal_aux( ?Must1, ?Shall2) is semidet.
%
% Modal Aux.
%
modal_aux(can,can).
modal_aux(may,may).
modal_aux(should,should).
modal_aux(would,would).
modal_aux(could,can).
modal_aux(shall,shall).
modal_aux(must,shall).
modal_aux(X,Y):- parser_chat80:modal_verb_form_aux(X,Y,_,_).



:- register_all_lexical_items([A], modal_aux(A,_)).

:- randomizable aux_do//2.


%=autodoc
%% aux_do( ?ARG1, ?ARG2) is semidet.
%
% Aux Do.
%
aux_do(present, Agreement)-->theTextM1(do), {Agreement\==third:singular}.
aux_do(present, third:singular) -->  
  theTextM1(does).
aux_do(past, _Agreement)-->theTextM1(did).

:- register_lexical_items([do, does, did, doing]).

:- randomizable aux_have//2.


%=autodoc
%% aux_have( ?ARG1, ?ARG2) is semidet.
%
% Aux Have.
%
aux_have(present, Agreement)-->theTextM1(have), {Agreement\==third:singular}.
aux_have(present, third:singular) -->  
  theTextM1(has).
aux_have(past, _Agreement)-->theTextM1(had).
aux_have(future, _Agreement)-->theTextM1(have).

:- register_lexical_items([have, has, had, having]).

:- randomizable aux_be//2.


%=autodoc
%% aux_be( ?ARG1, ?ARG2) is semidet.
%
% Aux Be.
%
aux_be(present, first:singular) -->  
  theTextM1(am).
aux_be(present, second:singular) -->  
  theTextM1(are).
aux_be(present, third:singular) -->  
  theTextM1(is).
aux_be(present, _:plural) -->  
  theTextM1(are).
aux_be(past, first:singular) -->  
  theTextM1(was).
aux_be(past, second:singular) -->  
  theTextM1(were).
aux_be(past, third:singular) -->  
  theTextM1(was).
aux_be(past, _:plural) -->  
  theTextM1(were).
aux_be(Tense, _Agreement) -->
   % Only applies if we already know this is future tense
   { Tense == future },
   [be].

:- register_lexical_items([am, are, is, was, were, be, being]).
