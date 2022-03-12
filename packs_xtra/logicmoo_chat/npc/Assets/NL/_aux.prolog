
:-if(current_prolog_flag(use_old_code_to,true)).

%%
%% Auxilliary verb forms (will, do have, be)
%%

:- randomizable opt_will//1.


%=autodoc
%% opt_will( ?ARG1) is semidet.
%
% Opt Will.
%
opt_will(future)-->theTextM1(will).


%=autodoc
%% opt_will( ?ARG1, ?ARG2, ?X) is semidet.
%
% Opt Will.
%
opt_will(past, X, X).
opt_will(present, X, X).

:- randomizable aux_do//2.


%=autodoc
%% aux_do( ?ARG1, ?ARG2) is semidet.
%
% Aux Do.
%
aux_do(present, Agreement)-->theTextM1(do), {Agreement\=third:singular}.
aux_do(present, third:singular) -->  
  theTextM1(does).
aux_do(past, _Agreement)-->theTextM1(did).

:- randomizable aux_have//2.


%=autodoc
%% aux_have( ?ARG1, ?ARG2) is semidet.
%
% Aux Have.
%
aux_have(present, Agreement)-->theTextM1(have), {Agreement\=third:singular}.
aux_have(present, third:singular) -->  
  theTextM1(has).
aux_have(past, _Agreement)-->theTextM1(had).
aux_have(future, _Agreement)-->theTextM1(have).

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
aux_be(future, _Agreement)-->theTextM1(be).


:- endif.
