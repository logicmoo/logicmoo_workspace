%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 13/07/96   File: lemma:config.pl              %%
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 13/07/96 Created                                                          %%
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog lemma:config.pl                                           %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- lemma_handling.                         % default is LEMMA HANDLING

:- lemma_mode(dynamic).                    % default is to use DYNAMIC lemmas only

:- lemma_type(delta).                      % default is to use DELTA lemmas only

:- lemma_format(unit).                     % default is to use UNIT lemmas only

add_lemmatization_p(Head :- Body) :-
	lemma_flag,
	!,
	(functor(Head,query,_) -> fail;
         functor(Head,alpha,_) -> fail;
	 functor(Head,gamma,_) -> lemma_type_parameter(delta);
	 true ->                  lemma_type_parameter(omega)).

dynamic_lemma_test_p(P,N) :-
	dynamic_lemma_flag,
	!,
	(P == query -> fail;
         P == alpha -> fail;
	 P == gamma -> lemma_type_parameter(delta);
	 true       -> lemma_type_parameter(omega)).

static_lemma_test_p(P,N) :-
	(static_lemma_flag;dystatic_lemma_flag),
	!,
	(P == query -> fail;
         P == alpha -> fail;
	 P == gamma -> lemma_type_parameter(delta);
	 true       -> lemma_type_parameter(omega)).
