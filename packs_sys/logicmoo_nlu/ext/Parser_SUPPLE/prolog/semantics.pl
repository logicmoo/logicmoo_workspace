
/* ------------------------------------------------------------------------
 > FILENAME:	semantics
 > PURPOSE:	extract sem feature from an edge, replace extraposed
 >              arguments with unique values, flatten any embedding
 >              and convert to predicate/argument representation.
 > AUTHORS:	Kevin Humphreys
 > NOTES:	
 ------------------------------------------------------------------------ */
:- assert('$package_name'('shef.nlp.supple.prolog.cafe')).

cvsid_semantics("$Id: semantics.pl 7085 2005-12-05 16:32:03Z ian_roberts $").


%
% NAME: 	semantics/4
% PURPOSE:	
% ARGS:		(?Cat, ?Activity, +Edge, -Sem)
%               Cat = grammatical category
%               Activity = active/inactive
%               Edge = edge identifier
%               Sem = list of pred/arg structures
%% NOTES:	
%
semantics(Cat,Activity,Edge,Sem) :-
    ((Activity=inactive,edge(Start,End,C,[],_,_,Level,_,_,Edge));
     (Activity=active,edge(Start,End,C,[X|Y],_,_,Level,_,_,Edge))),
    % take only highest level inactive edge if >1 with same Id
%    \+((edge(_,_,_,[],_,_,Level2,_,_,Edge), Level2 > Level)),
    C =.. [Cat|Features],
    member(sem:Val,Features),
    clean_semantics(Val,Sem).

clean_semantics([],[]) :- !.
clean_semantics(E^S,S2) :-                  % generate an atom for each
    buchart_gensym(e,E),                    % external var, and strip var
    clean_semantics(S,S2),!.
clean_semantics([H|T],List) :-
    nonvar(H), H = E^S,
    buchart_gensym(e,E),
    clean_semantics(S,S1),
    clean_semantics(T,S2),
    append(S1,S2,List),!.
clean_semantics([Pred,E,List],[Predicate]) :- % special case for surface forms
    nonvar(Pred),
    % s_forms
    member(Pred,[name,head,head1,head2,det,adj,pronoun,count,more,less,title]),
    semflatten(List,FlatList), % catch errors from grammar rules
    name_conc(FlatList,Name),
    Predicate =.. [Pred,E,Name], !.
clean_semantics([P|Args],[Predicate]) :-  % return predicate representation
    \+(member([X|_],Args)),
    lower(P,LP), % lower case predicate names
    Predicate =.. [LP|Args].
clean_semantics([compound([Verb|List])|Args],[Predicate]) :-
    % return predicate representation of compound verb
    nonvar(Verb),
    ((Verb =.. [compound,[_|_]], % allow for embedding
      clean_semantics([Verb],[Verb2]))
    ; Verb2 = Verb),
    name_conc([Verb2|List],P),
    clean_predicate(P,LP),
    Predicate =.. [LP|Args].
%clean_semantics([P|Args],[Predicate]) :- % instantiate vars with dummy
clean_semantics([P|Args],[]) :- % but don't pass anything back
    \+(P = [_|_]),
    \+(\+((member([X|_],Args),var(X)))),
    member('MISSING',Args).
%    lower(P,LP),
%    Predicate =.. [LP|Args].
%clean_semantics(FlatList,[FlatList]) :-     % return list representation
%   \+(member([X|Y],FlatList)),!.
clean_semantics([H|T],FlatList) :-
    clean_semantics(H,H2),
    clean_semantics(T,T2),
    append(H2,T2,FlatList),!.

semflatten(X,_) :- var(X), X = 'MISSING', !.   % instantiate vars with dummy
semflatten([X],_) :- var(X), X = 'MISSING', !.
semflatten([Head|Tail],Flatlist) :-
    semflatten(Head,Flathead),
    semflatten(Tail,Flattail),
    append(Flathead,Flattail,Flatlist),!.
semflatten([],[]) :- !.
semflatten(X,[X]) :- !.

clean_predicate(P,CP) :-
	lower(P,LP), % lower case predicate names
	atom_chars(LP,PChars),
	replace_whitespace(PChars,NPChars),
	atom_chars(CP,NPChars).

replace_whitespace([],[]) :-!.
replace_whitespace([32|Rest],[95|NRest]) :- !,
	replace_whitespace(Rest,NRest).
replace_whitespace([X|Rest],[X|NRest]) :-
	replace_whitespace(Rest,NRest).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Revision 1.13  1998/04/15 12:18:37  kwh
% pass quant semantics through
%
% Revision 1.12  1998/02/25 21:50:24  kwh
% allow for more incomplete semantics
%
% Revision 1.11  1998/01/28 16:35:23  kwh
% allow for null semantics entries
%
% Revision 1.10  1997/09/30 17:24:49  kwh
% merge buchart_cascade changes back in
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

