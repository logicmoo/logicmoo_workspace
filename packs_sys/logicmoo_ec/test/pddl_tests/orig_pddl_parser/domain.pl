     :- module(domain, [domain/2, domain/3, not_equal/2, bind/1]).
     
     :- use_module(library(atts)).
     :- use_module(library(ordsets), [
             ord_intersection/3,
             ord_intersect/2,
             ord_union/3,
             ord_union/2,
             ord_member/2,
             ord_nonmember/2,
				 ord_add_element/3,
				 ord_del_element/3,
             list_to_ord_set/2
        ]).
   	:- use_module(library(sets), [
				del_element/3   	
   		]).
     
     :- attribute dom/2.
	  %:- attribute forbidden/1.
     
     verify_attributes(Var, Other, Goals) :-
             get_atts(Var, dom(Da, Fa)), !,          % are we involved?
             (   var(Other) ->                   % must be attributed then
                 (   get_atts(Other, dom(Db, Fb)) -> %   has a domain?
                     (my_del_element2(Var, Fb, _) -> !, fail ; true),
                     ord_intersection(Da, Db, Dc),
                     ord_union(Fa, Fb, Fc),
                     Dc = [El|Els],              % at least one element
                     (   Els = [] ->             % exactly one element
                         Goals = [Other=El]      % implied binding
                     ;   Goals = [],
                         put_atts(Other, dom(Dc, Fc))% rescue intersection
                     )
                 ;   Goals = [],
                     put_atts(Other, dom(Da, Fa))    % rescue the domain
                 )
             ;   Goals = [],
                ord_intersect([Other], Da),      % value in domain?
                delete_from(Fa, Var, Other),
                put_atts(Var, dom([Other], Fa)),
%                my_del_element(Var, Fa, NewFa),
                bind_all(Fa)
             ).
     verify_attributes(_, _, []).                % unification triggered
                                                 % because of attributes
                                                 % in other modules
     
     attribute_goal(Var, domain(Var,Dom, F)) :-     % interpretation as goal
             get_atts(Var, dom(Dom, F)).
     
     domain(X, Dom) :-
   		domain(X, Dom, _).
     domain(X, List) :-
             list_to_ord_set(List, Set),
             Set = [El|Els],                     % at least one element
             (   Els = [] ->                     % exactly one element
                 X = El                          % implied binding
             ;   put_atts(Fresh, dom(Set, [])),
                 X = Fresh                       % may call
                                                 % verify_attributes/3
             ).
     domain(X, Dom, F) :-
             var(Dom), !,
             get_atts(X, dom(Dom, F)).
  
  
  
   delete_from([], _, _).
   delete_from([A|T], V, Value):-
   	(A==V -> true;
   		get_atts(A, dom(Ad, Af)),
   		my_del_element(Value, Ad, NewAd),
   		my_del_element(V, Af, NewAf),
   		put_atts(A, dom(NewAd, NewAf))
  		),
   	delete_from(T, V, Value).
  		
  my_del_element(_, [], []).
  my_del_element(E, [H|T], R):-
  		E==H, !, my_del_element(E, T, R).
  my_del_element(E, [H|T], [H|R]):-
  		my_del_element(E, T, R).
  
  my_del_element2(E, [H|T], R):-
  		E==H, !, my_del_element(E, T, R).
  my_del_element2(E, [H|T], [H|R]):-
  		my_del_element2(E, T, R).


	not_equal([], []).
	not_equal(A, B):-
  		ground(A),ground(B),!,
  		A\=B.

  not_equal(A, B):-
  		ground(A),!,
		not_equal(B, A).
  not_equal(A, B):-
  		var(A), ground(B),!,
  		get_atts(A, dom(Ad, Fa)),
  		ord_del_element(Ad, B, NewAd),
  		put_atts(A, dom(NewAd, Fa)),
		bind(Fa).
	not_equal(A, B):-
	  A==B, !, fail. 
	not_equal(A, B):-
		var(A), var(B),
		get_atts(A, dom(Da, Fa)),
		get_atts(B, dom(Db, Fb)),
%		ord_union([Fa,Fb,[A,B]], F),
		ord_union([[B],Fa], Faa),
		ord_union([[A],Fb], Fbb),
		put_atts(A, dom(Da, Faa)),
		put_atts(B, dom(Db, Fbb)),
		
		ord_union([[A],Fa], Faaaa),
		ord_union([[B],Fb], Fbbbb),
		bind(Faaaa),
    bind(Fbbbb).
	not_equal([Ha|Ta], [Hb|Tb]):-
		!,
		not_equal(Ha, Hb),
		not_equal(Ta, Tb).
	not_equal(A, B):-
		compound(A), compound(B),!,
		A =.. [Fa|Pa], B=..[Fb|Pb],
		(Fa=Fb ->
			not_equal(Pa, Pb) ;
			true
		).


	set_forbidden([], _).
	set_forbidden([H|T], F):-
		get_atts(H, dom(D, _)),
		put_atts(H, dom(D, F)),
%		write(H-D-F),nl,
		set_forbidden(T, F).	
	
	bind_all([]).
	bind_all([H|T]):-
	  (var(H) -> bind([H]) ; true),	  
	  bind_all(T).
	
	bind(F):-
		setof(B, solvable(F, [], B), Bs),
		rotate(Bs, RB),
		bind_value(F, RB).

	bind_value([], _).
	bind_value([H|T], [B|Bs]):-
	   (var(H) ->
  		list_to_ord_set(B, OB),
  		(OB=[VB] -> H=VB ; 
  			get_atts(H, dom(_, Hf)),
  			put_atts(H, dom(OB, Hf))
  		) ;
  		true
  	),
  	bind_value(T, Bs).
	
	rotate([[]|_], []).	
	rotate(S, [F|Fs]):-
		first(S, F, R),
		rotate(R, Fs).
	
	first([], [], []).
	first([[F|T]|T2], [F|Fs], [T|R]):-
		first(T2, Fs, R).
	
	solvable([], _, []).
	solvable([H|T], FV, [M|S]):-
	  (var(H) -> 
		  get_atts(H, dom(Hd, _)),
		  member(M, Hd),
		  ord_nonmember(M, FV),
		  ord_add_element(FV, M, NewFV) ;
		  NewFV=FV
		),
		solvable(T, NewFV, S).
		
		
		
		
