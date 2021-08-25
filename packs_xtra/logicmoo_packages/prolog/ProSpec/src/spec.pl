%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% sort specification module for prospec
%% -------------------------------------

%% Author: bernd thomas 
%% E-mail: bthomas@informatik.uni-koblenz.de
%% --------------------------------------------------
%% $Id: spec.pl,v 1.1 1998/02/10 18:01:01 bthomas Exp $
%% $Log: spec.pl,v $
%% Revision 1.1  1998/02/10 18:01:01  bthomas
%% Initial revision
%%

% we read the main prospec file and are looking
% for the begin(prospec) end(prospec) enviorment

find_spec(IN) :-
	repeat,
        read(IN,Term),
	( Term \== end_of_file ->
	   ( Term == begin(prospec) ->
		read_spec(IN)
	    ;
	    ( Term = read(File) ->
		open(File,read,NEWIN),
		find_spec(NEWIN)
	      ;
	        true
	    )
	  )
          ;
	   true ), 
	( Term == end_of_file ; Term == begin(prospec) ),
	close(IN),
	!.

spec_error(S1,S2) :-
	close(S1),
	close(S2),
	close(pipein),
	close(pipeout),
	error(spec).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% read the specification and build 
% o sorts hierachy (only tree structure !)
% o prototyps of functions 
% o prototyps of relations(predicates)
%
% prototyp means: sorts annotated terms
% with variables as arguments

% -------------------------------------
% now the bad news:
% o no other order than the below is allowed  
% o every spec. keyword must be given even
%   if it is empty, (e.g. subsorts none.) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_spec(S) :-
	( getval(spec,0) ; error(double_spec) ),
	setval(spec,1),
	msg(["\n\nreading specification "]),
	repeat,
	read(S,Term),
	( Term \== end(prospec) ->
	    ( Term \= prospec_flag(_,_) ->
		compile_term(Term)
	    ;
	        true
	    )
	;
	  ( Term == end_of_file -> 
	      error(specend)
	  ;
	  true )
	),
	Term == end(prospec),

	get_spec,
	build_hierachy,
	build_func_prototyps,
	build_rel_prototyps, !.

read_spec(S,OUT) :- spec_error(S,OUT), !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% read the specification and
% assert them.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

get_spec :-

	sorts Sorts,               % sorts must be given
	assert_spec(error,xx_sort,Sorts),

        ( subsorts SubSorts,
 	  assert_spec(error,xx_subsort,SubSorts) ; true ),

	( functions Functions,
	  assert_spec(error,xx_function,Functions) ; true ),

	( relations Relations,	  
          assert_spec(error,xx_relation,Relations) ; true ),

	( ignore Ignore,
	  assert_spec(error,xx_ignore,Ignore) ; true ),

	get_predefined.

get_spec :-
	msg(['\nERROR: missing sorts definition.\n At least you need sorts otherwise you do not need ProSpec. =;-]\n']),
	stop.

get_predefined :-	
	spec_type(SpecType,XX),
	predef(T),
	T =.. [SpecType,Defs],
	assert_spec(noerror,XX,Defs),
	false.

get_predefined.
	
assert_spec(_,xx_ignore,Definition) :-
	( Definition =.. [:|Defs] ->
	    assert(xx_ignore(Defs))
	  ;
	    ( Definition = none ->
	        assert(xx_ignore([]))
	    ;
	        error(ignore)
            )
        ), 
	!.

assert_spec(ErrorTyp,SpecType,Definition) :-
	Definition =.. [:|Defs],
	memo(ErrorTyp,SpecType,Defs), !.

memo(_,_,[]).
memo(Error,SpecType, [Ele|Elements] ) :-
	Term =.. [SpecType,Ele],
	\+ Term,
	assert(Term), !,
	memo(Error,SpecType,Elements).

memo(noerror,Type,[_|More]) :-
	memo(noerror,Type,More).

memo(error,_,[Ele|_]) :-
	error(double('in specification',Ele)).

