%===================================================================
%--- This program transforms TPTP clauses and first order formulae into   
%--- the format of geo. Since geo has no clause format, clauses
%--- have to be replaced by first-order formulas first.
%===================================================================

% Different Prolog dialects have different opinions about how
% $true and $false should be parsed.

geo_falseconstant( '$false' ) :- !.
geo_falseconstant( '$'( false )) :- !.

geo_truthconstant( '$true' ) :- !.
geo_truthconstant( '$'( true )) :- !.



% Add an element to a list, in case it does not occur already.
% This also works for variables. This is why the code has to be
% so ugly. 


geo_addunique( V, [], [ V ] ).
geo_addunique( V, [ F | R ], [ F | R ] ) :- V == F, !.
geo_addunique( V, [ F | R ], [ F | S ] ) :- 
   V \== F,
   geo_addunique( V, R, S ).


% length of list:

geo_length( [], 0 ).
geo_length( [ _ | R ], N1 ) :-
   geo_length( R, N ),
   N1 is N + 1.


% Collect the Prolog-variables in a term:

geo_collectvars_term( At, V1, V2 ) :-
   looks_like_a_variable( At ), !, 
   geo_addunique( At, V1, V2 ).

geo_collectvars_term( At, V1, V2 ) :-
   At =.. [ _Func | Args ],
   geo_collectvars_termlist( Args, V1, V2 ).

% Collect the Prolog-variables in a termlist.

geo_collectvars_termlist( [], X, X ).
geo_collectvars_termlist( [ F | R ], L1, L3 ) :-
   geo_collectvars_term( F, L1, L2 ), !, 
   geo_collectvars_termlist( R, L2, L3 ).

% Collect the Prolog-variables in an atom:

geo_collectvars_atom( At, V1, V2 ) :-
   At =..  [ _Pred | Args ],
   geo_collectvars_termlist( Args, V1, V2 ).



% Convert a clause to a first-order disjunction, and simultaneously
% collect the Prolog-variables: 
% geo_build_disjunction( Clause, Disj, Collectedvars ).


geo_build_disjunction( [], False, [] ) :- 
   geo_falseconstant( False ), !. 

geo_build_disjunction( [ '++'( At ) ], At, Vars ) :- 
   geo_collectvars_atom( At, [], Vars ), !.

geo_build_disjunction( [ '--'( At ) ], '~'( At ), Vars ) :-
   geo_collectvars_atom( At, [], Vars ), !. 
      
geo_build_disjunction( [ '++'( At ) | R ], '|'( At, S ), Vars2 ) :-
   geo_build_disjunction( R, S, Vars ), !,
   geo_collectvars_atom( At, Vars, Vars2 ).

geo_build_disjunction( [ '--'( At ) | R ], '|'( '~'(At), S ), Vars2 ) :-
   geo_build_disjunction( R, S, Vars ), !,
   geo_collectvars_atom( At, Vars, Vars2 ).

geo_build_disjunction( _, _, _ ) :-
   nl, nl,
   write( 'ERROR geo_build_disjunction' ),
   nl, nl.

% Convert a clause to a first-order formula, both in TPTP-format.

geo_convert_clause_fol( Cl, Fol ) :-
   geo_build_disjunction( Cl, Disj, Vars ), !,
   geo_build_forall( Vars, Disj, Fol ).

geo_build_forall( [], Disj, Disj ). 
geo_build_forall( [ F | R ], Disj, ':'( '!'( [ F | R ] ), Disj ) ). 
   

%-----------------------------------------
% Make sure that all formulas are in fol: 
%-----------------------------------------


geo_ensure_fol( [], [] ) :- !.
geo_ensure_fol( [ fof( Name, T, F ) | R ], [ fof( Name, T, F ) | S ] ) :-
   ( T = axiom ; T = hypothesis; T = lemma; T = theorem; T = definition; T = conjecture ),
   !,
   geo_ensure_fol( R, S ).

geo_ensure_fol( [ input_clause( Name, T, C ) | R ],
                [ fof( Name, T, F ) | S ] ) :-
   ( T = axiom ; T = hypothesis; T = negated_conjecture ), !,
   geo_convert_clause_fol( C, F ), 
   geo_ensure_fol( R, S ).

geo_ensure_fol( [ F | _ ], _ ) :-
   nl,
   write( 'ERROR, unknown problem type in ' ), write( F ), nl, nl.






% Collect the predicates and functions from a first-order formula.
% We rely on the fact that bound variables always
% start with a capital in TPTP, so that they are internal variables
% of Prolog. If this would not be the case, we would have to keep track 
% of bound variables in atoms.


% collect the functions from a term:

geo_collect_func_term( At, F, F ) :- 
   looks_like_a_variable( At ), !.

geo_collect_func_term( At, F1, F3 ) :- 
   At =.. [ Func | Args ],
   geo_length( Args, Arity ),
   geo_addunique( func( Func, Arity ), F1, F2 ),
   geo_collect_func_termlist( Args, F2, F3 ). 


% collect the functions from a list of terms:

geo_collect_func_termlist( [], F, F ).
geo_collect_func_termlist( [ F | R ], F1, F3 ) :-
   geo_collect_func_term( F, F1, F2 ),
   geo_collect_func_termlist( R, F2, F3 ).



% collect the functions and predicates from a formula:
% This is necessary because geo requires all predicates
% and functions to be declared.


geo_collect_signature_fol(  '~'( A ), P1, P2, F1, F2 ) :-
   geo_collect_signature_fol( A, P1, P2, F1, F2 ), !.

geo_collect_signature_fol( '|'( A, B ), P1, P3, F1, F3 ) :-
   geo_collect_signature_fol( A, P1, P2, F1, F2 ),
   geo_collect_signature_fol( B, P2, P3, F2, F3 ), !.

geo_collect_signature_fol( ';'( A, B ), P1, P3, F1, F3 ) :-
   geo_collect_signature_fol( A, P1, P2, F1, F2 ),
   geo_collect_signature_fol( B, P2, P3, F2, F3 ), !.

geo_collect_signature_fol( '&'( A, B ), P1, P3, F1, F3 ) :-
   geo_collect_signature_fol( A, P1, P2, F1, F2 ),
   geo_collect_signature_fol( B, P2, P3, F2, F3 ), !.

geo_collect_signature_fol( '<=>'( A, B ), P1, P3, F1, F3 ) :-
   geo_collect_signature_fol( A, P1, P2, F1, F2 ),
   geo_collect_signature_fol( B, P2, P3, F2, F3 ), !.

geo_collect_signature_fol( '=>'( A, B ), P1, P3, F1, F3 ) :-
   geo_collect_signature_fol( A, P1, P2, F1, F2 ),
   geo_collect_signature_fol( B, P2, P3, F2, F3 ), !. 

geo_collect_signature_fol( '<='( A, B ), P1, P3, F1, F3 ) :-
   geo_collect_signature_fol( A, P1, P2, F1, F2 ),
   geo_collect_signature_fol( B, P2, P3, F2, F3 ), !. 

geo_collect_signature_fol( '<~>'( A, B ), P1, P3, F1, F3 ) :-
   geo_collect_signature_fol( A, P1, P2, F1, F2 ),
   geo_collect_signature_fol( B, P2, P3, F2, F3 ), !.

geo_collect_signature_fol( '~|'( A, B ), P1, P3, F1, F3 ) :-
   geo_collect_signature_fol( A, P1, P2, F1, F2 ),
   geo_collect_signature_fol( B, P2, P3, F2, F3 ), !.

geo_collect_signature_fol( '~&'( A, B ), P1, P3, F1, F3 ) :-
   geo_collect_signature_fol( A, P1, P2, F1, F2 ),
   geo_collect_signature_fol( B, P2, P3, F2, F3 ), !.

geo_collect_signature_fol( ':'( '?'( _ ), F ), P1, P2, F1, F2 ) :-
   geo_collect_signature_fol( F, P1, P2, F1, F2 ), !.

geo_collect_signature_fol( ':'( '!'( _ ), F ), P1, P2, F1, F2 ) :-
   geo_collect_signature_fol( F, P1, P2, F1, F2 ), !.

geo_collect_signature_fol( False, P, P, F, F ) :- 
   geo_falseconstant( False ), !.

geo_collect_signature_fol( True, P, P, F, F ) :- 
   geo_truthconstant( True ), !. 

geo_collect_signature_fol( '$tptp_equal'( A, B ), P, P, F1, F3 ) :-
   geo_collect_func_term( A, F1, F2 ), !,
   geo_collect_func_term( B, F2, F3 ), !.

geo_collect_signature_fol( At, P1, P2, F1, F2 ) :-
   At =.. [ Pred | Args ],
   name( Pred, [ Firstchar | _ ] ),
   Firstchar >= 97, Firstchar =< 122, !, 
   geo_length( Args, Arity ), 
   geo_addunique( pred( Pred, Arity ), P1, P2 ), !,
   geo_collect_func_termlist( Args, F1, F2 ), !.

geo_collect_signature_fol( Garb, _, _, _, _ ) :-
   write( 'ERROR unknown operator ' ), 
   write( Garb ), nl, nl.


%---------------------------------------------


geo_collect_signature_problem( [], P, P, F, F ) :- !.
geo_collect_signature_problem( [ fof( _Name, T, F ) | R ], P1, P3, F1, F3 ) :-
   ( T = axiom ; T = hypothesis; T = lemma; T = theorem; T = definition; T = conjecture;
     T = negated_conjecture ), 
   geo_collect_signature_fol( F, P1, P2, F1, F2 ), !,
   geo_collect_signature_problem( R, P2, P3, F2, F3 ).

geo_collect_signature_problem( [ F | _ ], _, _, _, _ ) :-
   nl,
   write( 'ERROR, unknown problem type in ' ), write( F ), nl, nl.


   
%================================================

geo_print_predicate_declarations( [] ).
geo_print_predicate_declarations( [ pred( P, A ) | R ] ) :-
   write( '   ' ), write(P), write( '/' ), write(A), write( '.' ), nl, !, 
   geo_print_predicate_declarations( R ).


geo_print_function_declarations( [] ).
geo_print_function_declarations( [ func( F, A ) | R ] ) :-
   write( '   ' ), write( F ), write( '/' ), write(A), write( '.'), nl, !,
   geo_print_function_declarations( R ). 


%===============================================






% Print a formula in fof format. We do not attempt make the result
% human readable. 

geo_print_fol( '~'(A) ) :-
   !, geo_printunary( '!', A ). 
% the final proof that Prolog cannot be trusted: 
geo_print_fol( '|'( A, B) ) :-
   !, geo_printbinary( '\\/', A, B ). 
geo_print_fol( ';'( A , B) ) :-
   !, geo_printbinary( '\\/', A, B ). 
geo_print_fol( '&'( A, B ) ) :-
   !, geo_printbinary( '/\\', A, B ). 
geo_print_fol( '<=>'( A, B ) ) :-
   !, geo_printbinary( '<->', A, B ). 
geo_print_fol( '=>'( A, B )) :-
   !, geo_printbinary( '->', A, B ). 
geo_print_fol( '<='( A, B )) :-
   !, geo_printbinary( '->', B, A ).
geo_print_fol( '<~>'( A, B )) :-
   !, geo_printnegbinary( '<->', A, B ).
geo_print_fol( '~|'( A, B )) :-
   !, geo_printnegbinary( '\\/', A, B ).
geo_print_fol( '~&'( A, B )) :-
   !, geo_printnegbinary( '/\\', A, B ).

geo_print_fol( ':'( '!'( L ), F )) :-
   !, geo_printquantifier( '[', ']', L, F ). 
geo_print_fol( ':'( '?'( L ), F )) :-
   !, geo_printquantifier( '<', '>', L, F ). 
geo_print_fol( '$tptp_equal'( A, B )) :-
   !, write( '( ' ), write( A ), write( ' = ' ), write( B ), write( ' )' ).


geo_print_fol( True ):-
   geo_truthconstant( True ), !, 
   write( 'TRUE' ). 

geo_print_fol( False ):-
   geo_falseconstant( False ), !, 
   write( 'FALSE' ).

% Then it must be an atom. Let's hope it really is.

geo_print_fol( At ) :-
   At =.. [ Pred | _ ],
   name( Pred, [ Firstchar | _ ] ),
   Firstchar >= 97, Firstchar =< 122, !, 
   write( At ). 

geo_print_fol( Garb ) :-
   write( 'ERROR, unknown operator ' ),
   write( Garb ), nl. 

% We print in such a way that every formula can resist 
% every context. Therefore, we don't need to care
% about putting subformulas in parentheses. They will care
% for themselves. 


geo_printunary( Operator, A ) :-
   write( '(' ), write( Operator ),
   write( ' ' ), geo_print_fol( A ), write( ')' ).


geo_printbinary( Operator, A, B ) :-
   write( '(' ), geo_print_fol( A ), write( ' ' ), 
   write( Operator ),
   write( ' ' ), geo_print_fol( B ), write( ')' ).
      
geo_printnegbinary( Operator, A, B ) :-
   write( '( ! (' ), geo_print_fol( A ), write( ' ' ),   
   write( Operator ),
   write( ' ' ), geo_print_fol( B ), write( '))' ).

geo_printquantifier( Start, End, A, B ) :- 
   write( '( ' ),
   write( Start ),
   geo_printvarlist( A ),
   write( End ), write( ' ' ), 
   geo_print_fol( B ),
   write( ')' ). 


geo_printvarlist( [] ) :-
   write( ' ' ). 
geo_printvarlist( [ F ] ) :-
   !, 
   write( F ).
geo_printvarlist( [ F | R ] ) :-
   write( F ),
   write( ', ' ), !, 
   geo_printvarlist( R ). 



% Print list of first-order formulas in geo format. 
% When a clause is converted, it type is copied.  
% Therefore, there are more types here, than a formula in
% TPTP format can have.
%  %===================================================================
%  %--- This transforms TPTP clauses and first order formulae into   
%  %--- bliksem format. 
%  %===================================================================
%  % Print the header for first order formulae: 
%  blk_fof_header :-
%     nl, 
%     write( 'Auto.' ),
%     nl,
%     nl. 
%  
%  
%  % Print the header for CNF-clauses: 
%  
%  blk_cnf_header :- 
%     nl, 
%     write('Set( totalproof, 1 ).'),
%     nl, 
%     write('Set( prologoutput, 1 ).'),
%     nl,
%     write( 'Auto.' ),
%     nl, nl. 
%  
%  
%  % True if all elements of L have form fof( Name, Type, F ). 
%  
%  blk_isfof( [] ).
%  blk_isfof( [ fof( _, _, _ ) | R ] ) :-
%     blk_isfof( R ).    
%  
%  
%  % True if all elements of L have form input_clause( Name, Type, F ). 
%  
%  blk_iscnf( [] ). 
%  blk_iscnf( [ input_clause( _, _, _ ) | R ] ) :-
%     blk_iscnf( R ). 
%  
%  
%  
%  blk_printvarlist( [] ) :-
%     write( 'THIS SHOULD NOT HAPPEN' ). 
%  blk_printvarlist( [ F ] ) :-
%     !, 
%     write( F ).
%  blk_printvarlist( [ F | R ] ) :-
%     write( F ),
%     write( ', ' ),
%     blk_printvarlist( R ). 
%  
%  
%  % Print a formula in fof format. We do not attempt make the result
%  % human readable. 
%  
%  blk_printfof( ~ A ) :-
%     !, blk_printunary( '!', A ). 
%  % the final proof that Prolog cannot be trusted: 
%  blk_printfof( '|'(A,B) ) :-
%     !, blk_printbinary( '|', A, B ). 
%  blk_printfof( (A ; B) ) :-
%     !, blk_printbinary( '|', A, B ). 
%  blk_printfof( A & B ) :-
%     !, blk_printbinary( '&', A, B ). 
%  blk_printfof( A <=> B ) :-
%     !, blk_printbinary( '<->', A, B ). 
%  blk_printfof( A => B ) :-
%     !, blk_printbinary( '->', A, B ). 
%  blk_printfof( A <= B ) :-
%     !, blk_printbinary( '->', B, A ).
%  blk_printfof( A <~> B ) :-
%     !, blk_printnegbinary( '<->', A, B ).
%  blk_printfof( A '~|' B ) :-
%     !, blk_printnegbinary( '|', A, B ).
%  blk_printfof( A ~& B ) :-
%     !, blk_printnegbinary( '&', A, B ).
%  blk_printfof( ! L : F ) :-
%     !, blk_printquantifier( '[', ']', L, F ). 
%  blk_printfof( ? L : F ) :-
%     !, blk_printquantifier( '<', '>', L, F ). 
%  blk_printfof( '$tptp_equal'( A, B ) ) :-
%     !, blk_printbinary( '=', A, B ). 
%  
%  blk_printfof('$true'):-
%      !,
%      write('( && )').
%  
%  blk_printfof('$false'):-
%      !,
%      write('( || )').
%  
%  blk_printfof( F ) :-
%     write( F ). 
%  
%  
%  
%  blk_printunary( _, A ) :-
%     write( '! ( ' ), blk_printfof( A ), write( ')' ).
%  
%  blk_printbinary( Operator, A, B ) :-
%     write( '(' ), blk_printfof( A ), write( ')' ),
%     write( Operator ),
%     write( '(' ), blk_printfof( B ), write( ')' ).
%        
%  blk_printnegbinary( Operator, A, B ) :-
%     write( '! (( ' ), blk_printfof( A ), write( ')' ),   
%     write( Operator ),
%     write( '(' ), blk_printfof( B ), write( '))' ).
%  
%  blk_printquantifier( Start, End, A, B ) :- 
%     write( '( ' ),
%     write( Start ),
%     blk_printvarlist( A ),
%     write( End ),
%     blk_printfof( B ),
%     write( ')' ). 
%  
%  
%  % Print a list of first order formulae. A '.' is inserted after every
%  % formula, together with a newline. 
%  
%  blk_printfoflist( [] ).
%  blk_printfoflist( [ fof( Name, Status, F ) | R ] ) :-
%     tptp2X_member(Status,[axiom,hypothesis,lemma,definition]),
%     !,
%     write( '# '),
%     write(Status),
%     write(' ' ), write( Name ), 
%     write( ':' ), nl, !, 
%     blk_printfof( F ), !, 
%     write( '.' ), 
%     nl,
%     blk_printfoflist( R ). 
%  
%  blk_printfoflist( [ fof( Name, conjecture, F ) | R ] ) :-
%  %----Need this for dodgy processing in Ratify
%      write('#----This is the conjecture with negated conjecture'),
%      nl,
%     write( '# conjecture (has been negated) '), write( Name ), 
%     write( ':' ), nl, !, 
%     blk_printfof( ~ F ), !, 
%     write( '.' ), 
%     nl,
%     blk_printfoflist( R ). 
%  
%  % Print atoms: An atom can be printed without problems, because bliksem 
%  % has the Prolog convention for variables. '$tptp_equal'(A,B) has to be 
%  % replaced by A = B. 
%  
%  blk_printatom( '$tptp_equal'( A, B ) ) :-
%     write( A = B ),
%     !. 
%  
%  blk_printatom('$true'):-
%      !,
%      write(' && ').
%  
%  blk_printatom('$false'):-
%      !,
%      write(' || ').
%  
%  blk_printatom( X ) :-
%     write( X ). 
%  
%  % Print a literal: ++ X is printed as X. -- X is printed as ! X.  
%  
%  blk_printliteral( ++ X ) :-
%     blk_printatom( X ).
%  blk_printliteral( -- X ) :-
%     write( '! ' ),
%     blk_printatom( X ). 
%  
%  
%  % Print list of literals, separated by comma's. 
%  
%  blk_printliterals( [] ).
%  blk_printliterals( [ F ] ) :-
%     !, 
%     blk_printliteral( F ),
%     write( ' ' ).
%  blk_printliterals( [ F | R ] ) :-
%     blk_printliteral( F ),
%     write( ', ' ),
%     blk_printliterals( R ).
%  
%  
%  % Print a clause: 
%  
%  blk_printclause( Cls ) :-
%     write( '{ ' ),
%     blk_printliterals( Cls ),
%     write( '}.' ).
%  
%  
%  % Print a clause list: 
%  
%  blk_printclauselist( [] ).
%  blk_printclauselist( [ input_clause( Name, Type, F ) | R ] ) :-
%     write( '# ' ), write( Type ), write( ' ' ), write( Name ),
%     write( ':' ), nl, !, 
%     blk_printclause( F ), !, 
%     nl,
%     blk_printclauselist( R ).
%  
%  
%  % Convert TPTP to bliksem. It is necessary to check the format in advance 
%  % in order to produce the right header. 
%  
%  bliksem( bliksem, Formulalist, _ ) :-
%     blk_isfof( Formulalist ), !,
%     blk_fof_header,
%     blk_printfoflist( Formulalist ),
%     nl. 
%  
%  bliksem( bliksem, Formulalist, _ ) :- 
%     blk_iscnf( Formulalist ), !, 
%     blk_cnf_header,
%     blk_printclauselist( Formulalist ),
%     nl. 
%  
%  bliksem( bliksem, Formulalist, _ ) :-
%     write( 'Sorry, cannot handle this: ' ),
%     write( Formulalist ),
%     nl.
%  
%  
%  % We chose # for comment. % was not possible because it is allowed in 
%  % identifiers. 
%  
%  bliksem_format_information( '#', '.blk' ).
%  bliksem_file_information( format, bliksem, 'Bliksem format' ).
%  


geo_print_problem( [] ).
geo_print_problem( [ fof( Name, Type, F ) | R ] ) :-
   ( Type = axiom; Type = hypothesis; Type = lemma; Type = definition;
     Type = negated_conjecture ), !,
   write( '// ' ), write( Type ), write( ' ' ), write( Name ),
   write( ':' ), nl, !, 
   write( '   ' ), geo_print_fol( F ), write( '.' ), !, 
   nl,
   geo_print_problem( R ).

geo_print_problem( [ fof( Name, conjecture, F ) | R ] ) :-
%----Need this for dodgy processing in Ratify and SystemOnTPTP
   write('//----This is the conjecture with negated conjecture'),
   nl,
   write( '// ' ), write( 'negated conjecture ' ), 
   write( ' ' ), write( Name ), write( ':' ), nl, !,
   write( '   ' ), geo_print_fol( '~'(F) ), write( '.' ), !,
   nl,
   geo_print_problem( R ).


% Convert TPTP to geo. It is necessary to check the format in advance 
% in order to produce the right header. 

geo( geo, Problem, _ ) :-
   geo_ensure_fol( Problem, Problem_without_cnf ), !, 

   geo_collect_signature_problem( Problem_without_cnf, 
                                  [], Preds, [], Funcs ), !, 
   nl,
   write( '%predicates' ), nl,
   geo_print_predicate_declarations( Preds ), !, 
   nl, 
   write( '%functions' ), nl,
   geo_print_function_declarations( Funcs ), !, 
   nl, 
   write('%firstorder' ), nl,
   geo_print_problem( Problem_without_cnf ), !,
   nl, nl. 



geo_format_information( '//', '.geo' ).
geo_file_information( format, geo, 'Geo format' ).


