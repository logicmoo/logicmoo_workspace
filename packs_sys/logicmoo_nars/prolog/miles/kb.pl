% MODULE kb EXPORTS
:- module( kb, 
	[ init_kb/1,                   % Read, store and evaluate background knowledge
                                       % and examples, label them 'usr'
          init_kb/2,                   % same as before, using specified label
          save_kb/1,                   % Save knowledge base to qof-file
          consult_kb/1,                % Consult qof-file without additional processing
          gen_id/1,                    % generates a new kb id
	  id_count/1,                  % last generated id
          store_clause/4,              % Store horn-clause or Clist in kb
          store_clauses/2,             % Store a list of Clauses in kb 
          store_clauses/3,             % as store_clauses,but returns clauseIDs 
          store_ex/3,                  % Store example in kb
          get_example/3,               %          example          |
          get_clause/5,                % Retrieve clause           | from knowledge base
          get_fact/4,                  %          fact             |
          get_evaluation/2,            %          clause-evaluation|
          clear_kb/0,                  % Remove everything
          delete_clause/1,             % Remove clauses one by one
          delete_example/1,            % Remove examples one by one
          delete_all/1,
          random_ex/1,                 % one random pos. example
          two_random_ex/2,             % 2 random pos. examples
          two_random_uncovered_ex/2,   % 2 random uncovered pos. examples
          two_random_ex_from_list/3,   % 2 random examples from given list
          i_random_ex/2,               % i random pos. examples
          shortest_clause/1,           % shortest clause
          shortest_clause/2,           % shortest clause with label
          two_shortest_clauses/2,      % 2 shortest clauses
          two_shortest_clauses/3,      % 2 shortest clauses with label
          shortest_ex/1,               % shortest pos. example
          shortest_uncovered_ex/1,     % shortest uncovered pos. example
          shortest_uncovered_ex/2,     % shortest ex from list
          two_shortest_ex/2,           % 2 shortest pos. examples
          two_shortest_uncovered_ex/2, % 2 shortest pos. uncovered ex
          all_shortest_ex/1,           % list of all shortest pos. examples
          all_shortest_uncovered_ex/1, % list of all shortest uncovered pos. examples
          no_rules/0,
          no_pos_examples/0,
          no_neg_examples/0,
          no_examples/0,
          flatten_kb/0,
          flatten_rules/0,
          unflatten_kb/0,
	  delete_covered_examples/0,
	  get_predlist/1,
	  rename/3,
          known/6,
          ex/3,
          assertallz/1,
          interpretable_predicate/1
     ]).

% IMPORTS
:- use_module(home(div_utils),
              [body2list/2,mysetof/3]).
:- use_module(home(evaluation),
              [eval_examples/0,complexity/2,evaluated/1,change_evaluated/1]).
:- use_module(home(argument_types),
              [type_restriction/2,verify_types/0]).
:- use_module(home(flatten),
              [flatten_clause/2,unflatten_clause/2]).
:- use_module(home(interpreter),
              [prooftrees/3]).
:- use_module_if_exists(library(prompt)).
:- use_module_if_exists(library(ask), 
              [yesno/1]).
:- use_module_if_exists(library(basics),
              [nonmember/2,member/2]).
:- use_module_if_exists(library(random),
              [random_select/3]).

% METAPREDICATES
% none


:- dynamic id_count/1, ex/3, known/6.


%***********************************************************************
%*	
%* module: kb.pl        					
%*									
%* author: B.Jung, M.Mueller, I.Stahl, B.Tausend              date:12/92	
%*									
%* changed:								
%*									
%* description:	- knowledge base handling
%*              - flatten / unflatten knowledge base ( clauses & examples)
%*              - heuristics to select examples from kb randomly
%*		  or according to their complexities.
%*		  It is assumed that the examples' current evaluation
%*		  corresponds to the current rules.
%*									
%* see also:	
%*									
%***********************************************************************


%***********************************************************************
%*									
%* predicate: 	gen_id/1							
%*									
%* syntax:	gen_id(-New)							
%*									
%* args: -New kbID								
%*									
%* description:	generates a new kb id	
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*									
%***********************************************************************

gen_id(New) :- retract(id_count(Old)),
               New is Old+1,
               asserta(id_count(New)).
gen_id(1) :- asserta(id_count(1)).


%****************************************************************************************
%* 
%*  predicate: init_kb/1,2
%*
%*  syntax: init_kb (+Filename)
%*          init_kb (+Filename, +Label)
%*
%*  args:
%*
%*  description: the file Filename may contain Horn clauses "H:-B." and 
%*       "H.", examples "ex(Fact,Class)" and comments "%* blabla".
%*       Examples are stored in the kb as "ex(ID, Fact, Class)",
%*       clauses as
%*          "known(ID,Head,Body,Clist,Label,evaluation(1,2,3,4,5,6,7,8,9))".
%*       where ID ... unique kb identifier (a natural number)
%*             Class ... +,-,?
%*             Clist ... clause in list representation
%*                       [head:p, body1:n, body2:n, body3:r, ...]. Each literal is
%*                       marked p(positiv), n(negativ) or r(negativ + redundant)
%*             Label ... e.g. the generating operator
%*                       default used for init_kb/1: usr
%*             evaluation ... of the clauses w.r.t. the examples:
%*               1... #applications of the clause
%*               2... #definitively positive examples covered by the clause
%*               3... list of definitively positive examples covered by the clause
%*                    of the form [...exID:Fact........]
%*               4... #definitively negative  examples covered by the clause
%*               5... list of definitively negative  examples covered by the clause
%*                    of the form [...exID:Fact........]
%*               6... #probably positive examples covered by the clause
%*                    i.e. instantiations of the clause used in successful 
%*                    proofs of positive examples
%*               7... list of probably positive examples covered by the clause
%*                    [...exID:Fact........] where exID is the example of which the 
%*                    proof uses fact as subgoal
%*               8... #probably negative  examples covered by the clause
%*                    i.e. instantiations of the clause used in successful
%*                     proofs of negative examples
%*               9... list of probably negative examples covered by the clause
%*                    [...exID:Fact........] where exID is the example of which the 
%*                    proof uses fact as subgoal
%*
%*       For each example, all possible prooftrees are stored in the kb:
%*       "prooftrees(ID,M,Trees)" where M is success or fail and Trees contains
%*       all successful or failing proofs of example ID.
%*
%*       init_kb can be used successively for different files
%*
%* example:
%*
%* peculiarities:
%*
%*
%* see also:
%*
%*****************************************************************************************

init_kb(Filename) :- init_kb(Filename, usr).

init_kb(Filename, Origin) :- 
        open(Filename,read,S),
           repeat, read(S,Term),
           store_term(Term,Origin),
        close(S), !,
	eval_examples,
        verify_types.


%***********************************************************************
%*									
%* predicate:	consult_kb/1
%*									
%* syntax:	consult_kb(+ Filename)							
%*									
%* args:								
%*									
%* description:	Restore knowledge base from qof-file which was produced by save_kb/1.
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

consult_kb(Filename) :- clear_kb,
                        [Filename].


%***********************************************************************
%*									
%* predicate:	save_kb/1
%*									
%* syntax:	save_kb(+ Filename)						
%*									
%* args: Filename: name of a file (.qof)					
%*									
%* description:	 Save snapshot of current knowledge base as compiled file
%*
%* example:
%*									
%* peculiarities: suffix, .qof is recommended for Filename.	
%*
%* see also:
%*									
%***********************************************************************

save_kb(Filename) :- 
	save_predicates([known/6, ex/3, prooftrees/3, id_count/1,
                         type_restriction/2,evaluated/1],
                         Filename).


%***********************************************************************
%*									
%* predicate:	clear_kb/0							
%*									
%* syntax:	-							
%*									
%* args: 	none						
%*									
%* description:	deletes all rules and examples from the kb
%* 									
%* example:									
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

clear_kb :- %nl,yesno('Delete all knowledge and examples (y/n) '),
            retractall(known(_,_,_,_,_,_)), retractall(ex(_,_,_)),
            retractall(prooftrees(_,_,_)),retractall(id_count(_)),
            retractall(type_restriction(_,_)),retractall(evaluated(_)).



%***********************************************************************
%*									
%* predicate:	store_term/2							
%*									
%* syntax:	store_term(+Term,+Label)						
%*									
%* args: 								
%*									
%* description:	stores clause Term or example Term read from the 
%*		input file during init_kb in the kb using known/6
%* 		or ex/3							
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

store_term(end_of_file,_) :- !.
store_term(ex(F,C),_):-
        gen_id(ID),
        assertz(ex(ID,F,C)),
        !, fail.
store_term((H:- B),O) :- 
        body2list(B,L),
        gen_id(ID), 
        assertz(known(ID,H,B,[H:p|L],O,
                evaluation(0,0,[],0,[],0,[],0,[]))),
        !, fail.
store_term(type_restriction(M,A),_):-
        assertz(argument_types:type_restriction(M,A)),
        !,fail.
store_term(H,O) :- 
        gen_id(ID), 
        assertz(known(ID,H,true,[H:p],O,
                      evaluation(0,0,[],0,[],0,[],0,[]))), !, fail.


%***********************************************************************
%*									
%* predicate:	store_clause/4
%*									
%* syntax:	store_clause (?prolog-clause,?clause-list,+label,-ID)	
%*									
%* args:								
%*									
%* description:	Store new clause in knowledge base (provide either horn-clause	
%*	 or clause-list), label it and receive the unique clause-ID.	
%*       If store_clause is called with ID instantiated, it will fail if ID is 
%*       already in use in the knowledge-base. If not, ID will be used.
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

store_clause(A,B,_,ID) :-
        (nonvar(ID), (known(ID,_,_,_,_,_);ex(ID,_,_)))
        -> !, fail ; 
        var(A), var(B) -> !, fail ;                        
        fail.
store_clause((H:- B), [H:p|L], Label, ID) :- 
        body2list(B,L),
        (var(ID) -> gen_id(ID);
            id_count(Top),
            ID =< Top          ),
        (var(Label) -> Label = usr ; true),
        assertz(known(ID,H,B,[H:p|L],Label,evaluation(0,0,[],0,[],0,[],0,[]))), !,
        change_evaluated(no).
store_clause(H, [H:p], Label, ID) :-
        (var(ID) -> gen_id(ID);
            id_count(Top),
            ID =< Top          ),
        (var(Label) -> Label = usr ; true),
        assertz(known(ID,H,true,[H:p],Label,evaluation(0,0,[],0,[],0,[],0,[]))), !,
        change_evaluated(no).



%***********************************************************************
%*									
%* predicate:	store_clauses/2							
%*									
%* syntax:	store_clauses(+List_of_Clauses,+Label)
%*									
%* args: List_of_Clauses ... list of prolog clauses
%*									
%* description:	Same as store_clause/4 for a list of clauses
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

store_clauses([],_).
store_clauses([C|R],Label):-
   store_clause(C,_,Label,_),
   store_clauses(R,Label).


%***********************************************************************
%*									
%* predicate:	store_clauses/3							
%*									
%* syntax:	store_clauses(+List_of_Clauses,+Label,-IDlist)
%*									
%* args: List_of_Clauses ... list of prolog clauses
%*       IDlist... kb-ids for the clauses
%*									
%* description:	Same as store_clauses/2, but returns IDs of the clauses
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

store_clauses([],_,[]).
store_clauses([C|R],Label,[ID|R1]):-
   store_clause(C,_,Label,ID),
   store_clauses(R,Label,R1).


%***********************************************************************
%*									
%* predicate:	store_ex/3							
%*									
%* syntax:	store_ex(?fact,?classification,-ID)
%*									
%* args:								
%*									
%* description:	Store new example in knowledge base and receive the 
%*	unique identification number.
%* 	If it is called with ID already instantiated: see above.	
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

store_ex(F,Class,ID):-
    ex(ID1,F1,Class1),F == F1,!,
    Class = Class1, ID = ID1.
store_ex(F,_,ID) :-
        (nonvar(ID), (ex(ID,_,_);known(ID,_,_,_,_,_)))
        -> !, fail ;
	var(F) -> !, fail;
	fail.
store_ex(Fact, Class, ID) :-
        (var(ID) -> gen_id(ID);
            id_count(Top),
            ID =< Top          ),
        assertz(ex(ID,Fact,Class)), !,
        change_evaluated(no).



%***********************************************************************
%*									
%* predicate: get_example/3
%*            get_clause/5
%*            get_fact/4
%*            get_evaluation/2
%*
%* syntax: get_example (? ID, ? Example, ? Classification)
%*         get_clause (? ID, ? Head, ? Body, ? Clist, ? Label)
%*         get_fact (? ID, ? Fact, ? Clist, ? Label)
%*         get_evaluation (+ ID, - Evaluation)
%*
%* args:								
%*		
%* description:	read example/clause/fact or clause evaluation from the kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

get_example(ID, F, C) :- ex(ID, F, C).
get_clause(ID, H, B, L, O) :- 
     known(ID, H, B, L, O, _).        
get_fact(ID, F, L, O) :- 
     known(ID, F, true, L, O, _).
get_evaluation(ID, Eval) :- 
     known(ID,_,_,_,_,Eval).


%***********************************************************************
%*									
%* predicate:	delete_clause/1 , delete_example/1, delete_all/1
%*									
%* syntax: delete_clause(+ ID) , delete_example(+ ID), 
%*         delete_all(+list_of_clauseIDs)
%*									
%* args:								
%*									
%* description:	delete clause(s)/example(s) with identifier(s) ID(list_of_clauseIDs)
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

delete_clause(ID) :- 
        retract(known(ID,_,_,_,_,_)),
        change_evaluated(no).
delete_example(ID) :- 
        retract(ex(ID,_,_)),
        change_evaluated(no).


delete_all([]) :- !. 
delete_all([Id1|Rest]) :-
	delete_clause(Id1), !,
	delete_all(Rest).
delete_all([Id1|Rest]) :-
	delete_example(Id1), !,
	delete_all(Rest).


%***********************************************************************
%*									
%* predicate: interpretable_predicate/1
%*									
%* syntax:	interpretable_predicate(-Term)
%*									
%* args: Term .. prolog term with principal funtor P/N
%*									
%* description:	succeeds if rules or examples for P/N are in the kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

interpretable_predicate(A):-
    functor(A,F,N),functor(A1,F,N),
    ( get_clause(_,A1,_,_,_) ; get_example(_,A1,_) ).


%***********************************************************************
%*									
%* predicate: assertallz/1
%*									
%* syntax:    assertallz(+List)
%*									
%* args:								
%*									
%* description:	asserts all elements of List at the end of the kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

assertallz([]).
assertallz([X|R]):-
   assertz(X),
   assertallz(R).


%***********************************************************************
%*									
%* predicate:	rename_clause/3							
%*									
%* syntax:	rename (+ ID_list,+ Old_name,+ New_name )
%*									
%* args: Old_name, New_name ... atoms
%*									
%* description: rename	every occurence of predicate 'Old_name' to 'New_name' 
%*		in a set of clauses given as
%* 		a list of kb-references (Id-list). 'New_name' should be atomic.	
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

rename([],_,_) :- !.
rename([Id1|Rest], Old, New) :-
	get_clause(Id1,_,_,Clist,Label),
	rename_clause(Clist,NewClause,Old,New),
	delete_clause(Id1),
	store_clause(_,NewClause,Label,Id1), !,
	rename(Rest, Old, New).


%***********************************************************************
%*									
%* predicate:	rename_clause/4							
%*									
%* syntax: rename_clause(+CL,-CL1,+Old,+New)
%*									
%* args: CL,CL1.. clauses in list representation
%*       Old, New atoms 								
%*									
%* description:	replaces each literal Old(...) within CL with New(...)
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

rename_clause([], [], _, _) :- !.
rename_clause([Lit:X|Rest], [NewLit:X|NewRest], Old, New) :-
	( Lit =.. [Old|Args] ->
	    NewLit =.. [New|Args];
	    NewLit = Lit
	), !,
	rename_clause(Rest,NewRest,Old,New).


%***********************************************************************
%*									
%* predicate:	random_ex/1							
%*									
%* syntax: random_ex(-ID)								
%*									
%* args: ID exampleID								
%*									
%* description:	chooses randomly an example from the kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

random_ex(ID1):-
        findall(ID, get_example(ID,_,'+'), Bag),
        random_select( ID1, Bag, _ ).


%***********************************************************************
%*									
%* predicate: two_random_ex/2								
%*									
%* syntax: two_random_ex(-ID1,-ID2)
%*									
%* args: ID1,ID2 exampleIDs								
%*									
%* description:	chooses randomly two examples from the kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

two_random_ex(ID1,ID2):-
        findall(ID, get_example(ID,_,'+'), Bag),
        ( random_select( ID1, Bag, Residue) ;select( ID1, Bag, Residue) ) ,
        ( random_select( ID2, Residue, _) ; select( ID2, Residue, _) ).
        % sometimes random_select/3 doesn't work properly

two_random_ex_from_list(List,ID1,ID2):-
        random_select( ID1, List, Residue),
        random_select( ID2, Residue, _).

two_random_uncovered_ex(ID1,ID2):-
        findall( ID, ( kb:prooftrees(ID,fail,_),
                       get_example(ID,_,'+')
                     ), 
                Uncovered),
        two_random_ex_from_list(Uncovered,ID1,ID2).        


%***********************************************************************
%*									
%* predicate: i_random_ex/2								
%*									
%* syntax: i_random_ex(+I,-ExIDs)
%*									
%* args: I .. number								
%*									
%* description:	selects randomly I examples from the kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

i_random_ex(I,Examples):-
        I > 0,
        findall(ID, get_example(ID,_,'+'), Bag),
        length(Bag,J),
        ( J =< I      ->  Examples = Bag
        ;
        i_random_ex( I, Bag, Examples) 
        ).
i_random_ex( 0,_,[]):-!.
i_random_ex( N, Bag, [ ID | Rest]):-
        random_select( ID, Bag, Residue),
        M is N - 1,
        i_random_ex( M , Residue, Rest).


%***********************************************************************
%*									
%* predicate: shortest_clause/1								
%*									
%* syntax: shortest_clause(-ID:C)
%*									
%* args: ID .. clauseID, C ... complexity of the corresponding clause
%*									
%* description:	selects the shortest clause from the kb 
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

shortest_clause(ID1:C1):- 
        shortest_clause( _,ID1:C1). 
        
shortest_clause(Label,ID1:C1):-
        findall( ID:C, ( get_clause(ID,_,_,Clause,Label),
                        complexity(Clause,C)
                      ),
                Bag),
        shortest(Bag,ID1:C1,_).
       

%***********************************************************************
%*									
%* predicate: two_shortest_clauses/2
%*									
%* syntax: two_shortest_clauses(-ID1:CL1,-ID2:CL2)
%*									
%* args: ID1/2 .. clauseIDs, CL1/2 ... complexities of the corresponding clauses
%*									
%* description:	selects two shortest clauses from kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

two_shortest_clauses(ID1:C1,ID2:C2):-
       two_shortest_clauses(_ ,ID1:C1,ID2:C2).

two_shortest_clauses(Label,ID1:C1,ID2:C2):-
       findall( ID:C, ( get_clause(ID,_,_,Clause,Label),
                        complexity(Clause,C)
                      ),
                Bag),
       two_shortest(Bag, ID1:C1, ID2:C2).



%***********************************************************************
%*									
%* predicate: shortest_ex/1
%*									
%* syntax: shortest_ex(-ID:C)								
%*									
%* args: ID .. exID, C .. complexity of the corresponding example
%*									
%* description:	selects the shortest example from kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

shortest_ex(ID1:C1):-
        findall( ID:C, ( get_example(ID,Ex,'+'),
                        complexity(Ex,C)
                      ),
                Bag),
        shortest(Bag,ID1:C1,_).
       

%***********************************************************************
%*									
%* predicate:	two_shortest_ex/2							
%*									
%* syntax: two_shortest_ex(-ID1:C1,-ID2:C2)
%*									
%* args: ID1/2 .. exIDs, C1/2 .. complexities of the corresponding examples
%*									
%* description:	selects two shortest example from kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

two_shortest_ex(ID1:C1,ID2:C2):-
       findall( ID:C, ( get_example(ID,Ex,'+'),
                        complexity(Ex,C)
                      ),
                Bag),
       two_shortest(Bag, ID1:C1, ID2:C2).


%***********************************************************************
%*									
%* predicate:	shortest_uncovered_ex/1							
%*									
%* syntax: shortest_uncovered_ex(-ExID)
%*									
%* args:								
%*									
%* description:	selects the shortest example that is not covered by the kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

shortest_uncovered_ex(ID1):-
       findall( ID:C, ( kb:prooftrees(ID,fail,_), 
                        get_example(ID,Ex,'+'),
                        complexity(Ex,C) ), 
                Uncovered),
       shortest( Uncovered, ID1:_, _Residue).


%***********************************************************************
%*									
%* predicate:	shortest_uncovered_ex/2							
%*									
%* syntax: shortest_uncovered_ex(+ExIds,-ExId)
%*									
%* args: ExIds .. list of Ids of uncovered examples
%*									
%* description:	selects the shortest example among ExIds
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

shortest_uncovered_ex(Uncovered,ID1):-
       add_complexities(Uncovered,Bag),
       shortest( Bag, ID1:_, _Residue).


%***********************************************************************
%*									
%* predicate:	two_shortest_uncovered_ex/2
%*									
%* syntax: two_shortest_uncovered_ex(-ExID1,-ExID2)
%*									
%* args:								
%*									
%* description:	selects two shortest examples that are not covered by the kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

two_shortest_uncovered_ex(ID1,ID2):-
       findall( ID:C, ( kb:prooftrees(ID,fail,_), 
                        get_example(ID,Ex,'+'),
                        complexity(Ex,C) ), 
                Uncovered),
       two_shortest(Uncovered, ID1:_, ID2:_).


%***********************************************************************
%*									
%* predicate:	all_shortest_ex/1							
%*									
%* syntax: all_shortest_ex(-ExIds)
%*									
%* args:								
%*									
%* description: selects all shortest examples from kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

all_shortest_ex(Bag):-
       shortest_ex( _:C1),
       findall( ID, ( get_example(ID,Ex,'+'), complexity(Ex,C1) ), Bag).


%***********************************************************************
%*									
%* predicate:	all_shortest_uncovered_ex/1
%*									
%* syntax: all_shortest_uncovered_ex(-ExIds)
%*									
%* args:								
%*									
%* description: selects all shortest uncovered examples from kb
%*
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

all_shortest_uncovered_ex(Bag):-
       findall( ID:C, ( kb:prooftrees(ID,fail,_), 
                        get_example(ID,Ex,'+'),
                        complexity(Ex,C) ), 
                Uncovered),
       shortest( Uncovered, _:C1,_),
       findall( ID2, member( ID2:C1, Uncovered), Bag).


%***********************************************************************
%*									
%* predicate: two_shortest /3								
%*									
%* syntax: two_shortest(+Bag,-ID1:C1,-ID2:C2)
%*									
%* args: Bag = [ ID:C, ...] , where ID refers to example with complexity C
%*       ID1/2 ...exampleIDs, C1/2 ... corresponding complexities
%*									
%* description:	
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

two_shortest( Bag, ID1:C1,ID2:C2):-
       shortest( Bag, ID1:C1, Residue),
       shortest( Residue, ID2:C2, _),!.


%***********************************************************************
%*									
%* predicate:	shortest/3							
%*									
%* syntax: shortest(+Bag,-ID:C,-Residue)
%*									
%* args: Bag, Residue = [ ID:C, ...] , where ID is the complexity of ID
%*       ID ...kbID, C ... corresponding complexity
%*									
%* description:	selects the shortest ID from Bag wrt complexity, Residue is the rest
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

shortest( [ID:C],ID:C ,[]).
shortest( [ID1:C1|Rest] , ID:C, Residue):- 
       shortest( Rest, ID2:C2, Residue2),
       ( C1 < C2      ->    ID = ID1, C = C1, Residue = Rest
       | otherwise    ->    ID = ID2, C = C2, Residue = [ID1:C1|Residue2]
       ),!.


%***********************************************************************
%*									
%* predicate:add_complexities/2								
%*									
%* syntax: add_complexities(+L,-Pairs)
%*									
%* args: L = [ID:kb_entry_for_ID,...], 
%*       Pairs = [ID:complexity_of_kb_entry_for_ID,...]
%*									
%* description:								
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

add_complexities([],[]).

add_complexities( [ ID:Ex | More], [ ID:C | MorePairs ]):-
        complexity(Ex,C),
        add_complexities(More, MorePairs).


%***********************************************************************
%*									
%* predicate:	no_rules/0, no_pos_examples/0, 
%*              no_neg_examples/0, no_examples/0
%*									
%* syntax:								
%*									
%* args:								
%*									
%* description:	tests kb on the different properties
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

no_rules:- \+ get_clause(_,_,_,_,_).

no_pos_examples:- \+ get_example(_,_,'+').

no_neg_examples:- \+ get_example(_,_,'-').

no_examples:- \+ get_example(_,_,_).


%***********************************************************************
%*									
%* predicate:	delete_covered_examples/0
%*									
%* syntax:								
%*									
%* args:								
%*									
%* description:	deletes examples explained by the kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

delete_covered_examples:- 
        findall( I, ( 
                   get_evaluation(I,Eval),
                   arg(3,Eval,CoveredEx),
                   member(ID:_, CoveredEx),
                   delete_example(ID)
                    ),
                 _),!.
                     

%***********************************************************************
%*									
%* predicate: flatten_rules/0								
%*									
%* syntax:								
%*									
%* args:								
%*									
%* description:	flattens all clauses in the kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

flatten_rules:-
        findall( ID:C:Label, ( get_clause(ID,_,_,C,Label), delete_clause(ID) ), Bag1),
        store_flat_clauses(Bag1),!.

%***********************************************************************
%*									
%* predicate: flatten_kb/0								
%*									
%* syntax:								
%*									
%* args:								
%*									
%* description: flattens all clauses and examples in the kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

flatten_kb:-
        findall( ID:C:Label, ( get_clause(ID,_,_,C,Label), delete_clause(ID) ), Bag1),
        store_flat_clauses(Bag1),
        findall( ID:[C:p]:ex, ( get_example(ID,C,'+') ), Bag2),  
        store_flat_clauses(Bag2),!.


%***********************************************************************
%*									
%* predicate: unflatten_kb/0								
%*									
%* syntax:								
%*									
%* args:								
%*									
%* description:	unflattens a flat kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

unflatten_kb:- 
        findall( ID:C:Label, ( get_clause(ID,_,_,C,Label), delete_clause(ID) ), Bag),
        store_unflat_clauses(Bag).

%***********************************************************************
%*									
%* predicate: store_flat_clauses/1				
%*									
%* syntax: store_flat_clauses(+CL)
%*									
%* args: CL = [ID:C:Label,...] where ID is clause- or exampleID, C is the corresponding
%*	 clause in list notation and Label is the clause label or "ex" if examples are
%*       flattened
%*								
%* description:	 store flat clauses preferably with their old Id.
%*               After flattening, examples become clauses; they get a new Id
%*               while their unflat form remains in the kb.
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

store_flat_clauses([]).
store_flat_clauses([ID:C:Label|More]):-
        flatten_clause(C,CFlat),
        ( store_clause(_,CFlat,Label,ID) ; store_clause(_,CFlat,Label,_) ),
        store_flat_clauses(More).


%***********************************************************************
%*									
%* predicate: store_unflat_clauses/1
%*									
%* syntax: store_unflat_clauses(+CL)
%*									
%* args: CL = [ID:C:Label,...] where ID is a clause- or exampleID, C is the corresponding
%*	 clause in list notation and Label is the clause label or "ex"
%*									
%* args:								
%*									
%* description:	if Label \= ex, C is unflattened and replaced in the kb by 
%*      the unflat version
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

store_unflat_clauses([]).
store_unflat_clauses( [_ID:_C:ex |More]):-
        !,
        store_unflat_clauses(More).
store_unflat_clauses([ID:C:Label|More]):-
        Label \== ex,
        !,
        unflatten_clause(C,CUnFlat),
        store_clause(_,CUnFlat,Label,ID),
        store_unflat_clauses(More).


%***********************************************************************
%*
%* predicate: get_predlist/1
%*
%* syntax: get_predlist(-Predlist)  
%*		 	
%* args:  Predlist =  [P:PVars|_] 
%*	
%* description: selects all predicates with a type restriction from kb
%*        & adapts type restrictions by transfomation in a list [X:Tx,...]         
%*        of variables X and types Tx
%*
%* example:
%*	
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

get_predlist(Predlist):-
%   mysetof(P:PVars,
%           Vars^( type_restriction(P,Vars),
%                  adapt_v(Vars,PVars)), Predlist).
    mysetof(P:N,I^H^B^CL^L^(get_clause(I,H,B,CL,L), L\== type ,functor(H,P,N)),Plist),
    get_pred(Plist,Predlist).

get_pred([],[]).
get_pred([Pred:N|R],[P:PVars|R1]):-
    get_pred(R,R1),
    functor(P,Pred,N),
    (   type_restriction(P,Vars) ->
        adapt_v(Vars,PVars)
    ;   P =.. [_|Vars],
        adapt_v1(Vars,PVars)
    ).

%***********************************************************************
%*
%* predicate: adapt_v/2
%*
%* syntax: adapt_v(+TR,-Vars)                     					
%*		 	
%* args:  TR: [Tx(X),...] type restrictions for variables X of a predicate
%*        Vars: [X:Tx,...]                                              
%*	
%* description: transforms a set of type restrictions Tx(X) into       
%*        a set X:Tx of variables X and types Tx 
%*
%* example: adapt_v([list(A),atom(B)],[A:list,B:atom]                     
%*	
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

adapt_v([],[]).
adapt_v([T|R],[X:Tx|R1]):-
   adapt_v(R,R1),
   T =.. [Tx,X].

adapt_v1([],[]).
adapt_v1([X|R],[X:all|R1]):-
   adapt_v1(R,R1).

