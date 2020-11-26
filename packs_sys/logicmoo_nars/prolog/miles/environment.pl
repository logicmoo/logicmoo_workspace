% MODULE environment EXPORTS

:- module(environment,
        [ oracle/1, 
          oracle/2, 
          oracle/3,
          satisfiable/1,
          ask_for/1,
          ask_for_ex/1,
          confirm/2, 
          get_ci/2]).


% IMPORTS
:- use_module(home(kb),
              [get_clause/5,delete_clause/1,store_clause/4,
               interpretable_predicate/1,get_example/3,store_ex/3,
               rename/3,delete_all/1]).
:- use_module(home(show_utils),
              [show_clauses/1,show_names/0]).
:- use_module_if_exists(library(prompt),
              [prompt/1]).
:- use_module_if_exists(library(ask),
              [yesno/1,yesno/2,ask_chars/4]).
:- use_module_if_exists(library(sets),
              [union/3]).
:- use_module_if_exists(library(subsumes),
              [subsumes_chk/2]).

% METAPREDICATES
% none


%***********************************************************************
%*	
%* module: envirnonment.pl        					
%*									
%* author: B.Jung, M.Mueller, I.Stahl, B.Tausend              date:12/92	
%*									
%* changed:								
%*									
%*									
%* description: procedures for oracle interaction
%*      1. membership queries - oracle/1
%*	2. existential queries - oracle/2
%*	3. subset queries - oracle/2							
%*	4. name queries - oracle/2							
%*	5. general questions with default answers - oracle/3
%*									
%* see also:								
%*									
%*									
%***********************************************************************

%***********************************************************************
%*									
%* predicate:	satisfiable/1							
%*									
%* syntax: satisfiable(+SG_list)
%*									
%* args: SG_list ... list of subgoals [...,[ID,Subgoal,Proof],...]
%*									
%* description:	each Subgoal in SG_list	is tested on satisfiability.
%*     The oracle is used if the satisfiability of Subgoal can not be
%*     decided on the available knowledge
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

satisfiable([]).
satisfiable([[_,H,_]|R]):-
   ask_for(H),
   satisfiable(R).



%***********************************************************************
%*									
%* predicate:	ask_for/1							
%*									
%* syntax: ask_for(+Goal)								
%*									
%* args: Goal is a ground atom								
%*									
%* description: succeds if Goal is valid in the kb, or declared to be
%*      valid by the oracle
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

ask_for(Lit):-
   (   interpretable_predicate(Lit) ->
        (   (get_clause(_,Lit,true,_,_); get_example(_,Lit,'+')) ->
            true
        ;   (   get_example(_,Lit,'-') ->
                fail
            ;   (   ground(Lit) ->
                    (   oracle(Lit) ->
                        store_ex(Lit,'+',_)
                     ;  store_ex(Lit,'-',_), fail
                    )
                ;   fail
                )
            )
        )
   ;   Lit
   ).



%***********************************************************************
%*									
%* predicate:	ask_for_ex/1							
%*									
%* syntax: ask_for(+Goal)								
%*									
%* args: Goal is an atom								
%*									
%* description: succeds if Goal is valid in the kb, or declared to be
%*      valid by the oracle
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

ask_for_ex(Lit):-
   (   interpretable_predicate(Lit) ->
        (   (get_clause(_,Lit,true,_,_); get_example(_,Lit,'+')) ->
            true
        ;   (   get_example(_,Lit,'-') ->
                fail
            ;   (   oracle(Lit,Lit) ->
                    store_ex(Lit,'+',_)
                ;   fail
                )
            )
        )
   ;   Lit
   ).


%***********************************************************************
%*									
%* predicate:	term_help/0							
%*									
%* syntax:								
%*									
%* args:								
%*									
%* description:	prompts a help line							
%*									
%* example:								
%*									
%* peculiarities:							
%*									
%* see also:								
%*									
%***********************************************************************


term_help :-
 prompt('Please enter a proper PROLOG-term followed by a full-stop and RETURN').


%***********************************************************************
%*									
%* predicate:	 oracle/1							
%*									
%* syntax:	 oracle( + Literal)							
%*									
%* args:								
%*									
%* description: membership queries:
%*    "Is the following literal always true?" -> succeeds iff oracle answers yes
%*									
%* example:								
%*									
%* peculiarities:							
%*									
%* see also:								
%*									
%***********************************************************************
%%%oracle(mappend(A,B,C)):- !,append(A,B,C).
oracle(Lit) :- 
        nl, prompt('Is the following literal always true:'),
        nl, nl, portray_clause(Lit), nl, !,
        yesno('> (y/n) ').


%***********************************************************************
%*									
%* predicate: oracle/2							
%*									
%* syntax: oracle( + List_of_Clause_Ids, - Example_Id)
%*									
%* args:								
%*									
%* description:	subset queries
%*    "Are the following clauses always true?"
%*    If not, the user might supply a counter example.
%*									
%* example:								
%*									
%* peculiarities: fails only if the oracle answers "no" AND does not 
%*      supply a counter example
%*									
%* see also:								
%*									
%***********************************************************************

oracle([Id1|Rest], NegexId) :-
        nl, prompt('Are the following clauses always true:'),
	nl, show_clauses([Id1|Rest]), !,
	(yesno('> (y/n) ') -> true ;
	    yesno('> Would You like to give a counter-example','no') ->
	    repeat,
	    prompt('> Please enter negative example as Prolog-term: '),
	    read(Ex),
	    (Ex = h ->
		term_help, fail ;
		store_ex(Ex,'-',NegexId), !)
	    ).
           

%***********************************************************************
%*									
%* predicate:	oracle/2					
%*									
%* syntax:	oracle( + PnameAtom, - NewNameAtom)
%*									
%* args:								
%*									
%*									
%* description:	name queries
%*    "How would you like to call predicate pXYZ", where pXYZ is a new predicate.
%*    The oracle may use every atom as answer. However, the atom "list" 
%*    causes the system to show every known predicate symbol within the knowledge base
%*									
%* example:								
%*									
%* peculiarities: the predicate returns the new predicate name, but does not
%*         replace the old name by the new one within the kb.  
%*
%* see also:								
%*									
%***********************************************************************

oracle(Pname, Newname) :-
	functor(Pname,_,0),
	nl, prompt('How would You like to call predicate '), write(Pname), write(' ?'),
	!, repeat,
	ask_chars('> Please enter a name or "list" followed by RETURN',1,40,A1),
	atom_chars(A2,A1),
	(A2 == 'list'  ->
	prompt('So far the following predicates have been defined in the knowledge-base:'),
	    nl, show_names, fail ;
	    Newname = A2, !).


%***********************************************************************
%*									
%* predicate:	oracle/2							
%*									
%* syntax:	oracle(+Lit, -InstLit)							
%*									
%* args:								
%*									
%* description:	existential queries
%*    "Is there a correct instance of the following literal?"
%*    If yes, the oracle supplies an instance -> InstLit
%*    Else, the predicate fails
%*									
%* example:								
%*									
%* peculiarities:							
%*									
%* see also:								
%*									
%***********************************************************************

oracle(Lit, InstLit) :- 
        nl, prompt('Is there a correct instance of the following literal:'),
        nl, nl, portray_clause(Lit), nl, !,
        yesno('> (y/n) '),
        repeat,
           prompt('> Please enter an instance: '),
           read(InstLit),
           (InstLit = h ->
           term_help, fail
           ;  subsumes_chk(Lit, InstLit) -> !
              ;  prompt('This is no instantiation of the literal!'), fail          
           ).


%***********************************************************************
%*									
%* predicate:	oracle/3							
%*									
%* syntax:	oracle( + QuestionAtom, ? DefaultAtom, - AnswerAtom)
%*									
%* args:								
%*									
%* description:	general questions with default answers
%*      If no default is necessary, use '_' as second argument
%*									
%* example:								
%*									
%* peculiarities:							
%*									
%* see also:								
%*									
%***********************************************************************

oracle(Question, Default, Answer) :-
	atom_chars(Question,Qlist),
	append([62,32],Qlist,P1list),
	(var(Default) -> atom_chars(Prompt,P1list) ;
	    atom_chars(Default,Dlist),
	    append(Dlist,[93],D1list),
	    append(P1list,[32,91|D1list],P2list),
	    atom_chars(Prompt,P2list)),
	nl, ask_chars(Prompt,0,255,Alist),
	(Alist == [], nonvar(Default) -> 
	    Answer = Default ;
	    atom_chars(Answer,Alist)
	).


%***********************************************************************
%*									
%* predicate: confirm/2	
%*									
%* syntax: confirm(+Clause_IDs,+Oldterm)	
%*									
%* args: Clause_IDs .. list of clauseIDs, 
%*       Oldterm.. term of the predicate to be replaced	
%*									
%* description:	confirm new clauses and rename the new predicate (using the oracle)
%*              if oracle refuses the new clauses, delete 'em.
%*              if they are accepted, delete the old ones (see g2_op).
%*									
%* example:								
%*									
%* peculiarities:							
%*									
%* see also:								
%*									
%***********************************************************************

confirm( Clause_ids, L) :-
	oracle(Clause_ids, Ex),
	var(Ex),
	functor(L, Oldname, _),
	oracle(Oldname, Newname),
	rename(Clause_ids , Oldname, Newname), !.

confirm( Clause_ids, _) :-
	delete_all(Clause_ids),
	nl, write('New clauses deleted.'),
	fail.



%************************************************************************
%*
%* predicate: get_ci/2
%*
%* syntax: get_ci(+L,-L)
%*
%* args: L ... list of clauseIDs
%*
%* description: reads the IDs of the Ci used for the g2-operator one
%*              by one
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

get_ci(Sofar, CC) :- 
	oracle('Please enter a resolvent ID followed by RETURN','stop',Answer),
	Answer \== 'stop',
	atom_chars(Answer,Idc),
	( number_chars(Id,Idc),
	  union(Sofar,[Id],Sofarnew) ->
	  true;
	  Sofarnew = Sofar
	),!,
	get_ci(Sofarnew, CC).
get_ci(CC, CC) :- !.	
