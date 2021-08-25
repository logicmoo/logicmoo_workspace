%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% ProSPEC extended with pl2tme ( by peter baumgartner )
%% -------

%% Author: bernd thomas "benno" initial release 28.8.96 =:-] 
%% E-mail: bthomas@informatik.uni-koblenz.de
%% --------------------------------------------------
%% $Id: prospec.pl,v 1.11 1998/07/30 12:38:14 bthomas Exp $
%% $Log: prospec.pl,v $
%% Revision 1.11  1998/07/30 12:38:14  bthomas
%% singleton removed
%%
%% Revision 1.10  1998/07/30 12:33:37  bthomas
%% simplify treatment extended
%%
%% Revision 1.9  1998/04/06 09:50:48  bthomas
%% *** empty log message ***
%%
%% Revision 1.8  1998/02/10 18:01:01  bthomas
%% 1) I splitted the orig. file in a few files, to gain more
%%    modularity
%% 2) I wrote a complete new skolem treatment, means: the annotation
%%    of skolem functions is rewritten.
%% 2.1) during transformation all skolem functions are unified with
%%      the skolem functions delivered by the nft, this gives us the
%%      most specific skolem sort annotation whenever I get a skolem
%%      function in my hands.
%% 2.2) point 2.1 is not exhausting, because during processing one single
%%      literal the case may be given that a skolem function is annotated
%%      by a variable that will be later on instantiieted with a sort path.
%%      Therefore I have to process the whole set of clauses transformed by
%%      NFT from one formular has to processed again to collect the REAL most
%%      specific sort annotation of a skolem function and afterwards this sort
%%      paths have to be added to every appears of a skolem function. This
%%      means actually that we have to process the list of transformed terms
%%      twice.
%%
%% Revision 1.7  1998/01/21 15:26:44  doro
%% nnf of simplification rules optimized, transform for simplification rules corrected
%%
%% Revision 1.6  1998/01/21 10:54:04  bthomas
%% minor bug only syntax error
%%
%% Revision 1.5  1998/01/21 10:27:59  bthomas
%% dependencies for prospec source files added
%%
%% Revision 1.4  1998/01/16 14:48:08  bthomas
%% rewrote the skolemfunction treatment
%% changed to rcs system previous version w/o rcs was 1.31
%%
%% Revision 1.1  1998/01/16 14:43:35  bthomas
%% Initial revision
%%
%% 
%% ==================== Old - Changes ====================
%% Changes:
%% 8.1.98 there were some bugs in the simplifier treatment ...
%% 21.12.97 before writing out a clause or SR rule, once go through it again
%%   and unify all variables A:S1 and A:S2 so that S1 and S2 become equal.
%%   This is a bug fix
%% 19.11.97 simplifier treatment added Benno =:-]
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

version_number("$Revision: 1.11 $").

:- use_module(library(logicmoo_utils)).
%% ----------------------------------------
%%  global settings consults etc
%% ----------------------------------------

% specification op's
:- op(900,fy,'sorts'),
   op(900,fy,'subsorts'),
   op(900,fy,'functions'),
   op(900,fy,'relations'),
   op(900,fy,'ignore'),
   op(900,xfx,'subsort_of'),
   op(900,xfx,'eqisort_of'),
   op(900,fy,'partial_functions'),
   op(900,fy,'constructors'),
   op(990,xfy,'=>'),
   op(1040, xfy, '<->'). 

% global flags   
:- wdmsg((
:- set_flag(print_depth,900),
   set_flag(occur_check,on),
   set_error_handler(68,fail/0))). % dynamic not asserted, fail

:- [error], [skolem], [hierachy], [spec], [utils].

% nft op's
:- op(1050,xfy, '<->').
:- op(1050,xfy, '<-').

% NFT by peter
:- [pl2tme_pro],
   [myread].	

% MiK
user_exit :- halt.
:- set_interrupt_handler(14, halt/0).

:- dynamic xx_sort/1,
	   xx_subsort/1,
	   xx_const/1,
	   xx_function/1,
	   xx_relation/1,
	   xx_typ/2,
	   xx_proto/2,
	   xx_ignore/1,
           xx_skolem/1,
	   xx_mgu_sk/1,
	   xx_tterm/1,
	   xx_tmp_skf/1.

:- lib(lists),
   lib(strings),
   lib(numbervars).

spec_type(sorts,xx_sort).
spec_type(subsorts,xx_subsort).
spec_type(relations,xx_relation).
spec_type(functions,xx_function).

% ----------------------------------------
% predefined specs.
% args as in xxx.sort file 
% ----------------------------------------

%  - useful for preds/0 boolean

predef( relations : (false, true) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% main pred:  prospec(+FileName)
% o expects extender .sort
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prospec('') :- usage.

prospec(F) :-
	setval(spec,0),
	setval(simplifier_mode,false), %D%
	concat_atoms(F,'.sort',File),
	concat_atoms(F,'-sort.tme',OFile),
	pipe(pipein,pipeout),             % communicate with pl2tme_pro
	clean_up,
	( \+ exists(File) -> 
	    error(fileex(File))
	;
	 true ),
	open(File,read,S),
	open(OFile,write,OUT), 
	logo,
	file_header(OUT,File),
	find_spec(S),
	( getval(spec,1) ; error(no_spec) ),
	open(File,read,IN),
	msg(["\ntransforming "]),
	read_files(IN,OUT),
	close(OUT),
	close(pipein),
	close(pipeout),
	msg(["\nresult written to : ",OFile]),
	clean_up.

ignore_spec(S) :-
	repeat,
	read(S,Term),
	Term == end(prospec), !.

ignore_spec(_) :- !.
	

% --------------------------------------------------
% flag treatment
%
% query_conversion:
%   aggressive: in the clauses stemming from a query formula '?- F.'
%     only the purely negative clauses become queries ('?-'), all others
%     become input clauses (':-'). This is very incomplete.
%   conservative: in the clauses stemming from a query formula '?- F.'
%     all clauses become queries ('?-')
flag_eval(query_conversion,VALUE) :-
	setval(query_conversion,VALUE).
flag_eval(timeout, Seconds) :- alarm(Seconds).
flag_eval(_,_).

% --------------------------------------------------
% o because a .sort file may contain includes
%   we have to handle these file inclusions ...
% o here we handle all the different flag settings
% 

%| %%% aggressive: in the clauses stemming from a query formula '?- F.'
%| %%%   only the purely negative clauses become queries ('?-'), all others
%| %%%   become input clauses (':-'). This is very incomplete.
%| %%% conservative: in the clauses stemming from a query formula '?- F.'
%| %%%   all clauses become queries ('?-')

read_files(S,OUT) :-
	repeat,
	myread(S,Term),
	( Term \== end_of_file ->

	    ( Term = prospec_flag(Flag,Value) ->
		flag_eval(Flag,Value)
	    ;
	        ( Term == begin(prospec) ->
		  ignore_spec(S)
	        ;

	         ( Term = read(File) ->
		     open(File,read,NEWIN),
		     read_files(NEWIN,OUT)
	          ;
		     parse(OUT,Term)
		 )
	        )
	    )
	;
	true
        ),
	Term == end_of_file, !,
	close(S),
	!.


% we read the main prospec file and are looking
% for the begin(prospec) end(prospec) enviorment


write_to_file(OUT,SortedTerm) :-
	 ( SortedTerm = prospec_comment(Comment),
	   msg(OUT,["\n%% ",Comment])
         ;
	   msg(OUT,["\n",SortedTerm,"."])
         ),
	 !.

clean_up :-
	(retract_all(xx_sort(_)) ; true),
	(retract_all(xx_subsort(_)) ; true),
	(retract_all(xx_function(_)) ; true),
	(retract_all(xx_relation(_)) ; true),
	(retract_all(xx_typ(_,_)) ; true),
	(retract_all(xx_proto(_,_)) ; true),
	(retract_all(xx_ignore(_)) ; true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% transform a clause ...
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% comments are also written
% important for ilf
parse(OUT,COMMENT) :-
	string(COMMENT),
	writeln(OUT,COMMENT), !.

% ------------------------------
% special simplifier treatment
% ------------------------------
parse(OUT,begin(Simplify)):-
	( Simplify = simplify(_X) ; Simplify = simplify ),
	write_to_file(OUT, begin(Simplify)),
        setval(simplifier_mode,true), !.
    
parse(OUT,end(Simplify)) :-
	( Simplify = simplify(_X) ; Simplify = simplify ),
	write_to_file(OUT, end(Simplify)),
        setval(simplifier_mode,false), !.

% ------------------------------
% ignore foreign flags defined in parser.pl
% ------------------------------
parse(OUT,FOREIGN) :-
	foreign_term(FOREIGN),
	write_to_file(OUT,FOREIGN), !.

% ------------------------------
% flags from EQTRAFO, ignore it
% ------------------------------ 
parse(OUT,T) :-
	T = (partial_functions _PF),
	write_to_file(OUT, T), !.

parse(OUT,T) :-
	T = (constructors _C),  
	write_to_file(OUT, T), !.

% ------------------------------
% now the possible input syntax
% ------------------------------
%
% first transform terms to
% normal form
%-------------------------------

parse(OUT,T) :-                % call nft who
	nft(T),                % puts its nftterms in a pipe 
        read(pipein,SK),
	create_skolem_proto(SK),
	retract_all(xx_mgu_sk([])),
	assert(xx_mgu_sk([])),
	retract_all(xx_tterm([])),
	assert(xx_tterm([])),
	!,
	repeat,
	 read(pipein,NFT),
	 ( NFT \== end_of_nft ->	     
	    ( (NFT \= prospec_comment(_X)) ->   
	        transform(NFT,TT),
		unify_variables(TT),
		collect(TT)
	    ;
	       write_to_file(OUT,NFT)
	    )
	;
	true 
        ), 
	NFT == end_of_nft,
	xx_tterm(TL),
	xx_mgu_sk(SF),
	max_close_skolems(SF,TL,CLAUSES),
	write_trans_term(OUT,CLAUSES),
	retract_all(xx_tterm(_)),
	retract_all(xx_mgu_sk(_)),
	retract_all(xx_skolem(_)),
        !.

write_trans_term(_,[]).
write_trans_term(OUT,[Term|MoreT]) :-
	write_to_file(OUT,Term),
	write_trans_term(OUT,MoreT), !.

collect(TT) :-
	xx_tterm(TL),
	append(TL,[TT],NTL),
	retract(xx_tterm(TL)),
	assert(xx_tterm(NTL)),
	!.
	
%% ----------------------------------------
%% general syntax transformations ...
%% ----------------------------------------


% simplifier rules
% condition to use simpl.rule

transform((L -> K),(LT -> KT)) :-
	getval(simplifier_mode,true),
	!,
	transform(L,LT),
	transform(K,KT),!.

transform((L <-> K),(LT <-> KT)) :-
	getval(simplifier_mode,true),
	!,
	transform(L,LT),
	transform(K,KT),!.

% a query 
transform( (?- Query) , (?- TQuery) ) :-
	transform(Query,TQuery), !.

% a rule where the head consists of more then
% one literal, (disjunctive rule)  
transform( ((H1,Hn) :- Body) , (THead :- TBody) ) :-
         transform( (H1,Hn), THead ),
	 transform( Body, TBody ), !.

% a rule where the head consists of more then
% one literal, (disjunctive rule)  
transform( ((H1;Hn) :- Body) , (THead :- TBody) ) :-
         transform( (H1;Hn), THead ),
	 transform( Body, TBody ), !.

% prolog like rule
transform( (Head :- Body), (THead :- TBody) ) :-
	trans_term(Head, THead),
	transform(Body, TBody), !.

% disjunctions
transform( (Term;MoreTerms), (TTerm;TMoreTerms) ) :-
	trans_term(Term, TTerm),
	transform(MoreTerms, TMoreTerms), !.

% conjunctions	
transform( (Term,MoreTerms), (TTerm,TMoreTerms) ) :-
	trans_term(Term, TTerm),
	transform(MoreTerms, TMoreTerms), !.
	
transform( Term ,TTerm) :-
	trans_term(Term,TTerm), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% annotation 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trans_term(- Term, - TTerm) :-
	trans_term(Term, TTerm).

trans_term(~ Term, ~ TTerm) :-
	trans_term(Term, TTerm).

% ignore those terms specified
trans_term(Term,TTerm) :-
    check_ignore(Term,TTerm),
    !.

% treatment of skolem-functions
trans_term(Term,TTerm) :-
    check_skolem(Term,TTerm),
    !.

% const
trans_term(Term,TTerm) :-
	Term =.. [Id],
	( xx_proto(Id,TTerm) -> true
        ;
	error(relfc_def(Id:Term))
        ).

% pred, fct
trans_term(Term,NTerm) :-
	Term =.. [Id|Args],
	xx_proto(Id,Proto),       % first check if it is a spec. term
        nested(Args,SortedArgs),  % then process its args 
        TT =.. [Id|SortedArgs],   % build the annotated term
	
        % now we have two cases 1) its a func. 2) its a pred.
	%
	( Proto = _T : _S  ->
        % 1) func
	    NTerm = (TT:_Sort),
	    ( xx_proto(Id,NTerm) ->
		true
%		close_skolem_sorts(NTerm,TTerm)
	    ;    
	        error(sortconf(NTerm,Proto))
	    )
	;
        % 2) pred
	    NTerm = TT, 
	    ( xx_proto(Id,NTerm) -> 
		true
%		close_skolem_sorts(NTerm,TTerm)
            ;
	       error(sortconf(NTerm,Proto))
	    ) 
        ), !.

trans_term(Term,_) :-
	Term =.. [Id|_Args],
	error(relfc_def(Id:Term)).

check_ignore(Term,Term) :-
	nonvar(Term),
	!,
	functor(Term,Functor,_),
	xx_ignore(IGNORE),
	member(Functor,IGNORE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check if an argument of a term is a term
% nested(+FunctorOfTerm,+Args,-NewArgs)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nested([],[]).

% ----------------------------------------
% terms that do must not be annotated
%
nested([Arg|MoreArgs],[TTerm|MoreSortedTerms]) :-
	check_ignore(Arg,TTerm),
	nested(MoreArgs,MoreSortedTerms).

% ----------------------------------------
% skolem functions need a special treatment
%
nested([Arg|MoreArgs],[TTerm|MoreSortedTerms]) :-
	check_skolem(Arg,TTerm),
	nested(MoreArgs,MoreSortedTerms).

% ----------------------------------------
%
nested([Arg|MoreArgs],[TTerm|MoreSortedTerms]) :-
	nonvar(Arg),
	\+ (Arg = _X : _Sort),
	trans_term(Arg,TTerm),
	nested(MoreArgs,MoreSortedTerms).

% ----------------------------------------
% a function or Var as argument, process it 
% recursively by calling trans_term again
%
nested([Arg:Sort|MoreArgs],[TTerm|MoreSortedTerms]) :-
	nonvar(Arg),
	nonvar(Sort),
	( check_s_o_s(Sort,Hierachy) ->
	   true
	 ;
	  error(sortunknown(Sort))
        ),
	trans_term(Arg,TT),
	TT = Term : TermSort,
	( (Hierachy = TermSort) ->
	     TTerm = Term : Hierachy
        ;
	     xx_typ(FT,TermSort),
	     error(sortconf2(Sort,Term,FT))
        ),
	nested(MoreArgs,MoreSortedTerms). 

% ----------------------------------------
%
nested([Arg:Sort|MoreArgs],[TTerm|MoreSortedTerms]) :-
	var(Arg),
	nonvar(Sort),

% hier brauchen wir es um z.B. X:[s1,s|_]
% nicht weiter zu behandeln, is ja fertig
% und ist vieleicht durch vorherige unifikation
% entstanden.
 
	\+ Sort = [_|_],
	( check_s_o_s(Sort,Hierachy) ->
	   true
	 ;
	   error(sortunknown(Sort))
        ),
	TTerm = Arg : Hierachy,
	nested(MoreArgs,MoreSortedTerms). 
	
nested([Arg|MoreArgs],[Arg|MoreSortedTerms]) :-
	nested(MoreArgs,MoreSortedTerms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  end of prospec.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

