/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module( parse_term_from_atom,
         [ atom_to_term/4       % (+TermAtom,Term,VarNames,TermPos)
         , find_term_in_atom/4  % (+Atom,+SearchTerm,From,To)
         , find_term_in_term/5  % (+SearchTerm,+Term,TermPos,From,To)
         ]
         ).
 % TODO: 
 % Complete definition of find_term_in_term/5 so that it also
 % searches correctly in terms with lists.
 
 
         
% This is an extension to the predefined predicate:
%          atom_to_term(+Atom, -Term, -Bindings)!
% one can think of the predefined predicate as being
% defined by:
% atom_to_term(TermAtom, -Term, -Bindings) :-
%    atom_to_term(TermAtom,Term,_,Bindings).
    
atom_to_term(TermAtom,Term,VarNames,TermPos) :-
    new_memory_file(MemFile),
    open_memory_file(MemFile,write,Out),
    write(Out,TermAtom),
    write(Out,'.'),
    close(Out),
    open_memory_file(MemFile,read,In),
    read_term(In,Term,[ 
              subterm_positions(TermPos),  
              variable_names(VarNames)% ,  variables(Vars)
    ]).


%% find_term_in_atom(+Atom,+SearchTerm,?From,?To)
find_term_in_atom(Atom,SearchTerm,From,To) :-
    atom_to_term(Atom,TermFromAtom,_,AllTermPos),
    find_term_in_term(SearchTerm,TermFromAtom,AllTermPos,From,To).

%% find_term_in_term(+SearchTerm,+Term,?TermPos,?From,?To)
find_term_in_term(SearchTerm,Term,AllTermPos,From,To) :-
    nonvar(SearchTerm),
    nonvar(Term),
    find_term_in_term__(SearchTerm,Term,AllTermPos,From,To).
    
find_term_in_term__(SearchTerm,Term,AllTermPos,From,To) :-
    SearchTerm = Term,   
    !,
    extract_position(AllTermPos,From,To).
% TODO: The following clause is still incomplete, se TODO marker:
%find_term_in_term__(SearchTerm,Term,TermPos,From,To) :-
%    list(Term),
%    !,
%    TermPos = list_position(From, To, ElemsPos, TailPos),  % See manual for read_term/3.
%    % TODO: Deal with ElemsPos and eventually TailPos,
%    nth1(N,SubPosList,ArgumentPos),
%    find_term_in_term(SearchTerm,Argument,ArgumentPos,From,To).     
find_term_in_term__(SearchTerm,Term,TermPos,From,To) :-
    TermPos = term_position(_,_,_,_,SubPosList),  % See manual for read_term/3.
    arg(N,Term,Argument),
    nth1(N,SubPosList,ArgumentPos),
    find_term_in_term(SearchTerm,Argument,ArgumentPos,From,To). 


%find_term_in_term(SearchTerm,Term,AllTermPos,From,To) :-
%    find_term_in_term(SearchTerm,Term,AllTermPos,0,From,To). % init
%
%find_term_in_term(SearchTerm,Term,AllTermPos,FromStart,From,To) :-
%    nonvar(SearchTerm),
%    nonvar(Term),
%    find_term_in_term__(SearchTerm,Term,AllTermPos,FromStart,From,To).
%    
%find_term_in_term__(SearchTerm,Term,AllTermPos,FromStart,From,To) :-
%    SearchTerm = Term,   
%    !,
%    extract_position(AllTermPos,FromTerm,ToTerm),
%    From is FromStart+FromTerm,
%    To is FromStart+ToTerm.
%find_term_in_term__(SearchTerm,Term,TermPos,FromStart,From,To) :-
%    TermPos = term_position(FromTerm,_,_,_,SubPosList),  % See manual for read_term/3.
%    arg(N,Term,Argument),
%    nth1(N,SubPosList,ArgumentPos),
%    FromStartNew is FromStart+FromTerm,
%    find_term_in_term(SearchTerm,Argument,ArgumentPos,FromStartNew,From,To). 
    
% For the meaning of the terms in the first argument  
% see the SWI-Prolog manual entry for read_term/3:    
extract_position( term_position(From, To, _, _, _),     From,To) :- !.
extract_position( From-To,                              From,To) :- !.
extract_position( string_position(From, To),            From,To) :- !.
extract_position( brace_term_position(From, To, _),     From,To) :- !.
extract_position( list_position(From, To, _, _),        From,To) :- !.


  
      


