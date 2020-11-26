
/* ------------------------------------------------------------------------
 > FILENAME:	parse_file
 > PURPOSE:	Main control for parsing a file of edge terms
 > AUTHORS:	Kevin Humphreys, Mark Hepple

 > NOTES:	format of edges assumed to be:
               edge(Vertex0,VertexN,
                    Cat, ToFind, Parents, Children,
                    Level,
                    Token0, TokenN,
                    EdgeID)
               where ToFind = [] (ie the edge is inactive)
 % edge(ID, StartVertex, EndVertex, Category, Found, ToFind)

 ------------------------------------------------------------------------ */

:- assert('$package_name'('shef.nlp.supple.prolog.cafe')).

cvsid_parse_file("$Id: parse_file.pl 7085 2005-12-05 16:32:03Z ian_roberts $").

% Horacio: 
% top level predicate
% returns the semantics
% the semantics goes to Java and to the Annotations

parse(Original,Output1):-
     retractall(external_sem_file(_)),
     retractall(semantic_output(_)),
     assert(semantic_output([])),
     parse(Original),
      semantic_output(Output),
      update_match(Output,Output1),
% new
output_file(FOUT),
open(FOUT,append,Handle),
write_semantics(Handle,Output1),
close(Handle).
% end 
%     (external_sem_file(External),
%      open(External,write,Handle),
%      write_semantics(Handle,Output1),
%      close(Handle);true).
     


/*
parse [-v]                be verbose
      [-d]                 show debugging info
      [-level n]          parse up to level n grammar & don't do semantics
      [-m markup_file]    write NE markup to markup_file
      [-p parse_file]     write best parses to parse_file in tree form
      [-b parse_file]     write best parses to parse_file in bracketed form
      [-c chart_file]     write chart to chart_file
      [-o output_file]    write gdm output to output_file instead of stdout
      file
*/


parse([]) :- !.
parse(['--'|Args]) :- !, % swi-prolog flag - ignore it
    parse(Args).
% -n : non-interactive : dumping saved state
parse(['-n'|Args]) :- !.
% -v : be verbose
parse(['-v'|Args]) :-
    verbose(1), % trigger progress messages on stdout
    (retract(verbose_output(_)) 
    ;(telling(Current), assert(verbose_output(Current)))),
    parse(Args), !.
% -d : show debugging info
parse(['-d'|Args]) :-
    buchart_debug(1), !,
    ((member('-p',Args);best_parse_file(_))
    ;(telling(Current),assert(best_parse_file(Current)))), !,
    parse(['-v'|Args]).
% -ne : run first grammar only
parse(['-ne'|Args]) :-
	best_parse_cats(grammar1,Cs),
	retractall(best_parse_cats(_,_)),
	assert(best_parse_cats(grammar1,Cs)),
	parse(Args), !.
% -p parse_file : dump best parse trees into parse_file
parse(['-p', File |Args]) :-
    retractall(best_parse_file(_)),
    assert(best_parse_file(File)),
    ((member('-v',Args),assert(verbose_output(File))) ; true),
    ((retract(verbose_output(_)),assert(verbose_output(File))) ; true),
    parse(Args), !.
% -b parse_file : dump bracketed best parse trees into parse_file
parse(['-b', File |Args]) :-
    retractall(best_parse_file(_)),
    assert(best_parse_file(File)),
    assert(bracketed_parses),
    ((member('-v',Args),assert(verbose_output(File))) ; true),
    ((retract(verbose_output(_)),assert(verbose_output(File))) ; true),
    parse(Args), !.
% -c chart_file : dump chart into chart_file
parse(['-c', File |Args]) :-
    retractall(chart_file(_)),
    assert(chart_file(File)),
    parse(Args), !.
% -g grammar_file : read in and compile a single grammar
parse(['-g', File |Args]) :-
	retractall(grammar_file(_)),
	assert(grammar_file(File)),
	parse(Args), !.
% -f : filter the final chart to remove ambiguities, even if not
%      specified in the grammar file
parse(['-f'|Args]) :- 
	retractall(filter_chart),
	assert(filter_grammar(_)),
	parse(Args), !.

% -flag flag file for signal to buchart
parse(['-flag',File | Args]):-
     retractall(flag_file(_)),
     assert(flag_file(File)),
     parse(Args), !.
% -o output file : redirect stdout
parse(['-o', File |Args]) :-
    retractall(output_file(_)),
    assert(output_file(File)),
    parse(Args), !.
parse([File|Args]) :-
    ((output_file(OutFile), tell(OutFile))
    ;(telling(Current), tell(Current))),
    retractall(chart(_,_,_)),
    retractall(gensymmark(_,_)),
    vwrite('reading initial chart...'),
    read_chart_file(File,Charts),
    vwrite('done'),vnl,
    (grammar_file(Grammar); Grammar = []),
    vwrite('parsing charts...'), vnl,
    parse_charts(Charts,Grammar),told,
    vwrite('done'),vnl,
    ((chart_file(ChartFile), % do this last due to quintus seg. faults
      vwrite('writing out chart to '),vwrite(ChartFile),vnl,
      write_chart_file(File,ChartFile)) ; true),
    parse(Args),!.



% parse with all compiled-in grammars
parse_charts(Charts,[]) :- !,
	findall(Grammar, best_parse_cats(Grammar,_), Grammars),
	parse_charts2(Charts,Grammars).
% compile and parse with single grammar specified on command line
parse_charts(Charts,Grammar) :-
	retractall(best_parse_cats(_,_)),
	retractall(filter_grammar(_)),
	compile_grammars([Grammar],GrammarID),
	parse_charts2(Charts,[GrammarID]).

parse_charts2([],_).
parse_charts2([Chart|Charts],Grammars) :-
	parse_chart(Chart,Grammars),
	parse_charts2(Charts,Grammars).
	
% parse a single chart
parse_chart(chart(_:Sentence,_:Edges,_:NextEdge),Grammars) :-
        % set up gensym for edge identifiers
	LastEdge is NextEdge - 1,
	retractall(gensymmark('',_)),
	assert(gensymmark('',LastEdge)),
	% parse
	parse_chart2(Edges,Grammars,Sentence).


% last/only grammar - output best edges
parse_chart2(InputEdges,[Grammar],Sentence) :-
        % clear database
	retractall(edge(_,_,_,_,_,_,_,_,_,_)),
	sort_edges(InputEdges,Level1Edges,MaxVertex),
	parse_edges(Level1Edges,Grammar),

	((best_parse_cats(Grammar,all),
          % use all level 2 (= new) edges as best parse
	  findall(edge(S,E,C,[],0,Cs,1,TS,TE,I), % convert to level 1
	          edge(S,E,C,[],Ps,Cs,2,TS,TE,I),
	          BestEdges))
	; best_parse(Grammar,BestEdges,MaxVertex)), % converted to level 1
	% write results
	display_tdm_attributes(Sentence,BestEdges,InputEdges),

	((chart_file(_),	% cascaded version
	  % record next input chart ready to write out
	  ((filter_grammar(Grammar),
	    next_edges(BestEdges,InputEdges,NextInputEdges))
	  ;(findall(edge(S,E,C,[],0,[],1,TS,TE,I), % remove children list
		    member(edge(S,E,C,[],Ps,Cs,1,TS,TE,I),BestEdges),
		    NewBestEdges),
	    append(InputEdges,NewBestEdges,NextInputEdges))),

	  gensymmark('',X), NextEdge is X + 1,
	  assert(chart(sentence_n:Sentence,
	               edges:NextInputEdges,
		       next_edge_number:NextEdge)))
        ; true),

	((best_parse_file(_),
	  assert_edge_list(InputEdges),
	  display_best_parse(Sentence,BestEdges))
	;true).

% filter grammar - use only best edges in next parse
parse_chart2(InputEdges,[Grammar|Grammars],Sentence) :-
	filter_grammar(Grammar),
        % clear database
	retractall(edge(_,_,_,_,_,_,_,_,_,_)),
	sort_edges(InputEdges,Level1Edges,MaxVertex),
	parse_edges(Level1Edges,Grammar),
	best_parse(Grammar,BestEdges,MaxVertex),
	% keep all children of best edges at level 0 for final output
        next_edges(BestEdges,InputEdges,NextBestEdges),
	parse_chart2(NextBestEdges,Grammars,Sentence).

parse_chart2(InputEdges,[Grammar|Grammars],Sentence) :-
        % clear database
	retractall(edge(_,_,_,_,_,_,_,_,_,_)),
	sort_edges(InputEdges,Level1Edges,MaxVertex),
	parse_edges(Level1Edges,Grammar),
        % use all level 2 (= new) edges in next parse
	findall(edge(S,E,C,[],0,Cs,1,TS,TE,I),
	        edge(S,E,C,[],Ps,Cs,2,TS,TE,I),
	        NewEdges),
	append(InputEdges,NewEdges,NextInputEdges),
	parse_chart2(NextInputEdges,Grammars,Sentence).


% next_edges/3: convert all edges in best parse to level 1
next_edges([],_,[]).
next_edges([edge(S,E,C,[],Ps,Cs,_,TS,TE,I)|BestEdges],InputEdges,NextEdges) :-
        % and add children of level 2 edges as level 0 edges
	child_edges(Cs,InputEdges,ChildEdges),
	next_edges(BestEdges,InputEdges,RestEdges),
	append([edge(S,E,C,[],0,Cs,1,TS,TE,I)|ChildEdges],RestEdges,NextEdges).
next_edges([_|BestEdges],InputEdges,NextEdges) :-
	next_edges(BestEdges,InputEdges,NextEdges).

%child_edges(_,_,[]) :- !. % syntax output not required
child_edges([],_,[]).
child_edges([ID|RestIDs],InputEdges,Children) :- %nonvar(InputEdges),
	% check in both current chart and level 0 input
	(edge(S,E,C,[],Ps,Cs,_,TS,TE,ID)
        ;memberchk(edge(S,E,C,[],Ps,Cs,0,TS,TE,ID),InputEdges)),
	child_edges(Cs,InputEdges,CsChildren),
	child_edges(RestIDs,InputEdges,RestChildren),
	% convert to level 0, preventing inclusion in any future parse
	append([edge(S,E,C,[],Ps,Cs,0,TS,TE,ID)|CsChildren],RestChildren,Children).
child_edges([_|RestIDs],InputEdges,Children) :-
	child_edges(RestIDs,InputEdges,Children).



parse_edges([],GrammarID) :- !.
parse_edges([edge(V0,Vn,C,T,Ps,Cs,L,T0,Tn,EdgeID)|Edges],GrammarID) :-
%C =.. [Cat|_],write('adding '),write(Cat),
%write(' edge at '),write(V0),write('-'),write(Vn),nl,
    add_edge(V0,Vn,C,T,Ps,Cs,L,T0,Tn,EdgeID,GrammarID),
    parse_edges(Edges,GrammarID),!.


sort_edges(As,Cs,Max):- 
    sort_edges(As,[],Cs,0,Max).
	
% return sorted level 1 edges only, and remove creator
sort_edges([],Bs,Bs,Max,Max). 
sort_edges([edge(E0,E1,Cat,[],Ps,Chs,1,T1,TN,ID)|As],Bs,Cs,CMax,Max):- 
    add_to_sorted_edges(edge(E0,E1,Cat,[],0,Chs,1,T1,TN,ID),Bs,Bs1), 
    % find max vertex while we're scanning through 
    ((E1 >= CMax, NMax = E1)
    ;(E1 < CMax, NMax = CMax)),
    sort_edges(As,Bs1,Cs,NMax,Max). 
sort_edges([_|As],Bs,Cs,CMax,Max) :-
    sort_edges(As,Bs,Cs,CMax,Max).
	

add_to_sorted_edges(A,[B|Bs],[B|Cs]):- 
    A = edge(S1,_,_,_,_,_,_,_,_,_), 
    B = edge(S2,_,_,_,_,_,_,_,_,_), 
    S2 =< S1, !, % maintain input ordering for entries at same position
    add_to_sorted_edges(A,Bs,Cs). 
add_to_sorted_edges(A,Bs,[A|Bs]).


assert_edge_list([]).
assert_edge_list([edge(A,B,C,D,E,F,G,H,I,J)|Rest]) :-
	edge(A,B,C,D,E,F,G,H,I,J),
	assert_edge_list(Rest).
assert_edge_list([edge(A,B,C,D,E,F,G,H,I,J)|Rest]) :-
	assert(edge(A,B,C,D,E,F,G,H,I,J)),
	assert_edge_list(Rest).



parse_file(InFile,OutFile) :-
    (grammar_file(Grammar); Grammar = []),
    parse_file(InFile,OutFile,Grammar).

parse_file(InFile,OutFile,Grammar) :-
    retractall(chart(_,_,_,_,_)),
    read_chart_file(InFile,Charts),
    parse_charts(Charts,Grammar),
    write_chart_file(InFile,OutFile).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Revision 1.4  2002/03/15 13:26:00  saggion
% *** empty log message ***
%
% Revision 1.5  2002/02/11 13:32:24  saggion
% *** empty log message ***
%
% Revision 1.4  2001/12/19 11:56:34  saggion
% *** empty log message ***
%
% Revision 1.3  2001/12/11 13:25:16  saggion
% nothing
%
% Revision 1.2  2001/12/04 17:46:01  saggion
% Dealing with the external call (-sem argument, parse_file.pl and buchart_io.pl)
%
% Revision 1.1  2001/12/04 13:08:26  saggion
% files and directories for buchart now in mumis (from Lasie3)
%
% Revision 1.1  2001/06/26 17:28:36  saggion
% New directory for buchart and disint resources
%
% Revision 1.3  2001/05/14 17:42:57  saggion
% Modifications to buchart
%
% Revision 1.17  1998/02/20 16:12:04  kwh
% sicstus runtime version
%
% Revision 1.16  1998/01/09 16:43:30  kwh
% tree output (-p and -b options) working again
%
% Revision 1.15  1998/01/09 15:39:34  kwh
% -ne flag to run with ne grammar only
%
% Revision 1.14  1998/01/08 14:11:47  kwh
% use simple digits as rule id numbers, rather than ruleX format, to make comparison by sort correct
%
% Revision 1.13  1997/12/09 16:38:57  kwh
% remove best parse children from output chart (for next subgrammar)
%
% Revision 1.12  1997/12/01 15:55:52  kwh
% rationalised top level control, merge subgrammars during compilation, pass child edges through for syntax output, and revised docs
%
% Revision 1.11  1997/11/18 18:47:56  kwh
% keep all leaf/intermediate edges in chart for complete syntax output
%
% Revision 1.10  1997/10/15 13:41:58  kwh
% keep child histories in best parse charts
%
% Revision 1.9  1997/10/02 17:41:53  kwh
% minor speedup
%
% Revision 1.8  1997/09/30 17:24:47  kwh
% merge buchart_cascade changes back in
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

