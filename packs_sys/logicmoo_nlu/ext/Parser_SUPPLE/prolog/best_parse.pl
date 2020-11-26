
/* ------------------------------------------------------------------------
 > FILENAME:	best_parse
 > PURPOSE:	
 > AUTHORS:	Mark Hepple
 > NOTES:	
 ------------------------------------------------------------------------ */

:- assert('$package_name'('shef.nlp.supple.prolog.cafe')).

cvsid_best_parse("$Id: best_parse.pl 7085 2005-12-05 16:32:03Z ian_roberts $").



:- dynamic vertex_best_parse/2. 


%best_parse_file(InFile,OutFile) :-
%    retractall(chart(_,_,_,_,_)),
%    read_chart_file(InFile,Charts,Doc),
%    parse_charts(Charts,[]), % only inactive edges in output
%    write_semantics_file(InFile,OutFile,Doc).


best_parse(Grammar,Edges,Max) :-
    best_parse_cats(Grammar,Cats),
    retractall(vertex_best_parse(_,_,_)), 
    assert(vertex_best_parse(0,(0-0),nil)),
    best_parse_loop(1,Max,Cats,Grammar), 
    get_best_parse_edges(Max,Edges). 

get_best_parse_edges(Max,Edges):- 
    get_best_parse_edges(Max,[],Edges). 

get_best_parse_edges(V,Edges,Edges):- 
    V < 1, !. 
get_best_parse_edges(V,Edges1,Edges0):- 
    vertex_best_parse(V,_Score,nil), !, 
    V1 is V-1, 
    get_best_parse_edges(V1,Edges1,Edges0). 
get_best_parse_edges(V,Edges1,Edges0):- 
    vertex_best_parse(V,_Score,ID), 
    edge(V1,V,C,[],Ps,Chs,Level,T1,T2,ID), 
    E = edge(V1,V,C,[],[],Chs,1,T1,T2,ID), % convert to level 1
    get_best_parse_edges(V1,[E|Edges1],Edges0). 

best_parse_loop(CurrV,Max,_Cats,Grammar):- 
    CurrV > Max, !.  
best_parse_loop(CurrV,Max,Cats,Grammar):- 
    best_parse_vertex(CurrV,Cats,Grammar), 
    NextV is CurrV + 1, 
    best_parse_loop(NextV,Max,Cats,Grammar). 

best_parse_vertex(Vertex,Cats,Grammar):- 
    PrevVertex is Vertex - 1, 
    best_parse_vertex(PrevVertex,Vertex,Cats,Score,EdgeId,Grammar), 
    assert(vertex_best_parse(Vertex,Score,EdgeId)).
    
best_parse_vertex(CurrV,_V,_Cats,(0-0),nil,Grammar):- 
    CurrV < 0, !.
best_parse_vertex(CurrV,V,Cats,Score0,E0,Grammar):- 
    best_parse_highest_edge(CurrV,V,Cats,E1,Grammar),
    vertex_best_parse(CurrV,(Cov-Count),_E), 
    Cov1 is Cov + (V - CurrV), 
    Count1 is Count + 1, 
    Score1  = (Cov1 - Count1), 
    PrevV is CurrV - 1, 
    best_parse_vertex(PrevV,V,Cats,Score2,E2,Grammar), 
    pick_better_score_edge(Score1,E1,Score2,E2,Score0,E0). 
best_parse_vertex(CurrV,V,Cats,Score,E,Grammar):- 
    PrevV is CurrV - 1, 
    best_parse_vertex(PrevV,V,Cats,Score,E,Grammar). 

% for NE grammar, prefer edges with source:list feature
best_parse_highest_edge(CurrV,V,BestCats,E1,grammar1) :-
	edge(CurrV,V,Cat,[],_,_,_,_,_,E1), 
	Cat =.. [C|Fs], memberchk(C,BestCats),
	memberchk(source:Source,Fs), nonvar(Source), Source = list,
	!.
% find first (= most recently added) best_parse edge at current position
best_parse_highest_edge(CurrV,V,BestCats,E1,Grammar) :-
	edge(CurrV,V,Cat,[],_,_,_,_,_,E1), 
	Cat =.. [C|_], memberchk(C,BestCats), !.
best_parse_highest_edge(CurrV,V,_,E,Grammar) :-
	filter_grammar(Grammar), % -f command line flag
	V is CurrV + 1,
	% find lexical edge from original input
	edge(CurrV,V,Cat,[],_,_,1,_,_,E),
	Cat =.. [C|_], C \= list_np.


pick_better_score_edge(Score1,E1,Score2,_E2,Score1,E1):- 
    is_better_than_score(Score1,Score2), !.
pick_better_score_edge(_Score1,_E1,Score2,E2,Score2,E2).

% same coverage, less edges
is_better_than_score((C-E1),(C-E2)):- !, E1 < E2. 
% greater coverage
is_better_than_score((C1-_E1),(C2-_E2)):- C1 > C2. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Revision 1.9  1998/04/01 18:25:13  kwh
% added source feature - preferred by best parse if source=list for ne grammar
%
% Revision 1.8  1998/03/31 00:27:58  kwh
% filter out list_nps when passing on leaf edges
%
% Revision 1.7  1998/02/25 16:53:57  kwh
% allow for 'top' edge at vertex 0
%
% Revision 1.6  1997/12/01 15:55:43  kwh
% rationalised top level control, merge subgrammars during compilation, pass child edges through for syntax output, and revised docs
%
% Revision 1.5  1997/10/15 13:42:53  kwh
% allow for leaf node with children
%
% Revision 1.4  1997/09/30 17:24:31  kwh
% merge buchart_cascade changes back in
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
