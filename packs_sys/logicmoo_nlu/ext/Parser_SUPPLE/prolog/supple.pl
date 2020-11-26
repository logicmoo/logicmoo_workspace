
/* ------------------------------------------------------------------------
 > FILENAME:	supple
 > PURPOSE:	
 > AUTHORS:	Mark Hepple, Rob Gaizauskas, Kevin Humphreys
 > NOTES:	
 ------------------------------------------------------------------------ */

:- assert('$package_name'('shef.nlp.supple.prolog.cafe')).

cvsid_buchart("$Id: supple.pl 7085 2005-12-05 16:32:03Z ian_roberts $").


:- dynamic edge/10, chart/5, semantics/2.

:- op(505,xfx,:).       % feature:value separator
:- op(10,xfy,^).        % pseudo-lambda operator for extraposing
                        % arguments from terms


%
% NAME: 	add_edge/11
% PURPOSE:	add a new edge to the chart, possible adding further
%               edges as applications of the bottom-up and fundamental
%               rule allow
% ARGS:		()
% NOTES:	
%

%%% check for equivalent inactive edge
add_edge(V0,V1,Cat1,[],RuleID1,Children,Level,T0,T1,EdgeID,GrammarID) :-
    edge(V0,V1,Cat2,[],RuleID2,_,_,_,_,_),
    % surface forms can be different
    Cat1 =.. [Cat, edge:E, sem:S1, head:H, s_form:_ | Features1],
    Cat2 =.. [Cat, edge:E, sem:S2, head:H, s_form:_ | Features2],
    % and either same semantics
    (S1=S2
    % or different semantics added by a later rule
    ;sort([RuleID1,RuleID2],[RuleID1,RuleID2])),
    !.

%%% attempt full unification, including semantics
add_edge(V0,V1,Cat,ToFind,RuleID,Children,Level,T0,T1,EdgeID,GrammarID) :-
    edge(V0,V1,Cat,ToFind,_,_,_,_,_,_), !.

add_edge(V1,V2,Cat1,[],RuleID,Children1,Level,T1,T2,ID1,GrammarID) :-
    assert_edge(V1,V2,Cat1,[],RuleID,Children1,Level,T1,T2,ID1),
    %%% assign all newly created edges Level 2
    combine_edge(V1,V2,Cat1,2,T1,T2,ID1,GrammarID). 


combine_edge(LeftV,RightV,Cat,Level,Tok1,Tok2,EdgeID,GrammarID):- 
    compiled_rule(Cat,Daughters,Mother,RuleID,GrammarID), 
%write('trying '),write(RuleID),nl,
    find_daughter_edges(LeftV,FarLeftV,Tok1,NewTok1,Daughters,DaughterIDs), 
%write('suceeded'),nl,
    %add the edge over all the constituents or skip bottom
    ((\+(Cat =.. [bottom|_]),
    add_edge(FarLeftV,RightV,Mother,[],RuleID,[EdgeID|DaughterIDs],
             Level,NewTok1,Tok2,_NewEdgeID,GrammarID));
    (Cat =.. [bottom|_],
    add_edge(FarLeftV,LeftV,Mother,[],RuleID,DaughterIDs,
             Level,NewTok1,Tok2,_NewEdgeID,GrammarID))),
    fail.
combine_edge(_,_,_,_,_,_,_,_). 

%This ignores the top marker so that it is left for another
%rule to check; it is different because it doesn't add the extra daughter token.
find_daughter_edges(LeftV,FarLeftV,_LeftTok,FarLeftTok,
                   [Cat|Daughters],DaughterIDs):- 
    Cat =.. [top|_],
    edge(NewLeftV,LeftV,top(s_form:top,m_root:top,m_affix:_,text:_),[],_Parents,_,_,NewLeftTok,_,EdgeID), 
    find_daughter_edges(LeftV,FarLeftV,NewLeftTok,FarLeftTok,
                        Daughters,DaughterIDs). 

find_daughter_edges(LeftV,FarLeftV,_LeftTok,FarLeftTok,
                    [Cat|Daughters],[EdgeID|DaughterIDs]):- 
    %assure that it is not a top arc.
    \+(Cat =..[top|_]),
    edge(NewLeftV,LeftV,Cat,[],_Parents,_,_,NewLeftTok,_,EdgeID), 
    find_daughter_edges(NewLeftV,FarLeftV,NewLeftTok,FarLeftTok,
                        Daughters,DaughterIDs).

find_daughter_edges(LeftV,LeftV,LeftTok,LeftTok,[],[]).


%
% NAME: 	assert_edge/10
% PURPOSE:	gensym an edge identifier and bind it into the feature
%               structure associated with the edge
% ARGS:		()
% NOTES:	ultimately for use in semantic representation to allow
%               event/object identifiers to point back to the surface
%               structure which generated them, for e.g. surface
%               marking of coreference 
%

assert_edge(V1,V2,Cat1,[],Parents,Children,Level,T1,T2,ID) :-
        Cat1 =.. [Cat | Features],
        (member(edge:offsets(T1,T2),Features);true),
        ((var(ID),buchart_gensym('',ID));true),
        asserta(edge(V1,V2,Cat1,[],Parents,Children,Level,T1,T2,ID)),!.
assert_edge(V1,V2,Cat1,[Cat2|ToFind],Parents,Children,Level,T1,T2,ID) :-
        asserta(edge(V1,V2,Cat1,[Cat2|ToFind],Parents,Children,Level,T1,T2,ID)),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Revision 1.10  1998/03/30 18:17:57  kwh
% fix top rule to allow for new affix
%
% Revision 1.9  1998/02/25 18:33:33  kwh
% exclude top from daughter list
%
% Revision 1.8  1998/02/24 21:17:33  kwh
% new default ne grammar, using top/bottom
%
% Revision 1.7  1997/12/01 15:55:44  kwh
% rationalised top level control, merge subgrammars during compilation, pass child edges through for syntax output, and revised docs
%
% Revision 1.6  1997/11/12 13:29:08  huyck
% Added changes for top and bottom and kludged the comma problem.
%
% Revision 1.5  1997/09/30  17:24:33  kwh
% merge buchart_cascade changes back in
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

