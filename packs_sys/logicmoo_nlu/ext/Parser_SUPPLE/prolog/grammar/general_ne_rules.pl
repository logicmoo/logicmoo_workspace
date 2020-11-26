
%
:- multifile best_parse_cats/1, rule/2.
:- dynamic best_parse_cats/1, rule/2.

% Best Parse Categories
%%% pass on all inactive edges
best_parse_cats(all).


% NE_NP --> NAMES_NP
% ambiguous name
rule(ne_np(s_form:F,text:T,sem:S),[
        names_np(s_form:F,text:T,sem:S)]). % unclassified NE
 
%
% NAMES_NP rules
% NAMES_NPs have an ne_tag in their semantics for use in person and
% location rules - their semantics are just ignored in all other cases
%
%% NAMES_NP --> PN NAMES_NP
rule(names_np(edge:Edge,s_form:[F1,' ',F2],text:body,
              sem:E^[[ne_tag,E,Edge],[name,E,[F1,' ',F2]]]),[
        pn(s_form:F1,text:body),
        names_np(s_form:F2)
    ]).

%% NAMES_NP --> PN NAMES_NP
rule(names_np(edge:Edge,s_form:[F1,' ',F2],text:body,
              sem:E^[[ne_tag,E,Edge],[name,E,[F1,' ',F2]]]),[
        date(s_form:F1,text:body),
        names_np(s_form:F2)
    ]).

%% NAMES_NP --> PN SYM(-) NAMES_NP
rule(names_np(edge:Edge,s_form:[F1,'-',F2],text:body,
              sem:E^[[ne_tag,E,Edge],[name,E,[F1,'-',F2]]]),[
        pn(s_form:F1,text:body),
	sym(s_form:'-'),
        names_np(s_form:F2)
    ]).

%% NAMES_NP --> PN
rule(names_np(edge:Edge,s_form:F,text:T,sem:E^[[ne_tag,E,Edge],[name,E,F]]),[
        pn(s_form:F,text:T)
    ]).

% NAMES_NP2
%
% NOTE: These rules are currently decoupled from the grammar (they never
% appear on the RHS of a rule.  Deleted by Sam as an experiment on why
% the grammar hangs on lists separated by commas.
%
% for a series of names separated by comma (recusive rules)
%% NAMES_NP2 --> PN COMMA NAMES_NP2
%rule(names_np2(s_form:[F1,', ',F2]),[
%       pn(s_form:F1),
%       comma(s_form:','),
%       names_np2(s_form:F2)
%        ]).

%% NAMES_NP2 --> LIST_NP(person) COMMA NAMES_NP2
%rule(names_np2(s_form:[F1,', ',F2]),[
%       list_np(s_form:F1,ne_tag:person),
%       comma(s_form:','),
%       names_np2(s_form:F2)
%        ]).

% NAMES_NP2 base cases
%% NAMES_NP2 --> PN COMMA PN
%rule(names_np2(s_form:[F1,', ',F2]),[
%       pn(s_form:F1),
%       comma(s_form:','),
%       pn(s_form:F2)
%        ]).

%% NAMES_NP2 --> LIST_NP(person) COMMA PN
%rule(names_np2(s_form:[F1,', ',F2]),[
%       list_np(s_form:F1,ne_tag:person),
%       comma(s_form:','),
%       pn(s_form:F2)
%        ]).

%% NAMES_NP2 --> PN COMMA LIST_NP(person)
%rule(names_np2(s_form:[F1,', ',F2]),[
%       pn(s_form:F1),
%       comma(s_form:','),
%       list_np(s_form:F2,ne_tag:person)
%        ]).

%% NAMES_NP2 --> LIST_NP(person) COMMA LIST_NP(person)
%rule(names_np2(s_form:[F1,', ',F2]),[
%       list_np(s_form:F1,ne_tag:person),
%       comma(s_form:','),
%       list_np(s_form:F2,ne_tag:person)
%        ]).

% NAMES_NP3
% name 'and' name
%% NAMES_NP3 --> PN CC(and) PN
rule(names_np3(s_form:[F1,' and ',F2]),[
       pn(s_form:F1),
       cc(s_form:'and'),
       pn(s_form:F2)
        ]).


%% ORDINAL --> CD "ST"
rule(ordinal(s_form:[F1,' ',F2]),[
	cd(s_form:F1),
	n(s_form:F2,m_root:'st')
	]).
%% ORDINAL --> CD "ND"
rule(ordinal(s_form:[F1,F2]),[
	cd(s_form:F1),
	cc(s_form:F2,m_root:'nd')
	]).
%% ORDINAL --> CD "RD"
rule(ordinal(s_form:[F1,F2]),[
	cd(s_form:F1),
	pn(s_form:F2,m_root:'rd')
	]).
%% ORDINAL --> CD "RD"
rule(ordinal(s_form:[F1,F2]),[
	cd(s_form:F1),
	n(s_form:F2,m_root:'rd')
	]).
%% ORDINAL --> CD "TH"
rule(ordinal(s_form:[F1,F2]),[
	cd(s_form:F1),
	dt(s_form:F2,m_root:'th')
	]).


%% CHAR --> single uppercase alphabetic character
rule(char(s_form:'A'),[pn(s_form:'A')]).
rule(char(s_form:'A'),[dt(s_form:'A')]).
rule(char(s_form:'B'),[pn(s_form:'B')]).
rule(char(s_form:'C'),[pn(s_form:'C')]).
rule(char(s_form:'D'),[pn(s_form:'D')]).
rule(char(s_form:'E'),[pn(s_form:'E')]).
rule(char(s_form:'F'),[pn(s_form:'F')]).
rule(char(s_form:'G'),[pn(s_form:'G')]).
rule(char(s_form:'H'),[pn(s_form:'H')]).
rule(char(s_form:'I'),[pn(s_form:'I')]).
rule(char(s_form:'I'),[pp(s_form:'I')]).
rule(char(s_form:'J'),[pn(s_form:'J')]).
rule(char(s_form:'K'),[pn(s_form:'K')]).
rule(char(s_form:'L'),[pn(s_form:'L')]).
rule(char(s_form:'M'),[pn(s_form:'M')]).
rule(char(s_form:'N'),[pn(s_form:'N')]).
rule(char(s_form:'O'),[pn(s_form:'O')]).
rule(char(s_form:'P'),[pn(s_form:'P')]).
rule(char(s_form:'Q'),[pn(s_form:'Q')]).
rule(char(s_form:'R'),[pn(s_form:'R')]).
rule(char(s_form:'S'),[pn(s_form:'S')]).
rule(char(s_form:'T'),[pn(s_form:'T')]).
rule(char(s_form:'U'),[pn(s_form:'U')]).
rule(char(s_form:'V'),[pn(s_form:'V')]).
rule(char(s_form:'W'),[pn(s_form:'W')]).
rule(char(s_form:'X'),[pn(s_form:'X')]).
rule(char(s_form:'Y'),[pn(s_form:'Y')]).
rule(char(s_form:'Z'),[pn(s_form:'Z')]).

