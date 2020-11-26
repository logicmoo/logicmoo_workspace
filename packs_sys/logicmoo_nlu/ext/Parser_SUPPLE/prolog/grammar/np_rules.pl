% Full Noun Phrase rules
%

:- multifile best_parse_cats/1, rule/2.
:- dynamic best_parse_cats/1, rule/2.

% Best Parse Categories
best_parse_cats([np,pp]).


%% NP -> BNP
rule(np(sem:E^S,number:N), [
    bnp(sem:E^S,number:N)
]).

%% NP -> BNP PP
% minimally attach all PPs to initial nps
rule(np(sem:E^X^[N1S,N2S],number:N), [
    top(_),
    bnp(sem:E^N1S,number:N),
    pp(sem:E^X^N2S)
]).

%% NP -> BNP PP(of) % minimally attach all 'of' PPs
rule(np(sem:E^X^[N1S,N2S,[of,E,X]],number:N), [
    bnp(sem:E^N1S,number:N),
    pp(sem:E^X^[[of,E,X],N2S])
]).


%% SAM - NPs can be quoted.
rule(np(sem:NPSem),
    [sym(s_form:'"'),
     np(sem:NPSem),
     sym(s_form:'"')
]).

%% SAM - Book titles have subtitles.
%      - THis should probably be in the N.E. grammar
rule(ne_np(s_form:[S1,': ',S2],sem:N1^N2^[NPSem,NP2Sem,[apposed,N1,N2]]),
    [sym(s_form:'"'),
     np(s_form:S1,sem:N1^NPSem),
     sym(s_form:':'),
     np(s_form:S2,sem:N2^NP2Sem),
     sym(s_form:'"')
]).

%% NP -> BNP PP(in) if the pp is a location
%rule(np(sem:E^X^[N1S,N2S,[in,E,X]],number:N), [
%    bnp(sem:E^N1S,number:N),
%    pp(sem:E^X^[[in,E,X],N2S])
%]).
%
%% NP -> BNP PP(near)
% minimally attach all 'near' PPs (ie. assume location pp)
%rule(np(sem:E^X^[N1S,N2S,[in,E,X]],number:N), [
%    bnp(sem:E^N1S,number:N),
%    pp(sem:E^X^[[near,E,X],N2S])
%]).

% NP -> BNP ( NP )
% SAM -- The Human Genome Organization (HUGO) that is trying...
rule(np(sem:E1^E2^V^X^[S1,S2,RCSem,N2S,[apposed,E1,E2]],number:N), [
    bnp(sem:E1^S1,number:N),
    sym(s_form:'('),
    np(sem:E2^S2,number:N),
    sym(s_form:')'),
    {relc(sem:E1^V^RCSem)},
    {pp(sem:E1^X^N2S)}
]).

% NP -> BNP COMMA NP PP COMMA
% apposition and pp attachment: Steve, spokesman for IBM,
rule(np(sem:E1^E2^X^[S1,S2,N2S,[apposed,E1,E2]],number:N), [
    bnp(sem:E1^S1,number:N),
    comma(_),
    np(sem:E2^S2,number:N),
    pp(sem:E2^X^N2S),
    comma(_)
]).

% NP -> BNP COMMA NP PP period bottom
% apposition and pp attachment: Steve, spokesman for IBM,
rule(np(sem:E1^E2^X^[S1,S2,N2S,[apposed,E1,E2]],
	      number:N), [
    bnp(sem:E1^S1,number:N),
    comma(_),
    np(sem:E2^S2,number:N),
    pp(sem:E2^X^N2S),
    period(_),
    bottom(_)
]).

% NP -> BNP COMMA NP PP  bottom
% apposition and pp attachment: Steve, spokesman for IBM,
rule(np(sem:E1^E2^X^[S1,S2,N2S,[apposed,E1,E2]],
	      number:N), [
    bnp(sem:E1^S1,number:N),
    comma(_),
    np(sem:E2^S2,number:N),
    pp(sem:E2^X^N2S),
    bottom(_)
]).

%% NP -> BNP PP(of) % minimally attach all 'of' PPs
rule(np(sem:E^X^[N1S,N2S,[of,E,X]],number:N), [
    bnp(sem:E^N1S,number:N),
    pp(sem:E^X^[[of,E,X],N2S])
]).

% NP -> NP COMMA NP COMMA
% apposition: the chairman, Barry Diller,
rule(np(sem:E1^E2^[S1,S2,[apposed,E1,E2]],
	      number:N), [
    np(sem:E1^S1,number:N),
    comma(_),
    np(sem:E2^S2,number:N),
    comma(_)
]).

% NP -> NP COMMA NP period bottom
% apposition: the chairman, Barry Diller. bottom
rule(np(sem:E1^E2^[S1,S2,[apposed,E1,E2]],
	      number:N), [
    np(sem:E1^S1,number:N),
    comma(_),
    np(sem:E2^S2,number:N),
    period(_),
    bottom(_)
]).

% NP -> NP COMMA NP
% SAM -- apposition: the chairman, Barry Diller? bottom
%     -- but we can't consume the '?' or the question grammar will not work.
%     -- this rule might cause problems...
% REMOVED as part of experiment to see why grammar hanging up.
%rule(np(sem:E1^E2^[S1,S2,[apposed,E1,E2]],
%              number:N), [
%    np(sem:E1^S1,number:N),
%    comma(_),
%    np(sem:E2^S2,number:N)
%]).

% NP -> NP COMMA NP bottom
% apposition: the chairman, Barry Diller bottom
rule(np(sem:E1^E2^[S1,S2,[apposed,E1,E2]],
	      number:N), [
    np(sem:E1^S1,number:N),
    comma(_),
    np(sem:E2^S2,number:N),
    bottom(_)
]).

%% NP -> NP COMMA NP SYM('--')
%% apposition: the chairman, Barry Diller --
%rule(np(sem:E1^E2^[S1,S2,[apposed,E1,E2]],
%	      number:N), [
%    np(sem:E1^S1,number:N),
%    comma(_),
%    np(sem:E2^S2,number:N),
%    sym(s_form:'-'),	
%    sym(s_form:'-')
%]).
%
%% NP -> NP SYM('--') NP 
%% apposition: XXX -- a joint venture
%rule(np(sem:E1^E2^[S1,S2,[apposed,E1,E2]],
%	      number:N), [
%    np(sem:E1^S1,number:N),
%    sym(s_form:'-'),	
%    sym(s_form:'-'),
%    np(sem:E2^S2,number:N)
%]).
%
%% NP -> NP SYM('(') NP SYM(')')
%% abbreviation: British Petroleum (BP)
%rule(np(sem:E1^E2^[S1,S2,[apposed,E1,E2]]), [
%    np(sem:E1^S1,number:N),
%    sym(s_form:'('),
%    np(sem:E2^S2,number:N),
%    sym(s_form:')')
%]).

%%%%%%%%%%%%%%%%%%%%for processing lists of nps%%%%%%%%%%%%%%%%%%%%%
 
% NP -> NP CC NP (two element list)
% conjunction: the chairman and CEO
rule(np(edge:Edge,sem:E3^E2^E1^[S1,S2,[coord,E2,E1],
                      [set,E3],[set_member,E3,E1],[set_member,E3,E2],[number,E3,plural],
                      [realisation,E3,Edge]],
              number:plural), [
    np(sem:E1^S1,number:N),
    cc(_),
    np(sem:E2^S2,number:N)
]).
 
%nplist -> np, np (the first two elements of the list)
rule(nplist(edge:Edge,sem:E3^E2^E1^[S1,S2,[coord,E2,E1],
                      [set,E3],[set_member,E3,E1],[set_member,E3,E2],[number,E3,plural],
                      [realisation,E3,Edge]],
              number:plural), [
    np(sem:E1^S1,number:N),
    comma(_),
    np(sem:E2^S2,number:N)
]).
 
%np -> nplist,  np  (the middle of the list)
rule(nplist(edge:Edge,sem:E1^X^E2^[S1,S2,[coord,X,E2],
                      [set_member,E1,E2],[realisation,E1,Edge]],
              number:plural), [
    nplist(sem:E1^X^S1,number:N),
    comma(_),
    np(sem:E2^S2,number:N)
]).
 
%np -> nplist, cc np  (the end of the list)
rule(np(edge:Edge,sem:E1^X^E2^[S1,S2,[coord,X,E2],
                      [set_member,E1,E2],[realisation,E1,Edge]],
              number:plural), [
    nplist(sem:E1^X^S1,number:N),
    comma(_),
    cc(_),
    np(sem:E2^S2,number:N)
]).
 
%np -> nplist cc np  (the end of the list)
rule(np(edge:Edge,sem:E1^X^E2^[S1,S2,[coord,X,E2],
                      [set_member,E1,E2],[realisation,E1,Edge]],
              number:plural), [
    nplist(sem:E1^X^S1,number:N),
    cc(_),
    np(sem:E2^S2,number:N)
]).
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% retry PP rules which may include any new NPs

%% PP -> IN NP
rule(pp(sem:E^X^[[R,E,X],NPSem]),
    [in(m_root:R),
     np(sem:X^NPSem)]).

%% PP -> TO NP
rule(pp(sem:E^X^[[R,E,X],NPSem]),
    [to(m_root:R),
     np(sem:X^NPSem)]).
