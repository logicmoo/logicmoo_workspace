% Fix up ne rules
%

:- multifile best_parse_cats/1, filter_chart/0, rule/2.
:- dynamic best_parse_cats/1, filter_chart/0, rule/2.

best_parse_cats([ne_np,bnp,cd_np]).

filter_chart.

%%% unclassified ne_nps have ne_tag as the first entry in their semantics
%%% classified ne_nps have their semantic type as the first entry

%%% PP rules for changing unclassified ne_nps (X) to locations

% X at TIME
rule(ne_np(s_form:F1,sem:
           E1^E2^[[location,E1],[ne_tag,E1,Tag1],Sem1,
		  [time,E2],Sem2,
		  [at,E1,E2]]),[
        ne_np(s_form:F1,sem:E1^[[ne_tag,E1,Tag1]|Sem1]),
        in(m_root:'at'),
        ne_np(s_form:F2,sem:E2^[[time,E2]|Sem2])
]).
% X, at TIME
rule(ne_np(s_form:F1,sem:
           E1^E2^[[location,E1],[ne_tag,E1,Tag1],Sem1,
		  [time,E2],Sem2,
		  [at,E1,E2]]),[
        ne_np(s_form:F1,sem:E1^[[ne_tag,E1,Tag1]|Sem1]),
        comma(_),
	in(m_root:'at'),
        ne_np(s_form:F2,sem:E2^[[time,E2]|Sem2])
]).

% LOC in X
rule(ne_np(s_form:F1,sem:
           E1^E2^[[location,E1],Sem1,
		  [location,E2],[ne_tag,E2,Tag2],Sem2,
		  [in,E1,E2]]),[
        ne_np(s_form:F1,sem:E1^[[location,E1]|Sem1]),
        in(m_root:'in'),
        ne_np(s_form:F2,sem:E2^[[ne_tag,E2,Tag2]|Sem2])
]).
% LOC, in X
rule(ne_np(s_form:F1,sem:
           E1^E2^[[location,E1],Sem1,
		  [location,E2],[ne_tag,E2,Tag2],Sem2,
		  [in,E1,E2]]),[
        ne_np(s_form:F1,sem:E1^[[location,E1]|Sem1]),
        comma(_),
        in(m_root:'in'),
        ne_np(s_form:F2,sem:E2^[[ne_tag,E2,Tag2]|Sem2])
]).

% ORG in X
rule(ne_np(s_form:F1,sem:
           E1^E2^[[organization,E1],Sem1,
		  [location,E2],[ne_tag,E2,Tag2],Sem2,
		  [in,E1,E2]]),[
        ne_np(s_form:F1,sem:E1^[[organization,E1]|Sem1]),
        in(m_root:'in'),
        ne_np(s_form:F2,sem:E2^[[ne_tag,E2,Tag2]|Sem2])
]).
% ORG, in X
rule(ne_np(s_form:F1,sem:
           E1^E2^[[organization,E1],Sem1,
		  [location,E2],[ne_tag,E2,Tag2],Sem2,
		  [in,E1,E2]]),[
        ne_np(s_form:F1,sem:E1^[[organization,E1]|Sem1]),
        comma(_),
        in(m_root:'in'),
        ne_np(s_form:F2,sem:E2^[[ne_tag,E2,Tag2]|Sem2])
]).

% PER in X
rule(ne_np(s_form:F1,sem:
           E1^E2^[[person,E1],Sem1,
		  [location,E2],[ne_tag,E2,Tag2],Sem2,
		  [in,E1,E2]]),[
        ne_np(s_form:F1,sem:E1^[[person,E1]|Sem1]),
        in(m_root:'in'),
        ne_np(s_form:F2,sem:E2^[[ne_tag,E2,Tag2]|Sem2])
]).
% PER, in X
rule(ne_np(s_form:F1,sem:
           E1^E2^[[person,E1],Sem1,
		  [location,E2],[ne_tag,E2,Tag2],Sem2,
		  [in,E1,E2]]),[
        ne_np(s_form:F1,sem:E1^[[person,E1]|Sem1]),
        comma(_),
        in(m_root:'in'),
        ne_np(s_form:F2,sem:E2^[[ne_tag,E2,Tag2]|Sem2])
]).

% DATE in X
rule(ne_np(s_form:F1,sem:
           E1^E2^[[date,E1],Sem1,
		  [location,E2],[ne_tag,E2,Tag2],Sem2,
		  [in,E1,E2]]),[
        ne_np(s_form:F1,sem:E1^[[date,E1]|Sem1]),
        in(m_root:'in'),
        ne_np(s_form:F2,sem:E2^[[ne_tag,E2,Tag2]|Sem2])
]).
% DATE, in X
rule(ne_np(s_form:F1,sem:
           E1^E2^[[date,E1],Sem1,
		  [location,E2],[ne_tag,E2,Tag2],Sem2,
		  [in,E1,E2]]),[
        ne_np(s_form:F1,sem:E1^[[date,E1]|Sem1]),
        comma(_),
        in(m_root:'in'),
        ne_np(s_form:F2,sem:E2^[[ne_tag,E2,Tag2]|Sem2])
]).

% X, in DATE
rule(ne_np(s_form:F1,sem:
           E1^E2^[[location,E1],[ne_tag,E1,Tag1],Sem1,
		  [date,E2],Sem2,
		  [in,E1,E2]]),[
        ne_np(s_form:F1,sem:E1^[[ne_tag,E1,Tag1]|Sem1]),
        comma(_),
        in(m_root:'in'),
        ne_np(s_form:F2,sem:E2^[[date,E2]|Sem2])
]).

% PER of X
rule(ne_np(s_form:F1,sem:
           E1^E2^[[person,E1],Sem1,
		  [location,E2],[ne_tag,E2,Tag2],Sem2,
		  [of,E1,E2]]),[
        ne_np(s_form:F1,sem:E1^[[person,E1]|Sem1]),
        in(m_root:'of'),
        ne_np(s_form:F2,sem:E2^[[ne_tag,E2,Tag2]|Sem2])
]).
% PER, of X
rule(ne_np(s_form:F1,sem:
           E1^E2^[[person,E1],Sem1,
		  [location,E2],[ne_tag,E2,Tag2],Sem2,
		  [of,E1,E2]]),[
        ne_np(s_form:F1,sem:E1^[[person,E1]|Sem1]),
        comma(_),
        in(m_root:'of'),
        ne_np(s_form:F2,sem:E2^[[ne_tag,E2,Tag2]|Sem2])
]).

% X on DATE
rule(ne_np(s_form:F1,sem:
           E1^E2^[[location,E1],[ne_tag,E1,Tag1],Sem1,
		  [date,E2],Sem2,
		  [on,E1,E2]]),[
        ne_np(s_form:F1,sem:E1^[[ne_tag,E1,Tag1]|Sem1]),
        in(m_root:'on'),
        ne_np(s_form:F2,sem:E2^[[date,E2]|Sem2])
]).
% X, on DATE
rule(ne_np(s_form:F1,sem:
           E1^E2^[[location,E1],[ne_tag,E1,Tag1],Sem1,
		  [date,E2],Sem2,
		  [on,E1,E2]]),[
        ne_np(s_form:F1,sem:E1^[[ne_tag,E1,Tag1]|Sem1]),
        comma(_),
        in(m_root:'on'),
        ne_np(s_form:F2,sem:E2^[[date,E2]|Sem2])
]).

% X to LOC
rule(ne_np(s_form:F1,sem:
           E1^E2^[[location,E1],[ne_tag,E1,Tag1],Sem1,
		  [location,E2],Sem2,
		  [to,E1,E2]]),[
        ne_np(s_form:F1,sem:E1^[[ne_tag,E1,Tag1]|Sem1]),
        in(m_root:'to'),
        ne_np(s_form:F2,sem:E2^[[location,E2]|Sem2])
]).
% X, to LOC
rule(ne_np(s_form:F1,sem:
           E1^E2^[[location,E1],[ne_tag,E1,Tag1],Sem1,
		  [location,E2],Sem2,
		  [to,E1,E2]]),[
        ne_np(s_form:F1,sem:E1^[[ne_tag,E1,Tag1]|Sem1]),
        comma(_),
        in(m_root:'to'),
        ne_np(s_form:F2,sem:E2^[[location,E2]|Sem2])
]).

% LOC to X
rule(ne_np(s_form:F1,sem:
           E1^E2^[[location,E1],Sem1,
		  [location,E2],[ne_tag,E2,Tag2],Sem2,
		  [to,E1,E2]]),[
        ne_np(s_form:F1,sem:E1^[[location,E1]|Sem1]),
        in(m_root:'to'),
        ne_np(s_form:F2,sem:E2^[[ne_tag,E2,Tag2]|Sem2])
]).
% LOC, to X
rule(ne_np(s_form:F1,sem:
           E1^E2^[[location,E1],Sem1,
		  [location,E2],[ne_tag,E2,Tag2],Sem2,
		  [to,E1,E2]]),[
        ne_np(s_form:F1,sem:E1^[[location,E1]|Sem1]),
        comma(_),
        in(m_root:'to'),
        ne_np(s_form:F2,sem:E2^[[ne_tag,E2,Tag2]|Sem2])
]).


% the X in LOC
rule(ne_np(s_form:F1,sem:
           E1^E2^[[organization,E1],[ne_tag,E1,Tag1],Sem1,[det,E1,the],
		  [location,E2],Sem2,
		  [in,E1,E2]]),[
        dt(m_root:'the'),
	ne_np(s_form:F1,sem:E1^[[ne_tag,E1,Tag1]|Sem1]),
        in(m_root:'in'),
        ne_np(s_form:F2,sem:E2^[[location,E2]|Sem2])
]).

% X of the ORG
rule(ne_np(s_form:F1,sem:
           E1^E2^[[person,E1],[ne_tag,E1,Tag1],Sem1,
		  [organization,E2],Sem2,[det,E2,the],
		  [of,E1,E2]]),[
	ne_np(s_form:F1,sem:E1^[[ne_tag,E1,Tag1]|Sem1]),
        in(m_root:'of'),
        dt(m_root:'the'),
        ne_np(s_form:F2,sem:E2^[[organization,E2]|Sem2])
]).
% X, of the ORG
rule(ne_np(s_form:F1,sem:
           E1^E2^[[person,E1],[ne_tag,E1,Tag1],Sem1,
		  [organization,E2],Sem2,[det,E2,the],
		  [of,E1,E2]]),[
	ne_np(s_form:F1,sem:E1^[[ne_tag,E1,Tag1]|Sem1]),
        comma(_),
	in(m_root:'of'),
        dt(m_root:'the'),
        ne_np(s_form:F2,sem:E2^[[organization,E2]|Sem2])
]).

% PER of the X
rule(ne_np(s_form:F1,sem:
           E1^E2^[[person,E1],Sem1,
		  [organization,E2],[ne_tag,E2,Tag2],Sem2,[det,E2,the],
		  [of,E1,E2]]),[
        ne_np(s_form:F1,sem:E1^[[person,E1]|Sem1]),
	in(m_root:'of'),
        dt(m_root:'the'),
        ne_np(s_form:F2,sem:E2^[[ne_tag,E2,Tag2]|Sem2])
]).
% PER, of the X
rule(ne_np(s_form:F1,sem:
           E1^E2^[[person,E1],Sem1,
		  [organization,E2],[ne_tag,E2,Tag2],Sem2,[det,E2,the],
		  [of,E1,E2]]),[
        ne_np(s_form:F1,sem:E1^[[person,E1]|Sem1]),
        comma(_),
	in(m_root:'of'),
        dt(m_root:'the'),
        ne_np(s_form:F2,sem:E2^[[ne_tag,E2,Tag2]|Sem2])
]).


% unclassified ne_np apposed with a location is probably a location.
rule(ne_np(s_form:[F1,', ',F2],
     sem:E1^E2^[[location,E1],[ne_tag,E1,E1Tag],NP1Sem,
		[location,E2],NP2Sem,[in,E1,E2]]),[
        ne_np(s_form:F1,sem:E1^[[ne_tag,E1,E1Tag]|NP1Sem]), % unclassified
	comma(s_form:','),
        ne_np(s_form:F2,sem:E2^[[location,E2]|NP2Sem])
]).

% unclassified ne_np apposed with a province is probably a city.
rule(ne_np(s_form:[F1,', ',F2],
     sem:E1^E2^[[location,E1],[city,E1],[ne_tag,E1,E1Tag],NP1Sem,
		[location,E2],[province,E2],NP2Sem,[in,E1,E2]]),[
        ne_np(s_form:F1,sem:E1^[[ne_tag,E1,E1Tag]|NP1Sem]), % unclassified
	comma(s_form:','),
        ne_np(s_form:F2,sem:E2^[[location,E2],[province,E2]|NP2Sem])
]).

% unclassified ne_np apposed with a country is probably a city.
rule(ne_np(s_form:[F1,', ',F2],
     sem:E1^E2^[[location,E1],[city,E1],[ne_tag,E1,E1Tag],NP1Sem,
		[location,E2],[country,E2],NP2Sem,[in,E1,E2]]),[
        ne_np(s_form:F1,sem:E1^[[ne_tag,E1,E1Tag]|NP1Sem]), % unclassified
	comma(s_form:','),
        ne_np(s_form:F2,sem:E2^[[location,E2],[country,E2]|NP2Sem])
]).


% unclassified ne_np-based is probably a location.
rule(ne_np(s_form:F1,
     sem:E1^[[location,E1],[ne_tag,E1,E1Tag],NP1Sem]),[
        ne_np(s_form:F1,sem:E1^[[ne_tag,E1,E1Tag]|NP1Sem]), % unclassified
	sym(s_form:'-'),
        v(s_form:'based')
]).


% reclassify 'Soviet Union' to be a country
rule(ne_np(edge:Edge,s_form:F,sem:
           E^[[location,E],[country,E],[name,E,['Soviet',' ','Union']],[ne_tag,E,Edge]]),[
        ne_np(s_form:F,sem:
	     E^[[organization,E],[name,E,['Soviet',' ','Union']]|_])
]).


% remove news source abbreviations as unclassified ne_nps in header
rule(ne_np(s_form:F,sem:E^[[ne_mark,E,no]]),[
     sym(s_form:'-',text:header),
     ne_np(s_form:F,sem:E1^[[ne_tag,E1,Tag]|_]),
     bottom(text:header)
]).

% remove news source abbreviation as an unclassified ne_np in header
rule(bnp(s_form:F),[
     sym(s_form:'-',text:header),
     ne_np(s_form:F,sem:E1^[[ne_tag,E1,Tag],[name,E1,'NYT']|_])
]).

% remove news source abbreviation as an unclassified ne_np in header
rule(bnp(s_form:F),[
     sym(s_form:'-',text:header),
     ne_np(s_form:F,sem:E1^[[organization,E1],[name,E1,'NEWSWEEK']|_])
]).

% remove NYT as an unclassified ne_np in the trailer
rule(bnp(s_form:F),[
     top(text:header),
     ne_np(s_form:F,sem:E1^[[ne_tag,E1,Tag],[name,E1,'NYT']|_]),
     sym(s_form:'-',text:header)
]).

% remove BC as an unclassified ne_np in the header
rule(bnp(s_form:F),[
     top(text:header),
     ne_np(s_form:F,sem:E1^[[ne_tag,E1,Tag],[name,E1,'BC']|_]),
     sym(s_form:'-',text:header)
]).



% put together sequences of ne_nps (preferably from header only)
rule(names_np(s_form:[F1,' ',F2]),[
        ne_np(s_form:F1,text:header),
        names_np(s_form:F2)
    ]).

rule(names_np(s_form:F),[
        ne_np(s_form:F,text:header)
    ]).

% match bylines in header
rule(ne_np(sem:E1^E2^E3^[[person,E1],[name,E1,F1],
			 [organization,E2],[company,E2],[name,E2,F2],
			 [ne_tag,E1,Tag1],[ne_tag,E2,Tag2],
			 [realisation,E1,Tag1],[realisation,E2,Tag2],
			 [of,E1,E2],[date,E3]|DateSem]),[
	in(m_root:'by',text:header),
        names_np(edge:Tag1,s_form:F1),
        n(s_form:'c'),
        period(_),
        ne_np(sem:E3^[[date,E3]|DateSem]),
        dt(m_root:'the'),
        names_np(edge:Tag2,s_form:F2),
        bottom(_)
]).

rule(ne_np(sem:E1^E2^E3^[[person,E1],[name,E1,F1],
			 [organization,E2],[company,E2],[name,E2,F2],
			 [ne_tag,E1,Tag1],[ne_tag,E2,Tag2],
			 [realisation,E1,Tag1],[realisation,E2,Tag2],
			 [of,E1,E2],[date,E3]|DateSem]),[
	in(m_root:'by',text:header),
        names_np(edge:Tag1,s_form:F1),
        n(s_form:'c'),
        period(_),
        ne_np(sem:E3^[[date,E3]|DateSem]),
        names_np(edge:Tag2,s_form:F2),
        bottom(_)
]).

rule(ne_np(sem:E1^E2^E3^E4^[[person,E1],[name,E1,F1],
			    [person,E4],[name,E4,F4],
			    [organization,E2],[company,E2],[name,E2,F2],
			    [ne_tag,E1,Tag1],[ne_tag,E2,Tag2],[ne_tag,E4,Tag4],
			    [realisation,E1,Tag1],[realisation,E2,Tag2],[realisation,E4,Tag4],
			    [of,E1,E2],[of,E4,E2],[date,E3]|DateSem]),[
	in(m_root:'by',text:header),
        names_np(edge:Tag1,s_form:F1),
        cc(_),
        names_np(edge:Tag4,s_form:F4),
        n(s_form:'c'),
        period(_),
        ne_np(sem:E3^[[date,E3]|DateSem]),
        names_np(edge:Tag2,s_form:F2),
        bottom(_)
]).

rule(ne_np(sem:E1^E2^E3^E4^[[person,E1],[name,E1,F1],
			    [person,E4],[name,E4,F4],
			    [organization,E2],[company,E2],[name,E2,F2],
			    [ne_tag,E1,Tag1],[ne_tag,E2,Tag2],[ne_tag,E4,Tag4],
			    [realisation,E1,Tag1],[realisation,E2,Tag2],[realisation,E4,Tag4],
			    [of,E1,E2],[of,E4,E2],[date,E3]|DateSem]),[
	in(m_root:'by',text:header),
        names_np(edge:Tag1,s_form:F1),
        cc(_),
        names_np(edge:Tag4,s_form:F4),
        n(s_form:'c'),
        period(_),
        ne_np(sem:E3^[[date,E3]|DateSem]),
        dt(m_root:'the'),
        names_np(edge:Tag2,s_form:F2),
        bottom(_)
]).

% cases where byline precedes headline - try not to merge with headline
rule(ne_np(sem:E1^E2^E3^[[person,E1],[name,E1,F1],
			 [organization,E2],[company,E2],[name,E2,F2],
			 [ne_tag,E1,Tag1],[ne_tag,E2,Tag2],
			 [realisation,E1,Tag1],[realisation,E2,Tag2],
			 [of,E1,E2],[date,E3]|DateSem]),[
	in(m_root:'by',text:header),
        names_np(edge:Tag1,s_form:F1),
        n(s_form:'c'),
        period(_),
        ne_np(sem:E3^[[date,E3]|DateSem]),
        dt(m_root:'the'),
        ne_np(edge:Tag2,s_form:F2)
]).

rule(ne_np(sem:E1^E2^E3^[[person,E1],[name,E1,[F1,' ',FA]],
			 [organization,E2],[company,E2],[name,E2,F2],
			 [ne_tag,E1,Tag1],[ne_tag,E2,Tag2],
			 [realisation,E1,Tag1],[realisation,E2,Tag2],
			 [of,E1,E2],[date,E3]|DateSem]),[
	in(m_root:'by',text:header),
        ne_np(edge:Tag1,s_form:F1),
        jj(s_form:FA),
        n(s_form:'c'),
        period(_),
        ne_np(sem:E3^[[date,E3]|DateSem]),
        dt(m_root:'the'),
        ne_np(edge:Tag2,s_form:F2)
]).

rule(ne_np(sem:E1^E2^E3^[[person,E1],[name,E1,F1],
			 [organization,E2],[company,E2],[name,E2,F2],
			 [ne_tag,E1,Tag1],[ne_tag,E2,Tag2],
			 [realisation,E1,Tag1],[realisation,E2,Tag2],
			 [of,E1,E2],[date,E3]|DateSem]),[
	in(m_root:'by',text:header),
        names_np(edge:Tag1,s_form:F1),
        n(s_form:'c'),
        period(_),
        ne_np(sem:E3^[[date,E3]|DateSem]),
        ne_np(edge:Tag2,s_form:F2)
]).

rule(ne_np(sem:E1^E2^E3^[[person,E1],[name,E1,[F1,' ',FA]],
			 [organization,E2],[company,E2],[name,E2,F2],
			 [ne_tag,E1,Tag1],[ne_tag,E2,Tag2],
			 [realisation,E1,Tag1],[realisation,E2,Tag2],
			 [of,E1,E2],[date,E3]|DateSem]),[
	in(m_root:'by',text:header),
        ne_np(edge:Tag1,s_form:F1),
        jj(s_form:FA),
        n(s_form:'c'),
        period(_),
        ne_np(sem:E3^[[date,E3]|DateSem]),
        ne_np(edge:Tag2,s_form:F2)
]).

rule(ne_np(sem:E1^E2^E3^E4^[[person,E1],[name,E1,F1],
			    [person,E4],[name,E4,F4],
			    [organization,E2],[company,E2],[name,E2,F2],
			    [ne_tag,E1,Tag1],[ne_tag,E2,Tag2],[ne_tag,E4,Tag4],
			    [realisation,E1,Tag1],[realisation,E2,Tag2],[realisation,E4,Tag4],
			    [of,E1,E2],[of,E4,E2],[date,E3]|DateSem]),[
	in(m_root:'by',text:header),
        names_np(edge:Tag1,s_form:F1),
        cc(_),
        names_np(edge:Tag4,s_form:F4),
        n(s_form:'c'),
        period(_),
        ne_np(sem:E3^[[date,E3]|DateSem]),
        ne_np(edge:Tag2,s_form:F2)
]).

rule(ne_np(sem:E1^E2^E3^E4^[[person,E1],[name,E1,F1],
			    [person,E4],[name,E4,F4],
			    [organization,E2],[company,E2],[name,E2,F2],
			    [ne_tag,E1,Tag1],[ne_tag,E2,Tag2],[ne_tag,E4,Tag4],
			    [realisation,E1,Tag1],[realisation,E2,Tag2],[realisation,E4,Tag4],
			    [of,E1,E2],[of,E4,E2],[date,E3]|DateSem]),[
	in(m_root:'by',text:header),
        names_np(edge:Tag1,s_form:F1),
        cc(_),
        names_np(edge:Tag4,s_form:F4),
        n(s_form:'c'),
        period(_),
        ne_np(sem:E3^[[date,E3]|DateSem]),
        dt(m_root:'the'),
        ne_np(edge:Tag2,s_form:F2)
]).


rule(ne_np(sem:E1^E2^E3^E4^[[person,E1],[name,E1,F1],
			    [person,E4],[name,E4,F4],
			    [organization,E2],[company,E2],[name,E2,F2],
			    [ne_tag,E1,Tag1],[ne_tag,E4,Tag4],[ne_tag,E2,Tag2],
			    [realisation,E1,Tag1],[realisation,E4,Tag4],[realisation,E2,Tag2],
			    [of,E1,E2],[of,E4,E2],[date,E3]|DateSem]),[
        n(s_form:'c'),
        period(_),
        ne_np(sem:E3^[[date,E3]|DateSem]),
        ne_np(edge:Tag2,s_form:F2),
	in(m_root:'by',text:header),
        names_np(edge:Tag1,s_form:F1),
        cc(_),
        names_np(edge:Tag4,s_form:F4)
]).


% flag document source locations
rule(bnp(s_form:F,sem:E^[[location,E],Sem,[article_source,E]]),[
       top(text:body), % first line only
       ne_np(s_form:F,sem:E^[[location,E]|Sem]),
       sym(s_form:'&MD;')
]).

%% NE_NP(person) -> NE_NP(person,uncertain) N
% catch mistagged proper nouns in header
rule(ne_np(edge:Edge,s_form:[F1,' ',F2],
       sem:E^[[person,E],[gender,E,G],[name,E,[F1,' ',F2]],[ne_tag,E,Edge]]),[
       ne_np(s_form:F1,text:header,
	     sem:E^[[person,E],[[gender,E,G],[uncertain,E,person]]|_]),
        n(s_form:F2,text:header)
]).

% otherwise remove altogether from header
rule(ne_np(edge:Edge,s_form:[F1,' ',F2],
       sem:E^[[ne_tag,E,Edge],[name,E,F1]]),[
       ne_np(s_form:F1,text:header,
	     sem:E^[[person,E],[[gender,E,G],[uncertain,E,person]]|_])
]).


%title-military unknown yields military
rule(ne_np(edge:Edge,s_form:[F1,' ',F2],
       sem:E2^[[military,E2],[title,E2,T1],[ne_tag,E2,X]|PS2]),[
       ne_np(s_form:F1, sem:E1^[[title,E1,T1],[military,E1]]),
       ne_np(s_form:F2, sem:E2^[[ne_tag,E2,X]|PS2])
]).
 
%title-civilian unknown yields civilian
rule(ne_np(edge:Edge,s_form:[F1,' ',F2],
       sem:E2^[[civilian,E2],[title,E2,T1],[ne_tag,E2,X]|PS2]),[
       ne_np(s_form:F1, sem:E1^[[title,E1,T1],[civilian,E1]]),
       ne_np(s_form:F2, sem:E2^[[ne_tag,E2,X]|PS2])
]).
 

% Saudi Arabian DC-9
rule(ne_np(edge:Edge,s_form:[F1,' ',F2],number:N,
       sem:E2^E1^[[aircraft,E2],[organization,E1],[qual,E2,E1],
                  [descriptor,E2,Edge],MSem1,MSem2]),[
        ne_np(s_form:F1,sem:E1^[[organization,E1]|MSem1]),
	ne_np(s_form:F2,number:N,sem:E2^[[aircraft,E2]|MSem2])
]).

