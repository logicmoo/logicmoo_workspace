%**********************************************************************
%
%  Rules for Monetary expressions (Money and Percent)
%
%  money_np    : money NP
%  m_unit_np   : currency unit ($,dollars,...)
%  m_unit      : money unit (million, billion, trillion)
%  percent_np  : percetage NP
%  cd_np       : cardinal number in monetary expressions
%
%**********************************************************************

%
% Best Parse Categories
:- multifile best_parse_cats/1, rule/2.
:- dynamic best_parse_cats/1, rule/2.

best_parse_cats([ne_np,cd_np]).


%
%   NP Rules
%
%% NE_NP --> MONEY_NP
rule(ne_np(edge:Edge,s_form:F,
	   sem:E^[[money,E],[name,E,F],Sem,[ne_tag,E,Edge]]),
	[money_np(s_form:F,sem:Sem)]).

%
% MONEY_NP rules
%
%% MONEY_NP --> CD_NP JJ JJ M_UNIT_NP
rule(money_np(s_form:[F1,' ',F2,' ',F3,' ',F4]),[
        cd_np(s_form:F1),
        jj(s_form:F2),
        jj(s_form:F3),
	m_unit_np(s_form:F4)
        ]).

%% MONEY_NP --> CD_NP JJ M_UNIT_NP
rule(money_np(s_form:[F1,' ',F2,' ',F3]),[
        cd_np(s_form:F1),
        jj(s_form:F2),
	m_unit_np(s_form:F3)
        ]).

%% MONEY_NP --> CD_NP NE_NP(location) M_UNIT_NP
rule(money_np(sem:X^[[location,X]|Sem],s_form:[F1,' ',F2,' ',F3]),[
        cd_np(s_form:F1),
        ne_np(s_form:F2,sem:X^[[location,X]|Sem]),
	m_unit_np(s_form:F3)
        ]).

%% MONEY_NP --> NE_NP(location) M_UNIT_NP CD_NP
rule(money_np(sem:X^[[location,X]|Sem],s_form:[F1,' ',F2,' ',F3]),[
        ne_np(s_form:F1,sem:X^[[location,X]|Sem]),
	m_unit_np(s_form:F2),
        cd_np(s_form:F3)
        ]).


%% MONEY_NP --> CD_NP M_UNIT_NP
rule(money_np(s_form:[F1,' ',F2]),[
        cd_np(s_form:F1),
	m_unit_np(s_form:F2)
        ]).

%% MONEY_NP -->  M_UNIT_NP CD_NP
rule(money_np(s_form:[F1,F2]),[
        m_unit_np(s_form:F1),
        cd_np(s_form:F2)
        ]).


%% M_UNIT_NP --> SYM($)
rule(m_unit_np(s_form:'$'),[
        sym(s_form:'$')
        ]).

%% M_UNIT_NP --> PN SYM($)
rule(m_unit_np(s_form:[F1,'$']),[
        pn(s_form:F1),
        sym(s_form:'$')
        ]).

%% M_UNIT_NP --> SYM(#)
rule(m_unit_np(s_form:'#'),[
        sym(s_form:'#')
        ]).

%% M_UNIT_NP --> LIST_NP(currency_unit)
rule(m_unit_np(s_form:F),[
        list_np(s_form:F,ne_tag:currency_unit)
        ]).

%
%% M_UNIT --> CD(million)
rule(m_unit(s_form:'million'),[
        cd(s_form:'million')
        ]).

%% M_UNIT --> CD(billion)
rule(m_unit(s_form:'billion'),[
        cd(s_form:'billion')
        ]).

%% M_UNIT --> CD(trillion)
rule(m_unit(s_form:'trillion'),[
        cd(s_form:'trillion')
        ]).

%% M_UNIT --> CD(Million)
rule(m_unit(s_form:'Million'),[
        cd(s_form:'Million')
        ]).

%% M_UNIT --> CD(Billion)
rule(m_unit(s_form:'Billion'),[
        cd(s_form:'Billion')
        ]).

%% M_UNIT --> CD(Trillion)
rule(m_unit(s_form:'Trillion'),[
        cd(s_form:'Trillion')
        ]).

%
% PERCENT rules
%% NE_NP --> PERCENT_NP
rule(ne_np(edge:Edge,s_form:F,head:F,
	sem:E^[[percent,E],[name,E,F],[ne_tag,E,Edge]]),
	[percent_np(s_form:F)]).

%% PERCENT_NP --> CD_NP N(percent)
rule(percent_np(s_form:[F1,'%']),[
        cd_np(s_form:F1),
        n(s_form:'%')
        ]).

%% PERCENT_NP --> CD_NP N(percent)
rule(percent_np(s_form:[F1,'percent']),[
        cd_np(s_form:F1),
        n(s_form:'percent')
        ]).

%% PERCENT_NP --> CD_NP N(percentage) N(point)
rule(percent_np(s_form:[F1,' percentage ',S_Form]),[
        cd_np(s_form:F1),
        n(s_form:'percentage'),
	n(m_root:'point',s_form:S_Form)
        ]).


%% NE_NP --> CD_NP
% Handled in npcore
%rule(ne_np(s_form:F,sem:E^[[count,E,F],Sem]),
%	[cd_np(s_form:F,sem:Sem)]).

%
% CD_NP rules
%% CD_NP --> CD_NP M_UNIT
rule(cd_np(s_form:[F1,' ',F2]),[
        cd_np(s_form:F1),
        m_unit(s_form:F2)
        ]).

%% CD_NP --> CD COMMA CD
rule(cd_np(s_form:[F1,F2,F3]),[
        cd(s_form:F1),
        comma(s_form:F2),
        cd(s_form:F3)
        ]).

%% CD_NP --> CD PERIOD CD
rule(cd_np(s_form:[F1,F2,F3]),[
        cd(s_form:F1),
        period(s_form:F2),
        cd(s_form:F3)
        ]).

%% CD_NP --> CD
rule(cd_np(s_form:F),[
        cd(s_form:F)
        ]).

%% CD_NP --> CD N('/') CD
rule(cd_np(s_form:[F1,'/',F2]),[
        cd(s_form:F1),
	n(s_form:'/'),
	cd(s_form:F2)
        ]).

%% CD_NP --> CD CD N('/') CD
rule(cd_np(s_form:[F,' ',F1,'/',F2]),[
        cd(s_form:F),
        cd(s_form:F1),
	n(s_form:'/'),
	cd(s_form:F2)
        ]).

