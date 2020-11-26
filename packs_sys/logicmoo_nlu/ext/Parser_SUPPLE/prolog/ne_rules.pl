
/* ------------------------------------------------------------------------
 > FILENAME:	ne_rules
 > PURPOSE:	Grammar Rules for MUC6 Named Entity
 > AUTHORS:	Takahiro Wakao, Kevin Humphreys
 > NOTES:	grammar rule format A -> B C is translated into :
 >              rule(a(features), [b(features), c(features)]).
 ------------------------------------------------------------------------ */

cvsid_ne_rules("$Id: ne_rules.pl 7085 2005-12-05 16:32:03Z ian_roberts $").



%
% Grammar ID Tag
grammar(named_entity).

%
%   NP Rules
%
%% NP(Basic) --> NAMES_NP
% ambiguous name
%rule(basic_np(edge:Edge,head:F,
%	sem:E^[[name,E,F],[realisation,E,Edge]]),
%	[names_np(s_form:F)]).

%% NP(Basic) --> LIST_NP(others)
% non-markable proper names
rule(basic_np(edge:Edge,head:F,
	sem:E^[[name,E,F],[realisation,E,Edge],[ne_mark,E,no]]),
	[list_np(s_form:F,ne_tag:others)]).

%% NP(Basic) --> ORGAN_NAMES_NP LIST_NP(others)
rule(basic_np(edge:Edge,head:F2,
	sem:E^[[name,E,[F1,' ',F2]],[realisation,E,Edge],[ne_mark,E,no]]),[
        organ_names_np(s_form:F1),
        list_np(s_form:F2,ne_tag:others)]).



%**********************************************************************
%   Rules for Organization names
%
%   organ_np  : organization NP
%   cdg_np    : company designator NP
%   names_np  : ambiguous proper names
%   names_np2 : ambiguous proper names with comma
%   names_np3 : ambiguous proper names with 'and'
%   organ_names_np  : names consisted only of location, person,
%                     names_np3
%   organ_names_np2 : names consisted of various types of key words
%   organ_punc_np   : names with puctuation
%
%**********************************************************************


%% NP(Basic) --> ORGAN_NP
rule(basic_np(edge:Edge,head:F,
       sem:E^[[name,E,F],[organization,E],NSem,[realisation,E,Edge],[ne_tag,E,Edge]]),
       [organ_np(s_form:F,sem:E^NSem)]).

% ORGAN_NP rules
%
%% ORGAN_NP --> PN PERIOD PN PERIOD ORGAN_NAMES_NP CDG_NP
rule(organ_punc_np(s_form:[F1,'. ',F2,'. ',F3, ' ',F4],sem:E^[company,E]),[
        pn(s_form:F1),
	period(s_form:'.'),
        pn(s_form:F2),
	period(s_form:'.'),
        organ_names_np(s_form:F3),
	cdg_np(s_form:F4)
        ]).

%% ORGAN_NP --> PN PERIOD PN PERIOD PN PERIOD CDG_NP
rule(organ_punc_np(s_form:[F1,'. ',F2,'. ',F3, '. ',F4],sem:E^[company,E]),[
        pn(s_form:F1),
	period(s_form:'.'),
        pn(s_form:F2),
	period(s_form:'.'),
        pn(s_form:F3),
	period(s_form:','),
	cdg_np(s_form:F4)
        ]).

%% ORGAN_NP --> PERSON_PERIOD_NP CDG_NP
rule(organ_np(s_form:[F1,' ',F2],sem:E^[company,E]),[
        person_period_np(s_form:F1),
        cdg_np(s_form:F2)
        ]).

%% ORGAN_NP --> ORGAN_NAMES_NP CD CDG_NP
rule(organ_np(s_form:[F1,' ',F2,' ',F3],sem:E^[company,E]),[
        organ_names_np(s_form:F1),
	cd(s_form:F2),
        cdg_np(s_form:F3)
        ]).

%% ORGAN_NP --> ORGAN_NAMES_NP CDG_NP
rule(organ_np(s_form:[F1,' ',F2],sem:E^[company,E]),[
        organ_names_np(s_form:F1),
        cdg_np(s_form:F2)
        ]).

%% ORGAN_NP --> LIST_NP(organ) TAGGED_LOCATION_NP
rule(organ_np(s_form:[F1,' ',F2],sem:E^[T,E]),[
        list_np(s_form:F1,ne_tag:organization,ne_type:T),
        tagged_location_np(s_form:F2)
        ]).

%% ORGAN_NP --> TAGGED_LOCATION_NP LIST_NP(organ)
rule(organ_np(s_form:[F1,' ',F2],sem:E^[T,E]),[
        tagged_location_np(s_form:F1),
        list_np(s_form:F2,ne_tag:organization,ne_type:T)
        ]).

%% ORGAN_NP --> ORGAN_PUNC_NP CDG_NP
rule(organ_np(s_form:[F1,' ',F2],sem:E^[OPSem]),[
        organ_punc_np(s_form:F1,sem:E^OPSem),
        cdg_np(s_form:F2)
        ]).

%% ORGAN_NP --> ORGAN_PUNC_NP
rule(organ_np(s_form:[F1],sem:E^[OPSem]),[
        organ_punc_np(s_form:F1,sem:E^OPSem)
        ]).

% rules with '&'
%% ORGAN_NP --> NAMES_NP2 '&' ORGAN_NAMES_NP CDG_NP
rule(organ_np(s_form:[F1,' & ',F2,' ',F3],sem:E^[company,E]),[
        names_np2(s_form:F1),
        cc(s_form:'&'),
        organ_names_np(s_form:F2),
        cdg_np(s_form:F3)
        ]).

%% ORGAN_NP --> NAMES_NP2 '&' ORGAN_NAMES_NP
rule(organ_np(s_form:[F1,' & ',F2],sem:E^[company,E]),[
        names_np2(s_form:F1),
        cc(s_form:'&'),
        organ_names_np(s_form:F2)
        ]).

%% ORGAN_NP --> ORGAN_NAMES_NP '&' ORGAN_NAMES_NP CDG_NP
rule(organ_np(s_form:[F1,' & ',F2,' ',F3],sem:E^[company,E]),[
        organ_names_np(s_form:F1),
        cc(s_form:'&'),
        organ_names_np(s_form:F2),
        cdg_np(s_form:F3)
        ]).

%% ORGAN_NP --> ORGAN_NAMES_NP '&' ORGAN_NAMES_NP
rule(organ_np(s_form:[F1,' & ',F2],sem:E^[company,E]),[
        organ_names_np(s_form:F1),
        cc(s_form:'&'),
        organ_names_np(s_form:F2)
        ]).

%% ORGAN_NP --> NAMES_NP2 CDG_NP
rule(organ_np(s_form:[F1,' ',F2],sem:E^[company,E]),[
        names_np2(s_form:F1),
        cdg_np(s_form:F2)
        ]).

% rules for names which contain 'of' or 'for'
%% ORGAN_NP --> NAMES_NP2 CC(and) PN 'of' TAGGED_LOCATION_NP
rule(organ_np(s_form:[F1,' and ',F2,' of ',F3],sem:E^[company,E]),[
        names_np2(s_form:F1),
	cc(s_form:'and'),
	pn(s_form:F2),
        in(s_form:'of'),
        tagged_location_np(s_form:F3)
        ]).

%% ORGAN_NP --> ORGAN_NAMES_NP 'of' ORGAN_NAMES_NP CDG_NP
rule(organ_np(s_form:[F1,' of ',F2, ' ',F3],sem:E^[]),[
        organ_names_np(s_form:F1),
        in(s_form:'of'),
        organ_names_np(s_form:F2),
	cdg_np(s_form:F3)
        ]).

%% ORGAN_NP --> ORGAN_NAMES_NP2 'of' TAGGED_LOCATION_NP
rule(organ_np(s_form:[F1,' of ',F2],sem:E^[]),[
        organ_names_np2(s_form:F1),
        in(s_form:'of'),
        tagged_location_np(s_form:F2)
        ]).

%% ORGAN_NP --> LIST_NP(location) PN
% may be too general
rule(organ_np(s_form:[F1,' ',F2],sem:E^[]),[
        list_np(s_form:F1,ne_tag:location),
        pn(s_form:F2)
        ]).

% extensions of LIST_NP(organ)
%% ORGAN_NP --> NAMES_NP LIST_NP(organ)
rule(organ_np(s_form:[F1,' ',F2],sem:E^[T,E]),[
        names_np(s_form:F1),
        list_np(s_form:F2,ne_tag:organization,ne_type:T)
        ]).

% ORGAN_NP --> LIST_NP(organ) NAMES_NP  
% commented out for the time being
%rule(organ_np(s_form:[F1,' ',F2],sem:E^[T,E]),[
%        list_np(s_form:F1,ne_tag:organization,ne_type:T),
%        names_np(s_form:F2)
%        ]).

% Base case (list lookup organization)
%% ORGAN_NP --> LIST_NP(organ)
rule(organ_np(s_form:[F1],sem:E^[T,E]),[
        list_np(s_form:F1,ne_tag:organization,ne_type:T)
        ]).

% rules for governmental entities
%% ORGAN_NP --> ORGAN_NAMES_NP LIST_NP(govern_key)
rule(organ_np(s_form:[F1,' ',F2],sem:E^[[government,E],[R2,E]]),[
	organ_names_np(s_form:F1),
        list_np(s_form:F2,m_root:R2,ne_tag:govern_key)
         ]).

%% ORGAN_NP --> LIST_NP(Government) ORGAN_NAMES_NP LIST_NP(ORG_BASE)
rule(organ_np(s_form:[F1,' ',F2,' ',F3],sem:E^[[government,E],[R2,E]]),[
        list_np(s_form:F1,ne_tag:organization,ne_type:government),
	organ_names_np(s_form:F2),
        list_np(s_form:F3,m_root:R2,ne_tag:org_base)
         ]).

%% ORGAN_NP --> NAMES_NP(Federal) LIST_NP(ORG_BASE)
rule(organ_np(s_form:['Federal',F1,' ',F2],sem:E^[[government,E],[R2,E]]),[
	names_np(s_form:['Federal'|F1]),
        list_np(s_form:F2,m_root:R2,ne_tag:org_base)
         ]).

%% ORGAN_NP --> LIST_NP(National) NAMES_NP LIST_NP(ORG_BASE)
rule(organ_np(s_form:['National ',F1,' ',F2],sem:E^[[government,E],[R2,E]]),[
	list_np(s_form:'National',ne_tag:org_key),
	names_np(s_form:F1),
        list_np(s_form:F2,m_root:R2,ne_tag:org_base)
         ]).

% org_key rules
% org_key is a typical word in company names, e.g. 'Industires'
% which comes either at the beginning or at the end of the name
% and is not meaningful enough to be used as the word to co-refer
% (cf. org_base words are meant to be meaningful enough)
%% ORGAN_NP --> ORGAN_NAMES_NP LIST_NP(org_key) IN(of) TAGGED_LOCATION_NP
rule(organ_np(s_form:[F1,' ',F2,' of ',F3],sem:E^[company,E]),[
        organ_names_np(s_form:F1),
        list_np(s_form:F2,ne_tag:org_key),
        in(s_form:'of'),
        tagged_location_np(s_form:F3)
         ]).

%% ORGAN_NP --> LIST_NP(org_key) IN  ORGAN_NAMES_NP
rule(organ_np(s_form:[F,' ',F1,' ',F2],sem:E^[company,E]),[
        list_np(s_form:F,ne_tag:org_key),
        in(s_form:F1),
        organ_names_np(s_form:F2)
         ]).

%% ORGAN_NP --> ORGAN_NAMES_NP LIST_NP(org_key) TAGGED_LOCATION_NP
rule(organ_np(s_form:[F1,' ',F2,' ',F3],sem:E^[company,E]),[
        organ_names_np(s_form:F1),
        list_np(s_form:F2,ne_tag:org_key),
	tagged_location_np(s_form:F3)
         ]).

%% ORGAN_NP --> NAMES_NP2 CC(and) PN LIST_NP(org_key)
rule(organ_np(s_form:[F1,' and ',F2,' ',F3],sem:E^[company,E]),[
        names_np2(s_form:F1),
	cc(s_form:'and'),
	pn(s_form:F2),
        list_np(s_form:F3,ne_tag:org_key)
         ]).

%% ORGAN_NP --> ORGAN_NAMES_NP LIST_NP(org_key,airline)
rule(organ_np(s_form:[F1,' ',F2],sem:E^[[company,E],[airline,E]]),[
        organ_names_np(s_form:F1),
        list_np(s_form:F2,ne_tag:org_key,ne_type:airline)
         ]).

%% ORGAN_NP --> LIST_NP(org_key,airline) NAMES_NP
rule(organ_np(s_form:[F1,' ',F2],sem:E^[[company,E],[airline,E]]),[
        list_np(s_form:F1,ne_tag:org_key,ne_type:airline),
	names_np(s_form:F2)
         ]).

%% ORGAN_NP --> LIST_NP(org_key,airline) TAGGED_LOCATION_NP
rule(organ_np(s_form:[F1,' ',F2],sem:E^[[company,E],[airline,E]]),[
        list_np(s_form:F1,ne_tag:org_key,ne_type:airline),
        tagged_location_np(s_form:F2)
         ]).

%% ORGAN_NP --> ORGAN_NAMES_NP LIST_NP(org_key)
rule(organ_np(s_form:[F1,' ',F2],sem:E^[company,E]),[
        organ_names_np(s_form:F1),
        list_np(s_form:F2,ne_tag:org_key,ne_type:org_key)
         ]).

%% ORGAN_NP --> LIST_NP(org_key) NAMES_NP
rule(organ_np(s_form:[F1,' ',F2],sem:E^[company,E]),[
        list_np(s_form:F1,ne_tag:org_key,ne_type:org_key),
	names_np(s_form:F2)
         ]).

%% ORGAN_NP --> LIST_NP(org_key) TAGGED_LOCATION_NP
rule(organ_np(s_form:[F1,' ',F2],sem:E^[company,E]),[
        list_np(s_form:F1,ne_tag:org_key,ne_type:org_key),
        tagged_location_np(s_form:F2)
         ]).

% org_base is also a key word typically comes before
% 'of' or 'for' and the head of the name.
% e.g. Association
%

%% ORGAN_NP --> ORGAN_NAMES_NP IN(of) ORGAN_NAMES_NP LIST_NP(org_base)
rule(organ_np(s_form:[F1,' of ',F2,' ',F3],sem:E^[R3,E]),[
       organ_names_np(s_form:F1),
       in(s_form:'of'),
       organ_names_np(s_form:F2),
       list_np(s_form:F3,m_root:R3,ne_tag:org_base)
        ]).

%% ORGAN_NP --> ORGAN_NAMES_NP CC(and) ORGAN_NAMES_NP LIST_NP(org_base)
rule(organ_np(s_form:[F1,' and ',F2,' ',F3],sem:E^[R3,E]),[
       organ_names_np(s_form:F1),
       cc(s_form:'and'),
       organ_names_np(s_form:F2),
       list_np(s_form:F3,m_root:R3,ne_tag:org_base)
        ]).

%% ORGAN_NP --> ORGAN_NAMES_NP LIST_NP(org_base) IN(of) JJ ORGAN_NAMES_NP
rule(organ_np(s_form:[F,' ',F1,' of ',F2,' ',F3],sem:E^[R1,E]),[
       organ_names_np(s_form:F),
       list_np(s_form:F1,m_root:R1,ne_tag:org_base),
       in(s_form:'of'),
       jj(s_form:F2),
       organ_names_np(s_form:F3)
      ]).

%% ORGAN_NP --> ORGAN_NAMES_NP LIST_NP(org_base) IN(of) ORGAN_NAMES_NP
rule(organ_np(s_form:[F,' ',F1,' of ',F2],sem:E^[R1,E]),[
       organ_names_np(s_form:F),
       list_np(s_form:F1,m_root:R1,ne_tag:org_base),
       in(s_form:'of'),
       organ_names_np(s_form:F2)
      ]).

%% ORGAN_NP --> ORGAN_NAMES_NP LIST_NP(org_base) IN(on) ORGAN_NAMES_NP
rule(organ_np(s_form:[F,' ',F1,' on ',F2],sem:E^[R1,E]),[
       organ_names_np(s_form:F),
       list_np(s_form:F1,m_root:R1,ne_tag:org_base),
       in(s_form:'on'),
       organ_names_np(s_form:F2)
      ]).

%% ORGAN_NP -->  LIST_NP(org_base) IN JJ ORGAN_NAMES_NP
rule(organ_np(s_form:[F,' ',F1,' ',F2,' ',F3],sem:E^[R1,E]),[
       list_np(s_form:F,m_root:R1,ne_tag:org_base),
       in(s_form:F1),
       jj(s_form:F2),
       organ_names_np(s_form:F3)
      ]).

%% ORGAN_NP -->  LIST_NP(org_base) IN ORGAN_NAMES_NP
rule(organ_np(s_form:[F,' ',F1,' ',F2],sem:E^[R1,E]),[
       list_np(s_form:F,m_root:R1,ne_tag:org_base),
       in(s_form:F1),
       organ_names_np(s_form:F2)
      ]).

%% ORGAN_NP --> PERSON_NP LIST_NP(org_base)
rule(organ_np(s_form:[F1,' ',F2],sem:E^[[company,E],[R2,E]]),[
        person_np(s_form:F1),
        list_np(s_form:F2,m_root:R2,ne_tag:org_base)
	]).

%% ORGAN_NP --> ORGAN_NAMES_NP LIST_NP(org_base)
rule(organ_np(s_form:[F1,' ',F2],sem:E^[R2,E]),[
        organ_names_np(s_form:F1),
        list_np(s_form:F2,m_root:R2,ne_tag:org_base)
        ]).

%
% CDG rules
%% CDG_NP --> '&' LIST_NP(cdg) PERIOD
rule(cdg_np(s_form:['& ',F2,F3]),[
        cc(s_form:'&'),
        list_np(s_form:F2,ne_tag:cdg),
        period(s_form:F3)
        ]).

%% CDG_NP --> 'and' LIST_NP(cdg) PERIOD
rule(cdg_np(s_form:['and ',F2,F3]),[
        cc(s_form:'and'),
        list_np(s_form:F2,ne_tag:cdg),
        period(s_form:F3)
        ]).

%% CDG_NP --> '&' LIST_NP(cdg)
rule(cdg_np(s_form:['& ',F2]),[
        cc(s_form:'&'),
        list_np(s_form:F2,ne_tag:cdg)
        ]).

%% CDG_NP --> 'and' LIST_NP(cdg)
rule(cdg_np(s_form:['and ',F2]),[
        cc(s_form:'and'),
        list_np(s_form:F2,ne_tag:cdg)
        ]).

%% CDG_NP --> LIST_NP(cdg) PERIOD
rule(cdg_np(s_form:[F1,F2]),[
        list_np(s_form:F1,ne_tag:cdg),
        period(s_form:F2)
        ]).

%% CDG_NP --> N LIST_NP(cdg)
rule(cdg_np(s_form:[F1,' ',F2]),[
        n(s_form:F1), % preferably capitalised only
        list_np(s_form:F2,ne_tag:cdg)
        ]).

%% CDG_NP --> V LIST_NP(cdg)
rule(cdg_np(s_form:[F1,' ',F2]),[
        v(s_form:F1), % preferably capitalised only
        list_np(s_form:F2,ne_tag:cdg)
        ]).

%% CDG_NP --> LIST_NP(cdg)
rule(cdg_np(s_form:F1),[
        list_np(s_form:F1,ne_tag:cdg)
        ]).


%
% NAMES_NP rules
% NAMES_NPs have an ne_tag in their semantics for use in person and
% location rules - their semantics are just ignored in all other cases
%
%% NAMES_NP --> PN NAMES_NP
rule(names_np(edge:Edge,s_form:[F1,' ',F2],
	sem:E^[[ne_tag,E,Edge],[realisation,E,Edge]]),[
        pn(s_form:F1),
        names_np(s_form:F2)
    ]).

%% NAMES_NP --> PN SYM(-) PN
rule(names_np(edge:Edge,s_form:[F1,'-',F2],
        sem:E^[[ne_tag,E,Edge],[realisation,E,Edge]]),[
        pn(s_form:F1),
	sym(s_form:'-'),
        pn(s_form:F2)
    ]).

%% NAMES_NP --> PN
rule(names_np(edge:Edge,s_form:[F],
	sem:E^[[ne_tag,E,Edge],[realisation,E,Edge]]),[
        pn(s_form:F)
    ]).

% NAMES_NP2
% for a series of names separated by comma (recusive rules)
%% NAMES_NP2 --> PN COMMA NAMES_NP2
rule(names_np2(s_form:[F1,', ',F2]),[
       pn(s_form:F1),
       comma(s_form:','),
       names_np2(s_form:F2)
        ]).

%% NAMES_NP2 --> LIST_NP(person) COMMA NAMES_NP2
rule(names_np2(s_form:[F1,', ',F2]),[
       list_np(s_form:F1,ne_tag:person),
       comma(s_form:','),
       names_np2(s_form:F2)
        ]).

% NAMES_NP2 base cases
%% NAMES_NP2 --> PN COMMA PN
rule(names_np2(s_form:[F1,', ',F2]),[
       pn(s_form:F1),
       comma(s_form:','),
       pn(s_form:F2)
        ]).

%% NAMES_NP2 --> LIST_NP(person) COMMA PN
rule(names_np2(s_form:[F1,', ',F2]),[
       list_np(s_form:F1,ne_tag:person),
       comma(s_form:','),
       pn(s_form:F2)
        ]).

%% NAMES_NP2 --> PN COMMA LIST_NP(person)
rule(names_np2(s_form:[F1,', ',F2]),[
       pn(s_form:F1),
       comma(s_form:','),
       list_np(s_form:F2,ne_tag:person)
        ]).

%% NAMES_NP2 --> LIST_NP(person) COMMA LIST_NP(person)
rule(names_np2(s_form:[F1,', ',F2]),[
       list_np(s_form:F1,ne_tag:person),
       comma(s_form:','),
       list_np(s_form:F2,ne_tag:person)
        ]).

% NAMES_NP3
% name 'and' name
%% NAMES_NP3 --> PN CC(and) PN
rule(names_np3(s_form:[F1,' and ',F2]),[
       pn(s_form:F1),
       cc(s_form:'and'),
       pn(s_form:F2)
        ]).


% ORGAN_NAMES_NP
% mixture of possible names for organization
% based on organ_names_np2
%% ORGAN_NAMES_NP --> ORGAN_NAMES_NP2 ORGAN_NAMES_NP
rule(organ_names_np(s_form:[F1,' ',F2]),[
        organ_names_np2(s_form:F1),
	organ_names_np(s_form:F2)
        ]).

%% ORGAN_NAMES_NP --> TAGGED_LOCATION_NP ORGAN_NAMES_NP
rule(organ_names_np(s_form:[F1,' ',F2]),[
        tagged_location_np(s_form:F1),
	organ_names_np(s_form:F2)
        ]).

%% ORGAN_NAMES_NP --> LIST_NP(person) ORGAN_NAMES_NP
rule(organ_names_np(s_form:[F1,' ',F2]),[
        list_np(s_form:F1,ne_tag:person),
	organ_names_np(s_form:F2)
        ]).

%% ORGAN_NAMES_NP --> JJ(New) ORGAN_NAMES_NP
rule(organ_names_np(s_form:['New ',F2]),[
        jj(s_form:'New'),
	organ_names_np(s_form:F2)
        ]).

%% ORGAN_NAMES_NP --> TAGGED_LOCATION_NP
rule(organ_names_np(s_form:[F1]),[
        tagged_location_np(s_form:F1)
        ]).

%% ORGAN_NAMES_NP --> ORGAN_NAMES_NP2
rule(organ_names_np(s_form:[F1]),[
	organ_names_np2(s_form:F1)
        ]).

%% ORGAN_NAMES_NP --> LIST_NP(person)
rule(organ_names_np(s_form:[F1]),[
        list_np(s_form:F1,ne_tag:person)
        ]).

% ORGAN_NAMES_NP --> NAMES_NP POS
%rule(organ_names_np(s_form:[F1,F2]),[
%        names_np(s_form:F1),
%	pos(s_form:F2)
%        ]).

%% ORGAN_NAMES_NP --> NAMES_NP3
rule(organ_names_np(s_form:[F1]),[
        names_np3(s_form:F1)
        ]).

% organ_names_np2 is possible names for organization
% either names_np, list_np(organ), list_np(org_key), list_np(org_base),
% list_np(loc_key)
%% ORGAN_NAMES_NP2 --> NAMES_NP ORGAN_NAMES_NP2
rule(organ_names_np2(s_form:[F1,' ',F2]),[
        names_np(s_form:F1),
	organ_names_np2(s_form:F2)
        ]).

%% ORGAN_NAMES_NP2 --> LIST_NP(organ) ORGAN_NAMES_NP2
rule(organ_names_np2(s_form:[F1,' ',F2]),[
        list_np(s_form:F1,ne_tag:organization),
	organ_names_np2(s_form:F2)
        ]).

%% ORGAN_NAMES_NP2 --> LIST_NP(org_key) ORGAN_NAMES_NP2
rule(organ_names_np2(s_form:[F1,' ',F2]),[
        list_np(s_form:F1,ne_tag:org_key),
	organ_names_np2(s_form:F2)
        ]).

%% ORGAN_NAMES_NP2 --> LIST_NP(org_base) ORGAN_NAMES_NP2
rule(organ_names_np2(s_form:[F1,' ',F2]),[
        list_np(s_form:F1,ne_tag:org_base),
	organ_names_np2(s_form:F2)
        ]).

%% ORGAN_NAMES_NP2 --> LIST_NP(loc_key) ORGAN_NAMES_NP2
rule(organ_names_np2(s_form:[F1,' ',F2]),[
        list_np(s_form:F1,ne_tag:loc_key),
	organ_names_np2(s_form:F2)
        ]).

%% ORGAN_NAMES_NP2 --> NAMES_NP
rule(organ_names_np2(s_form:[F1]),[
        names_np(s_form:F1)
        ]).

%% ORGAN_NAMES_NP2 --> LIST_NP(organ)
rule(organ_names_np2(s_form:[F1]),[
        list_np(s_form:F1,ne_tag:organization)
        ]).

%% ORGAN_NAMES_NP2 --> LIST_NP(org_key)
rule(organ_names_np2(s_form:[F1]),[
        list_np(s_form:F1,ne_tag:org_key)
        ]).

%% ORGAN_NAMES_NP2 --> LIST_NP(org_base)
rule(organ_names_np2(s_form:[F1]),[
        list_np(s_form:F1,ne_tag:org_base)
        ]).

%% ORGAN_NAMES_NP2 --> LIST_NP(loc_key)
rule(organ_names_np2(s_form:[F1]),[
        list_np(s_form:F1,ne_tag:loc_key)
        ]).

%  organ_punc_np rules
%  organ names which include puctuation
%% ORGAN_PUNC_NP --> ORGAN_NAMES_NP SYM('(') TAGGED_LOCATION_NP SYM(')')
rule(organ_punc_np(s_form:[F1,' (',F2,')'],sem:E^[company,E]),[
        organ_names_np(s_form:F1),
	sym(s_form:'('),
	tagged_location_np(s_form:F2),
	sym(s_form:')')
        ]).

%% ORGAN_PUNC_NP --> NAMES_NP SYM(-) NAMES_NP SYM(-) NAMES_NP
rule(organ_punc_np(s_form:[F1,'-',F2,'-',F3],sem:E^[company,E]),[
        names_np(s_form:F1),
	sym(s_form:'-'),
        names_np(s_form:F2),
	sym(s_form:'-'),
        names_np(s_form:F3)
        ]).

%% ORGAN_PUNC_NP --> LIST_NP(organ) SYM(-) TAGGED_LOCATION_NP
rule(organ_punc_np(s_form:[F1,'-',F2],sem:E^[T,E]),[
        list_np(s_form:F1,ne_tag:organization,ne_type:T),
	sym(s_form:'-'),
        tagged_location_np(s_form:F2)
        ]).

%% ORGAN_PUNC_NP --> NAMES_NP SYM(-) TAGGED_LOCATION_NP
rule(organ_punc_np(s_form:[F1,'-',F2],sem:E^[company,E]),[
        names_np(s_form:F1),
	sym(s_form:'-'),
        tagged_location_np(s_form:F2)
        ]).

%% ORGAN_PUNC_NP --> TAGGED_LOCATION_NP SYM(-) NAMES_NP
rule(organ_punc_np(s_form:[F1,'-',F2],sem:E^[company,E]),[
        tagged_location_np(s_form:F1),
	sym(s_form:'-'),
        names_np(s_form:F2)
        ]).

%**********************************************************************
%
%  Rules for Person names
%
%  person_np : peron NP
%  person_peirod_np  : person NP which includes period inside
%  person_special_np : person NP special cases
%  person_ending_np  : person NP with person_ending
%  person_ending     : either 'Jr','Sr','II','III'
%  de_np    : special words ('de','di','von','van')
%  title_np : title or title NP where title is its head
%
%**********************************************************************

%
%   NP Rules
%
% realisation covers the whole NP
% semantic ne_tag covers the name less any title (MUC specific rule)

%% NP(Basic) --> PERSON_NP
rule(basic_np(edge:Edge,head:F,
       sem:E^[[name,E,F],PSem,[person,E],[realisation,E,Edge]]),
       [person_np(s_form:F,sem:E^PSem)]).

%% NP(Basic) --> TITLE_NP
rule(basic_np(edge:Edge,head:F,
       sem:E^[[title,E,F],[realisation,E,Edge]]),
       [title_np(s_form:F)]).

%
% PERSON_NP rules
%
%% PERSON_NP --> PN LIST_NP(person,first)
rule(person_np(edge:Edge,s_form:[F1,' ',F2],sem:E^[ne_tag,E,Edge]),[
        pn(s_form:F1),
        list_np(s_form:F2,ne_tag:person,ne_type:person_first)
        ]).

%% PERSON_NP --> PERSON_PERIOD_NP DE_NP NAMES_NP
rule(person_np(edge:Edge,s_form:[F1,' ',F2,' ',F3],sem:E^[ne_tag,E,Edge]),[
       person_period_np(s_form:F1),
       de_np(s_form:F2),
       names_np(s_form:F3)
        ]).

%% PERSON_NP --> PERSON_SPECIAL_NP DE_NP NAMES_NP
rule(person_np(edge:Edge,s_form:[F1,' ',F2,' ',F3],sem:E^[ne_tag,E,Edge]),[
       person_special_np(s_form:F1),
       de_np(s_form:F2),
       names_np(s_form:F3)
        ]).

%% PERSON_NP --> PERSON_PERIOD_NP
rule(person_np(s_form:[F1],sem:E^[PSem]),[
       person_period_np(s_form:F1,sem:E^PSem) % ne_tag in PSem
        ]).

%% PERSON_NP --> PERSON_SPECIAL_NP
rule(person_np(s_form:[F1],sem:E^[PSem]),[
       person_special_np(s_form:F1,sem:E^PSem) % ne_tag in PSem
        ]).

% Personal Names with a special ending (e.g. Jr.)
%% PERSON_NP --> LIST_NP(person,first) NAMES_NP PERSON_ENDING
rule(person_np(edge:Edge,s_form:[F1,' ',F2,' ',F3], sem:E^[ne_tag,E,Edge]),[
        list_np(s_form:F1,ne_tag:person,ne_type:person_first),
        names_np(s_form:F2),
	person_ending(s_form:F3)
        ]).

%% PERSON_NP --> LIST_NP(person,first) PERSON_ENDING
rule(person_np(edge:Edge,s_form:[F1,' ',F2], sem:E^[ne_tag,E,Edge]),[
        list_np(s_form:F1,ne_tag:person,ne_type:person_first),
	person_ending(s_form:F2)
        ]).

%% PERSON_NP --> PERSON_ENDING_NP
rule(person_np(s_form:[F1],sem:E^[PSem]),[
        person_ending_np(s_form:F1,sem:E^PSem) % ne_tag in PSem
        ]).

% Personal Names with a title
%% PERSON_NP --> TITLE_NP PERSON_PERIOD_NP
rule(person_np(s_form:[F2],sem:E^[PSem,[title,E,F]]),[
        title_np(s_form:F),
        person_period_np(s_form:F2,sem:E^PSem) % ne_tag in PSem
        ]).

%% PERSON_NP --> TITLE_NP PERSON_SPECIAL_NP
rule(person_np(s_form:[F2],sem:E^[PSem,[title,E,F]]),[
        title_np(s_form:F),
        person_special_np(s_form:F2,sem:E^PSem) % ne_tag in PSem
        ]).

%% PERSON_NP --> TITLE_NP NAMES_NP
rule(person_np(s_form:[F2],sem:E^[PSem,[title,E,F]]),[
        title_np(s_form:F),
        names_np(s_form:F2,sem:E^[PSem|_]) % pass on only the ne_tag
        ]).

%% PERSON_NP --> TITLE_NP PERSON_ENDING_NP
rule(person_np(s_form:[F2],sem:E^[PSem,[title,E,F]]),[
        title_np(s_form:F),
        person_ending_np(s_form:F2,sem:E^PSem) % ne_tag in PSem
        ]).

%% PERSON_NP --> TITLE_NP TAGGED_LOCATION_NP
rule(person_np(s_form:[F2],sem:E^[PSem,[title,E,F]]),[
        title_np(s_form:F),
        tagged_list_np(s_form:F2,sem:E^PSem) % ne_tag in PSem
        ]).

%% TAGGED_LIST_NP -> LIST_NP
rule(tagged_list_np(edge:Edge,s_form:F1,sem:E^[ne_tag,E,Edge]),[
        list_np(s_form:F1)
        ]).


%  PERSON_PERIOD_NP assigns its realisation to ne_tag semantic feature
%
%% PERSON_PERIOD_NP --> PN PN PERIOD PN
rule(person_period_np(edge:Edge,s_form:[F1,' ',F2,'. ',F3],
       sem:E^[ne_tag,E,Edge]),[
        pn(s_form:F1),
        pn(s_form:F2),
        period(s_form:'.'),
        pn(s_form:F3)
        ]).

%% PERSON_PERIOD_NP --> PN PERIOD PN PN
rule(person_period_np(edge:Edge,s_form:[F1,'. ',F2,' ',F3],
       sem:E^[ne_tag,E,Edge]),[
        pn(s_form:F1),
        period(s_form:'.'),
        pn(s_form:F2),
        pn(s_form:F3)
        ]).

%% PERSON_PERIOD_NP --> PN PN PERIOD LIST_NP(person)
rule(person_period_np(edge:Edge,s_form:[F1,' ',F2,'. ',F3],
       sem:E^[ne_tag,E,Edge]),[
        pn(s_form:F1),
	pn(s_form:F2),
	period(s_form:'.'),
	list_np(s_form:F3,ne_tag:person)
        ]).

%% PERSON_PERIOD_NP --> LIST_NP(person,first) PN PERIOD PN
rule(person_period_np(edge:Edge,s_form:[F1,' ',F2,'. ',F3],
	sem:E^[ne_tag,E,Edge]),[
        list_np(s_form:F1,ne_tag:person,ne_type:person_first),
	pn(s_form:F2),
        period(s_form:'.'),
        pn(s_form:F3)
        ]).

%% PERSON_PERIOD_NP --> PN PERIOD LIST_NP(person,first) PN
rule(person_period_np(edge:Edge,s_form:[F1,'. ',F2,' ',F3],
	sem:E^[ne_tag,E,Edge]),[
   	pn(s_form:F1),
        period(s_form:'.'),
        list_np(s_form:F2,ne_tag:person,ne_type:person_first),
        pn(s_form:F3)
        ]).

%% PERSON_PERIOD_NP --> LIST_NP(person,first) PN PERIOD LIST_NP(person)
rule(person_period_np(edge:Edge,s_form:[F1,' ',F2,'. ',F3],
        sem:E^[ne_tag,E,Edge]),[
        list_np(s_form:F1,ne_tag:person,ne_type:person_first),
	pn(s_form:F2),
	period(s_form:'.'),
	list_np(s_form:F3,ne_tag:person)
        ]).

%% PERSON_PERIOD_NP --> PN(St) PERIOD LIST_NP(person,first)
rule(person_period_np(edge:Edge,s_form:['St. ',F1],sem:E^[ne_tag,E,Edge]),[
        pn(s_form:'St'),
	period(s_form:'.'),
        list_np(s_form:F1,ne_tag:person,ne_type:person_first)
        ]).

%% PERSON_SPECIAL_NP --> LIST_NP(person,first)
rule(person_special_np(edge:Edge,s_form:[F1],
	sem:E^[ne_tag,E,Edge]),[
        list_np(s_form:F1,ne_tag:person,ne_type:person_first)
        ]).

%% PERSON_SPECIAL_NP --> LIST_NP(person,full)
rule(person_special_np(edge:Edge,s_form:[F1],
	sem:E^[ne_tag,E,Edge]),[
        list_np(s_form:F1,ne_tag:person,ne_type:person_full)
        ]).

%% PERSON_SPECIAL_NP -> LIST_NP(person,first) NAMES_NP
rule(person_special_np(edge:Edge,s_form:[F1,' ',F2],sem:E^[ne_tag,E,Edge]),[
        list_np(s_form:F1,ne_tag:person,ne_type:person_first),
        names_np(s_form:F2)
        ]).

%% PERSON_SPECIAL_NP -> LIST_NP(person,first) LIST_NP
rule(person_special_np(edge:Edge,s_form:[F1,' ',F2],sem:E^[ne_tag,E,Edge]),[
        list_np(s_form:F1,ne_tag:person,ne_type:person_first),
        list_np(s_form:F2)
        ]).

%% PERSON_SPECIAL_NP -> LIST_NP(date) LIST_NP(person,first)
rule(person_special_np(edge:Edge,s_form:[F1,' ',F2],sem:E^[ne_tag,E,Edge]),[
        list_np(s_form:F1,ne_tag:date),
        list_np(s_form:F2,ne_tag:person,ne_type:person_first)
        ]).

%% PERSON_SPECIAL_NP -> LIST_NP(person,first) TAGGED_LOCATION_NP
rule(person_special_np(edge:Edge,s_form:[F1,' ',F2],sem:E^[ne_tag,E,Edge]),[
        list_np(s_form:F1,ne_tag:person,ne_type:person_first),
        tagged_location_np(s_form:F2)
        ]).

% e.g. Karl "Tuffy" Rhodes
%% PERSON_SPECIAL_NP -> NAMES_NP SYM(") PN SYM(") NAMES_NP
rule(person_special_np(edge:Edge,s_form:[F1,' "',F2,'" ',F3],
	   sem:E^[ne_tag,E,Edge]),[
        names_np(s_form:F1),
	sym(s_form:'"'),
	pn(s_form:F2),
	sym(s_form:'"'),
        names_np(s_form:F3)
        ]).

%% PERSON_SPECIAL_NP -> NAMES_NP SYM(") PN SYM(") NAMES_NP
rule(person_special_np(edge:Edge,s_form:[F1,' "',F2,'" ',F3],
	   sem:E^[ne_tag,E,Edge]),[
        list_np(s_form:F1,ne_tag:person,ne_type:person_first),
	sym(s_form:'"'),
	pn(s_form:F2),
	sym(s_form:'"'),
        names_np(s_form:F3)
        ]).

% names ends with a special ending (e.g. Jr.)
%% PERSON_ENDING_NP --> NAMES_NP PERSON_ENDING
rule(person_ending_np(edge:Edge,s_form:[F1,' ',F2],sem:E^[ne_tag,E,Edge]),[
        names_np(s_form:F1),
	person_ending(s_form:F2)
        ]).

%% PERSON_ENDING_NP --> PERSON_PERIOD_NP PERSON_ENDING
rule(person_ending_np(edge:Edge,s_form:[F1,' ',F2],sem:E^[ne_tag,E,Edge]),[
        person_period_np(s_form:F1),
	person_ending(s_form:F2)
        ]).

%% PERSON_ENDING_NP --> PERSON_SPECIAL_NP PERSON_ENDING
rule(person_ending_np(edge:Edge,s_form:[F1,' ',F2],sem:E^[ne_tag,E,Edge]),[
        person_special_np(s_form:F1),
	person_ending(s_form:F2)
        ]).

%% PERSON_ENDING --> PN(Jr.)
rule(person_ending(s_form:['Jr.']),[
	pn(s_form:'Jr'),
	period(s_form:'.')
        ]).

%% PERSON_ENDING --> PN(Sr.)
rule(person_ending(s_form:['Sr.']),[
	pn(s_form:'Sr'),
	period(s_form:'.')
        ]).

%% PERSON_ENDING --> PN(II)
rule(person_ending(s_form:['II']),[
	pn(s_form:'II') 
        ]).

%% PERSON_ENDING --> PN(III)
rule(person_ending(s_form:['III']),[
	pn(s_form:'III') 
        ]).

% special word in person name: de, di, von, van
%% DE_NP --> PN(de)
rule(de_np(s_form:['de']),[
        pn(s_form:'de')]).

%% DE_NP --> PN(di)
rule(de_np(s_form:['di']),[
        pn(s_form:'di')]).

%% DE_NP --> PN(von)
rule(de_np(s_form:['von']),[
        pn(s_form:'von')]).

%% DE_NP --> PN(van)
rule(de_np(s_form:['van']),[
        pn(s_form:'van')]).

% Title noun phrases
% some of title phrases are more than just a listed title
% eg. senior president of planning
%% TITLE_NP --> LIST_NP(title) IN(of) N
rule(title_np(s_form:[F1,' of ',F2]),[
        list_np(s_form:F1,ne_tag:title),
	in(s_form:'of'),
	n(s_form:F2)]).

%% TITLE_NP --> LIST_NP(title) IN(of) JJ N
rule(title_np(s_form:[F1,' of ',F2,' ',F3]),[
        list_np(s_form:F1,ne_tag:title),
	in(s_form:'of'),
	jj(s_form:F2),
	n(s_form:F3)]).

%% TITLE_NP --> LIST_NP(title) IN(of) JJ CC(and) JJ N
rule(title_np(s_form:[F1,' of ',F2,' and ',F3,' ',F4]),[
        list_np(s_form:F1,ne_tag:title),
	in(s_form:'of'),
	jj(s_form:F2),
	cc(s_form:'and'),
	jj(s_form:F3),
	n(s_form:F4)]).

%% TITLE_NP --> LIST_NP(title)
rule(title_np(s_form:[F1]),[
        list_np(s_form:F1,ne_tag:title)]).



%**********************************************************************
%
%  Rules for Date/Time names
%
%  date_np : date NP (days of the week, months, years)
%  time_np : time NP (eg. 12 a.m.)
%
%**********************************************************************

%
%   NP Rules
%
%% NP(Basic) --> DATE_NP
rule(basic_np(edge:Edge,head:F,
	sem:E^[[name,E,F],[date,E],[realisation,E,Edge],[ne_tag,E,Edge]]),
	[date_np(s_form:F)]).

%
% DATE_NP rules
% 

%% DATE_NP --> CD N(/) CD N(/) CD (in DD line)
rule(date_np(s_form:[F1,'/',F3,'/',F5]),[
        cd(s_form:F1),
        n(s_form:'/'),
	cd(s_form:F3),
        n(s_form:'/'),
        cd(s_form:F5)
    ]).

%% LIST_NP(date) VBD(ended) DATE CD
% rule for 'ended'
rule(date_np(s_form:[F1,' ended ',F2,' ',F3]),[
        list_np(s_form:F1,ne_tag:date),
        vbd(s_form:'ended'),
        date(s_form:F2),
	cd(s_form:F3)
	]).


%% DATE_NP --> DATE COMMA DATE CD COMMA LIST_NP(date)
rule(date_np(s_form:[F1,', ',F2,' ',F3,', ',F4]),[
        date(s_form:F1),
	comma(s_form:','),
        date(s_form:F2),
	cd(s_form:F3),
        comma(s_form:','),
        list_np(s_form:F4,ne_tag:date)
    ]).

%% DATE_NP --> DATE PERIOD CD COMMA LIST_NP(date)
rule(date_np(s_form:[F1,F2,' ',F3,F4,' ',F5]),[
        date(s_form:F1),
        period(s_form:F2),
	cd(s_form:F3),
        comma(s_form:F4),
        list_np(s_form:F5,ne_tag:date)
    ]).

%% DATE_NP --> DATE CD COMMA LIST_NP(date)
rule(date_np(s_form:[F1,' ',F2,F3,' ',F4]),[
        date(s_form:F1),
	cd(s_form:F2),
        comma(s_form:F3),
        list_np(s_form:F4,ne_tag:date)
    ]).


%% DATE_NP --> LIST_NP(date) IN(of) LIST_NP(date)
rule(date_np(s_form:[F1,' of ',F2]),[
        list_np(s_form:F1,ne_tag:date),
	in(s_form:'of'),
        list_np(s_form:F2,ne_tag:date)
    ]).

%% DATE_NP --> LIST_NP(date) LIST_NP(date)
rule(date_np(s_form:[F1,' ',F2]),[
        list_np(s_form:F1,ne_tag:date),
        list_np(s_form:F2,ne_tag:date)
    ]).

%% DATE_NP --> LIST_NP(date) (for special date phrases from list-lookup)
rule(date_np(s_form:[F1]),[
        list_np(s_form:F1,ne_tag:date)
    ]).

%% DATE_NP --> DATE PERIOD LIST_NP(date)
rule(date_np(s_form:[F1,F2,' ',F3]),[
        date(s_form:F1),
        period(s_form:F2),
	list_np(s_form:F3,ne_tag:date)
    ]).

%% DATE_NP --> DATE COMMA LIST_NP(date)
rule(date_np(s_form:[F1,', ',F3]),[
        date(s_form:F1),
        comma(s_form:','),
	list_np(s_form:F3,ne_tag:date)
    ]).

%% DATE_NP --> DATE LIST_NP(date)
rule(date_np(s_form:[F1,' ',F2]),[
        date(s_form:F1),
        list_np(s_form:F2,ne_tag:date)
    ]).

%% DATE_NP --> DATE PERIOD CD
rule(date_np(s_form:[F1,F2,' ',F3]),[
        date(s_form:F1),
        period(s_form:F2),
	cd(s_form:F3)
    ]).

%% DATE_NP --> DATE CD
rule(date_np(s_form:[F1,' ',F2]),[
        date(s_form:F1),
        cd(s_form:F2)
    ]).

%% DATE_NP --> DATE

rule(date_np(s_form:F),[
        date(s_form:F)
    ]).

%% NP(Basic) --> TIME_NP
rule(basic_np(edge:Edge,head:F,
	sem:E^[[name,E,F],[muc_time,E],[realisation,E,Edge],[ne_tag,E,Edge]]),
	[time_np(s_form:F)]).

%% TIME_NP --> CD LIST_NP(time)
rule(time_np(s_form:[F1,' ',F2]),[
        cd(s_form:F1),
        list_np(s_form:F2,ne_tag:time)
    ]).

%% TIME_NP --> LIST_NP(time)
rule(time_np(s_form:[F1]),[
        list_np(s_form:F1,ne_tag:time)
    ]).


%**********************************************************************
%
%  Rules for Monetary expressions (Money and Percent)
%
%  money_np    : money NP
%  m_unit_np   : currency unit ($,dollars,...)
%  m_unit      : money unit (million, billion, trillion)
%  percent_np  : percetage NP
%  fraction_np : fraction NP (3/4, 7 3/4,...)
%  cd_np       : cardinal number in monetary expressions
%
%**********************************************************************

%
%   NP Rules
%
%% NP(Basic) --> MONEY_NP
rule(basic_np(edge:Edge,head:F,
	sem:E^[[name,E,F],[money,E],[realisation,E,Edge],Sem]),
	[money_np(s_form:F,sem:E^Sem)]).

%
% MONEY_NP rules
%
%% MONEY_NP --> CD_NP JJ JJ M_UNIT_NP
rule(money_np(edge:Edge,sem:E^[ne_tag,E,Edge],s_form:[F1,' ',F2,' ',F3,' ',F4]),[
        cd_np(s_form:F1),
        jj(s_form:F2),
        jj(s_form:F3),
	m_unit_np(s_form:F4)
        ]).

%% MONEY_NP --> CD_NP JJ M_UNIT_NP
rule(money_np(edge:Edge,sem:E^[ne_tag,E,Edge],s_form:[F1,' ',F2,' ',F3]),[
        cd_np(s_form:F1),
        jj(s_form:F2),
	m_unit_np(s_form:F3)
        ]).

%% MONEY_NP --> CD_NP TAGGED_LOCATION_NP M_UNIT_NP
rule(money_np(edge:Edge,sem:E^[[ne_tag,E,Edge],Sem],s_form:[F1,' ',F2,' ',F3]),[
        cd_np(s_form:F1),
        tagged_location_np(s_form:F2,sem:Sem),
	m_unit_np(s_form:F3)
        ]).

%% MONEY_NP --> TAGGED_LOCATION_NP M_UNIT_NP CD_NP
rule(money_np(edge:Edge,sem:E^[[ne_tag,E,Edge],Sem],s_form:[F1,' ',F2,' ',F3]),[
        tagged_location_np(s_form:F1,sem:Sem),
	m_unit_np(s_form:F2),
        cd_np(s_form:F3)
        ]).


%% MONEY_NP --> CD_NP M_UNIT_NP
rule(money_np(edge:Edge,sem:E^[ne_tag,E,Edge],s_form:[F1,' ',F2]),[
        cd_np(s_form:F1),
	m_unit_np(s_form:F2)
        ]).

%% MONEY_NP -->  M_UNIT_NP CD_NP
rule(money_np(edge:Edge,sem:E^[ne_tag,E,Edge],s_form:[F1,F2]),[
        m_unit_np(s_form:F1),
        cd_np(s_form:F2)
        ]).


%% M_UNIT_NP --> SYM($)
rule(m_unit_np(s_form:'$'),[
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
%% NP(Basic) --> PERCENT_NP
rule(basic_np(edge:Edge,head:F,
	sem:E^[[name,E,F],[percent,E],[realisation,E,Edge],[ne_tag,E,Edge]]),
	[percent_np(s_form:F)]).

%% PERCENT_NP --> CD_NP N(percent)
rule(percent_np(s_form:[F1,'%']),[
        cd_np(s_form:F1),
        n(s_form:'%')
        ]).

%% PERCENT_NP --> FRACTION_NP N(percent)
rule(percent_np(s_form:[F1,'%']),[
        fraction_np(s_form:F1),
        n(s_form:'%')
        ]).

%% PERCENT_NP --> FRACTION_NP N(percentage) N(point)
rule(percent_np(s_form:[F1,' percentage ',S_Form]),[
        fraction_np(s_form:F1),
        n(s_form:'percentage'),
	n(m_root:'point',s_form:S_Form)
        ]).

%% PERCENT_NP --> CD_NP N(percentage) N(point)
rule(percent_np(s_form:[F1,' percentage ',S_Form]),[
        cd_np(s_form:F1),
        n(s_form:'percentage'),
	n(m_root:'point',s_form:S_Form)
        ]).


%% FRACTION_NP --> CD CD N('/') CD
rule(fraction_np(s_form:[F1,F2,'/',F3]),[
        cd(s_form:F1),
	cd(s_form:F2),
	n(s_form:'/'),
	cd(s_form:F3)
        ]).

%% FRACTION_NP --> CD N('/') CD
rule(fraction_np(s_form:[F1,'/',F2]),[
        cd(s_form:F1),
	n(s_form:'/'),
	cd(s_form:F2)
        ]).


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

%**********************************************************************
%
%  Rules for Location names
%
%  location_np : locaton NP
%  tagged_location_np : location marked up at the list lookup
%
%**********************************************************************

%  NP rules
%% NP(Basic) --> LOCATION_NP
rule(basic_np(sem:Sem),
	[location_np(sem:Sem)]).

%% NP(Basic) --> TAGGED_LOCATION_NP
rule(basic_np(sem:Sem),
	[tagged_location_np(sem:Sem)]).

% 'State of' xxx is a location (province)
%% LOCATION_NP --> PN(State) IN(of) NAMES_NP
rule(location_np(edge:Edge,s_form:['State of ',F1],sem:E^[[location,E],
	[province,E],[name,E,['State of ',F1]],[ne_tag,E,Edge],
	   [realisation,E,Edge]]),[
	pn(s_form:'State'),
	in(s_form:'of'),
        names_np(s_form:F1)
        ]).

%% LOCATION_NP --> PN(State) IN(of) TAGGED_LOCATION_NP
rule(location_np(edge:Edge,s_form:['State of ',F1],sem:E^[[location,E],
	[province,E],[name,E,['State of ',F1]],[ne_tag,E,Edge],
	   [realisation,E,Edge]]),[
	pn(s_form:'State'),
	in(s_form:'of'),
        tagged_location_np(s_form:F1)
        ]).

%% LOCATION_NP --> DT(the) NAMES_NP N(region)
rule(location_np(edge:Edge,s_form:['the ',F1],sem:E^[[location,E],[region,E],
        [name,E,['the ',F1]],NP1Sem,[realisation,E,Edge]]),[
        dt(s_form:'the'),
        names_np(s_form:F1,sem:E^NP1Sem), % this will contain an ne_tag
	n(s_form:'region')
        ]).

%% LOCATION_NP --> NAMES_NP LIST_NP(loc_key)
rule(location_np(edge:Edge,s_form:[F1,' ',F2],sem:E^[[location,E],[region,E],
        [name,E,[F1,' ',F2]],[ne_tag,E,Edge],[realisation,E,Edge]]),[
        names_np(s_form:F1),
	list_np(s_form:F2,ne_tag:loc_key)
        ]).

%% LOCATION_NP --> TAGGED_LOCATION_NP LIST_NP(loc_key)
rule(location_np(edge:Edge,s_form:[F1,' ',F2],sem:E^[[location,E],[region,E],
        [name,E,[F1,' ',F2]],[ne_tag,E,Edge],[realisation,E,Edge]]),[
        tagged_location_np(s_form:F1),
	list_np(s_form:F2,ne_tag:loc_key)
        ]).

%% LOCATION_NP --> NAMES_NP COMMA TAGGED_LOCATION_NP(PROVINCE)
% Names_np is probably a city.
rule(location_np(s_form:[F1,', ',F2],
	sem:E^E2^[NP1Sem,[location,E],[name,E,F1],[city,E],NP2Sem,[in,E,E2]]),[
        names_np(s_form:F1,sem:E^NP1Sem), % this will contain an ne_tag
	comma(s_form:','),
        tagged_location_np(s_form:F2,ne_tag:location,ne_type:province,sem:E2^NP2Sem)
        ]).

%% LOCATION_NP --> NAMES_NP COMMA TAGGED_LOCATION_NP(COUNTRY)
% Names_np is an unknown location.
rule(location_np(s_form:[F1,', ',F2],
	sem:E^[NP1Sem,[location,E],[name,E,F1],E2^NP2Sem,[in,E,E2]]),[
        names_np(s_form:F1,sem:E^NP1Sem), % this will contain an ne_tag
	comma(s_form:','),
        tagged_location_np(s_form:F2,ne_tag:location,ne_type:country,sem:E2^NP2Sem)
        ]).

%% LOCATION_NP --> TAGGED_LOCATION_NP(LOCATION) COMMA TAGGED_LIST_NP(LOCATION)
rule(location_np(s_form:[F1,', ',F2],sem:[E1^NP1Sem,E2^NP2Sem,[in,E1,E2]]),[
        tagged_location_np(s_form:F1,sem:E1^NP1Sem),
	comma(s_form:','),
	tagged_location_np(s_form:F2,sem:E2^NP2Sem)
        ]).

%% TAGGED_LOCATION_NP --> LIST_NP(LOCATION)
% ne_type : either city, province, country, or region
rule(tagged_location_np(edge:Edge,s_form:[F1],ne_tag:location,ne_type:T,
	sem:E^[[ne_tag,E,Edge],[location,E],[name,E,F1],[T,E],[realisation,E,Edge]]),[
        list_np(s_form:F1,ne_tag:location,ne_type:T)
        ]).

