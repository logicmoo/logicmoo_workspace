%**********************************************************************
%
%  Rules for Location names
%
%  location_np : locaton NP
%  tagged_location_np : location marked up at the list lookup
%
%**********************************************************************

%
% Best Parse Categories
:- multifile best_parse_cats/1, rule/2.
:- dynamic best_parse_cats/1, rule/2.

best_parse_cats([ne_np]).



%% NE_NP --> LOCATION_NP
rule(ne_np(edge:Edge,s_form:F,sem:E^[[location,E],Sem,[ne_tag,E,Edge],[realisation,E,Edge]]),
	[location_np(s_form:F,sem:E^Sem)]).

%% NE_NP --> TAGGED_LOCATION_NP
rule(ne_np(s_form:F,source:list,sem:Sem),
	[tagged_location_np(s_form:F,sem:Sem)]).


% NE_NP -> LIST_NP(country_adj)
% Turkish
rule(ne_np(edge:Edge,
    sem:E2^[[location,E2],[country,E2],[adj,E2,F1],[name,E2,F1],[realisation,E2,Edge]]), [
     list_np(m_root:R,s_form:F1,ne_tag:country_adj)
]).

%% NE_NP --> TAGGED_LOCATION_NP TITLEPERSON_NP
rule(ne_np(s_form:F,sem:E1^E2^E3^[LSem,[person,E2],PSem,TSem]),
	[tagged_location_np(s_form:F,sem:E1^LSem),
	 title_np(sem:E3^TSem),
	 ne_np(sem:E2^[[person,E2]|PSem])]).


% 'State of' xxx is a location (province)
%% LOCATION_NP --> PN(State) IN(of) NAMES_NP
rule(location_np(edge:Edge,s_form:['State of ',F1],
	sem:E^[[province,E],[name,E,['State of ',F1]]]),[
	pn(s_form:'State'),
	in(s_form:'of'),
        names_np(s_form:F1)
        ]).

%% LOCATION_NP --> PN(State) IN(of) LIST_NP(location)
rule(location_np(edge:Edge,s_form:['State of ',F1],
	sem:E^[[province,E],[name,E,['State of ',F1]]]),[
	pn(s_form:'State'),
	in(s_form:'of'),
        list_np(s_form:F1,ne_tag:location)
        ]).

%% NE_NP --> DT(the) NAMES_NP N(region)
rule(ne_np(s_form:['the ',F1],
	sem:E^[[location,E],[region,E],[name,E,['the ',F1]],NP1Sem]),[
        dt(s_form:'the'),
        names_np(s_form:F1,sem:E^[NP1Sem|_]), % this will contain an ne_tag
	n(s_form:'region')
        ]).

%% LOCATION_NP --> NAMES_NP LIST_NP(loc_key)
rule(location_np(s_form:[F1,' ',F2],
	sem:E^[[name,E,[F1,' ',F2]],[R2,E]]),[
        names_np(s_form:F1),
	list_np(s_form:F2,ne_tag:loc_key,ne_type:post,m_root:R2)
        ]).

%% LOCATION_NP --> LIST_NP(location) LIST_NP(loc_key)
rule(location_np(s_form:[F1,' ',F2],
	sem:E^[[name,E,[F1,' ',F2]],[R2,E]]),[
        list_np(s_form:F1,ne_tag:location),
	list_np(s_form:F2,ne_tag:loc_key,ne_type:post,m_root:R2)
        ]).

%% LOCATION_NP --> LIST_NP(loc_prekey) NAMES_NP
rule(location_np(s_form:[F1,' ',F2],
	sem:E^[[region,E],[name,E,[F1,' ',F2]]]),[
        list_np(s_form:F1,ne_tag:loc_key,ne_type:pre),
        names_np(s_form:F2)
        ]).

%% LOCATION_NP --> LIST_NP(loc_prekey) LIST_NP(location)
rule(location_np(s_form:[F1,' ',F2],
	sem:E^[[region,E],[name,E,[F1,' ',F2]]]),[
	list_np(s_form:F1,ne_tag:loc_key,ne_type:pre),
        list_np(s_form:F2,ne_tag:location)
        ]).

%% NE_NP --> TAGGED_LOCATION_NP COMMA TAGGED_LOCATION_NP
rule(ne_np(s_form:[F1,', ',F2],
                 sem:E1^E2^[NP1Sem,NP2Sem,[in,E1,E2],[apposed,E1,E2]]),[
        tagged_location_np(s_form:F1,sem:E1^NP1Sem),
	comma(s_form:','),
	tagged_location_np(s_form:F2,sem:E2^NP2Sem)
        ]).

%% TAGGED_LOCATION_NP --> LIST_NP(location)
% ne_type : either city, province, country, or region
rule(tagged_location_np(edge:Edge,s_form:F1,ne_tag:location,ne_type:T,
	sem:E^[[location,E],[T,E],[ne_tag,E,Edge],[name,E,F1],[realisation,E,Edge]]),[
        list_np(s_form:F1,ne_tag:location,ne_type:T)
        ]).

% water %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% LOCATION_NP --> PN Sea
rule(location_np(s_form:[F1,' ',F2],
	sem:E^[[water_loc,E],[sea,E],[name,E,[F1,' ',F2]]]),[
        pn(s_form:F1),
	pn(s_form:F2,m_root:'sea')
        ]).

%% LOCATION_NP --> PN Ocean
rule(location_np(s_form:[F1,' ',F2],
	sem:E^[[water_loc,E],[ocean,E],[name,E,[F1,' ',F2]]]),[
        pn(s_form:F1),
	pn(s_form:F2,m_root:'ocean')
        ]).

%% LOCATION_NP --> PN Gulf
rule(location_np(s_form:[F1,' ',F2],
	sem:E^[[water_loc,E],[gulf,E],[name,E,[F1,' ',F2]]]),[
        pn(s_form:F1),
	pn(s_form:F2,m_root:'gulf')
        ]).

%% LOCATION_NP --> PN Everglades
rule(location_np(s_form:[F1,' ',F2],
	sem:E^[[water_loc,E],[name,E,[F1,' ',F2]]|Sem]),[
        ne_np(s_form:F,sem:Sem),
	pn(s_form:F2,m_root:'everglades')
        ]).

% airports %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%consider these make Fort Worth Airport a region if it is first in the file

rule(ne_np(s_form:[F,' Airport'],edge:Edge,sem:
     E^[[location,E],[airport,E],[name,E,[F,' Airport']],[ne_tag,E,Edge]]), [
        ne_np(s_form:F),
	pn(s_form:'Airport')
]).

rule(ne_np(s_form:F,sem:
     E^[[location,E],[airport,E],[name,E,F],[ne_tag,E,Edge]]), [
        ne_np(edge:Edge,s_form:F),
	n(s_form:'airport')
]).

rule(ne_np(edge:Edge,s_form:[F,' International'],sem:
     E^[[location,E],[airport,E],[name,E,[F,' International']],[ne_tag,E,Edge]]), [
        ne_np(s_form:F),
	pn(s_form:'International')
]).

rule(ne_np(s_form:F,sem:
     E^[[location,E],[airport,E],[name,E,F],[ne_tag,E,Edge]]), [
        ne_np(edge:Edge,s_form:F),
	n(s_form:F2,m_root:'international')
]).

