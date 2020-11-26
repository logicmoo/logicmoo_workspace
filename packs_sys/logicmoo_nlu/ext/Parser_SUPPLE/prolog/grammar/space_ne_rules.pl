%**********************************************************************
%
%  Rules for Space names
%
%  space_np : space NP
%
%**********************************************************************

%
% Best Parse Categories
:- multifile best_parse_cats/1, rule/2.
:- dynamic best_parse_cats/1, rule/2.

best_parse_cats([ne_np]).



% planets %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rule(location_np(s_form:F,sem:E^[[planet,E],[name,E,F]]),[
        list_np(s_form:F,ne_tag:planet)
]).


% spacecraft %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rule(ne_np(edge:Edge,source:list,s_form:F,
     sem:E^[Sem,[name,E,F],[realisation,E,Edge]]),[
        space_np(s_form:F,sem:E^Sem)
]).

rule(ne_np(edge:Edge,source:list,s_form:[F,' ',F2],
     sem:E^[Sem,[name,E,[F,' ',F2]],[realisation,E,Edge]]),[
        space_np(s_form:F,sem:E^Sem),
        cd(s_form:F2)
]).

% rockets
rule(space_np(s_form:F,sem:E^[[spacecraft,E],[rocket,E]]),[
        list_np(s_form:F,ne_tag:spacecraft,ne_type:rocket)
]).

% shuttle
rule(space_np(s_form:F,sem:E^[[spacecraft,E],[shuttle,E]]),[
        list_np(s_form:F,ne_tag:spacecraft,ne_type:shuttle)
]).

% space station
rule(space_np(s_form:F,sem:E^[[space_station,E]]),[
        list_np(s_form:F,ne_tag:spacestation)
]).

% space probe
rule(space_np(s_form:F,sem:E^[[space_probe,E]]),[
        list_np(s_form:F,ne_tag:spaceprobe)
]).

% satellites
rule(space_np(s_form:F,
    sem:E^[[satellite,E],[payload_list_func,E,destruct]]),[
        list_np(s_form:F,ne_tag:satellite,ne_type:destruct)
]).
rule(space_np(s_form:F,
    sem:E^[[satellite,E],[payload_list_func,E,intell]]),[
        list_np(s_form:F,ne_tag:satellite,ne_type:intell)
]).
rule(space_np(s_form:F,
    sem:E^[[satellite,E],[payload_list_func,E,research]]),[
        list_np(s_form:F,ne_tag:satellite,ne_type:research)
]).
rule(space_np(s_form:F,
    sem:E^[[satellite,E],[payload_list_func,E,tv]]),[
        list_np(s_form:F,ne_tag:satellite,ne_type:tv)
]).
rule(space_np(s_form:F,
    sem:E^[[satellite,E],[payload_list_func,E,weather]]),[
        list_np(s_form:F,ne_tag:satellite,ne_type:weather)
]).
rule(space_np(s_form:F,
    sem:E^[[satellite,E],[payload_list_func,E,unk]]),[
        list_np(s_form:F,ne_tag:satellite,ne_type:other)
]).


