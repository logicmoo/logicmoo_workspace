%
% Measure grammar
%
:- multifile best_parse_cats/1, rule/2.
:- dynamic best_parse_cats/1, rule/2.

%
% Best Parse Categories
best_parse_cats([ne_np]).


rule(ne_np(edge:Edge,s_form:[F1,' ',F2],
     sem:E^[[measure,E],[measure_type,E,T],[count,E,F1],[unit,E,F2],[ne_tag,E,Edge]]), [
    cd_np(s_form:F1),
    unit_np(s_form:F2,sem:T)
]).

rule(ne_np(edge:Edge,s_form:[F1,'-',F2],
     sem:E^[[measure,E],[measure_type,E,T],[count,E,F1],[unit,E,F2],[ne_tag,E,Edge]]), [
    cd_np(s_form:F1),
    sym(s_form:'-'),
    unit_np(s_form:F2,sem:T)
]).

rule(ne_np(edge:Edge,s_form:[F1,' ',F2],
     sem:E^[[measure,E],[measure_type,E,T],[count,E,F2],[unit,E,F1],[ne_tag,E,Edge]]), [
    list_np(s_form:F1,m_root:'ph',ne_tag:unit,ne_type:T),
    cd_np(s_form:F2)
]).

rule(cd_np(s_form:F1), [
    cd(s_form:F1)
]).

rule(cd_np(s_form:[F1,' ',F2]), [
    cd(s_form:F1),
    cd(s_form:F2)
]).

rule(cd_np(s_form:[F1,'.',F2]), [
    cd(s_form:F1),
    period(s_form:'.'),
    cd(s_form:F2)
]).

rule(cd_np(s_form:[F1,',',F2]), [
    cd(s_form:F1),
    period(s_form:','),
    cd(s_form:F2)
]).

rule(cd_np(s_form:[F1,'-',F2]), [
    cd(s_form:F1),
    sym(s_form:'&ndash;'),
    cd(s_form:F2)
]).

rule(cd_np(s_form:[F1,'-',F2]), [
    cd(s_form:F1),
    sym(s_form:'&plusmn;'),
    cd(s_form:F2)
]).

rule(cd_np(s_form:[F1,'-',F2]), [
    cd(s_form:F1),
    sym(s_form:'&'),
    n(s_form:'ndash'),
    sym(s_form:';'),
    cd(s_form:F2)
]).

rule(cd_np(s_form:[F1,' ',F2,' ',F3]), [
    cd(s_form:F1),
    in(s_form:F2,m_root:'to'),
    cd(s_form:F3)
]).


% SAM -> Take care of "4th", etc.
rule(unit_np(s_form:'th'), [
    dt(s_form:'th')
]).

rule(unit_np(s_form:'st'), [
    n(s_form:'st')
]).

rule(unit_np(s_form:'nd'), [
    cc(s_form:'nd')
]).

rule(unit_np(s_form:'%',sem:percentage), [
    sym(s_form:'%')
]).


rule(unit_np(s_form:F1,sem:Type), [
    list_np(s_form:F1,ne_tag:unit,ne_type:Type)
]).

rule(unit_np(s_form:[F1,'/',F2],sem:Type), [
    unit_np(s_form:F1),
    sym(s_form:'/'),
    list_np(s_form:F2,ne_tag:unit,ne_type:Type)
]).

rule(unit_np(s_form:[F1,' per ',F2],sem:Type), [
    unit_np(s_form:F1),
    n(s_form:'per'),
    list_np(s_form:F2,ne_tag:unit,ne_type:Type)
]).

rule(unit_np(s_form:[F1,'-',F2],sem:Type), [
    list_np(s_form:F1,ne_tag:unit,ne_type:Type),
    sym(s_form:'&minus;'),
    cd_np(s_form:F2)
]).

rule(unit_np(s_form:[F1,'-',F2],sem:Type), [
    list_np(s_form:F1,ne_tag:unit,ne_type:Type),
    sym(s_form:'&'),
    n(s_form:'minus'),
    sym(s_form:';'),
    cd_np(s_form:F2)
]).

