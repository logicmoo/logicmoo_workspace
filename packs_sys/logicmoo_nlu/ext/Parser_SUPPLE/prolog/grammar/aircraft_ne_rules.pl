
:- multifile best_parse_cats/1, rule/2.
:- dynamic best_parse_cats/1, rule/2.

best_parse_cats([ne_np]).


% aircraft %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Boeing 707
rule(ne_np(edge:Edge,s_form:F,number:N,sem:
     E^E1^[[aircraft,E],[name,E,F],[ne_tag,E,Edge],[number,E,N]|MSem]), [
        aircraft_np(s_form:F,number:N,sem:E^E1^MSem)
]).

% 747
rule(ne_np(edge:Edge,source:list,s_form:F,number:N,sem:
     E^[[aircraft,E],[name,E,F],[ne_tag,E,Edge],[number,E,N]]), [
        aircraft_name(s_form:F,number:N)
]).
 
% de Havilland - company
rule(ne_np(s_form:F,source:list,number:N,sem:Sem),[
        aircraft_manufacturer(s_form:F,number:N,sem:Sem)
]).


% Boeing 757
rule(aircraft_np(s_form:[F1,' ',F2],number:N,
       sem:E^E1^[[qual,E,E1],MSem]),[
        aircraft_manufacturer(s_form:F1,sem:E1^MSem),
	cd(s_form:F2)
]).

% Airbus A300
rule(aircraft_np(s_form:[F1,' ',F2],number:N,
       sem:E^E1^[[qual,E,E1],MSem]),[
        aircraft_manufacturer(s_form:F1,sem:E1^MSem),
	aircraft_name(s_form:F2,number:N)
]).


% aircraft manufacturer (from list) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rule(aircraft_manufacturer(edge:Edge,s_form:F1,
     sem:E^[[organization,E],[company,E],[aircraft_manufacturer,E],[name,E,F1],[ne_tag,E,Edge]]),[
        list_np(s_form:F1,ne_tag:artifact,ne_type:manufacturer)
]).
rule(aircraft_manufacturer(edge:Edge,s_form:[F1,' ',F2],
     sem:E^[[organization,E],[company,E],[aircraft_manufacturer,E],[name,E,[F1, ' ',F2]],[ne_tag,E,Edge]]),[
        list_np(s_form:F1,ne_tag:artifact,ne_type:manufacturer),
        list_np(s_form:F2,ne_tag:cdg)
]).


% aircraft names (from list) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A300
rule(aircraft_name(s_form:F,number:sing),[
        list_np(s_form:F,ne_tag:artifact,ne_type:aircraft) ]).

% F-14 Tomcat
rule(aircraft_name(s_form:[F1,' ',F2],number:sing),[
        list_np(s_form:F1,ne_tag:artifact,ne_type:aircraft),
        list_np(s_form:F2,ne_tag:artifact,ne_type:aircraft) ]).


% 747-400
rule(aircraft_name(s_form:[F1,'-',F2],number:sing),[
        list_np(s_form:F1,ne_tag:artifact,ne_type:aircraft),
        sym(s_form:'-'),
        cd(s_form:F2) ]).

% F-14A
rule(aircraft_name(s_form:[F1,F2],number:sing),[
        list_np(s_form:F1,ne_tag:artifact,ne_type:aircraft),
        char(s_form:F2) ]).

% F-14s
rule(aircraft_name(s_form:F,number:plural),[
        aircraft_name(s_form:F),
        prp(s_form:'s') ]).

% F-14S
rule(aircraft_name(s_form:F,number:plural),[
        aircraft_name(s_form:F),
        pn(s_form:'S') ]).


% flight numbers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Flight 123
rule(ne_np(edge:Edge,s_form:F,sem:E^[[flight,E],[name,E,F],[ne_tag,E,Edge]]), [
        flight_np(s_form:F)
]).

rule(flight_np(s_form:['Flight ',F]), [
        pn(s_form:'Flight'),
        cd(s_form:F)
]).				      


%%%%%%%%%%%%% ships (crh)
rule(ne_np(edge:Edge,s_form:F,sem:E^[[boat,E],[name,E,F],[ne_tag,E,Edge]]), [
        ship_np(s_form:F)
]).

%USS Kitty Hawk
rule(ship_np(s_form:['USS ',F2]), [
        pn(s_form:'USS'),
	names_np(s_form:F2)
]).
%HMS Invincible
rule(ship_np(s_form:['HMS ',F2]), [
        pn(s_form:'HMS'),
	names_np(s_form:F2)
]).


