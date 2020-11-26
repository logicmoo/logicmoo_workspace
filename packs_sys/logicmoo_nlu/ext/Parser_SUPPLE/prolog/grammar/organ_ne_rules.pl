
:- multifile best_parse_cats/1, rule/2.
:- dynamic best_parse_cats/1, rule/2.

%
% Best Parse Categories
best_parse_cats([ne_np]).


%% NE_NP --> ORGAN_NP
rule(ne_np(edge:Edge,source:S,s_form:F,
       sem:E^[[organization,E],[name,E,F],NSem,[ne_tag,E,Edge]]),
       [organ_np(s_form:F,source:S,sem:E^NSem)
]).

%% NE_NP --> ORGAN_BASE_NP of NE_NP(location)
%% don't include location name in ne_tag of org
rule(ne_np(s_form:F,sem:E1^E2^[[organization,E1],[name,E1,F],Sem,[ne_tag,E1,Edge],
			       [of,E1,E2],[location,E2]|LocSem]), [
        organ_base_np(edge:Edge,s_form:F,sem:E1^Sem),
        in(m_root:'of',s_form:F3),
        ne_np(sem:E2^[[location,E]|LocSem])
]).

%%% NE_NP --> ORGAN_NP CC PN
%%% overgenerates
%rule(ne_np(edge:Edge,s_form:[F1,' ',CC,' ',F2],
%       sem:E^[[organization,E],[name,E,[F1,' ',CC,' ',F2]],NSem,[ne_tag,E,Edge]]),[
%        organ_np(s_form:F1,sem:E^NSem),
%	conj(s_form:CC),
%	pn(s_form:F2)
%]).

%%% NE_NP --> PN CC ORGAN_NP
%%% overgenerates
%rule(ne_np(edge:Edge,s_form:[F1,' ',CC,' ',F2],
%       sem:E^[[organization,E],[name,E,[F1,' ',CC,' ',F2]],NSem,[ne_tag,E,Edge]]),[
%	pn(s_form:F2),
%	conj(s_form:CC),
%        organ_np(s_form:F1,sem:E^NSem)
%]).


% combine with adjacent locations %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ORGAN_NP --> ORGAN_NP LIST_NP(location)
rule(organ_np(s_form:[F1,' ',F2],sem:Sem),[
        organ_np(s_form:F1,sem:Sem),
        list_np(s_form:F2,ne_tag:location)
]).

%% ORGAN_NP --> LIST_NP(location) ORGAN_NP
rule(organ_np(s_form:[F1,' ',F2],sem:Sem),[
        list_np(s_form:F1,ne_tag:location),
        organ_np(s_form:F2,sem:Sem)
]).

%%% ORGAN_NP --> ORGAN_NP of LIST_NP(location,region)
%rule(organ_np(s_form:[F1,' ',F2,' ',F3],sem:Sem),[
%        organ_np(s_form:F1,sem:Sem),
%        in(m_root:'of',s_form:F2),
%        list_np(s_form:F3,ne_tag:location,ne_type:region)
%]).

%%% ORGAN_NP --> ORGAN_NP of LIST_NP(location,country)
%rule(organ_np(s_form:[F1,' ',F2,' ',F3],sem:Sem),[
%        organ_np(s_form:F1,sem:Sem),
%        in(m_root:'of',s_form:F2),
%        list_np(s_form:F3,ne_tag:location,ne_type:country)
%]).


%% ORGAN_NP --> ORDINAL ORGAN_NP
rule(organ_np(s_form:[F1,' ',F2],sem:Sem),[
        ordinal(s_form:F1),
        organ_np(s_form:F2,sem:Sem)
]).


% company designators %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ORGAN_NP --> NAMES_NP CDG_NP
rule(organ_np(s_form:[F1,' ',F2],sem:E^[company,E]),[
        names_np(s_form:F1),
        cdg_np(s_form:F2)
]).

%% ORGAN_NP --> NE_NP CDG_NP
rule(organ_np(s_form:[F1,' ',F2],sem:E^[company,E]),[
        ne_np(s_form:F1),
        cdg_np(s_form:F2)
]).

%% CDG_NP --> LIST_NP(cdg)
rule(cdg_np(s_form:F1),[
        list_np(s_form:F1,ne_tag:cdg)
        ]).

%% CDG_NP --> LIST_NP(cdg) PERIOD
rule(cdg_np(s_form:[F1,F2]),[
        list_np(s_form:F1,ne_tag:cdg),
        period(s_form:F2)
        ]).

%% CDG_NP --> CC CDG_NP
rule(cdg_np(s_form:[CC,' ',F2]),[
        conj(s_form:CC),			     
        cdg_np(s_form:F2)
        ]).

%% CDG_NP --> CDG_NP CDG_NP
rule(cdg_np(s_form:[F1,' ',F2]),[
        cdg_np(s_form:F1),
        cdg_np(s_form:F2)
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

rule(conj(s_form:'and'),[cc(s_form:'and')]).
rule(conj(s_form:'&'),[cc(s_form:'&')]).
rule(conj(s_form:'&AMP;'),[sym(s_form:'&AMP;')]).


% company keywords %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ORGAN_NP --> NAMES_NP LIST_NP(organ)
rule(organ_np(s_form:[F1,' ',F2],sem:E^[T,E]),[
        names_np(s_form:F1),
        list_np(s_form:F2,ne_tag:organization,ne_type:T)
]).

%% ORGAN_NP --> NAMES_NP LIST_NP(org_key)
rule(organ_np(s_form:[F1,' ',F2],sem:E^[company,E]),[
        names_np(s_form:F1),
        list_np(s_form:F2,ne_tag:org_key,ne_type:org_key)
]).

%% ORGAN_BASE_NP --> NAMES_NP LIST_NP(org_base)
% category required to distinguish ne_tag when followed by <of place>
rule(organ_base_np(s_form:[F1,' ',F2],sem:E^[[company,E],[R2,E]]),[
        names_np(s_form:F1),
        list_np(s_form:F2,m_root:R2,ne_tag:org_base)
]).

%% ORGAN_NP --> ORGAN_BASE_NP
rule(organ_np(s_form:F,sem:Sem),[
        organ_base_np(s_form:F,sem:Sem)
]).

%% ORGAN_NP --> NAMES_NP LIST_NP(govern_key)
rule(organ_np(s_form:[F1,' ',F2],sem:E^[[government,E],[R2,E]]),[
        names_np(s_form:F1),
        list_np(s_form:F2,m_root:R2,ne_tag:govern_key)
         ]).

%% ORGAN_NP --> LIST_NP(govern_key) NAMES_NP 
rule(organ_np(s_form:[F1,' ',F2],sem:E^[[government,E],[R2,E]]),[
        list_np(s_form:F1,m_root:R2,ne_tag:govern_key),
        names_np(s_form:F2)
         ]).

%%other organizations
%% ORGAN_NP --> NAMES_NP LIST_NP(other_key)
rule(organ_np(s_form:[F1,' ',F2],sem:E^[otherorg,E]),[
        names_np(s_form:F1),
        list_np(s_form:F2,m_root:R2,ne_tag:othorg_key)
         ]).

%% ORGAN_NP --> NAMES_NP LIST_NP(other_key)
rule(organ_np(s_form:[F1,' ',F2,' ',F3],sem:E^[otherorg,E]),[
        names_np(s_form:F1),
        list_np(s_form:F2,m_root:R2,ne_tag:othorg_key),
        cd(s_form:F3)
         ]).

%% ORGAN_NP --> NAMES_NP LIST_NP(other_key)
rule(organ_np(s_form:[F2,' ',F3],sem:E^[otherorg,E]),[
        list_np(s_form:F2,m_root:R2,ne_tag:othorg_key),
        cd(s_form:F3)
         ]).

%% ORGAN_NP --> NAMES_NP LIST_NP(org_key,airline)
rule(organ_np(s_form:[F1,' ',F2],sem:E^[[company,E],[airline,E]]),[
        names_np(s_form:F1),
        list_np(s_form:F2,ne_tag:org_key,ne_type:airline)
         ]).

%% ORGAN_NP --> LIST_NP(org_key,airline) NAMES_NP 
rule(organ_np(s_form:[F1,' ',F2],sem:E^[[company,E],[airline,E]]),[
        list_np(s_form:F1,ne_tag:org_key,ne_type:airline),
        names_np(s_form:F2)
         ]).


% of/for PPs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ORGAN_NP --> LIST_NP(org_base) of NAMES_NP
rule(organ_np(s_form:[F2,' ',F3,' ',F4],sem:E^[[company,E],[R2,E]]),[
        list_np(s_form:F2,m_root:R2,ne_tag:org_base),
        in(m_root:'of',s_form:F3),
        names_np(s_form:F4)
]).

%% ORGAN_NP --> LIST_NP(org_key) of NAMES_NP
rule(organ_np(s_form:[F2,' ',F3,' ',F4],sem:E^[[company,E],[R2,E]]),[
        list_np(s_form:F2,m_root:R2,ne_tag:org_key),
        in(m_root:'of',s_form:F3),
        names_np(s_form:F4)
]).

%% ORGAN_NP --> LIST_NP(org_base) for NAMES_NP
rule(organ_np(s_form:[F2,' ',F3,' ',F4],sem:E^[[company,E],[R2,E]]),[
        list_np(s_form:F2,m_root:R2,ne_tag:org_base),
        in(m_root:'for',s_form:F3),
        names_np(s_form:F4)
]).

%% ORGAN_NP --> LIST_NP(org_base) for the NAMES_NP
rule(organ_np(s_form:[F2,' ',F3,' ',F4,' ',F5],sem:E^[[company,E],[R2,E]]),[
        list_np(s_form:F2,m_root:R2,ne_tag:org_base),
        in(m_root:'for',s_form:F3),
        dt(m_root:'the',s_form:F4),
        names_np(s_form:F5)
]).

%% ORGAN_NP --> NAMES_NP LIST_NP(org_base) of NAMES_NP
rule(organ_np(s_form:[F1,' ',F2,' ',F3,' ',F4],sem:E^[[company,E],[R2,E]]),[
        names_np(s_form:F1),
        list_np(s_form:F2,m_root:R2,ne_tag:org_base),
        in(m_root:'of',s_form:F3),
        names_np(s_form:F4)
]).

%% ORGAN_NP --> NAMES_NP LIST_NP(org_key) of NAMES_NP
rule(organ_np(s_form:[F1,' ',F2,' ',F3,' ',F4],sem:E^[[company,E],[R2,E]]),[
        names_np(s_form:F1),
        list_np(s_form:F2,m_root:R2,ne_tag:org_key),
        in(m_root:'of',s_form:F3),
        names_np(s_form:F4)
]).

%% ORGAN_NP --> NAMES_NP LIST_NP(org_base) for NAMES_NP
rule(organ_np(s_form:[F1,' ',F2,' ',F3,' ',F4],sem:E^[[company,E],[R2,E]]),[
        names_np(s_form:F1),
        list_np(s_form:F2,m_root:R2,ne_tag:org_base),
        in(m_root:'for',s_form:F3),
        names_np(s_form:F4)
]).

%% ORGAN_NP --> NAMES_NP LIST_NP(org_base) for the NAMES_NP
rule(organ_np(s_form:[F1,' ',F2,' ',F3,' ',F4,' ',F5],sem:E^[[company,E],[R2,E]]),[
        names_np(s_form:F1),
        list_np(s_form:F2,m_root:R2,ne_tag:org_base),
        in(m_root:'for',s_form:F3),
        dt(m_root:'the',s_form:F4),
        names_np(s_form:F5)
]).


% ampersands %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ORGAN_NP --> NAMES_NP & NAMES_NP
rule(organ_np(s_form:[F1,' & ',F2],sem:E^[company,E]),[
        names_np(s_form:F1),
        cc(s_form:'&'),
        names_np(s_form:F2)
]).

%% ORGAN_NP --> NAMES_NP &AMP; NAMES_NP
rule(organ_np(s_form:[F1,' &AMP; ',F2],sem:E^[company,E]),[
        names_np(s_form:F1),
        sym(s_form:'&AMP;'),
        names_np(s_form:F2)
]).


% direct from gazetteer %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ORGAN_NP --> LIST_NP(organ)
rule(organ_np(s_form:F1,source:list,sem:E^[T,E]),[
        list_np(s_form:F1,ne_tag:organization,ne_type:T)
]).


% unclassify anything explicitly listed as a non-markable

%% NE_NP --> LIST_NP(others)
% non-markable proper name
rule(ne_np(s_form:F,source:list,sem:E^[[name,E,F],[ne_mark,E,no]]),
        [list_np(s_form:F,ne_tag:others)]).
 
%% NE_NP --> NAMES_NP LIST_NP(others)
% non-markable proper name
rule(ne_np(s_form:F,sem:E^[[name,E,[F1,' ',F2]],[ne_mark,E,no]]),[
        names_np(s_form:F1),
        list_np(s_form:F2,ne_tag:others)]).
 

% titled names %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% NE_NP --> TITLE_NP NAMES_NP
rule(ne_np(s_form:F2,
           sem:E^[[person,E],TSem,[name,E,F2],[title,E,F1],[ne_tag,E,Edge]]),[
       	title_np(s_form:F1,sem:E^TSem),
        names_np(edge:Edge,s_form:F2) % exclude title from span
]).

%% NE_NP --> TITLE_NP PERSON_NP
rule(ne_np(s_form:F2,
           sem:E^[[person,E],Gender,TSem,[name,E,F2],[title,E,F1],[ne_tag,E,Edge]]),[
       	title_np(s_form:F1,sem:E^TSem),
        person_np(edge:Edge,s_form:F2,sem:E^[Gender|_]) % exclude title
]).

% titled names + age %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% NE_NP --> TITLE_NP NAMES_NP AGE
rule(ne_np(s_form:F2,
           sem:E^[[person,E],TSem,[name,E,F2],[title,E,F1],[ne_tag,E,Edge]]),[
       	title_np(s_form:F1,sem:E^TSem),
        names_np(edge:Edge,s_form:F2), % exclude title from span
        age
]).

%% NE_NP --> TITLE_NP PERSON_NP AGE
rule(ne_np(s_form:F2,
           sem:E^[[person,E],TSem,[name,E,F2],[title,E,F1],Gender,[ne_tag,E,Edge]]),[
       	title_np(s_form:F1,sem:E^TSem),
        person_np(edge:Edge,s_form:F2,sem:E^[Gender|_]), % exclude title
        age
]).


