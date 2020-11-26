
:- multifile best_parse_cats/1, rule/2.
:- dynamic best_parse_cats/1, rule/2.

%
% Best Parse Categories
best_parse_cats([ne_np]).


%% NE_NP --> PERSON_NP
rule(ne_np(edge:Edge,source:S,s_form:F,text:T,
           sem:E^[[person,E],Sem,[name,E,F],[ne_tag,E,Edge]]),[
        person_np(s_form:F,source:S,text:T,sem:E^Sem)
]).

%%% NE_NP -> LIST_NP(person,first) CC PERSON_NP(certain)
%rule(ne_np(edge:Edge,sem:E1^E2^[[person,E1],Gender,[name,E1,F1],[realisation,E1,Edge1],
%				[person,E2],[gender,E2,G],[name,E2,F2],
%				[conj,E1,E2],[ne_tag,E1,Edge]]),[
%        list_np(s_form:F2,ne_tag:person,ne_type:person_first,gender:G),
%        cc(s_form:'and'),
%        person_np(edge:Edge1,s_form:F1,sem:E1^[Gender])
%]).

%% NE_NP --> TITLE_NP
rule(ne_np(s_form:F,source:S,sem:E^[[title,E,F],TSem]),[
        title_np(s_form:F,source:S,sem:E^TSem)
]).

%title-military person yields military
rule(ne_np(s_form:[F1,' ',F2],
       sem:E2^[[person,E2],[military,E2],[title,E2,F1],PS2]),[
       title_np(s_form:F1, sem:E1^[military,E1]),
       ne_np(s_form:F2, sem:E2^[[person,E2]|PS2])
]).
 
%title-civilian person yields civilian
rule(ne_np(s_form:[F1,' ',F2],
       sem:E2^[[person,E2],[civilian,E2],[title,E2,F1],PS2]),[
       title_np(s_form:F1, sem:E1^[civilian,E1]),
       ne_np(s_form:F2, sem:E2^[[person,E2]|PS2])
]).
%title with a country
rule(ne_np(s_form:[F1,' ',F2],
       sem:E2^EC^[[person,E2],[civilian,E2],[title,E2,F1],PS1,PS2]),[
       title_np(s_form:F1, sem:E2^EC^[[civilian,E2]|PS1]),
       ne_np(s_form:F2, sem:E2^[[person,E2]|PS2])
]).

%company title person
%this is just a kludge to handle some companies in person
rule(ne_np(s_form:[F1,' ',F2,' ',F3],
           sem:E3^E1^[OrgSem,[person,E3],[PType,E3],[name,E3,F3],Gender,
                      [certain,E3,person],[ne_tag,E3,Edge3],[qual,E3,E1]]),[
        tagged_org(s_form:F1,sem:E1^OrgSem),
        title_np(s_form:F2, sem:E2^[PType,E2]),
        person_np(edge:Edge3,s_form:F3,sem:E3^[Gender|_])
]).

rule(tagged_org(edge:Edge,s_form:F,sem:E^[[organization,E],[T,E],
		[ne_tag,E,Edge],[realisation,E,Edge]]), [
        list_np(s_form:F,ne_tag:organization,ne_type:T)
]).

% untitled + age %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% NE_NP --> PERSON_NP AGE
rule(ne_np(s_form:F,edge:Edge,sem:E^[[person,E],[name,E,F],Gender,[certain,E,person],[ne_tag,E,Edge]]),[
        person_np(edge:Edge,s_form:F,sem:E^[Gender|_]),
        age
]).

%% NE_NP --> TITLE_NP AGE
rule(ne_np(s_form:F,sem:E^[[title,E,F],TSem]),[
        title_np(s_form:F,sem:E^TSem),
        age
]).

% titled names %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% moved to end of organ grammar

% titled names + age %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% moved to end of organ grammar


% untitled names %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% PERSON_NP --> LIST_NP(person,full)
rule(person_np(s_form:F1,source:list,sem:E^[certain,E,person]),[
        list_np(s_form:F1,ne_tag:person,ne_type:person_full)
]).

% add extra feature for lone first names as person
%% PERSON_NP -> LIST_NP(person,first)
rule(person_np(s_form:F1,text:T,sem:E^[[gender,E,G],[uncertain,E,person]]),[
        list_np(s_form:F1,text:T,ne_tag:person,ne_type:person_first,gender:G)
]).

%%% PERSON_NP -> LIST_NP(person,first) N
%% catch mistagged proper nouns in header
%rule(person_np(s_form:[F1,' ',F2],sem:E^[[gender,E,G],[uncertain,E,person]]),[
%        list_np(s_form:F1,text:header,ne_tag:person,ne_type:person_first,gender:G),
%        n(s_form:F2,text:header)
%]).

%% PERSON_NP -> LIST_NP(person,first) NAMES_NP
rule(person_np(s_form:[F1,' ',F2],sem:E^[[gender,E,G]]),[
        list_np(s_form:F1,ne_tag:person,ne_type:person_first,gender:G),
        names_np(s_form:F2)
]).

%% PERSON_NP -> LIST_NP(person,first) PERSON_NP
rule(person_np(s_form:[F1,' ',F2],sem:E^[[gender,E,G]]),[
        list_np(s_form:F1,ne_tag:person,ne_type:person_first,gender:G),
        person_np(s_form:F2)
]).

%% PERSON_NP -> LIST_NP(person,first) SYM(-) PERSON_NP
rule(person_np(s_form:[F1,'-',F2],sem:E^[[gender,E,G]]),[
        list_np(s_form:F1,ne_tag:person,ne_type:person_first,gender:G),
        sym(s_form:'-'),
        person_np(s_form:F2)
]).

%% PERSON_NP -> INITIALS_NP NAMES_NP
rule(person_np(s_form:[F1,' ',F2]),[
        initials_np(s_form:F1),
        names_np(s_form:F2)
]).

%% PERSON_NP -> INITIALS_NP PERSON_NP
rule(person_np(s_form:[F1,' ',F2],sem:E^[Gender]),[
        initials_np(s_form:F1),
        person_np(s_form:F2,sem:E^[Gender|_])
]).

%% PERSON_NP -> NAMES_NP ENDING_NP
rule(person_np(s_form:[F1,' ',F2]),[
        names_np(s_form:F1),
        ending_np(s_form:F2)
]).

%% PERSON_NP -> PERSON_NP ENDING_NP
rule(person_np(s_form:[F1,' ',F2],sem:E^[Gender]),[
        person_np(s_form:F1,sem:E^[Gender|_]),
        ending_np(s_form:F2)
]).

%% PERSON_NP -> DE_NP NAMES_NP
rule(person_np(s_form:[F1,F2]),[
        de_np(s_form:F1),
        names_np(s_form:F2)
]).


% initials and nicknames %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% INITIALS_NP --> CHAR PERIOD
rule(initials_np(s_form:[F,'.']),[
        char(s_form:F),
        period(_)
]).

%% INITIALS_NP --> CHAR PERIOD INITIALS_NP
rule(initials_np(s_form:[F1,'. ',F2]),[
        char(s_form:F1),
        period(_),
        initials_np(s_form:F2)
]).

%% INITIALS_NP --> " NAMES_NP "
rule(initials_np(s_form:['"',F,'"']),[
        sym(s_form:'"'),
        names_np(s_form:F),
        sym(s_form:'"')
]).

%% INITIALS_NP --> ` NAMES_NP '
rule(initials_np(s_form:['`',F,'''']),[
        sym(s_form:'`'),
        names_np(s_form:F),
        sym(s_form:'''')
]).

%% INITIALS_NP --> ' NAMES_NP '
rule(initials_np(s_form:['''',F,'''']),[
        sym(s_form:''''),
        names_np(s_form:F),
        sym(s_form:'''')
]).

%% INITIALS_NP --> `` NAMES_NP ''
rule(initials_np(s_form:['``',F,'''''']),[
        sym(s_form:'``'),
        names_np(s_form:F),
        sym(s_form:'''''')
]).


% endings %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ENDING_NP --> PN(Jr)
rule(ending_np(s_form:S),[
	pn(m_root:'jr',s_form:S)
]).

%% ENDING_NP --> PN(Jr) PERIOD
rule(ending_np(s_form:[S,'.']),[
	pn(m_root:'jr',s_form:S),
	period(_)
]).

%% ENDING_NP --> PN(Sr)
rule(ending_np(s_form:S),[
	pn(m_root:'sr',s_form:S)
]).

%% ENDING_NP --> PN(Sr) PERIOD
rule(ending_np(s_form:[S,'.']),[
	pn(m_root:'sr',s_form:S),
	period(_)
]).

%% ENDING_NP --> PN(II)
rule(ending_np(s_form:['II']),[
	pn(s_form:'II') 
]).

%% ENDING_NP --> PN(III)
rule(ending_np(s_form:['III']),[
	pn(s_form:'III') 
]).

%% ENDING_NP --> ORDINAL (from general_ne_rules)
rule(ending_np(s_form:F),[
	ordinal(s_form:F) 
]).


% foreign middles %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% DE_NP --> PN(de)
rule(de_np(s_form:[S,' ']),[
        pn(m_root:'de',s_form:S)
]).

%% DE_NP --> PN(di)
rule(de_np(s_form:[S,' ']),[
        pn(m_root:'di',s_form:S)
]).

%% DE_NP --> PN(du)
rule(de_np(s_form:[S,' ']),[
        pn(m_root:'du',s_form:S)
]).

%% DE_NP --> PN(von)
rule(de_np(s_form:[S,' ']),[
        pn(m_root:'von',s_form:S)
]).

%% DE_NP --> JJ(von)
rule(de_np(s_form:[S,' ']),[
        jj(m_root:'von',s_form:S)
]).

%% DE_NP --> PN(van)
rule(de_np(s_form:[S,' ']),[
        pn(m_root:'van',s_form:S)
]).

%% DE_NP --> JJ(van)
rule(de_np(s_form:[S,' ']),[
        jj(m_root:'van',s_form:S)
]).

%% DE_NP --> FW(d) SYM(')
rule(de_np(s_form:[S,'''']),[
        fw(m_root:'d',s_form:S),
	sym(s_form:'''')
]).


% titles %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% TITLE_NP --> country-adj TITLE_NP
rule(title_np(s_form:[F1,' ',F2], 
  sem:E^E1^[TSem,[country,E1],[name,E1,F1],[adj,E1,F1]]),[
        list_np(s_form:F1,ne_tag:country_adj),
        title_np(s_form:F2,sem:E^TSem)
]).



rule(title_np(s_form:[F1,' ',F2,' ',F3],
              sem:E^E1^[TSem,[country,E1],[name,E1,F1],[adj,E1,F1]]),[
        list_np(s_form:F1,ne_tag:country_adj),
        names_np(s_form:F2),
        title_np(s_form:F3,sem:E^TSem)
]).

%% TITLE_NP --> LIST_NP(title)
rule(title_np(s_form:F1,source:list,sem:E^[T,E]),[
        list_np(s_form:F1,ne_tag:title,ne_type:T)
]).

%% TITLE_NP --> LIST_NP(title)
rule(title_np(s_form:[F1,' ',F2,' ',F3],sem:E^[T,E]),[
        list_np(s_form:F1,ne_tag:title),
	pn(s_form:F2),
        list_np(s_form:F3,ne_tag:title,ne_type:T)
]).

%% TITLE_NP --> LIST_NP(title) PERIOD
rule(title_np(s_form:[F1,'.'],source:list,sem:E^[T,E]),[
        list_np(s_form:F1,ne_tag:title,ne_type:T),
        period(_)
]).

%% TITLE_NP --> LIST_NP(title) IN(of) N
% eg. senior president of planning
rule(title_np(s_form:[F1,' of ',F2]),[
        list_np(s_form:F1,ne_tag:title),
	in(s_form:'of'),
	n(s_form:F2)
]).

%% TITLE_NP --> LIST_NP(secretary) IN(of) State
rule(title_np(s_form:[F1,' of ',F2],
             sem:E1^E2^[[civilian,E1],[organization,E2],[government,E2],
			[of,E1,E2],[name,E2,F2],
			[ne_tag,E2,Edge],[realisation,E2,Edge]]),[
        list_np(s_form:F1,m_root:'secretary',ne_tag:title),
	in(s_form:'of'),
	names_np(edge:Edge,s_form:F2)
]).

%% TITLE_NP --> LIST_NP(title) IN(of) JJ N
rule(title_np(s_form:[F1,' of ',F2,' ',F3]),[
        list_np(s_form:F1,ne_tag:title),
	in(s_form:'of'),
	jj(s_form:F2),
	n(s_form:F3)
]).

%% TITLE_NP --> LIST_NP(title) IN(of) JJ CC(and) JJ N
rule(title_np(s_form:[F1,' of ',F2,' and ',F3,' ',F4]),[
        list_np(s_form:F1,ne_tag:title),
	in(s_form:'of'),
	jj(s_form:F2),
	cc(s_form:'and'),
	jj(s_form:F3),
	n(s_form:F4)
]).

%% TITLE_NP --> LIST_NP(title) TITLE_NP
rule(title_np(s_form:[F1,' ',F2],sem:E^[T,E]),[
        list_np(s_form:F1,ne_tag:title,ne_type:T),
        title_np(s_form:F2)
]).

%% TITLE_NP --> LIST_NP(title) PERIOD TITLE_NP
rule(title_np(s_form:[F1,'. ',F2],sem:E^[T,E]),[
        list_np(s_form:F1,ne_tag:title,ne_type:T),
        period(_),
        title_np(s_form:F2)
]).

% utils %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% AGE --> COMMA CD TRAILING_PUNC
rule(age,[
        comma(s_form:_),
        cd(s_form:_),
        trailing_punc(s_form:_)
        ]).

% AGE --> COMMA CD 'years old' TRAILING_PUNC
rule(age,[
        comma(s_form:_),
        cd(s_form:_),
        n(s_form:'years'),
        jj(s_form:'old'),	  
        trailing_punc(s_form:_)
        ]).

rule(trailing_punc(s_form:F),[
        comma(s_form:F)
        ]).
rule(trailing_punc(s_form:F),[
        period(s_form:F)
        ]).
rule(trailing_punc(s_form:'?'),[
        sym(s_form:'?')
        ]).
rule(trailing_punc(s_form:'!'),[
        sym(s_form:'!')
        ]).

