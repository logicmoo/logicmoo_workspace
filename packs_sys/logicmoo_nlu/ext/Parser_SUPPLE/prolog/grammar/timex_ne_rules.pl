
:- multifile best_parse_cats/1, filter_chart/0, rule/2.
:- dynamic best_parse_cats/1, filter_chart/0, rule/2.

best_parse_cats([ne_np,bnp,cd_np]).

filter_chart.


%% BNP --> DATEX_NP
rule(bnp(edge:Edge,source:S,sem:E^[[date,E],Sem,[ne_tag,E,Edge]]),
	[datex_np(sem:E^Sem,source:S)]).

%% NE_NP --> DATE_NUM_NP
rule(ne_np(edge:Edge,source:S,sem:E^[[date,E],Sem,[ne_tag,E,Edge]]),
	[date_num_np(sem:E^Sem,source:S)]).

%% NE_NP --> TIMEX_NP
rule(ne_np(edge:Edge,sem:E^[[time,E],Sem,[ne_tag,E,Edge],[realisation,E,Edge]]),
	[timex_np(sem:E^Sem)]).

%% NE_NP --> DATE_CORE TIME_OF_DAY
rule(ne_np(sem:E2^E1^[[date,E1],Sem1,[ne_tag,E1,Edge1],
		      [time,E2],Sem2,[ne_tag,E2,Edge2],[qual,E2,E1]]),
     [date_core(edge:Edge1,sem:E1^Sem1),
      time_of_day(edge:Edge2,sem:E2^Sem2)
     ]).

rule(time_of_day(sem:E^[morning,E]),
     [n(m_root:'morning')]).
rule(time_of_day(sem:E^[afternoon,E]),
     [n(m_root:'afternoon')]).
rule(time_of_day(sem:E^[evening,E]),
     [n(m_root:'evening')]).
rule(time_of_day(sem:E^[night,E]),
     [n(m_root:'night')]).

%% DATEX_NP --> DATE_PRE DATE_CORE DATE_POST
rule(datex_np(s_form:[F1,' ',F2,' ',F3],
              sem:E2^E3^[Sem2,Sem1,Sem3,[of,E2,E3]]),[
	date_pre(s_form:F1,sem:E2^Sem1),
	date_core(s_form:F2,sem:E2^Sem2),
	date_post(s_form:F3,sem:E3^Sem3)
	]).
     					  
%% DATEX_NP --> DATE_PRE DATE_CORE
rule(datex_np(s_form:[F1,' ',F2],sem:E2^[Sem2,Sem1]),[
	date_pre(s_form:F1,sem:E2^Sem1),
	date_core(s_form:F2,sem:E2^Sem2)
	]).
     					  
%% DATE_NUM_NP --> DATE_CORE DATE_POST
rule(date_num_np(s_form:[F1,' ',F2],sem:E2^E3^[Sem2,Sem3,[of,E2,E3]]),[
	date_core(s_form:F1,sem:E2^Sem2),
	date_post(s_form:F2,sem:E3^Sem3)
	]).
     					  
%% DATE_NUM_NP --> DATE_CORE
rule(date_num_np(s_form:F1,source:S,sem:Sem),[
	date_core(s_form:F1,source:S,sem:Sem)
	]).

%% DATEX_NP --> DATE_PRE DATE_PARTIAL_CORE DATE_POST
rule(datex_np(s_form:[F1,' ',F2,' ',F3],
              sem:E2^E3^[Sem2,Sem1,Sem3]),[
	date_pre(s_form:F1,sem:E2^Sem1),
	date_partial_core(s_form:F2,sem:E2^Sem2),
	date_post(s_form:F3,sem:E3^Sem3)
	]).
     					  
%% DATEX_NP --> DATE_PRE DATE_PARTIAL_CORE
rule(datex_np(s_form:[F1,' ',F2],sem:E2^[Sem2,Sem1]),[
	date_pre(s_form:F1,sem:E2^Sem1),
	date_partial_core(s_form:F2,sem:E2^Sem2)
	]).
     					  
%% DATEX_NP --> DATE_PARTIAL_CORE DATE_POST
rule(datex_np(s_form:[F1,' ',F2],sem:E1^E2^[Sem2,Sem3]),[
	date_partial_core(s_form:F1,sem:E1^Sem1),
	date_post(s_form:F2,sem:E2^Sem2)
	]).
     					  
%% DATEX_NP --> DATEX_NP DATE_PARTIAL_CORE
rule(datex_np(s_form:[F1,' ',F2],sem:E1^E2^[Sem2,Sem3]),[
	datex_np(s_form:F1,sem:E1^Sem1),
	date_partial_core(s_form:F2,sem:E2^Sem2)
	]).

%% DATEX_NP --> DT(a) LIST_NP(nonspec_date) DATE_POST
rule(datex_np(edge:Edge,s_form:[F1,' ',F2,' ',F3],
              sem:E^E3^[[R2,E],[det,E,'a'],Sem3,[realisation,E,Edge]]),[
	dt(s_form:F1,m_root:'a'),
	list_np(s_form:F2,m_root:R2,ne_tag:non_specific_date),
	date_post(s_form:F3,sem:E3^Sem3)
	]).

%% DATEX_NP --> DT(an) LIST_NP(nonspec_date) DATE_POST
rule(datex_np(edge:Edge,s_form:[F1,' ',F2,' ',F3],
              sem:E^E3^[[R2,E],[det,E,'a'],Sem3,[realisation,E,Edge]]),[
	dt(s_form:F1,m_root:'an'),
	list_np(s_form:F2,m_root:R2,ne_tag:non_specific_date),
	date_post(s_form:F3,sem:E3^Sem3)
	]).

%% DATEX_NP --> LIST_NP(date_pre) CD LIST_NP(date_post)
rule(datex_np(edge:Edge,s_form:[F1,' ',F2,' ',F3],
              sem:E3^[[R3,E3],[adj,E3,R1],[count,E3,F2],[realisation,E3,Edge]]),[
        list_np(s_form:F1,m_root:R1,ne_tag:date_pre),
	cd(s_form:F2),
	list_np(s_form:F3,m_root:R3,ne_tag:date_post)
	]).

%% DATEX_NP --> DT(the) LIST_NP(date_pre) CD LIST_NP(date_post)
rule(datex_np(edge:Edge,s_form:[F1,' ',F2,' ',F3,' ',F4],
              sem:E4^[[R4,E4],[det,E4,'the'],[adj,E4,R2],[count,E4,F3],[realisation,E4,Edge]]),[
	dt(s_form:F1,m_root:'the'),
        list_np(s_form:F2,m_root:R2,ne_tag:date_pre),
	cd(s_form:F3),
	list_np(s_form:F4,m_root:R4,ne_tag:date_post)
	]).

% DATEX_NP --> DT(the) LIST_NP(timex_pre) LIST_NP(date_post)
rule(datex_np(edge:Edge,s_form:[F1,' ',F2,' ',F3],
              sem:E3^[[R3,E3],[det,E3,'the'],[adj,E3,R2],[realisation,E3,Edge]]),[
	dt(s_form:F1,m_root:'the'),
        list_np(s_form:F2,m_root:R2,ne_tag:timex_pre),
	list_np(s_form:F3,m_root:R3,ne_tag:date_post)
	]).

%% DATEX_NP --> IN(for) DT(the) LIST_NP(date_pre) CD LIST_NP(date_post)
rule(datex_np(edge:Edge,s_form:[F1,' ',F2,' ',F3,' ',F4,' ',F5],
	      sem:P^E5^[[R5,E5],[det,E5,'the'],[adj,E5,R3],[count,E5,F4],[for,P,E5],[realisation,E5,Edge]]),[
	in(s_form:F1,m_root:'for'),
	dt(s_form:F2,m_root:'the'),
        list_np(s_form:F3,m_root:R3,ne_tag:date_pre),
	cd(s_form:F4),
	list_np(s_form:F5,m_root:R5,ne_tag:date_post)
	]).

%% DATEX_NP --> IN(for) DT(the) LIST_NP(timex_pre) LIST_NP(date_post)
rule(datex_np(edge:Edge,s_form:[F1,' ',F2,' ',F3,' ',F4],
	      sem:P^E4^[[R4,E4],[det,E4,'the'],[adj,E4,R3],[for,P,E4],[realisation,E4,Edge]]),[
	in(s_form:F1,m_root:'for'),
	dt(s_form:F2,m_root:'the'),
        list_np(s_form:F3,m_root:R3,ne_tag:timex_pre),
	list_np(s_form:F4,m_root:R4,ne_tag:date_post)
	]).

%% DATEX_NP --> LIST_NP(timex_pre) CD LIST_NP(timespan)
rule(datex_np(edge:Edge,s_form:[F1,' ',F2,' ',F3],
	      sem:E3^[[R3,E3],[adj,E3,R1],[count,E3,F2],[realisation,E3,Edge]]),[
        list_np(s_form:F1,m_root:R1,ne_tag:timex_pre),
	cd(s_form:F2),
        list_np(s_form:F3,m_root:R3,ne_tag:timespan,ne_type:datespan)
	]).

%% DATEX_NP --> LIST_NP(timex_pre) LIST_NP(non_specific_date)
rule(datex_np(edge:Edge,s_form:[F1,' ',F2],
	      sem:E2^[[R2,E2],[adj,E2,R1],[realisation,E2,Edge]]),[
	list_np(s_form:F1,m_root:R1,ne_tag:timex_pre),
	list_np(s_form:F2,m_root:R2,ne_tag:non_specific_date)
	]).

%% DATEX_NP --> LIST_NP(timex_pre) LIST_NP(timex_pre) LIST_NP(non_specific_date)
rule(datex_np(edge:Edge,s_form:[F1,' ',F2,' ',F3],
	      sem:E3^[[R3,E3],[adj,E3,R1],[adj,E3,R2],[realisation,E3,Edge]]),[
	list_np(s_form:F1,m_root:R1,ne_tag:timex_pre),
	list_np(s_form:F2,m_root:R2,ne_tag:timex_pre),
	list_np(s_form:F3,m_root:R3,ne_tag:non_specific_date)
	]).

%% DATEX_NP --> FROM DATE TO DATE
rule(datex_np(s_form:[F1,' ',F2,' ',F3,' ',F4],
	      sem:P^E2^E4^[Sem2,[from,P,E2],Sem4,[R3,P,E4]]),[
        in(s_form:F1,m_root:'from'),
        date_core(s_form:F2,sem:E2^Sem2),
        to(s_form:F3,m_root:R3),
        date_core(s_form:F4,sem:E4^Sem4)
        ]).
/*
%% DATEX_NP --> "IN" DATE CC(AND) DATE_LIST
rule(datex_np(s_form:[F1,' ',F2,' ',F3,' ',F4]),[
        in(s_form:F1,m_root:'in'),
        date_core(s_form:F2),
        cc(s_form:F3,m_root:'and'),
        date_list(s_form:F4)
        ]).
*/

%% DATEX_NP --> CD_SEQ DATE_POST LIST_NP(timex_trailer)
rule(datex_np(edge:Edge,s_form:[F1,' ',F2,' ',F3],
	      sem:E2^[Sem2,[adj,E2,R3],[count,E2,F1],[realisation,E2,Edge]]),[
	cd_seq(s_form:F1),
	date_post(s_form:F2,sem:E2^Sem2),
        list_np(s_form:F3,m_root:R3,ne_tag:timex,ne_type:trailer)
	]).


%% DATEX_NP --> LIST_NP(timex_pre) CD_SEQ DATE_POST
rule(datex_np(edge:Edge,s_form:[F1,' ',F2,' ',F3],
	      sem:E3^[Sem3,[adj,E3,R1],[count,E3,F2],[realisation,E3,Edge]]),[
	list_np(s_form:F1,m_root:R1,ne_tag:timex_pre),
	cd_seq(s_form:F2),
	date_post(s_form:F3,sem:E3^Sem3)
	]).

%% DATEX_NP --> DT(the) LIST_NP(timex_pre) CD_SEQ DATE_POST
rule(datex_np(edge:Edge,s_form:[F1,' ',F2,' ',F3,' ',F4],
	      sem:E4^[Sem4,[adj,E4,R2],[count,E4,F3],[det,E4,'the'],[realisation,E4,Edge]]),[
	dt(s_form:F1,m_root:'the'),
	list_np(s_form:F2,m_root:R2,ne_tag:timex_pre),
	cd_seq(s_form:F3),
	date_post(s_form:F4,sem:E4^Sem4)
	]).

%% DATEX_NP --> IN_RESTRICTED CD_SEQ DATE_POST
rule(datex_np(edge:Edge,s_form:[F1,' ',F2,' ',F3],
	      sem:P^E3^[Sem3,[count,E3,F2],[R1,P,E3],[realisation,E3,Edge]]),[
	in_restricted(s_form:F1,m_root:R1),
	cd_seq(s_form:F2),
	date_post(s_form:F3,sem:E3^Sem3)
	]).

%% DATEX_NP --> IN_RESTRICTED LIST_NP(timex_pre) CD_SEQ DATE_POST
rule(datex_np(edge:Edge,s_form:[F1,' ',F2,' ',F3,' ',F4],
	      sem:P^E4^[Sem4,[adj,E4,R2],[count,E4,F3],[R1,P,E4],[realisation,E4,Edge]]),[
	in_restrcited(s_form:F1,m_root:R1),
	list_np(s_form:F2,m_root:R2,ne_tag:timex_pre),
	cd_seq(s_form:F3),
	date_post(s_form:F4,sem:E4^Sem4)
	]).

%% DATEX_NP --> THIS_THAT LIST_NP(non_specific_date)
rule(date_partial_core(edge:Edge,s_form:[F1,' ',F2],
                       sem:E^[[R2,E],[det,E,R1],[realisation,E,Edge]]),[
	this_that(s_form:F1,m_root:R1),
	list_np(s_form:F2,m_root:R2,ne_tag:non_specific_date)
	]).

%% DATEX_NP --> LIST_NP(timex_pre) THIS_THAT LIST_NP(non_specific_date)
rule(date_partial_core(edge:Edge,s_form:[F1,' ',F2],
                       sem:E^[[R3,E],[det,E,R2],[adj,E,R1],[realisation,E,Edge]]),[
	list_np(s_form:F1,m_root:R1,ne_tag:timex_pre),
	this_that(s_form:F2,m_root:R2),
	list_np(s_form:F3,m_root:R3,ne_tag:non_specific_date)
	]).
/*
%% DATEX_NP --> "THAT TIME"
rule(datex_np(s_form:[F1,' ',F2]),[
	dt(s_form:F1,m_root:'that'),
	n(s_form:F2,m_root:'time')
	]).
*/
%%% DATEX_NP --> CD HYPHEN LIST_NP(non_specific_date)
%rule(datex_np(s_form:[F1,' ','-',' ',F2]),[
%	cd(s_form:F1),
%	sym(s_form:'-'),
%	list_np(s_form:F2,ne_tag:non_specific_date)
%	]).

%% DATEX_NP --> DATEX_NP DATEX_NP
rule(datex_np(s_form:[F1,' ',F2],sem:E1^E2^[Sem1,Sem2]),[
	datex_np(s_form:F1,sem:E1^Sem1),
	datex_np(s_form:F2,sem:E2^Sem2)
	]).

%% DATE_NUM_NP --> DATE_NUM_NP DATE_NUM_NP
rule(date_num_np(s_form:[F1,' ',F2],sem:E1^E2^[Sem1,Sem2]),[
	date_num_np(s_form:F1,sem:E1^Sem1),
	date_num_np(s_form:F2,sem:E2^Sem2)
	]).

%% DATEX_NP --> DATEX_NP DATE_PARTIAL_CORE
rule(datex_np(s_form:[F1,' ',F2],sem:E1^E2^[Sem1,Sem2,[realisation,E2,Edge]]),[
	datex_np(s_form:F1,sem:E1^Sem1),
	date_partial_core(edge:Edge,s_form:F2,sem:E2^Sem2)
	]).

%% DATE_NUM_NP --> DATE_NUM_NP IN(of) DATE_NUM_NP
rule(date_num_np(s_form:[F1,' ',F2,' ',F3],sem:E1^E3^[Sem1,Sem2,[of,E1,E3]]),[
	date_num_np(s_form:F1,sem:E1^Sem1),
	in(s_form:F2,m_root:'of'),
	date_num_np(s_form:F3,sem:E3^Sem3)
	]).

%% DATEX_NP --> DATEX_NP IN(of)	DATEX_NP
rule(datex_np(s_form:[F1,' ',F2,' ',F3],sem:E1^E3^[Sem1,Sem2,[of,E1,E3]]),[
	datex_np(s_form:F1,sem:E1^Sem1),
	in(s_form:F2,m_root:'of'),
	datex_np(s_form:F3,sem:E3^Sem3)
	]).

%% DATEX_NP --> IN(for) DATE_PARTIAL_CORE
rule(datex_np(s_form:[F1,' ',F2],sem:P^E2^[Sem2,[for,P,E2],[realisation,E2,Edge]]),[
	in(s_form:F1,m_root:'for'),
	date_partial_core(edge:Edge,s_form:F2,sem:E2^Sem2)
	]).

%% DATEX_NP --> JJ(first) DATE_PARTIAL_CORE IN(of) DATEX_NP
rule(datex_np(s_form:[F1,' ',F2,' ',F3,' ',F4],
	      sem:E2^E4^[Sem2,[adj,E2,'first'],Sem4,[of,E2,E4]]),[
	jj(s_form:F1,m_root:'first'),
	date_partial_core(s_form:F2,sem:E2^Sem2),
	in(s_form:F3,m_root:'of'),
	date_partial_core(s_form:F4,sem:E4^Sem4)
	]).

%% DATEX_NP --> JJ(last) DATEX_NP IN(of) DATEX_NP
rule(datex_np(s_form:[F1,' ',F2,' ',F3,' ',F4],
	      sem:E2^E4^[Sem2,[adj,E2,'last'],Sem4,[of,E2,E4]]),[
	jj(s_form:F1,m_root:'last'),
	datex_np(s_form:F2,sem:E2^Sem2),
	in(s_form:F3,m_root:'of'),
	datex_np(s_form:F4,sem:E4^Sem4)
	]).

%% DATE_PRE --> DT(the) DATE_PRE
rule(date_pre(s_form:F2,sem:E^[[det,E,'the']|Sem]),[
	dt(s_form:F1,m_root:'the'),
	date_pre(s_form:F2,sem:E^Sem)
	]).

%% DATE_PRE --> LIST_NP(date_pre)
rule(date_pre(s_form:F1,sem:E^[adj,E,R1]),[
	list_np(s_form:F1,m_root:R1,ne_tag:date_pre)
	]).

%% DATE_PRE --> LIST_NP(timex_pre)
rule(date_pre(s_form:F1,sem:E^[adj,E,R1]),[
	list_np(s_form:F1,m_root:R1,ne_tag:timex_pre)
	]).

%% DATE_POST --> LIST_NP(date_post)
rule(date_post(s_form:F1,sem:E^[R1,E]),[
	list_np(s_form:F1,m_root:R1,ne_tag:date_post)
	]).

%% DATE_POST --> LIST_NP(date_post) LIST_NP(timex_trailer)a
rule(date_post(s_form:[F1,' ',F2],sem:E^[[R1,E],[adj,E,R2]]),[
	list_np(s_form:F1,m_root:R1,ne_tag:date_post),
        list_np(s_form:F2,m_root:R2,ne_tag:timex,ne_type:trailer)
	]).

%% DATE_POST --> LIST_NP(year)
rule(date_post(s_form:F1,sem:Sem),[
        year_np(s_form:F1,sem:Sem)				    
	]).

%% DATE_POST --> COMMA LIST_NP(year)
rule(date_post(s_form:[F1,' ',F2],sem:Sem),[
	comma(s_form:F1),
        year_np(s_form:F2,sem:Sem)				    
	]).

%% DATE_POST --> LIST_NP(timex_trailer)
rule(date_post(s_form:F1),[
	list_np(s_form:F1,ne_tag:timex,ne_type:trailer)
	]).

%%% qq
%%% DATE_CORE --> DATE
%rule(date_core(edge:Edge,s_form:F1,sem:E^[[name,E,F1],[realisation,E,Edge]]),[
%	date(s_form:F1)
%	]).


%% DATE_CORE --> LIST_NP(date)
rule(date_core(s_form:F1,source:list,sem:Sem),[
	tagged_date(s_form:F1,sem:Sem)
	]).

%% DATE_CORE --> LIST_NP(timex_pre) LIST_NP(day)
rule(date_core(sem:E^[Sem,[adj,E,R1]]),[
	list_np(s_form:F1,m_root:R1,ne_tag:timex_pre),
	day_np(sem:E^Sem)
	]).

%% DATE_CORE --> DT(this) LIST_NP(non_specific_date)
rule(date_core(edge:Edge,s_form:[F1,' ',F2],
               sem:E^[[R2,E],[det,E,'this'],[realisation,E,Edge]]),[
	dt(s_form:F1,m_root:'this'),
	list_np(s_form:F2,m_root:R2,ne_tag:non_specific_date)
	]).

%% DATE_CORE --> LIST_NP(timex_pre) DT(this) LIST_NP(non_specific_date)
rule(date_core(edge:Edge,s_form:[F1,' ',F2],
               sem:E^[[R3,E],[det,E,'this'],[adj,E,R1],[realisation,E,Edge]]),[
	list_np(s_form:F1,m_root:R1,ne_tag:timex_pre),
	dt(s_form:F2,m_root:'this'),
	list_np(s_form:F3,m_root:R3,ne_tag:non_specific_date)
	]).

%% DATE_CORE --> LIST_NP(festival) LIST_NP(date_post)
rule(date_core(s_form:[F1,' ',F2],sem:E1^E2^[[R2,E1],[name,E2,F1],[qual,E1,E2]]),[
	list_np(s_form:F1,ne_tag:date,ne_type:festival),
	list_np(s_form:F2,m_root:R2,ne_tag:date_post)
	]).

%% DATE_CORE --> LIST_NP(month) CD
rule(date_core(edge:Edge,s_form:[F1,' ',F2],
               sem:E^[[day,E],[name,E,[F1,' ',F2]],[realisation,E,Edge]]),[
	tagged_date(s_form:F1,sem:E^[[month,E]|Name]),
	cd(s_form:F2)
	]).

%% DATE_CORE --> LIST_NP(month) ORDINAL
rule(date_core(edge:Edge,s_form:[F1,' ',F2],
               sem:E^[[day,E],[name,E,[F1,' ',F2]],[realisation,E,Edge]]),[
	tagged_date(s_form:F1,sem:E^[[month,E]|Name]),
	ordinal(s_form:F2)
	]).

%% DATE_CORE --> CD LIST_NP(month)
rule(date_core(edge:Edge,s_form:[F1,' ',F2],
               sem:E^[[day,E],[name,E,[F1,' ',F2]],[realisation,E,Edge]]),[
	cd(s_form:F1),
	tagged_date(s_form:F2,sem:E^[[month,E]|Name])
	]).

%% DATE_PARTIAL_CORE --> CD_SEQ DATESPAN
rule(date_partial_core(s_form:[F1,' ',F2],sem:E^[[R2,E],[count,E,F1]]),[
        cd_seq(s_form:F1),
        list_np(s_form:F2,m_root:R2,ne_tag:timespan,ne_type:datespan)
        ]).

%% DATE_PARTIAL_CORE --> CD_SEQ DATE_POST
rule(date_partial_core(s_form:[F1,' ',F2],sem:E^[[R2,E],[count,E,F1]]),[
        cd_seq(s_form:F1),
	list_np(s_form:F2,m_root:R2,ne_tag:date_post)
        ]).

%% DATE_CORE --> LIST_NP(day)
rule(date_core(s_form:S,source:list,sem:Sem),[
        day_np(s_form:S,sem:Sem)
        ]).


%% CD_SEQ --> CD CC(and) DT(a) N(half)
rule(cd_seq(s_form:[F1,' ','and',' ','a',' ','half']),[
        cd(s_form:F1),
        cc(s_form:'and'),
        dt(s_form:'a'),
        n(s_form:'half')
        ]).
%% CD_SEQ --> CD CC(and) DT(a) N(quarter)
rule(cd_seq(s_form:[F1,' ','and',' ','a',' ','quarter']),[
        cd(s_form:F1),
        cc(s_form:'and'),
        dt(s_form:'a'),
        n(s_form:'quarter')
        ]).
%% CD_SEQ --> CD CC(and) DT(a) N(third)
rule(cd_seq(s_form:[F1,' ','and',' ','a',' ','third']),[
        cd(s_form:F1),
        cc(s_form:'and'),
        dt(s_form:'a'),
        n(s_form:'third')
        ]).
%% CD_SEQ --> CD CD / CD
rule(cd_seq(s_form:[F1,' ',F2,'/',F3]),[
        cd(s_form:F1),
        cd(s_form:F2),
        slash(s_form:'/'),
        cd(s_form:F3)
        ]).
%% CD_SEQ --> CD / CD
rule(cd_seq(s_form:[F1,'/',F2]),[
        cd(s_form:F1),
        slash(s_form:'/'),
        cd(s_form:F2)
        ]).
%% CD_SEQ --> CD
rule(cd_seq(s_form:F),[
        cd(s_form:F)
        ]).

%% SLASH --> SYM(/)
rule(slash(s_form:'/'),[
        sym(s_form:'/')
        ]).
%% SLASH --> N(/)
rule(slash(s_form:'/'),[
        n(s_form:'/')
        ]).

%% DATE_LIST --> DATE
rule(date_list(s_form:F1,sem:Sem),[
	tagged_date(s_form:F1,sem:Sem)
	]).

%% DATE_LIST --> DATE CC(AND) DATE
rule(date_list(s_form:[F1,' ',F2,' ',F3],
               sem:C^E1^E2^[[set,C],[set_member,C,E1],[set_member,C,E2],
			    Sem1,Sem2,[coord,E1,E2]]),[
	tagged_date(edge:Edge1,s_form:F1,sem:E1^Sem1),
	cc(s_form:F2,m_root:'and'),
	tagged_date(edge:Edge2,s_form:F3,sem:E2^Sem2)
        ]).

%% THIS_THAT --> DT(this)
rule(this_that(s_form:F1,m_root:'this'),[
	dt(s_form:F1,m_root:'this')
	]).
%% THIS_THAT --> DT(that)
rule(this_that(s_form:F1,m_root:'that'),[
	dt(s_form:F1,m_root:'that')
	]).


rule(tagged_date(edge:Edge,s_form:F1,
                 sem:E^[[R1,E],[realisation,E,Edge]]), [
	list_np(s_form:F1,m_root:R1,ne_tag:date,ne_type:specific)
]).

rule(tagged_date(edge:Edge,s_form:F1,
                 sem:E^[[day,E],[name,E,F1],[realisation,E,Edge]]), [
	list_np(s_form:F1,m_root:R1,ne_tag:date,ne_type:festival)
]).

rule(tagged_date(s_form:F1,sem:E^Sem), [
        day_np(s_form:F1,sem:E^Sem)
]).

rule(tagged_date(s_form:F1,sem:E^Sem), [
        month_np(s_form:F1,sem:E^Sem)
]).

rule(tagged_date(s_form:F1,sem:E^Sem), [
        year_np(s_form:F1,sem:E^Sem)
]).


rule(day_np(edge:Edge,s_form:F,
            sem:E^[[day,E],[name,E,F1],[realisation,E,Edge]]),[
        list_np(s_form:F1,ne_tag:date,ne_type:day)
]).

rule(month_np(edge:Edge,s_form:F2,
             sem:E^[[month,E],[name,E,F2],[realisation,E,Edge]]),[
	list_np(s_form:F2,ne_tag:date,ne_type:month)
]).

rule(year_np(edge:Edge,s_form:F2,
             sem:E^[[year,E],[name,E,F2],[realisation,E,Edge]]),[
	list_np(s_form:F2,ne_tag:date,ne_type:year)
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% TIMEX_NP --> TIME_PRE TIME_CORE TIME_POST
rule(timex_np(s_form:[F1,' ',F2,' ',F3],sem:E^[Sem2,Sem1,Sem3]),[
	time_pre(s_form:F1,sem:E^Sem1),
	time_core(s_form:F2,sem:E^Sem2),
	time_post(s_form:F3,sem:E^Sem3)
	]).
     					  
%% TIMEX_NP --> TIME_PRE TIME_CORE
rule(timex_np(s_form:[F1,' ',F2],sem:E^[Sem2,Sem1]),[
	time_pre(s_form:F1,sem:E^Sem1),
	time_core(s_form:F2,sem:E^Sem2)
	]).
     					  
%% TIMEX_NP --> TIME_CORE TIME_POST
rule(timex_np(s_form:[F1,' ',F2],sem:E^[Sem1,Sem2]),[
	time_core(s_form:F1,sem:E^Sem1),
	time_post(s_form:F2,sem:E^Sem2)
	]).
     					  
%% TIMEX_NP --> TIME_CORE
rule(timex_np(s_form:F1,sem:Sem),[
	time_core(s_form:F1,sem:Sem)
	]).

%% TIMEX_NP --> IN_RESTRICTED LIST_NP(time)
rule(timex_np(s_form:[F1,' ',F2],sem:P^E^[[name,E,F2],[R1,P,E]]),[
	in_restricted(s_form:F1,m_root:R1),
	list_np(s_form:F2,m_root:R2,ne_tag:time)
	]).

% Contraversial one this: is "last night" a time or a date?
% The MUC-7 rules say "time" but the examples say "date"!
% Let's stick to the rules.
%% TIMEX_NP --> "last night"
rule(timex_np(s_form:[F1,' ',F2],sem:E^[[night,E],[adj,E,'last']]),[
	jj(s_form:F1,m_root:'last'),
	n(s_form:F2,m_root:'night')
	]).
/*
%% TIMEX_NP --> LIST_NP(timex_pre) LIST_NP(day) TIME_CORE
rule(timex_np(s_form:[F1,' ',F2,' ',F3]),[
	list_np(s_form:F1,ne_tag:timex_pre),
	day_np(s_form:F2,sem:E^Sem),
	time_core(s_form:F3)
	]).
	*/
%% TIMEX_NP --> THIS_THAT LIST_NP(time)
rule(timex_np(s_form:[F1,' ',F2],sem:E^[[R2,E],[det,E,R1]]),[
	this_that(s_form:F1,m_root:R1),
	list_np(s_form:F2,m_root:R2,ne_tag:time)
	]).

%% TIMEX_NP --> TIMEX_NP TIMEX_NP
rule(timex_np(s_form:[F1,' ',F2],sem:E1^E2^[Sem1,Sem2]),[
	timex_np(s_form:F1,sem:E1^Sem1),
	timex_np(s_form:F2,sem:E2^Sem2)
	]).

%% TIME_CORE --> TIME
% morning
rule(time_core(s_form:F,sem:E^[name,E,F]),[
        list_np(s_form:F,m_root:R,ne_tag:time)
        ]).

%% TIME_CORE --> TIME TIMESPAN
% morning hours
rule(time_core(s_form:[F1,' ',F2],
        sem:E1^E2^[[R2,E1],[R1,E2],[qual,E1,E2]]),[ % no realisation for E2?
        list_np(s_form:F1,m_root:R1,ne_tag:time),
        list_np(s_form:F2,m_root:R2,ne_tag:timespan)
        ]).

%% TIME_CORE --> LIST_NP(day) "night"
% Tuesday night
rule(time_core(sem:E1^E^[[night,E1],Sem,[qual,E1,E]]), [
        day_np(sem:E^Sem),
        n(s_form:F2,m_root:'night')
        ]).

/*
%% TIME_CORE --> CD_SEQ TIMESPAN
rule(time_core(s_form:[F1,' ',F2]),[
        cd_seq(s_form:F1),
        list_np(s_form:F2,ne_tag:timespan,ne_type:timespan)
        ]).
*/


%% TIME_PRE --> LIST_NP(timex_pre)
rule(time_pre(s_form:F1,sem:E^[adj,E,R1]),[
	list_np(s_form:F1,m_root:R1,ne_tag:timex_pre)
	]).

%% TIME_POST --> LIST_NP(timex_trailer)
rule(time_post(s_form:F1,sem:E^[adj,E,R1]),[
	list_np(s_form:F1,m_root:R1,ne_tag:timex,ne_type:trailer)
	]).

%% TIME_POST --> LIST_NP(timezone)
rule(time_post(s_form:F1),[
	list_np(s_form:F1,ne_type:timezone)
	]).

%% TIME_POST --> COMMA LIST_NP(timezone)
rule(time_post(s_form:[',',' ',F1]),[
	comma(s_form:','),
	list_np(s_form:F1,ne_type:timezone)
	]).

%% IN_RESTRICTED --> IN(within)
rule(in_restricted(s_form:F1,m_root:'within'),[
	in(s_form:F1,m_root:'within')
	]).

%% IN_RESTRICTED --> IN(before)
rule(in_restricted(s_form:F1,m_root:'before'),[
	in(s_form:F1,m_root:'before')
	]).

%% IN_RESTRICTED --> IN(after)
rule(in_restricted(s_form:F1,m_root:'after'),[
	in(s_form:F1,m_root:'after')
	]).

%% IN_RESTRICTED --> IN(inside)
rule(in_restricted(s_form:F1,m_root:'inside'),[
	in(s_form:F1,m_root:'inside')
	]).
