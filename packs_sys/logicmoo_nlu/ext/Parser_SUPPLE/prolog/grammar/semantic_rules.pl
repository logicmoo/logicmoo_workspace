%
% Rules to map output from GATE front end into
% semantic representation 
%

:- multifile best_parse_cats/1, rule/2, filter_chart/0.
:- dynamic best_parse_cats/1, rule/2, filter_chart/0.

% Best Parse Categories
%%% pass on all inactive edges

best_parse_cats([ne_np,cd_np,s]).

%filter_chart.


rule(cd_np(s_form:F),[
sem_cat(s_form:F,text:TEXT,type:'Amount',kind:'number',name:NAME)
]).


rule(ne_np(s_form:F,sem:X^[[name,X,NAME],[KIND,X]]),[
sem_cat(s_form:F,text:TEXT,type:'Quoted',kind:KIND,name:NAME)
]).


rule(ne_np(s_form:F,sem:X^[[name,X,NAME],[KIND,X]]),[
sem_cat(s_form:F,text:TEXT,type:'Person',kind:KIND,name:NAME)
]).


rule(ne_np(s_form:F,sem:X^[[name,X,NAME],[KIND,X]]),[
sem_cat(s_form:F,text:TEXT,type:'Date',kind:KIND,name:NAME)
]).


rule(ne_np(s_form:F,sem:X^[[name,X,NAME],[KIND,X]]),[
sem_cat(s_form:F,text:TEXT,type:'Time',kind:KIND,name:NAME)
]).


rule(ne_np(s_form:F,sem:X^[[name,X,NAME],[location,X],[KIND,X]]),[
sem_cat(s_form:F,text:TEXT,type:'Location',kind:KIND,name:NAME)
]).


rule(ne_np(s_form:F,sem:X^[[name,X,NAME],[KIND,X]]),[
sem_cat(s_form:F,text:TEXT,type:'Organization',kind:KIND,name:NAME)
]).

rule(ne_np(s_form:F,sem:X^[[name,X,NAME],[KIND,X]]),[
sem_cat(s_form:F,text:TEXT,type:'Money',kind:KIND,name:NAME)
]).


rule(ne_np(s_form:F,sem:X^[[name,X,NAME],[KIND,X]]),[
sem_cat(s_form:F,text:TEXT,type:'Percent',kind:KIND,name:NAME)
]).


rule(ne_np(s_form:F,sem:X^[[name,X,NAME],[measure,X]]),[
sem_cat(s_form:F,text:TEXT,type:'Measurement',kind:'measurement',name:NAME)
]).


rule(ne_np(s_form:F,sem:X^[[name,X,NAME],[measure,X],[measure_type,X,distance],[count,X,NAME]]),[
sem_cat(s_form:F,text:TEXT,type:'distance',kind:'measurement',name:NAME)
]).

rule(ne_np(s_form:F,sem:X^[[name,X,NAME],[measure,X],[measure_type,X,time],[count,X,NAME]]),[
sem_cat(s_form:F,text:TEXT,type:'time',kind:'measurement',name:NAME)
]).


%rule(ne_np(s_form:F,sem:X^[[name,X,NAME],[KIND,X]]),[
%sem_cat(s_form:F,text:TEXT,type:TYPE,kind:KIND,name:NAME)
%]).



rule(ne_np(s_form:F,sem:X^[[name,X,F],[date,X]]),[
ne_date(s_form:F,text:TEXT)]).
