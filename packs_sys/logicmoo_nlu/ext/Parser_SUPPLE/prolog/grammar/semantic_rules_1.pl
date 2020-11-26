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



rule(ne_np(s_form:F,sem:X^[[name,X,NAME],[measure,X],[measure_type,X,distance],[count,X,C], [unit,X,U]]),[
sem_cat_1(s_form:F,text:TEXT,type:'Distance',unit:U,count:C,kind:'distance',name:NAME)
]).



rule(ne_np(s_form:F,sem:X^[[name,X,NAME],[measure,X],[measure_type,X,age],[count,X,C], [unit,X,U]]),[
sem_cat_1(s_form:F,text:TEXT,type:'age',unit:U,count:C,kind:'measurement',name:NAME)
]).


rule(ne_np(s_form:F,sem:X^[[name,X,NAME],[measure,X],[measure_type,X,number],[count,X,C], [unit,X,U]]),[
sem_cat_1(s_form:F,text:TEXT,type:'number',unit:U,count:C,kind:'measurement',name:NAME)
]).



rule(ne_np(s_form:F,sem:X^[[name,X,NAME],[measure,X],[measure_type,X,time],[count,X,C], [unit,X,U]]),[
sem_cat_1(s_form:F,text:TEXT,type:'time',unit:U,count:C,kind:'measurement',name:NAME)
]).

