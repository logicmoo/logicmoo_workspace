%:- consult('/var/lib/myfrdcsa/codebases/minor/formalog-pengines/formalog_pengines/formalog_pengines_client').
%:- consult('/var/lib/myfrdcsa/codebases/minor/formalog-pengines/formalog_pengines/formalog_pengines_server').
%:- start_agent(nomicMU).

/*
query_agent(Agent,Host,Message,Result) :-
 hasPenginePort(Agent,Port),
 larkc_client:query_formalog_pengines('http',Host,Port,Message,Result).
*/
