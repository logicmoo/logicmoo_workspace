minCycleTime(0.5). % seconds per LPS cycle; our REST server is faaaar away

if true at 1 then 
	restapi_request('tag/tag2/',ID) from 2, 
	restapi_request_result(ID,Result) to T2, writeln(Result) from T2.

/*
In straight Prolog:
?- restapi_login("https://ictlab.ap.ngrok.io/restapi/","USER","PASSWORD"), restapi_request('tag/devices/',ID).
Some seconds later, copying the request ID:
?- restapi_request_result(ID,Result), print_term(Result,[]).
*/

/** <examples> 
?- restapi_login("https://ictlab.ap.ngrok.io/restapi/","USER","PASSWORD"), go(Timeline). % must login every time
*/
