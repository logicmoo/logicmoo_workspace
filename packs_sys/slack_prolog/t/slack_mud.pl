
:- ensure_loaded(library(slack_client)).

% slack_send(WsOutput,Data):- is_dict(Data),!,json_write_dict(WsOutput,Data),json_write_dict(user_error,Data),!.

%  slack_get_websocket(WS),format(WS,'~w~n',[Data]).

%  curl -X POST --data-urlencode 'payload={"channel": "logicmoo", "username": "@prologmud_connection", "text": "This isosted to #general and comes from a bot named webhookbot.", "icon_emoji": ":ghost:"}' https://hooks.slack.com/services/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

% curl -X POST --data-urlencode 'payload={"channel":"D3U47CE4W", "username":  "@prologmud_connection" , "text":to #general and comes from a bot named webhookbot.", "icon_emoji": ":ghost:"}' https://hooks.slack.com/services/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



:- listing(slack_websocket/3).



:- if(( \+ (is_thread_running(slack_start_listener)))).
:- thread_create(slack_start_listener,_,[alias(slack_start_listener)]).
:- endif.

:- if(( \+ (is_thread_running(slack_start_listener)))).
:- slack_start_listener.
:- endif.


% :- trace.

:- $gvar.foo = 1.

:- writeln($gvar.foo).

:- trace.
:- $bar.set(1).

write_vars :- writeln($foo.get).

test:- $foo.set(33), write_vars.

end_of_file.

:- $app.client = slack_client.clients.new().


:- $Client.register(hello ,
  debug.print. ["Successfully connected, welcome ", $Client.self.name,
   "to the '", $Client.team.name,"' team at https://", team.domain, ".slack.com."]).


:- $Client.register(message , 
   data.text.contains("bot hi") -> message(channel: data.channel, text: ["Hi <@",data.user,">!"]) ;
   not(data.text.contains('bot')) -> message( channel: data.channel, text: ["Sorry <@",data.user,">, what?"])).

:- $Client.register(close ,  debug.print( "Client is about to disconnect")).

% can register on the last created client (returned by slack_client.clients.new )
:- slack_client.current.register(closed , 
  debug.print. "Client has disconnected successfully!").

:- $Client.start().
