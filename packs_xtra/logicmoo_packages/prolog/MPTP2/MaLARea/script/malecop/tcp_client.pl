:- use_module(library(socket)).
	
create_client(Host:Port,In,Out) :-
        tcp_socket(Socket),
	catch(tcp_connect(Socket, Host:Port),
	      E,
	      (	  tcp_close_socket(Socket),
		  throw(E)
	      )),
        tcp_open_socket(Socket, In, Out).
/*        set_stream(In,timeout(10)),
        chat_to_server(In, Out),
        close_connection(In, Out).
*/

close_connection(In, Out) :-
        close(In, [force(true)]),
        close(Out, [force(true)]).

/*
chat_to_server(In, Out) :-
        write(Out,'=,empty\n'),
        flush_output(Out),
        writeln('Ack1'),
        read(In,L),
        writeln('Server:' --> L),
        write(Out,'=,empty\n'),
        flush_output(Out),
        writeln('Ack2'),
        read(In,K),
        writeln('Server:' --> K).
*/
