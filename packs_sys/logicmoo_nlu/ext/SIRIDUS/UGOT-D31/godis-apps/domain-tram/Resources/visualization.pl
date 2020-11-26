:- module(visualization,[setup_connection/0,gfx_set_initial_values/2,

			 gfx_set_node_value/3,gfx_add_program/5,gfx_delete_program/1]).



:- use_module(library(sockets)).

%:- use_module(library(system),[delete_file/1]).

:- use_module(library(system)).

:- use_module(library(find_file)).

:- use_module(library(lists),[append/3]).

:- use_module(library(charsio)).



visualize_field(vcr,play_status).

visualize_field(vcr,program_position).



visualize_field(tp,answering_machine_onoff).

visualize_field(tp,autoanswer_onoff).

visualize_field(tp,base_station_language).

visualize_field(tp,handset_language).

visualize_field(tp,ring_volume).

visualize_field(tp,earpiece_volume).

visualize_field(tp,tone_or_melody(internal)).

visualize_field(tp,tone_or_melody(external)).

visualize_field(tp,tone_or_melody(message)).

visualize_field(tp,tone_or_melody(search_signal)).



:- dynamic client_stream/1, server_socket/1.



session_file('whatever.txt').



setup_connection :-

	socket('AF_INET',Socket),

	socket_bind(Socket,'AF_INET'(localhost,Port)),

	socket_listen(Socket,100),

	%session_file(SessionFile),

	%open(SessionFile,write,S),

	%format(S,'~d\n',[Port]),

	%close(S),

	format('Waiting for client connection at port ~d ...\n',[Port]),



	%very temporary hack:

	find_file('try2.class',TRY2),

	atom_concat(TRYDIR,'try2.class',TRY2),

	working_directory(ThisDir,TRYDIR),

	number_chars(Port,PortStr),

	append("java try2 localhost ",PortStr,CmdStr),

	name(Command,CmdStr),

	exec(Command,[null,null,null],_),

	working_directory(_,ThisDir),

	

	socket_accept(Socket,Stream),

	%delete_file(SessionFile),

	write('Client connected\n'),

	assert(server_socket(Socket)),

	assert(client_stream(Stream)).



session_file(SessionFile) :-

	find_file('godis.pl',SetupFileDotPL),

	atom_chars(SetupFileDotPL,SetupFileDotPLCs),

	( Slash = 0'/ ; Slash = 0'\\ ),

	append(PathCs,[Slash|"godis.pl"],SetupFileDotPLCs),

	append(PathCs,[Slash|"visualization_port.dat"],SessionFileCs),

	atom_chars(SessionFile,SessionFileCs).



gfx_set_node_value(Domain,Node,Value) :-

	output('<update domain="~w" key="~w" value="~w">\n',[Domain,Node,Value]).



output(Format,Args) :-

	client_stream(S),

	format(S,Format,Args),

	flush_output(S).



gfx_set_initial_values(Module,Device) :-

	%trace,

	implies( visualize_field(Device,Field),

		 (( Module:dev_get(Field,Value),

		    gfx_set_node_value(Device,Field,Value) )) ).



pad(A,B):-

	name(A,AStr),

	length(AStr,3),

	number_chars(B,[48|AStr]).

pad(A,A):-

	name(A,AStr),

	length(AStr,4).







gfx_add_program( N, Program, Date, Start, Stop ) :-

	atom_chars(Date,[Date1,Date2,Date3,Date4]),

	

	(

	  format_to_chars('~d',Start,[Start1,Start2,Start3,Start4])

	;

	  format_to_chars('~d',Start,[Start2,Start3,Start4]),

	  Start1=0'0

	),

(

	  format_to_chars('~d',Stop,[Stop1,Stop2,Stop3,Stop4])

	;

	  format_to_chars('~d',Stop,[Stop2,Stop3,Stop4]),

	  Stop1=0'0

	), 

	output('<addProgram slot="~d" program="~d" date="~c~c~c~c" start="~c~c~c~c" stop="~c~c~c~c">\n',

	       [N,Program,

		Date1,Date2,Date3,Date4,

		Start1,Start2,Start3,Start4,

		Stop1,Stop2,Stop3,Stop4]).



gfx_delete_program( N ) :-

	output('deleteProgram slot="~d">\n',[N]).



implies( A, B ) :-

	\+ ( A, \+ B ).





:-setup_connection.
