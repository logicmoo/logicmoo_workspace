%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  FILE     : Env/env_clima.pl
%
%  AUTHOR : Sebastian Sardina (2003)
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina www.cs.toronto.edu/cogrobo
%  TYPE   : system independent code
%  TESTED : SWI Prolog 5.0.10 www.swi-prolog.org
%
% This files provides the device for working with the CLIMA game simulation server 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             November 15, 2002
%
% This software was developed by the Cognitive Robotics Group under the
% direction of Hector Levesque and Ray Reiter.
%
%        Do not distribute without permission.
%        Include this notice in any copy made.
%
%
%         Copyright (c) 2000 by The University of Toronto,
%                        Toronto, Ontario, Canada.
%
%                          All Rights Reserved
%
% Permission to use, copy, and modify, this software and its
% documentation for non-commercial research purpose is hereby granted
% without fee, provided that the above copyright notice appears in all
% copies and that both the copyright notice and this permission notice
% appear in supporting documentation, and that the name of The University
% of Toronto not be used in advertising or publicity pertaining to
% distribution of the software without specific, written prior
% permission.  The University of Toronto makes no representations about
% the suitability of this software for any purpose.  It is provided "as
% is" without express or implied warranty.
% 
% THE UNIVERSITY OF TORONTO DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
% SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
% FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TORONTO BE LIABLE FOR ANY
% SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
% RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
% CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% This file assumes that the following is defined in env_gen.pl:
%
% -- start/0     : initialization of the environment (called when loaded)
% -- finalize/0  : finalization of the environment (called when exiting)
% -- main_dir/1  : obtain the root IndiGolog directory
% -- report_exog_event(A, M): 
%                  report exogenous event A with message M to the
%                  environment manager
% -- All compatibility libraries depending on the architecture such us:
%    -- compat_swi/compat_ecl compatibility libraries providing:
%
% -- The following two dynamic predicates should be available:
%    -- listen_to(Type, Name, Channel) 
%            listen to Channel of Type (stream/socket) with Name
%    -- terminate/0
%            order the termination of the application
%
% -- The following should be implemented here:
%
%  -- name_dev/1              : mandatory *
%  -- initializeInterfaces(L) : mandatory *
%  -- finalizeInterfaces(L)   : mandatory *
%  -- execute/4               : mandatory *
%  -- handle_steam/1          : as needed
%  -- listen_to/3             : as needed
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- include(env_gen).      % INCLUDE THE CORE OF THE DEVICE MANAGER

:- dynamic bye_message_received/0. % game is over and the device may finish

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTANTS TO BE USED
%
% name_dev/1 : state the name of the device manager (e.g., simulator, rcx)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Set name of the environment here. (THIS CONSTANT IS MANDATORY, DONT DELETE!)
name_dev(clima06). 

% Set verbose debug level
:- set_debug_level(5).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A - INITIALIZATION AND FINALIZATION OF INTERFACES
%     initializeInterfaces/1 and finalizeInterfaces/1
%
% HERE YOU SHOULD INITIALIZE AND FINALIZE EACH OF THE INTERFACES TO BE USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initializeInterfaces(L) :- 
           % 1 - Obtain IP and Port from L
        member([ipsim,SIP], L),   
        member([portsim, SP], L),  	% Get Host and Port of CLIMA simulator
        member([agentLogin, AL], L),  		
        member([agentPass, AP], L),  	% Get agent user/password participating in simulator
        string_to_number(SP, Port),
	string_to_atom(SIP, IP),
	assert(clima_location(IP, Port)),		% Assert the clima server location
	assert(clima_agentid(AL, AP)),			% Assert the agent login information
           % 2 - Setup communication with CLIMA game server
        report_message(system(2),'Establishing connection to CLIMA SIMULATOR'),
        printKbInstructions,
        connectToGameSimulator.

finalizeInterfaces(_) :- 
        disconnectFromGameSimulator,
        report_message(system(2), 'Disconnection from CLIMA SIMULATOR successful').



% printKbInstructions: Print instructions on how to enter keyboard input
printKbInstructions :-
    writeln('*********************************************************'), 
    writeln('* NOTE: This is the CLIMA SIMULATOR device manager'), 
    writeln('*   This window will show the communication'), 
    writeln('*   with the CLIMA GAME SIMULATOR'), 
    writeln('*********************************************************'),
    nl.


% Set main connection to CLIMA simulator socket at Host/Port
connectToGameSimulator :-
	clima_location(Host, Port),
	clima_agentid(AgentUser, AgentPass),
	clima_connect(Host, Port, comm_sim), 
	clima_authenticate(comm_sim, AgentUser, AgentPass, R), !,
	(R = ok -> 
	        report_message(system(2), 'Communication with CLIMA SIMULATOR established'),
		assert(listen_to(socket, comm_sim, comm_sim)),
		sleep(1),
		report_exog_event(connected(climaServer), _)
	; 
		report_message(error, ['Cannot authenticate to game server: ',R])
	).

% Finalize main connection to CLIMA simulator socket
disconnectFromGameSimulator :- 
	retractall(listen_to(socket, comm_sim, comm_sim)),
	clima_disconnect(comm_sim).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% B - HANDLERS FOR EACH STREAM/SOCKET THAT IS BEING HEARD:  handle_stream/1
%
% HERE YOU SHOULD WRITE HOW TO HANDLE DATA COMMING FROM EACH OF THE
% INTERFACES/CHANNELS USED
%
% OBS: handle_stream/1 must always end up succeeding!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic actionStatus/3. % Used to record each action request from the simulator


% Handle data comming from the CLIMA simulator
handle_stream(comm_sim) :- 
	get_socket_stream(comm_sim, read, Read),
	at_end_of_stream(Read), 
	report_message(system(2), ['Game server disconnection']), !,
	(bye_message_received -> 
		report_message(system(2), 
			['Bye message was already received. Ready for termination.']),
		disconnectFromGameSimulator
		
	;
		repeat,
		report_message(system(2), ['Reconnecting to game server...']),
		disconnectFromGameSimulator,
		sleep(5),
		(connectToGameSimulator -> true ; fail)
	).

	

handle_stream(comm_sim) :- 
	get_socket_stream(comm_sim, read, Read),
	\+ at_end_of_stream(Read),
	clima_receive_XML(comm_sim, XMLMess), !, 
	report_message(system(3), ['Message from game server: ', XMLMess]),
	handle_xml_message(XMLMess).




% Handle each possible message from the CLIMA simulator by sending the
% corresponding exogenous action to the agent
handle_xml_message(XMLMess) :-
	unfold_clima_message(XMLMess, sim-start, TimeStamp, BodyXMLMess), !,
	report_exog_event(simStart(TimeStamp, BodyXMLMess), _).
handle_xml_message(XMLMess) :-
	unfold_clima_message(XMLMess, request-action, TimeStamp, BodyXMLMess), !,
	member(id(IdAction), BodyXMLMess),
	member(deadline(Deadline), BodyXMLMess),
	asserta(actionStatus(Deadline, IdAction, pending)), % Observe we assert on the top!
	report_exog_event(requestAction(TimeStamp, BodyXMLMess), _).
	%drop_at_depot(BodyXMLMess).
handle_xml_message(XMLMess) :-
	unfold_clima_message(XMLMess, bye, _, _), !,
	assert(bye_message_received),
	report_exog_event(halt_exec, _).
handle_xml_message(XMLMess) :-
	unfold_clima_message(XMLMess, sim-end, TimeStamp, BodyXMLMess), !,
	report_exog_event(simEnd(TimeStamp, BodyXMLMess), _).
handle_xml_message(XMLMess) :- 
	report_message(warning, ['Message unknown from game simulator: ', XMLMess]).

:- dynamic justDropped/0.

drop_at_depot(Data) :-
	member(cells(LCells), Data), 
	member(cell(cur, LCellProp), LCells),
	member(depot,LCellProp), !,
	execute(drop, _, 0, _),
	assert(justDropped).
drop_at_depot(_).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% C - EXECUTION MODULE: execute/4
%
% This part implements the execution capabilities of the environment
%
% execute(Action, Type, Sensing) : execute Action of Type and return Sensing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Execute Action in game simulator
%execute(drop, _, _, ok) :- retract(justDropped), !.
execute(Action, _, N, Sensing) :- 
	(retract(actionStatus(Deadline, IdAction, pending)) ->
		true
	;
		report_message(warning,['Action *',(Action,N),'* is being sent without any previous request pending']),
		IdAction=0
		
	), 
 	%\+ tooLate(Deadline),
        report_message(action, ['Executing non-sensing action: *',(Action,N,IdAction),'*']), 
	(clima_execute(comm_sim, Action, IdAction, ok) ->
		Sensing=ok,
		report_message(system(4), ['The action *',(Action,N,IdAction),'* was sent to game server'])
	;
		Sensing=failed, 
		report_message(system(4), ['The following action failed to execute: *',	(Action,N,IdAction),'*'])
	),
	asserta(actionStatus(Deadline, IdAction, Sensing)).

% Currently, it is never too late to send an action to the game server.
tooLate(_) :- fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% DIRECT COMMUNICATION WITH CLIMA SIMULATOR %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%
% CLIMA PROTOCOL: High-level API
%%%%%%%%%%%%%%%%%%%%%%%

% Connection/disconnection
clima_connect(Host, Port, ConnID) :-
	catch(socket(internet, stream, ConnID), E, 
		(report_message(error, ['Cannot create socket : ',E]),fail) ), 
	catch(connect(ConnID, Host/Port), E, 
		(report_message(error, ['Cannot connect to game server: ',E]),fail) ), 
	get_socket_stream(ConnID, write, StreamW),
	get_socket_stream(ConnID, read, StreamR),
	set_stream(StreamW, encoding(text)),
	set_stream(StreamR, encoding(utf8)).

clima_disconnect(ConnID) :- close_socket(ConnID).

% Agent authentication
clima_authenticate(ConnID, AgentUser, AgentPass, Result) :-
	unfold_clima_message(XMLMess, auth-request, -1, [username(AgentUser), 
					password(AgentPass)]),
	clima_send_XML(ConnID, XMLMess),	!,
	(select([ConnID], 5, [ConnID]) ->
		clima_receive_XML(ConnID,MessReply), !,
		unfold_clima_message(MessReply, auth-response, _, BodyMess),
		member(result(Result), BodyMess)
	;	
		Result=timeout
	).


% Execute Action with IdAction in the simulator via socket ConnID
clima_execute(ConnID, Action, IdAction, ok) :-
	atom(Action), 
	unfold_clima_message(XMLMess, action, -1, [type(Action), id(IdAction)]),
	clima_send_XML(ConnID, XMLMess), !.
clima_execute(ConnID, Action, IdAction, ok) :-
	compound(Action),
	Action =.. [Name|Param],
	unfold_clima_message(XMLMess, action, -1, [type(Name), param(Param), id(IdAction)]),
	clima_send_XML(ConnID, XMLMess), !.
clima_execute(_, _, _, failed).



	
% 	[element(message, [type=auth-response], [element(authentication, [result=fail], [])])]

% 	(Type=[type='auth-request'] ; Type=[type=auth-request]),
% 	Body= element(authentication,[username=AgentUser,password=AgentPass], []),
% 	XMLMess=element(message, Type, ['\n', Body, '\n']),
% 	clima_send_XML(ConnID, XMLMess),
% 	(select([ConnID], 5, [ConnID]) ->
% 		clima_receive_XML(ConnID,MessReply),
% 		unfold_clima_message(MessReply, auth-response, _, BodyMess),
% 		member(result(Result), BodyMess)
% 	;	
% 		Result=timeout
% 	).


%%%%%%%%%%%%%%%%%%%%%%%
% CLIMA PROTOCOL: low-level interface
%%%%%%%%%%%%%%%%%%%%%%%

% SEND/RECEIVE XML Messages
clima_send_XML(ConnID, XMLMess) :-
	get_socket_stream(ConnID, write, Stream),
	xml_write(Stream, XMLMess, []),
	swritef(NullByte, '\0'),
	write(Stream, NullByte),	% Send necessary null-byte (thanks to Stavros)
	flush(Stream).

clima_receive_XML(ConnID, XMLMess) :-
	get_socket_stream(ConnID, read, Stream),
	( (peek_code(Stream, R), member(R,[0,10])) ->
		get_code(Stream, _),
		XMLMess=element(message, null, null)
	;
		catch(load_structure(Stream, LXMLMess, [parse(element)]), _, 
			(LXMLMess=[element(message,null,null)], writeln(segmentation)) ),
		(peek_code(Stream, 10) -> get_code(Stream,10) ; true),
		(peek_code(Stream, 0) -> get_code(Stream,0) ; true),
		XMLMess=element(message, _, _),
		member(XMLMess, LXMLMess)
	).




%%%%%%%%%%%%%%%%%%%%%%%
% CLIMA PROTOCOL: message handling
%%%%%%%%%%%%%%%%%%%%%%%

% Decompose a top-level CLIMA message into Type, TimeStamp, and its elements
clima_message(Mess, Type, TimeStamp, ListElements) :-
	\+ var(Mess),
	Mess=element(message, LAttrMess, LContentMess),
	member(type=Type, LAttrMess),
	(member(timestamp=TimeStamp, LAttrMess) -> true ; TimeStamp = -1),
	findall([NameE, LAttrE, LContE], 
		member(element(NameE, LAttrE, LContE), LContentMess), ListElements).

clima_message(Mess, Type, TimeStamp, ListElements) :-
	var(Mess),
	Mess=element(message, [type=Type|LAttrMess], LContentMess),
	(TimeStamp = -1 -> LAttrMess=[] ; LAttrMess=[timestamp=TimeStamp]),
	setof(element(NameE, LAttrE, LContE), 
		member([NameE, LAttrE, LContE], ListElements),  LContentMess).


% unfold_clima_messages(XMLMess, Type, TimeStamp, BodyXMLMess)
%	Extract all information from a CLIMA message XMLMess

% Messages from the simulation server: server---> agent
unfold_clima_message(XMLMess, auth-response, NTimeStamp, Data) :-
	permutation(Data, [result(Result)]), !,
	(T= 'auth-response' ; T = auth-response),
	clima_message(XMLMess, T, TimeStamp, LElements),
	any_to_number(TimeStamp, NTimeStamp),
	member([authentication,LAttr,[]], LElements),
	member(result=Result, LAttr).
unfold_clima_message(XMLMess, sim-start, NTimeStamp, Data) :-
	permutation(Data,
		[id(Id), opponent(Opp), steps(NSteps), 
			gsizeX(NGX), gsizeY(NGY), depotX(NDX), depotY(NDY)]), !,
	(T= 'sim-start' ; T = sim-start),
	clima_message(XMLMess, T, TimeStamp, LElements),
	any_to_number(TimeStamp, NTimeStamp),
	member([simulation,LAttr,[]], LElements),
	member(id=Id, LAttr),
	member(opponent=Opp, LAttr),
	member(steps=Steps, LAttr), any_to_number(Steps, NSteps),
	member(gsizex=GX, LAttr), any_to_number(GX, NGX),
	member(gsizey=GY, LAttr), any_to_number(GY, NGY),
	member(depotx=DX, LAttr), any_to_number(DX, NDX),
	member(depoty=DY, LAttr), any_to_number(DY, NDY).
unfold_clima_message(XMLMess, sim-end, NTimeStamp, Data) :-
	permutation(Data, [score(NScore), result(Result)]), !,
	(T= 'sim-end' ; T = sim-end),
	clima_message(XMLMess, T, TimeStamp, LElements), 
	any_to_number(TimeStamp, NTimeStamp),
	(member([sim-result, LAttr, []], LElements) ; 
			member(['sim-result', LAttr, []], LElements)),
	member(score=Score, LAttr), any_to_number(Score, NScore),
	member(result=Result, LAttr).
unfold_clima_message(XMLMess, bye, TimeStamp, []) :-
	(T= 'bye' ; T = bye),
	clima_message(XMLMess, T, TimeStamp, []).
unfold_clima_message(XMLMess, request-action, NTimeStamp, Data) :-
	permutation(Data, [step(NStep),posX(NPosX),posY(NPosY),items(NItems),
				deadline(NDeadline),id(Id),cells(CellsInfo)]), !,
		% CLIMA06 uses 'requestaction' - CLIMA07 uses 'request-action'
	(T= 'requestaction' ; T = requestaction ; T=request-action ; T='request-action'),
	clima_message(XMLMess, T, TimeStamp, LElements), 
	any_to_number(TimeStamp, NTimeStamp),
	member([perception,LAttr,LCont],  LElements),
	member(step=Step, LAttr),  any_to_number(Step, NStep),
	member(posx=PosX, LAttr), any_to_number(PosX, NPosX),
	member(posy=PosY, LAttr), any_to_number(PosY, NPosY),
		% No of gold items is only present in the 2007 edition of CLIMA
	(member(items=Items, LAttr) -> any_to_number(Items, NItems) ; NItems=(-1)),
	member(deadline=Deadline, LAttr), any_to_number(Deadline, NDeadline),
	member(id=Id, LAttr),
	extract_cells_info(LCont, CellsInfo).



% Messages from the agent to the simulation server: agent ---> server
unfold_clima_message(XMLMess, action, TimeStamp, Data) :-
	(TypeData = [id(Id), type(Action), param(Param)] ; TypeData = [id(Id), type(Action)]),
	length(TypeData, LenTypeData),
	permutation(Data, TypeData), !,
	(T= 'action' ; T = action),
	length(LElements, 1),
	length(LAttr,LenTypeData),
	clima_message(XMLMess, T, TimeStamp, LElements),
	member([action, LAttr, []],  LElements),
	member(type=Action, LAttr),
	member(id=Id, LAttr),
	(LenTypeData=3 -> member(param=Param, LAttr) ; true).

unfold_clima_message(XMLMess, auth-request, TimeStamp, Data) :-
	TypeData = [username(AgentUser), password(AgentPass)],
	length(TypeData, LenTypeData),
	permutation(Data, TypeData), !,
	(T= 'auth-request' ; T = auth-request),
	length(LElements,1),
	length(LAttr,LenTypeData),
	clima_message(XMLMess, T, TimeStamp, LElements),
	member([authentication, LAttr, []],  LElements),
	member(username=AgentUser, LAttr),
	member(password=AgentPass, LAttr).





% Given a content LContCells containing the information on the sorroundin cells
% it extracts that XML info into list LCellsInfo
extract_cells_info(LContCells, LCellsInfo) :-
	findall(cell(IDCell,PropCell), 
			(member(element(cell,[id=IDCell], PropCellRaw), LContCells),
			  extract_cell_info(PropCellRaw,PropCell), 
			  PropCell\=[]), 	LCellsInfo).	% only include cells with info
extract_cell_info(PropCellRaw, LPropCell) :-
	findall(agent(AgentType), 
		member(element(agent, [type=AgentType], []), PropCellRaw), L1),
	findall(obstacle, member(element(obstacle, [], []), PropCellRaw), L2),
	findall(gold, member(element(gold, [], []), PropCellRaw), L3),
	findall(mark(Mark), member(element(mark, [value=Mark], []), PropCellRaw), L4),
	findall(empty, member(element(empty, [], []), PropCellRaw), L5),
	findall(depot, member(element(depot, [], []), PropCellRaw), L6),
	LL=[L1,L2,L3,L4,L5,L6],
	flatten(LL, LPropCell).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF:  Env/env_clima.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*


% 

tcp_socket(S) ,tcp_connect(S,'tea.dyndns.org':12300), tcp_open_socket(S,R,W), set_stream(W,encoding(text)), 
 xml_write(W,element(message, [type='auth-request'], ['\n', element(authentication, [username=china3, password=1], []), '\n']),[]),nl
 (W),swritef(NullByte,'\0'),write(W,NullByte), flush_output(W).

*/