:- module(commTest,
		[startConnection/3,
		dtSend/1,
		dtDisconnect/0]).

%%%
%%% Helper functions
%%%
is_java_exception(_JVM, Thing) :- var(Thing), !, fail.
     is_java_exception(_JVM, Thing) :-
        Thing = java_exception(_),      % misc error in Java/Prolog glue
        !.
     is_java_exception(JVM, Thing) :-
        jasper_is_object(JVM, Thing),
        jasper_is_instance_of(JVM, Thing, 'java/lang/Throwable').
     print_exception_info(_JVM, java_exception(Message)) :- !,
        format(user_error, '~NJasper exception: ~w~n', [Message]).

print_exception_info(JVM, Excp) :-
        /*
        // Approximate Java code
        {
           String messageChars = excp.getMessage();
        }
        */
        jasper_call(JVM,
                    method('java/lang/Throwable', 'getMessage', [instance]),
                    get_message(+object('java/lang/Throwable'), [-chars]),
                    get_message(Excp, MessageChars)),
        /* // Approximate Java code
        {
           StringWriter stringWriter = new StringWriter();
           PrintWriter printWriter =  new PrintWriter(stringWriter);
           excp.printStackTrace(printWriter);
           printWriter.close();
           stackTraceChars = StringWriter.toString();
        }
        */
        jasper_new_object(JVM, 'java/io/StringWriter',
                          init, init, StringWriter),
        jasper_new_object(JVM, 'java/io/PrintWriter',
                          init(+object('java/io/Writer')),
                          init(StringWriter), PrintWriter),
        jasper_call(JVM,
                    method('java/lang/Throwable', 'printStackTrace', [instance]),
                    print_stack_trace(+object('java/lang/Throwable'),
                                      +object('java/io/PrintWriter')),
                    print_stack_trace(Excp, PrintWriter)),
        jasper_call(JVM,
                    method('java/io/PrintWriter','close',[instance]),
                    close(+object('java/io/PrintWriter')),
                    close(PrintWriter)),
        jasper_call(JVM,
                    method('java/io/StringWriter','toString',[instance]),
                    to_string(+object('java/io/StringWriter'),[-chars]),
                    to_string(StringWriter, StackTraceChars)),
        jasper_delete_local_ref(JVM, PrintWriter),
        jasper_delete_local_ref(JVM, StringWriter),
        %% ! exceptions are thrown as global references
        jasper_delete_global_ref(JVM, Excp),
        format(user_error, '~NJava Exception: ~s\nStackTrace: ~s~n',
               [MessageChars, StackTraceChars]).



startConnection(RBNBpath, ChannelName, RBNBServerAddress):-

   jasper_initialize([classpath([library('.'),RBNBpath])],JVM),


   format('~nSetting up the sender object ~n~n',[]),

    %Create the sender object
    jasper_new_object(JVM,
    	              'com/rbnb/sapi/Source',
    		       init(+integer,+string,+integer),
    		       init(1,'none',0),
    		       RBNBsource),
    
     format('~nConnecting to the server ~n~n',[]),
    %connect to the server
    jasper_call(JVM,
	        method('','OpenRBNBConnection', [instance]),
		openRBNBConnection( +object(''),+string,+string),
		openRBNBConnection(RBNBsource,RBNBServerAddress,'VoiceSource')),

    format('~nCreate channel map ~n~n',[]),
    %source map object
    jasper_new_object(JVM,
		      'com/rbnb/sapi/ChannelMap',
		      init,
		      init,
		      RBNBsendMap),

    %add a new channel
    jasper_call(JVM,
	        method('','Add', [instance]),
		add( +object(''),+string,[-integer]),
		add(RBNBsendMap,ChannelName,Test)),

    %register added channels
    jasper_call(JVM,
	        method('','Register', [instance]),
		register( +object(''),+object('com/rbnb/sapi/ChannelMap')),
		register(RBNBsource,RBNBsendMap)),

    %%%%
    % SETUP our receiver
    %%%%
    format('~nSetting up the receiver object~n~n',[]),

    %create the receiver object
    jasper_new_object(JVM,
	              'com/rbnb/sapi/Sink',
		       init,
		       init,
		       RBNBsink),

    %start communicatating with the server
    jasper_call(JVM,
	        method('','OpenRBNBConnection', [instance]),
		openRBNBConnection( +object(''),+string,+string),
		openRBNBConnection(RBNBsink,RBNBServerAddress,'VoiceSink')),

    %create map with incoming channels
    jasper_new_object(JVM,
		      'com/rbnb/sapi/ChannelMap',
		      init,
		      init,
		      RBNBreceiveMap),

    %decide from which channles you want to get data
    jasper_call(JVM,
	        method('','Add', [instance]),
		add( +object(''),+string,[-integer]),
		add(RBNBreceiveMap,'ClusterControllerVoiceSource/Paco',TestReceive)),

    %start waiting for data to come
    jasper_call(JVM,
	        method('','Monitor', [instance]),
		monitor( +object(''),+object('com/rbnb/sapi/ChannelMap'),+integer),
		monitor(RBNBsink,RBNBreceiveMap,0)),

    format('~nConnection looks good!!~n~n',[]),
    retractall(jvm(_)),
    retractall(rBNBsendMapGl(_)),
    retractall(rBNBsourceGlobal(_)),
    retractall(sinkGlobal(_)),
    retractall(dataMap(_)),
    assert(sendMapGl(RBNBsendMap)),
    assert(rBNBsourceGlobal(RBNBsource)),
    assert(sinkGlobal(RBNBsink)),
    assert(dataMap(RBNBdataMap)),
    assert(jvm(JVM)).


dtSend(Message) :-
%%%%%
%%% SENDING
%%%
%%%%
    jvm(JVM),
    rBNBsourceGlobal(RBNBsource),
    sendMapGl(RBNBsendMap),
    

    format('~nTrying to send~n~n',[]),
    %adds a timestamp
    jasper_call(JVM,
    	        method('','PutTimeAuto', [instance]),
    		putTimeAuto( +object(''),+string),
    		putTimeAuto(RBNBsendMap,'timeofday')),


    %sends a test string
    jasper_call(JVM,
    	        method('','PutDataAsString', [instance]),
    		putDataAsString( +object(''),+integer,+string),
    		putDataAsString(RBNBsendMap,0,Message)),


    %flushes it down the drain. On the other side is the receiver
    jasper_call(JVM,
    	        method('','Flush', [instance]),
    		flush( +object(''),+object('com/rbnb/sapi/ChannelMap'),+boolean,[-integer]),
    		flush(RBNBsource,RBNBsendMap,true,RetVal)),


    format('~nDONE SENDING!!!~n~n',[]).

%%%%%%%%%%%%
%%DISCONNECT
%%%%%%%%%%%%
dtDisconnect :-
    jvm(JVM),
    rBNBsourceGlobal(RBNBsource),
    sinkGlobal(RBNBsink),
    

 %disconnect to the server
    jasper_call(JVM,
	        method('','CloseRBNBConnection', [instance]),
		closeRBNBConnection( +object('')),
		closeRBNBConnection(RBNBsource)),
  
    jasper_call(JVM,
	        method('','CloseRBNBConnection', [instance]),
		closeRBNBConnection( +object('')),
		closeRBNBConnection(RBNBsink)).


%%%%%%%%%%%%%%
%% Receiveing
%%%%%%%%%%%
%dtReceive (ReceivedDataString) :- 
%    jvm(JVM),
%    sinkGlobal(RBNBsink),
%    dataMap(RBNBdataMap),


%%%%
%% MAIN STARTS HERE
%%%

:- dynamic jvm/1.
:- dynamic rBNBsourceGlobal/1,sendMapGl/1.
:- dynamic sinkGlobal/1.
:- use_module(library(jasper)).


%%%
%%% VARIABLES
%%%
%RBNBServerAddress='129.210.19.199',
%RBNBServerAddress='192.1',
%ChannelName='Voice',
%RBNBpath='C:/Progra~1/RBNB/V3.1B3/bin/rbnb.jar',


%startConnection(RBNBpath, ChannelName, RBNBServerAddress).
%dtSend('$CL3_P,A,7.0,*\n'),
%dtSend('$CL3_Q,A,7.0,*\n'),
%dtSend('$CL3_BETA,A,2.0,*\n').
%dtDisconnect.

