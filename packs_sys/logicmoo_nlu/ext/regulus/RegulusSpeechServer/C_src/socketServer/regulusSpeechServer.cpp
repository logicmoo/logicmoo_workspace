#include "regulusSpeechServer.hpp"

#include "platform_helper.c"

RegulusSpeechServer::RegulusSpeechServer(int argc, TCHAR **argv, NuanceConfig *config, Dispatcher *dispatcher):
m_argc(argc),
m_argv(argv),
m_config(config),
m_dispatcher(dispatcher),
m_recognition_requested(false),
m_print_debug_output(true),
m_ongoing_playback(false),
m_ongoing_recognition(false),
m_ongoing_recording(false),
m_recording_id(NullID),
m_recognition_id(NullID),
m_get_parameter_id(NullID),
m_record_to(NULL),
m_rc_engine(NULL),
m_is_rc_engine_initialized(false),
m_server_socket_port(NULL),
m_want_nbest(false),
m_nbest(false)
{
	NuanceStatus nuanceStatus;

	InitializeCriticalSection(&m_critical_section);
	InitializeCriticalSection(&m_client_connection_write_lock);
	InitializeCriticalSection(&m_logging_synchronization);

#ifdef WIN32
	//
	// This event object is used to create blocking
	// a call when Recognize() is beeing called
	//
	m_h_recognition_event	= CreateEvent(
			NULL,				// default security attributes
			FALSE,				// auto-reset event
			FALSE,				// initial state is signaled
			"RecognitionEvent"	// object name
			);
#endif

	//
	// initialize NL Result
	//
	m_nl_result = NLInitializeResult(&nuanceStatus);
	if (NUANCE_OK != nuanceStatus) {
		LogEvent(__FUNCTION__, "couldn't initialize NL result: ", MESSAGE_LEVEL_ERROR);
		LogEvent(__FUNCTION__, NuanceStatusMessage(nuanceStatus), MESSAGE_LEVEL_ERROR);

		return;
	}
	
	int val = 0;
	nuanceStatus = NuanceConfigGetIntParameter(config, "rec.DoNBest", &val);
	if (NUANCE_OK != nuanceStatus) {
		LogEvent(__FUNCTION__, "couldn't get rec.DoNBest param: ", MESSAGE_LEVEL_ERROR);
		LogEvent(__FUNCTION__, NuanceStatusMessage(nuanceStatus), MESSAGE_LEVEL_ERROR);

		return;
	}

	if (val == 1){
		m_want_nbest = true;
		m_nbest = true;
	}

	//
	// command line processing
	//
	for(int i=0 ; i<m_argc ; i++){
		if(strcmp(m_argv[i], "-v")==0){
			m_print_debug_output = true;
		}
		else if( (strcmp(m_argv[i], "-p")==0) || (strcmp(m_argv[i], "-port")==0)){
			if(((i+1)<=argc) && (m_argv[i+1] != NULL)){
				m_server_socket_port = strdup(m_argv[i+1]);
				LogEvent(__FUNCTION__, "Config: Port has been set to", MESSAGE_LEVEL_WARNING);
				LogEvent(__FUNCTION__, m_server_socket_port, MESSAGE_LEVEL_WARNING);
			}
			else{
				LogEvent(__FUNCTION__, "Config: '-p' option must be followed by port.", MESSAGE_LEVEL_WARNING);
				LogEvent(__FUNCTION__, "Config: Using default port.", MESSAGE_LEVEL_WARNING);
			}
		}	
		else if(strcmp(m_argv[i], "-f")==0){
			if(((i+1)<=argc) && (m_argv[i+1] != NULL)){
				try{
					// m_log_stream.open(m_argv[i+1], ios_base::app);
					m_log_stream.open(m_argv[i+1]);
					m_log_stream << "**********************************************************************" << endl << endl;
				}
				catch(...){
				}
			}
		}
	}

	if(NULL == m_server_socket_port){
		m_server_socket_port = strdup(DEFAULT_SERVER_SOCKET_PORT);
	}

}

RegulusSpeechServer::~RegulusSpeechServer(){
	if(m_nl_result){
		LogEvent(__FUNCTION__, "Cleaning up NLResult structure", MESSAGE_LEVEL_DEBUG);
		NLFreeResult(m_nl_result);
		m_nl_result = NULL;
	}

	if(m_record_to){
		LogEvent(__FUNCTION__, "Cleaning up recording data", MESSAGE_LEVEL_DEBUG);
		free(m_record_to);
		m_record_to = NULL;
	}

	if(m_server_socket_port){
		LogEvent(__FUNCTION__, "Freeing recording data", MESSAGE_LEVEL_DEBUG);
		free(m_server_socket_port);
	}

	//
	// close server socket
	//
	if(m_server_socket){
		LogEvent(__FUNCTION__, "Closing socket", MESSAGE_LEVEL_DEBUG);
		closesocket(m_server_socket);
	}

#ifdef WIN32
	LogEvent(__FUNCTION__, "Cleaning up socket library", MESSAGE_LEVEL_DEBUG);
	WSACleanup();
#endif

	LogEvent(__FUNCTION__, "Closing handles", MESSAGE_LEVEL_DEBUG);
#ifdef WIN32
	if(m_h_recognition_event){
		CloseHandle(m_h_recognition_event);
	}
#endif

	if(m_hthread){
#ifdef WIN32
		CloseHandle(m_hthread);
#endif
	}


//  TODO: this kills the app
//	LogEvent(__FUNCTION__, "Cleaning up locks", MESSAGE_LEVEL_DEBUG);
//	DeleteCriticalSection(&m_critical_section);
//	DeleteCriticalSection(&m_client_connection_write_lock);
//	DeleteCriticalSection(&m_logging_synchronization);

	if(m_log_stream.is_open()){
		LogEvent(__FUNCTION__, "Closing log stream", MESSAGE_LEVEL_DEBUG);
		m_log_stream.close();
	}
}

NuanceStatus RegulusSpeechServer::Startup(){
	LogEvent(__FUNCTION__, "entering", MESSAGE_LEVEL_DEBUG);

	NuanceStatus nuanceStatus = NUANCE_ERROR;

	bool isSocketCreated = false;
	isSocketCreated = CreateServerSocket(m_server_socket_port);

	if(false == isSocketCreated){
		LogEvent(__FUNCTION__, "Could not create server socket", MESSAGE_LEVEL_ERROR);
		nuanceStatus = NUANCE_ERROR;
	}
	else{
		this->m_rc_engine = new RCEngine(m_config, *m_dispatcher, *this, nuanceStatus);
		if (NULL == m_rc_engine) {
			LogEvent(__FUNCTION__, "couldn't create RCEngine: ", MESSAGE_LEVEL_ERROR);;
			LogEvent(__FUNCTION__, NuanceStatusMessage(nuanceStatus), MESSAGE_LEVEL_ERROR);
			nuanceStatus = NUANCE_ALLOCATE_FAILED;
		}
	}
	LogEvent(__FUNCTION__, "exiting", MESSAGE_LEVEL_DEBUG);
	return nuanceStatus;
}

bool RegulusSpeechServer::IsNuanceChannelInitialized(){
	return m_is_rc_engine_initialized;
}

void RegulusSpeechServer::HandlePlaybackStopped(PlaybackStoppedNotification const & n)
{
	LogEvent(__FUNCTION__, "entering", MESSAGE_LEVEL_DEBUG);

	EnterCriticalSection(&m_critical_section);

	m_ongoing_playback = false;

	//
	// check if queued files exist, and restart playback if needed
	//
	unsigned int queued_items = m_playback_buffer.GetNumQueuedPrompts();
	if( queued_items != 0 ){
		LogEvent(__FUNCTION__, "There are items in playback buffer:", MESSAGE_LEVEL_DEBUG);
		LogEvent(__FUNCTION__, (int)queued_items, MESSAGE_LEVEL_DEBUG);

		LeaveCriticalSection(&m_critical_section);

		StartPlayback(NULL);
	}
	else{
		LogEvent(__FUNCTION__, "There are no items in playback buffer", MESSAGE_LEVEL_DEBUG);
		LeaveCriticalSection(&m_critical_section);
	}
	LogEvent(__FUNCTION__, "exiting", MESSAGE_LEVEL_DEBUG);
}

/*
 * initiates playback.
 * if prompts is set to null it initiates a new playback action if there are any items in the playback buffer.
 * 
 */
bool RegulusSpeechServer::StartPlayback(const char * const *prompts)
{
	LogEvent(__FUNCTION__, "entering", MESSAGE_LEVEL_DEBUG);
	bool returnValue = false;

	EnterCriticalSection(&m_critical_section);

	if( ! m_ongoing_playback ){ // playback channel is idle, queue file list immediately
		m_playback_id = m_rc_engine->GetUniqueID();
		
		m_ongoing_playback = true;

		const char * const *promptsToQueue = NULL;
		if(prompts == NULL){
			promptsToQueue = m_playback_buffer.GetPrompts(m_playback_id);
		}
		else{
			promptsToQueue = prompts;
		}

/*
		cerr << "DEBUG: " << __FUNCTION__ << "Queueing " << promptsToQueue << endl;
		if(promptsToQueue != NULL){
			int i=0;
			while(promptsToQueue[i] != NULL){
				cerr << "\t" << i << "\t" << promptsToQueue[i] << endl;
				i++;
			}
		}
		cerr << endl << endl;
*/
		m_rc_engine->PlayPrompts(promptsToQueue, m_playback_id);

		returnValue = true;
	}
	else{						// copy files to playback buffer, once current playback is completed
								// files in the buffer will be played
		m_playback_buffer.AppendPrompts(prompts);
	}

	LeaveCriticalSection(&m_critical_section);
	LogEvent(__FUNCTION__, "exiting", MESSAGE_LEVEL_DEBUG);
	return returnValue;
}

bool RegulusSpeechServer::AbortPlayback()
{
	LogEvent(__FUNCTION__, "entering", MESSAGE_LEVEL_DEBUG);
	bool returnValue = false;

	EnterCriticalSection(&m_critical_section);

	if(m_ongoing_playback){
		m_rc_engine->AbortPlayback(m_playback_id);
		m_playback_buffer.FreePrompts(m_playback_id);
		returnValue = true;
	}

	LeaveCriticalSection(&m_critical_section);
	LogEvent(__FUNCTION__, "exiting", MESSAGE_LEVEL_DEBUG);
	return returnValue;
}

bool RegulusSpeechServer::GetParameter(const char *const parameter_name)
{
	LogEvent(__FUNCTION__, "entering", MESSAGE_LEVEL_DEBUG);
	bool returnValue = false;

	if(parameter_name != NULL){

		EnterCriticalSection(&m_critical_section);

		m_get_parameter_id = m_rc_engine->GetUniqueID();
		m_rc_engine->GetParameter(parameter_name, m_get_parameter_id);

		LeaveCriticalSection(&m_critical_section);
		returnValue = true;
	}
	LogEvent(__FUNCTION__, "exiting", MESSAGE_LEVEL_DEBUG);
	return returnValue;
}

void RegulusSpeechServer::HandleParameterGotten(ParameterGottenNotification const & n)
{
	LogEvent(__FUNCTION__, "entering", MESSAGE_LEVEL_DEBUG);

	if( n.GetID() == m_get_parameter_id ){
		ostringstream clientReply;
		char paramValue[512];
		string str ("");
		string searchString(""); 
		string replaceString("");
		string::size_type pos = 0;

		switch(n.GetDataType()){
			case ParameterEventNotification::Float :
				// cout << "got float param: " << ((FloatParameterGottenNotification const &)n).GetValue() << endl;
				n.GetValueAsString(paramValue, sizeof(paramValue));
				clientReply << "parameter(float, " << n.GetParameterName() << ", " << paramValue << ").";
				break;
			case ParameterEventNotification::Int :
				// cout << "got int param: " << ((IntParameterGottenNotification const &)n).GetValue() << endl;
				n.GetValueAsString(paramValue, sizeof(paramValue));
				clientReply << "parameter(int, " << n.GetParameterName() << ", " << paramValue << ").";
				break;
			case ParameterEventNotification::String :
				// cout << "got string param: " << ((StringParameterGottenNotification const &)n).GetValue() << endl;
				n.GetValueAsString(paramValue, sizeof(paramValue));
				str = paramValue;
				searchString = "\\"; 
				replaceString = "\\\\";
				// Escape backslashes	
				while ( (pos = str.find(searchString, pos)) != string::npos ) {
					str.replace( pos, searchString.size(), replaceString );
					pos++;
					pos++;
				}
				searchString = "'"; 
				replaceString = "\\'";
				pos = 0;
				// Escape single quote	
				while ( (pos = str.find(searchString, pos)) != string::npos ) {
					str.replace( pos, searchString.size(), replaceString );
					pos++;					
				}
				clientReply << "parameter(string, '" << n.GetParameterName() << "', '" << str << "').";
				//clientReply << "parameter(string, " << n.GetParameterName() << ", " << str << ").";				
				break;
			case ParameterEventNotification::Unknown :
				LogEvent(__FUNCTION__, "ParameterEventNotification::Unknown", MESSAGE_LEVEL_WARNING);
				clientReply << "parameter(error, unknown_parameter)";				
				break;
			default:
				LogEvent(__FUNCTION__, "this shouldn't happen", MESSAGE_LEVEL_DEBUG);
				clientReply << "parameter(error, error)";
				break;
		}
		EnterCriticalSection(&m_critical_section);
		m_get_parameter_id = NullID;
		LeaveCriticalSection(&m_critical_section);
		ReplyToClient(clientReply.str().c_str());
	}
	LogEvent(__FUNCTION__, "exiting", MESSAGE_LEVEL_DEBUG);
}

//
// Keep track of what is going on in the system.
//
void RegulusSpeechServer::HandleAcknowledgment(AcknowledgmentNotification const & n)
{
	LogEvent(__FUNCTION__, "entering", MESSAGE_LEVEL_DEBUG);

	if(NUANCE_OK != n.GetStatus()){
		LogEvent(__FUNCTION__, "Something went wrong: ", MESSAGE_LEVEL_ERROR);
		LogEvent(__FUNCTION__, n.GetRequestTypeAsString(n.GetRequestType()), MESSAGE_LEVEL_ERROR);
		LogEvent(__FUNCTION__, " ", MESSAGE_LEVEL_ERROR);
		LogEvent(__FUNCTION__, NuanceStatusMessage(n.GetStatus()), MESSAGE_LEVEL_ERROR);

		//
		// Something went wrong
		// reset the state of the action requested, so the app, can serve any new request
		//

		if(n.GetID() == m_playback_id){
			EnterCriticalSection(&m_critical_section);
			m_ongoing_playback = false;

//			string clientMsg = "playback_failed('";
//			clientMsg += NuanceStatusToString((PlaybackStoppedNotification)n.GetStatus());
//			clientMsg += "').";
//
//			LogEvent(__FUNCTION__, "Sending reply:", MESSAGE_LEVEL_DEBUG);
//			LogEvent(__FUNCTION__, clientMsg.c_str(), MESSAGE_LEVEL_DEBUG);
//			ReplyToClient(clientMsg.c_str());

			m_playback_buffer.FreePrompts(m_playback_id);

			LeaveCriticalSection(&m_critical_section);
		}
		else if(n.GetID() == m_recognition_id){
			EnterCriticalSection(&m_critical_section);
			m_ongoing_recognition = false;

			ostringstream clientReply;
			clientReply << "recognition_failed('" << NuanceStatusToString(n.GetStatus()) << "').";

			LogEvent(__FUNCTION__, "Sending reply:", MESSAGE_LEVEL_DEBUG);
			LogEvent(__FUNCTION__, clientReply.str().c_str(), MESSAGE_LEVEL_DEBUG);
			ReplyToClient(clientReply.str().c_str());

			LeaveCriticalSection(&m_critical_section);
		}
		else if(n.GetID() == m_recording_id){
			EnterCriticalSection(&m_critical_section);

			ostringstream clientReply;
			clientReply << "recording_failed('" << NuanceStatusToString(n.GetStatus()) << "').";

			LogEvent(__FUNCTION__, "Sending reply:", MESSAGE_LEVEL_DEBUG);
			LogEvent(__FUNCTION__, clientReply.str().c_str(), MESSAGE_LEVEL_DEBUG);
			ReplyToClient(clientReply.str().c_str());

			m_ongoing_recording = false;
			m_recording_requested = false;
			if(m_record_to != NULL){
				free(m_record_to);
				m_record_to = NULL;
			}
			LeaveCriticalSection(&m_critical_section);
		}
		else if(n.GetID() == m_set_parameter_id){
			EnterCriticalSection(&m_critical_section);

			ReplyToClient("set_parameter_failed.");

			LeaveCriticalSection(&m_critical_section);
		}
	}
	else{
		//
		// We got a positve notification
		// update flags depending on the request type
		//
		if(n.GetID() == m_playback_id){
			EnterCriticalSection(&m_critical_section);

			LogEvent(__FUNCTION__, "Got acknowledgment: ", MESSAGE_LEVEL_DEBUG);
			LogEvent(__FUNCTION__, n.GetRequestTypeAsString(n.GetRequestType()), MESSAGE_LEVEL_DEBUG);

			int notificationType = n.GetType();

			switch (notificationType) {
			case Notification::AbortAcknowledgment:
				LogEvent(__FUNCTION__, "Playback is being stopped: ", MESSAGE_LEVEL_DEBUG);
				LogEvent(__FUNCTION__, NuanceStatusMessage(n.GetStatus()), MESSAGE_LEVEL_DEBUG);

				m_ongoing_playback = false;
				break;
			case Notification::PlaybackAcknowledgment:
				LogEvent(__FUNCTION__, "Playback is being started: ", MESSAGE_LEVEL_DEBUG);
				LogEvent(__FUNCTION__, NuanceStatusMessage(n.GetStatus()), MESSAGE_LEVEL_DEBUG);

				m_ongoing_playback = true;
				m_playback_buffer.FreePrompts(m_playback_id);
				break;
			}

			LeaveCriticalSection(&m_critical_section);
		}
		else if (n.GetID() == m_recognition_id){
			EnterCriticalSection(&m_critical_section);
			LogEvent(__FUNCTION__, "Got acknowledgment: ", MESSAGE_LEVEL_DEBUG);
			LogEvent(__FUNCTION__, n.GetRequestTypeAsString(n.GetRequestType()), MESSAGE_LEVEL_DEBUG);

			int notificationType = n.GetType();
			switch (notificationType) {
			case Notification::AbortAcknowledgment:
				LogEvent(__FUNCTION__, "Recognition is being aborted: ", MESSAGE_LEVEL_DEBUG);
				LogEvent(__FUNCTION__, NuanceStatusMessage(n.GetStatus()), MESSAGE_LEVEL_DEBUG);
				m_ongoing_recognition = false;

				break;
			case Notification::RecognitionStopped:
				LogEvent(__FUNCTION__, "Recognition is being stopped: ", MESSAGE_LEVEL_DEBUG);
				LogEvent(__FUNCTION__, NuanceStatusMessage(n.GetStatus()), MESSAGE_LEVEL_DEBUG);
				m_ongoing_recognition = false;

				break;
			case Notification::RecognizeAcknowledgment:
				LogEvent(__FUNCTION__, "Recognition is being started: ", MESSAGE_LEVEL_DEBUG);
				LogEvent(__FUNCTION__, NuanceStatusMessage(n.GetStatus()), MESSAGE_LEVEL_DEBUG);
				m_ongoing_recognition = true;
				break;
			default:
				LogEvent(__FUNCTION__, "Unhandled notification on speech input channel: ", MESSAGE_LEVEL_WARNING);
				LogEvent(__FUNCTION__, NuanceStatusMessage(n.GetStatus()), MESSAGE_LEVEL_WARNING);
			}

			LeaveCriticalSection(&m_critical_section);
		}
		else if(n.GetID() == m_set_parameter_id){
			EnterCriticalSection(&m_critical_section);

			ReplyToClient("set_parameter_ok.");

			LeaveCriticalSection(&m_critical_section);

			if (m_want_nbest == true){
				m_nbest = true;				
			}
			else{
				m_nbest = false;
			}
		}
		else if(n.GetID() == m_recording_id){
			LogEvent(__FUNCTION__, "Got Acknowledgement: ", MESSAGE_LEVEL_DEBUG);
			LogEvent(__FUNCTION__, n.GetRequestTypeAsString(n.GetRequestType()), MESSAGE_LEVEL_DEBUG);

			EnterCriticalSection(&m_critical_section);

			int notificationType = n.GetType();
			switch (notificationType) {
				case Notification::RecordAcknowledgment:
					// cout << "INFO: " << "NuanceEngineController::HandleAcknowledgment: " << "" << endl;
					LogEvent(__FUNCTION__, "Got RecordAcknowledgment", MESSAGE_LEVEL_DEBUG);

					m_ongoing_recording = true;
					break;
				case Notification::RecordingStopped:
					LogEvent(__FUNCTION__, "Got RecordingStopped", MESSAGE_LEVEL_DEBUG);
					m_ongoing_recording = false;
					m_recording_requested = false;
					break;
				default:
					LogEvent(__FUNCTION__, "Unhandled notification on speech input channel: ", MESSAGE_LEVEL_WARNING);
					LogEvent(__FUNCTION__, NuanceStatusMessage(n.GetStatus()), MESSAGE_LEVEL_WARNING);
			}

			LeaveCriticalSection(&m_critical_section);
		}
	}
	LogEvent(__FUNCTION__, "exiting", MESSAGE_LEVEL_DEBUG);
}

void RegulusSpeechServer::HandleInitializationCompleted(InitializationCompletedNotification const & icn)
{
	LogEvent(__FUNCTION__, "entering", MESSAGE_LEVEL_DEBUG);

	// check status
	NuanceStatus status = icn.GetStatus();

	if (NUANCE_OK != status)
	{
		LogEvent(__FUNCTION__, NuanceStatusMessage(status), MESSAGE_LEVEL_ERROR);

		delete m_rc_engine;
		m_rc_engine = NULL;

		LogEvent(__FUNCTION__, "exiting", MESSAGE_LEVEL_DEBUG);
		return;
	}
	else{
		LogEvent(__FUNCTION__, "Nuance channel activated ...", MESSAGE_LEVEL_INFO);
		m_is_rc_engine_initialized = true;

		m_hthread = CreateSocketServerThread();

		if(NULL == m_hthread){
			LogEvent(__FUNCTION__, "couldn't create TCP thread", MESSAGE_LEVEL_DEBUG);
		}
	}
	LogEvent(__FUNCTION__, "exiting", MESSAGE_LEVEL_DEBUG);
}

bool RegulusSpeechServer::CreateServerSocket(const char* port){
	LogEvent(__FUNCTION__, "entering", MESSAGE_LEVEL_DEBUG);

	bool returnValue = false;
    struct sockaddr_in server;
	// struct servent *pse; /* Points to service information. */
#ifdef WIN32
	WSAData wsaData;

	if ( WSAStartup( MAKEWORD(2,2), &wsaData ) != NO_ERROR ) {
		LogEvent(__FUNCTION__, "Could not initialize socket library.", MESSAGE_LEVEL_ERROR);
		returnValue = false;
	}
	else {
#endif
		memset( (char *) &server, 0, sizeof(sockaddr_in) );

		m_server_socket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

		if(INVALID_SOCKET == m_server_socket){
			LogEvent(__FUNCTION__, "Could not create socket", MESSAGE_LEVEL_ERROR);
			returnValue = false;
		}
		else{
			server.sin_family = AF_INET;
			// just listen on the loopback device.
			server.sin_addr.s_addr = INADDR_ANY;
			// server.sin_addr.s_addr = INADDR_LOOPBACK;
			server.sin_port = htons( (u_short) atoi(port) );

			string msg = "Creating TCP socket at port ";
			msg += port;
			LogEvent(__FUNCTION__, msg.c_str(), MESSAGE_LEVEL_DEBUG);

/*
			if( (pse = getservbyname( port, "tcp" )) != NULL ) {
				server.sin_port = (u_short) pse->s_port;
			} else if( (server.sin_port = htons( (u_short) atoi(port) )) == 0 ) {
				LogEvent(__FUNCTION__, "Could not create socket: Invalid port number.", MESSAGE_LEVEL_ERROR);
				returnValue = false;
			}
*/
			if(bind(m_server_socket, (struct sockaddr *) &server, sizeof(server)) == SOCKET_ERROR){
				LogEvent(__FUNCTION__, "Unable to bind socket", MESSAGE_LEVEL_ERROR);
				returnValue = false;
			}
			else{
				returnValue = true;
			}
		}
#ifdef WIN32
	}
#endif

	LogEvent(__FUNCTION__, "exiting", MESSAGE_LEVEL_DEBUG);
	return returnValue;
}

HANDLE RegulusSpeechServer::CreateSocketServerThread(){
	LogEvent(__FUNCTION__, "entering", MESSAGE_LEVEL_DEBUG);
	HANDLE return_value = NULL;

    return_value = CreateThread(
        NULL,													// default security attributes
        0,														// use default stack size
		DispatchThread,											// thread function
        (LPVOID)this,											// argument to thread function
        0,														// use default creation flags
        NULL);													// returns the thread identifier
	LogEvent(__FUNCTION__, "CreateThread returned", MESSAGE_LEVEL_DEBUG);
 	// Check the return value for success.
	if (NULL == return_value) {
    	LogEvent(__FUNCTION__, "Creating tcp-thread failed.", MESSAGE_LEVEL_ERROR);
	}

	LogEvent(__FUNCTION__, "exiting", MESSAGE_LEVEL_DEBUG);
	return return_value;
}

#ifdef WIN32
DWORD WINAPI RegulusSpeechServer::DispatchThread(LPVOID param){
	((RegulusSpeechServer *)param)->SocketServerMainLoop();
	return 0;
}
#else
void *RegulusSpeechServer::DispatchThread(LPVOID param){
	((RegulusSpeechServer *)param)->SocketServerMainLoop();
	return NULL;
}

#endif

bool RegulusSpeechServer::ReplyToClient(const char* const message){
	LogEvent(__FUNCTION__, "entering", MESSAGE_LEVEL_DEBUG);

	bool returnValue = false;

	EnterCriticalSection(&m_client_connection_write_lock);

	LogEvent(__FUNCTION__, "Sending: ", MESSAGE_LEVEL_DEBUG);
	LogEvent(__FUNCTION__, message, MESSAGE_LEVEL_DEBUG);

	if(m_client_connection){
		if(send(m_client_connection, message, (int)strlen(message), 0) == SOCKET_ERROR){
			LogEvent(__FUNCTION__, "something went wrong - returning false" , MESSAGE_LEVEL_ERROR);
			returnValue = false;
		}
		else{
			send(m_client_connection, "\n", (int)strlen("\n"), 0);
			returnValue = true;
		}
	}

	LeaveCriticalSection(&m_client_connection_write_lock);

	LogEvent(__FUNCTION__, "exiting", MESSAGE_LEVEL_DEBUG);
	return returnValue;
}

void findAndReplace(string *source, string find, string replace) {
	size_t j;
	for ( ; (j = source->find(find)) != string::npos ; ) {
		*source = source->replace(j, find.length(), replace);
	}
}

// returns true if processing should continue
bool RegulusSpeechServer::InterpretCommand(string command){
	bool return_value = true;

	if( command.find("SAY_NUMBER ", 0, strlen("SAY_NUMBER ")) != string::npos){
		LogEvent(__FUNCTION__, "Received SAY_NUMBER.", MESSAGE_LEVEL_DEBUG);
		string number = command.substr(strlen("SAY_NUMBER "));
		findAndReplace(&number, " ", "");

		const char * playback_buffer[2] =  { number.c_str(), NULL };
		StartPlayback(playback_buffer);
	}
	else if(command.find("SAY_FILE ", 0, strlen("SAY_FILE ")) != string::npos){
		LogEvent(__FUNCTION__, "Received SAY_FILE.", MESSAGE_LEVEL_DEBUG);

		string arg = command.substr(strlen("SAY_FILE "));

		const char * playback_buffer[2] =  { arg.c_str(), NULL };
		StartPlayback(playback_buffer);
	}
	else if(command.find("SAY_LIST ", 0, strlen("SAY_LIST ")) != string::npos){
		LogEvent(__FUNCTION__, "Received SAY_LIST.", MESSAGE_LEVEL_DEBUG);

		string files = command.substr(strlen("SAY_LIST "));
			LogEvent(__FUNCTION__, "XXXX files", MESSAGE_LEVEL_DEBUG);
			LogEvent(__FUNCTION__, files.c_str(), MESSAGE_LEVEL_DEBUG);
			// cerr << "XXXX  files = '" << files.c_str() << "'" << endl;

		LogEvent(__FUNCTION__, "Creating file list vector:", MESSAGE_LEVEL_DEBUG);

		size_t start = 0;
		size_t end = 0;
		size_t index = 0;
		vector<string> vfiles;
		size_t copy_from = 0;
		end = files.find_last_of(",");

		// SAY_LIST zutsu.wav, wa.wav, okori_masu.wav, ka.wav
		if(end == string::npos){
			//
			// it's just one item -- no commata
			//
			const char * playback_buffer[2] =  { files.c_str(), NULL };

			LogEvent(__FUNCTION__, "will spool one file", MESSAGE_LEVEL_DEBUG);
			LogEvent(__FUNCTION__, files.c_str(), MESSAGE_LEVEL_DEBUG);

			StartPlayback(playback_buffer);
		}
		else{

			while(start != end){
				index = files.find(",", start+1);
				if( ((index-1) > 0)  && (files.at(index-1) != '\\') ){
					string tmp = files.substr(copy_from, index-copy_from);
					if(tmp.at(0) == ',') {
						tmp = tmp.substr(1);
					}
					findAndReplace(&tmp, "\\,", ",");
					LogEvent(__FUNCTION__, tmp.c_str(), MESSAGE_LEVEL_DEBUG);
					vfiles.push_back(tmp);

					copy_from = index;
				}

				start = index;
			}

			// set the copy_from pointer on back, when no other playback items are present in our list
			// otherwise the 1st character won't be copied
			if(copy_from == 0) {
				copy_from = -1;
			}
			string tmp = files.substr(copy_from+1, files.size());
			findAndReplace(&tmp, "\\,", ",");

			vfiles.push_back(tmp);

			char ** c_file_list = (char **)malloc((vfiles.size()+1) * sizeof(char *));
			
			LogEvent(__FUNCTION__, "file list: ", MESSAGE_LEVEL_DEBUG);
			for(unsigned int i=0 ; i < vfiles.size() ; i++){
				c_file_list[i] = (char *)vfiles[i].c_str();
				LogEvent(__FUNCTION__, c_file_list[i], MESSAGE_LEVEL_DEBUG);
			}
			c_file_list[vfiles.size()] = NULL;

			LogEvent(__FUNCTION__, "", MESSAGE_LEVEL_DEBUG);
			StartPlayback(c_file_list);

			free(c_file_list);
		}
	}
	else if(command.find("SAY_TTS", 0, strlen("SAY_TTS")) != string::npos){
		LogEvent(__FUNCTION__, "Received SAY_TTS.", MESSAGE_LEVEL_DEBUG);

		string arg = command.substr(strlen("SAY_TTS"));
		findAndReplace(&arg, " ", "");

		const char * playback_buffer[2] =  { arg.c_str(), NULL };
		StartPlayback(playback_buffer);
	}
	else if(command.find("SET_OUTPUT_VOLUME", 0, strlen("SET_OUTPUT_VOLUME")) != string::npos){
		LogEvent(__FUNCTION__, "Received SET_OUTPUT_VOLUME.", MESSAGE_LEVEL_DEBUG);

		string arg = command.substr(strlen("SET_OUTPUT_VOLUME"));
		findAndReplace(&arg, " ", "");

		SetParameter("int", "audio.OutputVolume", arg.c_str());
	}
	else if(command.find("RECOGNISE", 0, strlen("RECOGNISE")) != string::npos){
		if(command.find("RECOGNISE_FILE", 0, strlen("RECOGNISE_FILE")) != string::npos){
			string fileName;
			string grammar;

			int last_space = command.rfind(" ");

			if(last_space != string::npos){
				fileName = command.substr(strlen("RECOGNISE_FILE "), last_space-strlen("RECOGNISE_FILE "));
				grammar = command.substr(last_space+1);

				StartRecognition(grammar.c_str(), fileName.c_str());
			}
			else{
				LogEvent(__FUNCTION__, "Illegal command string:", MESSAGE_LEVEL_ERROR);
				LogEvent(__FUNCTION__, command.c_str(), MESSAGE_LEVEL_ERROR);
			}

		}
		else{
			LogEvent(__FUNCTION__, "Received RECOGNISE.", MESSAGE_LEVEL_DEBUG);
			string grammar = command.substr(strlen("RECOGNISE "));
			findAndReplace(&grammar, " ", "");

			StartRecognition(grammar.c_str());
		}
	}
	else if(command.find("RECORD", 0, strlen("RECORD")) != string::npos){
		//LogEvent(__FUNCTION__, "Received RECORD - unimplemented.", MESSAGE_LEVEL_DEBUG);
		LogEvent(__FUNCTION__, "Received RECORD", MESSAGE_LEVEL_DEBUG);
		string file = command.substr(strlen("RECORD "));
		findAndReplace(&file, " ", "");
		
		StartRecording(file.c_str());
	}
	else if(command.find("ABORT_RECOGNITION", 0, strlen("ABORT_RECOGNITION")) != string::npos){
		LogEvent(__FUNCTION__, "Received ABORT_RECOGNITION.", MESSAGE_LEVEL_DEBUG);
		AbortRecognition();
	}
	else if(command.find("ABORT_RECORDING", 0, strlen("ABORT_RECORDING")) != string::npos){
		LogEvent(__FUNCTION__, "Received ABORT_RECORDING.", MESSAGE_LEVEL_DEBUG);
		AbortRecording();
	}
	else if(command.find("ABORT_PLAYBACK", 0, strlen("ABORT_PLAYBACK")) != string::npos){
		LogEvent(__FUNCTION__, "Received ABORT_PLAYBACK.", MESSAGE_LEVEL_DEBUG);
		AbortPlayback();
	}
	else if(command.find("GET_PARAMETER", 0, strlen("GET_PARAMETER")) != string::npos){
		LogEvent(__FUNCTION__, "Received GET_PARAMETER.", MESSAGE_LEVEL_DEBUG);

		string paramName = command.substr(strlen("GET_PARAMETER "));
		findAndReplace(&paramName, " ", "");

		GetParameter(paramName.c_str());
	}
	else if(command.find("SET_PARAMETER", 0, strlen("SET_PARAMETER")) != string::npos){
		LogEvent(__FUNCTION__, "Received SET_PARAMETER.", MESSAGE_LEVEL_DEBUG);

		size_t second_space = command.find(" ", strlen("SET_PARAMETER "));

		if(second_space != string::npos){
			string parameter = command.substr(strlen("SET_PARAMETER "), second_space-strlen("SET_PARAMETER "));
			string value = command.substr(second_space+1);
			
			if (parameter == "rec.DoNBest"){				
				if (value == "TRUE"){					
					m_want_nbest = true;
					value = "1";
				}
				else{					
					m_want_nbest = false;
					value = "0";
				}		
			}
			
			SetParameter(parameter.c_str(), value.c_str());			
		}
		else{
			ReplyToClient("set_parameter_failed.");
		}

		LogEvent(__FUNCTION__, "Exiting SET_PARAMETER.", MESSAGE_LEVEL_DEBUG);
	}
	else if(command.find("INTERPRET ", 0, strlen("INTERPRET ")) != string::npos){
		LogEvent(__FUNCTION__, "Received INTERPRET", MESSAGE_LEVEL_DEBUG);

		size_t first_space = command.find(" ", strlen("INTERPRET "));
		string grammar = command.substr(strlen("INTERPRET "), first_space-strlen("INTERPRET "));
		string text = command.substr(first_space+1);

		StartInterpretation(grammar.c_str(), text.c_str());
	}
	else if(command.find("CLEAN_UP") != string::npos){
		LogEvent(__FUNCTION__, "Received CLEAN_UP - exiting.", MESSAGE_LEVEL_DEBUG);
		return_value = false;
	}
	else {
		LogEvent(__FUNCTION__, "Unknown message", MESSAGE_LEVEL_WARNING);
	}

	return return_value;
}

void RegulusSpeechServer::SocketServerMainLoop() {
	LogEvent(__FUNCTION__, "entering", MESSAGE_LEVEL_DEBUG);

	char buf[MAX_MESSAGE_SIZE];

	LogEvent(__FUNCTION__, "Calling listen()", MESSAGE_LEVEL_DEBUG);
    if(SOCKET_ERROR == listen(m_server_socket, 1)){
	int error = 0;
#ifdef WIN32
    	error = WSAGetLastError();
#endif
    	LogEvent(__FUNCTION__, "listen() failed: ", MESSAGE_LEVEL_DEBUG);
    	LogEvent(__FUNCTION__, error, MESSAGE_LEVEL_DEBUG);

		//
		// delete rc_engine - this will cause the dispatcher in main() to return
		//
		delete m_rc_engine;
		m_rc_engine = NULL;

		return;
    }

	LogEvent(__FUNCTION__, "Calling accept()", MESSAGE_LEVEL_DEBUG);
	m_client_connection = accept(m_server_socket, NULL, NULL);
	if(INVALID_SOCKET == m_client_connection){
		int error = 0;
#ifdef WIN32
		error = WSAGetLastError();
#endif
    		LogEvent(__FUNCTION__, "accept() failed: ", MESSAGE_LEVEL_DEBUG);
	    	LogEvent(__FUNCTION__, error, MESSAGE_LEVEL_DEBUG);

		//
		// delete rc_engine - this will cause the dispatcher in main() to return
		//
		delete m_rc_engine;
		m_rc_engine = NULL;

		return;
	}


	LogEvent(__FUNCTION__, "Waiting for client data", MESSAGE_LEVEL_DEBUG);
	memset(buf, 0, sizeof(buf));
	int retVal = recv(m_client_connection, buf, sizeof(buf), 0);
	string command;
	bool done = false;
	while((retVal != 0) && (retVal != SOCKET_ERROR) && (retVal <= sizeof(buf))){

		// cerr << "______ buff == ." << buf << "." << endl;

		command += buf;

		// cerr << "______ command == ." << command << "." << endl;

		findAndReplace(&command, "\r", "");

		bool commands_left = true;

		while( commands_left ){
			size_t command_pos = command.find_first_of("\n");
			string command_to_interpret;

			if(command_pos != string::npos){
				command_to_interpret = command.substr(0, command_pos);

				// cerr << "______ command_to_interpret == ." << command_to_interpret << "." << endl;
				if(false == InterpretCommand(command_to_interpret)){
					LogEvent(__FUNCTION__, "InterpretCommand is false -- breaking", MESSAGE_LEVEL_DEBUG);
					done = true;
					break;
				}

				command = command.substr(command_pos+1);

				// cerr << "______ command == ." << command << "." << endl;
			}
			else{
				commands_left = false;
			}

		}

		memset(buf, 0, sizeof(buf));
		LogEvent(__FUNCTION__, "Waiting for client data", MESSAGE_LEVEL_DEBUG);
		
		if( done == true ) {
			retVal = 0;
		}
		else{
			retVal = recv(m_client_connection, buf, sizeof(buf), 0);
			LogEvent(__FUNCTION__, "recv returned", MESSAGE_LEVEL_DEBUG);
		}


		if(retVal == 0){
			LogEvent(__FUNCTION__, "Client disconnected.", MESSAGE_LEVEL_DEBUG);
		}
		if(retVal == SOCKET_ERROR){
			int error_code = 0;
#ifdef WIN32
			error_code = WSAGetLastError();
			LogEvent(__FUNCTION__, "Error Code:", MESSAGE_LEVEL_DEBUG);
			LogEvent(__FUNCTION__, error_code, MESSAGE_LEVEL_DEBUG);
#endif

			LogEvent(__FUNCTION__, "Communication error - leaving message processing loop.", MESSAGE_LEVEL_DEBUG);
		}
	}

	//
	// stop communicating with client
	//
#ifdef WIN32
	shutdown(m_client_connection, SD_BOTH);
#else
	close(m_client_connection);
#endif

	//
	// stop any ongoing actions
	//
	AbortPlayback();
	AbortRecognition();
	AbortRecording();

	//
	// delete rc_engine - this will cause the dispatcher in main() to return
	//
	delete m_rc_engine;
	m_rc_engine = NULL;
}

void RegulusSpeechServer::StartInterpretation(const char *const grammar, const char *const text){
	EnterCriticalSection(&m_critical_section);

	m_interpretation_id = m_rc_engine->GetUniqueID();
	m_ongoing_interpretation = true;
	m_rc_engine->Interpret(text, grammar, m_interpretation_id);

	LeaveCriticalSection(&m_critical_section);
}

void RegulusSpeechServer::HandleInterpretationStopped(const InterpretationStoppedNotification &n){
	EnterCriticalSection(&m_critical_section);
	string client_msg;

	if(n.GetID() == m_interpretation_id) {
		if((n.GetReason() == InterpretationStoppedNotification::COMPLETED)){

			char regulus_message[2*MAX_MESSAGE_SIZE];

			m_interpretation_id = NullID;

			NuanceStatus nuance_status;
			NLResult *nl_result = NLInitializeResult(&nuance_status);

			if(NUANCE_OK == nuance_status){
				NLCopyResult(n.GetNLResult(), nl_result);
				RecResult *rec_result = RecResultNewFromNLResult(nl_result);

				int regulus_conversion = SPrintRecResultInRegulusForm(regulus_message, sizeof(regulus_message), m_nl_result, rec_result, GetNBest());

				RecResultDelete(rec_result);

				if( regulus_conversion >= 0){
					client_msg = regulus_message;
				}
				else{
					client_msg = "recognition_failed('could not produce regulus result string').";
					LogEvent(__FUNCTION__, "Failed to convert regulus message", MESSAGE_LEVEL_ERROR);
				}
			}
			else{
				client_msg = "recognition_failed('could not allocate NLResult structure').";
				LogEvent(__FUNCTION__, "Failed to convert regulus message: could not allocate NLResult structure", MESSAGE_LEVEL_ERROR);
			}
		}
		else{
			client_msg = "recognition_failed('interpretation failed: ";
			client_msg += n.GetReasonAsString();
			client_msg += "').";
			LogEvent(__FUNCTION__, "Failed to convert regulus message: could not allocate NLResult structure", MESSAGE_LEVEL_ERROR);
		}

		ReplyToClient(client_msg.c_str());
	}

	LeaveCriticalSection(&m_critical_section);
}

void RegulusSpeechServer::AbortInterpretation(){
	EnterCriticalSection(&m_critical_section);

	if(m_ongoing_interpretation){
		m_rc_engine->Abort(m_interpretation_id);
	}

	LeaveCriticalSection(&m_critical_section);
}

#ifdef WIN32
bool RegulusSpeechServer::Recognize(const char *grammar){
	if(StartRecognition(grammar)){
		WaitForSingleObject(m_h_recognition_event, INFINITE);
		return true;
	}
	else{
		return false;
	}
}
#endif

bool RegulusSpeechServer::AbortRecognition()
{
	LogEvent(__FUNCTION__, "entering", MESSAGE_LEVEL_DEBUG);
	bool returnValue = false;

	EnterCriticalSection(&m_critical_section);

	if(m_recognition_requested || m_ongoing_recognition){
		m_rc_engine->Abort(m_recognition_id);
		returnValue = true;
	}

	LeaveCriticalSection(&m_critical_section);
	LogEvent(__FUNCTION__, "exiting", MESSAGE_LEVEL_DEBUG);
	return returnValue;
}

bool RegulusSpeechServer::StartRecording(const char *const file)
{
	LogEvent(__FUNCTION__, "entering", MESSAGE_LEVEL_DEBUG);
	bool returnValue = false;

	EnterCriticalSection(&m_critical_section);

	if(! m_ongoing_recognition ){
		m_recognition_id = m_rc_engine->GetUniqueID();
		if(NULL != file){
			m_rc_engine->RecordUtterance(file, m_recognition_id);
			returnValue = true;
		}		
	}

	LeaveCriticalSection(&m_critical_section);
	LogEvent(__FUNCTION__, "exiting", MESSAGE_LEVEL_DEBUG);
	return returnValue;
}

bool RegulusSpeechServer::StartRecognition(const char *const grammar, const char *const file)
{
	LogEvent(__FUNCTION__, "entering", MESSAGE_LEVEL_DEBUG);
	bool returnValue = false;

	EnterCriticalSection(&m_critical_section);

	if(! m_ongoing_recognition ){
		m_recognition_id = m_rc_engine->GetUniqueID();
		if(NULL == file){
			m_rc_engine->RecognizeUtterance(grammar, m_recognition_id);
		}
		else{
			LogEvent(__FUNCTION__, "grammar:", MESSAGE_LEVEL_DEBUG);
			LogEvent(__FUNCTION__, grammar, MESSAGE_LEVEL_DEBUG);
			LogEvent(__FUNCTION__, "recognizing from file:", MESSAGE_LEVEL_DEBUG);
			LogEvent(__FUNCTION__, file, MESSAGE_LEVEL_DEBUG);

			m_rc_engine->RecognizeFile(grammar, file, m_recognition_id);
		}
		returnValue = true;
	}

	LeaveCriticalSection(&m_critical_section);
	LogEvent(__FUNCTION__, "exiting", MESSAGE_LEVEL_DEBUG);
	return returnValue;
}

void RegulusSpeechServer::HandleRecognitionStopped(RecognitionStoppedNotification const & n)
{
	LogEvent(__FUNCTION__, "entering", MESSAGE_LEVEL_DEBUG);
	int reason = n.GetReason();

	EnterCriticalSection(&m_critical_section);
	ostringstream clientMsg;

	switch (reason){
	case RecognitionStoppedNotification::COMPLETED:
	{

		//
		// Retrieve recognition result and broadcast to interested parties
		//
		RecResultType recResultType;
		const RecResult *recResult;

		NuanceStatus nuanceStatus = NUANCE_ERROR;

		//
		// get RecResult structure
		//
		recResult = n.GetRecResult();
		if(recResult != NULL){

			//
			// check if we got something interesting
			//
			nuanceStatus = RecResultGetType(recResult, &recResultType);

			if((NUANCE_OK == nuanceStatus) && (RECOGNITION == recResultType)){
				char regulusMessage[2*MAX_MESSAGE_SIZE];
				int regulus_conversion = SPrintRecResultInRegulusForm(regulusMessage, sizeof(regulusMessage), m_nl_result, recResult, GetNBest());
				if( regulus_conversion >= 0){

					clientMsg << regulusMessage << ".";

					LogEvent(__FUNCTION__, "clientMsg set to ", MESSAGE_LEVEL_DEBUG);
					LogEvent(__FUNCTION__, clientMsg.str().c_str(), MESSAGE_LEVEL_DEBUG);
				}
				else{
					LogEvent(__FUNCTION__, "Failed to convert regulus message", MESSAGE_LEVEL_ERROR);
				}
			}
			else if(NUANCE_OUTPUT_BUFFER_TOO_SMALL == nuanceStatus){
				LogEvent(__FUNCTION__, "Output buffer too small to hold recognition result. Increase buffer and recompile application.", MESSAGE_LEVEL_ERROR);
				clientMsg << "recognition_failed('Buffer too small').";
			}
			else if(NUANCE_OK != nuanceStatus){
				LogEvent(__FUNCTION__, "Could not get result: ", MESSAGE_LEVEL_ERROR);
				LogEvent(__FUNCTION__, NuanceStatusMessage(nuanceStatus), MESSAGE_LEVEL_ERROR);
				clientMsg << "recognition_failed('" << NuanceStatusMessage(nuanceStatus) << "').";
			}
			else{
				LogEvent(__FUNCTION__, "No recognition result", MESSAGE_LEVEL_ERROR);
				LogEvent(__FUNCTION__, NuanceStatusMessage(nuanceStatus), MESSAGE_LEVEL_ERROR);
				clientMsg << "recognition_failed('no or empty result').";
			}
		}
		else{
				LogEvent(__FUNCTION__, "no rec result available", MESSAGE_LEVEL_ERROR);
				clientMsg << "recognition_failed('no result').";
		}

		break;
	}
	case RecognitionStoppedNotification::ABORTED: {

		LogEvent(__FUNCTION__, "Recognition aborted: ", MESSAGE_LEVEL_DEBUG);
		LogEvent(__FUNCTION__, n.GetReasonAsString(), MESSAGE_LEVEL_DEBUG);
		clientMsg << "recognition_failed('" << n.GetReasonAsString() << "').";

		break;
	}
	default: {

		LogEvent(__FUNCTION__, "Could not recognize:  ", MESSAGE_LEVEL_DEBUG);
		LogEvent(__FUNCTION__, n.GetReasonAsString(), MESSAGE_LEVEL_DEBUG);
		clientMsg << "recognition_failed('" << n.GetReasonAsString() << "').";
	}


	}

	//
	// Send result to client
	//
	LogEvent(__FUNCTION__, "Sending reply:", MESSAGE_LEVEL_DEBUG);
	LogEvent(__FUNCTION__, clientMsg.str().c_str(), MESSAGE_LEVEL_DEBUG);
	ReplyToClient(clientMsg.str().c_str());

#ifdef WIN32
	PulseEvent(m_h_recognition_event);
#endif

	m_ongoing_recognition = false;

	if((true == m_recording_requested) && (false == m_ongoing_recording)){
		LogEvent(__FUNCTION__, "Calling RCEngine::RecordUtterance()", MESSAGE_LEVEL_DEBUG);

		m_recording_id = m_rc_engine->GetUniqueID();
		m_rc_engine->RecordUtterance(m_record_to, m_recording_id);
		m_recording_requested = false;
		LeaveCriticalSection(&m_critical_section);
	}
	else{
		LeaveCriticalSection(&m_critical_section);
	}
	LogEvent(__FUNCTION__, "exiting", MESSAGE_LEVEL_DEBUG);
}

bool RegulusSpeechServer::AbortRecording()
{
	LogEvent(__FUNCTION__, "entering", MESSAGE_LEVEL_DEBUG);
	bool returnValue = false;

	EnterCriticalSection(&m_critical_section);

	if(m_recording_requested || m_ongoing_recording){
		m_rc_engine->Abort(m_recording_id);
		returnValue = true;
	}

	LeaveCriticalSection(&m_critical_section);
	LogEvent(__FUNCTION__, "exiting", MESSAGE_LEVEL_DEBUG);
	return returnValue;
}

void RegulusSpeechServer::SetParameter(const char *const param, const char *const val, const char *const type){
	LogEvent(__FUNCTION__, "entering", MESSAGE_LEVEL_DEBUG);
	char const * param_type;

	if(type == NULL){
		ParamType nuanceParamType;
		NuanceStatus nuanceStatus = NuanceParameterGetType(param, &nuanceParamType);
		if(NUANCE_OK == nuanceStatus){
			switch(nuanceParamType){
				case TYPE_INT:{
					param_type = "int";
					break;
				}
				case TYPE_BOOL:{
					param_type = "int"; // ???					
					break;
				}
				case TYPE_FLOAT :{
					param_type = "float";
					break;
				}
				case TYPE_STRING:{
					param_type = "string";
					break;
				}
				default:{
					break;
				}
			}
		}
	}
	else{
		param_type = type;
	}

	m_set_parameter_id = m_rc_engine->GetUniqueID();
	if(strcmp_case_insensitive(param_type, "int") == 0){	
		m_rc_engine->SetParameter(param, atoi(val), m_set_parameter_id);	
	}
	else if(strcmp_case_insensitive(param_type, "float") == 0){
		m_rc_engine->SetParameter(param, atof(val), m_set_parameter_id);	
	}
	else if(strcmp_case_insensitive(param_type, "string") == 0){
		m_rc_engine->SetParameter(param, val, m_set_parameter_id);	
	}
	else{
		LogEvent(__FUNCTION__, "unknown value type: ", MESSAGE_LEVEL_INFO);
		LogEvent(__FUNCTION__, type, MESSAGE_LEVEL_INFO);	
	}
	LogEvent(__FUNCTION__, "exiting", MESSAGE_LEVEL_DEBUG);
}

void RegulusSpeechServer::HandleRecordingStopped(RecordingStoppedNotification const &n){
	LogEvent(__FUNCTION__, "entering", MESSAGE_LEVEL_DEBUG);
	LogEvent(__FUNCTION__, "exiting", MESSAGE_LEVEL_DEBUG);
}

void RegulusSpeechServer::LogEvent(const char *function, const char *message, int level=MESSAGE_LEVEL_DEFAULT){
	if(m_log_stream.is_open()){
		EnterCriticalSection(&m_logging_synchronization);
		switch(level){
			case MESSAGE_LEVEL_DEBUG:
				if(m_print_debug_output){
					m_log_stream << "DEBUG: " << function << ": " << message << endl;
				}
				break;
			case MESSAGE_LEVEL_INFO:
				m_log_stream << "INFO: " << function << ": " << message << endl;
				break;
			case MESSAGE_LEVEL_WARNING:
				m_log_stream << "WARNING: " << function << ": " << message << endl;
				break;
			case MESSAGE_LEVEL_ERROR:
				m_log_stream << "ERROR: " << function << ": " << message << endl;
				break;
			default:
				m_log_stream << "INFO: " << function << ": " << message << endl;
				break;
		}
		m_log_stream.flush();
		LeaveCriticalSection(&m_logging_synchronization);
	}
}

void RegulusSpeechServer::LogEvent(const char *function, int value, int level=MESSAGE_LEVEL_DEFAULT){
	if(m_log_stream.is_open()){
		EnterCriticalSection(&m_logging_synchronization);
		switch(level){
			case MESSAGE_LEVEL_DEBUG:
				if(m_print_debug_output){
					m_log_stream << "DEBUG: " << value << ": " << value << endl;
				}
				break;
			case MESSAGE_LEVEL_INFO:
				m_log_stream << "INFO: " << function << ": " << value << endl;
				break;
			case MESSAGE_LEVEL_WARNING:
				m_log_stream << "WARNING: " << function << ": " << value << endl;
				break;
			case MESSAGE_LEVEL_ERROR:
				m_log_stream << "ERROR: " << function << ": " << value << endl;
				break;
			default:
				m_log_stream << "INFO: " << function << ": " << value << endl;
				break;
		}
		m_log_stream.flush();
		LeaveCriticalSection(&m_logging_synchronization);
	}
}

void RegulusSpeechServer::LogEvent(const char *function, float value, int level=MESSAGE_LEVEL_DEFAULT){
	if(m_log_stream.is_open()){
		EnterCriticalSection(&m_logging_synchronization);
		switch(level){
			case MESSAGE_LEVEL_DEBUG:
				if(m_print_debug_output){
					m_log_stream << "DEBUG: " << value << ": " << value << endl;
				}
				break;
			case MESSAGE_LEVEL_INFO:
				m_log_stream << "INFO: " << function << ": " << value << endl;
				break;
			case MESSAGE_LEVEL_WARNING:
				m_log_stream << "WARNING: " << function << ": " << value << endl;
				break;
			case MESSAGE_LEVEL_ERROR:
				m_log_stream << "ERROR: " << function << ": " << value << endl;
				break;
			default:
				m_log_stream << "INFO: " << function << ": " << value << endl;
				break;
		}
		m_log_stream.flush();
		LeaveCriticalSection(&m_logging_synchronization);
	}
}

// The user may want nbest but this will be confirmed
// after the corresponding acknowledgment is received
bool RegulusSpeechServer::GetNBest(){
	return m_nbest;
}

void RegulusSpeechServer::SetNBest(bool nbest){
	m_nbest = nbest;
}
