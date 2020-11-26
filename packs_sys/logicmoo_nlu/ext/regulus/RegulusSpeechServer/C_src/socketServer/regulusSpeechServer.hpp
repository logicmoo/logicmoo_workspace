#ifdef WIN32
#pragma once
#endif

//
// make sure multithreaded libraries are used
//
#ifndef _MT
#define _MT
#endif

#ifndef _CPP
#define _CPP
#endif


#ifndef _CPP_REGULUS_SPEECH_SERVER
#define _CPP_REGULUS_SPEECH_SERVER

#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
// #include <string.h>

#ifdef WIN32


#include <Winsock2.h>
#include <process.h>


#else

#include <pthread.h>
#include <thread.h>
#include <synch.h>
#include <sys/types.h>
#include <sys/socket.h>

#define CRITICAL_SECTION mutex_t
#define HANDLE unsigned int *
#define SOCKET int
#define TCHAR char
#define LPVOID void *
#define INVALID_SOCKET -1
#define SOCKET_ERROR -1
// static HANDLE CreateThread(void *ign, int ign, void*(*start_routine)(void *), void *_tread_arg, void *ign);

#endif

//
// Nuance includes
//
#include "dispatcher.hpp"				// Dispatcher class
#include "nuance-config.h"				// NuanceConfig class
#include "param-constants.h"			// NCP_GEN_PARTIAL_RESULTS, etc.
#include "nuance-status-codes.h"		// NuanceStatus
#include "rc-engine.hpp"				// RCEngine
#include "notifiable-adaptor.hpp"		// NotifiableAdapter class
#include "recognition-notifications.hpp"// RecognitionStartedNotification, etc.
#include "recresult.h"					// RecResultString, etc.

//
// Regulus includes
//
#include "nl_regulus_utils.hpp"
#include "PlaybackBuffer.hpp"


// The __FUNCTION__ macro is used in LogEvent to pass the function name
// VC6 does not define __FUNCTION__, so let's work around this
// by setting it to the empty string.
// Compile with VC7 to get the function name in the output trace.
#ifndef __FUNCTION__
#define __FUNCTION__ ""
#endif

#define DEFAULT_SERVER_SOCKET_PORT "1974"
#define MAX_MESSAGE_SIZE 2048

#define MESSAGE_LEVEL_DEFAULT 256
#define MESSAGE_LEVEL_INFO 254
#define MESSAGE_LEVEL_WARNING 253
#define MESSAGE_LEVEL_ERROR 252
#define MESSAGE_LEVEL_DEBUG 251

using namespace std;

// The debugger can't handle symbols more than 255 characters long.
// STL often creates symbols longer than that.
// When symbols are longer than 255 characters, the warning is issued.
#pragma warning(disable:4786)

class RegulusSpeechServer: public NotifiableAdapter
{
private:
	void ClearPlaybackBuffer(void);
	// void AppendPromptsToPlaybackBuffer(const char * const *prompts);
	// void FreePlaybackBuffer();
	void HandleRecognitionStopped(RecognitionStoppedNotification const & n);
	void HandlePlaybackStopped(PlaybackStoppedNotification const & n);
	void HandleRecordingStopped(RecordingStoppedNotification const &n);
	void HandleInitializationCompleted(InitializationCompletedNotification const & n);
	void HandleAcknowledgment(AcknowledgmentNotification const & n);
	void HandleParameterGotten(ParameterGottenNotification const & n);
	void HandleInterpretationStopped(const InterpretationStoppedNotification &n);
	HANDLE CreateSocketServerThread();
	bool CreateServerSocket(const char*);
	void SocketServerMainLoop();
#ifdef WIN32
	static DWORD WINAPI DispatchThread(LPVOID);
#else
	static void *DispatchThread(void *);
#endif
	bool ReplyToClient(const char* const);
	bool InterpretCommand(string command);


	void LogEvent(const char *function, const char *message, int level);
	void LogEvent(const char *function, int value, int level);
	void LogEvent(const char *function, float value, int level);


	ofstream				m_log_stream;
	NuanceConfig *			m_config;			// to get command-line parameters
	Dispatcher *			m_dispatcher;		// to handle event scheduling
	RCEngine *				m_rc_engine;		// to get RecClient functionality
	NLResult *				m_nl_result;

	CRITICAL_SECTION		m_critical_section;
	CRITICAL_SECTION		m_client_connection_write_lock;
	CRITICAL_SECTION		m_logging_synchronization;

#ifdef WIN32
	HANDLE					m_h_recognition_event;
#endif
	HANDLE					m_hthread;
	SOCKET					m_server_socket;
	SOCKET					m_client_connection;
	char *					m_server_socket_port;
	unsigned int			m_recording_id;				// id for tracking ongoing recording
	unsigned int			m_recognition_id;			// id for tracking ongoing recognition
	unsigned int			m_get_parameter_id;			// id to track ongoing get paramater actions
	unsigned int			m_set_parameter_id;			// id to track ongoing set paramater actions
	unsigned int			m_interpretation_id;		// id to track ongoing interpretation actions
	unsigned int			m_playback_id;				// id to abort ongoing playbacks

	bool					m_is_rc_engine_initialized;
	bool					m_print_debug_output;
	bool					m_ongoing_recording;		// recognition status
	bool					m_ongoing_recognition;		// recognition status
	bool					m_ongoing_playback;			// playback status
	bool					m_ongoing_interpretation;	// interpretation status
	bool					m_recognition_requested;
	bool					m_recording_requested;		// flag to signal a recording request
	
	PlaybackBuffer			m_playback_buffer;

	int						m_argc;
	TCHAR **				m_argv;

	char *					m_record_to;				// file to record to

public:
	RegulusSpeechServer(int, TCHAR **, NuanceConfig*, Dispatcher *);
	~RegulusSpeechServer(void);
	NuanceStatus Startup(void);

	bool IsNuanceChannelInitialized();
	bool GetParameter(const char *const parameter);
	void SetParameter(const char *const param, const char *const val, const char *const type=NULL);

	bool StartPlayback(const char * const *prompts=NULL);
	bool AbortPlayback(void);

	bool StartRecognition(const char *const grammar, const char *const file=NULL);
	bool AbortRecognition(void);
	bool Recognize(const char *const grammar);
	void StartInterpretation(const char *const grammar, const char *const text);
	void AbortInterpretation(void);

	bool StartRecording(const char *fileName);
	bool AbortRecording(void);
	bool Record(const char *fileName);

public:
	bool GetNBest();
	void SetNBest(bool nbest);

private:
	// The user may want nbest but this will be confirmed
	// after the corresponding acknowledgment is received
	bool m_want_nbest;
	bool m_nbest;

};

#endif
