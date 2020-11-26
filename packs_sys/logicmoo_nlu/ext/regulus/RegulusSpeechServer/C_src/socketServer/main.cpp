//
// main.cpp : Defines the entry point for the console application.
//

// #include "stdafx.h"

#include <iostream>

#include <signal.h>                  // signal()
#include <stdarg.h>                  // va_list, etc.
#include <string.h>                  // strcmp, etc.

//
// NUANCE includes
//
#include "dispatcher.hpp"            // Dispatcher class
#include "nuance-config.h"           // NuanceConfig class
#include "nuance-status-codes.h"     // NuanceStatus
#include "param-constants.h"         // NCP_GEN_PARTIAL_RESULTS, etc.

#include "regulusSpeechServer.hpp"


using namespace std;


//----------------------------------------------------------------------------
// Global variables and constants
//----------------------------------------------------------------------------
// Signal handling information
typedef void (*SigHandlerType)(int);
enum EnumSigSet { EnumSIGINT, EnumSIGTERM, EnumSIGSEGV, EnumSIGBUS, EnumMAXSIG };
static int SigSetSig[EnumMAXSIG] = { 
#if !defined(WIN32) && !defined(NUANCE_WIN32)
    SIGBUS,
#endif
    SIGINT,
    SIGTERM,
    SIGSEGV
};

static char * SigSetMsg[EnumMAXSIG] = {
#if !defined(WIN32) && !defined(NUANCE_WIN32)
    "SIGBUS, memory alignment error",
#endif
    "SIGINT, probably ^C",
    "SIGTERM, probably via 'kill'",
    "SIGSEGV, memory referencing error"
};

static SigHandlerType g_old_sig_handler[EnumMAXSIG];

// Program name
static char const * program;

//----------------------------------------------------------------------------
// Function prototype declarations
//----------------------------------------------------------------------------
void PrintUsage (void);
void SigHandler (int sig);

int main(int argc, TCHAR* argv[], TCHAR* envp[])
{
	int nRetCode = 0;

	if( setvbuf(stdout, NULL, _IONBF, 0) != 0 ) {
		cerr << "WARNING: Disabling buffering on stdout failed" << endl;
	}
	if( setvbuf(stderr, NULL, _IONBF, 0) != 0 ) {
		cerr << "WARNING: Disabling buffering on stderr failed" << endl;
	}

	program = *argv;
	NuanceStatus status;
	
	//------------------------------------------------------------------------
	// Set up signal handlers
	//------------------------------------------------------------------------  
	for (int i = 0; i < EnumMAXSIG; i++) {
		g_old_sig_handler[i] = signal(SigSetSig[i], SigHandler);
	}
	
	//------------------------------------------------------------------------
	// Build NuanceConfig object (out of command line arguments)
	//------------------------------------------------------------------------
	NuanceConfig * config = NuanceConfigBuildFromCommandLine(&argc, argv, 1, &status);
	if ( (NULL == config) || (NUANCE_OK != status) ) {
		printf("NuanceConfig construction failed because %s\n", NuanceStatusMessage(status));
		PrintUsage();
		return __LINE__;
	}
	else{
		printf("Nuance configuration build.");
	}
	
	//------------------------------------------------------------------------
	// Create the nuance dispatcher.
	//------------------------------------------------------------------------
	Dispatcher * d = new Dispatcher(config, status);
	if (NUANCE_OK != status) {
		cerr << "Dispatcher construction failed because %s" << NuanceStatusMessage(status) << endl;
		delete d;
		d = NULL;
		NuanceConfigFree(config);
		config = NULL;
		return __LINE__;
	}
	printf("Dispatcher built.");
	
	//------------------------------------------------------------------------
	// Create the SimpleRCApp object.
	// rec-client engine is created through the application.
	//
	// If the application has been constructed successfully, it owns the
	// NuanceConfig object.
	//------------------------------------------------------------------------
	RegulusSpeechServer *speechServer = new RegulusSpeechServer(argc, argv, config, d);
	if (NUANCE_OK != status) {
		printf("application construction failed because %s\n", NuanceStatusMessage(status));
		delete speechServer;
		speechServer = NULL;
		delete d;
		d = NULL;
		NuanceConfigFree(config);
		config = NULL;
		return __LINE__;
	}
	printf("Application created.\n");
	
	//------------------------------------------------------------------------
	// Create and initialize the RCEngine object. This can only be called once
	// the RCApp object has been constructed successfully.
	//------------------------------------------------------------------------
	
	status = speechServer->Startup();
	if (NUANCE_OK != status)
	{
		cerr << "RCEngine initialization failed because '" <<  NuanceStatusMessage(status) << "'" << endl;
		delete speechServer;
		speechServer = NULL;
		delete d;
		d = NULL;
		NuanceConfigFree(config);
		config = NULL;
		return __LINE__;
	}
	cout << "Application object created." << endl;
	
	
	//------------------------------------------------------------------------
	// Activate the engine via the dispatcher.
	// The Dispatcher will return if we cannot connect to/create an RCEngine
	//------------------------------------------------------------------------
	d->Dispatch();
	
	cout << "Dispatcher returned, deleting app." << endl;

	//------------------------------------------------------------------------
	// Delete and free objects.
	//------------------------------------------------------------------------
	
	if(speechServer){
		cout << "deleting speechServer" << endl;
		delete speechServer;
		speechServer = NULL;
	}
	
	if(d){
		cout << "deleting Dispatcher" << endl;
		delete d;
		d = NULL;
	}
	
	if(config){
		NuanceConfigFree(config);
		config = NULL;
	}
	
	//------------------------------------------------------------------------
	// Return
	//------------------------------------------------------------------------
	nRetCode = 0;

	cout << "exiting main" << endl;
	return nRetCode;
}

void PrintUsage (void)
{
    cout << endl;
    cout << "Usage: " << program << " \\" << endl;
    cout << "        -package <package dir> \\" << endl;
    cout << "        [nuance parameters] \\" << endl;
	cout << "        [-port <tcp port the server listens for connection - default is " << DEFAULT_SERVER_SOCKET_PORT << ">] \\" << endl;
	cout << "        [-v] \\" << endl;
	cout << "        [-f <log file>]" << endl;
}


void SigHandler(int sig) {
	try{
		// Print signal information
		cerr << endl;
		cerr << "***** SIGNAL RECEIVED *****" << endl;
		int i = 0;
		for (i = 0; i < EnumMAXSIG; i++) {
			if (SigSetSig[i] == sig) {
				cerr << SigSetMsg[i] << endl;
				break;
			}
		}

		// Put back old signal handlers
		for (i = 0; i < EnumMAXSIG; i++) {
			signal(SigSetSig[i], g_old_sig_handler[i]);
		}

		// Exit application
		exit(-1);
	}
	catch(...)
	{
		exit (0);
	}
}