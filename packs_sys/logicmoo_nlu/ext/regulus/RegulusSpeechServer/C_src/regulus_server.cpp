/*  
regulus_server.cpp  -- server to provide file-based Prolog interface to Regulus grammars
*/ 
 
// NEW

#include <stdlib.h>
#include <string.h>
#include "app.h"
#include "nl.h"
#include "recresult.h"
#include "regulus_server.h"
 
/*-----------------------------------------------------------------------*/

int main(int argc, char *argv[])
{	
	App *app;
    app = AppNew(&argc, argv);

	if (app == NULL) {
		printf("\nError: unable to initialise filetalk.");
		exit(-1);
	}

	// arg 1 should be name of config file
	if ( argc > 0 && InitFiletalk(argv[1]) ) {
		printf("\nFiletalk initialised.\n");
	}
	else {
		printf("\nError: unable to initialise filetalk.");
		exit(-1);
	}

	AppCreateStateClass(app, "root", NULL, RootEntryFn, RootPostRecFn);

	AppCreateState(app, "main_loop", "root", MainLoopFn);
	AppCreateState(app, "init", "root", InitFn);

	AppGo(app, "init");
        return 0;
}

/*-----------------------------------------------------------------------*/

// Top level of main loop:
// 
//   - Get client request
//
//   - If request is an append, 
//		- do it
//		- acknowledge request
//
//   - If request is a recognise, 
//		- set grammar appropriately
//		- do recognition
//		- send back recognition result

void MainLoopFn(App *app)
{
	int request_type = NO_REQUEST;
	char string_arg[MAX_PROMPT_STRING_LENGTH];

	GetClientRequestsUntilRecognitionRequested(app, &request_type, string_arg);

	switch ( request_type ) {
	case RECOGNISE:
		printf("\nRecognising with grammar %s\n", string_arg);
		AppSetGrammar(app, string_arg);
	
		if(!AppRecognize(app))
			return;

		SendRecResultToClient(app);

		AppGotoSelf(app);
		break;

	case RECORD:
		printf("\nRecording to file %s\n", string_arg);
		AppSetRecordFilename(app, string_arg);
		
		if(!AppRecord(app))
			return;

		SendRecordDoneNotificationToClient(string_arg);

		AppGotoSelf(app);
		break;

	default:
		printf("\n*** Error: unknown type of client request %d returned by GetClientRequestsUntilGrammarRequested\n", request_type);
	}
}

void GetClientRequestsUntilRecognitionRequested(App *app, int *request_type, char *string_arg)
{
	int number_arg;

	while ( true ) {
		GetClientRequest(request_type, string_arg, &number_arg);

		switch ( *request_type ) {
		case SAY_NUMBER:
			AppAppendNumberPrompt(app, number_arg, 0);
			//AppPlayPrompts(app, 1);
			break;
		case SAY_FILE:
			AppAppendPrompt(app, string_arg);
			//AppPlayPrompts(app, 1);
			break;
		case SAY_TTS:
			AppAppendTTSPrompt(app, string_arg);
			//AppPlayPrompts(app, 1);
			break;
		case CLEAN_UP:
			AppPlayPrompts(app, 1);
			printf("\nSent request to play queued messages\n");
			break;
		case SET_OUTPUT_VOLUME:
			AppSetIntParameter(app, "audio.OutputVolume", number_arg);
			break;
		case RECOGNISE:
		case RECORD:
			return;
		default:
			printf("\n*** Error: unknown type of client request %d returned by GetClientRequest\n", request_type);
		}
	}
}

/*-----------------------------------------------------------------------*/

// Functionality for initial state: say hello and move to the "choose house" state.

void InitFn(App *app)
{	
	//printf("Sending test message to TTS...\n\n");
	//AppAppendTTSPrompt(app, "\nStarted app - testing text to speech");
	//AppPlayPrompts(app, 1);

	AppSetIntParameter(app, "rec.ConfidenceRejectionThreshold", 0);
	AppGoto(app, "main_loop");
}

/*-----------------------------------------------------------------------*/

// Pre-recognition initialization

void RootEntryFn(App *app)
{
	AppSetBeginSpeechTimeout(app, (float)30.0);
	AppSetEndSpeechTimeout(app, (float)30.0);
	AppSetEndRecognitionTimeout(app, (float)30.0);
	AppSetRejectionThreshold(app, 45);
	AppSetBargeInAllowed(app, TRUE);
}

// Post-recognition clean-up

void RootPostRecSucceeded(App *app)
{
}

void RootPostRecFailed(App *app, int status)
{
	SendFailedRecResultToClient(status);
}


void RootPostRecFn(App *app)
{
	UttStatus status = AppGetUttStatus(app);
	switch(status){
	case UTT_NORMAL:
	case UTT_REJECT:	
	case UTT_SESSION_ABORTED:
		RootPostRecSucceeded(app);
		break;
	//case UTT_REJECT:
	case UTT_BEGIN_SPEECH_TIMEOUT:
	case UTT_END_SPEECH_TIMEOUT:
	case UTT_END_RECOGNITION_TIMEOUT:
	case UTT_ERROR:	
		RootPostRecFailed(app, status);
        AppGotoSelf(app);
		break;
	default:
		fprintf(stderr, "Unexpected utterance status\n");
		RootPostRecFailed(app, status);
		break;
  }
}


/*-----------------------------------------------------------------------*/

	
