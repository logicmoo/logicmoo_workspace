/*  
regulus_filetalk.cpp  -- file-based protocol for communication between Regulus server and client
*/ 
 
// NEW

#include <stdlib.h>
#include <string.h>
#include "app.h"
#include "nl.h"
#include "recresult.h"
#include "regulus_server.h"

//#define CONFIG_FILE "REGULUS_SERVER.CFG"

char client_file_pointer_file[MAX_FILE_NAME_LENGTH];
char server_file_pointer_file[MAX_FILE_NAME_LENGTH];

char client_file_prefix[MAX_FILE_NAME_LENGTH];
char server_file_prefix[MAX_FILE_NAME_LENGTH];

int client_file_serial_number = 0;
int server_file_serial_number = 0;

/*-----------------------------------------------------------------------*/

int InitFiletalk(char *config_file)
{
	if ( InitFiletalkConstantsFromConfigFile(config_file) ) {
		UpdateClientFilePointerFile();
		UpdateServerFilePointerFile();
		return 1;
	}
	else {
		return 0;
	}
}

int InitFiletalkConstantsFromConfigFile(char *config_file)
{
	char keyword[100];
	char filename[MAX_FILE_NAME_LENGTH];
	FILE *stream;
	bool config_file_ok = true;

	stream = fopen( config_file, "r");

	if (stream == NULL) {
		printf("\nError: unable to open filerec config file %s\n", config_file);
		return 0;
	} 

	strcpy(client_file_pointer_file, "");
	strcpy(server_file_pointer_file, "");
	strcpy(client_file_prefix, "");
	strcpy(server_file_prefix, "");

	printf("\nReading init file %s...\n\n", config_file);
   
	// Read in config info
	while ( fscanf( stream, "%s %s\n", keyword, filename) != EOF ) {

		if ( !strcmp("client_file_pointer_file", keyword ) ) {
			strcpy(client_file_pointer_file, filename);
		}
		else if ( !strcmp("server_file_pointer_file", keyword ) ) {
			strcpy(server_file_pointer_file, filename);
		}
		else if ( !strcmp("client_file_prefix", keyword ) ) {
			strcpy(client_file_prefix, filename);
		}
		else if ( !strcmp("server_file_prefix", keyword ) ) {
			strcpy(server_file_prefix, filename);
		}
		else {
			printf("\nError: unknown keyword %s in filerec config file %s\n", keyword, config_file);
			return 0;
		}

		printf("%s = %s\n", keyword, filename);
	}

	if ( !strcmp(client_file_pointer_file, "") ) {
		printf("\nError: client_file_pointer_file not defined in config file\n");
		config_file_ok = false;
	}
	if ( !strcmp(server_file_pointer_file, "") ) {
		printf("\nError: server_file_pointer_file not defined in config file\n");
		config_file_ok = false;
	}
	if ( !strcmp(client_file_prefix, "") ) {
		printf("\nError: client_file_prefix not defined in config file\n");
		config_file_ok = false;
	}
	if ( !strcmp(server_file_prefix, "") ) {
		printf("\nError: server_file_prefix not defined in config file\n");
		config_file_ok = false;
	}

	if ( config_file_ok = false ) {
		return 0;
	}
	else {
		printf("\nConfig file OK\n\n");
		return 1;
	}
}
		   
/*-----------------------------------------------------------------------*/

void SendRecResultToClient(App *app)
{
	char file[100];
	FILE *stream;

	CurrentServerFile(file);
	printf("Writing to Regulus server file %s\n", file);

	stream = fopen(file, "w");

	if (stream == NULL) {
		printf("\nError: unable to open file %s for writing\n", file);
	} else {
		PrintRecResultInRegulusForm(stream, app);	
		fclose(stream);
		MarkCurrentServerFileAsProcessed();
	}
}

void SendRecordDoneNotificationToClient(char *recorded_file)
{
	char prolog_format_recorded_file[MAX_PROMPT_STRING_LENGTH];
	StringToPrologForm(recorded_file, prolog_format_recorded_file);

	char file[100];
	FILE *stream;

	CurrentServerFile(file);
	printf("Writing to Regulus server file %s\n", file);

	stream = fopen(file, "w");

	if (stream == NULL) {
		printf("\nError: unable to open file %s for writing\n", file);
	} else {
		fprintf(stream, "file_recorded(%s).\n", prolog_format_recorded_file);
	}

	MarkCurrentServerFileAsProcessed();
	fclose(stream);
}


void SendFailedRecResultToClient(int status)
{
	char file[100];
	FILE *stream;

	CurrentServerFile(file);
	printf("Writing to Regulus server file %s\n", file);

	stream = fopen(file, "w");

	if (stream == NULL) {
		printf("\nError: unable to open file %s for writing\n", file);
	} else {
		switch (status) {
		case UTT_SESSION_ABORTED:
			fprintf(stream, "recognition_failed('UTT_SESSION_ABORTED').\n");
			break;
		case UTT_REJECT:
			fprintf(stream, "recognition_failed('UTT_REJECT').\n");
			break;
		case UTT_BEGIN_SPEECH_TIMEOUT:
			fprintf(stream, "recognition_failed('UTT_BEGIN_SPEECH_TIMEOUT').\n");
			break;
		case UTT_END_SPEECH_TIMEOUT:
			fprintf(stream, "recognition_failed('UTT_END_SPEECH_TIMEOUT').\n");
			break;
		case UTT_END_RECOGNITION_TIMEOUT:
			fprintf(stream, "recognition_failed('UTT_END_RECOGNITION_TIMEOUT').\n");
			break;
		case UTT_ERROR:	
			fprintf(stream, "recognition_failed('UTT_ERROR').\n");
			break;
		default:
			fprintf(stream, "recognition_failed('UNKNOWN_STATUS').\n");
			break;
		}

		MarkCurrentServerFileAsProcessed();
		fclose(stream);
	}
}

/*-----------------------------------------------------------------------*/

void GetClientRequest(int *request_type, char *string_arg, int *number_arg)
{
	char file[100];

	CurrentClientFile(file);

	printf("Waiting for Regulus client file %s\n", file);

	while ( !ReadClientRequestFile(file, request_type, string_arg, number_arg) ) {
			Sleep(200);
		}
	
	printf("\nRead file...\n");
	MarkCurrentClientFileAsProcessed();
	
}

int ReadClientRequestFile(char *file, int *request_type, char *string_arg, int *number_arg)
{
	char request_type_string[50];
	FILE *stream;

	stream = fopen( file, "r");

	if (stream == NULL) {
		return 0;
	} 

	fscanf( stream, "%s\n", request_type_string);

	if (!strcmp(request_type_string, "SAY_NUMBER")) {
		*request_type = SAY_NUMBER;
	}
	else if (!strcmp(request_type_string, "SAY_FILE")) {
		*request_type = SAY_FILE;
	}
	else if (!strcmp(request_type_string, "SAY_TTS")) {
		*request_type = SAY_TTS;
	}
	else if (!strcmp(request_type_string, "SET_OUTPUT_VOLUME")) {
		*request_type = SET_OUTPUT_VOLUME;
	}
	else if (!strcmp(request_type_string, "RECOGNISE")) {
		*request_type = RECOGNISE;
	}
	else if (!strcmp(request_type_string, "RECORD")) {
		*request_type = RECORD;
	}
	else if (!strcmp(request_type_string, "CLEAN_UP")) {
		*request_type = CLEAN_UP;
	}
	else {
		printf("\nError: unknown request type %s\n", request_type_string);
		return 0;
	}
   
	// Read in device info
	switch ( *request_type ) {
	case SAY_NUMBER:
	case SET_OUTPUT_VOLUME:
		fscanf( stream, "%d\n", number_arg);
		break;
	case SAY_FILE:
	case RECOGNISE:
	case RECORD:
		fscanf( stream, "%s\n", string_arg);
		//printf("\nPrompt file: %s\n", prompt);
		break;
	case SAY_TTS:
		fgets( string_arg, MAX_PROMPT_STRING_LENGTH, stream );
		//printf("\nPrompt string: %s", prompt);
		break;		
	case CLEAN_UP:
		break;
	default:
		printf("\nUnknown request type %d\n", *request_type);
		break;
	}


	fclose(stream);
	return 1;
}

/*-----------------------------------------------------------------------*/

void UpdateClientFilePointerFile()
{
	FILE *stream;

	stream = fopen(client_file_pointer_file, "w");

	if (stream == NULL) {
		printf("\nError: unable to open file %s for writing\n", client_file_pointer_file);
	} else {
		fprintf(stream, "next_client_file_serial_number(%d).\n", client_file_serial_number);	
		fclose(stream);
	}
}

void UpdateServerFilePointerFile()
{
	FILE *stream;

	stream = fopen(server_file_pointer_file, "w");

	if (stream == NULL) {
		printf("\nError: unable to open file %s for writing\n", server_file_pointer_file);
	} else {
		fprintf(stream, "next_server_file_serial_number(%d).\n", server_file_serial_number);	
		fclose(stream);
	}
}

/*-----------------------------------------------------------------------*/

void CurrentClientFile(char *file) 
{
	char suffix[10];

	NumberToSuffixString(client_file_serial_number, suffix);
	strcpy(file, client_file_prefix);
	strcat(file, suffix);
	strcat(file, ".txt");
}

void CurrentServerFile(char *file) 
{
	char suffix[10];

	NumberToSuffixString(server_file_serial_number, suffix);
	strcpy(file, server_file_prefix);
	strcat(file, suffix);
	strcat(file, ".txt");
}

void MarkCurrentClientFileAsProcessed() 
{
	client_file_serial_number++;
	UpdateClientFilePointerFile();
}

void MarkCurrentServerFileAsProcessed() 
{
	server_file_serial_number++;
	UpdateServerFilePointerFile();
}

/*-----------------------------------------------------------------------*/

int NumberToSuffixString(int i, char* s) 
{	
	if ( i > 9999 ) {
		return 0;
	}
	
	s[0] = '0' + i / 1000;
	s[1] = '0' + ( i % 1000 ) / 100;
	s[2] = '0' + ( i % 100 ) / 10;
	s[3] = '0' + ( i % 10 );
	s[4] = '\0';
	return 1;
}

/*-----------------------------------------------------------------------*/
