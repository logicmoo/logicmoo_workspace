// regulus_server.h 
//
// Header for regulus_server application.

#define MAX_TABLE_SIZE 100
#define MAX_GSL_ATOM_LENGTH 100
#define MAX_GRAMMAR_NAME_LENGTH 100
#define MAX_PROMPT_STRING_LENGTH 2000
#define MAX_RECOGNISED_STRING_LENGTH 2000
#define MAX_FILE_NAME_LENGTH 200

#define SYMBOL_TABLE_SIZE 1000

// Symbolic constants

enum symbols { 
	NO_REQUEST, SAY_NUMBER, SAY_FILE, SAY_TTS, SET_OUTPUT_VOLUME, RECOGNISE, RECORD, CLEAN_UP
};


/*-----------------------------------------------------------------------*/

// Global function prototypes


// regulus_server.cpp

int main(int argc, char *argv[]);
void MainLoopFn(App *app);
void GetClientRequestsUntilRecognitionRequested(App *app, int *request_type, char *string_arg);
void InitFn(App *app);
void RootEntryFn(App *app);
void RootPostRecSucceeded(App *app);
void RootPostRecFailed(App *app, int status);
void RootPostRecFn(App *app);

// nlval_to_regulus.cpp

void PrintRecResultInRegulusForm(FILE *stream, App *app);
void PrintNLValueInRegulusForm(FILE *stream, NLValue *nl_val, NLValueType type);
void PrintNLIntValueInRegulusForm(FILE *stream, NLValue *nl_val) ;
void PrintNLStringValueInRegulusForm(FILE *stream, NLValue *nl_val);
void PrintNLListValueInRegulusForm(FILE *stream, NLValue *list) ;
void PrintNLStructureValueInRegulusForm(FILE *stream, NLValue *structure);
void StringToPrologForm(char *in, char *out);

// regulus_filetalk.cpp

int InitFiletalk(char *config_file);
int InitFiletalkConstantsFromConfigFile(char *config_file);
void SendRecordDoneNotificationToClient(char *recorded_file);
void SendFailedRecResultToClient(int status);
void SendRecResultToClient(App *app);
void GetClientRequest(int *request_type, char *string_arg, int *number_arg);
int ReadClientRequestFile(char *file, int *request_type, char *string_arg, int *number_arg);
void UpdateClientFilePointerFile();
void UpdateServerFilePointerFile();
void CurrentClientFile(char *file);
void CurrentServerFile(char *file);
void MarkCurrentClientFileAsProcessed();
void MarkCurrentServerFileAsProcessed();
int NumberToSuffixString(int i, char* s);





