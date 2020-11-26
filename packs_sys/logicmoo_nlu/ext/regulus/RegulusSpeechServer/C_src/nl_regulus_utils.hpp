#define MAX_TABLE_SIZE 100
#define MAX_GSL_ATOM_LENGTH 100
#define MAX_GRAMMAR_NAME_LENGTH 100
#define MAX_PROMPT_STRING_LENGTH 2000
#define MAX_RECOGNISED_STRING_LENGTH 2000
#define MAX_FILE_NAME_LENGTH 200

#define SYMBOL_TABLE_SIZE 1000



extern "C" int SPrintRecResultInRegulusForm(char *buffer, size_t buff_size, NLResult *nl_result, const RecResult *rec_result);
extern "C" int SPrintNLValueInRegulusForm(char *buffer, size_t buff_size, const NLValue *nl_val, NLValueType type);
extern "C" int SPrintNLIntValueInRegulusForm(char *buffer, size_t buff_size, const NLValue *nl_val) ;
extern "C" int SPrintNLStringValueInRegulusForm(char *buffer, size_t buff_size, const NLValue *nl_val);
extern "C" int SPrintNLListValueInRegulusForm(char *buffer, size_t buff_size, const NLValue *list) ;
extern "C" int SPrintNLStructureValueInRegulusForm(char *buffer, size_t buff_size, const NLValue *structure);
extern "C" void StringToPrologForm(const char *in, char *out);
