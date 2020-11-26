#include "StdAfx.h"

#include <stdio.h>

#include "recresult.h"					// RecResultString, etc.

#include "nl_regulus_utils.hpp"
#include "nuance-status-codes.h"		// NuanceStatus


int SPrintRecResultInRegulusForm(char *buffer, size_t buff_size, NLResult *nl_result, const RecResult *rec_result){
	NuanceStatus status;
	NLValueType type;
	char rec_string[MAX_RECOGNISED_STRING_LENGTH];
	char prolog_string[MAX_RECOGNISED_STRING_LENGTH];
	char slot_name[MAX_GSL_ATOM_LENGTH];

	char *buffer_ptr = buffer;
	size_t num_chars_written = 0;
	size_t remaining_buffer_size=buff_size;

	if((NULL == buffer) || (NULL == nl_result) || (NULL == rec_result)){
		return -1;
	}

	status = RecResultNLResult(rec_result, 0, nl_result);
	if(NUANCE_OK != status){
		printf("Error: %s\n", NuanceStatusMessage(status));
		return -1;
	}

	NLValue *nl_val = NLNewValue(&status);
	if(NUANCE_OK != status){
		printf("Error: %s\n", NuanceStatusMessage(status));
		return -1;
	}

	int n_slots = NLGetNumberOfFilledSlots(nl_result, &status);
	if(NUANCE_OK != status){
		printf("Error: %s\n", NuanceStatusMessage(status));
		return -1;
	}

	int rec_confidence = 0;

	status = RecResultOverallConfidence(rec_result, 0, &rec_confidence);
	if(NUANCE_OK != status){
		printf("WARNING: %s\n", NuanceStatusMessage(status));
		rec_confidence = 0;
	}

	status = RecResultString(rec_result, 0, rec_string, MAX_RECOGNISED_STRING_LENGTH);
	if(NUANCE_OK != status){
		printf("Error: %s\n", NuanceStatusMessage(status));
		return -1;
	}

	// Print rec confidence score, rec string and opening bracket
	StringToPrologForm(rec_string, prolog_string);

	num_chars_written = sprintf(buffer_ptr, "recognition_succeeded(%d, %s, [ ", rec_confidence, prolog_string);
	buffer_ptr = &buffer_ptr[num_chars_written];
	remaining_buffer_size -= num_chars_written;

	if(remaining_buffer_size <= 0){
		printf("Error: Output buffer to small\n");
		return -1;		
	}

	for ( int i = 0; i < n_slots; i++ ) {
		// Get type and value for ith slot
		NLGetIthSlotNameAndType(nl_result, i, slot_name, MAX_GSL_ATOM_LENGTH, &type);
		if ( NLGetSlotValue(nl_result, slot_name, nl_val) != NUANCE_OK ) {
			printf("\nError: no '%s' slot in recogniser output\n");
			return -1;
		}

		// Print slot name and value
		if ( i > 0 ) {
			num_chars_written = sprintf(buffer_ptr, ", ");
			buffer_ptr = &buffer_ptr[num_chars_written];
			remaining_buffer_size -= num_chars_written;
			if(remaining_buffer_size <= 0){
				printf("Error: Output buffer to small\n");
				return -1;
			}
		}
		StringToPrologForm(slot_name, prolog_string);
		num_chars_written = sprintf(buffer_ptr, "%s, =, ", prolog_string);
		buffer_ptr = &buffer_ptr[num_chars_written];
		remaining_buffer_size -= num_chars_written;
		if(remaining_buffer_size <= 0){
			printf("Error: Output buffer to small\n");
			return -1;
		}

		num_chars_written = SPrintNLValueInRegulusForm(buffer_ptr, remaining_buffer_size, nl_val, type);
		if(num_chars_written < 0){
			return -1;
		}

		buffer_ptr = &buffer_ptr[num_chars_written];
		remaining_buffer_size -= num_chars_written;
		if(remaining_buffer_size <= 0){
			printf("Error: Output buffer to small\n");
			return -1;
		}
	}

	// Print closing bracket
	num_chars_written = sprintf(buffer_ptr, " ])");
	remaining_buffer_size -= num_chars_written;
	if(remaining_buffer_size <= 0){
		printf("Error: Output buffer to small\n");
		return -1;
	}

	NLFreeValue(nl_val);

	return remaining_buffer_size;
}


int SPrintNLValueInRegulusForm(char *buffer, size_t buff_size, const NLValue *nl_val, NLValueType type)
{
	int num_chars_written = 0;

	switch( type ) {
	case NL_INT_VALUE:
		num_chars_written = SPrintNLIntValueInRegulusForm(buffer, buff_size, nl_val);
		break;
	case NL_STRING_VALUE:
		num_chars_written = SPrintNLStringValueInRegulusForm(buffer, buff_size, nl_val);
		break;
	case NL_LIST_VALUE:
		num_chars_written = SPrintNLListValueInRegulusForm(buffer, buff_size, nl_val);
		break;
	case NL_STRUCTURE_VALUE:
		num_chars_written = SPrintNLStructureValueInRegulusForm(buffer, buff_size, nl_val);
		break;
	default:
		printf("\nError: unknown type %d in call to PrintNLValueInRegulusForm\n", type);
		num_chars_written = -1;
		break;
	}

	return num_chars_written;
}


int SPrintNLIntValueInRegulusForm(char *buffer, size_t buff_size, const NLValue *nl_val)
{
	int i;
	int num_chars_written = 0;

	if ( NLGetIntFromValue(nl_val, &i) != NUANCE_OK ) {
		printf("\nError: bad call to PrintNLIntValueInRegulusForm\n");
		num_chars_written = -1;
	}
	else {
		num_chars_written = sprintf(buffer, "%d", i);
	}

	return num_chars_written;
}

int SPrintNLStringValueInRegulusForm(char *buffer, size_t buff_size, const NLValue *nl_val)
{
	char string_val[MAX_GSL_ATOM_LENGTH];
	char prolog_string[MAX_RECOGNISED_STRING_LENGTH];
	int num_chars_written = 0;

	if ( NLGetStringFromValue(nl_val, string_val, MAX_GSL_ATOM_LENGTH) != NUANCE_OK ) {
		printf("\nError: bad call to PrintNLStringValueInRegulusForm\n");
		num_chars_written = -1;
	}
	else {
		StringToPrologForm(string_val, prolog_string);
		num_chars_written = sprintf(buffer, "%s", prolog_string);
	}

	return num_chars_written;
}

int SPrintNLListValueInRegulusForm(char *buffer, size_t buff_size, const NLValue *list)
{
	int len;
	NLValueType type;
	NuanceStatus status;
	NLValue *nl_val = NLNewValue(&status);

	int total_num_chars_written = 0;
	int num_chars_written = 0;
	char *buffer_ptr = buffer;
	size_t remaining_buffer_size = buff_size;

	NLGetLengthOfList(list, &len);

	num_chars_written = sprintf(buffer_ptr, "[ ");
	buffer_ptr = &buffer_ptr[num_chars_written];
	remaining_buffer_size =- num_chars_written;
	total_num_chars_written += num_chars_written;

	for ( int i = 0; i < len; i++ ) {
		// Get type and value for ith member
		if ( NLGetIthValueAndTypeInList(list, i, nl_val, &type) != NUANCE_OK ) {
			printf("\nError: bad call to PrintNLListValueInRegulusForm\n");
			return -1;
		}

		// Print value of member
		if ( i > 0 ) {
			num_chars_written = sprintf(buffer_ptr, ", ");
			buffer_ptr = &buffer_ptr[num_chars_written];
			remaining_buffer_size =- num_chars_written;
			total_num_chars_written += num_chars_written;
		}
		num_chars_written = SPrintNLValueInRegulusForm(buffer_ptr, remaining_buffer_size, nl_val, type);
		buffer_ptr = &buffer_ptr[num_chars_written];
		remaining_buffer_size =- num_chars_written;
		total_num_chars_written += num_chars_written;
	}

	// Print closing bracket
	num_chars_written = sprintf(buffer_ptr, " ]");
	total_num_chars_written += num_chars_written;

	return total_num_chars_written;
}

int SPrintNLStructureValueInRegulusForm(char *buffer, size_t buff_size, const NLValue *structure)
{
	NuanceStatus status;
	NLValueType type;

	NLValue *nl_val = NLNewValue(&status);
	char slot_name[MAX_GSL_ATOM_LENGTH];
	char prolog_string[MAX_RECOGNISED_STRING_LENGTH];

	int total_num_chars_written = 0;
	int num_chars_written = 0;
	char *buffer_ptr = buffer;
	size_t remaining_buffer_size = buff_size;


	int i = 0;
	// Print opening bracket
	num_chars_written = sprintf(buffer_ptr, "[ ");
	buffer_ptr = &buffer_ptr[num_chars_written];
	remaining_buffer_size =- num_chars_written;
	total_num_chars_written += num_chars_written;


	for ( i = 0; NLGetIthFeatureNameAndType(structure, i, slot_name, MAX_GSL_ATOM_LENGTH, &type) != NUANCE_ARGUMENT_OUT_OF_RANGE; i++) {
		if ( NLGetFeatureValue(structure, slot_name, nl_val) != NUANCE_OK ) {
			printf("\nError: bad call to PrintNLStructureValueInRegulusForm\n");
			return -1;
		}

		// Print slot name and value
		if ( i > 0 ) {
			num_chars_written = sprintf(buffer_ptr, ", ");
			buffer_ptr = &buffer_ptr[num_chars_written];
			remaining_buffer_size =- num_chars_written;
			total_num_chars_written += num_chars_written;
		}
		StringToPrologForm(slot_name, prolog_string);
		num_chars_written = sprintf(buffer_ptr, "%s, =, ", prolog_string);
		buffer_ptr = &buffer_ptr[num_chars_written];
		remaining_buffer_size =- num_chars_written;
		total_num_chars_written += num_chars_written;

		num_chars_written = SPrintNLValueInRegulusForm(buffer_ptr, remaining_buffer_size, nl_val, type);
		buffer_ptr = &buffer_ptr[num_chars_written];
		remaining_buffer_size =- num_chars_written;
		total_num_chars_written += num_chars_written;
	}

	// Print closing bracket
	num_chars_written = sprintf(buffer_ptr, " ]");
	buffer_ptr = &buffer_ptr[num_chars_written];
	remaining_buffer_size =- num_chars_written;
	total_num_chars_written += num_chars_written;

	return total_num_chars_written;
}

void StringToPrologForm(const char *in, char *out)
{
	int j = 0;
	// Prolog string starts with a single quote
	out[j++] = '\'';

	for ( int i = 0 ; in[i] != '\0' ; i++ ) {
		// Copy char to Prolog string
		out[j++] = in[i];
		// If current char is a single quote or backslash, double it.
		if ( in[i] == '\'' || in[i] == '\\' ) {
			out[j++] = in[i];
		}
	}
	// Prolog string ends with a single quote and a \0
	out[j++] = '\'';
	out[j++] = '\0';
}

