/*
nlval_to_regulus.cpp -- routines to write out Nuance NLValue structures in Regulus form
*/

#include <stdlib.h>
#include <string.h>
#include "app.h"
#include "nl.h"
#include "recresult.h"
#include "regulus_server.h"
  
/*-----------------------------------------------------------------------*/

void PrintRecResultInRegulusForm(FILE *stream, App *app)
{
	NuanceStatus status;
	NLValueType type;
	char rec_string[MAX_RECOGNISED_STRING_LENGTH];
	char prolog_string[MAX_RECOGNISED_STRING_LENGTH];
	char slot_name[MAX_GSL_ATOM_LENGTH];

	RecResult *rec_result = AppGetRecResult(app);
	NLResult *nl_result = AppGetNLResult(app);
	NLValue *nl_val = NLNewValue(&status);
	int n_slots = NLGetNumberOfFilledSlots(nl_result, &status);
	int rec_confidence = 0;

	RecResultOverallConfidence(rec_result, 0, &rec_confidence);
	RecResultString(rec_result, 0, rec_string, MAX_RECOGNISED_STRING_LENGTH);

	// Print rec confidence score, rec string and opening bracket
	StringToPrologForm(rec_string, prolog_string);
	fprintf(stream, "recognition_succeeded(%d, %s, [ ", rec_confidence, prolog_string);

	for ( int i = 0; i < n_slots; i++ ) {
		// Get type and value for ith slot
		NLGetIthSlotNameAndType(nl_result, i, slot_name, MAX_GSL_ATOM_LENGTH, &type);
		if ( NLGetSlotValue(nl_result, slot_name, nl_val) != NUANCE_OK ) {
			printf("\nError: no '%s' slot in recogniser output\n");
			return;
		}

		// Print slot name and value
		if ( i > 0 ) fprintf(stream, ", ");
		StringToPrologForm(slot_name, prolog_string);
		fprintf(stream, "%s = ", prolog_string);
		PrintNLValueInRegulusForm(stream, nl_val, type);
	}

	// Print closing bracket
	fprintf(stream, " ]).\n");
}

void PrintNLValueInRegulusForm(FILE *stream, NLValue *nl_val, NLValueType type) 
{
	switch( type ) {
	case NL_INT_VALUE:
		PrintNLIntValueInRegulusForm(stream, nl_val);
		break;
	case NL_STRING_VALUE:
		PrintNLStringValueInRegulusForm(stream, nl_val);
		break;
	case NL_LIST_VALUE:
		PrintNLListValueInRegulusForm(stream, nl_val);
		break;
	case NL_STRUCTURE_VALUE:
		PrintNLStructureValueInRegulusForm(stream, nl_val);
		break;
	default:
		printf("\nError: unknown type %d in call to PrintNLValueInRegulusForm\n", type);
		break;
	}
}

void PrintNLIntValueInRegulusForm(FILE *stream, NLValue *nl_val) 
{
	int i;

	if ( NLGetIntFromValue(nl_val, &i) != NUANCE_OK ) {
		printf("\nError: bad call to PrintNLIntValueInRegulusForm\n");
	}
	else {
		fprintf(stream, "%d", i);
	}
}

void PrintNLStringValueInRegulusForm(FILE *stream, NLValue *nl_val) 
{
	char string_val[MAX_GSL_ATOM_LENGTH];
	char prolog_string[MAX_RECOGNISED_STRING_LENGTH];

	if ( NLGetStringFromValue(nl_val, string_val, MAX_GSL_ATOM_LENGTH) != NUANCE_OK ) {
		printf("\nError: bad call to PrintNLStringValueInRegulusForm\n");
	}
	else {
		StringToPrologForm(string_val, prolog_string);
		fprintf(stream, "%s", prolog_string);
	}
}

void PrintNLListValueInRegulusForm(FILE *stream, NLValue *list) 
{
	int len;
	NLValueType type;
	NuanceStatus status;
	NLValue *nl_val = NLNewValue(&status);

	NLGetLengthOfList(list, &len);

	fprintf(stream, "[ ");

	for ( int i = 0; i < len; i++ ) {
		// Get type and value for ith member
		if ( NLGetIthValueAndTypeInList(list, i, nl_val, &type) != NUANCE_OK ) {
			printf("\nError: bad call to PrintNLListValueInRegulusForm\n");
			return;
		}

		// Print value of member
		if ( i > 0 ) fprintf(stream, ", ");
		PrintNLValueInRegulusForm(stream, nl_val, type);
	}

	// Print closing bracket
	fprintf(stream, " ]");
}

void PrintNLStructureValueInRegulusForm(FILE *stream, NLValue *structure) 
{
	NuanceStatus status;
	NLValueType type;

	NLValue *nl_val = NLNewValue(&status);
	char slot_name[MAX_GSL_ATOM_LENGTH];
	char prolog_string[MAX_RECOGNISED_STRING_LENGTH];

	int i = 0;
	// Print opening bracket
	fprintf(stream, "[ ");

	for ( i = 0; NLGetIthFeatureNameAndType(structure, i, slot_name, MAX_GSL_ATOM_LENGTH, &type) != NUANCE_ARGUMENT_OUT_OF_RANGE; i++) {
		if ( NLGetFeatureValue(structure, slot_name, nl_val) != NUANCE_OK ) {
			printf("\nError: bad call to PrintNLStructureValueInRegulusForm\n");
			return;
		}

		// Print slot name and value
		if ( i > 0 ) fprintf(stream, ", ");
		StringToPrologForm(slot_name, prolog_string);
		fprintf(stream, "%s = ", prolog_string);
		PrintNLValueInRegulusForm(stream, nl_val, type);
	}

	// Print closing bracket
	fprintf(stream, " ]\n");
}

void StringToPrologForm(char *in, char *out) 
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
	

