/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RegulusDialogueResult.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include "ace/Log_Msg.h"
#include "RegulusDialogueResult.hpp"

RegulusDialogueResult::RegulusDialogueResult() :
		m_selected(""),
		m_paraphrase(""),
		m_query_output("")
{
	ACE_TRACE("[RegulusDialogueResult::RegulusDialogueResult()]");
	
	return;
}

RegulusDialogueResult::RegulusDialogueResult(const string& input) :
		m_selected(""),
		m_paraphrase(""),
		m_query_output("")
{
	ACE_TRACE("[RegulusDialogueResult::RegulusDialogueResult()]");
	
	ParseInput(input);
	
	return;
}

RegulusDialogueResult::~RegulusDialogueResult()
{
	ACE_TRACE("[RegulusDialogueResult::~RegulusDialogueResult()]");
	
	return;
}

void RegulusDialogueResult::SetSelected(const string& text)
{
	ACE_TRACE("[RegulusDialogueResult::SetSelected()]");
	
	m_selected = text;
			
	return;
}
	
const string& RegulusDialogueResult::GetSelected()
{
	ACE_TRACE("[RegulusDialogueResult::GetSelected()]");
	
	return m_selected;
}
	
void RegulusDialogueResult::SetParaphrase(const string& text)
{
	ACE_TRACE("[RegulusDialogueResult::SetParaphrase()]");
	
	m_paraphrase = text;
	
	return;
}

const string& RegulusDialogueResult::GetParaphrase()
{
	ACE_TRACE("[RegulusDialogueResult::GetParaphrase()]");
	
	return m_paraphrase;
}
	
void RegulusDialogueResult::SetQueryOutput(const string& text)
{
	ACE_TRACE("[RegulusDialogueResult::SetQueryOutput()]");
	
	m_query_output = text;
	
	return;
}

const string& RegulusDialogueResult::GetQueryOutput()
{
	ACE_TRACE("[RegulusDialogueResult::GetQueryOutput()]");
	
	return m_query_output;
}

void RegulusDialogueResult::ClearResult()
{
	ACE_TRACE("[RegulusDialogueResult::ClearResult()]");
	
	SetStatus(EMPTY);

	m_selected = "";
	m_paraphrase = "";
	m_query_output = "";
	
	return;
}

void RegulusDialogueResult::ParseInput(const string& input)
{
	ACE_TRACE("[RegulusDialogueResult::ParseInput()]");
	
	string buffer(input);
	
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RegulusDialogueResult::ParseInput()] "
							"[Input: %s]\n"), buffer.c_str()));
	
	ClearResult();
	
	FindAndReplace(buffer, "\r\n", "");
	
	if ((buffer == "error.") || (buffer == ""))
	{
		m_status = ERROR;
		
		return;
	}	
	
	// Get the selected n-best hypothesis
	if (ExtractOutput(buffer, "'", "',") == false)
	{	
		buffer = "";
	}
	else
	{
		FindAndReplace(buffer, "\\", "");
	}
	
	SetSelected(buffer);
	
	buffer = input;				
	
	// Get the paraphrase
	if (ExtractOutput(buffer, "paraphrase=['", "',") == false)
	{	
		buffer = "";
	}
	else
	{
		FindAndReplace(buffer, "\\", "");
	}
	
	SetParaphrase(buffer);
		
	buffer = input;				
	
	// Get the query output
	if (ExtractOutput(buffer, "('", "')") == false)
	{
		if (ExtractOutput(buffer, "(", ")") == false)
		{				
			buffer = "";
		}
	}
				
	if (buffer != "")
	{
		FindAndReplace(buffer, "\\", "");
	}
	
	SetQueryOutput(buffer);	
				
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RegulusDialogueResult::ParseInput()] "
							"[Selected: %s]\n"), GetSelected().c_str()));
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RegulusDialogueResult::ParseInput()] "
							"[Paraphrase: %s]\n"), GetParaphrase().c_str()));
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RegulusDialogueResult::ParseInput()] "
							"[Query Output: %s]\n"), GetQueryOutput().c_str()));
	
	if (GetQueryOutput() == "Sorry, something went wrong")
	// TODO: Recheck this condition
	//if ((GetParaphrase() == "WARNING: PARAPHRASE GENERATION FAILED" || 
	//		(GetQueryOutput() == "Sorry, something went wrong"))
	{
		m_status = ERROR;
	}
	else
	{
		m_status = OK;
	}
	
	return;
}
