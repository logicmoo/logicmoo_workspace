/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RegulusTranslationResult.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include "ace/Log_Msg.h"
#include "RegulusTranslationResult.hpp"

RegulusTranslationResult::RegulusTranslationResult() :
		m_selected(""),
		m_text_translation(""),
		m_source_lf(""),
		m_target_lf(""),
		m_interlingua("")
{
	ACE_TRACE("[RegulusTranslationResult::RegulusTranslationResult()]");
	
	return;
}

RegulusTranslationResult::RegulusTranslationResult(const string& input) :
		m_selected(""),
		m_translation(""),
		m_text_translation(""),
		m_source_lf(""),
		m_target_lf(""),
		m_interlingua("")
{
	ACE_TRACE("[RegulusTranslationResult::RegulusTranslationResult()]");
	
	ParseInput(input);
	
	return;
}

RegulusTranslationResult::~RegulusTranslationResult()
{
	ACE_TRACE("[RegulusTranslationResult::~RegulusTranslationResult()]");
	
	return;
}

void RegulusTranslationResult::SetSelected(const string& text)
{
	ACE_TRACE("[RegulusTranslationResult::SetSelected()]");
	
	m_selected = text;
			
	return;
}
	
const string& RegulusTranslationResult::GetSelected()
{
	ACE_TRACE("[RegulusTranslationResult::GetSelected()]");
	
	return m_selected;
}
	
void RegulusTranslationResult::SetTranslation(const string& text)
{
	ACE_TRACE("[RegulusTranslationResult::SetTranslation()]");
	
	m_translation = text;
	
	return;
}

const string& RegulusTranslationResult::GetTranslation()
{
	ACE_TRACE("[RegulusTranslationResult::GetTranslation()]");
	
	return m_translation;
}
	
void RegulusTranslationResult::SetTextTranslation(const string& text)
{
	ACE_TRACE("[RegulusTranslationResult::SetTextTranslation()]");
	
	m_text_translation = text;
	
	return;
}

const string& RegulusTranslationResult::GetTextTranslation()
{
	ACE_TRACE("[RegulusTranslationResult::GetTextTranslation()]");
	
	return m_text_translation;
}
	
void RegulusTranslationResult::SetSourceLf(const string& text)
{
	ACE_TRACE("[RegulusTranslationResult::SetSourceLf()]");
	
	m_source_lf = text;
	
	return;
}

const string& RegulusTranslationResult::GetSourceLf()
{
	ACE_TRACE("[RegulusTranslationResult::GetSourceLf()]");
	
	return m_source_lf;
}

void RegulusTranslationResult::SetTargetLf(const string& text)
{
	ACE_TRACE("[RegulusTranslationResult::SetTargetLf()]");
	
	m_target_lf = text;
	
	return;
}

const string& RegulusTranslationResult::GetTargetLf()
{
	ACE_TRACE("[RegulusTranslationResult::GetTargetLf()]");
	
	return m_target_lf;
}
	
void RegulusTranslationResult::SetInterlingua(const string& text)
{
	ACE_TRACE("[RegulusTranslationResult::SetInterlingua()]");
	
	m_interlingua = text;
	
	return;
}

const string& RegulusTranslationResult::GetInterlingua()
{
	ACE_TRACE("[RegulusTranslationResult::GetInterlingua()]");
	
	return m_interlingua;
}

void RegulusTranslationResult::ClearResult()
{
	ACE_TRACE("[RegulusTranslationResult::ClearResult()]");
	
	SetStatus(EMPTY);
	
	m_selected =  "";
	m_translation = "";
	m_text_translation = "";
	m_source_lf = "";
	m_target_lf = "";
	m_interlingua = "";
	
	return;
}

void RegulusTranslationResult::ParseInput(const string& input)
{
	ACE_TRACE("[RegulusTranslationResult::ParseInput()]");
	
	string buffer(input);
	
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RegulusTranslationResult::ParseInput()] "
							"[Input: %s]\n"), buffer.c_str()));
	
	ClearResult();
	
	FindAndReplace(buffer, "\r\n", "");
		
	if ((buffer == "error.") || (buffer == ""))
	{
		m_status = ERROR;
		
		return;
	}	
	
	// Get the selected field
	if (ExtractOutput(buffer, "selected='", "')") == false)
	{
		buffer = "";
	}
	
	SetSelected(buffer);
	
	buffer = input;
	
	// Get the translation field
	if (ExtractOutput(buffer, "translation='", "')") == false)
	{
		buffer = "";
	}
	else
	{
		FindAndReplace(buffer, "\\", "");
	}	
	
	SetTranslation(buffer);
		
	buffer = input;
	
	// Get the text translation field
	if (ExtractOutput(buffer, "text_translation='", "')") == false)
	{
		buffer = "";
	}
	else
	{
		FindAndReplace(buffer, "\\", "");
		FindAndReplace(buffer, "-", " ");
	}
		
	SetTextTranslation(buffer);
			
	buffer = input;
	
	// Get the source_lf field
	if (ExtractOutput(buffer, "source_lf=", ")") == false)
	{
		buffer = "";
	}
		
	SetSourceLf(buffer);
			
	buffer = input;
	
	// Get the target_lf field
	if (ExtractOutput(buffer, "target_lf=", ")") == false)
	{
		buffer = "";
	}
			
	SetTargetLf(buffer);
				
	buffer = input;

	// Get the interlingua field
	if (ExtractOutput(buffer, "interlingua='", "')") == false)
	{
		buffer = "";
	}
		
	SetInterlingua(buffer);
	
	m_status = OK;
	
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RegulusTranslationResult::ParseInput()] "
						"[Selected: %s]\n"), GetSelected().c_str()));
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RegulusTranslationResult::ParseInput()] "
							"[Translation: %s]\n"), GetTranslation().c_str()));
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RegulusTranslationResult::ParseInput()] "
							"[Text Translation: %s]\n"), GetTextTranslation().c_str()));
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RegulusTranslationResult::ParseInput()] "
							"[SourceLf: %s]\n"), GetSourceLf().c_str()));
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RegulusTranslationResult::ParseInput()] "
								"[TargetLf: %s]\n"), GetTargetLf().c_str()));
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RegulusTranslationResult::ParseInput()] "
							"[Interlingua: %s]\n"), GetInterlingua().c_str()));
		
		
	return;
}
