/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RecognitionGrammar.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include "RecognitionGrammar.hpp"
#include "ace/Log_Msg.h"

RecognitionGrammar::RecognitionGrammar()
	: 	m_grammar(""),
		m_content_id(""),
		m_content_type(""),
		m_parameters("")
{
	ACE_TRACE(ACE_TEXT("[RecognitionGrammar::RecognitionGrammar()]"));

	return; 
}
	
RecognitionGrammar::RecognitionGrammar(string const& grammar, 
										string const& content_id, 
										string const& content_type, 
										string const& parameters)
{
	ACE_TRACE(ACE_TEXT("[RecognitionGrammar::RecognitionGrammar()]"));

	m_grammar = grammar;
	m_content_id = content_id;
	m_content_type = content_type;
	m_parameters = parameters;
	
	return;
}
	
RecognitionGrammar::~RecognitionGrammar()
{
	ACE_TRACE(ACE_TEXT("[RecognitionGrammar::~RecognitionGrammar()]"));

	return; 
}
	
void RecognitionGrammar::SetGrammar(const string& input)
{
	ACE_TRACE(ACE_TEXT("[RecognitionGrammar::SetGrammar()]"));

	m_grammar = input;
	
	return;
}

const string& RecognitionGrammar::GetGrammar()
{
	ACE_TRACE(ACE_TEXT("[RecognitionGrammar::GetGrammar()]"));

	return m_grammar;
}
	
void RecognitionGrammar::SetContentId(const string& input)
{
	ACE_TRACE(ACE_TEXT("[RecognitionGrammar::SetContentId()]"));
	
	m_content_id = input;
	
	return;
}

const string& RecognitionGrammar::GetContentId()
{
	ACE_TRACE(ACE_TEXT("[RecognitionGrammar::GetContentId()]"));
	
	return m_content_id;
}
	
void RecognitionGrammar::SetContentType(const string& input)
{
	ACE_TRACE(ACE_TEXT("[RecognitionGrammar::SetContentType()]"));
	
	m_content_type = input;
	
	return;
}

const string& RecognitionGrammar::GetContentType()
{
	ACE_TRACE(ACE_TEXT("[RecognitionGrammar::GetContentType()]"));

	return m_content_type;
}
	
void RecognitionGrammar::SetParameters(const string& input)
{
	ACE_TRACE(ACE_TEXT("[RecognitionGrammar::SetParameters()]"));

	m_parameters = input;
	
	return;
}

const string& RecognitionGrammar::GetParameters()
{
	ACE_TRACE(ACE_TEXT("[RecognitionGrammar::GetParameters()]"));
	
	return m_parameters;
}
