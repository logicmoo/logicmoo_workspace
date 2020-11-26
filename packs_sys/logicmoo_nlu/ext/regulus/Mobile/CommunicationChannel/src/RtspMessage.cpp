/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RtspMessage.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <iostream>
#include <sstream>
#include "RtspMessage.hpp"
#include "ace/Log_Msg.h"

using namespace std;

// requires <locale>
static void StringToLower(string& s)
{
    for (string::size_type i = 0; i < s.size(); ++i)
    {
    	s[i] = tolower(s[i]);
    }
    
    return;    
}

RtspMessage::~RtspMessage()
{
	ACE_TRACE(ACE_TEXT("[RtspMessage::~RtspMessage()]"));
	
	return;
}

// TODO ?? should be bool
void RtspMessage::ParseField(const string& field)
{	
#if _DEBUG_1
	ACE_TRACE(ACE_TEXT("[RtspMessage::ParseField()]"));
#endif
//	const char *func_name = "RtspMessage::ParseField";
// ETK    detailed_info(func_name, "\n");
	istringstream is(field);

	string field_name;
	string field_value;

	getline(is, field_name, ':');

	StringToLower(field_name);
// ETK    detailed_info(func_name, "field_name = '%s'\n", field_name.c_str());

    getline(is, field_value);
    
    while (field_value.size() && (field_value[0] == ' ' 
    		|| field_value[0] == '\t'))
    {
		field_value.erase(field_value.begin());
    }

// ETK    detailed_info(func_name, "field_value = '%s'\n", field_value.c_str());
    if (field_name == "connection")
    {
		m_connection = field_value;
    }
    else if (field_name == "content-encoding")
    {
		m_content_encoding = field_value;
    }
    else if (field_name == "content-language")
	{
		m_content_language = field_value;
	}
    else if (field_name == "content-length")
    {
		m_content_length = atoi(field_value.c_str());
    }
    else if (field_name == "content-type")
	{
		m_content_type = field_value;
	}
    else if (field_name == "cseq")
	{
		m_cseq = atoi(field_value.c_str());
	}
    else if (field_name == "session")
	{
		m_session = field_value;
	}
    else if (field_name == "transport")
    {
// ETK	detailed_info(func_name, "transport field  m_transport.size() = %d '%s'\n",
// ETK		     m_transport.size(), m_transport.c_str());
		if (m_transport.size())
		{
	    	m_transport += "," + field_value;
		}
		else
		{
	    	m_transport = field_value;
		}
    }
    else
    {
    	return;
	// return false;
    }
    // return true;
    
    return;
}

bool RtspMessage::GetContentLength(size_t& content_length) const
{
#if _DEBUG_1
	ACE_TRACE(ACE_TEXT("[RtspMessage::GetContentLength()]"));
#endif

    // weird...
    content_length = m_content_length;
    
    return true;
}

void RtspMessage::SetContentLength(size_t content_length)
{
#if _DEBUG_1
	ACE_TRACE(ACE_TEXT("[RtspMessage::SetContentLength()]"));
#endif

    m_content_length = content_length;
    
    return;
}

void RtspMessage::SetBody(const string& body)
{
	ACE_TRACE(ACE_TEXT("[RtspMessage::SetBody()]"));
	
    m_entity_body = body;
    SetContentLength(body.size());
    
    return;
}
