/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RtspUrl.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <iostream>
#include "RtspUrl.hpp"
#include "MrcpError.hpp"
#include "ace/Log_Msg.h"

using namespace std;

RtspUrl::RtspUrl(const string& url):
    m_url(url), m_port(554), m_valid(false)
{
	ACE_TRACE(ACE_TEXT("[RtspUrl::RtspUrl()]"));

    BuildFromString(url);
    
    return;
}

void RtspUrl::Clear()
{
	ACE_TRACE(ACE_TEXT("[RtspUrl::Clear()]"));
	
    m_url.erase();
    m_protocol.erase();
    m_host.erase();
    m_abs_path.erase();
    m_port = 554;
    m_valid = false;
    
    return;
}

void RtspUrl::BuildFromString(const string& url)
{
	ACE_TRACE(ACE_TEXT("[RtspUrl::BuildFromString()]"));

    string::size_type str_index;
    m_url = url;
    // ETK detailed_info(func_name, "m_url = %s\n", m_url.c_str());
    str_index = m_url.find("//");

    int start = 0;

    if (str_index == string::npos)
    {
		return;
    }
    
    m_protocol = url.substr(start, str_index);
    start = str_index + 2;

    str_index = m_url.find("/", start);

    if (str_index == string::npos)
    {
		return;
    }

    m_host = url.substr(start, str_index - start);

    start = str_index + 1;

    m_abs_path = url.substr(start);

    // this may not be the ideal solution, best would be to tokenize based on '/' and
    // ignore empty components

    while ((m_abs_path.length()) 
    		&& ((m_abs_path[m_abs_path.length() - 1]) == '/'))
    {
		m_abs_path.erase(m_abs_path.length() - 1);
	}

    // fprintf(stderr, "m_abs_path = %s\n", m_abs_path.c_str());

    str_index = m_host.find(":");
    
    if (str_index != string::npos)
    {
		// ETK detailed_info(func_name, "host substr = %s\n", m_host.substr( str_index + 1 ).c_str());
		m_port = atoi(m_host.substr(str_index + 1).c_str());
    }

    // ETK detailed_info(func_name, "m_protocol = %s\n", m_protocol.c_str());
    // ETK detailed_info(func_name, "m_host = %s\n", m_host.c_str());
    // ETK detailed_info(func_name, "m_abs_path = %s\n", m_abs_path.c_str());
    // ETK detailed_info(func_name, "m_port = %d\n", m_port);
    
    if (m_protocol != "rtsp:")
    {
		return;
    }

    if ((m_abs_path != "synthesizer") && (m_abs_path != "recognizer"))
	{
		return;
	}

    // ETK detailed_info(func_name, "valid = true\n");
    m_valid = true;
    
    return;
}
