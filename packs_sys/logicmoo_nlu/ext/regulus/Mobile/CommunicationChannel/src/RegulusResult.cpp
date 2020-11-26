/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RegulusResult.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include "ace/Log_Msg.h"
#include "RegulusResult.hpp"

RegulusResult::RegulusResult() :
		m_status(OK)
{
	ACE_TRACE("[RegulusResult::RegulusResult()]");
	
	return;
}

RegulusResult::RegulusResult(const string& input) :
		m_status(OK)
{
	ACE_TRACE("[RegulusResult::RegulusResult()]");
	
	ParseInput(input);
	
	return;
}

RegulusResult::~RegulusResult()
{
	ACE_TRACE("[RegulusResult::~RegulusResult()]");
	
	return;
}

void RegulusResult::SetStatus(ResultStatus status)
{
	ACE_TRACE("[RegulusResult::SetStatus()]");
	
	m_status = status;
	
	return;
}

RegulusResult::ResultStatus RegulusResult::GetStatus()
{
	ACE_TRACE("[RegulusResult::GetStatus()]");
	
	return m_status;
}

void RegulusResult::ParseInput(const string& input)
{
	ACE_TRACE("[RegulusResult::ParseInput()]");
	
	return;
}

bool RegulusResult::FindAndReplace(string &input_str,
									const string& search_str, 
									const string& replace_str)
{
	ACE_TRACE(ACE_TEXT("[RegulusResult::FindAndReplace()]"));
	
	string::size_type pos = 0;
	bool found = false;
	
	while ((pos = input_str.find(search_str, pos)) != string::npos)
	{
		input_str.replace(pos, search_str.size(), replace_str);
		pos = pos + replace_str.length();
		
		found = true;
	}
	
	return found;
}

bool RegulusResult::ExtractOutput(string& buffer, 
									const string left, 
									const string right)
{
	ACE_TRACE(ACE_TEXT("[RegulusResult::ExtractOutput()]"));
		
	string::size_type pos = 0;
	string replace_str("");
	char rm_buff[1000];
	
	pos = buffer.find(left, pos);
	
	if (pos	== string::npos)
	{
		return false;
	}
	
	pos = pos + left.size();

	if (pos >= 1000)
 	{
 		pos = 1000 - 1;
 	}
	
	int length = buffer.copy(rm_buff, pos, 0);
	
	if (length >= 1000)
 	{
 		length = 1000 - 1;
 	}
 	 	
 	rm_buff[length] = '\0';
 	 	
 	if (FindAndReplace(buffer, rm_buff, replace_str) == false)
 	{
 		return false;
 	}
 	
 	pos = buffer.find(right, 0);
	
	if (pos	== string::npos)
	{
		return false;
	}
	
	if (pos >= 1000)
 	{
 		pos = 1000 - 1;
 	}
	
	length = buffer.copy(rm_buff, pos, 0);
	
	if (length >= 1000)
 	{
 		length = 1000 - 1;
 	}
 	
 	rm_buff[length] = '\0';
 	
	if (FindAndReplace(buffer, rm_buff, replace_str) == false)
 	{
 		return false;
 	}
 	
	buffer = rm_buff;
		
#if _DEBUG
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RegulusResult::ExtractOutput()] "
			"[Ouput: %s]\n"), buffer.c_str()));				
#endif
	
	return true;
}
