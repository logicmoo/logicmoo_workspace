/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RegulusController.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <iostream>
#include <sstream>
#include "RegulusController.hpp"
#include "ace/Log_Msg.h"

RegulusController::RegulusController(RegulusControllerObserver& observer, 
									 unsigned int mask) 
	: 	m_observer(observer),
		m_result(NULL),
		m_input(""),				
		m_buffer(""),
		m_is_connected(false)
{
	ACE_TRACE(ACE_TEXT("[RegulusController::RegulusController()]"));
	
	m_server = "";
	m_port = 0;
	m_socket_engine = new SocketsEngine(*this);
	
	if ((mask & DIALOGUE_RESULT) == DIALOGUE_RESULT)
	{
		m_result = new RegulusDialogueResult();
	}
	else
	{
		m_result = new RegulusTranslationResult();
	}
		
	return;
}

RegulusController::~RegulusController()
{
	ACE_TRACE(ACE_TEXT("[RegulusController::~RegulusController()]"));
	
	if (m_result != NULL)
	{
		delete m_result;
	}
	
	if (m_socket_engine != NULL)
	{
		delete m_socket_engine;
	}
	
	return;
}

void RegulusController::SetServerName(const string name)
{
	ACE_TRACE(ACE_TEXT("[RegulusController::SetServerName()]"));
		
	m_server = name;
	
	return;
}

const string RegulusController::GetServerName()
{
	ACE_TRACE(ACE_TEXT("[RegulusController::GetServerName()]"));
		
	return m_server;
}

void RegulusController::SetServerPort(int port)
{
	ACE_TRACE(ACE_TEXT("[RegulusController::SetServerPort()]"));
	
	m_port = port;	
	
	return;
}

int RegulusController::GetServerPort()
{
	ACE_TRACE(ACE_TEXT("[RegulusController::GetServerPort()]"));
		
	return m_port;
}

void RegulusController::SendHello()
{
	ACE_TRACE(ACE_TEXT("[RegulusController::SendHello()]"));
	
	ostringstream os;

    os << "Hello!!!!";
    
	string msg = os.str();

	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [******* SENDING REQUEST *******]\n%s\n"),
				msg.c_str()));	

	// Send it to the Regulus server
	if (m_is_connected == true)
	{
		m_socket_engine->WriteData((Uint8*)msg.c_str(), msg.size());
	}
	
	return;
}

void RegulusController::StartComms()
{
	ACE_TRACE(ACE_TEXT("[RegulusController::StartComms()]"));
	
	if (m_socket_engine != NULL)
	{
		m_socket_engine->SetServerName(m_server);
		m_socket_engine->SetServerPort(m_port);
		m_socket_engine->SetBufferSize(1);
		m_socket_engine->ConnectServer("TCP_CLIENT");
	
		m_is_connected = true;
	}
 	
 	return;
}

void RegulusController::StopComms()
{
	ACE_TRACE(ACE_TEXT("[RegulusController::StopComms()]"));
	
	if ((m_is_connected == true)  && (m_socket_engine != NULL))
	{
		SendDisconnectMessage();
		m_socket_engine->DisconnectServer();
		
		m_is_connected = false;
		
		m_response_type = DISCONNECT;
	}
 	
 	return;
}

void RegulusController::SendMessage(const string& message)
{
	ACE_TRACE(ACE_TEXT("[RegulusController::SendMessage()]"));

	// Send message to the Regulus server
	if (m_is_connected == true)
	{
		m_socket_engine->WriteData((Uint8*)message.c_str(), message.size());
	}
	
	return;
}

void RegulusController::SendDlgMessage(const string& input)
{
	ACE_TRACE(ACE_TEXT("[RegulusController::SendDlgMessage()]"));

	m_response_type = SEND_MESSAGE;

	SendMessage(input);
	
	return;
}

void RegulusController::SendDisconnectMessage()
{
	ACE_TRACE(ACE_TEXT("[RegulusController::SendDisconnectMessage()]"));

	string text("disconnect.\n");
	
	SendMessage(text);
	
	return;
}

void RegulusController::SendRecognitionText(const string& input)
{
	ACE_TRACE(ACE_TEXT("[RegulusController::SendRecognitionText()]"));

	string buffer(input);
	string search_str("\'");
	string replace_str("\\\'");
	FindAndReplace(buffer, search_str, replace_str);
	
	search_str = "?";
	replace_str = "";
	FindAndReplace(buffer, search_str, replace_str);
	
	string text("action(process_rec_string('" + buffer + "')).\n");

	m_input = input;
	
	m_response_type = QUERY_OUTPUT_FROM_HELP;

	SendMessage(text);
	
	return;
}

void RegulusController::SendXmlText(const string& input)
{
	ACE_TRACE(ACE_TEXT("[RegulusController::SendXmlText()]"));
	
	string buffer(input);
	string search_str("\'");
	string replace_str("\\\'");
	FindAndReplace(buffer, search_str, replace_str);
	
	string text("action(process_xml_message('" + buffer + "')).\n");
	
	m_response_type = QUERY_OUTPUT;
	
	SendMessage(text);
	
	return;
}

void RegulusController::SendHelpRequest(const string& input)
{
	ACE_TRACE(ACE_TEXT("[RegulusController::SendHelpRequest()]"));

	string buffer(input);
	string search_str("'");
	string replace_str("\\\'");
	FindAndReplace(buffer, search_str, replace_str);
	
	search_str = "?";
	replace_str = "";
	FindAndReplace(buffer, search_str, replace_str);
	
	string text("action(get_help_examples('" + buffer + "', 2)).\n");
	
	m_response_type = HELP_SENTENCES;
	
	SendMessage(text);
	
	return;
}

void RegulusController::SendTranslationRequest(const string& input, bool type)
{
	ACE_TRACE(ACE_TEXT("[RegulusController::SendTranslationRequest()]"));
	
	if (type == true)
	{
		m_response_type = BACK_TRANSLATION;
	}
	else
	{
		m_response_type = TRANSLATION;
	}

	string buffer(input);
	string search_str("\'");
	string replace_str("\\\'");
	FindAndReplace(buffer, search_str, replace_str);
	
	string text("action(translate_nbest('" + buffer + "')).\n");
	
	SendMessage(text);
	
	return;
}

void RegulusController::SendRegisterExternalUtterance(const string& utterance)
{
	ACE_TRACE(ACE_TEXT("[RegulusController::SendTranslationRequest()]"));

	string text("");
	
	text += "action(register_external_utterance(";
	text += utterance;
	text += ")).\n";
	
/*	text += "action(register_external_utterance((translation='";
	text += m_result->GetTranslation();
	text += "')+(text_translation='";
	text += m_result->GetTextTranslation();
	text += "')+(target_lf=";
	text += m_result->GetTargetLf();
	text += ")+(interlingua='";
	text += m_result->GetInterlingua();
	text += "'))).\n";
*/
	
#if _DEBUG
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RegulusController::SendRegisterExternalUtterance()] "
			"[Ouput: %s]\n"), text.c_str()));				
#endif
	
	m_response_type = REGISTER_EXTERNAL;
	
	SendMessage(text);
	
	return;
}

void RegulusController::SendRevertDiscourseContext()
{
	ACE_TRACE(ACE_TEXT("[RegulusController::SendRevertDiscourseContext()]"));
	
	if (m_result->GetStatus() == RegulusResult::OK)
	{	
		string text("action(revert_discourse_context).\n");
	
		m_response_type = REVERT_CONTEXT;
		
		SendMessage(text);
	}
	
	return;
}

bool RegulusController::FindAndReplace(string &input_str,
										const string& search_str, 
										const string& replace_str)
{
	ACE_TRACE(ACE_TEXT("[RegulusController::FindAndReplace()]"));
	
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

void RegulusController::SplitHelpSentences(string& buffer)
{
	ACE_TRACE(ACE_TEXT("[RegulusController::SplitHelpSentences()]"));

	string search_str("help('");
	string replace_str("");
	FindAndReplace(buffer, search_str, replace_str);
	
	search_str = "').";
	FindAndReplace(buffer, search_str, replace_str);
	
	search_str = "\r\n";
	replace_str = "_";
	FindAndReplace(buffer, search_str, replace_str);

	search_str = "\n";
	replace_str = "_";
	FindAndReplace(buffer, search_str, replace_str);
		
	search_str = "\\n";
	replace_str = "_";
	FindAndReplace(buffer, search_str, replace_str);
		
	search_str = "\\";
	replace_str = "";
	FindAndReplace(buffer, search_str, replace_str);
	
	return;
}

bool RegulusController::ExtractOutput(string& buffer, 
										const string left, const string right)
{
	ACE_TRACE(ACE_TEXT("[RegulusController::ExtractOutput()]"));
		
	string::size_type pos = 0;
	string replace_str("");
	char rm_buff[1000];
	
	pos = buffer.find(left, pos);
	
	if (pos	== string::npos)
	{
		return false;
	}
	
	pos = pos + left.size();

	if (pos >= 500)
 	{
 		pos = 5000 - 1;
 	}
	
	int length = buffer.copy(rm_buff, pos, 0);
	
	if (length >= 500)
 	{
 		length = 500 - 1;
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
	
	if (pos >= 500)
 	{
 		pos = 500 - 1;
 	}
	
	length = buffer.copy(rm_buff, pos, 0);
	
	if (length >= 500)
 	{
 		length = 500 - 1;
 	}
 	
 	rm_buff[length] = '\0';
	if (FindAndReplace(buffer, rm_buff, replace_str) == false)
 	{
 		return false;
 	}
 	
	buffer = rm_buff;
		
#if _DEBUG
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RegulusController::ExtractOutput()] "
			"[Ouput: %s]\n"), buffer.c_str()));				
#endif
	
	return true;
}

void RegulusController::HandleResponse()
{
	ACE_TRACE(ACE_TEXT("[RegulusController::HandleResponse()]"));
	
	string txt(m_buffer);
	string paraphrase(m_buffer);
	
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RegulusController::HandleResponse()] "
				"[Response Type: %d]\n"), m_response_type));			
	
	switch (m_response_type)
	{
	case QUERY_OUTPUT:	
		
		m_result->ParseInput(txt);
		
		m_observer.QueryOutputReceived((RegulusDialogueResult*)m_result);
		break;
	case QUERY_OUTPUT_FROM_HELP:
		if (ExtractOutput(m_buffer, "('", "')") == false)
		{
			if (ExtractOutput(m_buffer, "(", ")") == false)
			{				
				m_buffer = "";
			}
		}
		
		if (m_buffer != "")
		{
			FindAndReplace(m_buffer, "\\", "");
		}		

		//paraphrase = "";
		
		//m_observer.QueryOutputReceived(m_input, paraphrase, m_buffer);
		m_observer.GetFromHelpCompleted(m_input, m_buffer);		
		break;
	case HELP_SENTENCES:
		SplitHelpSentences(m_buffer);
		m_observer.HelpSentencesReceived(m_buffer);
		break;
	case BACK_TRANSLATION:
		
		m_result->ParseInput(txt);
		
		/*if (m_result->GetStatus() == RegulusTranslationResult::FAILED)
		{
			SendRevertDiscourseContext();
		}*/
		
		m_observer.BackTranslationReceived((RegulusTranslationResult*)m_result);
		break;
	case TRANSLATION:
		
		m_result->ParseInput(txt);
		
		/*if (m_result->GetStatus() == RegulusTranslationResult::FAILED)
		{
			SendRevertDiscourseContext();
		}*/
		
		m_observer.TranslationReceived((RegulusTranslationResult*)m_result);
		break;
	case REGISTER_EXTERNAL:
		break;
	case SEND_MESSAGE:		
		break;
	case REVERT_CONTEXT:		
			break;
	case DISCONNECT:
		break;
	default:
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RegulusController::HandleResponse()] "
			"[Unknown Response Type]\n")));		
	}
	
	return;
}

void RegulusController::MessageReceived(ostringstream& buffer)
{
	ACE_TRACE(ACE_TEXT("[RegulusController::MessageReceived()]"));
	
	m_buffer = buffer.str();
	
#if _DEBUG	
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RegulusController::MessageReceived()] "
			"[Received Message: %s]\n"), m_buffer.c_str()));
#endif

	HandleResponse();
	
	return;
}
