/*
 * Copyright 2007-2008 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	DialogueController.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@issco.unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <iostream>
#include <sstream>
#include "DialogueController.hpp"
#include "ace/Log_Msg.h"

DialogueController::DialogueController(DialogueControllerObserver& observer) 
	: 	m_observer(observer),
		m_input(""),				
		m_buffer(""),
		m_is_connected(false)
{
	ACE_TRACE(ACE_TEXT("[DialogueController::DialogueController()]"));
	
	m_server = "";
	m_port = 0;
	m_socket_engine = new SocketsEngine(*this);
	
	return;
}

DialogueController::~DialogueController()
{
	ACE_TRACE(ACE_TEXT("[DialogueController::~DialogueController()]"));

	if (m_socket_engine != NULL)
	{
		delete m_socket_engine;
	}
	
	return;
}

void DialogueController::SetServerName(const string name)
{
	ACE_TRACE(ACE_TEXT("[DialogueController::SetServerName()]"));
		
	m_server = name;
	
	return;
}

const string DialogueController::GetServerName()
{
	ACE_TRACE(ACE_TEXT("[DialogueController::GetServerName()]"));
		
	return m_server;
}

void DialogueController::SetServerPort(int port)
{
	ACE_TRACE(ACE_TEXT("[DialogueController::SetServerPort()]"));
	
	m_port = port;	
	
	return;
}

int DialogueController::GetServerPort()
{
	ACE_TRACE(ACE_TEXT("[DialogueController::GetServerPort()]"));
		
	return m_port;
}

void DialogueController::SendHello()
{
	ACE_TRACE(ACE_TEXT("[DialogueController::SendHello()]"));
	
	ostringstream os;

    os << "Hello!!!!";
    
	string msg = os.str();

	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [******* SENDING REQUEST *******]\n%s\n"),
				msg.c_str()));	

	// Send it to the Dialogue server
	if (m_is_connected == true)
	{
		m_socket_engine->WriteData((Uint8*)msg.c_str(), msg.size());
	}
	
	return;
}

void DialogueController::StartComms()
{
	ACE_TRACE(ACE_TEXT("[DialogueController::StartComms()]"));
	
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

void DialogueController::StopComms()
{
	ACE_TRACE(ACE_TEXT("[DialogueController::StopComms()]"));
	
	if ((m_is_connected == true)  && (m_socket_engine != NULL))
	{
		SendDisconnectMessage();
		m_socket_engine->DisconnectServer();
		
		m_is_connected = false;
	}
 	
 	return;
}

void DialogueController::SendMessage(const string& message)
{
	ACE_TRACE(ACE_TEXT("[DialogueController::SendMessage()]"));

	// Send message to the Dialogue server
	if (m_is_connected == true)
	{
		m_socket_engine->WriteData((Uint8*)message.c_str(), message.size());
	}
	
	return;
}

void DialogueController::SendDlgMessage(const string& input)
{
	ACE_TRACE(ACE_TEXT("[DialogueController::SendDlgMessage()]"));

	m_response_type = SEND_MESSAGE;

	SendMessage(input);
	
	return;
}

void DialogueController::SendDisconnectMessage()
{
	ACE_TRACE(ACE_TEXT("[DialogueController::SendDisconnectMessage()]"));

	string text("disconnect.\n");
	
	SendMessage(text);
	
	return;
}

void DialogueController::SendRecognitionText(const string& input)
{
	ACE_TRACE(ACE_TEXT("[DialogueController::SendRecognitionText()]"));

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

void DialogueController::SendXmlText(const string& input)
{
	ACE_TRACE(ACE_TEXT("[DialogueController::SendXmlText()]"));
	
	string buffer(input);
	string search_str("\'");
	string replace_str("\\\'");
	FindAndReplace(buffer, search_str, replace_str);
	
	string text("action(process_xml_message('" + buffer + "')).\n");
	
	m_response_type = QUERY_OUTPUT;
	
	SendMessage(text);
	
	return;
}

void DialogueController::SendHelpRequest(const string& input)
{
	ACE_TRACE(ACE_TEXT("[DialogueController::SendHelpRequest()]"));

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

void DialogueController::SendTranslationRequest(const string& input, bool type)
{
	ACE_TRACE(ACE_TEXT("[DialogueController::SendTranslationRequest()]"));
	
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
   
bool DialogueController::FindAndReplace(string &input_str,
										const string& search_str, 
										const string& replace_str)
{
	ACE_TRACE(ACE_TEXT("[DialogueController::FindAndReplace()]"));
	
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

void DialogueController::SplitHelpSentences(string& buffer)
{
	ACE_TRACE(ACE_TEXT("[DialogueController::SplitHelpSentences()]"));

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

bool DialogueController::ExtractOutput(string& buffer, 
										const string left, const string right)
{
	ACE_TRACE(ACE_TEXT("[DialogueController::ExtractOutput()]"));
		
	string::size_type pos = 0;
	string replace_str("");
	char rm_buff[500];
	
	pos = buffer.find(left, pos);
	
	if (pos	== string::npos)
	{
		return false;
	}
	
	pos = pos + left.size();

	if (pos >= 250)
 	{
 		pos = 250 - 1;
 	}
	
	int length = buffer.copy(rm_buff, pos, 0);
	
	if (length >= 250)
 	{
 		length = 250 - 1;
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
	
	if (pos >= 250)
 	{
 		pos = 250 - 1;
 	}
	
	length = buffer.copy(rm_buff, pos, 0);
	
	if (length >= 250)
 	{
 		length = 250 - 1;
 	}
 	
 	rm_buff[length] = '\0';
	if (FindAndReplace(buffer, rm_buff, replace_str) == false)
 	{
 		return false;
 	}
 	
	buffer = rm_buff;
		
#if _DEBUG
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [DialogueController::ExtractOutput()] "
			"[Ouput: %s]\n"), buffer.c_str()));				
#endif
	
	return true;
}

void DialogueController::HandleResponse()
{
	ACE_TRACE(ACE_TEXT("[DialogueController::HandleResponse()]"));
	
	string txt(m_buffer);
	string paraphrase(m_buffer);
	
	switch (m_response_type)
	{
	case QUERY_OUTPUT:	
		
		if (ExtractOutput(txt, "'", "',") == false)
		{	
			txt = "";
		}
		else
		{
			FindAndReplace(txt, "\\", "");
			txt += "?";
		}
		
		if (ExtractOutput(paraphrase, "paraphrase=['", "',") == false)
		{	
			paraphrase = "";
		}
		else
		{
			FindAndReplace(paraphrase, "\\", "");
			paraphrase += "?";
		}
		
		if (ExtractOutput(m_buffer, "('", "')") == false)
		{
			m_buffer = "";
		}
		else
		{
			FindAndReplace(m_buffer, "\\", "");
		}				
		
		m_observer.QueryOutputReceived(txt, paraphrase, m_buffer);
		break;
	case QUERY_OUTPUT_FROM_HELP:
		if (ExtractOutput(m_buffer, "('", "')") == false)
		{
			m_buffer = "";
		}
		else
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
		if (ExtractOutput(txt, "text_translation='", "')") == false)
		{
			txt = "";
		}
		else
		{
			FindAndReplace(txt, "\n", "");
		}
		
		FindAndReplace(txt, "\\", "");
		m_observer.BackTranslationReceived(txt);
		break;
	case TRANSLATION:
		if (ExtractOutput(txt, "text_translation='", "')") == false)
		{
			txt = "";
		}
		else
		{
			FindAndReplace(txt, "\n", "");
		}
		
		FindAndReplace(txt, "\\", "");
		m_observer.TranslationReceived(txt);
		break;
	case SEND_MESSAGE:		
		break;
	default:
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [DialogueController::HandleResponse()] "
			"[Unknown Response Type]\n")));		
	}
	
	return;
}

void DialogueController::MessageReceived(ostringstream& buffer)
{
	ACE_TRACE(ACE_TEXT("[DialogueController::MessageReceived()]"));
	
	m_buffer = buffer.str();
	
#if _DEBUG	
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [DialogueController::MessageReceived()] "
			"[Received Message: %s]\n"), m_buffer.c_str()));
#endif

	HandleResponse();
	
	return;
}
