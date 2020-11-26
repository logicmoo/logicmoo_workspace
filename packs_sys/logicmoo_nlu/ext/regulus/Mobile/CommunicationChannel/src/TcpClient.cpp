/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	TcpClient.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <iostream>
#include <sstream>
#include "TcpClient.hpp"
#include "ace/Log_Msg.h"

TcpClient::TcpClient(TcpClientObserver& observer) 
	: 	m_observer(observer),
		m_input(""),				
		m_buffer(""),
		m_is_connected(false)
{
	ACE_TRACE(ACE_TEXT("[TcpClient::TcpClient()]"));
	
	m_server = "";
	m_port = 0;
	m_socket_engine = new SocketsEngine(*this);
	
	return;
}

TcpClient::~TcpClient()
{
	ACE_TRACE(ACE_TEXT("[TcpClient::~TcpClient()]"));

	if (m_socket_engine != NULL)
	{
		delete m_socket_engine;
	}
	
	return;
}

void TcpClient::SetServerName(const string name)
{
	ACE_TRACE(ACE_TEXT("[TcpClient::SetServerName()]"));
		
	m_server = name;
	
	return;
}

const string TcpClient::GetServerName()
{
	ACE_TRACE(ACE_TEXT("[TcpClient::GetServerName()]"));
		
	return m_server;
}

void TcpClient::SetServerPort(int port)
{
	ACE_TRACE(ACE_TEXT("[TcpClient::SetServerPort()]"));
	
	m_port = port;	
	
	return;
}

int TcpClient::GetServerPort()
{
	ACE_TRACE(ACE_TEXT("[TcpClient::GetServerPort()]"));
		
	return m_port;
}

void TcpClient::StartComms()
{
	ACE_TRACE(ACE_TEXT("[TcpClient::StartComms()]"));
	
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

void TcpClient::StopComms()
{
	ACE_TRACE(ACE_TEXT("[TcpClient::StopComms()]"));
	
	if ((m_is_connected == true)  && (m_socket_engine != NULL))
	{		
		m_socket_engine->DisconnectServer();
		
		m_is_connected = false;
	}
 	
 	return;
}

void TcpClient::SendMessage(const string& message)
{
	ACE_TRACE(ACE_TEXT("[TcpClient::SendMessage()]"));

	// Send message to the client
	if (m_is_connected == true)
	{
		m_socket_engine->WriteData((Uint8*)message.c_str(), message.size());
	}
	
	return;
}

void TcpClient::SendDlgMessage(const string& input)
{
	ACE_TRACE(ACE_TEXT("[TcpClient::SendDlgMessage()]"));

	//m_response_type = SEND_MESSAGE;

	SendMessage(input);
	
	return;
}

void TcpClient::SendDisconnectMessage()
{
	ACE_TRACE(ACE_TEXT("[TcpClient::SendDisconnectMessage()]"));

	string text("disconnect.\n");
	
	SendMessage(text);
	
	return;
}

void TcpClient::SendRecognitionText(const string& input)
{
	ACE_TRACE(ACE_TEXT("[TcpClient::SendRecognitionText()]"));

	string buffer(input);
	string search_str("\'");
	string replace_str("\\\'");
	FindAndReplace(buffer, search_str, replace_str);
	
	search_str = "?";
	replace_str = "";
	FindAndReplace(buffer, search_str, replace_str);
	
	string text("action(process_rec_string('" + buffer + "')).\n");

	m_input = input;
	
	//m_response_type = QUERY_OUTPUT_FROM_HELP;

	SendMessage(text);
	
	return;
}

void TcpClient::SendXmlText(const string& input)
{
	ACE_TRACE(ACE_TEXT("[TcpClient::SendXmlText()]"));
	
	string buffer(input);
	string search_str("\'");
	string replace_str("\\\'");
	FindAndReplace(buffer, search_str, replace_str);
	
	string text("action(process_xml_message('" + buffer + "')).\n");
	
	//m_response_type = QUERY_OUTPUT;
	
	SendMessage(text);
	
	return;
}

void TcpClient::SendHelpRequest(const string& input)
{
	ACE_TRACE(ACE_TEXT("[TcpClient::SendHelpRequest()]"));

	string buffer(input);
	string search_str("'");
	string replace_str("\\\'");
	FindAndReplace(buffer, search_str, replace_str);
	
	search_str = "?";
	replace_str = "";
	FindAndReplace(buffer, search_str, replace_str);
	
	string text("action(get_help_examples('" + buffer + "', 2)).\n");
	
	//m_response_type = HELP_SENTENCES;
	
	SendMessage(text);
	
	return;
}

void TcpClient::SendTranslationRequest(const string& input, bool type)
{
	ACE_TRACE(ACE_TEXT("[TcpClient::SendTranslationRequest()]"));
	
	if (type == true)
	{
		//m_response_type = BACK_TRANSLATION;
	}
	else
	{
		//m_response_type = TRANSLATION;
	}

	string buffer(input);
	string search_str("\'");
	string replace_str("\\\'");
	FindAndReplace(buffer, search_str, replace_str);
	
	string text("action(translate_nbest('" + buffer + "')).\n");
	
	SendMessage(text);
	
	return;
}
   
bool TcpClient::FindAndReplace(string &input_str,
								const string& search_str, 
								const string& replace_str)
{
	ACE_TRACE(ACE_TEXT("[TcpClient::FindAndReplace()]"));
	
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

void TcpClient::SplitHelpSentences(string& buffer)
{
	ACE_TRACE(ACE_TEXT("[TcpClient::SplitHelpSentences()]"));
		
	string search_str("help('");
	string replace_str("");
	FindAndReplace(buffer, search_str, replace_str);
	
	search_str = "').";
	FindAndReplace(buffer, search_str, replace_str);
	
//	search_str = "\\n";
//	replace_str = "\n";
	search_str = "\n";
	replace_str = "-";
	FindAndReplace(buffer, search_str, replace_str);
	
	search_str = "\\n";
	replace_str = "-";
	FindAndReplace(buffer, search_str, replace_str);
		
	search_str = "\\";
	replace_str = "";
	FindAndReplace(buffer, search_str, replace_str);
	
	return;
}

bool TcpClient::ExtractOutput(string& buffer, 
								const string left, const string right)
{
	ACE_TRACE(ACE_TEXT("[TcpClient::ExtractOutput()]"));
		
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
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [TcpClient::ExtractOutput()] "
			"[Ouput: %s]\n"), buffer.c_str()));				
#endif
	
	return true;
}

void TcpClient::HandleResponse()
{
	ACE_TRACE(ACE_TEXT("[TcpClient::HandleResponse()]"));
	
	string txt(m_buffer);
	
	m_observer.ServerMessageReceived(txt);
	
	/*switch (m_response_type)
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
		
		if (ExtractOutput(m_buffer, "(", ")") == false)
		{
			m_buffer = "";
		}
		else
		{
			FindAndReplace(m_buffer, "\\", "");
		}				
		
		m_observer.QueryOutputReceived(txt, m_buffer);
		break;
	case QUERY_OUTPUT_FROM_HELP:
		if (ExtractOutput(m_buffer, "(", ")") == false)
		{
			m_buffer = "";
		}
		else
		{
			FindAndReplace(m_buffer, "\\", "");
		}
		
		m_observer.QueryOutputReceived(m_input, m_buffer);
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
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [TcpServer::HandleResponse()] "
			"[Unknown Response Type]\n")));		
	}*/
	
	return;
}

void TcpClient::MessageReceived(ostringstream& buffer)
{
	ACE_TRACE(ACE_TEXT("[TcpClient::MessageReceived()]"));
	
	m_buffer = buffer.str();
	
#if _DEBUG	
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [TcpClient::MessageReceived()] "
			"[Received Message: %s]\n"), m_buffer.c_str()));
#endif

	HandleResponse();
	
	return;
}
