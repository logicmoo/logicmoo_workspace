/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MrcpController.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <iostream>
#include <sstream>
#include <libxml/xmlreader.h>
#include "MrcpController.hpp"
#include "MrcpControllerDefs.hpp"
#include "RtspRequest.hpp"
#include "RtspResponse.hpp"

MrcpController::MrcpController(MrcpControllerObserver& observer) 
	:	m_observer(observer),
		m_message(NULL),
		m_server_rtp_port(0),
		m_is_connected(false),
		m_cseq(1),
		m_request_tag(1),		
		m_recv_state(header)
{
	ACE_TRACE(ACE_TEXT("[MrcpController::MrcpController()]"));
		
	m_server = "";
	m_server_port = 0,
	m_client_rtp_port = 0;
	
	
	m_socket_engine = new SocketsEngine(*this);
	
	return;
}

MrcpController::~MrcpController()
{
	ACE_TRACE(ACE_TEXT("[MrcpController::~MrcpController()]"));
	
	if (m_socket_engine != NULL)
	{
		delete m_socket_engine;
	}
	
	return;
}

void MrcpController::SetServerName(const string name)
{
	ACE_TRACE(ACE_TEXT("[MrcpController::SetServerName()]"));
		
	m_server = name;
	
	return;
}

const string MrcpController::GetServerName()
{
	ACE_TRACE(ACE_TEXT("[MrcpController::GetServerName()]"));
		
	return m_server;
}

void MrcpController::SetServerPort(int port)
{
	ACE_TRACE(ACE_TEXT("[MrcpController::SetServerPort()]"));
	
	m_server_port = port;	
	
	return;
}

int MrcpController::GetServerPort()
{
	ACE_TRACE(ACE_TEXT("[MrcpController::GetServerPort()]"));
		
	return m_server_port;
}

void MrcpController::SetLocalRtpPort(int port)
{
	ACE_TRACE(ACE_TEXT("[MrcpController::SetLocalRtpPort()]"));
	
	m_client_rtp_port = port;	
	
	return;
}

int MrcpController::GetLocalRtpPort()
{
	ACE_TRACE(ACE_TEXT("[MrcpController::GetLocalRtpPort()]"));
		
	return m_client_rtp_port;
}

void MrcpController::SendDescribe(string const& resource)
{
	ACE_TRACE(ACE_TEXT("[MrcpController::SendDescribe()]"));
	
	if (m_is_connected == true)
	{
		ostringstream os;

    	os << "DESCRIBE rtsp://" << m_server << "/ RTSP/1.0" << CRLF;
    	os << "CSeq: " << m_cseq++ << CRLF;
    	os << "Accept: application/sdp" << CRLF;

	    os << CRLF;

		string msg = os.str();

#if _DEBUG_0
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MrcpController::SendDescribe()]\n")));
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [******* SENDING REQUEST *******]\n\n%s\n"), msg.c_str()));
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [*******************************]\n\n")));
#endif
		
		// Send it to the MRCP server
		m_socket_engine->WriteData((Uint8*)msg.c_str(), msg.size());
	}
		
	return;
}

void MrcpController::SendSetup(string const& resource, string const& sdp)
{
	ACE_TRACE(ACE_TEXT("[MrcpController::SendSetup()]"));
	
	if (m_is_connected == true)
	{
		ostringstream os;

		os << "SETUP rtsp://" << m_server << "/" << resource << "/ RTSP/1.0" << CRLF;
		os << "CSeq: " << m_cseq++ << CRLF;

		if (m_session.size())
		{
			os << "Session: " << m_session << CRLF;
		}

		os << "Transport: RTP/AVP;unicast;client_port=" << m_client_rtp_port << CRLF;

		if (sdp.size())
		{
			os << "Content-Type: application/sdp" << CRLF;
			os << "Content-Length: " << sdp.size() << CRLF;
		}

		os << CRLF;

		if (sdp.size())	
		{
			os << sdp;
		}

		string msg = os.str();

#if _DEBUG_0
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MrcpController::SendSetup()]\n")));
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [******* SENDING REQUEST *******]\n\n%s\n"), msg.c_str()));		
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [*******************************]\n\n")));
#endif

		m_rtsp_request_type = SETUP;

		// Send it to the MRCP server
		m_socket_engine->WriteData((Uint8*)msg.c_str(), msg.size());
	}
	
	return;
}

void MrcpController::SendDefineGrammar(string const& grammar, string const& content_id, 
										string const& content_type, string const& parameters)
{
	ACE_TRACE(ACE_TEXT("[MrcpController::SendDefineGrammar()]"));
		
	if (m_is_connected == true)
	{
		ostringstream os;

		// TODO: check
		string mrcp_message;

//		m_wait_for_tag = m_request_tag; // must set before sending

		os << "DEFINE-GRAMMAR" << SP << m_request_tag++ << SP << "MRCP/1.0" << CRLF;
		os << parameters;
		os << "Content-Id: " << content_id << CRLF;
		os << "Content-Type: " << content_type << CRLF;
		os << "Content-Length: " << grammar.size() << CRLF;
		os << CRLF;
		os << grammar;

		mrcp_message = os.str();
		os.str( "" );

		os << "ANNOUNCE rtsp://" << m_server << "/" << g_recognizer<< SP << "RTSP/1.0" << CRLF;
		os << "CSeq: " << m_cseq++ << CRLF;
		os << "Session: " << m_session << CRLF;
		os << "Content-Type: application/mrcp" << CRLF;
		os << "Content-Length: " << mrcp_message.size() << CRLF;
		os << CRLF;
		os << mrcp_message;

		string msg = os.str();

		m_rtsp_request_type = ANNOUNCE;
		m_mrcp_request_type = DEFINE_GRAMMAR;

#if _DEBUG_0
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MrcpController::SendDefineGrammar()]\n")));
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [******* SENDING REQUEST *******]\n\n%s\n"), msg.c_str()));		
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [*******************************]\n\n")));
#endif

		m_rtsp_request_type = SETUP;

		// Send it to the MRCP server
		m_socket_engine->WriteData((Uint8*)msg.c_str(), msg.size());
	}
		
	return;
}

int  MrcpController::SendRecognize(string const& uri_list, // should be a single grammar (really?)
									string const& parameters)
{
	ACE_TRACE(ACE_TEXT("[MrcpController::SendRecognize()]"));
	
	if (m_is_connected == true)
	{
		ostringstream os;

    	string mrcp_message;

//		m_wait_for_tag = m_request_tag; // must set before sending

    	int req_tag = m_request_tag++;

    	os << "RECOGNIZE" << SP << req_tag << SP << "MRCP/1.0" << CRLF;    	
    	os << parameters;
    	os << "Content-Type: text/uri-list" << CRLF;
    	os << "Content-Length: " << uri_list.size() << CRLF;
    	os << CRLF;
    	os << uri_list;

    	mrcp_message = os.str();
    	os.str( "" );

    	os << "ANNOUNCE rtsp://" << m_server << "/" << g_recognizer << SP << "RTSP/1.0" << CRLF;
    	os << "CSeq: " << m_cseq++ << CRLF;
    	os << "Session: " << m_session << CRLF;
    	os << "Content-Type: application/mrcp" << CRLF;
    	os << "Content-Length: " << mrcp_message.size() << CRLF;
    	os << CRLF;
    	os << mrcp_message;

    	string msg = os.str(); 

#if _DEBUG_0	
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MrcpController::SendRecognize()]\n")));
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [******* SENDING REQUEST *******]\n\n%s\n"), msg.c_str()));
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [*******************************]\n\n")));
#endif

		m_rtsp_request_type = ANNOUNCE;
		m_mrcp_request_type = RECOGNIZE;

		// Send it to the MRCP server	
		m_socket_engine->WriteData((Uint8*)msg.c_str(), msg.size());
	
		return req_tag;
	}
	else
	{
    	return m_request_tag;
	}
}

//void MrcpController::SendStop(int req_tag)
void MrcpController::SendStop(bool restype)
{
	ACE_TRACE(ACE_TEXT("[MrcpController::SendStop()]"));
		
	if (m_is_connected == true)
	{
		ostringstream os;

    	string mrcp_message;

    	//os << "STOP" << SP << req_tag << SP << "MRCP/1.0" << CRLF;
    	os << "STOP" << SP << m_request_tag++ << SP << "MRCP/1.0" << CRLF;    	
    	os << CRLF;

    	mrcp_message = os.str();
    	os.str("");

		if (restype == true)
		{
    		os << "ANNOUNCE rtsp://" << m_server << "/" << g_recognizer << SP << "RTSP/1.0" << CRLF;
		}
		else
		{
    		os << "ANNOUNCE rtsp://" << m_server << "/" << g_synthesizer << SP << "RTSP/1.0" << CRLF;
		}
		
    	os << "CSeq: " << m_cseq++ << CRLF;
    	os << "Session: " << m_session << CRLF;
    	os << "Content-Type: application/mrcp" << CRLF;
    	os << "Content-Length: " << mrcp_message.size() << CRLF;
    	os << CRLF;
    	os << mrcp_message;

    	string msg = os.str();

#if _DEBUG_0
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MrcpController::SendStop()]\n")));
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [******* SENDING REQUEST *******]\n\n%s\n"), msg.c_str()));		
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [*******************************]\n\n")));
#endif

		m_rtsp_request_type = ANNOUNCE;
		m_mrcp_request_type = STOP;

		// Send it to the MRCP server	
		m_socket_engine->WriteData((Uint8*)msg.c_str(), msg.size());
	}
	
	return;
}
    
void MrcpController::SendTeardown(string const& resource)
{
	ACE_TRACE(ACE_TEXT("[MrcpController::SendTeardown()]"));
	
	if (m_is_connected == true)
	{
		ostringstream os;

    	os << "TEARDOWN rtsp://" << m_server << "/" << resource << " RTSP/1.0" << CRLF;
    	os << "CSeq: " << m_cseq++ << CRLF;
    	os << "Session: " << m_session << CRLF;
    	os << CRLF;

    	string msg = os.str();

#if _DEBUG_0
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MrcpController::SendTeardown()]\n")));
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [******* SENDING REQUEST *******]\n\n%s\n"), msg.c_str()));
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [*******************************]\n\n")));
#endif

		m_rtsp_request_type = TEARDOWN;

		// Send it to the MRCP server	
		m_socket_engine->WriteData((Uint8*)msg.c_str(), msg.size());
	}

	return;
}
    
void MrcpController::SendSetParams(string const& params)
{
	ACE_TRACE(ACE_TEXT("[MrcpController::SendSetParams()]"));
	
	if (m_is_connected == true)
	{
		ostringstream os;

    	string mrcp_message;

//		m_wait_for_tag = m_request_tag; // must set before sending

    	os << "SET-PARAMS" << SP << m_request_tag++ << SP << "MRCP/1.0" << CRLF;
    	os << params << CRLF;
    	os << CRLF;

    	mrcp_message = os.str();
    	os.str( "" );

    	os << "ANNOUNCE rtsp://" << m_server << "/" << g_recognizer << SP << "RTSP/1.0" << CRLF;
    	os << "CSeq: " << m_cseq++ << CRLF;
    	os << "Session: " << m_session << CRLF;
    	os << "Content-Type: application/mrcp" << CRLF;
    	os << "Content-Length: " << mrcp_message.size() << CRLF;
    	os << CRLF;
    	os << mrcp_message;

    	string msg = os.str();
 
#if _DEBUG_0	
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MrcpController::SendSetParams()]\n")));
 		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [******* SENDING REQUEST *******]\n\n%s\n"), msg.c_str()));
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [*******************************]\n\n")));
#endif

		m_rtsp_request_type = ANNOUNCE;
		m_mrcp_request_type = SET_PARAMS;

		// Send it to the MRCP server	
		m_socket_engine->WriteData((Uint8*)msg.c_str(), msg.size());
	}	
	
	return;
}
    
void MrcpController::SendGetParams(string const& params)
{
	ACE_TRACE(ACE_TEXT("[MrcpController::SendGetParams()]"));
	
	if (m_is_connected == true)
	{
		ostringstream os;

    	string mrcp_message;

//		m_wait_for_tag = m_request_tag; // must set before sendingm

		os << "GET-PARAMS" << SP << m_request_tag++ << SP << "MRCP/1.0" << CRLF;
		os << params << CRLF;
		os << CRLF;
	
		mrcp_message = os.str();
		os.str( "" );

    	os << "ANNOUNCE rtsp://" << m_server << "/" << g_recognizer << SP << "RTSP/1.0" << CRLF;
    	os << "CSeq: " << m_cseq++ << CRLF;
    	os << "Session: " << m_session << CRLF;
    	os << "Content-Type: application/mrcp" << CRLF;
    	os << "Content-Length: " << mrcp_message.size() << CRLF;
    	os << CRLF;
    	os << mrcp_message;

    	string msg = os.str();

#if _DEBUG_0
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MrcpController::SendGetParams()]\n")));
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [******* SENDING REQUEST *******]\n\n%s\n"), msg.c_str()));		
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [*******************************]\n\n")));
#endif

		m_rtsp_request_type = ANNOUNCE;
		m_mrcp_request_type = GET_PARAMS;

//		os << CRLF;
	
		// Send it to the MRCP server	
		m_socket_engine->WriteData((Uint8*)msg.c_str(), msg.size());
	}	
		
	return;
}

int  MrcpController::SendInterpret(string const& transcription,
									string const& uri_list, // should be a single grammar (really?)
									string const& parameters)
{
	ACE_TRACE(ACE_TEXT("[MrcpController::SendInterpret()]"));
	
	if (m_is_connected == true)
	{
		ostringstream os;

    	string mrcp_message;

//		m_wait_for_tag = m_request_tag; // must set before sending

    	int req_tag = m_request_tag++;

    	os << "INTERPRET" << SP << req_tag << SP << "MRCP/1.0" << CRLF;
    	os << "Interpret-Text: " << transcription << CRLF;
    	os << parameters;    	
    	os << "Content-Type: text/uri-list" << CRLF;
    	os << "Content-Length: " << uri_list.size() << CRLF;
    	os << CRLF;
    	os << uri_list;
  
    	mrcp_message = os.str();
    	os.str( "" );

    	os << "ANNOUNCE rtsp://" << m_server << "/" << g_recognizer << SP << "RTSP/1.0" << CRLF;
    	os << "CSeq: " << m_cseq++ << CRLF;
    	os << "Session: " << m_session << CRLF;
    	os << "Content-Type: application/mrcp" << CRLF;
    	os << "Content-Length: " << mrcp_message.size() << CRLF;
    	os << CRLF;
    	os << mrcp_message;

    	string msg = os.str(); 

#if _DEBUG_0	
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MrcpController::SendInterpret()]\n")));
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [******* SENDING REQUEST *******]\n\n%s\n"), msg.c_str()));
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [*******************************]\n\n")));
#endif

		m_rtsp_request_type = ANNOUNCE;
		m_mrcp_request_type = INTERPRET;

		// Send it to the MRCP server	
		m_socket_engine->WriteData((Uint8*)msg.c_str(), msg.size());
	
		return req_tag;
	}
	else
	{
    	return m_request_tag;
	}
}

void MrcpController::SendSpeak(string const& text, string const& lang)
{
	ACE_TRACE(ACE_TEXT("[MrcpController::SendSpeak()]"));
	
	if (m_is_connected == true)
	{	
		ostringstream os;

    	string mrcp_message;

//		m_wait_for_tag = m_request_tag; // must set before sending

    	os << "SPEAK" << SP << m_request_tag++ << SP << "MRCP/1.0" << CRLF;
    	os << "Speech-Language: " << lang << CRLF;
    	os << "Content-Type: text/plain" << CRLF;
    	os << "Content-Length: " << text.size() << CRLF;
    	os << CRLF;
    	os << text;

    	mrcp_message = os.str();
    	os.str( "" );

    	os << "ANNOUNCE rtsp://" << m_server << "/" << g_synthesizer << SP << "RTSP/1.0" << CRLF;
    	os << "CSeq: " << m_cseq++ << CRLF;
    	os << "Session: " << m_session << CRLF;
    	os << "Content-Type: application/mrcp" << CRLF;
    	os << "Content-Length: " << mrcp_message.size() << CRLF;
    	os << CRLF;
    	os << mrcp_message;

    	string msg = os.str();

#if _DEBUG_0
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MrcpController::SendSpeak()]\n")));
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [******* SENDING REQUEST *******]\n\n%s\n"), msg.c_str()));		
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [*******************************]\n\n")));
#endif

		m_rtsp_request_type = ANNOUNCE;
		m_mrcp_request_type = SPEAK;

		// Send it to the MRCP server	
		m_socket_engine->WriteData((Uint8*)msg.c_str(), msg.size());
	}
	
	return;
}

void MrcpController::SendSpeakSSML(string const& text, string const& lang)
{
	ACE_TRACE(ACE_TEXT("[MrcpController::SendSpeakSSML()]"));
	
	if (m_is_connected == true)
	{	
		ostringstream os;

    	string mrcp_message;

//		m_wait_for_tag = m_request_tag; // must set before sending

    	os << "SPEAK" << SP << m_request_tag++ << SP << "MRCP/1.0" << CRLF;
		os << "Speech-Language: " << lang << CRLF;
    	os << "Content-Type: application/synthesis+ssml" << CRLF;
    	os << "Content-Length: " << text.size() << CRLF;
    	os << CRLF;
    	os << text;

    	mrcp_message = os.str();
    	os.str( "" );

    	os << "ANNOUNCE rtsp://" << m_server << "/" << g_synthesizer << SP << "RTSP/1.0" << CRLF;
    	os << "CSeq: " << m_cseq++ << CRLF;
    	os << "Session: " << m_session << CRLF;
    	os << "Content-Type: application/mrcp" << CRLF;
    	os << "Content-Length: " << mrcp_message.size() << CRLF;
    	os << CRLF;
    	os << mrcp_message;

    	string msg = os.str();

#if _DEBUG_0
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MrcpController::SendSpeakSSML()]\n")));
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [******* SENDING REQUEST *******]\n\n%s\n"), msg.c_str()));
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [*******************************]\n\n")));
#endif

		m_rtsp_request_type = ANNOUNCE;
		m_mrcp_request_type = SPEAK;

		// Send it to the MRCP server	
		m_socket_engine->WriteData((Uint8*)msg.c_str(), msg.size());
	}
	
	return;
}

//	Method to read the data received from the MRCP server
void MrcpController::HandleReadTrigger()
{
	ACE_TRACE(ACE_TEXT("[MrcpController::HandleReadTrigger()]"));

    switch (m_recv_state)
    {
    case header:
		ReceiveHeader();
		break;
    case body:
		ReceiveBody();
		break;
	default:
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MrcpController::HandleReadTrigger()] "
				"[Invalid receive state]\n")));	
		break;
    }
    
	return;
}

void MrcpController::ReceiveHeader()
{
	ACE_TRACE(ACE_TEXT("[MrcpController::ReceiveHeader()]"));
	
    if (m_socket_buf.size() > 0)
    {
        // set the index for the next end-of-headers and search for it
        static const string eoh_marker_crlf("\r\n\r\n");
		static const string eoh_marker_lf("\n\n");
		size_t eoh_marker_size = eoh_marker_crlf.size();
//		const string& eoh_marker = eoh_marker_crlf;
        string::size_type eoh_idx = m_socket_buf.find(eoh_marker_crlf);

		// didn't find CRLF CRLF look for LF LF
		if (eoh_idx == string::npos)
		{
			eoh_idx = m_socket_buf.find(eoh_marker_lf);
			eoh_marker_size = eoh_marker_lf.size();
		}

        if (eoh_idx != string::npos)
        {
            // got end of header, and possibly some body as well
            // trim off any body portion and set it in the body buffer
			m_head_buf = m_socket_buf.substr(0, eoh_idx);
			m_socket_buf.erase(0, eoh_idx + eoh_marker_size);
			
			if (m_message != NULL)
			{
				delete m_message;
				m_message = NULL;
			}
			
		    // could be either a request or response header
		    if (m_head_buf.substr(0, 4) != "RTSP")
			{
				m_message = new RtspRequest();
				m_message->Parse(m_head_buf);
			}
			else
			{
				m_message = new RtspResponse();
				m_message->Parse(m_head_buf);
				// process response
			}

            // if the content length is defined and we haven't received it all,
            // then we we expect more entity body, else we're done
            string::size_type content_length = 0;
            
            if (m_message->GetContentLength(content_length))
            {
                m_recv_state = body;
				ReceiveBody();
            }
            else
            {
                HandleMessage();
            }

        } // if eoh_idx != npos

    } // if m_socket_buf.size() > 0
    
	return;
}

void MrcpController::ReceiveBody()
{
	ACE_TRACE(ACE_TEXT("[MrcpController::ReceiveBody()]"));

    string::size_type content_length = 0;
    m_message->GetContentLength(content_length);

    if (m_socket_buf.size() > 0)
    {
		unsigned int bytes_to_copy = content_length - m_body_buf.size();

		bytes_to_copy = bytes_to_copy > m_socket_buf.size() ? m_socket_buf.size() : bytes_to_copy;

		m_body_buf += m_socket_buf.substr(0, bytes_to_copy);
		m_socket_buf.erase(0, bytes_to_copy);
    }

    if (m_body_buf.size() == content_length)
    {
		m_message->SetBody(m_body_buf);
		HandleMessage();
    }
    
	return;
}

void MrcpController::HandleMessage()
{
	ACE_TRACE(ACE_TEXT("[MrcpController::HandleMessage()]"));
		
	RtspMessage::Type message_type = m_message->GetType();

#if _DEBUG_0
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MrcpController::HandleMessage()]\n")));
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [******* RECEIVED MESSAGE *******]\n\n%s\n%s\n"), m_head_buf.c_str(), m_body_buf.c_str()));
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [********************************]\n\n")));
#endif
	    
    switch (message_type)
    {
	case RtspMessage::Request:
		HandleRequest();
		break;
    case RtspMessage::Response:
		HandleResponse();
		break;
    }

    m_head_buf.erase();
    m_body_buf.erase();
    m_recv_state = header;
    
	return;
}

void MrcpController::HandleRequest()
{
	ACE_TRACE(ACE_TEXT("[MrcpController::HandleRequest()]"));
		
	istringstream is(m_body_buf);
	string       method;
	unsigned int tag;
	string       status;
	string       version;
	string		 consume;
	
	is >> method;
	is >> tag;
	is >> status;
	is >> version;
	is >> consume;
	is >> consume;
	
	// TODO: Utilize the m_mrcp_request_type
	if  (method == "RECOGNITION-COMPLETE")
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MrcpController::HandleRequest()] "
									"[RECOGNITION-COMPLETE]\n")));
		
//		cout << "[Result:]" << endl
//			<< m_body_buf << endl;
			
		// Recognition succeeded
		if (consume == "000")
		{
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MrcpController::HandleRequest()] "
											"[RECOGNITION-SUCCEEDED]\n")));
			
			// TODO: Recheck
			for (int i = 0; i < 7; ++i)
			{
				is >> consume;
			}
			
			string result("");
						
			while (is >> consume)
			{
				result += consume + " ";
			}
			
			m_observer.RecognitionCompleted(0, result);
		}
		// NO_MATCH
		else if (consume == "001")
		{
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MrcpController::HandleRequest()] "
											"[RECOGNITION-NO-MATCH]\n")));
			
			string st = is.str();
			m_observer.RecognitionCompleted(1, st);
		}
		// E.g. NO_INPUT
		else
		{
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MrcpController::HandleRequest()] "
											"[RECOGNITION-FAILED]\n")));
			
			string st = is.str();
			m_observer.RecognitionCompleted(2, st);
		}
	}
	else if (method == "INTERPRETATION-COMPLETE")
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MrcpController::HandleRequest()] "
									"[INTERPRETATION-COMPLETE]\n")));
		
		// Interpretation succeeded
		if (consume == "000")
		{
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MrcpController::HandleRequest()] "
											"[INTERPRETATION-SUCCEEDED]\n")));
			
			// TODO: Recheck
			for (int i = 0; i < 5; ++i)
			{
				is >> consume;
			}
			
			string result("");
						
			while (is >> consume)
			{
				result += consume + " ";
			}
		
			m_observer.InterpretationCompleted(0, result);
		}
		// NO_MATCH
		else if (consume == "001")
		{
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MrcpController::HandleRequest()] "
											"[INTERPRETATION-NO-MATCH]\n")));
			
			string st = is.str();
			m_observer.InterpretationCompleted(1, st);
		}		
	}
	else if (method == "START-OF-SPEECH")
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MrcpController::HandleRequest()] "
										"[START-OF-SPEECH]\n")));	
		
		m_observer.StartOfSpeech();
	}
			
	//TODO: Check
//	if (status == "COMPLETE")
//	{
//		SignalCompletion(tag);
//	}
	
	return;
}

void MrcpController::HandleResponse()
{
	ACE_TRACE(ACE_TEXT("[MrcpController::HandleResponse()]"));
	
	switch (m_rtsp_request_type)
	{
	case SETUP:
		HandleSetupResponse();
		break;
	case ANNOUNCE:
		HandleAnnounceResponse();
		break;
	case TEARDOWN:
		HandleTeardownResponse();
		break;
	default:
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MrcpController::HandleResponse()] "
					"[Invalid RTSP request type. Must be one of SETUP, ANNOUNCE, or TEARDOWN]\n")));		

	}
	
	return;
}

void MrcpController::HandleSetupResponse()
{
	ACE_TRACE(ACE_TEXT("[MrcpController::HandleSetupResponse()]"));
	
	RtspTransportHeader th;
    th.Parse( m_message->GetTransport() );

	//TODO: Check
	/*// Obtain the server's RTP IPAddress:port
	char sbuf[gxsMAX_NAME_LEN];
	int rv = m_client->GetBoundIPAddress(sbuf);
	if (rv < 0)
	{
		fprintf(stderr, m_client->SocketExceptionMessage()); fprintf(stderr, "\n");
	}
*/
	ostringstream os;
//	os << sbuf << ":" << th.ServerRtpPort();
	os << m_server << ":" << th.ServerRtpPort();
	m_server_rtp_port = th.ServerRtpPort();
	m_server_rtp_address = os.str();
	m_session = m_message->GetSession();
	
	return;
}

void MrcpController::HandleAnnounceResponse()
{
	ACE_TRACE(ACE_TEXT("[MrcpController::HandleAnnounceResponse()]"));
		
	istringstream is(m_body_buf);
	string       version;
	unsigned int tag;
	string       request_status;
	
	is >> version;
	is >> tag;
	is >> m_status_code;
	is >> request_status;

	//TODO: Check	
//	if (request_status == "COMPLETE")
//	{
//		SignalCompletion(tag);
//	}

	return;
}

void MrcpController::HandleTeardownResponse()
{
	ACE_TRACE(ACE_TEXT("[MrcpController::HandleTeardownResponse()]"));
	
	//TODO: Check
//	SignalEvent(NUANCE_OK);
	
	return;
}

unsigned short MrcpController::GetServerRtpPort()
{	
	ACE_TRACE(ACE_TEXT("[MrcpController::GetServerRtpPort()]"));
	
	return m_server_rtp_port;
}

void MrcpController::StartComms()
{
	ACE_TRACE(ACE_TEXT("[MrcpController::StartComms()]"));
	
	if (m_socket_engine != NULL)
	{
		m_socket_engine->SetServerName(m_server);
		m_socket_engine->SetServerPort(m_server_port);
		m_socket_engine->SetBufferSize(1);
		m_socket_engine->ConnectServer("TCP_CLIENT");
		
		m_is_connected = true;
	}
	
 	return;
}

void MrcpController::StopComms()
{
	ACE_TRACE(ACE_TEXT("[MrcpController::StopComms()]"));
	
	if ((m_is_connected == true) && (m_socket_engine != NULL))
	{
		m_socket_engine->DisconnectServer();
 		m_is_connected = false;
	} 	
 	
 	return;
}

void MrcpController::MessageReceived(ostringstream& buffer)
{
	ACE_TRACE(ACE_TEXT("[MrcpController::MessageReceived()]"));
	
	m_socket_buf = buffer.str();

/*!#if _DEBUG*/
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MrcpController::MessageReceived()] "
				"[Received Message:] \n\n%s\n\n"), m_socket_buf.c_str()));
/*!#endif
	*/	
	HandleReadTrigger();
	
	return;
}

const string& MrcpController::GetMessageBody()
{
	ACE_TRACE(ACE_TEXT("[MrcpController::GetMessageBody()]"));
	
	return m_message->GetBody();
}

// TODO: Implement the method
const string& MrcpController::GetRecTransription()
{
	ACE_TRACE(ACE_TEXT("[MrcpController::GetRecTransription()]"));
	
	return m_message->GetBody();
}
