/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	CommunicationChannel.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <iostream>
#include <sstream>
#include "CommunicationChannel.hpp"
#include "RtpTransceiver.hpp"
#include "RtspRequest.hpp"
#include "RtspResponse.hpp"
// TODO: put it in the class
//bool grm = true;
//string ansbuffer("");

using namespace std;

#include <vector>
				
CommunicationChannel::CommunicationChannel(CommunicationChannelObserver& observer,
											unsigned int server_mask)
	:	m_observer(observer),
		m_ccstatus(CC_DISCONNECTED),
		m_ccrequest_type(CC_NULL),
		m_mrcp_ctl_name(""),
		m_mrcp_ctl_port(0),
		m_dlg_ctl_name(""),
		m_dlg_ctl_port(0),
		m_trans_ctl_name(""),
		m_trans_ctl_port(0),
		m_btrans_ctl_name(""),
		m_btrans_ctl_port(0),
		m_local_server_name(""),
		m_local_server_port(0),
		m_local_rtp_port(0),
		m_allow_playback(true),
		m_recognize_from_tts(false),
		m_confidence(30)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::CommunicationChannel()]"));

	m_dlg_ctl = NULL;
	m_trans_ctl = NULL;
	m_btrans_ctl = NULL;
	m_mrcp_ctl = NULL;
	m_mrcp_ctl_tts = NULL;
	m_rtp_trc = NULL;
	m_audio_eng = NULL;
	m_tcp_server = NULL;
	m_tcp_client = NULL;
	
	if ((server_mask & DIALOGUE_SERVER) == DIALOGUE_SERVER)
	{
		cout << endl << "---[DEBUG] [******* 1 *******]" << endl;
		m_dlg_ctl = new RegulusController(*this, 0x01);
	}

	if ((server_mask & TRANSLATION_SERVER) == TRANSLATION_SERVER)
	{
		cout << endl << "---[DEBUG] [******* 2 *******]" << endl;
		m_trans_ctl = new RegulusController(*this, 0x02);
	}

	if ((server_mask & BACK_TRANSLATION_SERVER) == BACK_TRANSLATION_SERVER) 
	{
		cout << endl << "---[DEBUG] [******* 3 *******]" << endl;
		m_btrans_ctl = new RegulusController(*this, 0x02);		
	}

	if ((server_mask & MRCP_SERVER_1) == MRCP_SERVER_1) 
	{
		cout << endl << "---[DEBUG] [******* 4 *******]" << endl;
		m_mrcp_ctl = new MrcpController(*this);	
		m_rtp_trc = new RtpTransceiver(*this);
		m_audio_eng = new AudioStreamEngine(*this);
	}

	if ((server_mask & MRCP_SERVER_2) == MRCP_SERVER_2) 
	{
		cout << endl << "---[DEBUG] [******* 5 *******]" << endl;
		m_mrcp_ctl_tts = new MrcpController(*this);
	}

	if ((server_mask & TCP_SERVER) == TCP_SERVER) 
	{
		cout << endl << "---[DEBUG] [******* 6 *******]" << endl;
		m_tcp_server = new TcpServer(*this);
	}

	if ((server_mask & TCP_CLIENT) == TCP_CLIENT) 
	{
		cout << endl << "---[DEBUG] [******* 7 *******]" << endl;
		m_tcp_client = new TcpClient(*this);
	}
	
	return;
}

CommunicationChannel::~CommunicationChannel()
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::~CommunicationChannel()]"));

	cout << endl << "---[DEBUG] [******* 1 *******]" << endl;
	
	if (m_audio_eng != NULL)
	{
		CloseAudio();
	}

	cout << endl << "---[DEBUG] [******* 2 *******]" << endl;
	
	if (m_rtp_trc != NULL)
	{			
		delete m_rtp_trc;
	}

	if (m_dlg_ctl != NULL)
	{		
		delete m_dlg_ctl;
	}
	
	if (m_trans_ctl != NULL)
	{				
		delete m_trans_ctl;
	}
	
	cout << endl << "---[DEBUG] [******* 4 *******]" << endl;
	
	if (m_btrans_ctl != NULL)
	{				
		delete m_btrans_ctl;
	}
	
	if (m_mrcp_ctl != NULL)
	{				
		delete m_mrcp_ctl;
	}
	
	if (m_mrcp_ctl_tts != NULL)
	{	
		delete m_mrcp_ctl_tts;
	}
	
	cout << endl << "---[DEBUG] [******* 5 *******]" << endl;
	
	if (m_audio_eng != NULL)
	{
		delete m_audio_eng;
	}
	
	cout << endl << "---[DEBUG] [******* 6 *******]" << endl;
		
	if (m_tcp_server != NULL)
	{
		delete m_tcp_server;
	}
	
	cout << endl << "---[DEBUG] [******* 7 *******]" << endl;
	
	if (m_tcp_client != NULL)
	{
		delete m_tcp_client;
	}
		
	cout << endl << "---[DEBUG] [******* 8 *******]" << endl;
		
	return;
}

void CommunicationChannel::SetMrcpServerName(const string name)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::SetMrcpServerName()]"));
	
	m_mrcp_ctl_name = name;
	
	return;
}

const string CommunicationChannel::GetMrcpServerName()
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::GetMrcpServerName()]"));
	
	return m_mrcp_ctl_name;
}

void CommunicationChannel::SetMrcpServerPort(int port)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::SetMrcpServerPort()]"));
	
	m_mrcp_ctl_port = port;
	
	return;
}

int CommunicationChannel::GetMrcpServerPort()
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::GetMrcpServerPort()]"));
	
	return m_mrcp_ctl_port;
}
	
void CommunicationChannel::SetDlgServerName(const string name)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::SetDlgServerName()]"));
	
	m_dlg_ctl_name = name;
	
	return;
}

const string CommunicationChannel::GetDlgServerName()
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::GetDlgServerName()]"));
	
	return m_dlg_ctl_name;
}

void CommunicationChannel::SetDlgServerPort(int port)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::SetDlgServerPort()]"));
	
	m_dlg_ctl_port = port;
	
	return;
}

int CommunicationChannel::GetDlgServerPort()
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::GetDlgServerPort()]"));
	
	return m_dlg_ctl_port;
}

void CommunicationChannel::SetTransServerName(const string name)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::SetTransServerName()]"));
	
	m_trans_ctl_name = name;
	
	return;
}

const string CommunicationChannel::GetTransServerName()
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::GetTransServerName()]"));
	
	return m_trans_ctl_name;
}

void CommunicationChannel::SetTransServerPort(int port)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::SetTransServerPort()]"));
	
	m_trans_ctl_port = port;
	
	return;
}

int CommunicationChannel::GetTransServerPort()
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::GetTransServerPort()]"));
	
	return m_trans_ctl_port;
}

void CommunicationChannel::SetBTransServerName(const string name)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::SetBTransServerName()]"));
	
	m_btrans_ctl_name = name;
	
	return;
}

const string CommunicationChannel::GetBTransServerName()
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::GetBTransServerName()]"));
	
	return m_btrans_ctl_name;
}

void CommunicationChannel::SetBTransServerPort(int port)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::SetBTransServerPort()]"));
	
	m_btrans_ctl_port = port;
	
	return;
}

int CommunicationChannel::GetBTransServerPort()
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::GetBTransServerPort()]"));
	
	return m_btrans_ctl_port;
}

void CommunicationChannel::SetLocalServerName(const string name)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::SetLocalServerName()]"));
	
	m_local_server_name = name;
	
	return;
}

const string CommunicationChannel::GetLocalServerName()
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::GetLocalServerName()]"));
	
	return m_local_server_name;
}

void CommunicationChannel::SetLocalServerPort(int port)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::SetLocalServerPort()]"));
	
	m_local_server_port = port;
	
	return;
}

int CommunicationChannel::GetLocalServerPort()
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::GetLocalServerPort()]"));
	
	return m_local_server_port;
}

void CommunicationChannel::SetRtpLocalPort(int port)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::SetRtpLocalrPort()]"));
	
	m_local_rtp_port = port;
	
	return;
}

int CommunicationChannel::GetRtpLocalPort()
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::GetRtpLocalPort()]"));
	
	return m_local_rtp_port;
}
	
void CommunicationChannel::SetConfidence(int value)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::SetConfidence()]"));
	
	m_confidence = value;
	
	return;
}
	
int CommunicationChannel::GetConfidence()
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::GetConfidence()]"));
	
	return m_confidence;
}
	
void CommunicationChannel::StartComms()
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::StartComms()]"));

	if (m_dlg_ctl != NULL)
	{
		//m_dlg_ctl->SetServerName("192.33.226.43");
		m_dlg_ctl->SetServerName(m_dlg_ctl_name);
		m_dlg_ctl->SetServerPort(m_dlg_ctl_port);		
		m_dlg_ctl->StartComms();
	}
	
	if (m_trans_ctl != NULL)
	{		
		m_trans_ctl->SetServerName(m_trans_ctl_name);
		m_trans_ctl->SetServerPort(m_trans_ctl_port);				
		m_trans_ctl->StartComms();
		
		ACE_OS::sleep(3);
	}
	
	if (m_btrans_ctl != NULL)
	{		
		m_btrans_ctl->SetServerName(m_btrans_ctl_name);
		m_btrans_ctl->SetServerPort(m_btrans_ctl_port);		
		m_btrans_ctl->StartComms();
	}
	
	if (m_mrcp_ctl != NULL)
	{
		m_mrcp_ctl->SetServerName(m_mrcp_ctl_name);
		m_mrcp_ctl->SetServerPort(m_mrcp_ctl_port);
		m_mrcp_ctl->SetLocalRtpPort(m_local_rtp_port);
		m_mrcp_ctl->StartComms();
		
		ACE_OS::sleep(2);
		m_ccrequest_type = CC_DESCRIBE;
		m_mrcp_ctl->SendDescribe(g_recognizer);
		//!ACE_OS::sleep(2);
		
		/*!m_mrcp_ctl->SendSetup(g_recognizer);		
		ACE_OS::sleep(2);*/
		
//		m_mrcp_ctl->SendGetParams("Vendor-Specific-Parameters:audio.rtp.Streams;audio.rtp.AudioDetectionTimeout;audio.rtp.RecordPayloadSize;audio.rtp.SessionState;audio.rtp.LocalAddress;audio.rtp.RemoteAddress");
//		ACE_OS::sleep(2);
	}

	if (m_mrcp_ctl_tts != NULL)
	{
		m_mrcp_ctl_tts->SetServerName(m_mrcp_ctl_name);
		// TODO: add method to set the port
		m_mrcp_ctl_tts->SetServerPort(556);
		m_mrcp_ctl_tts->SetLocalRtpPort(m_local_rtp_port);
		m_mrcp_ctl_tts->StartComms();
		
		ACE_OS::sleep(2);
		
//		m_mrcp_ctl_tts->SendDescribe(g_recognizer);
//		ACE_OS::sleep(2);
		
//		m_mrcp_ctl_tts->SendSetup(g_recognizer);		
//		ACE_OS::sleep(2);
	}
			
	/*!if ((m_rtp_trc != NULL) && (m_mrcp_ctl != NULL))
	{
		m_rtp_trc->SetServerName(m_mrcp_ctl_name);
		m_rtp_trc->SetServerPort(m_mrcp_ctl->GetServerRtpPort());		
		m_rtp_trc->SetLocalRtpPort(m_local_rtp_port);
		
		RTPPayloadFormat format = {1, 160, 1, 8000.0};
		m_rtp_trc->SetSyncSource(0);
		m_rtp_trc->SetPayloadFormat(format);
		m_rtp_trc->SetPayloadType(RTP_PAYLOAD_PCMU);
		
		m_rtp_trc->StartComms();
		
		ACE_OS::sleep(2);
	}*/

	if (m_mrcp_ctl != NULL)
	{
//		m_mrcp_ctl->SendGetParams("Vendor-Specific-Parameters:audio.rtp.Streams;audio.rtp.AudioDetectionTimeout;audio.rtp.RecordPayloadSize;audio.rtp.SessionState;audio.rtp.LocalAddress;audio.rtp.RemoteAddress");
//		ACE_OS::sleep(2);
		
		/*!m_mrcp_ctl->SendSetup(g_synthesizer);
		ACE_OS::sleep(2);*/
		
		/*m_mrcp_ctl->SendSetParams("Vendor-Specific-Parameters:audio.rtp.SessionState=idle");
		ACE_OS::sleep(2);
		m_mrcp_ctl->SendSetParams("Vendor-Specific-Parameters:audio.rtp.RemoteAddress=192.33.227.21:1263");
		ACE_OS::sleep(2);
		m_mrcp_ctl->SendSetParams("Vendor-Specific-Parameters:audio.rtp.SessionState=live");
		ACE_OS::sleep(2);*/
		
//_		m_mrcp_ctl->SendDefineGrammar(G_slm_grammar, "calendar_slm.grammar", "application/x-nuance-gsl", "");
//_		m_mrcp_ctl->SendDefineGrammar("http://" + m_mrcp_ctl_name + ":9080/Calendar/grammars/recogniser.grammar", "calendar_gsl.grammar", "text/uri-list", "");
//		m_mrcp_ctl->SendDefineGrammar(G_speech_digit_grammar, "digit.grammar", "application/grammar+xml", "");
//_		ACE_OS::sleep(2);
		
//_		m_mrcp_ctl->SendSpeak("<speak>Welcome 1.</speak>");		
//		ACE_OS::sleep(2);
		/*!m_mrcp_ctl->SendGetParams("Vendor-Specific-Parameters:audio.rtp.Streams;audio.rtp.AudioDetectionTimeout;audio.rtp.RecordPayloadSize;audio.rtp.SessionState;audio.rtp.LocalAddress;audio.rtp.RemoteAddress");		
		ACE_OS::sleep(2);*/
	}

	if (m_mrcp_ctl_tts != NULL)
	{	
		m_mrcp_ctl_tts->SendSetup(g_synthesizer);
		ACE_OS::sleep(2);
				
//_		m_mrcp_ctl_2->SendDefineGrammar(G_slm_grammar, "calendar_slm.grammar", "application/x-nuance-gsl", "");
//_		m_mrcp_ctl_2->SendDefineGrammar("http://" + m_mrcp_ctl_name + ":9080/Calendar/grammars/recogniser.grammar", "calendar_gsl.grammar", "text/uri-list", "");
//_		ACE_OS::sleep(2);
		
//_		m_mrcp_ctl_2->SendSpeak("<speak>Welcome 2.</speak>");
		
		m_mrcp_ctl_tts->SendGetParams("Vendor-Specific-Parameters:audio.rtp.Streams;audio.rtp.AudioDetectionTimeout;audio.rtp.RecordPayloadSize;audio.rtp.SessionState;audio.rtp.LocalAddress;audio.rtp.RemoteAddress");		
		ACE_OS::sleep(2);
	}
	
	if (m_tcp_server != NULL)
	{
		m_tcp_server->SetServerName("");		
		m_tcp_server->SetServerPort(m_local_server_port);		
		m_tcp_server->StartComms();
	}

	if (m_tcp_client != NULL)
	{
		m_tcp_client->SetServerName(m_local_server_name);
		//m_tcp_client->SetServerName("129.194.32.96");		
		m_tcp_client->SetServerPort(m_local_server_port);		
		m_tcp_client->StartComms();
	}
	
	//!m_ccstatus = CC_CONNECTED;
	//!m_observer.InitCompleted(CommunicationChannelObserver::CCStatus(m_ccstatus));
	
	return;
}

// TODO: Recheck
void CommunicationChannel::StopComms()
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::StopComms()]"));

	if (m_mrcp_ctl != NULL)
	{	
		m_mrcp_ctl->SendSetParams("Vendor-Specific-Parameters:audio.rtp.SessionState=idle");
		ACE_OS::sleep(2);	
		m_mrcp_ctl->SendTeardown(g_recognizer);			
		ACE_OS::sleep(1);
		m_mrcp_ctl->SendTeardown(g_synthesizer);
		ACE_OS::sleep(1);		
	}

	if (m_mrcp_ctl_tts != NULL)
	{	
		m_mrcp_ctl_tts->SendSetParams("Vendor-Specific-Parameters:audio.rtp.SessionState=idle");
		ACE_OS::sleep(2);	
		//m_mrcp_ctl_tts->SendTeardown(g_recognizer);			
		//ACE_OS::sleep(1);
		m_mrcp_ctl_tts->SendTeardown(g_synthesizer);
		ACE_OS::sleep(1);		
	}
	
	if (m_rtp_trc != NULL)
	{
		m_rtp_trc->StopComms();
	}
		
	if (m_dlg_ctl != NULL)
	{
		m_dlg_ctl->StopComms();
	}
	
	if (m_trans_ctl != NULL)
	{
		m_trans_ctl->StopComms();
	}
	
	if (m_btrans_ctl != NULL)
	{
		m_btrans_ctl->StopComms();
	}
	
	if (m_mrcp_ctl != NULL)
	{	
		m_mrcp_ctl->StopComms();	
	}
	
	if (m_mrcp_ctl_tts != NULL)
	{	
		m_mrcp_ctl_tts->StopComms();	
	}
	
	if (m_tcp_server != NULL)
	{	
		m_tcp_server->StopComms();	
	}
	
	if (m_tcp_client != NULL)
	{	
		m_tcp_client->StopComms();	
	}
	
	return;
}

void CommunicationChannel::SetGrammar(RecognitionGrammar grammar)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::SetGrammar()]"));

	if (m_mrcp_ctl != NULL)
	{	
		m_grammars.insert(pair<string, RecognitionGrammar>(grammar.GetContentId(), grammar));
		m_mrcp_ctl->SendDefineGrammar(grammar.GetGrammar(), grammar.GetContentId(),
										grammar.GetContentType(), grammar.GetParameters());
	}
	
	return;

}

void CommunicationChannel::UnsetGrammar(const string& grammar)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::UnsetGrammar()]"));

	map<string, RecognitionGrammar>::iterator it;
	
	it = m_grammars.find(grammar);
	
	if (it != m_grammars.end())
	{
		m_grammars.erase(it);
	}
	else
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [CommunicationChannel::UnsetGrammar()] "
					"[Grammar: %s not found.]\n"), grammar.c_str()));		
	}
	
	return;

}

void CommunicationChannel::SetRecognizeFromTTS(bool choice)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::SetRecognizeFromTTS()]"));
	
	m_recognize_from_tts = choice;
	
	return;
}

bool CommunicationChannel::GetRecognizeFromTTS()
{
#if _DEBUG_1
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::GetRecognizeFromTTS()]"));
#endif
	
	return m_recognize_from_tts;
}

void CommunicationChannel::RecognitionCompleted(int status, string& result)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::RecognitionCompleted()]"));

	StopRecording();
	
	if (GetRecognizeFromTTS() == true)
	{
		StopTTSAudioInternal();
		SetRecognizeFromTTS(false);
	}

	m_observer.RecognitionCompleted(status, result);
		
	return;
}

void CommunicationChannel::InterpretationCompleted(int status, string& result)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::InterpretationCompleted()]"));

	m_observer.InterpretationCompleted(status, result);
		
	return;
}

void CommunicationChannel::HandleResponseStatus(string status)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::HandleResponseStatus()]"));
	
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [CommunicationChannel::HandleResponseStatus()] "
						"[Response Status: %s]\n"), status.c_str()));
	
	switch(m_ccrequest_type)
	{
		case CC_DESCRIBE:
			if (m_mrcp_ctl != NULL)
			{
				m_ccrequest_type = CC_SETUP_REC;
				m_mrcp_ctl->SendSetup(g_recognizer);
			}
			break;
		case CC_SETUP_REC:
			if (m_mrcp_ctl != NULL)
			{
				m_ccrequest_type = CC_SETUP_SYN;
				m_mrcp_ctl->SendSetup(g_synthesizer);
			}			
			break;
		case CC_SETUP_SYN:
			if (m_mrcp_ctl != NULL)
			{
				m_ccrequest_type = CC_GET_PARAMS;
				m_mrcp_ctl->SendGetParams("Vendor-Specific-Parameters:audio.rtp.Streams;"
											"audio.rtp.AudioDetectionTimeout;audio.rtp.RecordPayloadSize;"
											"audio.rtp.SessionState;audio.rtp.LocalAddress;audio.rtp.RemoteAddress");
			}
			break;
		case CC_GET_PARAMS:
			if ((m_rtp_trc != NULL) && (m_mrcp_ctl != NULL))
			{
				m_rtp_trc->SetServerName(m_mrcp_ctl_name);
				m_rtp_trc->SetServerPort(m_mrcp_ctl->GetServerRtpPort());		
				m_rtp_trc->SetLocalRtpPort(m_local_rtp_port);
				
				RTPPayloadFormat format = {1, 160, 1, 8000.0};
				m_rtp_trc->SetSyncSource(0);
				m_rtp_trc->SetPayloadFormat(format);
				m_rtp_trc->SetPayloadType(RTP_PAYLOAD_PCMU);
				
				m_rtp_trc->StartComms();
				
				ACE_OS::sleep(2);
			}
			m_ccrequest_type = CC_NULL;
			m_ccstatus = CC_CONNECTED;
			m_observer.InitCompleted(CommunicationChannelObserver::CCStatus(m_ccstatus));
			break;
		default:
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [CommunicationChannel::HandleResponseStatus()] "
										"[Unknown response status!]\n")));
	}
	
	return;
}

/*
void CommunicationChannel::RecognitionCompleted(int status, string& result)
{
#if _DEBUG
	cout << endl << "---[DEBUG] [CommunicationChannel::RecognitionCompleted()]" << endl;
#endif

	if (GetRecognizeFromTTS() == false)
	{
		StopRecording();	
		AllowPlayback(true);
	}
	else
	{
		SetRecognizeFromTTS(false);
	}
	
	if ((status == 1) && (m_mrcp_ctl != NULL))
	{
		if (grm == true)
		{ 
			grm = false;
			RecognizeSLM();
			SendAudioDataForSLM();
			
			//m_mrcp_ctl->SendSpeak("<speak>No match.</speak>");
			ansbuffer = "No match.";
			
			string txt("_NO_MATCH");
			m_observer.RecognitionCompleted(status, txt);		
		}
		else
		{			
			if (ansbuffer.length() > 0)
			{
				m_mrcp_ctl->SendSpeak("<speak>" + ansbuffer + "</speak>");
				ansbuffer = "";
			}
			
			//m_rtp_trc->ClearPacketsBuffer();
			grm = true;
		}
	}
	else if ((status == 2) && (m_mrcp_ctl != NULL))
	{
		if (grm == true)
		{ 
			m_mrcp_ctl->SendSpeak("<speak>Repeat please.</speak>");
			
			string txt("_REPEAT_PLEASE");
			m_observer.RecognitionCompleted(status, txt);	
			
			//m_rtp_trc->ClearPacketsBuffer();					
		}
		else
		{
			if (ansbuffer.length() > 0)
			{
				m_mrcp_ctl->SendSpeak("<speak>" + ansbuffer + "</speak>");
				ansbuffer = "";
			}
			
			//m_rtp_trc->ClearPacketsBuffer();
			grm = true;			
		}		
	}
	else if (m_dlg_ctl != NULL)
	{	
		if (grm == true)
		{ 
			string txt("");
			m_observer.RecognitionCompleted(status, txt);
			
			m_dlg_ctl->SendXmlText(result);
			//ansbuffer = result;		
			grm = false;
			RecognizeSLM();
			SendAudioDataForSLM();
		}
		else
		{
			m_dlg_ctl->SendHelpRequest(result);
			grm = true;
		}		
	}

	if (m_rtp_trc != NULL)
	{
		m_rtp_trc->ClearPacketsBuffer();
	}
	
	return;
}
*/

void CommunicationChannel::GetFromHelpCompleted(string& understood, string& result)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::GetFromHelpCompleted()]"));

//_	ansbuffer = "";
	
/*	if (m_mrcp_ctl_tts != NULL)
	{
		if (GetRecognizeFromTTS() == false)
		{
			m_mrcp_ctl_tts->SendSpeak("<speak>" + result + "</speak>");
		}		
	}*/
	
	m_observer.GetFromHelpCompleted(understood, result);
		
	return;
}

void CommunicationChannel::AudioDataReceived(const Uint8* buffer)
//void CommunicationChannel::AudioDataReceived(ostringstream& buffer)
{	
#if _DEBUG_1
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::AudioDataReceived()]"));
#endif
	
//	string str = buffer.str();
//	const Uint8* buf((Uint8*)str.c_str());	
	if (m_rtp_trc != NULL)
	{
		m_rtp_trc->CreateAndSendPacket(buffer);
	}
	
	return;
}

void CommunicationChannel::SendAudioDataForSLM()
{
#if _DEBUG
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::SendAudioDataForSLM()]"));
#endif
	
	if (m_rtp_trc != NULL)
	{
		m_rtp_trc->SendPackets();		
	}
					
	return;
}

void CommunicationChannel::CloseAudio()
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::CloseAudio()]"));
	
	if (m_audio_eng != NULL)
	{
		m_audio_eng->CloseAudio();
	}
 	
 	return;
}

// TODO: deprecated?
void CommunicationChannel::AllowPlayback(bool choice)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::AllowPlayback()]"));

	m_allow_playback = choice;
	
	return;
}

void CommunicationChannel::StartRecording()
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::StartRecording()]"));
	
//_	grm = true;
	
	if (m_audio_eng != NULL)
	{
		m_audio_eng->StartRecording();
	}
 	
 	return;
}

void CommunicationChannel::StopRecording()
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::StopRecording()]"));

	if (m_audio_eng != NULL)
	{
		m_audio_eng->StopRecording();
	}
	
	return;
}

void CommunicationChannel::SendRegulusMessage(const string message)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::SendRegulusMessage()]"));

	if ((m_dlg_ctl != NULL) && (message != ""))
	{
		m_dlg_ctl->SendDlgMessage(message);
	}
	
	return;
}

void CommunicationChannel::SendBackTranslationMessage(const string message)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::SendBackTranslationMessage()]"));

	if ((m_btrans_ctl != NULL) && (message != ""))
	{
		m_btrans_ctl->SendDlgMessage(message);
	}
	
	return;
}

void CommunicationChannel::SendTranslationMessage(const string message)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::SendTranslationMessage()]"));

	if ((m_trans_ctl != NULL) && (message != ""))
	{
		m_trans_ctl->SendDlgMessage(message);
	}
	
	return;
}

void CommunicationChannel::RequestFromHelp(const string text)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::RequestFromHelp()]"));

	if ((m_dlg_ctl != NULL) && (text != ""))
	{
		m_dlg_ctl->SendRecognitionText(text);
	}
	
	return;
}

void CommunicationChannel::TTSDataReceived(const Uint8* buffer)
{
#if _DEBUG_1
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::TTSDataReceived()]"));
#endif
	
	// Instead of playing back the TTS data, recognize with it
	if (GetRecognizeFromTTS() == false)
	{
		if (m_audio_eng != NULL)
		{		
			m_audio_eng->PlayData(buffer);
		}
	}
	else
	{	
		if (m_rtp_trc != NULL)
		{
			m_rtp_trc->CreateAndSendPacket(buffer);
		}
	}
	
	return;
}

void CommunicationChannel::Recognize(const string grammar, const string lang)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::Recognize()]"));

	map<string, RecognitionGrammar>::iterator it;
	
	it = m_grammars.find(grammar);
	
	if (it == m_grammars.end())
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [CommunicationChannel::Recognize()] "
					"[Grammar: %s was not set.]\n"), grammar.c_str()));	
		return;
	}
	
	ostringstream os;
//	os << "session:digit.grammar" << CRLF;
//	os << "session:calendar_gsl.grammar" << CRLF;
	os << "session:" << grammar << CRLF;
	string uri_list = os.str();
	os.str("");
	os << "Speech-Language: " << lang << CRLF;
//	os << "Confidence-Threshold: 30" << CRLF;
	os << "Confidence-Threshold: " << m_confidence << CRLF;
	os << "Sensitivity-Level: 50" << CRLF;
	os << "Speed-Vs-Accuracy: 50" << CRLF;
	os << "Speech-Complete-Timeout: 300" << CRLF;
	os << "Speech-Incomplete-Timeout: 500" << CRLF;
	os << "No-Input-Timeout: 10000" << CRLF;
	string parameters = os.str();
	
	if (m_mrcp_ctl != NULL)
	{		
		m_mrcp_ctl->SendRecognize(uri_list, parameters);			
	}
	
	return;
}

void CommunicationChannel::StopRecognize()
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::StopRecognize()]"));

	if (m_mrcp_ctl != NULL)
	{		
		m_mrcp_ctl->SendStop(true);			
	}
	
	return;
}

/*
// TODO: Remove this function which is only used
// for changing the confidence threshold
void CommunicationChannel::RecognizeSLM(const string& grammar)
{
#if _DEBUG
	cout << endl << "---[DEBUG] [CommunicationChannel::RecognizeSLM()]" << endl;
#endif

	map<string, RecognitionGrammar>::iterator it;
	
	it = m_grammars.find(grammar.GetContentId());
	
	if (it != m_grammars.end())
	{
		cout << "---[DEBUG] [CommunicationChannel::Recognize()]" 
			<< "[You should set the grammar: " << grammar.GetContentId() 
			<< " before using it.]" << endl;
		
		return;
	}
	
	ostringstream os;
//	os << "session:digit.grammar" << CRLF;
//	os << "session:calendar_slm.grammar" << CRLF;
	os << "session:" << grammar.GetContentId() << CRLF;	
	string uri_list = os.str();
	os.str("");
	os << "Confidence-Threshold: 5" << CRLF;
	os << "Sensitivity-Level: 50" << CRLF;
	os << "Speed-Vs-Accuracy: 50" << CRLF;
	os << "Speech-Complete-Timeout: 300" << CRLF;
	os << "Speech-Incomplete-Timeout: 500" << CRLF;
	os << "No-Input-Timeout: 10000" << CRLF;
	string parameters = os.str();
	
	if (m_mrcp_ctl != NULL)
	{	
		m_mrcp_ctl->SendRecognize(uri_list, parameters);			
	}
	
	return;
}*/

void CommunicationChannel::QueryOutputReceived(RegulusDialogueResult* result)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::QueryOutputReceived()]"));

	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [CommunicationChannel::QueryOutputReceived()] "
					"[Answer: %s]\n"), result->GetQueryOutput().c_str()));	

//_
/*	if (m_mrcp_ctl != NULL)
	{
		ansbuffer = result;
		//m_mrcp_ctl->SendSpeak("<speak>" + result + "</speak>");		
		//grm = false;
		//RecognizeSLM();
		//SendAudioDataForSLM();	
	}
*/	
	
	//!
	/*if (m_dlg_ctl != NULL)
	{
		m_dlg_ctl->SendHelpRequest(ansbuffer);
		ansbuffer = "";
	}*/
	
	m_observer.QueryOutputReceived(result);
		
	return;
}

void CommunicationChannel::HelpSentencesReceived(string& buffer)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::HelpSentencesReceived()]"));
	
/*	if (ansbuffer.length() > 0)
	{
		m_mrcp_ctl->SendSpeak("<speak>" + ansbuffer + "</speak>");
		ansbuffer = "";
	}
*/	

	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [CommunicationChannel::HelpSentencesReceived()] "
					"[Help sentences: %s]\n"), buffer.c_str()));	
	
	m_observer.HelpSentencesReceived(buffer);
			
	return;
}

void CommunicationChannel::BackTranslationReceived(RegulusTranslationResult* result)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::BackTranslationReceived()]"));
	
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [CommunicationChannel::BackTranslationReceived()] "
					"[Back translation: %s]\n"), (result->GetTextTranslation()).c_str()));	
	
	m_observer.BackTranslationReceived(result);
			
	return;
}
	
void CommunicationChannel::TranslationReceived(RegulusTranslationResult* result)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::TranslationReceived()]"));

	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [CommunicationChannel::TranslationReceived()] "
					"[Translation: %s]\n"), (result->GetTextTranslation()).c_str()));	
	
	m_observer.TranslationReceived(result);
	
	return;
}

void CommunicationChannel::ClientMessageReceived(string& buffer)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::ClientMessageReceived()]"));
	
	m_observer.ClientMessageReceived(buffer);
	
	return;
}

void CommunicationChannel::ServerMessageReceived(string& buffer)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::ServerMessageReceived()]"));
	
	m_observer.ServerMessageReceived(buffer);
	
	return;
}

void CommunicationChannel::RecognizeFromUser(const string& grammar, 
												const string& lang, bool online)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::RecognizeFromUser()]"));

	SetRecognizeFromTTS(false);
	
	// Ugly hack. You can do slm recognition 
	// with prestored audio packets.
	// This problem is due to the fact that MRCP 1.0
	// does not support offline recognition.
	if (online == true)
	{
		if (m_rtp_trc != NULL)
		{
			m_rtp_trc->ClearPacketsBuffer();
		}
		
		Recognize(grammar, lang);
	}
	else
	{
		Recognize(grammar, lang);
		SendAudioDataForSLM();
	}
	
	/*
	if (model == 0)
	{
		if (m_rtp_trc != NULL)
		{
			m_rtp_trc->ClearPacketsBuffer();
		}
		
		//RecognizeGLM();
	}
	else
	{
		//RecognizeSLM();
		SendAudioDataForSLM();
	}*/
		
	return;
}

void CommunicationChannel::RecognizeFromTTS(const string input, const string& grammar, const string lang)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::RecognizeFromTTS()]"));
		 
	SetRecognizeFromTTS(true);
		
	if (m_mrcp_ctl_tts != NULL)
	{
		//string txt = "<speak>When is the next meeting.<break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/>n.</speak>";		
		//string txt = "<?xml version=\"1.0\"?><speak>" + input + "<break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/>n.</speak>"; 
		string txt = "<?xml version=\"1.0\"?><speak>" + input + "<break strength=\"x-strong\"/>n.</speak>";
		m_mrcp_ctl_tts->SendSpeakSSML(txt.c_str(), lang);	
	}

	if (m_rtp_trc != NULL)
	{
		m_rtp_trc->ClearPacketsBuffer();
	}
	
	Recognize(grammar, lang);
		
	/*
	if (model == 0)
	{		
		RecognizeGLM();
	}
	else
	{
		RecognizeSLM();
	}*/
	
	return;
}

void CommunicationChannel::StartOfSpeech()
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::StartOfSpeech()]"));

	m_observer.StartOfSpeech();
	
	return;
}

void CommunicationChannel::Speak(const string input, const string lang)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::Speak()]"));
						
	if (m_mrcp_ctl != NULL)
	{		
		m_mrcp_ctl->SendSpeak(input.c_str(), lang);	
	}

	return;
}

void CommunicationChannel::SpeakSSML(const string input, const string lang)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::SpeakSSML()]"));
				
	if (m_mrcp_ctl != NULL)
	{		
		m_mrcp_ctl->SendSpeakSSML(input.c_str(), lang);	
	}

	return;
}

void CommunicationChannel::SpeakSSMLInternal(const string input, const string lang)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::SpeakSSML()]"));
				
	if (m_mrcp_ctl_tts != NULL)
	{		
		m_mrcp_ctl_tts->SendSpeakSSML(input.c_str(), lang);	
	}

	return;
}

void CommunicationChannel::GetQueryData(const string result)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::GetQueryData()]"));
				
	if (m_dlg_ctl != NULL)
	{	
		m_dlg_ctl->SendXmlText(result);
	}
	
	return;
}

void CommunicationChannel::GetHelpExamples(const string result)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::GetHelpExamples()]"));
				
	if (m_dlg_ctl != NULL)
	{	
		m_dlg_ctl->SendHelpRequest(result);
	}
	else if (m_trans_ctl != NULL)
	{	
		m_trans_ctl->SendHelpRequest(result);
	}
	
	return;
}

void CommunicationChannel::GetInterpretation(const string transcription,
											 const string grammar, const string lang)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::GetInterpretation()]"));
	
	map<string, RecognitionGrammar>::iterator it;
	
	it = m_grammars.find(grammar);
	
	if (it == m_grammars.end())
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [CommunicationChannel::GetInterpretation()] "
					"[Grammar: %s was not set.]\n"), grammar.c_str()));		
		
		return;
	}
	
	ostringstream os;
//	os << "session:digit.grammar" << CRLF;
//	os << "session:calendar_gsl.grammar" << CRLF;
	os << "session:" << grammar << CRLF;
	string uri_list = os.str();
	os.str("");
	os << "Speech-Language: " << lang << CRLF;	
//	os << "Confidence-Threshold: 30" << CRLF;
//	os << "Confidence-Threshold: " << m_confidence << CRLF;
//	os << "Sensitivity-Level: 50" << CRLF;
//	os << "Speed-Vs-Accuracy: 50" << CRLF;
//	os << "Speech-Complete-Timeout: 300" << CRLF;
//	os << "Speech-Incomplete-Timeout: 500" << CRLF;
//	os << "No-Input-Timeout: 10000" << CRLF;
	string parameters = os.str();
	
	if (m_mrcp_ctl != NULL)
	{		
		m_mrcp_ctl->SendInterpret(transcription, uri_list, parameters);			
	}
	
	return;
}

void CommunicationChannel::GetBackTranslation(const string result)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::GetBackTranslation()]"));
				
	if (m_btrans_ctl != NULL)
	{	
		m_btrans_ctl->SendTranslationRequest(result, true);
	}
	
	return;
}

void CommunicationChannel::GetTranslation(const string result)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::GetTranslation()]"));
				
	if (m_trans_ctl != NULL)
	{	
		m_trans_ctl->SendTranslationRequest(result, false);
	}
	
	return;
}

void CommunicationChannel::RegisterExternalUtterance(const string& utterance)
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::RegisterExternalUtterance()]"));
				
	if (m_btrans_ctl != NULL)
	{	
		m_btrans_ctl->SendRegisterExternalUtterance(utterance);
	}
	
	if (m_trans_ctl != NULL)
	{	
		m_trans_ctl->SendRegisterExternalUtterance(utterance);
	}
	
	return;
}

void CommunicationChannel::RevertDiscourseContext()
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::RevertDiscourseContext()]"));

	if (m_dlg_ctl != NULL)
	{	
		m_dlg_ctl->SendRevertDiscourseContext();
	}
	
	if (m_btrans_ctl != NULL)
	{	
		m_btrans_ctl->SendRevertDiscourseContext();
	}
	
	if (m_trans_ctl != NULL)
	{	
		m_trans_ctl->SendRevertDiscourseContext();
	}
	
	return;
}

void CommunicationChannel::StopTTSAudio()
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::StopTTSAudio()]"));
				
	if (m_mrcp_ctl != NULL)
	{		
		m_mrcp_ctl->SendStop(false);	
	}
	
	return;
}

void CommunicationChannel::StopTTSAudioInternal()
{
	ACE_TRACE(ACE_TEXT("[CommunicationChannel::StopTTSAudioInternal()]"));
				
	if (m_mrcp_ctl_tts != NULL)
	{		
		m_mrcp_ctl_tts->SendStop(false);	
	}
	
	return;
}

void CommunicationChannel::SendResultToClient(const string result)
{
	ACE_TRACE("[CommunicationChannel::SendResultToClient()]");
	
	if (m_tcp_server != NULL)
	{
		m_tcp_server->SendMessage(result);		
	}
	
	return;
}

void CommunicationChannel::SendResultToServer(const string result)
{
	ACE_TRACE("[CommunicationChannel::SendResultToServer()]");
	
	if (m_tcp_client != NULL)
	{
		m_tcp_client->SendMessage(result);		
	}
	
	return;
}
