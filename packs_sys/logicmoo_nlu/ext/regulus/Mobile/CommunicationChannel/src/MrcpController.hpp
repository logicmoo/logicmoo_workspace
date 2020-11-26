/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MrcpController.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef MRCPCONTROLLER_HPP_
#define MRCPCONTROLLER_HPP_

#include <string>
#include <sstream>
#include "MrcpControllerObserver.hpp"
#include "MrcpControllerDefs.hpp"
#include "SocketsEngineObserver.hpp"
#include "SocketsEngine.hpp"
#include "RtspMessage.hpp"
#include "TcpSocket.hpp"
#include "SDL_net.h"

using namespace std;

class MrcpController : public SocketsEngineObserver
{
public:

	enum State { header, body };
	enum RtspRequestType { SETUP, ANNOUNCE, TEARDOWN };
	enum MrcpRequestType { DEFINE_GRAMMAR, RECOGNIZE, SET_PARAMS, GET_PARAMS, INTERPRET, STOP, SPEAK } ;
	
	MrcpController::MrcpController(MrcpControllerObserver& observer);
	~MrcpController();
	
	// Set the name of the Mrcp Server
	void SetServerName(const string name);
	// Get the name of the Mrcp Server
	const string GetServerName();
	// Set the port number of the Mrcp Server
	void SetServerPort(int port);
	// Get the port number of the Mrcp Server
	int GetServerPort();
	// Set the local rtp port number
	void SetLocalRtpPort(int port);
	// Get the local rtp port number
	int GetLocalRtpPort();
		
	// All the requests for ASR and TTS.
	void SendDescribe(string const& resource);
	void SendSetup(string const& resource, string const& sdp = "");
	void SendDefineGrammar(string const& grammar, string const& content_id, 
							string const& content_type, string const& parameters = "");
	int  SendRecognize(string const& uri_list, // should be a single grammar (really?)
						string const& parameters = "");
	void SendStop(bool restype);
	void SendTeardown(string const& resource);
	void SendSetParams(string const& params);
	void SendGetParams(string const& params);
	int SendInterpret(string const& transcription,
						string const& uri_list,
						string const& parameters);
	void SendSpeak(string const& text, string const& lang);
	void SendSpeakSSML(string const& text, string const& lang);
	 
	void HandleReadTrigger();
	void ReceiveHeader();
	void ReceiveBody();
	
	void HandleMessage();
	void HandleRequest();
	void HandleResponse();
	
	void HandleSetupResponse();
	void HandleAnnounceResponse();
	void HandleTeardownResponse();
	
	unsigned short GetServerRtpPort();
	
	// Connect to the Mrcp server
	void StartComms();
	// Disconnect from the Mrcp server
	void StopComms();
	// From SocketsObserver 
	// Process the received message from the Mrcp server
	void MessageReceived(ostringstream& buffer);
	// Return th body of the MRCP message
	const string& GetMessageBody();
	// Temporary helper function in order to get 
	// the recognition transcription
	const string& GetRecTransription();

private:
	
	// Avoid accidental copy or assignment
	MrcpController(const MrcpController&);
	MrcpController& operator = (const MrcpController&);
	
private:
	MrcpControllerObserver&	m_observer;
	unsigned short			m_client_rtp_port;

	// Socket and data buffer variables
	char				m_simUserSpeech[MAX_FILENAME_LEN];
	string				m_server;
	int					m_server_port;
	string				m_socket_buf;
	string				m_head_buf;
	string				m_body_buf;
	unsigned int		m_message_body_rcvd_size;
	RtspMessage*		m_message;
	string				m_session;
	unsigned short		m_server_rtp_port;
	string				m_server_rtp_address;
	RtspRequestType		m_rtsp_request_type;
	MrcpRequestType		m_mrcp_request_type;
	
	bool				m_is_connected;

//	unsigned int m_state;
//	unsigned int m_rtp_parameters_set;
//	unsigned int m_undo_rtp;
//	unsigned int m_wait_for_tag; // tag for which we await completion

	unsigned int	m_cseq;
	unsigned int	m_request_tag;
	
    State			m_recv_state;

    int				m_status_code;
    
    SocketsEngine*	m_socket_engine;
};
		      
#endif /*MRCPCONTROLLER_HPP_*/
