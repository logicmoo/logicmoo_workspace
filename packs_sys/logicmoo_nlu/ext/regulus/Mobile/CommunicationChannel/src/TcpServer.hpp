/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	TcpServer.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#ifndef TCPSERVER_HPP_
#define TCPSERVER_HPP_

#include <string>
#include "TcpServerObserver.hpp"
#include "SocketsEngineObserver.hpp"
#include "SocketsEngine.hpp"
#include "TcpSocket.hpp"
#include "SDL_net.h"

class TcpServer : public SocketsEngineObserver
{
public:
	//enum ResponseType { SEND_MESSAGE, QUERY_OUTPUT, QUERY_OUTPUT_FROM_HELP, HELP_SENTENCES, TRANSLATION, BACK_TRANSLATION };
	
	TcpServer(TcpServerObserver& observer);
						
	~TcpServer();
	
	// Set the name of the Tcp Server
	void SetServerName(const string name);
	// Get the name of the Tcp Server
	const string GetServerName();
	// Set the port number of the Tcp Server
	void SetServerPort(int port);
	// Get the port number of the Tcp Server
	int GetServerPort();
	// Connect to the Tcp server
    void StartComms();
    // Disconnect from the Tcp server
	void StopComms();
	// Send a message to the Tcp server
	void SendMessage(const string& message);
	// Send the input to the Tcp server
	void SendDlgMessage(const string& input);
	// Send disconnect message to the Tcp server
	void SendDisconnectMessage();
    // Send the recognition text to the Tcp server
    void SendRecognitionText(const string& input);
    // Request the help sentences from the Tcp server
    void SendHelpRequest(const string& input);
    // Send the recognition result in xml format
    void SendXmlText(const string& input);
    // Send the xml input for translation
    void SendTranslationRequest(const string& input, bool type);
    // Find the search string in the input string and 
    // replace it with the replace string
    bool FindAndReplace(string& input_str,
						const string& search_str, 
						const string& replace_str);
     // Split help sentences
    void SplitHelpSentences(string& buffer);
    // Extract the output from the Tcp server response
    bool ExtractOutput(string& buffer, 
    					const string left, const string right);
    // Handle the response received by the Tcp server
    void HandleResponse();
     // From SocketsObserver 
    // Process the received message from the Tcp server
    void MessageReceived(ostringstream& buffer);

private:
	
	// Avoid accidental copy or assignment
	TcpServer(const TcpServer&);
	TcpServer& operator = (const TcpServer&);
	
private:
   
	TcpServerObserver& m_observer;
	string				m_server;
	int					m_port;
	string				m_str;
//	ResponseType		m_response_type;
	
	string				m_input;
	string				m_buffer;
	
	bool				m_is_connected;
	
	SocketsEngine*		m_socket_engine;
};

#endif /*TCPSERVER_HPP_*/
