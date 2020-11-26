/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RegulusController.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#ifndef REGULUSCONTROLLER_HPP_
#define REGULUSCONTROLLER_HPP_

#include <string>
#include "RegulusControllerObserver.hpp"
#include "RegulusTranslationResult.hpp"
#include "SocketsEngineObserver.hpp"
#include "SocketsEngine.hpp"
#include "TcpSocket.hpp"
#include "SDL_net.h"

#define	DIALOGUE_RESULT			0x01
#define	TRANSLATION_RESULT		0x02

class RegulusController : public SocketsEngineObserver
{
public:
	enum ResponseType { SEND_MESSAGE, QUERY_OUTPUT, QUERY_OUTPUT_FROM_HELP, HELP_SENTENCES, 
						TRANSLATION, BACK_TRANSLATION, REGISTER_EXTERNAL, REVERT_CONTEXT, DISCONNECT };
	
	RegulusController(RegulusControllerObserver& observer, unsigned int mask);
						
	~RegulusController();
	
	void SendHello();
	
	// Set the name of the Regulus Server
	void SetServerName(const string name);
	// Get the name of the Regulus Server
	const string GetServerName();
	// Set the port number of the Regulus Server
	void SetServerPort(int port);
	// Get the port number of the Regulus Server
	int GetServerPort();
	// Connect to the Regulus server
    void StartComms();
    // Disconnect from the Regulus server
	void StopComms();
	// Send a message to the Regulus server
	void SendMessage(const string& message);
	// Send the input to the Regulus server
	void SendDlgMessage(const string& input);
	// Send disconnect message to the Regulus server
	void SendDisconnectMessage();
    // Send the recognition text to the Regulus server
    void SendRecognitionText(const string& input);
    // Request the help sentences from the Regulus server
    void SendHelpRequest(const string& input);
    // Send the recognition result in xml format
    void SendXmlText(const string& input);
    // Send the xml input for translation
    void SendTranslationRequest(const string& input, bool type);
    // Register the utterance to the Regulus server
    void SendRegisterExternalUtterance(const string& utterance); 
    // Revert the discourse context of the Regulus server
    void SendRevertDiscourseContext();
    // Find the search string in the input string and 
    // replace it with the replace string
    bool FindAndReplace(string& input_str,
						const string& search_str, 
						const string& replace_str);
     // Split help sentences
    void SplitHelpSentences(string& buffer);
    // Extract the output from the Regulus server response
    bool ExtractOutput(string& buffer, 
    					const string left, const string right);
    // Handle the response received by the Regulus server
    void HandleResponse();
     // From SocketsObserver 
    // Process the received message from the Regulus server
    void MessageReceived(ostringstream& buffer);

private:
	
	// Avoid accidental copy or assignment
	RegulusController(const RegulusController&);
	RegulusController& operator = (const RegulusController&);
	
private:
   
	RegulusControllerObserver&	m_observer;	
	RegulusResult*				m_result;
	string						m_server;
	int							m_port;
	string						m_str;
	ResponseType				m_response_type;
	
	string						m_input;
	string						m_buffer;
	
	bool						m_is_connected;
	
	SocketsEngine*				m_socket_engine;
};

#endif /*REGULUSCONTROLLER_HPP_*/
