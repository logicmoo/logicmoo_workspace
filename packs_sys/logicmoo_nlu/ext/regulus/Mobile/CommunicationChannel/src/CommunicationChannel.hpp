/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	CommunicationChannel.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef COMMUNICATIONCHANNEL_HPP_
#define COMMUNICATIONCHANNEL_HPP_

/* Use the newer ALSA API */
#define ALSA_PCM_NEW_HW_PARAMS_API

#include <map>
#include <alsa/asoundlib.h>

#include "CommunicationChannelObserver.hpp"
#include "MrcpControllerObserver.hpp"
#include "MrcpController.hpp"
#include "RtpTransceiverObserver.hpp"
#include "RtpTransceiver.hpp"
#include "AudioStreamEngineObserver.hpp"
#include "AudioStreamEngine.hpp"
#include "RegulusControllerObserver.hpp"
#include "RegulusController.hpp"
#include "TcpServerObserver.hpp"
#include "TcpServer.hpp"
#include "TcpClient.hpp"
#include "RecognitionGrammar.hpp"

#define	ALL_SERVERS				0xff
#define	DIALOGUE_SERVER			0x01
#define	TRANSLATION_SERVER		0x02
#define	BACK_TRANSLATION_SERVER	0x04
#define	MRCP_SERVER_1			0x08
#define MRCP_SERVER_2       	0x10
#define TCP_SERVER       		0x20
#define TCP_CLIENT       		0x40

class CommunicationChannel : public MrcpControllerObserver, RtpTransceiverObserver, 
									AudioStreamEngineObserver, RegulusControllerObserver,
									TcpServerObserver, TcpClientObserver
{
public:
	enum CCStatus { CC_CONNECTED, CC_DISCONNECTED, CC_OK, CC_ERROR };
	enum CCRequestType { CC_NULL, CC_DESCRIBE, CC_SETUP_REC, 
							CC_SETUP_SYN, CC_DEFINE_GRAMMAR, CC_SET_PARAMS, CC_GET_PARAMS };
	
	CommunicationChannel(CommunicationChannelObserver& observer,
						 unsigned int server_mask);
	~CommunicationChannel();
	
	// Set the name of the Mrcp Server
	void SetMrcpServerName(const string name);
	// Get the name of the Mrcp Server
	const string GetMrcpServerName();
	// Set the port number of the Mrcp Server
	void SetMrcpServerPort(int port);
	// Get the port number of the Mrcp Server
	int GetMrcpServerPort();
	
	// Set the name of the Regulus Server
	void SetDlgServerName(const string name);
	// Get the name of the Regulus Server
	const string GetDlgServerName();
	// Set the port number of the Regulus Server
	void SetDlgServerPort(int port);
	// Get the port number of the Regulus Server
	int GetDlgServerPort();
	
	// Set the name of the Translation Server
	void SetTransServerName(const string name);
	// Get the name of the Translation Server
	const string GetTransServerName();
	// Set the port number of the Translation Server
	void SetTransServerPort(int port);
	// Get the port number of the Translation Server
	int GetTransServerPort();
	
	// Set the name of the Back Translation Server
	void SetBTransServerName(const string name);
	// Get the name of the Back Translation Server
	const string GetBTransServerName();
	// Set the port number of the Back Translation Server
	void SetBTransServerPort(int port);
	// Get the port number of the Back Translation Server
	int GetBTransServerPort();
	
	// Set the name of the Local TCP Server
	void SetLocalServerName(const string name);
	// Get the name of the Local TCP Server
	const string GetLocalServerName();
	// Set the port number of the Local TCP Server
	void SetLocalServerPort(int port);
	// Get the port number of the Local TCP Server
	int GetLocalServerPort();
		
	// Set the port number of the local Rtp port
	void SetRtpLocalPort(int port);
	// Get the port number of the local Rtp port
	int GetRtpLocalPort();
	
	// Set the confidence value for recognition
	void SetConfidence(int value);
	// Get the confidence value for recognition
	int GetConfidence();
	
	// Start all communications
	void StartComms();
	// Stop all communications
	void StopComms();
	
	// Add a new grammar in the list 
	// and send it to the MRCP server
	void SetGrammar(RecognitionGrammar grammar);
	
	// Remove a new grammar in the list
	void UnsetGrammar(const string& grammar);
			
	// Set whether or not to recognize using the TTS
	void SetRecognizeFromTTS(bool choice);
	// Get whether or not to recognize using the TTS
	bool GetRecognizeFromTTS();
	
	// From MrcpControllerObserver
	// Start of Speech Event
	void StartOfSpeech();
	// Recognition completed event
	void RecognitionCompleted(int status, string& result);
	// Interpretation completed
	void InterpretationCompleted(int status, string& result);
	// Handle the status of a response
	void HandleResponseStatus(string status);
	
	// From AudioStreamEngineObserver 
	// Audio data is available
	void AudioDataReceived(const Uint8* buffer);
	//void AudioDataReceived(ostringstream& buffer);	
		
	// From RtpTransceiverObserver 
	// TTS data is available
	void TTSDataReceived(const Uint8* buffer);
	
	// From RegulusControllerObserver 
	// Data from the Regulus server are available
	void QueryOutputReceived(RegulusDialogueResult* result);
	// Data from a help request are available
	void GetFromHelpCompleted(string& understood, string& result);
	// Help sentences are received
	void HelpSentencesReceived(string& buffer);
	// Interpretation received
	void InterpretationReceived(string& buffer);
	// Back translation received
	void BackTranslationReceived(RegulusTranslationResult* result);
	// Translation received
	void TranslationReceived(RegulusTranslationResult* result);
	
	//From TcpServerObserver
	void ClientMessageReceived(string& buffer);

	//From TcpServerObserver
	void ServerMessageReceived(string& buffer);
	
	// Use user's audio and perform recognition
	void RecognizeFromUser(const string& grammar, 
							const string& lang, bool online);
	// Use TTS audio and perform recognition
	void RecognizeFromTTS(const string input, 
							const string& grammar, const string lang);	
	// Send a recognize request
	void Recognize(const string grammar, const string lang);
	// Stop recognizing
	void StopRecognize();
	// Send plain text to the recognizer
	void Speak(const string input, const string lang);
	// Send ssml text to the recognizer
	void SpeakSSML(const string input, const string lang);	
	// Stop the audio from the TTS
	void StopTTSAudio();
	// Close the audio interface
	void CloseAudio();
	// Allow or not the playback of the received audio packets
	void AllowPlayback(bool choice);
	// Capture the audio from the device
	void StartRecording();
	// Stop capturing the audio from the device
	void StopRecording();
	// Send a message to the Regulus Server
	void SendRegulusMessage(const string message);
	// Send a message to the Translation Server
	void SendTranslationMessage(const string message);
	// Send a message to the Back Translation Server
	void SendBackTranslationMessage(const string message);
	// Request new result using a help sentence
	void RequestFromHelp(const string text);
	// Request data using an xml rec result
	void GetQueryData(const string result);
	// Request help examples using an xml rec result
	void GetHelpExamples(const string result);
	// Request the interpretation of a transcription 
	void GetInterpretation(const string transcription,
							const string grammar, const string lang);
	// Request back translation using an xml rec result
	void GetBackTranslation(const string result);
	// Request translation using an xml rec result
	void GetTranslation(const string result);
	// Register the utterance to the two translation servers
	void RegisterExternalUtterance(const string& utterance);
	// Revert the context in the Back Translation Server
	void RevertDiscourseContext();
	// Send the result to the tcp server
	void SendResultToServer(const string result);
	// Send the result to the tcp client
	void SendResultToClient(const string result);
	
private:
	
	// Use the second TTS for recognition
	void SpeakSSMLInternal(const string input, const string lang);
	// Work around for doing a second recognition using the SLM
	void SendAudioDataForSLM();
	// Stop audio from the second TTS
	void StopTTSAudioInternal();

private:
	
	// Avoid accidental copy or assignment
	CommunicationChannel(const CommunicationChannel&);
	CommunicationChannel& operator = (const CommunicationChannel&);
	
public:

	MrcpController*		m_mrcp_ctl;
	MrcpController*		m_mrcp_ctl_tts;
	RtpTransceiver*		m_rtp_trc;
	AudioStreamEngine*	m_audio_eng;
	RegulusController*	m_dlg_ctl;
	RegulusController*	m_trans_ctl;
	RegulusController*	m_btrans_ctl;
	TcpServer*			m_tcp_server;
	TcpClient*			m_tcp_client;
	
private:
	
	CommunicationChannelObserver& m_observer;
	map<string, RecognitionGrammar> m_grammars;
	
	CCStatus			m_ccstatus;
	CCRequestType		m_ccrequest_type;
	string				m_mrcp_ctl_name;
	int					m_mrcp_ctl_port;
	string				m_rtp_trc_name;
	int					m_rtp_trc_port;
	string				m_dlg_ctl_name;
	int					m_dlg_ctl_port;
	string				m_trans_ctl_name;
	int					m_trans_ctl_port;
	string				m_btrans_ctl_name;
	int					m_btrans_ctl_port;
	string				m_local_server_name;
	int					m_local_server_port;
	int					m_local_rtp_port;
	bool				m_allow_playback;
	bool				m_recognize_from_tts;
	int					m_confidence;
};

#endif /*COMMUNICATIONCHANNEL_HPP_*/
