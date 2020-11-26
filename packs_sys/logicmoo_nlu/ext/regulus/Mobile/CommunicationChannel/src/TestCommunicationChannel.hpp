/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	TestCommunicationChannel.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef TESTCOMMUNICATIONCHANNEL_HPP_
#define TESTCOMMUNICATIONCHANNEL_HPP_

#include "CommunicationChannel.hpp"

using namespace std;

class TestCommunicationChannel : public CommunicationChannelObserver
{
public:

	TestCommunicationChannel();
	~TestCommunicationChannel();
	
	void StartComms(string server);
	void StopComms();
	void SetRecGrammars();
	void StopRecording();
	void RecognizeFromUser(const string& grammar, 
							const string& lang, bool online);
	void RecognizeFromTTS(const string& input, 
							const string& grammar, const string& lang);
	void StartRecording();
	void AllowPlayback(bool choice);
	
	void FindAndReplace(string &input_str,
						const string &search_str, 
						const string &replace_str);
	void SplitHelpExamples(string& buffer, 
								string& result, const string token);
	void PlayResult();
	void SetRecognizeFromTTS(bool choice);	
	bool IsRecognizeFromTTS();
	void SetConfidence(int value);	
	int GetConfidence();
		
	// From CommunicationChannelObserver
	void InitCompleted(CCStatus status);
	void StartOfSpeech();
	void RecognitionCompleted(int status, string& result);
	void InterpretationCompleted(int status, string& result);
	void GetFromHelpCompleted(string& understood, string& result);
	void QueryOutputReceived(RegulusDialogueResult* result);
	void HelpSentencesReceived(string& buffer);	
	void BackTranslationReceived(RegulusTranslationResult* result);
	void TranslationReceived(RegulusTranslationResult* result);
	void ClientMessageReceived(string& buffer);
	void ServerMessageReceived(string& buffer);
	
private:
	
	// Avoid accidental copy or assignment
	TestCommunicationChannel(const TestCommunicationChannel&);
	TestCommunicationChannel& operator = (const TestCommunicationChannel&);
	
private:
	CommunicationChannel*		m_comm_channel;
	RegulusDialogueResult*		m_result;
	int							m_online;
	bool						m_recognize_from_tts;
};

#endif /*TESTCOMMUNICATIONCHANNEL_HPP_*/
