/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	CommunicationChannelObserver.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef COMMUNICATIONCHANNELOBSERVER_HPP_
#define COMMUNICATIONCHANNELOBSERVER_HPP_

#include <string>
#include "RegulusDialogueResult.hpp"
#include "RegulusTranslationResult.hpp"

using namespace std;

class CommunicationChannelObserver
{
public:
	enum CCStatus { CC_CONNECTED, CC_DISCONNECTED, CC_OK, CC_ERROR };
		
	virtual ~CommunicationChannelObserver(){};
	
	virtual void InitCompleted(CCStatus status)=0;
	virtual void StartOfSpeech()=0;
	virtual void RecognitionCompleted(int status, string& result)=0;
	virtual void InterpretationCompleted(int status, string& result)=0;
	virtual void QueryOutputReceived(RegulusDialogueResult* result)=0;
	virtual void GetFromHelpCompleted(string& understood, string& result)=0;
	virtual void HelpSentencesReceived(string& buffer)=0;	
	virtual void BackTranslationReceived(RegulusTranslationResult* result)=0;
	virtual void TranslationReceived(RegulusTranslationResult* result)=0;
	virtual void ClientMessageReceived(string& buffer)=0;
	virtual void ServerMessageReceived(string& buffer)=0;
};

#endif /*COMMUNICATIONCHANNELOBSERVER_HPP_*/
