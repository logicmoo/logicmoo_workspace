/*
 * Copyright 2007-2008 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	DialogueControllerObserver.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@issco.unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef DIALOGUECONTROLLEROBSERVER_HPP_
#define DIALOGUECONTROLLEROBSERVER_HPP_

#include <string>

using namespace std;

class DialogueControllerObserver
{
public:
	
	virtual ~DialogueControllerObserver(){};
	
	virtual void RecognitionCompleted(int status, string& result)=0;
	virtual void QueryOutputReceived(string& understood, string& paraphrase, string& result)=0;
	virtual void GetFromHelpCompleted(string& understood, string& result)=0;
	virtual void HelpSentencesReceived(string& buffer)=0;
	virtual void BackTranslationReceived(string& buffer)=0;
	virtual void TranslationReceived(string& buffer)=0;
};

#endif /*DIALOGUECONTROLLEROBSERVER_HPP_*/
