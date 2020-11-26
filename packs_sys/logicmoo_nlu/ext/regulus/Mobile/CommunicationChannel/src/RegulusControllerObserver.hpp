/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RegulusControllerObserver.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef REGULUSCONTROLLEROBSERVER_HPP_
#define REGULUSCONTROLLEROBSERVER_HPP_

#include <string>
#include "RegulusDialogueResult.hpp"
#include "RegulusTranslationResult.hpp"

using namespace std;

class RegulusControllerObserver
{
public:
	
	virtual ~RegulusControllerObserver(){};
	
	virtual void RecognitionCompleted(int status, string& result)=0;
	virtual void QueryOutputReceived(RegulusDialogueResult* result)=0;
	virtual void GetFromHelpCompleted(string& understood, string& result)=0;
	virtual void HelpSentencesReceived(string& buffer)=0;
	virtual void BackTranslationReceived(RegulusTranslationResult* result)=0;
	virtual void TranslationReceived(RegulusTranslationResult* result)=0;
};

#endif /*REGULUSCONTROLLEROBSERVER_HPP_*/
