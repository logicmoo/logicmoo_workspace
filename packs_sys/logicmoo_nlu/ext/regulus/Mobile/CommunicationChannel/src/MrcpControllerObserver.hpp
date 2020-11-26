/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MrcpControllerObserver.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef MRCPCONTROLLEROBSERVER_HPP_
#define MRCPCONTROLLEROBSERVER_HPP_

#include <string>

using namespace std;

class MrcpControllerObserver
{
public:
	
	virtual ~MrcpControllerObserver(){};
	
	virtual void StartOfSpeech()=0;
	virtual void RecognitionCompleted(int status, string& result)=0;
	virtual void InterpretationCompleted(int status, string& result)=0;
};

#endif /*MRCPCONTROLLEROBSERVER_HPP_*/
