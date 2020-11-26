/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MedSLTPatWindowObserver.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#ifndef MEDSLTPATWINDOWOBSERVER_HPP_
#define MEDSLTPATWINDOWOBSERVER_HPP_

class MedSLTPatWindowObserver
{
public:
	
	virtual ~MedSLTPatWindowObserver(){};
	
	virtual void ConnectToRemote()=0;
	virtual void DisconnectFromRemote()=0;
	virtual void StartRecognize()=0;	
	virtual void AbortRecognize()=0;
	virtual void GetTranslationFromHelp(gchar* text)=0;
	virtual void QuitApp()=0;	
};

#endif /*MEDSLTPATWINDOWOBSERVER_HPP_*/
