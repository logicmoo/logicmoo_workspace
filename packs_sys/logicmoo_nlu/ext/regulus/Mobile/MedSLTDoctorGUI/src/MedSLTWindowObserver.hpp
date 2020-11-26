/*
 * Copyright 2007-2008 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MedSLTWindowObserver.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@issco.unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#ifndef MEDSLTWINDOWOBSERVER_HPP_
#define MEDSLTWINDOWOBSERVER_HPP_

class MedSLTWindowObserver
{
public:
	
	virtual ~MedSLTWindowObserver(){};
	
	virtual void ConnectToRemote()=0;
	virtual void DisconnectFromRemote()=0;
	virtual void StartRecognize()=0;	
	virtual void AbortRecognize()=0;
	virtual void GetTranslationFromHelp(gchar* text)=0;
	virtual void QuitApp()=0;	
};

#endif /*MEDSLTWINDOWOBSERVER_HPP_*/
