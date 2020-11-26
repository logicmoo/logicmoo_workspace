/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	CalendarWindowObserver.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#ifndef CALENDARWINDOWOBSERVER_HPP_
#define CALENDARWINDOWOBSERVER_HPP_

class CalendarWindowObserver
{
public:
	
	virtual ~CalendarWindowObserver(){};
	
	virtual void ConnectToRemote()=0;
	virtual void DisconnectFromRemote()=0;
	virtual void InitializeDialogue()=0;
	virtual void ShowHistory()=0;
	virtual void StartRecognize()=0;	
	virtual void AbortRecognize()=0;
	virtual void RequestFromHelp(gchar* text, bool allowplayback)=0;
	virtual void QuitApp()=0;	
};

#endif /*CALENDARWINDOWOBSERVER_HPP_*/
