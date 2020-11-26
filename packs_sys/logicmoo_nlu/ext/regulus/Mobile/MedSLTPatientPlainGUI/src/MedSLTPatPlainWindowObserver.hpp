/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MedSLTPatPlainWindowObserver.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <string>

using namespace std;

#ifndef MEDSLTPATPLAINWINDOWOBSERVER_HPP_
#define MEDSLTPATPLAINWINDOWOBSERVER_HPP_

class MedSLTPatPlainWindowObserver
{
public:
	
	virtual ~MedSLTPatPlainWindowObserver(){};
	
	virtual void ConnectToRemote()=0;
	virtual void DisconnectFromRemote()=0;
	virtual void QuitApp()=0;
	virtual void SendResultToServer(string& result)=0;
};

#endif /*MEDSLTPATPLAINWINDOWOBSERVER_HPP_*/
