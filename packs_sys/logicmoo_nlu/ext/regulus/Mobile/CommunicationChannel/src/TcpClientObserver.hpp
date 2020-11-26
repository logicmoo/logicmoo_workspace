/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	TcpClientObserver.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#ifndef TCPCLIENTOBSERVER_HPP_
#define TCPCLIENTOBSERVER_HPP_

#include <string>

using namespace std;

class TcpClientObserver
{
public:
	
	virtual ~TcpClientObserver(){};
	
	virtual void ServerMessageReceived(string& buffer)=0;

};

#endif /*TCPCLIENTOBSERVER_HPP_*/
