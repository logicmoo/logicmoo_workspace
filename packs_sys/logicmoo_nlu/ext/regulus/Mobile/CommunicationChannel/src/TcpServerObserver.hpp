/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	TcpServerObserver.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef TCPSERVEROBSERVER_HPP_
#define TCPSERVEROBSERVER_HPP_

#include <string>

using namespace std;

class TcpServerObserver
{
public:
	
	virtual ~TcpServerObserver(){};
	
	virtual void ClientMessageReceived(string& buffer)=0;
};

#endif /*TCPSERVEROBSERVER_HPP_*/
