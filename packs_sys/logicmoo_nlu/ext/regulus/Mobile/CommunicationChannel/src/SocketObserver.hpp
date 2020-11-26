/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	SocketObserver.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef SOCKETOBSERVER_HPP_
#define SOCKETOBSERVER_HPP_

#include <SDL_net.h>
#include "TcpSocket.hpp"

using namespace std;

class SocketObserver
{
public:
	
	virtual ~SocketObserver(){};
	
	// Packet received
	virtual void PacketReceived(ostringstream& buffer)=0;
	
	// New Client arrived
	virtual void ClientConnected(TcpSocket* socket)=0;
};

#endif /*SOCKETOBSERVER_HPP_*/
