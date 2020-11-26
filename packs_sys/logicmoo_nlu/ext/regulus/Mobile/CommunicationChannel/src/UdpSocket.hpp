/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	UdpSocket.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
// Send and receipt of udp data
#ifndef UDPSOCKET_HPP_
#define UDPSOCKET_HPP_

#include <string>
#include "SocketClass.hpp"

using namespace std;

class UdpSocket : public SocketClass
{
public: 

	UdpSocket(const string& hostname, Uint16 port); 

	// Construct from one socket 
	UdpSocket(UDPsocket socket);

	// Disconnection
	virtual void Disconnect();

	// Accept a connection
	virtual void Bind(Uint8 channel = 0);
	
	// Open a port
	virtual void Open(Uint16 port = 20300);
	
	// Send a message through the socket
	virtual Uint16 Send(const Uint8* buffer, Uint16 len);

	// Receive a message through the socket
	virtual Uint16 Receive(Uint8* buffer, Uint16 len);

	// Verify that data is waiting
	virtual bool HasData();

	// Check if the socket is online
	bool IsOnline();

protected:
	
	virtual ~UdpSocket(); 

private:

	// The udp socket
	UDPsocket m_sock;
	
};

#endif /*UDPSOCKET_HPP_*/
