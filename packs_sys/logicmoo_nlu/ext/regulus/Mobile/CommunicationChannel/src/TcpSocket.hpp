/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	TcpSocket.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
// Send and receipt of tcp data
#ifndef TCPSOCKET_HPP_
#define TCPSOCKET_HPP_

#include <string>
#include "SocketClass.hpp"

using namespace std;

class TcpSocket : public SocketClass
{
public: 

	TcpSocket(const string& hostname, Uint16 port); 
	~TcpSocket();
		
	// Construct from one socket 
	TcpSocket(TCPsocket socket);

	// Bind to a port	 
	virtual void Bind();

	// Connection	 
	virtual void Connect();

	// Disconnection	 
	virtual void Disconnect();

	// Accept a connection (blocking??)	
	virtual TcpSocket* Accept();
	
	// Send a message through the socket	 
	virtual Uint16 Send(const Uint8* buffer, Uint16 len);

	// Receive a message through the socket
	virtual Uint16 Receive(Uint8* buffer, Uint16 len);

	// Verify that data is waiting
	virtual bool HasData();

	// Check if the socket is online
	bool IsOnline();

private:
	
	// Avoid accidental copy or assignment
	TcpSocket(const TcpSocket&);
	TcpSocket& operator = (const TcpSocket&);
	
private:

	// The tcp socket
	TCPsocket m_sock;
};


#endif /*TCPSOCKET_HPP_*/
