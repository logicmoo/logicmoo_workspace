/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	SocketClass.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
// Superclass for TCP & UDP sockets.
#ifndef SOCKETCLASS_HPP_
#define SOCKETCLASS_HPP_

#include <map>
#include <string>
#include <SDL_net.h>

using namespace std;

class SocketClass
{
public: 

	SocketClass(const string& hostname, Uint16 port); 
	
	virtual ~SocketClass();
	
	// Connect the socket
//	virtual void Connect() = 0;
	
	// Disconnect the socket
	virtual void Disconnect() = 0;
	
	// Send a packet through the socket	 
	virtual Uint16 Send(const Uint8* buffer, Uint16 len) = 0;

	// Receive a packet through the socket	
	virtual Uint16 Receive(Uint8* buffer, Uint16 len) = 0;

	// Send a message through the socket
//	Uint16 SendMessage(CMessage* message);

	// Receive a message through the socket
//	Uint16 ReceiveMessage(CMessage** message);
		
	// Return true if data is available
	virtual bool HasData();

	// Return the status of the socket (online/offline)
	virtual bool IsOnline() = 0;

	// Return the name of the remote host
	string GetHostname() const;

	// Return the remote port number	 
	Uint16 GetPort() const;

protected:
	
	// Set remote hostname
	inline void SetHostname(const string& hostname) 
	{
		m_hostname = hostname;
	};

	// Set remote port	 
	inline void SetPort(Uint16 port) 
	{
		m_port = port;
	};
	
	// IP address (SDL)
	IPaddress* m_ipaddr;
 
	// SocketSet
	SDLNet_SocketSet m_set;

	// Header a of network message
	struct NetHeader
	{
		// length of the payload
		Uint16	Length;

		// packet ID
		Uint16	PacketId;

		// destination ID
		Uint16 Destination;

		// source ID
		Uint16 Source;
	};

private:
	
	// Avoid accidental copy or assignment
	SocketClass(const SocketClass&);
	SocketClass& operator = (const SocketClass&);
	
private: 
	
	// Remote host name
	string m_hostname;

	// Remote port	 
	Uint16 m_port;
	
	// Packet ID
	 Uint16 m_pid;
};

#endif /*SOCKETCLASS_HPP_*/
