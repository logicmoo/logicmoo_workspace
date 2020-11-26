/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	SocketClass.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <iostream>
#include "SocketClass.hpp"
#include "NetPool.hpp"
//#include "message.h"
#include <SDL_net.h>
//#include <SDL/SDL_gfxPrimitives.h>
#include "ace/Log_Msg.h"

SocketClass::SocketClass(const string& hostname, Uint16 port)
{
	ACE_TRACE(ACE_TEXT("[SocketClass::SocketClass()]"));
	
	m_hostname = hostname;
	m_port = port;
	m_pid = 1;
	
	m_ipaddr = new IPaddress();
	m_set = SDLNet_AllocSocketSet(1);
	
	if (GetHostname() == "") 
	{
		if (GetPort() != 0) 
		{
			if (SDLNet_ResolveHost(m_ipaddr	, 0, GetPort())) 
			{
				ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [SocketClass::SocketClass()] "
					"[UnknownHostException]")));				
//				exit(1);
			}
		}
	} 
	else
	{
		if (SDLNet_ResolveHost(m_ipaddr, (char*)GetHostname().c_str(), GetPort()))		
		{
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [SocketClass::SocketClass()] "
					"[UnknownHostException]")));			
//			exit(1);
		}
	}
	
	return;
}

SocketClass::~SocketClass() 
{
	ACE_TRACE(ACE_TEXT("[SocketClass::~SocketClass()]"));
	
	if (m_set != NULL)
	{		
		SDLNet_FreeSocketSet(m_set);
	}
	
	//cleanupNetwork();
	//m_set == NULL;
	
	return;
}

// Return the name of the remote host
string SocketClass::GetHostname() const
{
	ACE_TRACE(ACE_TEXT("[SocketClass::GetHostname()]"));
		
	return m_hostname;
}

// Return the remote port number
Uint16 SocketClass::GetPort() const
{
	ACE_TRACE(ACE_TEXT("[SocketClass::GetPort()]"));
		
	return m_port;
}

// Send a packet
Uint16 SocketClass::Send(const Uint8* buffer, Uint16 len) 
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[SocketClass::Send()]"));
#endif
	
	NetPool::GetInstance()->stats["len_out"] += len;

	return 0;
}

// Receive a packet
Uint16 SocketClass::Receive(Uint8* buffer, Uint16 len) 
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[SocketClass::Receive()]"));
#endif
	
	NetPool::GetInstance()->stats["len_in"] += len;
	
	return 0;
}

// Check if data is available
bool SocketClass::HasData() 
{
#if _DEBUG_3
	ACE_TRACE(ACE_TEXT("[SocketClass::HasData()]"));
#endif

	int numready = SDLNet_CheckSockets(m_set, 1);
	
	if (numready == -1) 
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [SocketClass::HasData()] "
					"[SocketException]")));
//		exit(1);
	}
	
	return numready ? true : false;
}
/*
// Send a message through the socket
Uint16 SocketClass::SendMessage (CMessage * message)
{
	// Setup of the packet header
	struct NetHeader header;

	// FIXME check if the source and destination are < 2^16
	
#if SDL_BYTEORDER==SDL_BIG_ENDIAN
	header.Length		= SDL_Swap16(message->GetBufferLen());
	header.PacketId		= SDL_Swap16(_pid++);
	header.Destination	= SDL_Swap16(message->GetDestination());
	header.Source		= SDL_Swap16(message->GetSource());
#else
	header.Length		= message->GetBufferLen();
	header.PacketId		= _pid++;
	header.Destination	= message->GetDestination();
	header.Source		= message->GetSource();
#endif

	Uint8 * buffer = new Uint8 [sizeof(header) + message->GetBufferLen()];

	memcpy(buffer, &header, sizeof (header));
	memcpy(buffer + sizeof(header), message->GetBuffer(),
			message->GetBufferLen());

	// Send the message
	int result = Send(buffer, sizeof(header) + message->GetBufferLen());
	
	if (result <= 0)
	{
		EXCEPTION (SocketException);
	}

	delete [] buffer;

	return result;
}

// Receive a message through the socket
Uint16 CSocket::ReceiveMessage (CMessage ** message)
{
	Uint8 * buffer = 0;
	int result = 0;
	struct NetHeader header;

	// Read the header
	result = Receive ((Uint8 *) & header, sizeof (header));
	
	if (result <= 0)
	{
		EXCEPTION( SocketException );
	}

#if SDL_BYTEORDER==SDL_BIG_ENDIAN
	header.Length		= SDL_Swap16(header.Length);
	header.PacketId		= SDL_Swap16(header.PacketId);
	header.Destination	= SDL_Swap16(header.Destination);
	header.Source		= SDL_Swap16(header.Source);
#endif

	// Allocate the reception buffer for the payload, if necessary
	if (header.Length > 0)
	{
		buffer = new Uint8 [header.Length];

		// Read the payload
		result = Receive (buffer, header.Length);
	
		if (result <= 0)
		{
			EXCEPTION( SocketException );
		}
	}

	// FIXME
	cout << "socket: source = " << header.Source
			<< " dest = " << header.Destination << endl;

#if _DEBUG >= 3
	clog << "[SocketClass::ReceiveMessage] Packet "
		<< (int) header.PacketId
		<< " from "
		<< (int) header.Source
		<< " to "
		<< (int) header.Destination
		<< ", payload length = "
		<< (int) header.Length
		<< endl;
#endif

	// Create the message
	* message = new CMessage 
		(	header.Destination,
			header.Source,
			buffer,
			header.Length);

	return result;
}*/
