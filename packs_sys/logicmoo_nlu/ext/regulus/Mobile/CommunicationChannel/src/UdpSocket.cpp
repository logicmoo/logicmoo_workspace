/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	UdpSocket.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

//#include <SDL/SDL_gfxPrimitives.h>
#include <iostream>
#include <sstream>
#include <cassert>
#include "ace/Log_Msg.h"

#ifdef _ARCH_WIN32
	#include <windows.h>
#else
	#include <sys/types.h>
	#include <netinet/in.h>
	#include <inttypes.h>
#endif

#include "UdpSocket.hpp"
#include "NetPool.hpp"
//#include "message.h"

UdpSocket::UdpSocket(const string& hostname, Uint16 port) 
	: SocketClass(hostname, port) 
{
	ACE_TRACE(ACE_TEXT("[UdpSocket::UdpSocket()]"));
	
#if _DEBUG	
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [UdpSocket::New()] "
					"[To %s:%d]\n"), GetHostname().c_str(), GetPort()));
#endif

	m_sock = NULL;
	
	return;
}

// Construct from a socket
UdpSocket::UdpSocket(UDPsocket socket) : SocketClass("", 0) 
{
	ACE_TRACE(ACE_TEXT("[UdpSocket::UdpSocket()]"));
	
	m_sock = socket;
	SDLNet_UDP_AddSocket(m_set, m_sock);
	m_ipaddr = SDLNet_UDP_GetPeerAddress(m_sock, -1);

	if (m_ipaddr == NULL)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [UdpSocket::UdpSocket()] "
				"[Error: %s]\n"), SDLNet_GetError()));		   
	}
	else 
	{		
		SetHostname(SDLNet_ResolveIP(m_ipaddr));
		SetPort(SDL_Swap16(m_ipaddr->port));
	}
	
#if _DEBUG
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [UdpSocket::New()] "
				"[From: %s:%d]\n"),  GetHostname().c_str(), GetPort()));				
#endif

	return;
}

UdpSocket::~UdpSocket() 
{
	ACE_TRACE(ACE_TEXT("[UdpSocket::~UdpSocket()]"));
	
	Disconnect();
	// FIXME: if (_ipaddr) delete _ipaddr
	//        plante:erreur de segmentation ?!
	//        p-e le constructeur par recopie avec 
	//        SDLNet_UDP_GetPeerAddress() ?
	
	return;
}

// Send a packet
Uint16 UdpSocket::Send(const Uint8* buffer, Uint16 len) 
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[UdpSocket::Send()]"));	
#endif

	Uint16 result = 0; 
	
	if (m_sock) 
	{
		UDPpacket pack;
		pack.channel = 1;
		pack.data = const_cast<Uint8 *>(buffer);
		pack.len = len;
		pack.maxlen = len;
		pack.address.port = m_ipaddr->port;
		pack.address.host = m_ipaddr->host;	

#if _DEBUG_2	
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [UdpSocket::Send()] "
					"[Address: %d Port:%d]\n"),  ntohl(pack.address.host), 
					ntohs(pack.address.port)));
#endif
		
		if (SDLNet_UDP_Send(m_sock, pack.channel, &pack)) 
		{
			result = len;
		} 
		else 
		{
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [UdpSocket::Send()] "
						"[UdpSocketException]\n")));
//			exit(1);
			return 0;
		}
	}

	NetPool::GetInstance()->stats["udp_n_out"] ++;
	NetPool::GetInstance()->stats["udp_len_out"] += len;

	return result;
}

// Receive a packet
Uint16 UdpSocket::Receive(Uint8* buffer, Uint16 len) 
{	
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[UdpSocket::Receive()]"));	
#endif
	
	Uint16 read = 0;
	
	if (m_sock) 
	{
#if _DEBUG_2
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [UdpSocket::Receive()] "
					"[Want receive: %d packet(s)]\n"), (int)len));		
#endif
		
		while (read != len) 
		{
			UDPpacket* pack;
			pack = SDLNet_AllocPacket(len);
			
			if (!pack)
			{
				ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [UdpSocket::Receive()] "
					"%s\n"), SDLNet_GetError()));				
//				exit(1);
				return 0;
			}			
			else if (SDLNet_UDP_Recv(m_sock, pack)) 
			{
//				cout << "---[DEBUG] [-----1]" 
//						<< (int)buffer[7] << endl;
						
				memcpy(&buffer[read], pack->data, pack->len);
				read += pack->len;
				
				//ostringstream ostream;
				//ostream << pack->data;
//				cout << "---[DEBUG] [-----2]" 
//						<< (int)buffer[7] << endl;
#if _DEBUG_2
				ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [UdpSocket::Receive()] "
					"[Receive %d/%d/%d-]\n"), (int)pack->len, (int)read, len,
					(int)pack->maxlen));
#endif
			}
			// TODO: Recheck if this else is really needed
			else 
			{
				//cout << "wait" << endl;
				SDL_Delay(100);
			}

			if (read > len) 
			{
				ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [UdpSocket::Receive()] "
						"[UdpSocketException]\n")));
//				exit(1);
				return 0;
			}
			
			SDLNet_FreePacket(pack);
		}
#if _DEBUG_2
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [UdpSocket::Receive()] "
					"[Receive %d packet(s)]\n"), (int)read));
#endif
	}
	
	NetPool::GetInstance()->stats["udp_n_in"] ++;
	NetPool::GetInstance()->stats["udp_len_in"] += len;

	return read;
}

// Disconnection of the socket
void UdpSocket::Disconnect() 
{
	ACE_TRACE(ACE_TEXT("[UdpSocket::Disconnect()]"));
		
	if (m_sock) 
	{
		while (SDLNet_UDP_DelSocket(m_set, m_sock) >= 0)
		{ 
			SDLNet_UDP_Close(m_sock);
		}
		
#if _DEBUG
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [UdpSocket::Disconnect()] "
					"[From %s:%d]\n"), GetHostname().c_str(), GetPort()));
#else
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [UdpSocket::Disconnect()] "
					"[Socket disconnect from %s:%d]\n"),
					GetHostname().c_str(), GetPort()));
#endif
	}

	m_sock = NULL;
	
	return;
}

// Bind a connection
void UdpSocket::Bind(Uint8 channel) 
{
	ACE_TRACE(ACE_TEXT("[UdpSocket::Bind()]"));
	
	if (SDLNet_UDP_Bind(m_sock, channel, m_ipaddr) == -1)
	{ 
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [UdpSocket::Bind()] "
				"[UdpOpenException]\n")));
//		exit(1);
		return;
	}

	return;
}

// Open a port
void UdpSocket::Open(Uint16 port) 
{
	ACE_TRACE(ACE_TEXT("[UdpSocket::Open()]"));
		
	assert(!m_sock);
	m_sock = SDLNet_UDP_Open(port);
	
	if (!m_sock)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [UdpSocket::Open()] "
				"[UdpOpenException]\n")));
//		exit(1);
		return;
	}
	
	SDLNet_UDP_AddSocket(m_set, m_sock);
	
	return;
}

// Verify that there are data
bool UdpSocket::HasData() 
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[UdpSocket::HasData()]"));	
#endif

	return (SocketClass::HasData() && SDLNet_SocketReady(m_sock)) ? true : false;
}

// Return whether the socket is on or not
bool UdpSocket::IsOnline() 
{
	ACE_TRACE(ACE_TEXT("[UdpSocket::IsOnline()]"));
	
	return m_sock ? true : false;
}
