/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	TcpSocket.cpp
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
#include "TcpSocket.hpp"
#include "NetPool.hpp"
#include "ace/Log_Msg.h"

TcpSocket::TcpSocket(const string& hostname, Uint16 port) : SocketClass(hostname, port) 
{
	ACE_TRACE(ACE_TEXT("[TcpSocket::TcpSocket()]"));
	
#if _DEBUG
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [TcpSocket::New()] "
					"[To %s:%d]\n"), GetHostname().c_str(), GetPort()));
#endif

	m_sock = NULL;
	
	return;
}

// Construct from one socket
TcpSocket::TcpSocket(TCPsocket socket) : SocketClass("", 0) 
{
	ACE_TRACE(ACE_TEXT("[TcpSocket::TcpSocket()]"));	

	m_sock = socket;
	SDLNet_TCP_AddSocket(m_set, m_sock);
	m_ipaddr = SDLNet_TCP_GetPeerAddress(m_sock);
	
	const char* resolveip = SDLNet_ResolveIP(m_ipaddr);
	
	if (resolveip)
	{ 
		SetHostname(resolveip);
	}
	else
	{
		stringstream ss;
		ss << (int)((m_ipaddr->host & 0xFF000000) >> 24);		
		ss << ".";
		ss << (int)((m_ipaddr->host & 0x00FF0000) >> 16);
		ss << ".";
		ss << (int)((m_ipaddr->host & 0x0000FF00) >> 8);
		ss << ".";
		ss << (int)(m_ipaddr->host & 0x000000FF);
		SetHostname(ss.str());
	}

	SetPort(SDL_Swap16(m_ipaddr->port));
	
#if _DEBUG
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [TcpSocket::New()] "
					"[From %s:%d]\n"), GetHostname().c_str(), GetPort()));	
#endif

	return;
}

TcpSocket::~TcpSocket() 
{
	ACE_TRACE(ACE_TEXT("[TcpSocket::~TcpSocket()]"));
	
	Disconnect();
	// FIXME: if (_ipaddr) delete _ipaddr
	//        plante:erreur de segmentation ?!
	//        p-e le constructeur par recopie avec 
	//        SDLNet_TCP_GetPeerAddress() ?
	
	return;
}

// Send a packet
Uint16 TcpSocket::Send(const Uint8* buffer, Uint16 len) 
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[TcpSocket::Send()]"));	
#endif
	
	Uint16 result = 0; 
	
	if (m_sock) 
	{
		result = SDLNet_TCP_Send(m_sock, const_cast<Uint8 *>(buffer), len);
	
		if (result <= 0) 
		{
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [TcpSocket::Send()] "
					"[SocketException]\n")));		
//			exit(1);
			return 0;
		}
	}

	NetPool::GetInstance()->stats["tcp_n_out"] ++;
	NetPool::GetInstance()->stats["tcp_len_out"] += len;
	SocketClass::Send(buffer, len);

	return result;
}

// Receive a packet
Uint16 TcpSocket::Receive(Uint8* buffer, Uint16 len) 
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[TcpSocket::Receive()]"));
#endif
	
	Uint16 result = 0;
	Uint16 read = 0;
	
	while (read != len) 
	{
		result = SDLNet_TCP_Recv(m_sock, &buffer[read], len - read);
		
		if (result <= 0)
		{ 
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [TcpSocket::Receive()] "
					"[SocketException]\n")));
			
			//exit(1);
			return 0;
		}
		else
		{
			read += result;
		}
	}
	
	NetPool::GetInstance()->stats["tcp_n_in"] ++;
	NetPool::GetInstance()->stats["tcp_len_in"] += len;
	SocketClass::Receive(buffer, len);

	return read;
}

// Bind to a port
void TcpSocket::Bind() 
{
	ACE_TRACE(ACE_TEXT("[TcpSocket::Bind()]"));
	
#if _DEBUG
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [TcpSocket::Bind()] "
					"[Binding on %s:%d]\n"), GetHostname().c_str(), GetPort()));	
#else 

	if (GetHostname() == "")
	{ 
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [TcpSocket::Bind()] "
					"[Binding on port %d]\n"), GetPort()));		
	}
	else
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [TcpSocket::Bind()] "
					"[Binding on %s:%d]\n"), GetHostname().c_str(), GetPort()));		
	}
#endif
	
	m_sock = SDLNet_TCP_Open(m_ipaddr);
	
	if (!m_sock)
	{ 
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [TcpSocket::Bind()] "
					"[TcpBindException]\n")));		
//		exit(1);
		return;
	}
	
	SDLNet_TCP_AddSocket(m_set, m_sock);
	
	return;
}

// Connection
void TcpSocket::Connect() 
{
	ACE_TRACE(ACE_TEXT("[TcpSocket::Connect()]"));
	
#if _DEBUG
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [TcpSocket::Connect()] "
					"[Connecting on %s:%d]\n"), GetHostname().c_str(), GetPort()));
#else 

	if (GetHostname() == "")
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [TcpSocket::Connect()] "
					"[Connecting on port %d]\n"), GetPort()));		
	}
	else
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [TcpSocket::Connect()] "
					"[Connecting on %s:%d]\n"), GetHostname().c_str(), GetPort()));		
	}
#endif
	
	m_sock = SDLNet_TCP_Open(m_ipaddr);
	
	if (!m_sock)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [TcpSocket::Connect()] "
					"[TcpConnectException]\n")));		
	}
	
	SDLNet_TCP_AddSocket(m_set, m_sock);
	
	return;
}

// Disconnection of the socket
void TcpSocket::Disconnect() 
{
	ACE_TRACE(ACE_TEXT("[TcpSocket::Disconnect()]"));
	
	if (m_sock) 
	{
		while (SDLNet_TCP_DelSocket(m_set, m_sock) >= 0)
		{ 
			SDLNet_TCP_Close(m_sock);
		}
		
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [TcpSocket::Disconnect()] "
					"[Disconnecting on %s:%d]\n"), GetHostname().c_str(), GetPort()));		
	}
	
	m_sock = NULL;
	
	return;
}

// Accept a connection
TcpSocket* TcpSocket::Accept() 
{
#if _DEBUG_3
	ACE_TRACE(ACE_TEXT("[TcpSocket::Accept()]"));
#endif
	
	assert(m_sock);

	TCPsocket new_sock = SDLNet_TCP_Accept(m_sock);
	
	if (new_sock)
	{
		return new TcpSocket(new_sock);
	}
	else 
	{
		return NULL;
	}
}

// Verify that there are data
bool TcpSocket::HasData() 
{
#if _DEBUG_3
	ACE_TRACE(ACE_TEXT("[TcpSocket::HasData()]"));
#endif
	
	return (SocketClass::HasData() && SDLNet_SocketReady(m_sock)) ? true : false;
}

// Return whether the socket is on or not
bool TcpSocket::IsOnline() 
{
	ACE_TRACE(ACE_TEXT("[TcpSocket::IsOnline()]"));
	
	return m_sock ? true : false;
}
