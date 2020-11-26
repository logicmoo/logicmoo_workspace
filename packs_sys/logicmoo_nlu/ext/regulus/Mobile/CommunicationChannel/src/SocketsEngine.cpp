/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	SocketsEngine.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <iostream>
#include "SocketsEngine.hpp"
#include "TcpSocket.hpp"
#include "UdpSocket.hpp"
#include "ace/Log_Msg.h"

SocketsEngine::SocketsEngine(SocketsEngineObserver& observer) : m_observer(observer)
{
	ACE_TRACE(ACE_TEXT("[SocketsEngine::SocketsEngine()]"));
	
	m_socket_read = NULL;
	m_socket_write = NULL;
	m_socket = NULL;
	m_socket_server = NULL;
	m_buffer_size = 1;
	m_local_port = 0;
	
	return;
}

SocketsEngine::~SocketsEngine()
{
	ACE_TRACE(ACE_TEXT("[SocketsEngine::~SocketsEngine()]"));
		
	if (m_socket_read != NULL)
	{
		delete m_socket_read;
		m_socket_read = NULL;
	}

	if (m_socket_write != NULL)
	{
		delete m_socket_write;
		m_socket_write = NULL;
	}
	
	if (m_socket_server != NULL)
	{		
		delete m_socket_server;
		m_socket_server = NULL;
	}
	
	if (m_socket != NULL)
	{
		delete m_socket;
		m_socket = NULL;
	}
	
	return;
}

void SocketsEngine::SetServerName(const string name)
{
	ACE_TRACE(ACE_TEXT("[SocketsEngine::SetServerName()]"));
		
	m_server_name = name;
	
	return;
}
	
string SocketsEngine::GetServerName() const
{
	ACE_TRACE(ACE_TEXT("[SocketsEngine::GetServerName()]"));
		
	return m_server_name;
}

void SocketsEngine::SetServerPort(Uint32 port)
{
	ACE_TRACE(ACE_TEXT("[SocketsEngine::SetServerPort()]"));
		
	m_server_port = port;
	
	return;
}

Uint32 SocketsEngine::GetServerPort() const
{
	ACE_TRACE(ACE_TEXT("[SocketsEngine::GetServerPort()]"));
		
	return m_server_port;
}

void SocketsEngine::SetLocalPort(Uint32 port)
{
	ACE_TRACE(ACE_TEXT("[SocketsEngine::SetLocalPort()]"));
	
	m_local_port = port;
	
	return;
}

Uint32	SocketsEngine::GetLocalPort(void) const
{
	ACE_TRACE(ACE_TEXT("[SocketsEngine::GetLocalPort()]"));
	
	return m_local_port;
}

void SocketsEngine::SetBufferSize(Uint16 length)
{
	ACE_TRACE(ACE_TEXT("[SocketsEngine::GetBufferSize()]"));
	
	m_buffer_size = length;
	
	return;
}

Uint16 SocketsEngine::GetBufferSize() const
{
	ACE_TRACE(ACE_TEXT("[SocketsEngine::GetBufferSize()]"));
	
	return m_buffer_size;
}

void SocketsEngine::ConnectServer(string type)
{
	ACE_TRACE(ACE_TEXT("[SocketsEngine::ConnectServer()]"));
	
	if (m_socket != NULL)
	{
		m_socket->Disconnect();
	}
	
	if (type == "TCP_SERVER")
	{
		m_socket = new TcpSocket(m_server_name, m_server_port);
		static_cast<TcpSocket*>(m_socket)->Connect();
		m_socket_server = new SocketServer(*static_cast<TcpSocket*>(m_socket), *this);
		m_socket_server->StartServer();
	}
	else if (type == "TCP_CLIENT")
	{
		m_socket = new TcpSocket(m_server_name, m_server_port);
		static_cast<TcpSocket*>(m_socket)->Connect();
		Initialize();
	}
	else if (type == "UDP")
	{
		m_socket = new UdpSocket(m_server_name, m_server_port);
		static_cast<UdpSocket*>(m_socket)->Open(m_local_port);
		// Bind server address to channel 1
		static_cast<UdpSocket*>(m_socket)->Bind(1);
		Initialize();
	}
	else
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [SocketsEngine::ConnectServer()] "
					"[Unknown socket type]\n")));		
//		exit(1);
	}
	
	return;
}

void SocketsEngine::DisconnectServer()
{
	ACE_TRACE(ACE_TEXT("[SocketsEngine::DisconnectServer()]"));
			
	if (m_socket_read != NULL)
	{
		m_socket_read->StopListening();
		//delete m_socket_read;
		//m_socket_read = NULL;
	}

	if (m_socket_server != NULL)
	{
		m_socket_server->StopServer();
	}
	
	if (m_socket != NULL)
	{
		m_socket->Disconnect();
		//delete m_socket;
		//m_socket = NULL;
	}
	
	return;
}

void SocketsEngine::RunAsServer()
{
	ACE_TRACE(ACE_TEXT("[SocketsEngine::RunAsServer()]"));
	
	return;
}

void SocketsEngine::Initialize()
{
	ACE_TRACE(ACE_TEXT("[SocketsEngine::Initialize()]"));
	
	m_socket_read = new SocketRead(*m_socket, *this, m_buffer_size);
//	m_socket_read->SetDataLength(m_data_length);
	m_socket_read->StartListening();
	
	m_socket_write = new SocketWrite(*m_socket, *this);
	
	return;
}

Uint16 SocketsEngine::WriteData(const Uint8* buffer, Uint16 len)
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[SocketsEngine::WriteData()]"));
#endif
	
	if (m_socket_write != NULL)
	{
		return m_socket_write->IssueWrite(buffer, len);
	}
	else
	{
		return 0;
	}
}

void SocketsEngine::ReadData()
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[SocketsEngine::ReadData()]"));
#endif
	
	if (m_socket_read != NULL)
	{
		return m_socket_read->Run();
	}
	else
	{
		return;
	}
}

void SocketsEngine::PacketReceived(ostringstream& buffer)
{
#if _DEBUG_1
	ACE_TRACE("[SocketsEngine::PacketReceived()]");
#endif
	
	m_observer.MessageReceived(buffer);
	
	return;
}

void SocketsEngine::ClientConnected(TcpSocket* socket)
{
	ACE_TRACE("[SocketsEngine::ClientConnected()]");

	m_socket_read = new SocketRead(*socket, *this, m_buffer_size);	
	m_socket_read->StartListening();
		
	m_socket_write = new SocketWrite(*socket, *this);
	
	return;
}
