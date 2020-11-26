/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	SocketServer.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include "SocketServer.hpp"
#include "UtilsAllocator.hpp"

SocketServer::SocketServer(TcpSocket& socket, 
		SocketObserver& observer) 
	: m_server_socket(socket), m_observer(observer)
{
	ACE_TRACE(ACE_TEXT("[SocketServer::SocketServer()]"));

	m_client_socket = NULL;
	
	return;
}

SocketServer::~SocketServer()
{
	ACE_TRACE(ACE_TEXT("[SocketServer::~SocketServer()]"));

	if (m_client_socket != NULL)
	{
		delete m_client_socket;
	}	
	
	return;
}

void SocketServer::Run(TcpSocket* client_socket)
{
	ACE_TRACE(ACE_TEXT("[SocketServer::Run()]"));
	
	m_client_socket = client_socket;
	m_observer.ClientConnected(client_socket);
	
	//m_results = m_proxy.CheckSocket(m_server_socket);
	//m_results.attach(this);
	
	return;
}

void SocketServer::StartServer()
{
	ACE_TRACE(ACE_TEXT("[SocketServer::StartServer()]"));
	
	m_results = m_proxy.CheckSocket(m_server_socket);
	m_results.attach(this);
		
	return;
}

void SocketServer::StopServer()
{
	ACE_TRACE(ACE_TEXT("[SocketServer::StopServer()]"));
	
	m_proxy.Exit();

	return;
}

void SocketServer::update(const ACE_Future<TcpSocket*>& future)
{
#if _DEBUG_3
	ACE_TRACE(ACE_TEXT("[SocketServer::update()]"));
#endif
	
	TcpSocket* client_socket = NULL;
	
	((ACE_Future<TcpSocket*>)future).get(client_socket);
	
	if (client_socket != NULL)
	{
		Run(client_socket);
	}
	else if (client_socket == NULL)
	{
		m_results = m_proxy.CheckSocket(m_server_socket);
		m_results.attach(this);
	}	
	else
	{	
		m_proxy.Exit();
	}
	
	return;
}
