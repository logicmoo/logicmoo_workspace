/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	SocketServer.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef SOCKETSERVER_HPP_
#define SOCKETSERVER_HPP_

#include <iostream>
#include <sstream>
#include "ace/config-lite.h"
#include "ace/OS_NS_unistd.h"
#include "ace/Activation_Queue.h"
#include "ace/Method_Request.h"
#include "ace/Task.h"
#include "ace/Future.h"
#include "ace/Auto_Ptr.h"
#include "SocketClass.hpp"
#include "SocketObserver.hpp"
#include "TcpSocket.hpp"

using namespace std;

class _SocketControllerAgent
{
// Proxy to the SocketController that is on the network.
public:
	
	_SocketControllerAgent()
	{
#if _DEBUG
		ACE_TRACE
			(ACE_TEXT("[_SocketControllerAgent::SocketControllerAgent]"));	
#endif
		
		return;
	}

	TcpSocket* CheckSocket(TcpSocket& socket)
	{
#if _DEBUG_2
		ACE_TRACE(ACE_TEXT("[_SocketControllerAgent::CheckSocket]"));

		ACE_DEBUG((LM_DEBUG,
			ACE_TEXT("Obtaining socket status in %t ")
			ACE_TEXT("thread of control\n")));
#endif
		
		//TcpSocket* client_socket;
		
		// Wait until data are avaible for reading		
		//while ((client_socket = socket.Accept()) == NULL) {}
		
		return socket.Accept();
	}
};

class _CheckSocketRequest : public ACE_Method_Request
{
public:
	
	_CheckSocketRequest(_SocketControllerAgent& controller, 
						ACE_Future<TcpSocket*>& returnVal, 
						TcpSocket& socket)
				: m_controller(controller), m_returnVal(returnVal),
					m_socket(socket)
	{
#if _DEBUG_2
		ACE_TRACE(ACE_TEXT("[_CheckSocketRequest::_CheckSocketRequest]"));
#endif
		
		return;
	}

	virtual int call(void)
	{
#if _DEBUG_2
		ACE_TRACE (ACE_TEXT("[_CheckSocketRequest::call]"));		
#endif

		// CheckSocket with the controller
		this->m_returnVal.set(this->m_controller.CheckSocket(m_socket));
		
		return 0;
	}

private:
	
	_SocketControllerAgent&	m_controller;
	ACE_Future<TcpSocket*>	m_returnVal;
	TcpSocket& 				m_socket;
};

class _ExitRequest : public ACE_Method_Request
{
public:
	
	virtual int call(void)
	{
#if _DEBUG_2
		ACE_TRACE (ACE_TEXT("[_ExitRequest::call]"));
#endif

		// Cause exit.
		return -1;
	}
};

class _Scheduler : public ACE_Task_Base
{
public:
 
	_Scheduler()
	{
#if _DEBUG_2
		ACE_TRACE(ACE_TEXT("[_Scheduler::_Scheduler]"));
#endif

		this->activate();
		
		return;
	}

	~_Scheduler()
	{
#if _DEBUG_2
		ACE_TRACE(ACE_TEXT("[_Scheduler::~_Scheduler]"));
#endif

			return;
		}
	
	virtual int svc(void)
	{
#if _DEBUG_2
		ACE_TRACE(ACE_TEXT("[_Scheduler::svc]"));
#endif
		
		while (1)
		{
			// Dequeue the next method object
			auto_ptr<ACE_Method_Request>
				request(this->m_activation_queue.dequeue());

			// Invoke the method request.
			if (request->call() == -1)
			{
				break;
			}
      	}

		return 0;
	}

	int Enqueue(ACE_Method_Request *request)
	{
#if _DEBUG_2
		ACE_TRACE(ACE_TEXT("[_Scheduler::Enqueue]"));		
#endif

		return this->m_activation_queue.enqueue(request);
	}

private:

	ACE_Activation_Queue m_activation_queue;
};

class _SocketControllerAgentProxy
{
// This acts as a Proxy to the controller impl object.
public:
	
	ACE_Future<TcpSocket*> CheckSocket(TcpSocket& socket)
	{
#if _DEBUG_2
	ACE_TRACE
			(ACE_TEXT("[_SocketControllerAgentProxy::CheckSocket]"));
#endif
		
		ACE_Future<TcpSocket*> result;

		// Create and enqueue a method request on the scheduler.
		this->m_scheduler.Enqueue
			(new _CheckSocketRequest(this->m_controller, result, socket));

		// Return Future to the client.
		return result;
	}

	void Exit(void)
	{
#if _DEBUG_2
		ACE_TRACE (ACE_TEXT("[_SocketControllerAgentProxy::Exit]"));
#endif
		
		this->m_scheduler.Enqueue(new _ExitRequest);
		
		return;
	}
	
private:
	
	_Scheduler 				m_scheduler;
	_SocketControllerAgent	m_controller;
};

class SocketServer : public ACE_Future_Observer<TcpSocket*>
{
public:

	SocketServer(TcpSocket& socket, 
				SocketObserver& observer);
	~SocketServer();
	
	// Handles request completion event
	void Run(TcpSocket* client_socket);
	// Start the server
	void StartServer();
	// Stop the server
	void StopServer();
	
	virtual void update(const ACE_Future<TcpSocket*>& future);
	
	private:

	TcpSocket&					m_server_socket;
	TcpSocket*					m_client_socket;
	SocketObserver&				m_observer;
	ostringstream				m_ostream;	
	_SocketControllerAgentProxy	m_proxy;
	ACE_Future<TcpSocket*> 		m_results;
};

#endif /*SOCKETSERVER_HPP_*/
