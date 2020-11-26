/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	SocketRead.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef SOCKETREAD_HPP_
#define SOCKETREAD_HPP_

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

using namespace std;

class SocketControllerAgent
{
// Proxy to the SocketController that is on the network.
public:
	
	SocketControllerAgent()
	{
#if _DEBUG
		ACE_TRACE
			(ACE_TEXT("[SocketControllerAgent::SocketControllerAgent]"));	
#endif
		
		return;
	}

	~SocketControllerAgent()
	{
#if _DEBUG
		ACE_TRACE
			(ACE_TEXT("[SocketControllerAgent::~SocketControllerAgent]"));	
#endif
		
		return;
	}
	
	int CheckSocket(SocketClass& socket)
	{
#if _DEBUG_2
		ACE_TRACE(ACE_TEXT("[SocketControllerAgent::CheckSocket]"));

		ACE_DEBUG((LM_DEBUG,
			ACE_TEXT("Obtaining socket status in %t ")
			ACE_TEXT("thread of control\n")));
#endif
		
		// Wait until data are avaible for reading		
		//while (socket.HasData() < 1) {}
		
		return socket.HasData();		
	}
};

class CheckSocketRequest : public ACE_Method_Request
{
public:
	
	CheckSocketRequest(SocketControllerAgent& controller, 
						ACE_Future<int>& returnVal, 
						SocketClass& socket)
				: m_controller(controller), 
					m_returnVal(returnVal),
					m_socket(socket)
	{
#if _DEBUG_2
		ACE_TRACE(ACE_TEXT("[CheckSocketRequest::CheckSocketRequest]"));
#endif
		
		return;
	}

	~CheckSocketRequest()
	{
#if _DEBUG_2
		ACE_TRACE(ACE_TEXT("[CheckSocketRequest::~CheckSocketRequest]"));
#endif

		return;
	}
							
	virtual int call(void)
	{
#if _DEBUG_2
		ACE_TRACE (ACE_TEXT("[CheckSocketRequest::call]"));		
#endif

		// CheckSocket with the controller
		this->m_returnVal.set(this->m_controller.CheckSocket(m_socket));
				
		return 0;
	}

private:
	
	SocketControllerAgent&	m_controller;
	ACE_Future<int>			m_returnVal;
	SocketClass& 			m_socket;	
};

class ExitRequest : public ACE_Method_Request
{
public:
	
	ExitRequest()
	{
#if _DEBUG
		ACE_TRACE (ACE_TEXT("[ExitRequest::ExitRequest]"));
#endif		
		}
	
	~ExitRequest()
	{
#if _DEBUG
		ACE_TRACE (ACE_TEXT("[ExitRequest::~ExitRequest]"));
#endif		
	}
	
	virtual int call(void)
	{
#if _DEBUG_2
		ACE_TRACE (ACE_TEXT("[ExitRequest::call]"));
#endif

		// Cause exit.
		return -1;
	}
};

class Scheduler : public ACE_Task_Base
{
public:
 
	Scheduler()
	{
#if _DEBUG_2
		ACE_TRACE(ACE_TEXT("[Scheduler::Scheduler]"));
#endif

		this->activate();
		
		return;
	}

	~Scheduler()
	{
#if _DEBUG_2
		ACE_TRACE(ACE_TEXT("[Scheduler::~Scheduler]"));
#endif

			return;
		}
	
	virtual int svc(void)
	{
#if _DEBUG_2
		ACE_TRACE(ACE_TEXT("[Scheduler::svc]"));
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
		ACE_TRACE(ACE_TEXT("[Scheduler::Enqueue]"));		
#endif

		return this->m_activation_queue.enqueue(request);
	}

private:

	ACE_Activation_Queue m_activation_queue;
};

class SocketControllerAgentProxy
{
// This acts as a Proxy to the controller impl object.
public:
	
	SocketControllerAgentProxy()
	{
#if _DEBUG
	ACE_TRACE
			(ACE_TEXT("[SocketControllerAgentProxy::SocketControllerAgentProxy]"));
#endif
		
		return;
	}
	
	~SocketControllerAgentProxy()
	{
#if _DEBUG
	ACE_TRACE
			(ACE_TEXT("[SocketControllerAgentProxy::~SocketControllerAgentProxy]"));
#endif
		
		return;
	}
	
	ACE_Future<int> CheckSocket(SocketClass& socket)
	{
#if _DEBUG_2
	ACE_TRACE
			(ACE_TEXT("[SocketControllerAgentProxy::CheckSocket]"));
#endif
		
		ACE_Future<int> result;

		// Create and enqueue a method request on the scheduler.
		this->m_scheduler.Enqueue
			(new CheckSocketRequest(this->m_controller, result, socket));
		
		// Return Future to the client.
		return result;
	}

	void Exit(void)
	{
#if _DEBUG_2
		ACE_TRACE (ACE_TEXT("[SocketControllerAgentProxy::Exit]"));
#endif
		
		this->m_scheduler.Enqueue(new ExitRequest);
		
		return;
	}

private:
	
	Scheduler 				m_scheduler;
	SocketControllerAgent	m_controller;
};

class SocketRead : public ACE_Future_Observer<int>
{
public:

	SocketRead(SocketClass& socket, 
				SocketObserver& observer, int buffer_size);
	~SocketRead();
	
	// Handles request completion event
	void Run();
	// Reads from socket
	void IssueRead();
	// Start listening for incoming data
	void StartListening();
	// Stop listening for incoming data
	void StopListening();
	
	virtual void update(const ACE_Future<int>& future);
		
	private:

	SocketClass&				m_socket;
	SocketObserver&				m_observer;
	ostringstream				m_ostream;
	Uint16						m_buffer_size;
	SocketControllerAgentProxy	m_proxy;
	ACE_Future<int> 			m_results;
	Uint8*						m_buffer;
};

#endif /*SOCKETREAD_HPP_*/
