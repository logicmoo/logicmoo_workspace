/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	SocketRead.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include "SocketRead.hpp"
#include "UtilsAllocator.hpp"

//int counter3 = 0;

SocketRead::SocketRead(SocketClass& socket, 
		SocketObserver& observer, int buffer_size) 
	: m_socket(socket), m_observer(observer)
{
	ACE_TRACE(ACE_TEXT("[SocketRead::SocketRead()]"));

	m_buffer_size = buffer_size;
	m_buffer = NULL;
	m_buffer = static_cast<Uint8*>(Memory::Allocate((m_buffer_size+1)*sizeof(Uint8)));
	m_buffer[buffer_size] =  '\0';
	
	return;
}

SocketRead::~SocketRead()
{
	ACE_TRACE(ACE_TEXT("[SocketRead::~SocketRead()]"));
		
	if (m_buffer != NULL)
	{
		Memory::Deallocate(m_buffer);		
	}
	
	return;
}

void SocketRead::Run()
{
#if _DEBUG_1
	ACE_TRACE(ACE_TEXT("[SocketRead::Run()]"));
#endif
	
	IssueRead();
	m_observer.PacketReceived(m_ostream);
	
	m_results = m_proxy.CheckSocket(m_socket);
	m_results.attach(this);
	
	return;
}

void SocketRead::StartListening()
{
	ACE_TRACE(ACE_TEXT("[SocketRead::StartListening()]"));
	
	m_results = m_proxy.CheckSocket(m_socket);
	m_results.attach(this);	
	
	return;
}

void SocketRead::StopListening()
{
	ACE_TRACE(ACE_TEXT("[SocketRead::StopListening()]"));
		
	m_proxy.Exit();

	return;
}


void SocketRead::update(const ACE_Future<int>& future)
{
#if _DEBUG_3
	ACE_TRACE(ACE_TEXT("[SocketRead::update()]"));
#endif
	
	int result = 0;
	
	((ACE_Future<int>)future).get(result);

#if _DEBUG_3
	ACE_DEBUG((LM_INFO,
		ACE_TEXT("[SocketRead::update()] [(%t) New Status %d]\n"), result));
#endif
	
	if (result > 0)
	{
		Run();
	}	
	else if (result == 0)
	{
		//ACE_OS::sleep(5);
		m_results = m_proxy.CheckSocket(m_socket);
		m_results.attach(this);		
	}
	else
	{
		m_proxy.Exit();
	}
	
	return;
}
	
void SocketRead::IssueRead()
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[SocketRead::IssueRead()]"));
#endif
	
	m_ostream.str("");
	
	while (m_socket.HasData())
	{
		if (m_socket.Receive(m_buffer, m_buffer_size) == 0)
		{
			m_proxy.Exit();
			break;
		}
		else
		{
			for (int i = 0; i < m_buffer_size; ++i)
			{
				m_ostream << m_buffer[i];
			}
		}
	}
/*std::string tmp;
tmp = m_ostream.str();
counter3 += tmp.size();
cout << "---[DEBUG] [---SocketRead::IssueRead()]" << tmp.size() << endl;*/
	return;
}
