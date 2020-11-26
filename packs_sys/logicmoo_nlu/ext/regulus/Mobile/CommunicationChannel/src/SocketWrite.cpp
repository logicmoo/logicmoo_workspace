/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	SocketWrite.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <iostream>
#include "SocketWrite.hpp"
#include "ace/Log_Msg.h"

using namespace std;

SocketWrite::SocketWrite(SocketClass& socket, SocketObserver& observer) : m_socket(socket), m_observer(observer)
{
	ACE_TRACE(ACE_TEXT("[SocketWrite::SocketWrite()]"));
		
	return;
}

SocketWrite::~SocketWrite()
{
	ACE_TRACE(ACE_TEXT("[SocketWrite::~SocketWrite()]"));

	return;
}

Uint16 SocketWrite::IssueWrite(const Uint8* buffer, Uint16 len)
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[SocketWrite::IssueWrite()]"));
#endif
		
	return m_socket.Send(buffer, len);
}
