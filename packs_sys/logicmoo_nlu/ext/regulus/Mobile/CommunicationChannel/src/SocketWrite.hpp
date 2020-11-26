/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	SocketWrite.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef SOCKETWRITE_HPP_
#define SOCKETWRITE_HPP_

#include "SocketClass.hpp"
#include "SocketObserver.hpp"

class SocketWrite
{
public:

	SocketWrite(SocketClass& socket, SocketObserver& observer);
	~SocketWrite();

	// Writes to socket
	Uint16 IssueWrite(const Uint8* buffer, Uint16 len);
	
private:

	SocketClass&	m_socket;
	SocketObserver&	m_observer;
};

#endif /*SOCKETWRITE_HPP_*/
