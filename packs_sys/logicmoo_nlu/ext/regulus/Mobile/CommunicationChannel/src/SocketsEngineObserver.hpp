/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	SocketsEngineObserver.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef SOCKETSENGINEOBSERVER_HPP_
#define SOCKETSENGINEOBSERVER_HPP_

//#include <sstream>
#include <SDL_net.h>

using namespace std;

class SocketsEngineObserver
{
public:
	
	virtual ~SocketsEngineObserver(){};
	
	virtual void MessageReceived(ostringstream& buffer)=0;
};

#endif /*SOCKETSENGINEOBSERVER_HPP_*/
