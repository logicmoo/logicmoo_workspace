/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	NetPool.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <SDL_net.h>
#include <sstream>
#include <iostream>
#include "NetPool.hpp"
#include "ace/Log_Msg.h"

using namespace std;

// Initialize the static members
NetPool* NetPool::_instance = NULL;

NetPool::NetPool() 
{
	ACE_TRACE(ACE_TEXT("[NetPool::NetPool()]"));

	if(SDLNet_Init() == -1)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [NetPool::NetPool()] "
				"[Couldn't initialize net: %s]\n"), SDLNet_GetError()));
//		exit(1);
		return;
	}
	
	return;
}

NetPool::~NetPool() 
{
	ACE_TRACE(ACE_TEXT("[NetPool::~NetPool()]"));
		
	return;
}

// Return the instance
NetPool* NetPool::GetInstance() 
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[NetPool::GetInstance()]"));
#endif
	
	if (!_instance)
	{ 
		_instance = new NetPool();
	}
	
	return _instance;
}
