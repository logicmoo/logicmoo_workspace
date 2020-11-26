/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RtpTransceiverObserver.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef RTPTRANSCEIVEROBSERVER_HPP_
#define RTPTRANSCEIVEROBSERVER_HPP_

#include "SDL_net.h"

using namespace std;

class RtpTransceiverObserver
{
public:
	
	virtual ~RtpTransceiverObserver(){};
	
	virtual void TTSDataReceived(const Uint8* buffer)=0;
};

#endif /*RTPTRANSCEIVEROBSERVER_HPP_*/
