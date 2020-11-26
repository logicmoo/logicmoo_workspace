/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	AudioStreamEngineObserver.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#ifndef AUDIOSTREAMENGINEOBSERVER_HPP_
#define AUDIOSTREAMENGINEOBSERVER_HPP_

#include <sstream>
#include "SDL_net.h"

using namespace std;

class AudioStreamEngineObserver
{
public:
	
	virtual ~AudioStreamEngineObserver(){};
	
	virtual void AudioDataReceived(const Uint8* buffer)=0;
	//virtual void AudioDataReceived(ostringstream& buffer)=0;
};

#endif /*AUDIOSTREAMENGINEOBSERVER_HPP_*/
