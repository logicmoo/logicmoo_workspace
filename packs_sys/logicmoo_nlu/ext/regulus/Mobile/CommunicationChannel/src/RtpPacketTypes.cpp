/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RtpPacketTypes.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include "RtpPacketTypes.hpp"

RTPHeader& RTPHeader::Cast(const Uint8* data)
{
	return *(RTPHeader*)(data + 0);
}

RTPSrc&	RTPHeader::GetCSrc(Uint32 index)
{
	return ((RTPSrc*)(this + 1))[index];
}
