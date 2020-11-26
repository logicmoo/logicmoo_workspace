/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RtpPacketTypes.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef RTPPACKETTYPES_HPP_
#define RTPPACKETTYPES_HPP_

#include "RtpTypes.hpp"

// RTP packet header layout 
struct RTPHeader
{
	// Casts the supplied non-null byte array into
	// an RTP header structure.
	static RTPHeader& Cast(const Uint8* data);

	// Accessor to the ith contributing source
	// identifier.
	// Contributing source identifiers are stored
	// immediately after the end of the RTP header.
	RTPSrc&	GetCSrc(Uint32 index);

#ifdef _IS_LITTLE_ENDIAN

	Uint32	count:4;		// csrc count
	Uint32	extension:1;	// header extension flag
	Uint32	padding:1;	// padding flag - for encryption
	Uint32	version:2;	// protocol version

	Uint32	type:7;		// payload type
	Uint32	marker:1;	// marker bit - for profile

#else // if _IS_BIG_ENDIAN

	Uint32	version:2;	// protocol version
	Uint32	padding:1;	// padding flag - for encryption
	Uint32	extension:1;	// header extension flag
	Uint32	count:4;	// csrc count

	Uint32	marker:1;	// marker bit - for profile
	Uint32	type:7;		// payload type

#endif

	Uint32	sequence:16;	// sequence number of this packet 
	RTPTime timestamp;	// timestamp of this packet
	RTPSrc 	ssrc;		// source of packet

};

#endif // RTPPACKETTYPES_HPP_
