/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RtpTypes.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef RTPTYPES_HPP_
#define RTPTYPES_HPP_

#include "NtpTime.hpp"

// Public constants.

#define RTP_VERSION 		2		// RTP version (must be 2)
//#define RTP_MTU			8129	// Maximum UDP packet size
#define RTP_MTU				172		// Maximum UDP packet size
#define RTP_MAX_PAYLOAD		1012	// Maximum payload size
#define RTP_IN_BUFFER_SIZE	24576	// Incoming buffer size, including RTP header
#define RTP_OUT_BUFFER_SIZE	8192	// Outgoing buffer size, including RTP header

// RTP sequence number
typedef unsigned long RTPSeqNumber;

// RTP timestamp
typedef unsigned long RTPTime;

// RTP source identifier
typedef unsigned long RTPSrc;

// Supported types of RTP payloads
enum RTPPayloadType
{
	RTP_PAYLOAD_PCMU 		= 0,
	RTP_PAYLOAD_1016 		= 1,
	RTP_PAYLOAD_G726_32		= 2,
	RTP_PAYLOAD_GSM			= 3,
	RTP_PAYLOAD_G723		= 4,
	RTP_PAYLOAD_DVI4_8KHz 	= 5,
	RTP_PAYLOAD_DVI4_16KHz	= 6,
	RTP_PAYLOAD_LPC			= 7,
	RTP_PAYLOAD_PCMA		= 8,
	RTP_PAYLOAD_G722		= 9,
	RTP_PAYLOAD_L16_STEREO	= 10,
	RTP_PAYLOAD_L16_MONO	= 11,
	RTP_PAYLOAD_QCELP		= 12,
	RTP_PAYLOAD_MPA			= 14,
	RTP_PAYLOAD_G728		= 15,
	RTP_PAYLOAD_DVI4_11KHz	= 16,
	RTP_PAYLOAD_DVI4_22KHz	= 17,
	RTP_PAYLOAD_G729		= 18,
	RTP_PAYLOAD_CN			= 19,
	RTP_PAYLOAD_GSMEFR		= 20,
	RTP_PAYLOAD_G726_40		= 21,
	RTP_PAYLOAD_G726_24		= 22,
	RTP_PAYLOAD_G726_16		= 23,
	RTP_PAYLOAD_CEL_B		= 25,
	RTP_PAYLOAD_JPEG		= 26,
	RTP_PAYLOAD_NV			= 28,
	RTP_PAYLOAD_H261		= 31,
	RTP_PAYLOAD_MPV			= 32,
	RTP_PAYLOAD_MP2T		= 33,
	RTP_PAYLOAD_H263		= 34,

	RTP_PAYLOAD_DYN_MIN		= 96,

	RTP_PAYLOAD_DTMF		= 100,
	RTP_PAYLOAD_CISCO_RTP	= 121,
	RTP_PAYLOAD_L16_8K_MONO	= 122,
	RTP_PAYLOAD_UNDEFINED	= 123,

	RTP_PAYLOAD_DYN_MAX		= 127
};

// RTP port pair structure
struct RTPPorts
{
	Uint32	rtpPort;	/// Port used by the RTP transceiver
	Uint32	rtcpPort;	/// Port used by the RTCP transceiver
};

// RTP payload descriptor
struct RTPPayloadFormat
{
	Uint32	bytesPerBlock;	 /// Minimum divisable unit of the transmitted data.
	Uint32	blocksPerPacket; /// Number of blocks required to form the packet payload.
	Uint32	blockDuration;	 /// RTP time duration of a single block payload.
	double	blocksPerSecond; /// Blocks required to sustain playback for 1 second.
};

// RTP data related metrics
struct RTPCountMetrics
{
	Uint32 packetCount;	/// Packet counter.
	Uint32 octetCount;	/// Payload byte counter.
};


// RTP sequence identification related metrics
struct RTPTimeMetrics
{
	Uint32			cycles;		/// Sequence number overflow counter.
	RTPSeqNumber	sequence;	/// Packet sequence number.
	RTPTime			timestamp;	/// Time relative to the beginning of the data stream.
	NTPTime			wallclock;	/// Absolute wall-clock time.
};


// RTP packet packet statistics
struct RTPStats
{
	RTPSrc			ssrc;		/// Synchronization source identifier.

	RTPTimeMetrics	seed;		/// Initially generated packet sequence values.
	RTPTimeMetrics	last;		/// Sequence of the most recently processed packet.

	Uint32 			jitter;		/// Estimated jitter.

	RTPCountMetrics	dropped;	/// Dropped packets/payload octets.
	RTPCountMetrics	handled;	/// Processed packets/payload octets.
};

#endif /*RTP_TYPES_HPP_*/
