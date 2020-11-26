/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RtpPacket.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef RTPPACKET_HPP_
#define RTPPACKET_HPP_

#ifdef _ARCH_WIN32
	#include <windows.h>
#else
	#include <sys/types.h>
	#include <netinet/in.h>
	#include <inttypes.h>
#endif

#include "RtpTypes.hpp"
#include "UtilsBuffer.hpp"
#include "ExceptionClass.hpp"

// Packet object used by the RTP protocol.

class RTPPacket
{
public:

	// The indicated number of contributing sources must
	// lay in the range [1,15] or it will automatically
	// be adjusted.
	// csrc_tab [in] Array containing the contributing
	// source identifiers that will be inserted into
	// the RTP packet.
	// csrc_num [in] Number of contributing sources
	// contained in the supplied array.
	
	RTPPacket(void);
	RTPPacket(RTPSrc ssrc);
	RTPPacket(RTPSrc ssrc,
				const RTPSrc* csrc_tab,
				Uint32 csrc_num);	
	~RTPPacket(void);
	
	// Retrieves the RTP packet data.
	// Returns a pointer to a memory area
	// containing the packet data.
	const Uint8* GetPacketData(void) const;
	
	// Retrieves the size of the RTP packet.
	// Returns the total size of the packet
	// data expressed in bytes.
	Uint32	GetPacketSize(void) const;

	// Retrieves size of the RTP packet excluding any
	// trailing padding bytes.
	// Returns the actual size of the RTP
	// packet expressed in bytes, excluding
	// any existing trailing padding.
	Uint32 GetActualSize(void) const;
 	
	// Sets the RTP protocol version of the packet.
	// value [in] The RTP protocol version of
	// the RTP packet.
	void SetVersion(Uint32 value);

	// Retrieves the RTP protocol version of the packet.
	// Returns the RTP protocol version of the
	// RTP packet (default value is 2).
	Uint32 GetVersion(void) const;
	
	// Applies or removes padding from the RTP packet.
	// Padding will only be applied if necessary, in order
	// to adjust the size of the RTP packet so that it is
	// an exact multiple of four (4) octets/bytes.
	// value [in] New setting of the RTP packet
	// padding flag.
	void SetPadding	(bool value);
	
	// Tests if padding has been applied to the RTP
	// packet.
	// Returns true if the RTP packet
	// is padded and false otherwise.
	bool HasPadding(void) const;

	// Sets the extension flag of the RTP packet.
	// value [in] The new value of the extension
	// flag of the RTP packet.
	void SetExtension(bool value);

	// Retrieves the extension flag of the RTP packet.
	// Returns true if the extension flag
	// of the RTP packet is set and false
	// otherwise.
	bool HasExtension(void) const;
	
	// Sets the marker flag of the RTP packet.
	// value [in] The new value of the marker flag
	// of the RTP packet.
	void SetMarker(bool value);

	// Retrieves the marker flag of the RTP packet.
	// Returns true if the marker flag of
	// the RTP packet is set and false
	// otherwise.
	Uint32 HasMarker(void) const;
	
	// Sets the sequence number of the RTP packet.
	// value [in] The new sequence number of
	// the RTP packet.
	void SetSequence(RTPSeqNumber seq);

	// Retrieves the sequence number of the RTP packet.
	// Returns the sequence number of the RTP
	// packet.
	RTPSeqNumber GetSequence(void) const;
	
	// Sets the timestamp of the RTP packet.
	// value [in] The timestamp of the packet.
	void SetTimestamp(RTPTime value);

	// Retrieves the timestamp of the RTP packet.
	// Returns the timestamp of the RTP packet.
	RTPTime	GetTimestamp(void) const;
	
	// Sets the synchronization source identifier of the
	// RTP data stream.
	// ssrc [in] The new synchronization source
	// identifier.        
	void SetSyncSource(RTPSrc ssrc);
	
	// Retrieves the synchronization source identifier of
	// the RTP data stream.
	// Returns the synchronization source identifier
	// of the RTP data stream.        
	RTPSrc GetSyncSource(void) const;

	// Retrieves the number of the contributing sources
	// contained in this RTP packet.
	// Returns the number of the contributing
	// sources contained in this RTP packet.
	Uint32 GetNumCntrSources(void) const;
	
	// Retrieves the ith contributing source identifier 
	// contained in this RTP packet.
	// index [in] Index of the contibuting source
	// indentifier to be retrieved.
	// Returns the contributing source identifier
	// on success, or zero (0) if the supplied
	// index is out of range.	
	RTPSrc GetIthCntrSource(Uint32 index) const;

	// Sets the contributing sources of this RTP packet.
	// Any previously set contributing source identifiers 
	// will be replaced by the newlly supplied ones.
	// csrc_tab [in] Array containing the contributing
	// source identifiers to be inserted into the
	// RTP packet.
	// csrc_num [in] Total number of contributing
	// source identifiers contained in the supplied
	// array.
	void SetCntrSources(const RTPSrc* csrc_tab, 
						Uint32 csrc_num);

	// Adds a contributing source identifier into the RTP
	// packet.
	// csrc [in] Contributing source identifier
	// to be added into the RTP packet.
	void AddCntrSource(RTPSrc csrc);

	// Removes any previously added contributing source
	// identifiers.
	// if no contributing source identifiers exist, the
	// method has no effect.
	void ClearCntrSources(void);

	// Set the payload type of the packet.
	// type [in] The new type of the packet.
	void SetPayloadType(RTPPayloadType type);

	// Retrieves the payload type of the RTP packet.
	// Returns the payload type of the packet.
	RTPPayloadType	GetPayloadType	(void) const;

	// Sets the payload part of the RTP packet.
	// Any previously inserted payload is replaced by
	// the newlly supplied payload data.
	// NOTE: Previously applied padding will implicitly
	// be removed.
	// data [in] Pointer to the data buffer.
	// size [in] The number of bytes to be added 
	// to the payload.
	// Returns the new size of the payload part
	// of the RTP packet.
	Uint32 SetPayloadData(const Uint8* data, 
							Uint32 size);

	// Appends data to the payload part of the RTP packet.
	// NOTE: Previously applied padding will implicitly
	// be removed.
	// data [in] Pointer to the data buffer.
	// size [in] The number of bytes to be
	// appended.
	// Returns the number of bytes actually
	// appended.
	Uint32 AddPayloadData(const Uint8* data, 
							Uint32 size);
	
	// Removes any previously set payload data content.
	// Any previously allocated memory is released. If the
	// RTP packet does not contain any payload data, the
	// method has no effect.
	// NOTE: Any previously applied padding will implicitly
	// be removed, since it will no longer be necessary.
	void ClearPayloadData(void);

	// Retrieves the payload size.
	// Returns the size of the packet payload
	// data expressed in bytes.
	Uint32	GetPayloadSize(void) const;

	// Retrieves the payload data.
	// Returns a pointer to a memory area that
	// contains the payload data.
	const Uint8* GetPayloadData(void)	const;	

	//	Miscellaneous methods
	
	// Performs basic RTP packet validation.
	// Returns true if the RTP packet
	// seems to be valid and false
	// otherwise.
	bool IsValid(void) const;
	
	// Prints packet information primarily for debugging
	// purposes.
	void PrintSelf(void) const;

protected:

	// Protected attributes.	

	ByteBuffer m_data;
};

#endif /*RTPPACKET_HPP_*/
