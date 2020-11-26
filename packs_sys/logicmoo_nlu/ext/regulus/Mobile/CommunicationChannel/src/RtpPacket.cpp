/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RtpPacket.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <iostream>
#include "RtpPacketTypes.hpp"
#include "RtpPacket.hpp"
#include "ace/Log_Msg.h"

using namespace std;

// Implementation of the 'RTPPacket'.
RTPPacket::RTPPacket(void)
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::RTPPacket()]"));
#endif

	// Initialize and clear the RTP packet contents.

	Uint32	size(sizeof(RTPHeader));

	m_data.Truncate(size);

	// Set default header values.

	SetVersion(RTP_VERSION);
	SetPayloadType(RTP_PAYLOAD_UNDEFINED);
	
	return;
}

RTPPacket::RTPPacket(RTPSrc ssrc)
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::RTPPacket()]"));
#endif

	// Initialize and clear the RTP packet contents.

	Uint32 size(sizeof(RTPHeader));

	m_data.Truncate(size);
	
	// Set default header values.

	SetVersion(RTP_VERSION);
	SetPayloadType(RTP_PAYLOAD_UNDEFINED);
	SetSyncSource(ssrc);
	
	return;
}

RTPPacket::RTPPacket(RTPSrc ssrc,
					 const RTPSrc* csrc_tab,
					 Uint32 csrc_num)
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::RTPPacket()]"));
#endif

	// Initialize and clear the RTP packet contents.

	Uint32	size(sizeof(RTPHeader));

	m_data.Truncate(size);
	
	// Set default header values.

	SetVersion(RTP_VERSION);
	SetPayloadType(RTP_PAYLOAD_UNDEFINED);
	SetSyncSource(ssrc);
	SetCntrSources(csrc_tab, csrc_num);

	return;
}

RTPPacket::~RTPPacket(void)
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::~RTPPacket()]"));
#endif

	m_data.Clear();
	 
	return;
}

const Uint8* RTPPacket::GetPacketData(void) const
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::GetPacketData()]"));
#endif

	return m_data.Data();
}

Uint32 RTPPacket::GetPacketSize(void) const
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::GetPacketSize()]"));
#endif

	return m_data.Size();
}

Uint32 RTPPacket::GetActualSize(void) const
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::GetActualSize()]"));
#endif

	Uint32 padding(0);

	if ((HasPadding() == true) && (m_data.Size() != 0))
	{ 
		padding = m_data[m_data.Size() - 1]; 
	}
	
	if (padding > m_data.Size())
	{ 
		padding = 0; 
	}

	return m_data.Size() - padding;
}

void RTPPacket::SetVersion(Uint32 value)
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::SetVersion()]"));
#endif

	RTPHeader::Cast(GetPacketData()).version = value;		
	
	return;
}

Uint32 RTPPacket::GetVersion(void) const
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::GetVersion()]"));
#endif
		
	return (RTPHeader::Cast(GetPacketData()).version);	
}

void RTPPacket::SetPadding(bool value)
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::SetPadding()]"));
#endif
	
	if (value == HasPadding())
	{ 
		return; 
	}

	if (value == true)
	{
		// Apply padding.

		// NOTE: Padding needs to be at least 1 byte long in
		// order to store the padding size as the last byte 
		// of the packet.

		Uint32	mod(m_data.Size() % 4);

		if (mod != 0)
		{
			Uint32	pad(4 - mod);

			m_data.Truncate(m_data.Size() + pad - 1);
			m_data.Append((Uint8)pad);

			RTPHeader::Cast(GetPacketData()).padding = 1;
		}
	}
	else
	{
		// Remove padding.

		Uint32 pad(0);

		if (m_data.Size() != 0)
		{ 
			pad = m_data[m_data.Size() - 1]; 
		}
		
		if ((pad != 0) && (pad >= m_data.Size()))
		{ 
			m_data.Truncate(m_data.Size() - pad); 
		}

		RTPHeader::Cast(GetPacketData()).padding = 0;
	}

	return;
}

bool RTPPacket::HasPadding(void) const
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::HasPadding()]"));
#endif

	return (RTPHeader::Cast(GetPacketData()).padding != 0);	
}

void RTPPacket::SetExtension(bool value)
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::SetExtension()]"));
#endif

	RTPHeader::Cast(GetPacketData()).extension =
		(value == true) ? 1 : 0;
				
	return;
}

bool RTPPacket::HasExtension(void) const
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::HasExtension()]"));
#endif

	return (RTPHeader::Cast(GetPacketData()).extension != 0);	
}

void RTPPacket::SetMarker(bool value)
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::SetMarker()]"));
#endif

	RTPHeader::Cast(GetPacketData()).marker =
		(value == true) ? 1 : 0;
		
	return;
}

Uint32 RTPPacket::HasMarker(void) const
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::HasMarker()]"));
#endif

	return (RTPHeader::Cast(GetPacketData()).marker != 0);
}

void RTPPacket::SetSequence(RTPSeqNumber value) 
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::SetSequence()]"));
#endif
	
	RTPHeader::Cast(GetPacketData()).sequence =
		htons((unsigned short)value);
		
	return;
}

RTPSeqNumber RTPPacket::GetSequence(void) const
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::GetSequence()]"));
#endif

	return ntohs(RTPHeader::Cast(GetPacketData()).sequence);
}

void RTPPacket::SetTimestamp(RTPTime value)  
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::SetTimestamp()]"));
#endif

	RTPHeader::Cast(GetPacketData()).timestamp = htonl(value);
				
	return;
}

RTPTime	RTPPacket::GetTimestamp(void) const
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::GetTimestamp()]"));
#endif

	return ntohl(RTPHeader::Cast(GetPacketData()).timestamp);
}

void RTPPacket::SetSyncSource(RTPSrc ssrc) 
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::SetSyncSource()]"));
#endif

	RTPHeader::Cast(GetPacketData()).ssrc = htonl(ssrc);
			
	return;
} 

RTPSrc RTPPacket::GetSyncSource(void) const
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::GetSyncSource()]"));
#endif
	
	return ntohl(RTPHeader::Cast(GetPacketData()).ssrc);
} 
	
Uint32 RTPPacket::GetNumCntrSources(void) const
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::GetNumCntrSources()]"));
#endif

	return RTPHeader::Cast(GetPacketData()).count;
}	

RTPSrc RTPPacket::GetIthCntrSource(Uint32 i) const
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::GetIthCntrSource()]"));
#endif

	if (i >= GetNumCntrSources())
	{ 
		return RTPSrc(); 
	}
	
	return ntohl(RTPHeader::Cast(GetPacketData()).GetCSrc(i));
}

void RTPPacket::SetCntrSources(const RTPSrc* csrc_tab, 
								Uint32 csrc_num)
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::SetCntrSources()]"));
#endif

	if (csrc_num > 15)
	{ 
		csrc_num = 15; 
	}
	
	RTPSrc	tmp_tab[16];

	for (Uint32 i = 0; i < csrc_num; ++i)
	{ 
		tmp_tab[i] = htonl(csrc_tab[i]);
	}

	Uint32	csrc_cur(GetNumCntrSources());

	Uint32	length(csrc_cur * sizeof(RTPSrc));
	Uint32	offset(sizeof(RTPHeader));

	m_data.Replace(offset, length, (const Uint8*)tmp_tab,
		csrc_num * sizeof(RTPSrc));

	RTPHeader::Cast(GetPacketData()).count = csrc_num;
	
	return;	
}	

void RTPPacket::AddCntrSource(RTPSrc csrc)
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::AddCntrSource()]"));
#endif

	Uint32	csrc_cur(GetNumCntrSources());

	if (csrc_cur >= 15)
	{ 
		return; 
	}

	csrc = htonl(csrc);

	Uint32	length(csrc_cur * sizeof(RTPSrc));
	Uint32	offset(sizeof(RTPHeader) + length);

	m_data.Insert(offset, (const Uint8*)&csrc, sizeof(RTPSrc));

	RTPHeader::Cast(GetPacketData()).count = csrc_cur + 1;
	
	return;
}

void RTPPacket::ClearCntrSources(void)
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::ClearCntrSources()]"));
#endif

	Uint32 csrc_cur(GetNumCntrSources());

	Uint32 length(csrc_cur * sizeof(RTPSrc));
	Uint32	offset(sizeof(RTPHeader));

	m_data.Erase(offset, length);

	RTPHeader::Cast(GetPacketData()).count = 0;
	
	return;
}

void RTPPacket::SetPayloadType(RTPPayloadType type) 
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::SetPayloadType()]"));
#endif
	
	RTPHeader::Cast(GetPacketData()).type = (type & 0x7f);
	
	return;
}

RTPPayloadType RTPPacket::GetPayloadType(void) const
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::GetPayloadType()]"));
#endif
	
	return RTPPayloadType(RTPHeader::Cast(GetPacketData()).type);			
}
	
Uint32	RTPPacket::AddPayloadData(const Uint8* data, Uint32 size)
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::AddPayloadData()]"));
#endif

	Uint32 remain(RTP_MTU - GetActualSize());

	if (size > remain)
	{
//		cout << endl << "---[DEBUG] [-----------]" << endl;
		// TODO: Recheck
		size = remain;
	}

	if (size != 0)
	{
		// NOTE: By restting and then setting the padding
		// flag, we force the removal and the re-insertion
		// of any required trailling zeros.

		SetPadding(false);
		m_data.Append(data, size);
	}
	
	return size;
}

Uint32 RTPPacket::SetPayloadData(const Uint8* data, Uint32 size)
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::SetPayloadData()]"));
#endif

	ClearPayloadData();
	
	return AddPayloadData(data, size);
}

void RTPPacket::ClearPayloadData(void)
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::ClearPayloadData()]"));
#endif

	Uint32 size(GetPayloadSize());

	if (size != 0)
	{
		Uint32 	offset(sizeof(RTPHeader)
			+ (GetNumCntrSources() * sizeof(RTPSrc)));

		SetPadding(false);
		m_data.Erase(offset, size);		
	}	

	return;
}

const Uint8* RTPPacket::GetPayloadData(void) const
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::GetPayloadData()]"));
#endif

	return (GetPacketData() + sizeof(RTPHeader)
		+ (GetNumCntrSources() * sizeof(RTPSrc)));
}

Uint32 RTPPacket::GetPayloadSize(void) const
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::GetPayloadSize()]"));
#endif

	// NOTE: The value returned by the 'getActualSize()' does
	// not include the trailling padding zeros.

	return (GetActualSize() - sizeof(RTPHeader)
		- (GetNumCntrSources() * sizeof(RTPSrc)));
}

bool RTPPacket::IsValid(void) const
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RTPPacket::IsValid()]"));
#endif

	Uint32	size(sizeof(RTPHeader)
		+ (GetNumCntrSources() * sizeof(RTPSrc)));
	
//	cout << "---" << sizeof(RTPHeader) << "---" 
//	<< GetNumCntrSources() << "---" << sizeof(RTPSrc) << endl;
	
	if (GetPacketSize() < size)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RTPPacket::IsValid()] "
				"[Invalid packet size]")));
	
		return false;
	}
	
	if (GetVersion() != RTP_VERSION)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RTPPacket::IsValid()] "
				"[Invalid version]")));		
				
		return false;
	}
	
#if 0
	if (GetPacketSize() < (RTP_MTU / 3 ))
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RTPPacket::IsValid()] "
				"[SID packet?]")));		
		
		return false;
	}
#endif

	return true;	
}

void RTPPacket::PrintSelf(void) const
{
	ACE_TRACE(ACE_TEXT("[RTPPacket::printSelf()]"));
	
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RTPPacket::PrintSelf()] "
				"[RTPPacket] [PacketSize: %d(%%d)]\n"
				"[RTPPacket] [PayloadSize: %d]\n"
				"[RTPPacket] [Version: %d]\n"
				"[RTPPacket] [Padding: %d]\n"
				"[RTPPacket] [Extension: %d]\n"
				"[RTPPacket] [CSRC Count: %d]\n"
				"[RTPPacket] [Marker: %d]\n"
				"[RTPPacket] [Payload Type: %d]\n"
				"[RTPPacket] [Sequence Number: %d]\n"
				"[RTPPacket] [Timestamp: %d]\n"
				"[RTPPacket] [SyncSource: %d]\n"),
				GetPacketSize(), GetActualSize(), GetPayloadSize(),
				GetVersion(), (HasPadding() ? m_data[m_data.Size() - 1] : 0),
				(HasExtension() ? 1 : 0), GetNumCntrSources(),
				(HasMarker() ? 1 : 0), (Uint32)GetPayloadType(),
				GetSequence(), GetTimestamp(), (Uint32)GetSyncSource()));
				
	return;
}
