/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RtpTransceiver.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <limits.h>
#include <string.h>
#include <iostream>
#include "RtpTransceiver.hpp"
#include "ace/Log_Msg.h"

//#include "core-rand-gen.hpp"

#include <vector>

using namespace std;

vector<RTPPacket> Packets;
//vector<RTPPacket> Packets2;
int counter = 0;

//	Helper classes and functions.

class RTPPacketEx : public RTPPacket
{
public:

	RTPPacketEx (const Uint8* data, Uint32 size)
	{
		m_data.Replace(0, data, size);
		
		return;
	}
};

// Public variables.

static const RTPPayloadFormat s_payload_format = { 1, 20, 1, 0.0 };

//	Helper functions

static long	AbsInt(long value)
{
	return (value < 0) ? -value : value;
}

static int CmpSeq(RTPSeqNumber s1,
					RTPSeqNumber s2,
					unsigned short thresh)
{
	const int bnd(USHRT_MAX - thresh);
	const int adj(USHRT_MAX + 1);
	int diff(s1 - s2);

	if (diff > bnd)
	{ 
		return diff - adj; 
	}
	else if (diff < -bnd)
	{ 
		return diff + adj; 
	}
	else
	{ 
		return diff; 
	}
}

static Uint32 GenerateRandom(void)
{
	// NOTE: Should be seeded elsewhere..
	Uint32 rnd = 0;
	
	while (rnd == 0)
	{
		rnd = rand();
	}
	
	return rnd;
}

//	Implementation of the 'RtpTransceiver' class.

RtpTransceiver::RtpTransceiver(RtpTransceiverObserver& observer) 
	:	m_observer(observer),
		m_ssrc(0),
		m_payload_type(RTP_PAYLOAD_UNDEFINED),
		m_payload_format(s_payload_format),
		m_rx_limit(RTP_IN_BUFFER_SIZE),
		m_tx_limit(RTP_OUT_BUFFER_SIZE)
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::RtpTransceiver()]"));

	ClearRxStats();
	ClearTxStats();
	CloneState();
	
	m_server = "";
	m_remote_port = 0;
	m_local_port = 0;
	
	m_socket_engine = NULL;
	m_socket_engine = new SocketsEngine(*this);
//	m_audiostream_engine = NULL;
//	m_audiostream_engine = new AudioStreamEngine(*this);
	
	return;
}

RtpTransceiver::~RtpTransceiver(void)
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::~RtpTransceiver()]"));

	Close();

	if (m_socket_engine != NULL)
	{
		delete m_socket_engine;
	}
	
/*	if (m_audiostream_engine != NULL)
	{
		delete m_audiostream_engine;
	}*/
	
	return;
}

void RtpTransceiver::SetServerName(const string name)
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::SetServerName()]"));
		
	m_server = name;
	
	return;
}

const string RtpTransceiver::GetServerName()
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::GetServerName()]"));
			
	return m_server;
}

void RtpTransceiver::SetServerPort(int port)
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::SetServerPort()]"));
	
	m_remote_port = port;	
	
	return;
}

int RtpTransceiver::GetServerPort()
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::GetServerPort()]"));
		
	return m_remote_port;
}

void RtpTransceiver::SetLocalRtpPort(int port)
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::SetLocalRtpPort()]"));
	
	m_local_port = port;	
	
	return;
}

int RtpTransceiver::GetLocalRtpPort()
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::GetLocalRtpPort()]"));
	
	return m_local_port;
}

void RtpTransceiver::Close(void)
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::Close()]"));
	
//!	clearState(IOStream::STATE_FAIL);
	m_rx_data.Clear();
	m_tx_data.Clear();
	
	return;
}

Uint32 RtpTransceiver::Flush(bool force)
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::Flush()]"));
	
	if (m_tx_data.Size() == 0)
	{ 
		return 0; 
	}
	
/*!	if (eos() == true)
	{ 
		throw IOStream::EndOfStream(); 
	}
	
	if (fail() == true)
	{ 
		throw IOStream::InvalidStream(); 
	}
	
	if (IsConnected() == false)
	{ 
		throw IOStream::InvalidStream(); 
	}
*/
	// Initialize the transmission statistics, if not
	// already inititalized.

	if (HasTxStats() == false)
	{ 
		ResetTxStats();
	}

	Uint32 writen(0);
	Uint32 needed(m_payload_format.blocksPerPacket
					* m_payload_format.bytesPerBlock);

	RTPPayloadInfo info;
	RTPPacket packet(m_ssrc, m_csrcs.Data(), m_csrcs.Size());

	packet.SetPayloadType(m_payload_type);

	while (m_tx_data.Size() != 0)
	{
		packet.ClearPayloadData();

		if (m_tx_data.Size() < needed)
		{
			if (force == true)
			{ 
				needed = m_tx_data.Size(); 
			}
			else
			{ 
				break; 
			}
		}

		for (Uint32 remain = needed; remain != 0; )
		{
			const Uint8* buffer;
			Uint32	length(m_tx_data.Access(
						buffer, needed - remain));

			if (remain < length)
			{ 
				length = remain; 
			}

			packet.AddPayloadData(buffer, length);

			remain -= length;
		}

		packet.SetSequence(m_tx_next.sequence);
		packet.SetTimestamp(m_tx_next.timestamp);

		// Step 3: Failing to transmit the packet indicates a
		// fatal error. However, in the case of success, adjust
		// the outgoing data buffer and loop over. 

		EvalPayloadInfo(packet.GetPayloadType(),
			packet.GetPayloadSize(), info);

		if (SendPacket(packet, info) != packet.GetPacketSize())
		{ 
			break; 
		}

		m_tx_data.Discard(packet.GetPayloadSize());
		writen += packet.GetPayloadSize();
	}  

	return writen;
}

void RtpTransceiver::SetLocalPort(Uint32 port)
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::SetLocalPort()]"));
	
	m_socket_engine->SetLocalPort(port);
	
	return;
}

Uint32	RtpTransceiver::GetLocalPort(void) const
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::GetLocalPort()]"));
	
	return m_socket_engine->GetLocalPort();
}

bool RtpTransceiver::GetPacket(void)
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::GetPacket()]"));
	
/*!	if (eos() == true)
	{ 
		throw IOStream::EndOfStream(); 
	}
	
	if (fail() == true)
	{ 
		throw IOStream::InvalidStream(); 
	}
	
	if (IsListening() == false)
	{ 
		throw IOStream::InvalidStream(); 
	}
*/

	return (RecvPacket("") != 0);
}

bool RtpTransceiver::PutPacket(const RTPPacket& packet)
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::PutPacket()]"));
#endif

/*!	if (eos() == true)
	{ 
		throw IOStream::EndOfStream(); 
	}
	
	if (fail() == true)
	{ 
		throw IOStream::InvalidStream(); 
	}
	
	if (IsConnected() == false)
	{ 
		throw IOStream::InvalidStream(); 
	}
*/
/*	cout << "---" << packet.IsValid() 
			<< "---" << packet.GetSequence() 
			<< "---" << m_tx_next.sequence
			<< "---" << packet.GetTimestamp()
			<< "---" << m_tx_next.timestamp
			<< "---" << packet.GetSyncSource()
			<< "---" << m_ssrc << endl;
*/
	if ((packet.IsValid() == false)
	 || (packet.GetSequence() != m_tx_next.sequence)
	 || (packet.GetTimestamp() < m_tx_next.timestamp)
	 || (packet.GetSyncSource() != m_ssrc))
	{ 
		// TODO: Recheck
//		return false; 
	}

	// Initialize the transmission statistics, if not
	// already inititalized.

	if (HasTxStats() == false)
	{ 
		ResetTxStats(); 
	}

	// Evaluate the time metrics of the supplied packet
	// and proceed by transmitting it over the network.

	RTPPayloadInfo	info;

	EvalPayloadInfo(packet.GetPayloadType(),
					packet.GetPayloadSize(), info);

	if (SendPacket(packet, info) != packet.GetPacketSize())
	{
		DropPayload(packet.GetPayloadSize(), info);
		
		return false;
	}

	return true;
}

void RtpTransceiver::Connect(void)
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::Connect()]"));

/*!	try
	{ 
		m_udp_peer.connect(); 
	}
	catch (...)
	{ 
		cloneState(); throw; 
	}
*/
	ClearTxStats();
	CloneState();
	
	return;
}

void RtpTransceiver::Listen(Uint32 port)
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::Listen()]"));
	
/*!	try
	{ 
		m_udp_peer.listen(port); 
	}
	catch (...)
	{ 
		CloneState(); throw; 
	}
*/
	ClearRxStats();
	CloneState();
	
	return;
}
/*
void RtpTransceiver::SetHostname(const String& addr)
{
	m_udp_peer.SetHostname(addr);
	
	return;
}

String RtpTransceiver::GetHostname(void) const
{
	return m_udp_peer.getHostname();
}

void RtpTransceiver::SetRemotePort(Uint32 port)
{
	m_udp_peer.setRemotePort(port);
	
	return;
}

Uint32 RtpTransceiver::GetRemotePort(void) const
{
	return m_udp_peer.getRemotePort();
}

void RtpTransceiver::SetAddress(const IPAddress& addr)
{
	m_udp_peer.setAddress(addr);
	
	return;
}

IPAddress RtpTransceiver::GetAddress(void) const
{
	return m_udp_peer.getAddress();
}

bool RtpTransceiver::IsConnected(void) const
{
	return m_udp_peer.isConnected();
}

bool RtpTransceiver::IsListening(void) const
{
	return m_udp_peer.isListening();
}

void RtpTransceiver::SetTimeout(Uint32 msecs)
{
	m_udp_peer.setTimeout(msecs);
	
	return;
}

Uint32 RtpTransceiver::GetTimeout(void) const
{
	return m_udp_peer.getTimeout();
}
*//*
void RtpTransceiver::SetFilter(RTPPacketFilter* filter)
{
	m_filter = filter;
	
	return;
}

RTPPacketFilter* RtpTransceiver::GetFilter(void) const
{
	return m_filter;
}
*/
void RtpTransceiver::SetRxBufferSize(Uint32 value)
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::SetRxBufferSize()]"));
#endif

	Uint32 min_size(m_payload_format.blocksPerPacket
				* m_payload_format.bytesPerBlock);
				
	if (value == 0)
	{ 
		value = RTP_IN_BUFFER_SIZE; 
	}
	
	if (value < min_size)
	{ 
		value = min_size; 
	}

	m_rx_limit = value;

	while (m_rx_data.Size() > m_rx_limit)
	{ 
		m_rx_data.Discard(min_size); 
	}

	return;
}

Uint32 RtpTransceiver::GetRxBufferSize(void) const
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::GetRxBufferSize()]"));
	
	return m_rx_limit;
}

Uint32 RtpTransceiver::GetRxOctets(void) const
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::GetRxOctets()]"));
	
	return m_rx_data.Size();
}

RTPStats RtpTransceiver::GetRxStats(void) const
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::GetRxStats()]"));
	
	return m_rx_stats;
}

void RtpTransceiver::ClearRxStats(void)
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::ClearRxStats()]"));
	
	memset(&m_rx_stats, 0, sizeof(RTPStats));
	m_rx_transit = 0;
	
	return;
}

void RtpTransceiver::SetTxBufferSize(Uint32 value)
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::SetTxBufferSize()]"));
	
	Uint32	min_size(m_payload_format.blocksPerPacket
				* m_payload_format.bytesPerBlock);
				
	if (value == 0)
	{ 
		value = RTP_OUT_BUFFER_SIZE; 
	}
	
	if (value < min_size)
	{ 
		value = min_size; 
	}

	m_tx_limit = value;

	while (m_tx_data.Size() > m_tx_limit)
	{ 
		m_tx_data.Discard(min_size); 
	}

	return;
}

Uint32	RtpTransceiver::GetTxBufferSize(void) const
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::GetTxBufferSize()]"));
	
	return m_tx_limit;
}

Uint32	RtpTransceiver::GetTxOctets(void) const
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::GetTxOctets()]"));
	
	return m_tx_data.Size();
}

RTPStats RtpTransceiver::GetTxStats(void) const
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::GetTxStats()]"));
	
	return m_tx_stats;
}

void RtpTransceiver::ClearTxStats(void)
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::ClearTxStats()]"));

	memset(&m_tx_stats, 0, sizeof(RTPStats));
	memset(&m_tx_next, 0, sizeof(RTPTimeMetrics));
	m_tx_transit = 0;
	
	return;
}

void RtpTransceiver::SetSyncSource(RTPSrc ssrc)
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::SetSyncSource()]"));

	if (ssrc == 0)
	{ 
		ssrc = GenerateRandom(); 
	}

	m_ssrc = ssrc;

	ClearTxStats();
	m_tx_data.Clear();
	
	return;
}

RTPSrc RtpTransceiver::GetSyncSource(void) const
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::GetSyncSource()]"));

	return m_ssrc;
}

void RtpTransceiver::SetCntrSources(const RTPSrc* csrc_tab,
									Uint32 csrc_num)
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::SetCntrSources()]"));
	
	if (csrc_num > 15)
	{ 
		csrc_num = 15; 
	}

	m_csrcs.Clear();
	m_csrcs.Append(csrc_tab, csrc_num);
	m_tx_data.Clear();
	
	return;
}

Uint32	RtpTransceiver::GetCntrSources(RTPSrc* csrc_tab,
										Uint32 csrc_num) const
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::GetCntrSources()]"));

	if (m_csrcs.Size() < csrc_num)
	{ 
		csrc_num = m_csrcs.Size(); 
	}

	memcpy(csrc_tab, m_csrcs.Data(), csrc_num * sizeof(RTPSrc));
	
	return csrc_num;
}

void RtpTransceiver::SetPayloadType(RTPPayloadType type)
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::SetPayloadType()]"));

	m_payload_type = type;
	m_tx_data.Clear();
	
	return;
}

RTPPayloadType RtpTransceiver::GetPayloadType(void) const
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::GetPayloadType()]"));
	
	return m_payload_type;
}

void RtpTransceiver::SetPayloadFormat(RTPPayloadFormat format)
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::SetPayloadFormat()]"));
	
	m_payload_format = format;

	if (m_payload_format.bytesPerBlock == 0)
	{ 
		m_payload_format.bytesPerBlock = 1; 
	}
	
	if (m_payload_format.blocksPerPacket == 0)
	{ 
		m_payload_format.blocksPerPacket = 20; 
	}
	
	if (m_payload_format.blockDuration == 0)
	{ 
		m_payload_format.blockDuration = 1; 
	}
	
	if (m_payload_format.blocksPerSecond == 0.0)
	{ 
		m_payload_format.blocksPerSecond = 1.0; 
	}

	return;
}

RTPPayloadFormat RtpTransceiver::GetPayloadFormat(void) const
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::GetPayloadFormat()]"));
	
	return m_payload_format;
}

void RtpTransceiver::CloneState(void)
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::CloneState()]"));

/*!	ClearState();

	if (m_udp_peer.eos() == true)
	{ 
		setState(IOStream::STATE_EOS); 
	}
	
	if (m_udp_peer.fail() == true)
	{ 
		setState(IOStream::STATE_FAIL); 
	}
*/
	return;
}

bool RtpTransceiver::HasRxStats(void) const
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::HasRxStats()]"));
#endif

	return (m_rx_stats.seed.wallclock != NTPTime());
}

bool RtpTransceiver::HasTxStats(void) const
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::HasTxStats()]"));
#endif

	return (m_tx_stats.seed.wallclock != NTPTime());
}

void RtpTransceiver::ResetTxStats(void)
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::ResetTxStats()]"));

	ClearTxStats();

	if (GetSyncSource() == 0)
	{ 
		SetSyncSource(0); 
	}

	m_tx_stats.ssrc = GetSyncSource();
	m_tx_stats.seed.sequence = GenerateRandom();
	m_tx_stats.seed.timestamp = GenerateRandom();
	m_tx_stats.seed.wallclock = NTPTime::Now();
	m_tx_next = m_tx_stats.seed;
	m_tx_transit = 0;
	
	return;
}

void RtpTransceiver::EvalPayloadInfo(RTPPayloadType type,
									 Uint32 octets,
									 RTPPayloadInfo& info)
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::EvalPayloadInfo()]"));
#endif

	// Set the default packet time info values.

	info.sequence = 1;
	info.timestamp = 0;

	// NOTE: The RTP time offset is not affected by
	// transmitted payload data that is not of the
	// configured type.

	if (type != m_payload_type)
	{ 
		return; 
	}

	// Evaluate the RTP duration of the indicated payload
	// size. If the bytes-per-block and block-duration
	// values have not been defined, the default value
	// one (1) is assumed.

	Uint32 blocks(octets / m_payload_format.bytesPerBlock);
	Uint32 duration(blocks * m_payload_format.blockDuration);

	info.timestamp = duration;

	return;
}

Uint32 RtpTransceiver::SendPacket(const RTPPacket& packet,
									const RTPPayloadInfo& info)
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::SendPacket()]"));
#endif

	//packet.PrintSelf();
	const Uint8* buffer(packet.GetPacketData());
	Uint32 length(packet.GetPacketSize());
	//cout << "---[DEBUG] [GetPacketSize]" << length << endl;
	Uint32 writen(0);
/*
	try
	{ 
		writen = m_udp_peer.put(buffer, length); 
	}
	catch (...)
	{ 
		writen = 0; 
		CloneState(); 
	}
*/

	writen = m_socket_engine->WriteData(buffer, length);
	//cout << "---[DEBUG] [writen]" << writen << endl;

	if (writen != length)
	{ 
		return writen; 
	}
		
	// Update transmission statistics.

	Uint32 sequence(packet.GetSequence());
	Uint32 timestamp(packet.GetTimestamp());
	const NTPTime wallclock(NTPTime::Now());

	Uint32	last_seq(m_tx_stats.last.sequence);

	m_tx_stats.last.sequence  = sequence;
	m_tx_stats.last.timestamp = timestamp;
	m_tx_stats.last.wallclock = wallclock;

	if ((sequence < last_seq)
			&& (CmpSeq(last_seq, sequence, 128) < 0))
	{ 
		m_tx_stats.last.cycles += 1; 
	}

	m_tx_next.sequence  = sequence  + info.sequence;
	m_tx_next.timestamp = timestamp + info.timestamp;

	if (m_tx_next.sequence > USHRT_MAX)
	{
		m_tx_next.cycles   += 1;
		m_tx_next.sequence &= USHRT_MAX;
	}

	Uint32	rtp_offset(timestamp - m_tx_stats.seed.timestamp);
	Uint32	rtp_blocks(rtp_offset / m_payload_format.blockDuration);
	double	ntp_offset((double)rtp_blocks / m_payload_format.blocksPerSecond);
	const NTPTime ntp_expect(m_tx_stats.seed.wallclock 
								+ (Uint32)(1000.0 * ntp_offset));

	long trans(NTPTime::Diff(wallclock, ntp_expect));
	long delay(trans - m_tx_transit);

	m_tx_transit = trans;

	m_tx_stats.jitter -= (m_tx_stats.jitter + 8) / 16;
	m_tx_stats.jitter += AbsInt(delay);

	m_tx_stats.handled.packetCount += 1;
	m_tx_stats.handled.octetCount  +=
	packet.GetPayloadSize();

	return writen;
}

Uint32	RtpTransceiver::RecvPacket(string buffer)
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::RecvPacket()]"));
#endif

/*	Uint8 rtp_data[RTP_MTU];
	Uint32 rtp_size(sizeof(rtp_data));
//!	IPAddress rtp_addr;

	Uint32 readn(0);*/

/*	try
	{
		readn = m_udp_peer.getFrom(rtp_data,
				rtp_size, rtp_addr); 
	}
	catch (...)
	{ 
		readn = 0; 
		CloneState(); 
	}

	if (readn == 0)
	{ 
		return readn; 
	}*/
	
	Uint32 readn(0);
//	readn = m_socket_buf.size();
	readn = buffer.size();
//	const RTPPacketEx packet((Uint8*)m_socket_buf.c_str(), readn);
	const RTPPacketEx packet((Uint8*)buffer.c_str(), readn);
	//const RTPPacketEx packet(rtp_data, readn);

	if (packet.IsValid() == false)
	{ 
		//return readn; 
	}

	// Apply packet filtering and bypass standard processing
	// if the user-defined filter indicates that the packet
	// should be dropped.

/*!	if ((m_filter != 0)
	 && (m_filter->FilterPacket(packet, rtp_addr) == true))
	{ 
		return readn; 
	}
*/
	// Apply standard packet processing.

	Uint32 sequence(packet.GetSequence());
	Uint32 timestamp(packet.GetTimestamp());
	const NTPTime wallclock(NTPTime::Now());

	Uint32 last_seq(m_rx_stats.last.sequence);
	int	diff_seq(CmpSeq(sequence, last_seq, 128));

	if (HasRxStats() == false)
	{
		ClearRxStats(); // clears 'm_rx_transit' too.

		m_rx_stats.ssrc = packet.GetSyncSource();
		m_rx_stats.seed.sequence = sequence;
		m_rx_stats.seed.timestamp = timestamp;
		m_rx_stats.seed.wallclock = wallclock;
		m_rx_stats.last = m_rx_stats.seed;

		diff_seq = 1;
	}
	else if (diff_seq > 0)
	{
		m_rx_stats.last.sequence = sequence;
		m_rx_stats.last.timestamp = timestamp;
		m_rx_stats.last.wallclock = wallclock;

		if (sequence < last_seq)
		{ 
			m_rx_stats.last.cycles += 1; 
		}
	}

	if (diff_seq == 0)
	{
		// Duplicate packet. Discard and update statistics.

		m_rx_stats.dropped.packetCount += 1;
		m_rx_stats.dropped.octetCount +=
			packet.GetPayloadSize();
	}
	else if (diff_seq > 0)
	{
		// If the sequence numbers differ by more than one,
		// assume that all intermediate packets were lost.

		m_rx_stats.dropped.packetCount += diff_seq - 1;

		// Process the received packet, if and only if the
		// payload is of the expected type.

		if (packet.GetPayloadType() == m_payload_type)
		{
			// Packet seems to be valid and the payload of the
			// expected type. Enqueue the payload data, discard
			// data if necessary to prevent incoming data buffer
			// overflow, and then update the reception statistics.

			m_rx_data.Enqueue(packet.GetPayloadData(),
				packet.GetPayloadSize());

			SetRxBufferSize(m_rx_limit);

			// NOTE: If this is the very first RTP packet to 
			// be received, the 'ntp_expect' timestamp will
			// be equal to the wallclock time and the delay
			// equal to zero (0).

			Uint32 rtp_offset(timestamp
						- m_rx_stats.seed.timestamp);
			Uint32 rtp_blocks(rtp_offset
						/ m_payload_format.blockDuration);
			double ntp_offset((double)rtp_blocks
						/ m_payload_format.blocksPerSecond);
			const NTPTime ntp_expect(m_rx_stats.seed.wallclock
						+ (Uint32)(1000.0 * ntp_offset));

			long trans(NTPTime::Diff(wallclock, ntp_expect));
			long delay(trans - m_rx_transit);

			m_rx_transit = trans;

			m_rx_stats.jitter -= (m_rx_stats.jitter + 8) / 16;
			m_rx_stats.jitter += AbsInt(delay);

			m_rx_stats.handled.packetCount += 1;
			m_rx_stats.handled.octetCount  +=
					packet.GetPayloadSize();
		}
		else
		{
			m_rx_stats.dropped.packetCount += 1;
			m_rx_stats.dropped.octetCount  +=
					packet.GetPayloadSize();
		}
	}
	
//	Packets.push_back(packet);
	
	m_observer.TTSDataReceived(packet.GetPayloadData());
	
//	m_audiostream_engine->PlayData(packet.GetPayloadData());
	
	return readn;
}

void RtpTransceiver::DropPayload(Uint32 octets,
								 const RTPPayloadInfo& info)
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::DropPayload()]"));
	
	m_tx_next.sequence  += info.sequence;
	m_tx_next.timestamp += info.timestamp;

	if (m_tx_next.sequence > USHRT_MAX)
	{
		m_tx_next.cycles   += 1;
		m_tx_next.sequence &= USHRT_MAX;
	}

	m_tx_stats.dropped.packetCount += 1;
	m_tx_stats.dropped.octetCount  += octets;

	return;
}

void RtpTransceiver::StartComms()
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::StartComms()]"));
		
	m_socket_engine->SetServerName(m_server);
	m_socket_engine->SetServerPort(m_remote_port);
	m_socket_engine->SetLocalPort(m_local_port);
	m_socket_engine->SetBufferSize(RTP_MTU);
	m_socket_engine->ConnectServer("UDP");
 	
 	return;
}

void RtpTransceiver::StopComms()
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::StopComms()]"));
		
	// TODO: Recheck if stop listening to port must be implemented
 	
	if (m_socket_engine != NULL)
	{
		m_socket_engine->DisconnectServer();		
	}
	
 	return;
}

void RtpTransceiver::MessageReceived(ostringstream& buffer)
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::MessageReceived()]"));
#endif
	
//	m_socket_buf = buffer;
	m_socket_buf = buffer.str();
//	string m_socket_buf = buffer.str();
	
#if _DEBUG_2
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RtpTransceiver::MessageReceived()] "
					"[Received Message: %d]\n"), m_socket_buf.size()));	
#endif

//	cout << buffer.str() << endl;
//	m_socket_engine->WriteData((Uint8*)m_socket_buf.c_str(), m_socket_buf.size());

	while (m_socket_buf.size() >= RTP_MTU)
	{
		counter++;
//		cout << "---[DEBUG] [Packet: " << counter << "]" << endl;
		string buffer;
		buffer.assign(m_socket_buf, 0, RTP_MTU);
		m_socket_buf.erase(0, RTP_MTU);			
		RecvPacket(buffer);
	}
	
	return;
}

void RtpTransceiver::SendPackets()
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::SendPackets()]"));
	
	counter = 0;
	int data[172] = {0};
	//RTPPayloadInfo info;
	//Uint32 needed(m_payload_format.blocksPerPacket
	//				* m_payload_format.bytesPerBlock);
	RTPPacket pack(m_ssrc, (RTPSrc*)data, 0);
	pack.SetPayloadType(m_payload_type);
	
//cout << "---[DEBUG] [RtpTransceiver::SendPackets()] >>" << endl;
//cout << "---[DEBUG] [Packets.size()] " << Packets.size() << endl;

	for (int i = 0; i < (int)Packets.size(); ++i)
	{
		pack.ClearPayloadData();
		
		if (Packets[i].GetPacketData() == NULL)
		{
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RtpTransceiver::SendPackets()] "
					"[Packets.size()] [NULL PACKET!!!]\n")));				
		}		
		
		pack.AddPayloadData(Packets[i].GetPayloadData(), Packets[i].GetPayloadSize());
		pack.SetSequence(m_tx_next.sequence);
		pack.SetTimestamp(m_tx_next.timestamp);

		++counter;		
		
		if (PutPacket(pack) == false)
		{
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RtpTransceiver::SendPackets()] "
					"[PutPacket returned false]\n")));			
		}
	}
	
// cout << "---[DEBUG] [RtpTransceiver::SendPackets()] <<" << endl;
#if 0	
	// ?
	//ACE_Time_Value tv;
	//tv.msec(300);
		
	counter = 0;
	
	int data[172] = {0};
	RTPPayloadInfo info;
	Uint32 needed(m_payload_format.blocksPerPacket
					* m_payload_format.bytesPerBlock);
	RTPPacket pack(m_ssrc, (RTPSrc*)data, 0);
	pack.SetPayloadType(m_payload_type);
#if 0	
	//for (int i = 0; i < Packets.size(); ++i)	
	for (int i = 0; i < Packets2.size(); ++i)
	{
		pack.ClearPayloadData();
		pack.AddPayloadData(Packets2[i].GetPayloadData(), Packets2[i].GetPayloadSize());
		//pack.AddPayloadData(Packets[i].GetPayloadData(), Packets[i].GetPayloadSize());
		/*if ((i > 5) && (i < 350))
		{			
			pack.AddPayloadData(Packets2[i].GetPayloadData(), Packets2[i].GetPayloadSize());			
		}		
		else
		{			
			pack.AddPayloadData(Packets[i].GetPayloadData(), Packets[i].GetPayloadSize());			
		}*/
		pack.SetSequence(m_tx_next.sequence);
		pack.SetTimestamp(m_tx_next.timestamp);
		//const Uint8* tmp(pack.GetPayloadData());
		//cout << "---tmp[0]" << tmp[0] << endl;		
		//Packets[i].SetSequence(m_tx_next.sequence);
		//Packets[i].SetTimestamp(m_tx_next.timestamp);	
				
		counter++;		
		
		if (PutPacket(pack) == false)
		//if (PutPacket(Packets[i]) == false)
		//if (PutPacket(Packets2[i]) == false)
		{
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RtpTransceiver::SendPackets()] "
					"[PutPacket returned false]\n")));
		}
		//m_audiostream_engine->PlayData(Packets2[i].GetPayloadData());
		//m_audiostream_engine->PlayData(Packets2[i].GetPayloadData());
		//const Uint8* buf(Packets[i].GetPacketData());		
		//m_audiostream_engine->PlayData(Packets[i].GetPayloadData());
		//m_audiostream_engine->PlayData(pack.GetPayloadData());
		//PutPacket(Packets[i]);
		//Packets[i].PrintSelf();
		//pack.PrintSelf();
		//PutPacket(pack);
	}
#endif

#endif

	// TODO: Recheck
	for (int i = 0; i < 50; ++i)
	{
		counter++;
		PutPacket(Packets[Packets.size()-1]);
	}

#if _DEBUG_0
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RtpTransceiver::SendPackets()] "
					"[Send %d packets]\n"), counter));	
#endif
	
	return;
}

void RtpTransceiver::ClearPacketsBuffer()
{
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::ClearPacketsBuffer()]"));
	
	Packets.clear();
 
	return;
}

void RtpTransceiver::CreateAndSendPacket(const Uint8* buffer)
{
#if _DEBUG_1
	ACE_TRACE(ACE_TEXT("[RtpTransceiver::CreateAndSendPacket()]"));
#endif
	
	int data[172] = {0};
	//RTPPayloadInfo info;
	//Uint32 needed(m_payload_format.blocksPerPacket
	//				* m_payload_format.bytesPerBlock);
	RTPPacket pack(m_ssrc, (RTPSrc*)data, 0);

	pack.SetPayloadType(m_payload_type);
	// TODO: recheck size
	pack.AddPayloadData(buffer, 160);
	pack.SetSequence(m_tx_next.sequence);
	pack.SetTimestamp(m_tx_next.timestamp);
		
	if (PutPacket(pack) == false)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [RtpTransceiver::CreateAndSendPacket()] "
					"[PutPacket returned false]\n")));
	}
	
	Packets.push_back(pack);
	
	return;
}

/*
//void RtpTransceiver::AudioDataReceived(const Uint8* buffer)
void RtpTransceiver::AudioDataReceived(ostringstream& buffer)
{
#if _DEBUG
	cout << endl << "---[DEBUG] [RtpTransceiver::AudioDataReceived()]" << endl;
#endif

	int data[172] = {0};
	RTPPayloadInfo info;
	Uint32 needed(m_payload_format.blocksPerPacket
					* m_payload_format.bytesPerBlock);
	RTPPacket packet(m_ssrc, (RTPSrc*)data, 0);
	packet.SetPayloadType(m_payload_type);
		
	packet.ClearPayloadData();	
	
	string str = buffer.str();
	const Uint8* buf((Uint8*)str.c_str());	
	// TODO: Recheck				
	//packet.AddPayloadData(buf, 160);
	//Uint8* buff= const_cast<Uint8 *>(buffer);	
	//packet.AddPayloadData(buffer, 160);
	//for (int i = 0; i <160; ++i)
	//{
	//	buf[i]=2*buf[i];
	//}
	packet.AddPayloadData(buf, 160);	
	packet.SetSequence(m_tx_next.sequence);
	packet.SetTimestamp(m_tx_next.timestamp);		
	
	Packets2.push_back(packet);
	
	return;
}

void RtpTransceiver::CloseAudio()
{
#if _DEBUG
	cout << endl << "---[DEBUG] [RtpTransceiver::CloseAudio()]" << endl;
#endif
	
	if (m_audiostream_engine != NULL)
	{
		m_audiostream_engine->CloseAudio();
	}
 	
 	return;
}

void RtpTransceiver::CaptureAudio()
{
#if _DEBUG
	cout << endl << "---[DEBUG] [RtpTransceiver::CaptureAudio()]" << endl;
#endif
	
	Packets2.clear();
	
	if (m_audiostream_engine != NULL)
	{
		m_audiostream_engine->RecordData();
	}
 	
 	return;
}
*/
