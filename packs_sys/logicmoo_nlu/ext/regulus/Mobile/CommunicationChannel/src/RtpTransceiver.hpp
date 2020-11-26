/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RtpTransceiver.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef RTPTRANSCEIVER_HPP_
#define RTPTRANSCEIVER_HPP_

#include <string>
#include <list>
#include "RtpTransceiverObserver.hpp"
#include "SocketsEngineObserver.hpp"
#include "SocketsEngine.hpp"
#include "UdpSocket.hpp"
#include "RtpPacket.hpp"
#include "NtpTime.hpp"
#include "UtilsBuffer.hpp"
#include "UtilsListBuffer.hpp"
#include "SDL_net.h"

class RtpTransceiver : public SocketsEngineObserver
{
public:

	RtpTransceiver(RtpTransceiverObserver& observer);	
					
	~RtpTransceiver();

	// Set the name of the Rtp Server
	void SetServerName(const string name);
	// Get the name of the Rtp Server
	const string GetServerName();
	// Set the port number of the Rtp Server
	void SetServerPort(int port);
	// Get the port number of the Rtp Server
	int GetServerPort();
	// Set the local rtp port number
	void SetLocalRtpPort(int port);
	// Get the local rtp port number
	int GetLocalRtpPort();
	
	// Methods provided by the 'RTPTransceiver' class.

	void Close(void);
	
	void Connect(void);
	void Listen(Uint32 port);

	Uint32 Flush(bool force);

	bool GetPacket(void);
	bool PutPacket(const RTPPacket& packet);

//	void SetHostname(const string& addr);
//	string GetHostname(void) const;

//	void SetRemotePort(Uint32 port);
//	Uint32 GetRemotePort(void) const;

//	void SetAddress(const IPAddress& addr);
//	IPAddress GetAddress(void) const;

	bool IsConnected(void) const;

	void SetLocalPort(Uint32 port);
	Uint32 GetLocalPort(void) const;

	bool IsListening(void) const;

	void SetTimeout(Uint32 msecs);
	Uint32 GetTimeout(void) const;

//	void SetFilter(RTPPacketFilter* filter);
//	RTPPacketFilter* GetFilter(void) const;

	void SetRxBufferSize(Uint32 value);
	Uint32 GetRxBufferSize(void) const;

	Uint32 GetRxOctets(void) const;
	
	RTPStats GetRxStats(void) const;
	void ClearRxStats(void);

	void SetTxBufferSize(Uint32 value);

	Uint32 GetTxBufferSize(void) const;

	Uint32 GetTxOctets(void) const;
	RTPStats GetTxStats(void) const;

	void ClearTxStats(void);
	
	void SetSyncSource(RTPSrc ssrc);
	RTPSrc GetSyncSource(void) const;

	void SetCntrSources(const RTPSrc* csrc_tab,
						Uint32 csrc_num);

	Uint32 GetCntrSources(RTPSrc* csrc_tab,
							Uint32 csrc_num) const;

	void SetPayloadType(RTPPayloadType type);
	RTPPayloadType GetPayloadType(void) const;

	void SetPayloadFormat(RTPPayloadFormat format);
	RTPPayloadFormat GetPayloadFormat(void) const;

	// Connect to the Udp peer
	void StartComms();
	// Disconnect from the Udp peer
	void StopComms();
	// From SocketsEngineObserver 
	// Process the received packets from the Udp peer
	void MessageReceived(ostringstream& buffer);

	void SendPackets();
	
	void ClearPacketsBuffer();
	
	void CreateAndSendPacket(const Uint8* buffer);
	
private:
	
	// Avoid accidental copy or assignment
	RtpTransceiver(const RtpTransceiver&);
	RtpTransceiver& operator = (const RtpTransceiver&);
	
	/*RtpTransceiver(const RtpTransceiver&):m_observer(this->m_observer)
	{ return;}
	
	RtpTransceiver& operator = (const RtpTransceiver&)
	{ return *this; }*/

	// Private type definitions.

	struct RTPPayloadInfo
	{
		Uint32	sequence;	// sequence increment
		Uint32	timestamp;	// duration in payload dependent units
	};

	typedef Buffer<RTPSrc> RTPSrcArray;
	typedef ListBuffer<Uint8, 2048> RTPDataBuffer;

	// Private methods.

	void CloneState(void);
	bool HasRxStats(void) const;
	bool HasTxStats(void) const;
	void ResetTxStats(void);

	void EvalPayloadInfo(RTPPayloadType type,
						 Uint32 octets,
						 RTPPayloadInfo& info);

	Uint32 SendPacket(const RTPPacket& packet,
							 const RTPPayloadInfo& info);

	Uint32 RecvPacket(string buffer);

	void DropPayload(Uint32 octets,
					 const RTPPayloadInfo& info);
		
	// Private attributes.

	RtpTransceiverObserver& m_observer;
//	RTPPacketFilter*	m_filter;

	RTPSrc				m_ssrc;
	RTPSrcArray			m_csrcs;
	RTPPayloadType		m_payload_type;
	RTPPayloadFormat	m_payload_format;

	RTPStats		m_rx_stats;
	Uint32			m_rx_limit;
	long			m_rx_transit;
	RTPDataBuffer	m_rx_data;
	
	RTPStats		m_tx_stats;
	RTPTimeMetrics	m_tx_next;
	Uint32			m_tx_limit;
	long			m_tx_transit;
	RTPDataBuffer	m_tx_data;
	
	string	m_server;
	int		m_local_port;
	int		m_remote_port;
	
	string	m_socket_buf;
	SocketsEngine*	m_socket_engine;
};

#endif /*RTPTRANSCEIVER_HPP_*/
