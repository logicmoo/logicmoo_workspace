/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RtspRequest.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef RTSPREQUEST_HPP_
#define RTSPREQUEST_HPP_

#include "RtspUrl.hpp"
#include "RtspMessage.hpp"

#include <string>
#include <vector>

using namespace std;

enum RtspMethod { UNKNOWN = -1, SETUP, ANNOUNCE, TEARDOWN };

enum RtspHeaderFieldType { CONNECTION };

class RtspHeader
{
};

class RtspGeneralHeader : public RtspHeader
{
public:

private:

};

class RtspRequestHeader : public RtspHeader
{
};

class RtspTransportHeader : public RtspHeader
{
public:

	enum Mode { NONE = -2, UNKNOWN = -1, PLAY, RECORD }; // Cisco requires this

	RtspTransportHeader();
	~RtspTransportHeader();

	bool IsValid() const { return m_valid; }

	unsigned short ClientRtpPort() const { return m_client_rtp_port; }
	unsigned short ClientRtcpPort() const { return m_client_rtcp_port; }

	unsigned short ServerRtpPort() const { return m_server_rtp_port; }
	unsigned short ServerRtcpPort() const { return m_server_rtcp_port; }

	Mode GetMode() const { return m_mode; }
	void SetMode( Mode mode ) { m_mode = mode; }

	void Parse( const string& transport );

private:

	bool ParseField( const string& f );

	unsigned short m_client_rtp_port;
	unsigned short m_client_rtcp_port;
	unsigned short m_server_rtp_port;
	unsigned short m_server_rtcp_port;

	Mode m_mode;

	bool m_valid;
};

class RtspRequest : public RtspMessage
{
public:

	enum Method { UNKNOWN = -1, SETUP, ANNOUNCE, TEARDOWN, DESCRIBE };

	RtspRequest() :
	m_expected_responses(0)
    {
    }

	RtspMessage::Type GetType() const;

	void Clear();

	virtual void Parse(const string &req);
	void ParseRequestLine(const string& req);
	void Dump();

	void SetMethod(Method method) { m_method = method; }
	Method GetMethod() const { return m_method; }

	void SetUrl(const string& url) { m_uri.BuildFromString( url ); }
	RtspUrl GetUrl() const { return m_uri; }
    
	string const& GetTransport() const { return m_transport; }

	void ToString();

private:

	void ParseField(const string& field);

	Method     		 m_method;
	RtspUrl    		 m_uri;
	string			m_version;

	string			m_require;
	string			m_proxy_require;
	
	unsigned int	m_expected_responses;

	bool			m_valid;
};

#endif /*RTSPREQUEST_HPP_*/
