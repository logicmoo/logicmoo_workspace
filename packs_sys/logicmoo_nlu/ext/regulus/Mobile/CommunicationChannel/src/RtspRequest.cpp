/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RtspRequest.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <vector>
#include <iostream>
#include <sstream>
#include "RtspRequest.hpp"
#include "ace/Log_Msg.h"

using namespace std;

// requires <locale>
static void StringToLower(string& s)
{
    for (string::size_type i = 0; i < s.size(); ++i)
    {
		s[i] = tolower(s[i]);
    }
    
    return;
}

// unused?
/*
static string GetLine(const string& s, string::size_type start, string::size_type& end)
{
    string::size_type index;

    if (((index = s.find("\r\n", start)) == string::npos) &&
    	((index = s.find("\n", start)) == string::npos))
    {
		return string("");
    }

    end = index - 1;

    return s.substr(start, end - start + 1);
}*/

RtspMessage::Type RtspRequest::GetType() const
{
	ACE_TRACE(ACE_TEXT("[RtspRequest::GetType()]"));
	
    return RtspMessage::Request;
}

void RtspRequest::Clear()
{
	ACE_TRACE(ACE_TEXT("[RtspRequest::Clear()]"));
		
    m_method = UNKNOWN;
    m_uri.Clear();
    m_version.erase();

    m_connection.erase();
    m_content_encoding.erase();
    m_content_language.erase();
    m_content_length = 0;
    m_content_type.erase();
    m_cseq = 0;
    m_proxy_require.erase();
    m_require.erase();
    m_session.erase();
    m_transport.erase();

    m_entity_body.erase();

    m_valid = false;
    
    return;
}

void RtspRequest::ParseRequestLine(const string& request_line)
{
	ACE_TRACE(ACE_TEXT("[RtspRequest::ParseRequestLine()]"));

    string method;
    string uri;
    string version;

    string::size_type index;
    unsigned start;

// ETK    // ETK detailed_info(func_name, "\n");

    index = request_line.find(' ');
    
    if (index == string::npos)
    {
		return;
	}

    method = request_line.substr(0, index);

    // ETK // ETK detailed_info( func_name, "method = '%s'\n", method.c_str() );

    if (method == "SETUP")
    {
		m_method = SETUP;
	}
    else if (method == "ANNOUNCE")
    {
		m_method = ANNOUNCE;
	}
    else if (method == "TEARDOWN")
    {
		m_method = TEARDOWN;
	}
    else if (method == "DESCRIBE")
    {
		m_method = DESCRIBE;
    }
    else
    {
		m_method = UNKNOWN;
    }

    start = request_line.find_first_not_of(" \t", index);
    index = request_line.find_first_of(' ', start);
    
    if (index == string::npos)
    {
		return;
    }

	uri = request_line.substr(start, index - start);

	// ETK // ETK detailed_info( func_name, "uri = '%s'\n", uri.c_str() );

	start = request_line.find_first_not_of(' ', index);
//	cout << "start = " << start << endl;
    
	if (start == string::npos)
	{
		return;
	}
	
#if 0 // end-of-line has already been trimmed
    index = request_line.find("\r\n", start);
    
    if (index == string::npos)
    {
		index = request_line.find("\n", start);
    }
    
//	cout << "index = " << index << endl;
    
    if (index == string::npos)
	{	
		return;
	}
	
#endif

    version = request_line.substr(start, index - start);

    // ETK // ETK detailed_info(func_name, "version = '%s'\n", version.c_str());

    m_version = version;

    // ETK // ETK detailed_info(func_name, "m_method = %d\n", m_method);

    m_uri.BuildFromString(uri);

    if ((m_method == UNKNOWN) || (!m_uri.IsValid()) || (m_version != "RTSP/1.0"))
	{	
		return;
	}
	
	return;
}

void RtspRequest::Parse(const string& req)
{
#if _DEBUG_1
	ACE_TRACE(ACE_TEXT("[RtspRequest::Parse()]"));
#endif
	
	istringstream is(req);

	string line;
    string field;

    m_valid = false;

    // read request line

    getline(is, line);
    
    if (line[line.size() - 1] == '\r')
    {
		line = line.substr(0, line.size() - 1);
	}

    // ETK // ETK detailed_info(func_name, "request line = '%s'\n", line.c_str());
    ParseRequestLine(line);

    // read header fields

    while (!is.eof())
    {
		getline(is, line);

		if (!line.size())
		{
	    	break;
	    }

		if (line[line.size() - 1] == '\r')
		{
		    line = line.substr(0, line.size() - 1);
		}
	
		// ETK // ETK detailed_info(func_name, "line = '%s'\n", line.c_str());

		if ((line[0] == ' ') || (line[0] == '\t'))
		{
		    field += line;
		}
		else
		{
		    if (field.size())
		    {
				ParseField(field);
				// ETK // ETK detailed_info(func_name, "field = '%s'\n", field.c_str());
		    }
		    
		    field = line;
		}
    }
    
    if (field.size())
    {
		ParseField(field);
		// ETK // ETK detailed_info(func_name, "field = '%s'\n", field.c_str());
    }
    
    return;
}

void RtspRequest::ParseField(const string& field)
{
	ACE_TRACE(ACE_TEXT("[RtspRequest::ParseField()]"));

    istringstream is(field);

    string field_name;
    string field_value;

    getline(is, field_name, ':');

    StringToLower(field_name);
    // ETK // ETK detailed_info(func_name, "field_name = '%s'\n", field_name.c_str());

    getline(is, field_value);
    
    while ((field_value.size()) && 
    	((field_value[0] == ' ') || (field_value[0] == '\t')))
    {
		field_value.erase(field_value.begin());
	}

    // ETK // ETK detailed_info(func_name, "field_value = '%s'\n", field_value.c_str());

    // should only care about proxy-require and require

    if (field_name == "proxy-require")
    {
        m_proxy_require = field_value;
	}
    else if (field_name == "require")
    {
        if (m_require.size())
        {
            m_require += "," + field_value;
        }
        else
        {
            m_require = field_value;
		}
    }
    else
    {
		RtspMessage::ParseField(field); // should return bool??
    }

#if 0 // moved to RtspMessage::ParseField
    if (field_name == "connection")
    {
		m_connection = field_value;
	}
    else if (field_name == "content-encoding")
    {
		m_content_encoding = field_value;
	}
    else if (field_name == "content-language")
    {
		m_content_language = field_value;
	}
    else if (field_name == "content-length")
    {
		m_content_length = atoi(field_value.c_str());
	}
    else if (field_name == "content-type")
    {
		m_content_type = field_value;
	}
    else if (field_name == "cseq")
    {
		m_cseq = atoi(field_value.c_str());
	}
    else if (field_name == "proxy-require")
    {
        m_proxy_require = field_value;
	}
    else if (field_name == "require")
    {
        if (m_require.size())
        {
            m_require += "," + field_value;
		}
        else
        {
            m_require = field_value;
        }
    }
    else if (field_name == "session")
    {
		m_session = field_value;
	}
    else if (field_name == "transport")
    {
//		cout << "transport field  m_transport.size() = " << m_transport.size() << "'" << m_transport.c_str() << "'" << endl;
		
		if (m_transport.size())
	    {
	    	m_transport += "," + field_value;
	    }
		else
		{
	    	m_transport = field_value;
	    }
    }
#endif

	return;
}

void RtspRequest::Dump()
{
	ACE_TRACE(ACE_TEXT("[RtspRequest::Dump()]"));
    
    if (m_connection.size())
    {
	// ETK // ETK detailed_info(func_name, "Connection: %s\n", m_connection.c_str());
    }

    if (m_content_encoding.size())
    {
	// ETK // ETK detailed_info(func_name, "Content-Encoding: %s\n", m_content_encoding.c_str());
    }

    if (m_content_language.size())
    {
	// ETK // ETK detailed_info(func_name, "Content-Language: %s\n", m_content_language.c_str());
    }
    
    if (m_content_length)
    {
	// ETK // ETK detailed_info(func_name, "Content-Length: %d\n", m_content_length);
    }
    
    if (m_content_type.size())
	{
	// ETK // ETK detailed_info(func_name, "Content-Type: %s\n", m_content_type.c_str());
	}

    if (m_cseq)
    {
	// ETK // ETK detailed_info(func_name, "CSeq: %d\n", m_cseq);
    }
    
    if (m_proxy_require.size())
    {
        // ETK // ETK detailed_info(func_name, "Proxy-Require: %s\n", m_proxy_require.c_str());
    }
    
    if (m_require.size())
    {
        // ETK // ETK detailed_info(func_name, "Require: %s\n", m_require.c_str());
    }
    
    if (m_session.size())
    {
	// ETK // ETK detailed_info(func_name, "Session: %s\n", m_session.c_str());
    }
    
    if (m_transport.size())
    {
	// ETK // ETK detailed_info( func_name, "Transport: %s\n", m_transport.c_str() );
    }
	;
	
	return;
}

RtspTransportHeader::RtspTransportHeader() :
    m_client_rtp_port(0),
    m_client_rtcp_port(0),
    m_server_rtp_port(0),
    m_server_rtcp_port(0),
    m_mode(NONE),
    m_valid(false)
{
	ACE_TRACE(ACE_TEXT("[RtspTransportHeader::RtspTransportHeader()]"));
	
	return;
}

RtspTransportHeader::~RtspTransportHeader()
{
	ACE_TRACE(ACE_TEXT("[RtspTransportHeader::~RtspTransportHeader()]"));
		
	return;
}

void RtspTransportHeader::Parse(const string& transport)
{
	ACE_TRACE(ACE_TEXT("[RtspTransportHeader::Parse()]"));
		
    // ETK // ETK detailed_info(func_name, "transport = '%s'\n", transport.c_str());
    istringstream is(transport);

    string single_transport;

    getline(is, single_transport, ',');

    while (!single_transport.empty())
    {
		// ETK // ETK detailed_info(func_name, "Transport: %s\n", single_transport.c_str());
	
		istringstream ts( single_transport );

		string transport_spec, delivery;

		getline(ts, transport_spec, ';');
		// ETK // ETK detailed_info(func_name, "transport-spec: '%s'\n", transport_spec.c_str());

		// TODO: case-insensitive compare or ToLower for transport field value
		if ((transport_spec != "RTP/AVP") && (transport_spec != "RTP/AVP/UDP") &&
	    	(transport_spec != "rtp/avp") && (transport_spec != "rtp/avp/udp"))
		{
	    	// ETK detailed_info(func_name, "ERROR : invalid tranport_spec\n");
	    	return;
		}

		getline(ts, delivery, ';');
		// ETK detailed_info(func_name, "delivery mode: '%s'\n", delivery.c_str());

		if (delivery != "unicast")
		{
	    	// ETK detailed_info(func_name, "ERROR : unsupported delivery mode: '%s'\n",
			// ETK			      delivery.c_str() );
	    	return;
		}

		string field;
		getline(ts, field, ';');
	
		while (!field.empty())
		{
#if 0
	    	string::size_type idx;

//			cout << "original field = '" << field.c_str() << "'" << endl;
		    idx = field.find_first_not_of(" \t");
//			cout << "start idx = " << idx << endl;
	    
	    	if (idx != string::npos)
	    	{
				field = field.erase(0, idx);
	    	}

	    	idx = field.find_last_not_of(" \t");
//			cout << "end idx = " << idx << endl;
	    
	    	if (idx != string::npos)
	    	{
				field = field.erase(idx + 1);
	    	}
#endif
	    	// ETK detailed_info(func_name, "field = '%s'\n", field.c_str());
	    	ParseField(field);

#if 0
	    	string field_name, field_value;

	    	idx = field.find('=');
	    
	    	if (idx != string::npos)
	    	{
				field_name = field.substr(0, idx);
				field_value = field.substr(idx + 1);
	    	}

//	    	cout << "field_name = '" << field_name.c_str() << "'" << endl;
//	    	cout << "field_value = '" << field_value.c_str() << "'" << endl;
#endif

	    	field.erase();

	    	getline(ts, field, ';');
		}

		single_transport.erase();
		getline(is, single_transport, ',');
    }
    
    return;
}

bool RtspTransportHeader::ParseField(const string& f)
{
	ACE_TRACE(ACE_TEXT("[RtspRequest::ParseField()]"));
	
    string field(f);
    string::size_type idx;

    // ETK detailed_info(func_name, "original field = '%s'\n", field.c_str());
    idx = field.find_first_not_of(" \t");
    // printf("start idx = %d\n", idx);
    
    if (idx != string::npos)
    {
		field.erase(0, idx);
    }
    
    idx = field.find_last_not_of(" \t");
    // printf("end idx = %d\n", idx);
    
    if (idx != string::npos)
    {
		field.erase(idx + 1);
    }
    
    string field_name, field_value;
    
    idx = field.find('=');
    
    if (idx != string::npos)
    {
		field_name = field.substr(0, idx);
		field_value = field.substr(idx + 1);
    }
    
    // ETK detailed_info(func_name, "field_name = '%s'\n", field_name.c_str());
    // ETK detailed_info(func_name, "field_value = '%s'\n", field_value.c_str());

    // the only incoming parameter that we support is client_port
    if (field_name == "client_port")
    {
		istringstream is( field_value );
		unsigned short rtp_port, rtcp_port;
		char dash;
		string extra;

		//printf("is.eof() = %d\n", is.eof());

		is >> rtp_port;

		if (!is.eof())
		{
	    	is >> dash >> rtcp_port;
	    	if ((!is.fail()) && (dash == '-'))
	    	{
				m_client_rtp_port = rtp_port;
				m_client_rtcp_port = rtcp_port;
				m_valid = true;
	    	}
		}
		else
		{
	    	m_client_rtp_port = rtp_port;
	    	m_valid = true;
		}

		// ETK detailed_info(func_name, "rtp_port = %d rtcp_port = %d dash = '%c'\n",
		// ETK		  rtp_port, rtcp_port, dash);

		// ETK detailed_info(func_name, "m_client_rtp_port = %d\n", m_client_rtp_port);

		//printf("is.eof() = %d\n", is.eof());
		//printf("is.fail() = %d\n", is.fail());

#if 0
		if ((!is.fail()) && (dash == '-'))
		{
	    	m_client_rtp_port = rtp_port;
	    	m_client_rtcp_port = rtcp_port;
	    	m_valid = true;
		}
#endif
    }
    else if (field_name == "server_port") // minimal support added for client (needs server_port)
    {
		istringstream is(field_value);
		unsigned short rtp_port, rtcp_port;
		char dash;
		string extra;

		is >> rtp_port;

		if (!is.eof())
		{
	    	is >> dash >> rtcp_port;
	    	
	    	if ((!is.fail()) && (dash == '-'))
	    	{
				m_server_rtp_port = rtp_port;
				m_server_rtcp_port = rtcp_port;
				m_valid = true;
	    	}
		}
		else
		{
	    	m_server_rtp_port = rtp_port;
	    	m_valid = true;
		}

		// ETK detailed_info(func_name, "rtp_port = %d rtcp_port = %d dash = '%c'\n",
		// ETK		  rtp_port, rtcp_port, dash);

		// ETK detailed_info(func_name, "m_server_rtp_port = %d\n", m_server_rtp_port);
    }
    else if (field_name == "mode")
    {
		if (field_value == "record")
		{
		    m_mode = RECORD;
		}
		else if (field_value == "play")
	    {
	    	m_mode = PLAY;
	    }
		else
		{
	    	m_mode = UNKNOWN;
		}
    }
    
    return true;
}

static const string SP(" ");
static const string CRLF("\r\n");

void RtspRequest::ToString()
{
	ACE_TRACE(ACE_TEXT("[RtspRequest::ToString()]"));
	
    // ETK detailed_info(func_name, "\n");

    ostringstream os;
    string method;

    switch (m_method)
    {
    case SETUP:
		method = "SETUP";
		break;
    case ANNOUNCE:
		method = "ANNOUNCE";
		break;
    case TEARDOWN:
		method = "TEARDOWN";
		break;
    case DESCRIBE:
		method = "DESCRIBE";
		break;
    default:
		method = "UNKNOWN";
		break;
    }

    os << method << SP << m_uri.GetUrl() << SP << "RTSP/1.0" << CRLF;

    os << "CSeq: " << m_cseq << CRLF;
    os << "Session: " << m_session << CRLF;

    if (m_entity_body.size())
    {
		os << "Content-Length: " << m_entity_body.size() << CRLF;
		os << "Content-Type: " << m_content_type << CRLF;
		os << CRLF;
    
		os << m_entity_body;
    }
    else
    {
		os << CRLF;
    }

    // ETK detailed_info(func_name, "response : '%s'\n", os.str().c_str());

    m_output_stream = os.str();
    
    return;
}
