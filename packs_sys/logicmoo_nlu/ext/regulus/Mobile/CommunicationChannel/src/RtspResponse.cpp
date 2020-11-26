/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RtspResponse.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <iostream>
#include <sstream>
#include "RtspResponse.hpp"
#include "ace/Log_Msg.h"

using namespace std;

class ReasonMap : public map<RtspResponse::StatusCode, string>
{
public:
    ReasonMap()
    {
	(*this) [RtspResponse::Continue] = "Continue";
	(*this) [RtspResponse::OK] = "OK";
	(*this) [RtspResponse::Created] = "Created";
	(*this) [RtspResponse::LowOnStorageSpace] = "Low on Storage Space";
	(*this) [RtspResponse::MultipleChoices] = "Multiple Choices";
	(*this) [RtspResponse::MovedPermanently] = "Moved Permanently";
	(*this) [RtspResponse::MovedTemporarily] = "Moved Temporarily";
	(*this) [RtspResponse::SeeOther] = "See Other";
	(*this) [RtspResponse::NotModified] = "Not Modified";
	(*this) [RtspResponse::UseProxy] = "Use Proxy";
	(*this) [RtspResponse::BadRequest] = "Bad Request";
	(*this) [RtspResponse::Unauthorized] = "Unauthorized";
	(*this) [RtspResponse::PaymentRequired] = "Payment Required";
	(*this) [RtspResponse::Forbidden] = "Forbidden";
	(*this) [RtspResponse::NotFound] = "Not Found";
	(*this) [RtspResponse::MethodNotAllowed] = "Method Not Allowed";
	(*this) [RtspResponse::NotAcceptable] = "Not Acceptable";
	(*this) [RtspResponse::ProxyAuthenticationRequired] = "Proxy Authentication Required";
	(*this) [RtspResponse::RequestTimeOut] = "Request Time-out";
	(*this) [RtspResponse::Gone] = "Gone";
	(*this) [RtspResponse::LengthRequired] = "Length Required";
	(*this) [RtspResponse::PreconditionFailed] = "Precondition Failed";
	(*this) [RtspResponse::RequestEntityTooLarge] = "Request Entity Too Large";
	(*this) [RtspResponse::RequestURITooLarge] = "Request-URI Too Large";
	(*this) [RtspResponse::UnsupportedMediaType] = "Unsupported Media Type";
	(*this) [RtspResponse::ParameterNotUnderstood] = "Parameter Not Understood";
	(*this) [RtspResponse::ConferenceNotFound] = "Conference Not Found";
	(*this) [RtspResponse::NotEnoughBandwidth] = "Not Enough Bandwidth";
	(*this) [RtspResponse::SessionNotFound] = "Session Not Found";
	(*this) [RtspResponse::MethodNotValidInThisState] = "Method Not Valid in This State";
	(*this) [RtspResponse::HeaderFieldNotValidForResource] = "Header Field Not VAlid for Resource";
	(*this) [RtspResponse::InvalidRange] = "Invalid Range";
	(*this) [RtspResponse::ParameterIsReadOnly] = "Parameter Is Read-Only";
	(*this) [RtspResponse::AggregateOperationNotAllowed] = "Aggregate Operation Not Allowed";
	(*this) [RtspResponse::OnlyAggregateOperationAllowed] = "Only Aggregate Operation Allowed";
	(*this) [RtspResponse::UnsupportedTransport] = "Unsupported Transport";
	(*this) [RtspResponse::DestinationUnreachable] = "Destination Unreachable";
	(*this) [RtspResponse::InternalServerError] = "Internal Server Error";
	(*this) [RtspResponse::NotImplemented] = "Not Implemented";
	(*this) [RtspResponse::BadGateway] = "Bad Gateway";
	(*this) [RtspResponse::ServiceUnavailable] = "Service Unavailable";
	(*this) [RtspResponse::GatewayTimeOut] = "Gateway Time-out";
	(*this) [RtspResponse::RtspVersionNotSupported] = "RTSP Version Not Supported";
	(*this) [RtspResponse::OptionNotSupported] = "OPtion Not Supported";

    }
};

//map<RtspResponse::StatusCode, string> RtspResponse::m_reason_phrase = RtspResponse::Populate();
map<RtspResponse::StatusCode, string> RtspResponse::m_reason = ReasonMap();

map<RtspResponse::StatusCode, string> RtspResponse::Populate()
{
	ACE_TRACE(ACE_TEXT("[RtspResponse::Populate()]"));
	
    map<RtspResponse::StatusCode, string> local_map;

    local_map [Continue] = "Continue";
    local_map [OK] = "OK";

    return local_map;
}

RtspResponse::RtspResponse() : 
    m_version( "RTSP/1.0" )
{
	ACE_TRACE(ACE_TEXT("[RtspResponse::RtspResponse()]"));
	
	return;
}

RtspResponse::~RtspResponse()
{
	ACE_TRACE(ACE_TEXT("[RtspResponse::~RtspResponse()]"));
	
	return;
}

RtspMessage::Type RtspResponse::GetType() const
{
	ACE_TRACE(ACE_TEXT("[RtspResponse::GetType()]"));
	
    return RtspMessage::Response;
}

void RtspResponse::ParseResponseLine(const string& resp)
{
	ACE_TRACE(ACE_TEXT("[RtspResponse::ParseResponseLine()]"));
	
    string version;
    unsigned short status_code; 
    
    istringstream is(resp);

    is >> version >> status_code;

    // ETK detailed_info(func_name, "fail:%d eof=%d\n", is.fail(), is.eof());

    if (version != "RTSP/1.0")
    {
	// ETK warning(func_name, "invalid RTSP version in response : %s\n", version.c_str());
    }

    if ((status_code < 100) || (status_code >= 600))
    {
	// ETK warning(func_name, "invalid status code in response : %d\n", status_code);
    }
    
    return;
}

void RtspResponse::Parse(const string& resp)
{
	ACE_TRACE(ACE_TEXT("[RtspResponse::Parse()]"));
		
    // ETK detailed_info(func_name, "\n");

    istringstream is(resp);

    string line;
    string field;

    m_valid = false;

    getline(is, line);
    
    if (line[line.size() - 1] == '\r')
    {
		line = line.substr(0, line.size() - 1);
	}

    // ETK detailed_info(func_name, "response line = '%s'\n", line.c_str());
    ParseResponseLine(line);

    // read header fields

    while (!is.eof())
    {
		getline(is, line);

		if (!line.size())
		{
	    	break;
	    }

		if (line [line.size() - 1] == '\r')
		{
	    	line = line.substr(0, line.size() - 1);
	    }

		// ETK detailed_info(func_name, "line = '%s'\n", line.c_str());

		if ((line [0] == ' ') || (line [0] == '\t'))
		{
	    	field += line;
		}
		else
		{
	    	if (field.size())
	    	{
				ParseField(field);
			// ETK detailed_info(func_name, "field = '%s'\n", field.c_str());
	    	}
	    	
	    	field = line;
		}
    }

    if (field.size())
    {
		ParseField(field);
		// ETK detailed_info("field = '%s'\n", field.c_str());
    }
    
    return;
}

void RtspResponse::SetStatusCode(StatusCode status)
{
	ACE_TRACE(ACE_TEXT("[RtspResponse::SetStatusCode()]"));
	
    m_status_code = status;
    m_reason_phrase = m_reason.count( status ) ? m_reason [status] : string("UNKNOWN STATUS");
    
    return;
}

string RtspResponse::GetStatusLineAsString()
{
	ACE_TRACE(ACE_TEXT("[RtspResponse::GetStatusLineAsString()]"));
	
    ostringstream os;
    os << m_version << " " << m_status_code << " " << m_reason_phrase << "\r" << endl;
    
    return os.str();
}

static const string SP(" ");
static const string CRLF("\r\n");

void RtspResponse::ToString()
{
	ACE_TRACE(ACE_TEXT("[RtspResponse::ToString()]"));
	
    // ETK detailed_info( func_name, "\n" );

    ostringstream os;

    // ETK detailed_info( func_name, "m_session = %s\n", m_session.c_str() );

    os << "RTSP/1.0" << SP << m_status_code << SP << m_reason [m_status_code] << CRLF;
    os << "CSeq: " << m_cseq << CRLF;
    
    if (m_session.size())
    {
		os << "Session: " << m_session << CRLF;
	}

    if (m_transport.size())
    {
		os << "Transport: " << m_transport << CRLF;
    }

    if (m_content_encoding.size())
    {
		os << "Content-Encoding: " << m_content_encoding << CRLF;
	}
	
    if (m_content_language.size())
	{
		os << "Content-Language: " << m_content_language << CRLF;
	}
    
    if (m_content_length)
    {
		os << "Content-Length: " << m_content_length << CRLF;
    }
    
    if (m_content_type.size())
    {
		os << "Content-Type: " << m_content_type << CRLF;
    }
    
    os << CRLF;

    if (m_entity_body.size())
    {
		os << m_entity_body;
    }

    // ETK detailed_info(func_name, "response : '%s'\n", os.str().c_str());

	m_output_stream = os.str();
	
	return;
}
