/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RtspResponse.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef RTSPRESPONSE_HPP_
#define RTSPRESPONSE_HPP_

#include <map>
#include <string>

#include "RtspMessage.hpp"

using namespace std;

class RtspResponse : public RtspMessage
{
public:

	enum StatusCode
	{
		Continue                       = 100,
		OK                             = 200,
		Created                        = 201,
		LowOnStorageSpace              = 250,
		MultipleChoices                = 300,
		MovedPermanently               = 301,
		MovedTemporarily               = 302,
		SeeOther                       = 303,
		NotModified                    = 304,
		UseProxy                       = 305,
		BadRequest                     = 400,
		Unauthorized                   = 401,
		PaymentRequired                = 402,
		Forbidden                      = 403,
		NotFound                       = 404,
		MethodNotAllowed               = 405,
		NotAcceptable                  = 406,
		ProxyAuthenticationRequired    = 407,
		RequestTimeOut                 = 408,
		Gone                           = 410,
		LengthRequired                 = 411,
		PreconditionFailed             = 412,
		RequestEntityTooLarge          = 413,
		RequestURITooLarge             = 414,
		UnsupportedMediaType           = 415,
		ParameterNotUnderstood         = 451,
		ConferenceNotFound             = 452,
		NotEnoughBandwidth             = 453,
		SessionNotFound                = 454,
		MethodNotValidInThisState      = 455,
		HeaderFieldNotValidForResource = 456,
		InvalidRange                   = 457,
		ParameterIsReadOnly            = 458,
		AggregateOperationNotAllowed   = 459,
		OnlyAggregateOperationAllowed  = 460,
		UnsupportedTransport           = 461,
		DestinationUnreachable         = 462,
		InternalServerError            = 500,
		NotImplemented                 = 501,
		BadGateway                     = 502,
		ServiceUnavailable             = 503,
		GatewayTimeOut                 = 504,
		RtspVersionNotSupported        = 505,
		OptionNotSupported             = 551
	};

	RtspResponse();
	
	~RtspResponse();

	RtspMessage::Type GetType() const;

	void Parse(const string& resp);
	void ParseResponseLine(const string& line);

	void SetStatusCode(StatusCode status);

	string GetStatusLineAsString();

	void ToString();

private:

	static map<StatusCode, string> m_reason;
	static map<StatusCode, string> Populate(); // still used???

	string		m_unsupported;

	string		m_version;
	StatusCode	m_status_code;
	string		m_reason_phrase;

	bool		m_valid;
};

#endif /*RTSPRESPONSE_HPP_*/
