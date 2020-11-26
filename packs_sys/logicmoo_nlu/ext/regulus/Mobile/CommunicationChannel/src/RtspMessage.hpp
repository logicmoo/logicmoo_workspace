/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RtspMessage.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef RTSPMESSAGE_HPP_
#define RTSPMESSAGE_HPP_

#include <string>

using namespace std;

class RtspMessage
{
public:

	enum Type {Request, Response};

	RtspMessage() : m_cseq( 0 ), m_content_length( 0 ) {}
	virtual ~RtspMessage();

	virtual Type GetType() const = 0;
	virtual void Parse(const string& msg) = 0;

	virtual void ParseField(const string& field);

	unsigned int GetCSeq() const {return m_cseq;}
	void SetCSeq(unsigned int cseq) { m_cseq = cseq; }

	string const &GetSession() const { return m_session; }
	void SetSession(string const &session) { m_session = session; }

	string const &GetTransport() const { return m_transport; }
	void SetTransport(string const &trans) { m_transport = trans; }

	string const &GetContentEncoding() const { return m_content_encoding; }
	void SetContentEncoding(string const &enc) { m_content_encoding = enc; }

	string const &GetContentLanguage() const { return m_content_language; }
	void SetContentLanguage(string const &lang) { m_content_language = lang; }

	bool GetContentLength(size_t& content_length) const;
	void SetContentLength(size_t content_length);

	string const &GetContentType() const { return m_content_type; }
	void SetContentType(string const &type) { m_content_type = type; }
   
	string const& GetOutputStream() const { return m_output_stream; }

	void SetBody(string const& body);
	string const& GetBody() const { return m_entity_body; }

protected:

	// general headers
	unsigned int	m_cseq;
	string			m_connection;
	
	// request-response headers
	string  m_session;
	string  m_transport; // this one may have to be parsed

	// entity headers
	unsigned int	m_content_length;
	string			m_content_encoding;
	string			m_content_language;
	string			m_content_type;
	string			m_entity_body;
	string			m_output_stream; // when it needs to be streamed out (i.e. to client)
};

#endif /*RTSPMESSAGE_HPP_*/
