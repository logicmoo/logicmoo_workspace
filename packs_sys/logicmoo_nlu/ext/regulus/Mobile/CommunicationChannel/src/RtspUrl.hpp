/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RtspUrl.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef RTSPURL_HPP_
#define RTSPURL_HPP_

#include <string>

using namespace std;

class RtspUrl
{
public:

	RtspUrl() : m_valid(false) {}
	RtspUrl(const string& url);

	void Clear();
    
	void BuildFromString(const string& url);

	string const &GetUrl() const { return m_url; }
	string const &GetAbsPath() const { return m_abs_path; }

	bool IsValid() const { return m_valid; }

private:

	string	m_url;
	string	m_protocol;
	string	m_host;
	string	m_abs_path;
	short 	m_port;
	bool	m_valid;
};

#endif /*RTSPURL_HPP_*/
