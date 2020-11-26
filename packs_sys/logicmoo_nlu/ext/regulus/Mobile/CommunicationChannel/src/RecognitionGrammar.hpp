/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RecognitionGrammar.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef RECOGNITIONGRAMMAR_HPP_
#define RECOGNITIONGRAMMAR_HPP_

#include <iostream>
#include <string>

using namespace std;

// Class containg all the information of a recognition grammar
class RecognitionGrammar
{
public:

	RecognitionGrammar();
		
	RecognitionGrammar(string const& grammar, string const& content_id, 
						string const& content_type, string const& parameters);
		
	~RecognitionGrammar(void);
	
	void SetGrammar(const string& input);
	const string& GetGrammar();
	
	void SetContentId(const string& input);
	const string& GetContentId();
	
	void SetContentType(const string& input);
	const string& GetContentType();
	
	void SetParameters(const string& input);
	const string& GetParameters();
	
private:
	
	string m_grammar;
	string m_content_id; 
	string m_content_type;
	string m_parameters;
};

#endif /*RECOGNITIONGRAMMAR_HPP_*/
