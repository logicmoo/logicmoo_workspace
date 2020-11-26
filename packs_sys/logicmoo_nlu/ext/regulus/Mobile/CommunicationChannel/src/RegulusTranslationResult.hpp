/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RegulusTranslationResult.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#ifndef REGULUSTRANSLATIONRESULT_HPP_
#define REGULUSTRANSLATIONRESULT_HPP_

#include <string>
#include "RegulusResult.hpp"

using namespace std;

class RegulusTranslationResult : public RegulusResult
{
public:

	RegulusTranslationResult();
	RegulusTranslationResult(const string& input);
	
	~RegulusTranslationResult();
		
	void SetSelected(const string& text);
	const string& GetSelected();
	
	void SetTranslation(const string& text);
	const string& GetTranslation();
	
	void SetTextTranslation(const string& text);
	const string& GetTextTranslation();
	
	void SetSourceLf(const string& text);
	const string& GetSourceLf();

	void SetTargetLf(const string& text);
	const string& GetTargetLf();
	
	void SetInterlingua(const string& text);
	const string& GetInterlingua();
	
	void ClearResult();
	void ParseInput(const string& input);

private:
	
	// Avoid accidental copy or assignment
	RegulusTranslationResult(const RegulusTranslationResult&);
	RegulusTranslationResult& operator = (const RegulusTranslationResult&);
	
private:
	
	string	m_selected;
	string	m_translation;
	string	m_text_translation;
	string	m_source_lf;
	string	m_target_lf;
	string	m_interlingua;
};

#endif /*REGULUSTRANSLATIONRESULT_HPP_*/
