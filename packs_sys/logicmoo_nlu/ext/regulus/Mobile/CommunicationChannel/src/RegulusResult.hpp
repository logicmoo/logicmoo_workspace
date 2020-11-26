/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	RegulusResult.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#ifndef REGULUSRESULT_HPP_
#define REGULUSRESULT_HPP_

#include <string>

using namespace std;

class RegulusResult
{
public:

	enum ResultStatus { OK, ERROR, EMPTY };
	
	RegulusResult();
	RegulusResult(const string& input);
	virtual ~RegulusResult();
	
	void SetStatus(ResultStatus status);
	ResultStatus GetStatus();	

	virtual void ParseInput(const string& input);
	
	bool FindAndReplace(string& input_str,
						const string& search_str, 
						const string& replace_str);
	     
	bool ExtractOutput(string& buffer, 
	   					const string left, const string right);
		
private:
	
	// Avoid accidental copy or assignment
	RegulusResult(const RegulusResult&);
	RegulusResult& operator = (const RegulusResult&);
	
public:

	ResultStatus m_status;
};

#endif /*REGULUSRESULT_HPP_*/
