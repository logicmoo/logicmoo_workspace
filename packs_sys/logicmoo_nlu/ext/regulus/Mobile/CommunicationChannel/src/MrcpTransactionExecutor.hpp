/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MrcpTransactionExecutor.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef MRCPTRANSACTIONEXECUTOR_HPP_
#define MRCPTRANSACTIONEXECUTOR_HPP_

// Foraward declarations

class MrcpEngine;

class MrcpTransactionExecutor
{
public:

	MrcpTransactionExecutor(MrcpEngine* engine);
	~MrcpTransactionExecutor();
	
	void ExecuteState();

private:
	
	// Avoid accidental copy or assignment
	MrcpTransactionExecutor(const MrcpTransactionExecutor&);
	MrcpTransactionExecutor& operator = (const MrcpTransactionExecutor&);
	
private:

	MrcpEngine* m_engine;
};

#endif /*MRCPTRANSACTIONEXECUTOR_HPP_*/
