/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MrcpTransactionExecutor.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <iostream>
#include "MrcpTransactionExecutor.hpp"
#include "ace/Log_Msg.h"

using namespace std;

MrcpTransactionExecutor::MrcpTransactionExecutor(MrcpEngine* engine)
{
	ACE_TRACE(ACE_TEXT("[MrcpTransactionExecutor::MrcpTransactionExecutor()]"));

	m_engine = engine;
	
	return;
}

MrcpTransactionExecutor::~MrcpTransactionExecutor()
{
	ACE_TRACE(ACE_TEXT("[MrcpTransactionExecutor::~MrcpTransactionExecutor()]"));
	
	return;
}

void MrcpTransactionExecutor::ExecuteState()
{
	ACE_TRACE(ACE_TEXT("[MrcpTransactionExecutor::ExecuteState()]"));
	
	return;
}
