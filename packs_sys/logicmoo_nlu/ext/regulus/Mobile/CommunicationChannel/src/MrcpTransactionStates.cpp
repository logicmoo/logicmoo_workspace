/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MrcpTransactionStates.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <iostream>
#include "MrcpTransactionStates.hpp"
#include "ace/Log_Msg.h"

using namespace std;

MrcpStateBase::MrcpStateBase(MrcpEngine& engine) : m_engine(engine)
{
	ACE_TRACE(ACE_TEXT("[MrcpStateBase::MrcpStateBase()]"));

	return;
}

MrcpStateBase::~MrcpStateBase()
{
	ACE_TRACE(ACE_TEXT("[MrcpStateBase::~MrcpStateBase()]"));
	
	return;
}

void MrcpStateBase::SendMessage()
{
	ACE_TRACE(ACE_TEXT("[MrcpStateBase::SendMessage()]"));
	
	return;
}
