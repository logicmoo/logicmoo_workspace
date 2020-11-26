/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MrcpTransactionStates.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef MRCPTRANSACTIONSTATES_HPP_
#define MRCPTRANSACTIONSTATES_HPP_

#include <string>

// Foraward declarations

class MrcpEngine;

using namespace std;

class MrcpStateBase
{
public:

	MrcpStateBase(MrcpEngine& engine);
	virtual ~MrcpStateBase();
	
	// Sends MRCP message
	virtual void SendMessage();
	
protected:
	
	MrcpEngine& m_engine;
};

class MrcpStateSendDescribe : public MrcpStateBase
{
public:
	MrcpStateSendDescribe(MrcpEngine& engine);
	~MrcpStateSendDescribe();
	
	virtual void SendDescribe(string const &resource);
};

class MrcpStateWait200OK : public MrcpStateBase
{
public:
	MrcpStateWait200OK(MrcpEngine& engine);
	~MrcpStateWait200OK();
	
	virtual void IncomingResponse();
};

class MrcpStateSendSetup : public MrcpStateBase
{
public:
	MrcpStateSendSetup(MrcpEngine& engine);
	~MrcpStateSendSetup();
	
	virtual void SendSetup(string const &resource, string const &sdp = "");
};

class MrcpStateSendDefineGrammar : public MrcpStateBase
{
public:
	MrcpStateSendDefineGrammar(MrcpEngine& engine);
	~MrcpStateSendDefineGrammar();
	
	virtual void SendDefineGrammar(string const &grammar, string const &content_id, 
								string const &content_type, string const &parameters = "");
};

class MrcpStateSendRecognize : public MrcpStateBase
{
public:
	MrcpStateSendRecognize(MrcpEngine& engine);
	~MrcpStateSendRecognize();
	
	virtual int  SendRecognize(string const &uri_list, // should be a single grammar (really?)
								string const &parameters = "");
};

class MrcpStateSendStop : public MrcpStateBase
{
public:
	MrcpStateSendStop(MrcpEngine& engine);
	~MrcpStateSendStop();
	
	virtual void SendStop(int req_tag);
};
    
class MrcpStateSendTeardown : public MrcpStateBase
{
public:
	MrcpStateSendTeardown(MrcpEngine& engine);
	~MrcpStateSendTeardown();
	
	virtual void SendTeardown(string const &resource);
};
  
class MrcpStateSendSetParams : public MrcpStateBase
{
public:
	MrcpStateSendSetParams(MrcpEngine& engine);
	~MrcpStateSendSetParams();
	
	virtual void SendSetParams(string const &params);
};   

class MrcpStateSendGetParams : public MrcpStateBase
{
public:
	MrcpStateSendGetParams(MrcpEngine& engine);
	~MrcpStateSendGetParams();
	
	virtual void SendGetParams(string const &params);
};

class MrcpStateSendSpeak : public MrcpStateBase
{
public:
	MrcpStateSendSpeak(MrcpEngine& engine);
	~MrcpStateSendSpeak();
	
	virtual void SendSpeak(string const & text);
};  

#endif /*MRCPTRANSACTIONSTATES_HPP_*/
