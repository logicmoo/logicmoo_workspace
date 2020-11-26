/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	NetPool.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
// Initialization of the SDL_net and statistics of the network.
#ifndef NETPOOL_HPP_
#define NETPOOL_HPP_

#include <map>
#include <string>

using namespace std;

class NetPool
{
public: 

	static NetPool* GetInstance();

	// Statistics variables
 	// len_in
 	// len_out
	// tcp_len_in
	// tcp_len_out
	// udp_len_in
	// udp_len_out
	// tcp_n_in
	// tcp_n_out
	// udp_n_in
	// udp_n_out
	 
	map<string, int> stats;

protected:
	
	NetPool();

	virtual ~NetPool();

private:
	
	// Avoid accidental copy or assignment
	NetPool(const NetPool&);
	NetPool& operator = (const NetPool&);
	
private:

	static NetPool* _instance;
};

#endif /*NETPOOL_HPP_*/
