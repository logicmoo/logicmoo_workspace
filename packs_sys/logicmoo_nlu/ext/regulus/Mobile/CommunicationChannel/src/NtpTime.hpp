/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	NTPTime.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef NTPTIME_HPP_
#define NTPTIME_HPP_

#include "SDL_net.h"

// Network time object. 
class NTPTime
{
public:
	
	NTPTime(void);
	NTPTime(Uint32 secs, Uint32 frac);
	~NTPTime(void);
	
	// Static methods
	
	// Creates an NTP timestamp that reflects the current
	// time.
	// Returns an NTP timestamp object reflecting
	// the current wall-clock time.
	static NTPTime Now(void);	

	// Evaluates the time difference between two NTP
	// timestamps.
	// Returns the time difference between the
	// supplied NTP timestamps, expressed in
	// milliseconds. A positive value indicates 
	// that tm1 reflects a more recent
	// wall-clock time than tm2.
	static long	Diff(const NTPTime& tm1, const NTPTime& tm2);	
		
	// Comparison operators
	bool operator == (const NTPTime& other) const;
	bool operator != (const NTPTime& other) const;
	bool operator <= (const NTPTime& other) const;
	bool operator >= (const NTPTime& other) const;
	bool operator <	(const NTPTime& other) const;
	bool operator >	(const NTPTime& other) const;
	
	// Arithmetical operators
	NTPTime& operator += (Uint32 msecs);
	NTPTime& operator -= (Uint32 msecs);
	NTPTime operator + (Uint32 msecs) const;
	NTPTime	operator - (Uint32 msecs) const;

	// Property getter methods
	Uint32 GetSeconds(void) const;
	Uint32 GetFractional(void) const;
	Uint32 GetFracMSecs(void) const;

private:

	// Private attributes.
	Uint32 	m_secs;
	Uint32	m_frac;   
};

#endif /*NTPTIME_HPP_*/
