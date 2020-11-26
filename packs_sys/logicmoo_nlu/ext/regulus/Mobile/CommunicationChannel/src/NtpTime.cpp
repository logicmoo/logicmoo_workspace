/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	NtpTime.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <limits.h>

#ifdef _ARCH_WIN32
#  include <windows.h>
#else
#  include <sys/time.h>
#endif

#include "NtpTime.hpp"

// Helper functions

#ifdef _ARCH_WIN32

static void	GetTimeOfDay(Uint32* secs, Uint32* usecs)
{
	// NOTE: The adjustment expresses the number of 100
	// nanosecond units from 1/1/1601 to 1/1/1970.

	static const __int64 adj(116444736000000000i64);

	union FileTime
	{
		__int64	 scalar;
        FILETIME filetime;
	};

	FileTime tm;

	GetSystemTimeAsFileTime(&tm.filetime);

	tm.scalar -= adj;

	*secs  = (Uint32)(tm.scalar / 10000000i64);
	*usecs = (Uint32)(tm.scalar % 10000000i64) / 10;

	return;
}

#else

static void	GetTimeOfDay(Uint32* secs, Uint32* usecs)
{
	struct timeval tm;

	gettimeofday(&tm, 0);

	*secs  = tm.tv_sec;
	*usecs = tm.tv_usec;

	return;
}

#endif

// Implementation of the 'NTPTime' class.

NTPTime::NTPTime(void) :
	m_secs(0), 
	m_frac(0)
{
	return;
}

NTPTime::NTPTime(Uint32 secs, Uint32 frac) : 
	m_secs(secs), 
	m_frac(frac)
{ 
	return;	
}

NTPTime::~NTPTime (void)
{
	return;
}

bool NTPTime::operator == (const NTPTime& other) const
{
	return ((m_secs == other.m_secs)
				&& (m_frac == other.m_frac));
}

bool NTPTime::operator != (const NTPTime& other) const
{
	return !(*this == other);
}

bool NTPTime::operator <= (const NTPTime& other) const
{
	return !(*this > other);
}

bool NTPTime::operator >= (const NTPTime& other) const
{
	return !(*this < other);
}

bool NTPTime::operator < (const NTPTime& other) const
{
	return (m_secs == other.m_secs)
		? (m_frac < other.m_frac)
		: (m_secs < other.m_secs);
}

bool NTPTime::operator > (const NTPTime& other) const
{
	return (other < *this);
}

NTPTime& NTPTime::operator += (Uint32 msecs)
{
	Uint32	secs = (msecs / 1000);
	Uint32	frac = (msecs % 1000) * (ULONG_MAX / 1000);

	if (m_frac > (ULONG_MAX - frac)) // Will overflow.
	{ 
		++m_secs; 
	}
	
	if (m_secs > (ULONG_MAX - secs)) // Will overflow.
	{ 
		m_secs = ULONG_MAX - secs; 
	}

	m_secs += secs;
	m_frac += frac;
	
	return *this;
}

NTPTime& NTPTime::operator -= (Uint32 msecs)
{
	Uint32	secs = (msecs / 1000);
	Uint32	frac = (msecs % 1000) * (ULONG_MAX / 1000);

	// Prevent underflow.

	if (m_frac < frac)
	{ 
		++secs; 
	}
	
	if (m_secs < secs)
	{ 
		secs = m_secs; 
		frac = m_frac; 
	}

	m_secs -= secs;
	m_frac -= frac;

	return *this;
}

NTPTime	NTPTime::operator +	(Uint32 msecs) const
{
	return (NTPTime(*this) += msecs);
}

NTPTime	NTPTime::operator -	(Uint32 msecs) const
{
	return (NTPTime(*this) -= msecs);
}

Uint32 NTPTime::GetSeconds(void) const
{
	return m_secs;
}

Uint32 NTPTime::GetFractional(void) const
{
	return m_frac;
}

Uint32 NTPTime::GetFracMSecs(void) const
{
	return m_frac / (ULONG_MAX / 1000);
}

NTPTime NTPTime::Now(void)
{
	// NOTE: The adjustment expresses the number of 
	// seconds from 1/1/1900 to 1/1/1970.

	static const Uint32 s_adj(2208988800ul);

	Uint32	secs(0);
	Uint32	usecs(0);

	GetTimeOfDay(&secs, &usecs);

	return NTPTime(secs + s_adj, usecs * (ULONG_MAX / 1000000));
}

long NTPTime::Diff(const NTPTime& tm1, const NTPTime& tm2)
{
	if (tm1 < tm2)
	{
		return -Diff(tm2, tm1); 
	}
	else
	{
		Uint32	secs(tm2.m_secs);
		Uint32	frac(tm2.m_frac);

		// Prevent underflow.

		if (tm1.m_frac < frac)
		{ 
			++secs; 
		}
		
		if (tm1.m_secs < secs)
		{ 
			secs = tm1.m_secs; frac = tm1.m_frac; 
		}

		secs = tm1.m_secs - secs;
		frac = tm1.m_frac - frac;

		return (secs * 1000) + (frac / (ULONG_MAX / 1000));
	}

	// Unreachable code.

	return 0;
}
