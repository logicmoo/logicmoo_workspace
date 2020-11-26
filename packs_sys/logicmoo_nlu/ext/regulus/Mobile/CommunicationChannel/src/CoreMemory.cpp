/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	CoreMemory.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "CoreMemory.hpp"

struct AcctTag
{
	unsigned int	size;
	unsigned short	token;
	unsigned short	account;
};

const unsigned short ACCT_TAG_VALUE = 44224;

#ifdef _MDEBUG

void* Memory::Allocate(size_t n)
				throw (OutOfMemory)
{
	m_stats.callsToAllocate++;

	size_t  *ptr = 0;

	if (n != 0)
	{ 
		ptr = (size_t*)malloc(n + 2 * sizeof(size_t)); 
	}
	
	if (ptr == 0) 
	{ 
		throw OutOfMemory(); 
	}

	*ptr++ = n;
	*ptr++ = 0;

	m_stats.totalMemoryUsed += n;

	if (m_stats.maximumMemoryUsed < m_stats.totalMemoryUsed) 
	{
		m_stats.maximumMemoryUsed = m_stats.totalMemoryUsed;
		m_stats.minimumMemoryUsed = m_stats.totalMemoryUsed;
	}

	if (m_stats.logFp)
	{
		fprintf(m_stats.logFp, "---- [INFO] [Memory] [A] "
			"[0x%p] [%8d] [%5d]\n", ptr, m_stats.totalMemoryUsed, n);
	}

	return (void*)ptr;
}

void* Memory::Reallocate(void *ptr, size_t n)
						 throw (OutOfMemory) 
{
	if (ptr == 0)
	{ 
		return allocate(n); 
	}

	m_stats.callsToReallocate++;

	size_t *old_ptr = reinterpret_cast<size_t*>(ptr) - 2;
	size_t old_size = *old_ptr;

	if (n <= old_size)
	{ 
		return ptr; 
	}

	old_ptr = (size_t*)realloc(old_ptr, n + 2 * sizeof(size_t));

	if (old_ptr == 0) 
	{ 
		throw OutOfMemory(); 
	}

	*old_ptr++ = n;
	*old_ptr++ = 0;

	m_stats.totalMemoryUsed += n - old_size;

	if (m_stats.maximumMemoryUsed < m_stats.totalMemoryUsed)
	{ 
		m_stats.maximumMemoryUsed = m_stats.totalMemoryUsed;
		m_stats.minimumMemoryUsed = m_stats.totalMemoryUsed;
	}

	if (m_stats.logFp)
	{
		fprintf(m_stats.logFp, "---- [INFO] [Memory] [R] "
			"[0x%p] [%8d] [%5d -> %5d]\n", old_ptr,
			m_stats.totalMemoryUsed, old_size, n);
	}

	return old_ptr;
}

void Memory::Deallocate(void *ptr) 
{
	m_stats.callsToDeallocate++;

	if (ptr == 0)
	{
		if (m_stats.logFp)
		{
			fprintf(m_stats.logFp, "---- [INFO] [Memory] [E] "
				"[0x000000000]\n");
		}

		return;
	}

	if ((reinterpret_cast<AcctTag*>(ptr) - 1)->token == ACCT_TAG_VALUE)
	{
		Core::Log::warning(_TEXT("[Memory] ")
			_TEXT("[Tag (1) at: 0x%08x]\n"), ptr);
			
		return;
	}

	size_t  *old_ptr = reinterpret_cast<size_t*>(ptr) - 2;
	size_t   old_size = *old_ptr;

	m_stats.totalMemoryUsed -= old_size;

	if (m_stats.totalMemoryUsed < m_stats.minimumMemoryUsed)
	{ 
		m_stats.minimumMemoryUsed = m_stats.totalMemoryUsed; 
	}

	if (old_ptr[1] == 0)
	{ 
		free(old_ptr); 
	}
	else
	{ 
		Core::Log::warning(_TEXT("[Memory] ")
			_TEXT("[Tag (2) at: 0x%08x]\n"), old_ptr); 
	}

	if (m_stats.logFp)
	{
		fprintf(m_stats.logFp, "---- [INFO] [Memory] [F] "
			"[0x%p] [%8d] [%5d]\n", old_ptr + 2,
			m_stats.totalMemoryUsed, old_size);
	}

	return;
}

void Memory::startLogging(FILE* fp)
{
	m_stats.logFp = fp;
	
	return;
}

void Memory::stopLogging(void)
{
	m_stats.logFp = 0;
	
	return;
}

#else

void* Memory::Allocate(size_t n)
						throw (OutOfMemory)
{
	size_t*	ptr = (size_t*)malloc(n + sizeof(size_t));

	if (ptr == 0)
	{ 
		throw OutOfMemory(); 
	}

	*ptr++ = 0;

	return ptr;
}

void* Memory::Reallocate(void *old, size_t n)
						 throw (OutOfMemory) 
{
	if (old == 0)
	{ 
		return Allocate(n); 
	}

	size_t*	tmp = (size_t*)old;
	size_t*	ptr = (size_t*)realloc(tmp - 1, n + sizeof(size_t));

	if (ptr == 0)
	{ 
		throw OutOfMemory(); 
	}

	*ptr++ = 0;

	return ptr;
}

void Memory::Deallocate(void *old) 
{
	if (old == 0)
	{ 
		return; 
	}

	size_t*	tmp = (size_t*)old;
	size_t*	ptr = tmp - 1;

	if (*ptr != 0)
	{ 
		return; 
	}

	free(ptr);
	
	return;
}

void Memory::StartLogging(FILE* fp)
{
	return;
}

void Memory::StopLogging(void)
{
	return;
}

#endif

Memory::Memory(void)
{
	return;
}

Memory::Memory(const Memory& m)
{ 
	return;
}

Memory::~Memory(void)
{ 
	return; 
}

Memory&	Memory::operator = (const Memory& m)
{ 
	return *this; 
}

const Memory::Stats& Memory::GetStats(void)
{ 
	return m_stats;
}

void Memory::PrintStats(const Memory::Stats& s)
{
	fprintf(stderr, 
		"Memory stats\n"
		"[Total: %6u | MaxMem:  %6u | MinMem:  %6u]\n"
		"[Alloc: %6lu | Realloc: %6lu | Dealloc: %6lu]\n",
		s.totalMemoryUsed,
		s.maximumMemoryUsed,
		s.minimumMemoryUsed,
		s.callsToAllocate,
		s.callsToReallocate,
		s.callsToDeallocate);

	return;
}

void Memory::ClearStats(void)
{
	memset(&m_stats, 0, sizeof(Memory::Stats));
	
	return;
}

Memory::Stats Memory::m_stats;
/*
void* operator new	(size_t sz, const char*)
{
	size_t* ptr = (size_t*)Memory::Allocate(sz);
	ptr[-1] = sz;
	
	return ptr;
}
		
void* operator new [] (size_t sz, const char*)
{
	size_t* ptr = (size_t*)Memory::Allocate(sz);
	ptr[-1] = sz;
	
	return ptr;
}

void* operator new [] (size_t sz)
{ 
	return Memory::Allocate(sz); 
}

void operator delete [] (void *ptr)
{ 
	Memory::Deallocate(ptr); 
	
	return; 
}

void* operator new(size_t sz)
{ 
	return Memory::Allocate(sz); 
}

void operator delete(void *ptr)
{ 
	Memory::Deallocate(ptr); 
	
	return; 
}*/
