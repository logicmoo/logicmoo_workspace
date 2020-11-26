#ifdef WIN32
#pragma once
#endif

#ifndef _CPP
#define _CPP
#endif

#ifndef PLAYBACKBUFFER_HPP_
#define PLAYBACKBUFFER_HPP_

#include <vector>
#include <string>
#include <map>

#include <iostream>
#include <fstream>

using namespace std;

typedef pair <unsigned int, int> Int_Pair;



class PlaybackBuffer
{
private:
	char** m_playback_buffer;
	int	m_playback_buffer_items;
	map<unsigned int, char **> m_playback_buffer_map;
	void FreeCArray(char **);

public:
	PlaybackBuffer();
	~PlaybackBuffer();
	
	void AppendPrompts(const char * const *prompts);
	char **GetPrompts(unsigned int promptID);
	void FreePrompts(unsigned int promptID);
	unsigned int GetNumQueuedPrompts();
};



#endif /*PLAYBACKBUFFER_HPP_*/
