#include "PlaybackBuffer.hpp"

PlaybackBuffer::PlaybackBuffer():
m_playback_buffer(NULL),
m_playback_buffer_items(0)
{
}

PlaybackBuffer::~PlaybackBuffer()
{
	FreeCArray(m_playback_buffer);
	
	map <unsigned int, char **>::iterator iter;
	
	for ( iter = m_playback_buffer_map.begin( ) ; iter != m_playback_buffer_map.end( ) ; iter++ ){
		FreeCArray((char **) iter->second);
		m_playback_buffer_map.erase(iter);
	}
}

void PlaybackBuffer::AppendPrompts(const char * const *prompts)
{
	if( prompts != NULL ){
		int i = 0;
		int num_new_items = 0;

		while(prompts[i] != NULL){
			i++;
		}
		num_new_items = i;

#ifdef DEBUG
		cerr << "DEBUG: " << __FUNCTION__ << "\tadding " << num_new_items << " item(s) to playback buffer." << endl;
#endif
		m_playback_buffer = (char **)realloc(m_playback_buffer, (m_playback_buffer_items+num_new_items+1)*sizeof(char *));

		i=0;
		int j=0;
		for(j=m_playback_buffer_items ; j<m_playback_buffer_items+num_new_items ; j++){
			m_playback_buffer[j] = strdup(prompts[i]);
			i++;
		}
		m_playback_buffer[j] = NULL;
		m_playback_buffer_items += i;
		
#ifdef DEBUG
		cerr << "DEBUG: " << __FUNCTION__ << "\tbuffer = " << m_playback_buffer << "\titems = " << m_playback_buffer_items << endl;
#endif		
	}
}

char ** PlaybackBuffer::GetPrompts(unsigned int promptID)
{
	char **result = NULL;

#ifdef DEBUG	
	cerr << "DEBUG: " << __FUNCTION__ << "\tm_playback_buffer_items = " << m_playback_buffer_items << endl;
#endif
	if(m_playback_buffer_items != 0){
		result = m_playback_buffer;
		m_playback_buffer_map.insert( pair<unsigned int, char**>(promptID, result) );
		m_playback_buffer = NULL;
		m_playback_buffer_items = 0;
	}

	return result;
}

void PlaybackBuffer::FreePrompts(unsigned int promptID)
{
	map <unsigned int, char **>::iterator iter;
	
	iter = m_playback_buffer_map.find(promptID);
	
	if( iter != m_playback_buffer_map.end() ){
		FreeCArray(iter->second);
		m_playback_buffer_map.erase(iter);
	}
}

unsigned int PlaybackBuffer::GetNumQueuedPrompts()
{
	return m_playback_buffer_items;
}

void PlaybackBuffer::FreeCArray(char **array){
	if(array != NULL){
		int i=0;

		while(array[i] != NULL){
			free(array[i]);
			i++;
		}
		free(array);
		array = NULL;
	}
}