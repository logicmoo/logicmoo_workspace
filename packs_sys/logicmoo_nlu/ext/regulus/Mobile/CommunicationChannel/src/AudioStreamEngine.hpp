/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	AudioControllerAgent.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#ifndef AUDIOSTREAMENGINE_HPP_
#define AUDIOSTREAMENGINE_HPP_

#include <alsa/asoundlib.h>
#include "ace/config-lite.h"
#include "ace/OS_NS_unistd.h"
#include "ace/Activation_Queue.h"
#include "ace/Method_Request.h"
#include "ace/Task.h"
#include "ace/Future.h"
#include "ace/Auto_Ptr.h"
#include "AudioStreamEngineObserver.hpp"
#include "SDL_net.h"

// Use the newer ALSA API
#define ALSA_PCM_NEW_HW_PARAMS_API

using namespace std;

class AudioControllerAgent
{
// Proxy to the AudioController that is on the network.
public:
	
	AudioControllerAgent()
	{
		ACE_TRACE
			(ACE_TEXT("AudioControllerAgent::AudioControllerAgent"));
		
		return;
	}

	int WantRecording(bool& want_rec)
	{
#if _DEBUG_2
		ACE_TRACE(ACE_TEXT("AudioControllerAgent::WantRecording"));
#endif

		//cout << "---[DEBUG] [AudioControllerAgent::WantRecording()]: " 
		//	<< want_rec << endl;
		
		// Wait until we want audio data
		//while (want_rec == false) {}
		
		return want_rec;
	}
	
private:
	
	// Avoid accidental copy or assignment
	AudioControllerAgent(const AudioControllerAgent&);
	AudioControllerAgent& operator = (const AudioControllerAgent&);
};

class AudioRecordingRequest : public ACE_Method_Request
{
public:
	
	AudioRecordingRequest(AudioControllerAgent& controller, 
							ACE_Future<bool>& returnVal, 
							bool& want_rec)
				: m_controller(controller), m_returnVal(returnVal),
					m_want_rec(want_rec)
	{
#if _DEBUG_2
		ACE_TRACE(ACE_TEXT("AudioRecordingRequest::AudioRecordingRequest"));
#endif
		
		return;
	}

	virtual int call(void)
	{
#if _DEBUG_2
		ACE_TRACE (ACE_TEXT("AudioRecordingRequest::call"));
#endif

		//  WantRecording with the controller
		this->m_returnVal.set(this->m_controller.WantRecording(m_want_rec));
		
		return 0;
	}
	
private:
	
	AudioControllerAgent&	m_controller;
	ACE_Future<bool>		m_returnVal;
	bool& 					m_want_rec;
};

class AudioExitRequest : public ACE_Method_Request
{
public:
	
	virtual int call(void)
	{
#if _DEBUG_2
		ACE_TRACE (ACE_TEXT("AudioExitRequest::call"));		
#endif

		// Cause exit.
		return -1;
	}
};

class AudioScheduler : public ACE_Task_Base
{
public:
 
	AudioScheduler()
	{
#if _DEBUG_2
		ACE_TRACE(ACE_TEXT("AudioScheduler::AudioScheduler"));	
#endif

		this->activate();
		
		return;
	}

	virtual int svc(void)
	{
#if _DEBUG_2
		ACE_TRACE(ACE_TEXT("AudioScheduler::svc"));		
#endif

		while (1)
		{
			// Dequeue the next method object
			auto_ptr<ACE_Method_Request>
				request(this->m_activation_queue.dequeue());

			// Invoke the method request.
			if (request->call() == -1)
			{
				break;
			}
      	}

		return 0;
	}

	int Enqueue(ACE_Method_Request *request)
	{
#if _DEBUG_2
		ACE_TRACE(ACE_TEXT("AudioScheduler::Enqueue"));
#endif

		return this->m_activation_queue.enqueue(request);
	}

private:
	
	// Avoid accidental copy or assignment
	AudioScheduler(const AudioScheduler&);
	AudioScheduler& operator = (const AudioScheduler&);
	
private:

	ACE_Activation_Queue m_activation_queue;
};

class AudioControllerAgentProxy
{
// This acts as a Proxy to the controller impl object.
public:
	
	ACE_Future<bool> WantRecording(bool& want_rec)
	{
#if _DEBUG_2
		ACE_TRACE
			(ACE_TEXT("AudioControllerAgentProxy::WantRecording"));		
#endif
		
		ACE_Future<bool> result;

		// Create and enqueue a method request on the scheduler.
		this->m_scheduler.Enqueue
			(new AudioRecordingRequest(this->m_controller, result, want_rec));

		// Return Future to the client.
		return result;
	}

	void Exit(void)
	{
#if _DEBUG_2
		ACE_TRACE (ACE_TEXT("AudioControllerAgentProxy::Exit"));		
#endif
		
		this->m_scheduler.Enqueue(new AudioExitRequest);
		
		return;
	}

private:
	
	AudioScheduler 			m_scheduler;
	AudioControllerAgent	m_controller;
};

class AudioStreamEngine : public ACE_Future_Observer<bool>
{
public:
	AudioStreamEngine(AudioStreamEngineObserver& observer);
	~AudioStreamEngine();
	
	// Handles request completion event
	void Run();
	virtual void update(const ACE_Future<bool>& future);
	
	void InitializeHandler(snd_pcm_t* handle);
	void StartRecording();
	void StopRecording();
	void PlayData(const Uint8* buffer);
	void RecordData();
	void CloseAudio();	

private:
	AudioStreamEngineObserver&	m_observer;
	snd_pcm_t*					m_chandle;
	snd_pcm_t*					m_phandle;
	snd_pcm_uframes_t 			m_frames;
	ostringstream				m_ostream;
	Uint8*						m_buffer;
	bool						m_want_rec;
	AudioControllerAgentProxy	m_proxy;
	ACE_Future<bool> 			m_results;
};

#endif /*AUDIOSTREAMENGINE_HPP_*/
