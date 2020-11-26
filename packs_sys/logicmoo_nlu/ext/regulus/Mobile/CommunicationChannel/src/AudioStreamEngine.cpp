/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	AudioStreamEngine.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <iostream>
#include "AudioStreamEngine.hpp"
#include "ace/Log_Msg.h"

AudioStreamEngine::AudioStreamEngine(AudioStreamEngineObserver& observer) : m_observer(observer)
{
	ACE_TRACE(ACE_TEXT("[AudioStreamEngine::AudioStreamEngine()]"));

	m_phandle = NULL;
	m_chandle = NULL;

	m_want_rec = false;
	int rc = 0;
	int size;
	unsigned int val;
	int dir;
	snd_pcm_hw_params_t *params;

	m_chandle = NULL;

	// Open PCM device for recording (capture)
	rc = snd_pcm_open(&m_chandle, "default",
						SND_PCM_STREAM_CAPTURE, 0);

	if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::AudioStreamEngine()] "
				 "[Unable to open pcm device for recording: %s]\n"), snd_strerror(rc)));
		exit(1);
//		return;
	}

//	InitializeHandler(m_chandle);

	// Allocate a hardware parameters object
	snd_pcm_hw_params_alloca(&params);

	// Fill it in with default values
	rc = snd_pcm_hw_params_any(m_chandle, params);

	if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::AudioStreamEngine()] "
				 "[Unable to fill in parameter object with default values: %s]\n"), 
				 snd_strerror(rc)));
//		exit(1);
		return;
	}

	// Set the desired hardware parameters
	// Interleaved mode
	rc = snd_pcm_hw_params_set_access(m_chandle, params,
										SND_PCM_ACCESS_RW_INTERLEAVED);

	if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::AudioStreamEngine()] "
				 "[Unable to set interleaved mode: %s]\n"), 
				 snd_strerror(rc)));
//		exit(1);
		return;
	}

	// Signed 16-bit little-endian format
	rc = snd_pcm_hw_params_set_format(m_chandle, params,
										SND_PCM_FORMAT_MU_LAW);

	if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::AudioStreamEngine()] "
				 "[Unable to set signed 16-bit little-endian format: %s]\n"), 
				 snd_strerror(rc)));
//		exit(1);
		return;
	}

	// One channel (mono)
	rc = snd_pcm_hw_params_set_channels(m_chandle, params, 1);

	if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::AudioStreamEngine()] "
				 "[Unable to set one channel: %s]\n"),
				 snd_strerror(rc)));
//		exit(1);
		return;
	}

	// 8000 bits/second sampling rate
	val = 8000;
	rc = snd_pcm_hw_params_set_rate_near(m_chandle, params,
											&val, &dir);

	if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::AudioStreamEngine()] "
				 "[Unable to set 8000 bits/second sampling rate: %s]\n"), 
				 snd_strerror(rc)));
//		exit(1);
		return;
	}

	// Set period size to 80 frames
	m_frames = 160;
	rc = snd_pcm_hw_params_set_period_size_near(m_chandle,
													params, &m_frames, &dir);

	//cout << "---[DEBUG] -------------Frames" << m_frames << "Dir" << dir << endl;

	if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::AudioStreamEngine()] "
				 "[Unable to set period size to 80 frames: %s]\n"),
				 snd_strerror(rc)));
//		exit(1);
		return;
	}

	// Write the parameters to the driver
	rc = snd_pcm_hw_params(m_chandle, params);

	if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::AudioStreamEngine()] "
				 "[Unable to set hw parameters: %s]\n"), 
				 snd_strerror(rc)));
//		exit(1);
		return;
	}

	// Use a buffer large enough to hold one period
	rc = snd_pcm_hw_params_get_period_size(params,
											&m_frames, &dir);
	//cout << "---[DEBUG] -------------Frames" << m_frames << endl;
	if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::AudioStreamEngine()] "
				 "[Unable to get period size: %s]\n"), 
				 snd_strerror(rc)));
//		exit(1);
		return;
	}

	size = m_frames * 1; // 1 byte/sample, 1 channel
	//m_buffer = (char *) malloc(size);

	// We want to loop for 5 seconds ???
	rc = snd_pcm_hw_params_get_period_time(params,
											&val, &dir);

	if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::AudioStreamEngine()] "
				 "[Unable to get period: %s]\n"), 
				 snd_strerror(rc)));
//		exit(1);
		return;
	}

	m_phandle = NULL;

	// Open PCM device for playback
	rc = snd_pcm_open(&m_phandle, "default",
						SND_PCM_STREAM_PLAYBACK, 0);

	if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::AudioStreamEngine()] "
				 "[Unable to open pcm device for playback: %s]\n"), 
				 snd_strerror(rc)));
//		exit(1);
		return;
	}

//	InitializeHandler(m_phandle);

	// Allocate a hardware parameters object
	snd_pcm_hw_params_alloca(&params);

	// Fill it in with default values
	rc = snd_pcm_hw_params_any(m_phandle, params);

	if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::AudioStreamEngine()] "
				 "[Unable to fill in parameter object with default values: %s]\n"), 
				 snd_strerror(rc)));
//		exit(1);
		return;
	}

	// Set the desired hardware parameters
	// Interleaved mode
	rc = snd_pcm_hw_params_set_access(m_phandle, params,
										SND_PCM_ACCESS_RW_INTERLEAVED);

	if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::AudioStreamEngine()] "
				 "[Unable to set interleaved mode: %s]\n"), 
				 snd_strerror(rc)));
//		exit(1);
		return;
	}

	// Signed 16-bit little-endian format
	rc = snd_pcm_hw_params_set_format(m_phandle, params,
										SND_PCM_FORMAT_MU_LAW);

	if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::AudioStreamEngine()] "
				 "[Unable to set signed 16-bit little-endian format: %s]\n"), 
				 snd_strerror(rc)));
//		exit(1);
		return;
	}

	// One channel (mono)
	rc = snd_pcm_hw_params_set_channels(m_phandle, params, 1);

	if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::AudioStreamEngine()] "
				 "[Unable to set one channel: %s]\n"), 
				 snd_strerror(rc)));
//		exit(1);
		return;
	}

	// 8000 bits/second sampling rate
	val = 8000;
	rc = snd_pcm_hw_params_set_rate_near(m_phandle, params,
											&val, &dir);

	if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::AudioStreamEngine()] "
				 "[Unable to set 8000 bits/second sampling rate: %s]\n"), 
				 snd_strerror(rc)));
//		exit(1);
		return;
	}

	// Set period size to 80 frames
	m_frames = 160;
	/*!rc = snd_pcm_hw_params_set_period_size_near(handle,
													params, &m_frames, &dir);

	//cout << "---[DEBUG] -------------Frames" << m_frames << "Dir" << dir << endl;

	if (rc < 0)
	{
		cout << "---[DEBUG] [AudioStreamEngine::AudioStreamEngine]"
			<< "[Unable to set period size to 80 frames: "
			<< snd_strerror(rc)	<< "]" << endl;

//		exit(1);
		return;
	}*/

	// Write the parameters to the driver
	rc = snd_pcm_hw_params(m_phandle, params);

	if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::AudioStreamEngine()] "
				 "[Unable to set hw parameters: %s]\n"), 
				 snd_strerror(rc)));
//		exit(1);
		return;
	}
	// Use a buffer large enough to hold one period */
	/*rc = snd_pcm_hw_params_get_period_size(params, &frames, &dir);

	if (rc < 0)
	{
		cout << "---[DEBUG] [AudioStreamEngine::AudioStreamEngine]"
			<< "[Unable to get period size: "
			<< snd_strerror(rc)	<< "]" << endl;

//		exit(1);
		return;
	}*/

	size = m_frames * 1; /* 1 byte/sample, 1 channels */

	//fprintf(stderr, "size: %d, frames: %d\n", size, frames);

	// We want to loop for 5 seconds
	rc = snd_pcm_hw_params_get_period_time(params, &val, &dir);

	// TODO: Recheck
	m_buffer = (Uint8*)malloc(size);

	return;
}

void AudioStreamEngine::InitializeHandler(snd_pcm_t* handle)
{
	ACE_TRACE(ACE_TEXT("[AudioStreamEngine::InitializeHandlers()]"));

	int rc = 0;
	//int size;
	unsigned int val;
	int dir;
	snd_pcm_hw_params_t *params;

	// Allocate a hardware parameters object
	snd_pcm_hw_params_alloca(&params);

	// Fill it in with default values
	rc = snd_pcm_hw_params_any(handle, params);

	if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::InitializeHandler()] "
				 "[Unable to fill in parameter object with default values: %s]\n"), 
				 snd_strerror(rc)));
//		exit(1);
		return;
	}

	// Set the desired hardware parameters
	// Interleaved mode
	rc = snd_pcm_hw_params_set_access(handle, params,
										SND_PCM_ACCESS_RW_INTERLEAVED);

	if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::InitializeHandler()] "
				 "[Unable to set interleaved mode: %s]\n"), 
				 snd_strerror(rc)));
//		exit(1);
		return;
	}

	// Signed 16-bit little-endian format
	rc = snd_pcm_hw_params_set_format(handle, params,
										SND_PCM_FORMAT_MU_LAW);

	if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::InitializeHandler()] "
				 "[Unable to set signed 16-bit little-endian format: %s]\n"), 
				 snd_strerror(rc)));
//		exit(1);
		return;
	}

	// One channel (mono)
	rc = snd_pcm_hw_params_set_channels(handle, params, 1);

	if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::InitializeHandler()] "
				 "[Unable to set one channel: %s]\n"), 
				 snd_strerror(rc)));
//		exit(1);
		return;
	}

	// 8000 bits/second sampling rate
	val = 8000;
	rc = snd_pcm_hw_params_set_rate_near(handle, params,
											&val, &dir);

	if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::InitializeHandler()] "
				 "[Unable to set 8000 bits/second sampling rate: %s]\n"), 
				 snd_strerror(rc)));
//		exit(1);
		return;
	}

	// Set period size to 80 frames
	m_frames = 160;
	/*!rc = snd_pcm_hw_params_set_period_size_near(handle,
													params, &m_frames, &dir);

	//cout << "---[DEBUG] -------------Frames" << m_frames << "Dir" << dir << endl;

	if (rc < 0)
	{
		cout << "---[DEBUG] [AudioStreamEngine::AudioStreamEngine]"
			<< "[Unable to set period size to 80 frames: "
			<< snd_strerror(rc)	<< "]" << endl;

//		exit(1);
		return;
	}*/

	// Write the parameters to the driver
	rc = snd_pcm_hw_params(handle, params);

	if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::InitializeHandler()] "
				 "[Unable to set hw parameters: %s]\n"), 
				 snd_strerror(rc)));
//		exit(1);
		return;
	}
	/*!
	// Use a buffer large enough to hold one period
	rc = snd_pcm_hw_params_get_period_size(params,
											&m_frames, &dir);
	//cout << "---[DEBUG] -------------Frames" << m_frames << endl;
	if (rc < 0)
	{
		cout << "---[DEBUG] [AudioStreamEngine::AudioStreamEngine]"
			<< "[Unable to get period size: "
			<< snd_strerror(rc)	<< "]" << endl;

//		exit(1);
		return;
	}

	size = m_frames * 2; // 2 bytes/sample, 1 channel
	//m_buffer = (char *) malloc(size);

	// We want to loop for 5 seconds ???
	rc = snd_pcm_hw_params_get_period_time(params,
											&val, &dir);

	if (rc < 0)
	{
		cout << "---[DEBUG] [AudioStreamEngine::AudioStreamEngine]"
			<< "[Unable to get period: "
			<< snd_strerror(rc)	<< "]" << endl;

//		exit(1);
		return;
	}*/

	return;
}

AudioStreamEngine::~AudioStreamEngine()
{
	ACE_TRACE(ACE_TEXT("[AudioStreamEngine::~AudioStreamEngine()]"));

//	m_proxy.Exit();

	return;
}

void AudioStreamEngine::PlayData(const Uint8* buffer)
{
#if _DEBUG_2
	ACE_TRACE(ACE_TEXT("[AudioStreamEngine::PlayData()]"));	
#endif

	int rc;

	if (m_phandle == NULL)
	{
		return;
	}

	if (buffer == NULL)
	{
		return;
	}

	rc = snd_pcm_writei(m_phandle, buffer, m_frames);

	if (rc == -EPIPE)
	{
		// EPIPE means underrun
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::PlayData()] "
				 "[Underrun occurred]\n")));		
		snd_pcm_prepare(m_phandle);
	}
	else if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::PlayData()] "
				 "[Error from writei: %s]\n"), 
				 snd_strerror(rc)));
	}
	else if (rc != (int)m_frames)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::PlayData()] "
				 "[Short write, write: %s frames]\n"), 
				 snd_strerror(rc)));
	}

	return;
}

void AudioStreamEngine::CloseAudio()
{
	ACE_TRACE(ACE_TEXT("[AudioStreamEngine::CloseAudio()]"));

	StopRecording();
	m_proxy.Exit();
	
	if (m_phandle != NULL)
	{
cout << endl << "---[DEBUG] [******* 1.1 *******]" << endl;
		//snd_pcm_drain(m_phandle);
		snd_pcm_drop(m_phandle);
cout << endl << "---[DEBUG] [******* 1.2 *******]" << endl;
		int err = snd_pcm_close(m_phandle);
		cout << endl << "---[DEBUG] err: " << err << endl;
	}

	if (m_chandle != NULL)
	{
cout << endl << "---[DEBUG] [******* 1.3 *******]" << endl;
		//snd_pcm_drain(m_chandle);
		snd_pcm_drop(m_chandle);
cout << endl << "---[DEBUG] [******* 1.4 *******]" << endl;		
		snd_pcm_close(m_chandle);
	}

	if (m_buffer != NULL)
	{
		free(m_buffer);
	}

	return;
}

void AudioStreamEngine::StartRecording()
{
	ACE_TRACE(ACE_TEXT("[AudioStreamEngine::StartRecording()]"));

	m_want_rec = true;
	m_results = m_proxy.WantRecording(m_want_rec);
	m_results.attach(this);

	return;
}

void AudioStreamEngine::StopRecording()
{
	ACE_TRACE(ACE_TEXT("[AudioStreamEngine::StopRecording()]"));

	m_want_rec = false;
//	m_results = m_proxy.WantRecording(m_want_rec);
//	m_results.attach(this);

	return;
}

void AudioStreamEngine::RecordData()
{
	ACE_TRACE(ACE_TEXT("[AudioStreamEngine::RecordData()]"));

	long loops;
	int rc = 0;

	loops = 350;

	if (m_chandle == NULL)
	{
		return;
	}

	while (loops > 0)
	{
		loops--;
		rc = snd_pcm_readi(m_chandle, m_buffer, m_frames);

		if (rc == -EPIPE)
		{
			// EPIPE means overrun
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::RecordData()] "
				 "[Overrun occurred]\n")));			
			snd_pcm_prepare(m_chandle);
		}
		else if (rc < 0)
		{
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::RecordData()] "
				 "[Error from read: %s]\n"), 
				 snd_strerror(rc))); 
			
		}
		else if (rc != (int)m_frames)
		{
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::RecordData()] "
				 "[Short read, write: %s frames]\n"), 
				 snd_strerror(rc)));			
		}

		m_ostream.str("");

		// TODO: Recheck
		for (int i = 0; i < 160; ++i)
		{
			m_ostream << m_buffer[i];
		}

		//m_observer.AudioDataReceived(m_ostream);
		m_observer.AudioDataReceived(m_buffer);
	}

	return;
}

void AudioStreamEngine::Run()
{
#if _DEBUG_1
	ACE_TRACE(ACE_TEXT("[AudioStreamEngine::Run()]"));
#endif

	int rc = 0;

	if (m_chandle == NULL)
	{
		return;
	}

	rc = snd_pcm_readi(m_chandle, m_buffer, m_frames);

	if (rc == -EPIPE)
	{
		// EPIPE means overrun
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::Run()] "
				 "[Overrun occurred]\n")));
		snd_pcm_prepare(m_chandle);
	}
	else if (rc < 0)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::Run()] "
				 "[Error from read: %s]\n"), 
				 snd_strerror(rc)));
	}
	else if (rc != (int)m_frames)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [AudioStreamEngine::Run()] "
				 "[Short read, read: %s frames]\n"), 
				 snd_strerror(rc)));		
	}

	m_observer.AudioDataReceived(m_buffer);

	m_results = m_proxy.WantRecording(m_want_rec);
	m_results.attach(this);

	return;
}

void AudioStreamEngine::update(const ACE_Future<bool>& future)
{
#if _DEBUG_1
	ACE_TRACE(ACE_TEXT("[AudioStreamEngine::update()]"));
#endif

	bool result = false;

	((ACE_Future<bool>)future).get(result);

#if _DEBUG_1
	ACE_DEBUG((LM_INFO,
		ACE_TEXT("[AudioStreamEngine::update()] "
				"[(%t) New Status %d]\n"), result));
#endif

	if (result > 0)
	{
		Run();
	}
	/*else
	{
		m_proxy.Exit();
	}*/

	return;
}
