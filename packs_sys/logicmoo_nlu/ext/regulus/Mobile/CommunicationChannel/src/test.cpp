/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	test.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <iostream>
#include <sstream>
#include <ctime>
#include "TestCommunicationChannel.hpp"
#include "ace/streams.h"

/* Use the newer ALSA API */
//#define ALSA_PCM_NEW_HW_PARAMS_API
//#include <alsa/asoundlib.h>

using namespace std;

char* corpus[] = {"what time is the meeting on sunday ", 
				"what time is the meeting today ",
				"is there a meeting on monday ",
				"what is pierrette's email address ",
				"is there a meeting scheduled for this month ",
				"is there a meeting tomorrow around ten thirty ",
				"show me the next two meetings in martigny ",
				"show me all the meetings in july ",
				"will pierrette attend ",
				"when will it start ",
				"what is pierrette's address ",
				"was anyone from martigny at the meeting last week ",
				"will nikos attend a meeting on monday ",
				"what meetings will there be next week ",
				"what meetings were last week ",
				"will pierrette be attending ",
				"are there meetings next month in geneva ",
				"was there a meeting on tuesday morning ",
				"where is it ",
				"what time is the next meeting this friday ",
				"is there a meeting today three p m ",
				"is there a meeting scheduled between three p m and five p m today ",
				"was there a meeting on monday afternoon ",
				"when will it end ",
				"do i have meetings in the next two months ",
				"what meetings are being held next week ",
				"what meetings will there be next week ",
				"did i miss a meeting last week ",
				"where is the meeting ",
				"is elisabeth attending a meeting on monday ",
				"was anyone from martigny at the meeting last week ",
				"what is pierrette's phone number ",
				"is there a meeting on the ninth of july ",
				"what meetings were there in june ",
				"when is it ",
				"is there a meeting today ",
				"will there be any meetings tomorrow ",
				"who will attend ",
				"will pierrette be at that meeting ",
				"on august tenth ",
				"in which room",
				"what is the address of the meeting next week ",
				"can you list all the meetings on monday ",
				"show me all the meetings last year in geneva ",
				"was there a meeting in may ",
				"the next fifteen days ",
				"what meetings are there during the next two months ",
				"will any people from geneva be there ",
				"when is the next meeting ",
				"give me the first two meetings in september "};
				
//const static string CRLF("\r\n");

void FindAndReplace(string &input_str,
					string const &search_str, 
					string const &replace_str)
{
	string::size_type pos = 0;
	cout << "===" << input_str << endl;
	while ((pos = input_str.find(search_str, pos)) != string::npos)
	{
		cout << "pos " << pos << " search_str " << search_str << " replace_str " << replace_str << endl;
		input_str.replace(pos, search_str.size(), replace_str);
		cout << input_str << endl;
		pos = pos + replace_str.length();
	}
	
	return;
}

void ExtractOutput(string& buffer)
{
	string::size_type pos = 0;
	string search_str("");
	string replace_str("");
	char rm_buff[100];
	
	search_str = "action=tts(";	
	
	pos = buffer.find(search_str, pos);
	
	if (pos	== string::npos)
	{
		return;
	}
	
	if (pos >= 100)
 	{
 		pos = 100 - 1;
 	}
	
	cout << "1 pos " << pos << endl;
	int length = buffer.copy(rm_buff, pos, 0);
	
	if (length >= 100)
 	{
 		length = 100 - 1;
 	}
 	
 	rm_buff[length] = '\0';
 	cout << "1 length " << length << endl;
 	FindAndReplace(buffer, rm_buff, replace_str);
 	cout << "2" << endl;
 	search_str = "action=tts("; 	
	FindAndReplace(buffer, search_str, replace_str);
	cout << "3" << endl;
	search_str = ")].";	
	FindAndReplace(buffer, search_str, replace_str);

//#if _DEBUG
	cout << "[Query Ouput: " << buffer << "]" << endl;
//#endif
	
	return;
}

void ExtractOutput2(string& buffer, const string left, const string right)
{
	string::size_type pos = 0;
	string search_str("");
	string replace_str("");
	char rm_buff[100];
	
	//search_str = "'";
	pos = buffer.find(left, pos);
	
	if (pos	== string::npos)
	{
		return;
	}
	
	++pos;
	if (pos >= 100)
 	{
 		pos = 100 - 1;
 	}
	
	cout << "1 pos " << pos << endl;
	int length = buffer.copy(rm_buff, pos, 0);
	
	if (length >= 100)
 	{
 		length = 100 - 1;
 	}
 	
 	rm_buff[length] = '\0';
 	cout << "1 length " << length << endl;
 	cout << "1 buffer: " << buffer.c_str() << endl;
 	FindAndReplace(buffer, rm_buff, replace_str);
 	
 	pos = buffer.find(right, 0);
	
	if (pos >= 100)
 	{
 		pos = 100 - 1;
 	}
	
	if (pos	== string::npos)
	{
		cout << "------- " << endl;
		return;
	}
	
	length = buffer.copy(rm_buff, pos, 0);
	
	if (length >= 100)
 	{
 		length = 100 - 1;
 	}
 	
 	rm_buff[length] = '\0';
 	cout << "2 length " << length << endl;
 	cout << "2 buffer: " << buffer.c_str() << endl;
 	FindAndReplace(buffer, rm_buff, replace_str);
 	
	buffer = rm_buff;
 	
//#if _DEBUG
	cout << "[Query Ouput: " << buffer << "]" << endl;
//#endif
	
	return;
}

void ExtractOutput3(string& buffer, string& result, const string token)
{
	string::size_type pos = 0;
	string search_str("");
	string replace_str("");
	char rm_buff[100];
	
	pos = buffer.find(token, pos);
	
	if (pos	== string::npos)
	{
		return;
	}
	
	++pos;
	if (pos >= 100)
 	{
 		pos = 100 - 1;
 	}

	cout << "1 pos " << pos << endl;
	int length = buffer.copy(rm_buff, pos, 0);
	
	if (length >= 100)
 	{
 		length = 100 - 1;
 	}
 	
 	rm_buff[length] = '\0';
 	cout << "1 length " << length << endl;
 	cout << "1 rm_buff " << rm_buff << endl;
 	FindAndReplace(buffer, rm_buff, "");

	result = rm_buff;
	FindAndReplace(result, token, "");
	
//#if _DEBUG
	cout << "[Query Ouput: " << buffer << "]" << endl;
//#endif
	
	return;
}

void ExtractOutput(string& buffer, 
					const string left, const string right)
{
#if _DEBUG
	cout << endl << "---[DEBUG] [ExtractOutput()]" << endl;
#endif
		
	string::size_type pos = 0;
	string replace_str("");
	char rm_buff[100];
	
	pos = buffer.find(left, pos);
	
	if (pos	== string::npos)
	{
		return;
	}
	
	++pos;
	if (pos >= 100)
 	{
 		pos = 100 - 1;
 	}
	cout << "1 pos " << pos << endl;
	int length = buffer.copy(rm_buff, pos, 0);
	cout << "1 length " << length << endl;
	if (length >= 100)
 	{
 		length = 100 - 1;
 	}
 	cout << "2 length " << length << endl;
 	rm_buff[length] = '\0'; 	
 	FindAndReplace(buffer, rm_buff, replace_str);
 	
 	pos = buffer.find(right, 0);
 
	if (pos	== string::npos)
	{
		return;
	}
	
	if (pos >= 100)
 	{
 		pos = 100 - 1;
 	}
	cout << "2 pos " << pos << endl;
	length = buffer.copy(rm_buff, pos, 0);
	cout << "3 length " << length << endl;
	if (length >= 100)
 	{
 		length = 100 - 1;
 	}
 	cout << "4 length " << length << endl;
 	rm_buff[length] = '\0';
 	FindAndReplace(buffer, rm_buff, replace_str);
 	
	buffer = rm_buff;
		
#if _DEBUG
	cout << "---[DEBUG] [ExtractOutput()]" 
			"[Query Ouput: " << buffer << "]" << endl;
#endif
	
	return;
}

/*
class Trace
{
public:
	
	Trace(const ACE_TCHAR* prefix,
			const ACE_TCHAR* name,
			int line,
			const ACE_TCHAR* file)
	{
		this->prefix_ = prefix;
		this->name_    = name;
		this->line_    = line;
		this->file_    = file;
	
		ACE_Log_Msg* lm = ACE_LOG_MSG;
		
		if ((lm->tracing_enabled())
			&& (lm->trace_active() == 0))
		{
			lm->trace_active(1);
			
			ACE_DEBUG ((LM_TRACE,
						ACE_TEXT("%s%*s(%t) calling %s in file `%s'")
						ACE_TEXT(" on line %d\n"),
						this->prefix_,
						Trace::nesting_indent_* lm->inc(),
						ACE_TEXT(""),
						this->name_,
						this->file_,
						this->line_));
			lm->trace_active(0);
		}
		
		return;
	}
	
	void setLine (int line)
	{
		this->line_ = line;
	
		return;
	}
	
	~Trace (void)
	{
		ACE_Log_Msg* lm = ACE_LOG_MSG;
		
		if ((lm->tracing_enabled())
			&& (lm->trace_active () == 0))
		{
			lm->trace_active(1);			
			ACE_DEBUG ((LM_TRACE,
						ACE_TEXT("%s%*s(%t) leaving %s in file `%s'")
						ACE_TEXT(" on line %d\n"),
						this->prefix_,
						Trace::nesting_indent_* lm->dec(),
						ACE_TEXT(""),
						this->name_,
						this->file_,
						this->line_));
			lm->trace_active (0);
		}
		
		return;
	}
     
private:
	enum { nesting_indent_ = 3 };
	const ACE_TCHAR* prefix_;
	const ACE_TCHAR* name_;
	const ACE_TCHAR* file_;
	int line_;
};

#define TRACE_PREFIX	ACE_TEXT("TRACE ")

#if (ACE_NTRACE == 1)
#	define TRACE(X)
#	define TRACE_RETURN(V)
#	define TRACE_RETURN_VOID(V)
#else
#	define TRACE(X)								\
		Trace ____ (TRACE_PREFIX,				\
					ACE_TEXT(X),				\
					__LINE__,					\
					ACE_TEXT(__FILE__))

#	define TRACE_RETURN(V)						\
	do { ____.setLine(__LINE__); return V; } while (0)

#	define TRACE_RETURN_VOID()					\
	do { ____.setLine(__LINE__); } while (0)
#endif
*/

int main(int argc, char *argv[])
{
//	string str("[selected='will nikos attend',action=tts('meeting at nikos\'s room on january 29 meeting at nikos\'s room on february 1 and meeting at pierrette\'s room on february 3')].");
//	ExtractOutput(str, "(", ")");
//	string str("where is\nwhen is\n");
//	cout << "--- str: " << str << endl;
//	cout << "--- str.size: " << str.size() << endl;
//	FindAndReplace(str, "\n", "-");
//	string result("");
//	cout << "--- str: " << str << endl;
//	cout << "--- str.size: " << str.size() << endl;
//	cout << "--- result: " << result.c_str() << endl;
//		
//	while (str.size() > 0)
//	{
//		ExtractOutput3(str, result, "-");
//		cout << "--- str.size: " << str.size() << endl;
//		cout << "--- str: " << str << endl;
//		cout << "--- result.size: " << str.size() << endl;
//		cout << "--- result: " << result.c_str() << endl;
//	}
//	ExtractOutput3(str, result, "-");
//	cout << "--- str.size: " << str.size() << endl;
//	cout << "--- result: " << result.c_str() << endl;
//	ExtractOutput3(str, result, "-");
//	cout << "--- str.size: " << str.size() << endl;
//	cout << "--- result: " << result.c_str() << endl;
//	string str1("[selected='will nikos \'s attend',action=tts(no)].");
//	string str2("[selected='will nikos \'s attend',action=tts(no)].");
//	cout << "1 str1: " << str1.c_str() << endl;
//	cout << "1 str2: " << str2.c_str() << endl;
//	ExtractOutput2(str1, "(", ")");
//	cout << "2 str1: " << str1.c_str() << endl;	
//	ExtractOutput2(str2, "'", "',");	
//	cout << "3 str2: " << str2.c_str() << endl;
//	string buffer("what is pierrette 's address");
//	string search_str("\'");
//	string replace_str("\\\'");
//	FindAndReplace(buffer, search_str, replace_str);
	
//	string buffer("'where is next meeting\\nwhen is the next meeting in july'");
//	ostringstream obuffer("'where is next meeting\\nwhen is the next meeting in july'");
	
//	string buffer = obuffer.str();
//	cout << buffer.c_str() << endl;

//	string search_str("\\n");
//	string replace_str("\n");
//	FindAndReplace(buffer, search_str, replace_str);
	
//	cout << buffer.c_str() << endl;
	
//	string search_str("\n");
//	string replace_str("\n");
//	FindAndReplace(buffer, search_str, replace_str);
	
//	cout << buffer << endl;
	const struct tm* tm;
	size_t len;
	time_t now;
	char* s;
	
	now = time(NULL);
	tm = localtime(&now);
	
	s = new char[40];
	len = strftime(s, 40, "%Y-%m-%d.%H-%M-%S", tm);
	string str(s);
	str = "../logs/test_communicationchannel." + str + ".output";	
	delete s;

	ACE_OSTREAM_TYPE* output = 
		new std::ofstream(str.c_str());
	ACE_LOG_MSG->msg_ostream(output, 0);
	ACE_LOG_MSG->set_flags(ACE_Log_Msg::OSTREAM);
	ACE_LOG_MSG->set_flags(ACE_Log_Msg::VERBOSE_LITE);
	
	ACE_LOG_MSG->priority_mask(LM_TRACE|LM_DEBUG, ACE_Log_Msg::PROCESS);
	
	char line[100];
	string server("129.194.32.96");
	
	if (argc == 2)
	{	
		server = argv[1];
	}

	ACE_TRACE(ACE_TEXT("MAIN"));
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [Main] [--- Wait... ---]\n")));
			
	TestCommunicationChannel test;
	test.StartComms(server);
		
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [Main] [Press enter and speak (Type exit to stop)]\n")));	
//	int count = 0;


	while (true)
	{					
		cin.getline(line, 100, '\n');
		string input(line);
		
		//if ((input == "exit") || (count == 50))
		if (input == "exit")
		{
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [Main] [--- Goodbye --]\n")));
			test.StopRecording();
			test.StopComms();						
			
			return 0;
		}
		else if (input == "exit2")
		{
							
			return 0;
		}		
		else if (input == "tts")
		{			
			//test.AllowPlayback(false);
			//test.RecognizeGLM();
			//test.RecognizeFromTTS("<speak>When is the next meeting.<break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/>n.</speak>", 1);		
			test.SetConfidence(5);
			test.RecognizeFromTTS("When is the next meeting.", "calendar_slm.grammar", "en-US");
			//test.RecognizeFromTTS("When is the next meeting.", "calendar_slm.grammar", "jp");
		}
		else if (input == "tts2")
		{			
			//test.AllowPlayback(false);
			//test.RecognizeGLM();
			//test.RecognizeFromTTS("<speak>Will nikos attend.<break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/>n.</speak>", "calendar_gsl.grammar");
			test.SetConfidence(30);		
			test.RecognizeFromTTS("When is the next meeting.", "calendar_gsl.grammar", "en-US");
			//test.RecognizeFromTTS("When is the next meeting.", "calendar_gsl.grammar", "jp");
		}
		else if (input == "rec")
		{	
			//cout << endl << "--- " << corpus[count] << " ";
			//count += 1;
			//test.AllowPlayback(false);
			test.SetConfidence(5);
			test.RecognizeFromUser("calendar_slm.grammar", "en-us", true);
			//test.RecognizeFromUser("calendar_slm.grammar", "jp", true);
			//test.RecognizeGLM();			
			test.StartRecording();
		}
		else
		{	
			//cout << endl << "--- " << corpus[count] << " ";
			//count += 1;
			//test.AllowPlayback(false);
			test.SetConfidence(30);
			test.RecognizeFromUser("calendar_gsl.grammar", "en-us", true);
			//test.RecognizeFromUser("calendar_gsl.grammar", "jp", true);
			//test.RecognizeGLM();			
			test.StartRecording();
		}
	}

	
	ACE_LOG_MSG->clr_flags(ACE_Log_Msg::OSTREAM);
	delete output;
	
	ACE_Thread_Manager::instance()->wait();	





#if 0	
	char line[100];
	string server("129.194.32.96");
	
	if (argc == 2)
	{	
		server = argv[1];
	}
	
	cout << endl << "---[DEBUG] [Main]--- Wait... ----------" << endl << endl;
		
	CommunicationChannel comm_channel();
	comm_channel.SetMrcpServerName(server);
	comm_channel.SetMrcpServerPort(554);
	comm_channel.SetDlgServerName(server);
	comm_channel.SetMrcpServerPort(1985);
	comm_channel.SetRtpLocalPort(1263);
	comm_channel.StartComms();
	
	cout << endl << "---[DEBUG] [Main]--- Press enter and speak (Type exit to stop): ";
	
	while (true)
	{					
		cin.getline(line, 100, '\n');
		string input(line);
		
		if (input == "exit")
		{	
			/*cout << "---[DEBUG] [Main]--------------------- " << endl;
			cout << "---[DEBUG] [Main]--------------------- " << endl;
			cout << "---[DEBUG] [Main]--------------------- " << endl;
			cout << "---[DEBUG] [Main]--- Stop speaking --- " << endl;
			cout << "---[DEBUG] [Main]--------------------- " << endl;
			cout << "---[DEBUG] [Main]--------------------- " << endl;
			cout << "---[DEBUG] [Main]--------------------- " << endl;*/
			comm_channel.StopRecording();
			//comm_channel.StopComms();	
			cout << endl << "---[DEBUG] [Main]--- Goodbye ----------" << endl << endl;
			
			return 0;
		}	
		else
		{	
			comm_channel.RecognizeGLM();
			/*ACE_OS::sleep(1);
			
			cout << "---[DEBUG] [Main]--------------------- " << endl;
			cout << "---[DEBUG] [Main]--------------------- " << endl;
			cout << "---[DEBUG] [Main]--------------------- " << endl;		
			cout << "---[DEBUG] [Main]--- Start speaking ---" << endl;
			cout << "---[DEBUG] [Main]--------------------- " << endl;
			cout << "---[DEBUG] [Main]--------------------- " << endl;
			cout << "---[DEBUG] [Main]--------------------- " << endl;
			*/
			comm_channel.StartRecording();
		}
	}
	
	ACE_Thread_Manager::instance()->wait();	
#endif
	
#if 0
	long loops;
	int rc;
	int size;
	snd_pcm_t *handle;
	snd_pcm_hw_params_t *params;
	unsigned int val;
	int dir;
	snd_pcm_uframes_t frames;
	char *buffer;
	
	/* Open PCM device for recording (capture). */
	rc = snd_pcm_open(&handle, "default",
						SND_PCM_STREAM_CAPTURE, 0);
	
	if (rc < 0) 
	{
		fprintf(stderr, 
				"unable to open pcm device: %s\n",
				snd_strerror(rc));
		exit(1);
	}
	
	/* Allocate a hardware parameters object. */
	snd_pcm_hw_params_alloca(&params);
		
	/* Fill it in with default values. */
	rc = snd_pcm_hw_params_any(handle, params);
	
	if (rc < 0) 
	{
		fprintf(stderr, 
				"unable to fill in parameter object with default values: %s\n",
				snd_strerror(rc));
		exit(1);
	}
	
	/* Set the desired hardware parameters. */
	/* Interleaved mode */
	rc = snd_pcm_hw_params_set_access(handle, params,
										SND_PCM_ACCESS_RW_INTERLEAVED);
	
	if (rc < 0) 
	{
		fprintf(stderr, 
				"unable to set interleaved mode: %s\n",
				snd_strerror(rc));
		exit(1);
	}
	
	/* Signed 16-bit little-endian format */
	rc = snd_pcm_hw_params_set_format(handle, params,
										SND_PCM_FORMAT_MU_LAW);
	
	if (rc < 0) 
	{
		fprintf(stderr, 
				"unable to set signed 16-bit little-endian format: %s\n",
				snd_strerror(rc));
		exit(1);
	}
	
	/* One channel (mono) */
	rc = snd_pcm_hw_params_set_channels(handle, params, 1);
	
	if (rc < 0) 
	{
		fprintf(stderr, 
				"unable to set one channel: %s\n",
				snd_strerror(rc));
		exit(1);
	}
	
	/* 8000 bits/second sampling rate */
	val = 8000;	
	rc = snd_pcm_hw_params_set_rate_near(handle, params,
											&val, &dir);
	
	fprintf(stderr, "val: %d, dir: %d\n", val, dir);
	
	if (rc < 0) 
	{
		fprintf(stderr, 
				"unable to set 8000 bits/second sampling rate: %s\n",
				snd_strerror(rc));
		exit(1);
	}
	
	/* Set period size to 80 frames. */
	frames = 160;
	rc = snd_pcm_hw_params_set_period_size_near(handle,
											params, &frames, &dir);
	
	fprintf(stderr, "frames: %d, dir: %d\n", frames, dir);
	
	if (rc < 0) 
	{
		fprintf(stderr, 
				"unable to set period size to 80 frames: %s\n",
				snd_strerror(rc));
		exit(1);
	}
	
	/* Write the parameters to the driver */
	rc = snd_pcm_hw_params(handle, params);
	
	if (rc < 0) 
	{
		fprintf(stderr,
				"unable to set hw parameters: %s\n",
				snd_strerror(rc));
		
		exit(1);
	}

	/* Use a buffer large enough to hold one period */
	rc = snd_pcm_hw_params_get_period_size(params,
											&frames, &dir);
	
	if (rc < 0) 
	{
		fprintf(stderr, 
				"unable to get period size: %s\n",
				snd_strerror(rc));
		exit(1);
	}
	
	size = frames * 1; /* 2 bytes/sample, 1 channel */
	buffer = (char *) malloc(size);
	fprintf(stderr, "size: %d, frames: %d\n", size, frames);
	
	/* We want to loop for 5 seconds */
	rc = snd_pcm_hw_params_get_period_time(params,
											&val, &dir);
	
	fprintf(stderr, "val: %d, dir: %d\n", val, dir);
	
	if (rc < 0) 
	{
		fprintf(stderr, 
				"unable to get period: %s\n",
				snd_strerror(rc));
		exit(1);
	}
	
	loops = 5000000 / val;
	fprintf(stderr, "loops: %d, val: %d\n", loops, val);
	
	while (loops > 0) 
	{
		loops--;
		rc = snd_pcm_readi(handle, buffer, frames);
    
		if (rc == -EPIPE) 
		{
			/* EPIPE means overrun */
			fprintf(stderr, "overrun occurred\n");
			snd_pcm_prepare(handle);
		} 
		else if (rc < 0) 
		{
			fprintf(stderr,
					"error from read: %s\n",
					snd_strerror(rc));
		} 
		else if (rc != (int)frames) 
		{
			fprintf(stderr, "short read, read %d frames\n", rc);
		}

		rc = write(1, buffer, size);
		
		if (rc != size)
		{
			fprintf(stderr,
					"short write: wrote %d bytes\n", rc);
		}
	}

	snd_pcm_drain(handle);
	snd_pcm_close(handle);
	free(buffer);

#endif
#if 0
	long loops;
	int rc;
	int size;
	snd_pcm_t *handle;
	snd_pcm_hw_params_t *params;
	unsigned int val;
	int dir;
	snd_pcm_uframes_t frames;
	Uint8 *buffer;
	/* Open PCM device for playback. */
	rc = snd_pcm_open(&handle, "default",
						SND_PCM_STREAM_PLAYBACK, 0);
	
	if (rc < 0) 
	{
		fprintf(stderr,
				"unable to open pcm device: %s\n",
				snd_strerror(rc));
		
		exit(1);
	}
	
	/* Allocate a hardware parameters object. */
	snd_pcm_hw_params_alloca(&params);
	
	/* Fill it in with default values. */
	rc = snd_pcm_hw_params_any(handle, params);
	
	if (rc < 0) 
	{
		fprintf(stderr, 
				"unable to fill in parameter object with default values: %s\n",
				snd_strerror(rc));
		exit(1);
	}
	/* Set the desired hardware parameters. */
	/* Interleaved mode */
	rc = snd_pcm_hw_params_set_access(handle, params,
										SND_PCM_ACCESS_RW_INTERLEAVED);
									
	if (rc < 0) 
	{
		fprintf(stderr, 
				"unable to set interleaved mode: %s\n",
				snd_strerror(rc));
		exit(1);
	}
	
	/* Signed 16-bit little-endian format */
	rc = snd_pcm_hw_params_set_format(handle, params,
										SND_PCM_FORMAT_MU_LAW);
	if (rc < 0) 
	{
		fprintf(stderr, 
				"unable to set signed 16-bit little-endian format: %s\n",
				snd_strerror(rc));
		exit(1);
	}
	
	/* One channel (mono) */
	rc = snd_pcm_hw_params_set_channels(handle, params, 1);
	
	if (rc < 0) 
	{
		fprintf(stderr, 
				"unable to set one channel: %s\n",
				snd_strerror(rc));
		exit(1);
	}
	
	/* 8000 bits/second sampling rate */
	val = 8000;
	rc = snd_pcm_hw_params_set_rate_near(handle, params, &val, &dir);
	fprintf(stderr, "val: %d, dir: %d\n", val, dir);
	if (rc < 0) 
	{
		fprintf(stderr, 
				"unable to set 8000 bits/second sampling rate: %s\n",
				snd_strerror(rc));
		exit(1);
	}
	
	/* Set period size to 80 frames. */
	frames = 160;
	//rc = snd_pcm_hw_params_set_period_size_near(handle,
	//											params, &frames, &dir);
	fprintf(stderr, "frames: %d, dir: %d\n", frames, dir);										
	if (rc < 0) 
	{
		fprintf(stderr, 
				"unable to set period size to 80 frames: %s\n",
				snd_strerror(rc));
		exit(1);
	}
	
	/* Write the parameters to the driver */
	rc = snd_pcm_hw_params(handle, params);

	if (rc < 0) 
	{
		fprintf(stderr,
				"unable to set hw parameters: %s\n",
				snd_strerror(rc));
		
		exit(1);
	}
	
	/* Use a buffer large enough to hold one period */
	//rc = snd_pcm_hw_params_get_period_size(params, &frames, &dir);
	
	if (rc < 0) 
	{
		fprintf(stderr, 
				"unable to get period size: %s\n",
				snd_strerror(rc));
		exit(1);
	}
	
	size = frames * 1; /* 2 bytes/sample, 1 channels */
	buffer = (Uint8 *) malloc(size);
	fprintf(stderr, "size: %d, frames: %d\n", size, frames);
	
	/* We want to loop for 5 seconds */
	rc = snd_pcm_hw_params_get_period_time(params, &val, &dir);
	fprintf(stderr, "val: %d, dir: %d\n", val, dir);
	if (rc < 0) 
	{
		fprintf(stderr, 
				"unable to get period: %s\n",
				snd_strerror(rc));
		exit(1);
	}
	val = 20000;
	/* 5 seconds in microseconds divided by
	* period time */
	loops = 5000000 / val;
	fprintf(stderr, "loops: %d, val: %d\n", loops, val);
	
	while (loops > 0) 
	{
		loops--;
		rc = read(0, buffer, size);
    
		if (rc == 0) 
		{
			fprintf(stderr, "end of file on input\n");
			break;
		} 
		else if (rc != size) 
		{
			fprintf(stderr,
					"short read: read %d bytes\n", rc);
		}

		rc = snd_pcm_writei(handle, buffer, frames);
		
		if (rc == -EPIPE) 
		{
			/* EPIPE means underrun */
			fprintf(stderr, "underrun occurred\n");
			snd_pcm_prepare(handle);
		} 
		else if (rc < 0) 
		{
			fprintf(stderr,
					"error from writei: %s\n",
			snd_strerror(rc));
		}  
		else if (rc != (int)frames) 
		{
			fprintf(stderr,
					"short write, write %d frames\n", rc);
		}
	}
	
	snd_pcm_drain(handle);
	snd_pcm_close(handle);
	free(buffer);
#endif
#if 0
	int rc;
	snd_pcm_t *handle;
	snd_pcm_hw_params_t *params;
	unsigned int val, val2;
	int dir;
	snd_pcm_uframes_t frames;
	
	/* Open PCM device for playback. */
	rc = snd_pcm_open(&handle, "default",
						SND_PCM_STREAM_PLAYBACK, 0);
	
	if (rc < 0) 
	{
		fprintf(stderr, 
				"unable to open pcm device: %s\n",
				snd_strerror(rc));
		
		exit(1);
	}

	/* Allocate a hardware parameters object. */
	snd_pcm_hw_params_alloca(&params);
	/* Fill it in with default values. */
	snd_pcm_hw_params_any(handle, params);
	/* Set the desired hardware parameters. */
	/* Interleaved mode */
	snd_pcm_hw_params_set_access(handle, params,
								 SND_PCM_ACCESS_RW_INTERLEAVED);
	/* Signed 16-bit little-endian format */
	snd_pcm_hw_params_set_format(handle, params,
								 SND_PCM_FORMAT_S16_LE);
	/* Two channels (stereo) */
	snd_pcm_hw_params_set_channels(handle, params, 1);
	/* 44100 bits/second sampling rate (CD quality) */
	val = 8000;
	snd_pcm_hw_params_set_rate_near(handle,
									params, &val, &dir);
	
	/* Write the parameters to the driver */
	rc = snd_pcm_hw_params(handle, params);
  
	if (rc < 0) 
	{
		fprintf(stderr,
				"unable to set hw parameters: %s\n",
				snd_strerror(rc));
		
		exit(1);
	}
	
	/* Display information about the PCM interface */
	printf("PCM handle name = '%s'\n",
			snd_pcm_name(handle));
	printf("PCM state = %s\n",
			snd_pcm_state_name(snd_pcm_state(handle)));
	
	snd_pcm_hw_params_get_access(params,
								 (snd_pcm_access_t *) &val);
	printf("access type = %s\n",
			snd_pcm_access_name((snd_pcm_access_t)val));
	
	snd_pcm_hw_params_get_format(params, (snd_pcm_format_t*)&val);
	
	printf("format = '%s' (%s)\n",
			snd_pcm_format_name((snd_pcm_format_t)val),
			snd_pcm_format_description((snd_pcm_format_t)val));
	
	snd_pcm_hw_params_get_subformat(params,
									(snd_pcm_subformat_t *)&val);
	
	printf("subformat = '%s' (%s)\n",
			snd_pcm_subformat_name((snd_pcm_subformat_t)val),
    		snd_pcm_subformat_description((snd_pcm_subformat_t)val));
  
	snd_pcm_hw_params_get_channels(params, &val);
	printf("channels = %d\n", val);
	snd_pcm_hw_params_get_rate(params, &val, &dir);
	printf("rate = %d bps\n", val);
	snd_pcm_hw_params_get_period_time(params, &val, &dir);
	printf("period time = %d us\n", val);
	snd_pcm_hw_params_get_period_size(params,&frames, &dir);
	printf("period size = %d frames\n", (int)frames);
	snd_pcm_hw_params_get_buffer_time(params, &val, &dir);
	printf("buffer time = %d us\n", val);
	snd_pcm_hw_params_get_buffer_size(params,
										(snd_pcm_uframes_t *) &val);
	
	printf("buffer size = %d frames\n", val);
	snd_pcm_hw_params_get_periods(params, &val, &dir);
	printf("periods per buffer = %d frames\n", val);
	snd_pcm_hw_params_get_rate_numden(params, &val, &val2);
	printf("exact rate = %d/%d bps\n", val, val2);
	val = snd_pcm_hw_params_get_sbits(params);
	printf("significant bits = %d\n", val);
	snd_pcm_hw_params_get_tick_time(params, &val, &dir);
	
	printf("tick time = %d us\n", val);
	val = snd_pcm_hw_params_is_batch(params);
	printf("is batch = %d\n", val);
	val = snd_pcm_hw_params_is_block_transfer(params);
	printf("is block transfer = %d\n", val);
	val = snd_pcm_hw_params_is_double(params);
	printf("is double = %d\n", val);
	val = snd_pcm_hw_params_is_half_duplex(params);
	printf("is half duplex = %d\n", val);
	val = snd_pcm_hw_params_is_joint_duplex(params);
	printf("is joint duplex = %d\n", val);
	val = snd_pcm_hw_params_can_overrange(params);
	printf("can overrange = %d\n", val);
	val = snd_pcm_hw_params_can_mmap_sample_resolution(params);
	printf("can mmap = %d\n", val);
	val = snd_pcm_hw_params_can_pause(params);
	printf("can pause = %d\n", val);
	val = snd_pcm_hw_params_can_resume(params);
	printf("can resume = %d\n", val);
	val = snd_pcm_hw_params_can_sync_start(params);
	printf("can sync start = %d\n", val);
	snd_pcm_close(handle);
#endif		
/*	cout << "---[DEBUG] [RTSP]--- Test module RTSP.\n";
	cout << "---[DEBUG] [RTSP]--- Test started.\n";
	
	ostringstream os;

    os << "RTSP/1.0 200 OK" << CRLF;
    os << "CSeq: " << "1" << CRLF;
    os << "Content-Length: 220" << CRLF;
    os << "Content-Type: application/sdp" << CRLF;

    os << CRLF;

	string msg = os.str();
	
	cout << "---[DEBUG] [RTSP]--- Parse message: " << msg.c_str() << "\n\n";
	
	RtspRequest* req = new RtspRequest();
	req->Parse(msg);
	
	cout << "---[DEBUG] [RTSP]--- CSeq: " << req->GetCSeq() << "\n";	
	cout << "---[DEBUG] [RTSP]--- RTSP test completed succefully.\n";

	string resource;
*/	
//	ListBuffer<Uint8, 2048> RTPDataBuffer;
//	RTPDataBuffer.size();
//	ByteBuffer m_data;
//	m_data.Size();
	
/*!	DialogueController dlg_("129.194.32.96", 1985);
	dlg_.StartComms();
	ACE_OS::sleep(1);
*/	
//	string text = "action(process_rec_string('what meetings are there next week')).\n";
#if 0	
	MrcpController mrcp_("129.194.32.96", 554, 1263);
	mrcp_.StartComms();
		
/*	MrcpController mrcp2_("", "129.194.32.96", 551);
	mrcp2_.StartComms();
*/

/*	mrcp_.SendDescribe(g_recognizer);
	ACE_OS::sleep(2);
*/	mrcp_.SendSetup(g_recognizer);
	ACE_OS::sleep(2);

	RtpTransceiver rtp_("129.194.32.96", mrcp_.GetServerRtpPort(), 1263);
//	RtpTransceiver rtp_("129.194.32.96", 1821, 1263);

	RTPPayloadFormat format = {1, 160, 1, 8000.0};
	rtp_.SetSyncSource(0);
	rtp_.SetPayloadFormat(format);
	rtp_.SetPayloadType(RTP_PAYLOAD_PCMU);

	rtp_.StartComms();
	ACE_OS::sleep(2);

	mrcp_.SendSetup(g_synthesizer);
	ACE_OS::sleep(2);

	mrcp_.SendDefineGrammar( G_speech_digit_grammar, "digit.grammar", "application/grammar+xml", "" );
//	mrcp_.SendDefineGrammar( "http://192.33.226.245:8080/cbas/grammar.grxml", "digit.grammar", "text/uri-list", "" );
//	mrcp_.SendDefineGrammar( "c:/grammar.grxml", "digit.grammar", "text/uri-list", "" );
//	mrcp_.SendDefineGrammar( "http://129.194.32.96:9080/Calendar/grammars/recogniser.grammar", "calendar.grammar", "text/uri-list", "" );
	ACE_OS::sleep(2);

//	mrcp_.SendGetParams("");
//	ACE_OS::sleep(2);

	ostringstream os;
	os << "session:digit.grammar" << CRLF;
//	os << "session:calendar.grammar" << CRLF;
	string uri_list = os.str();
	os.str("");
	os << "Confidence-Threshold: 50" << CRLF;
	os << "Sensitivity-Level: 50" << CRLF;
	os << "Speed-Vs-Accuracy: 50" << CRLF;
	os << "Speech-Complete-Timeout: 300" << CRLF;
	os << "Speech-Incomplete-Timeout: 500" << CRLF;
	os << "No-Input-Timeout: 10000" << CRLF;
	string parameters = os.str();
	
/*	mrcp_.SendSetParams("Vendor-Specific-Parameters:audio.rtp.SessionState=idle");
	ACE_OS::sleep(2);
	mrcp_.SendSetParams("Vendor-Specific-Parameters:audio.rtp.RecordPayloadSize=20");
	ACE_OS::sleep(2);
	mrcp_.SendSetParams("Vendor-Specific-Parameters:audio.rtp.AudioDetectionTimeout=2");
	ACE_OS::sleep(2);
	mrcp_.SendGetParams("Vendor-Specific-Parameters:audio.rtp.Streams;audio.rtp.AudioDetectionTimeout;audio.rtp.RecordPayloadSize");
	ACE_OS::sleep(2);
	mrcp_.SendSetParams("Vendor-Specific-Parameters:audio.rtp.SessionState=live");
	ACE_OS::sleep(2);
	mrcp_.SendSpeak("<speak>a</speak>");
	ACE_OS::sleep(4);*/
/*	mrcp_.SendGetParams("Vendor-Specific-Parameters:audio.rtp.Streams;audio.rtp.AudioDetectionTimeout;audio.rtp.RecordPayloadSize;audio.rtp.SessionState");
	ACE_OS::sleep(2);
	mrcp_.SendSpeak("<speak>a</speak>");
	ACE_OS::sleep(4);*/
	
	cout << endl;
	cout << "---[DEBUG] [********************]---" << endl;
	cout << "---[DEBUG] [********************]---" << endl;
	cout << "---[DEBUG] [********************]---" << endl;
	cout << "---[DEBUG] [********************]---" << endl;
	cout << "---[DEBUG] [********************]---" << endl;
	cout << "---[DEBUG] [Speak!]---" << endl;
	cout << "---[DEBUG] [********************]---" << endl;
	cout << "---[DEBUG] [********************]---" << endl;
	cout << "---[DEBUG] [********************]---" << endl;
	cout << "---[DEBUG] [********************]---" << endl;
	cout << "---[DEBUG] [********************]---" << endl;
	
	rtp_.CaptureAudio();
		
	char line[100];
	
	while (true)
	{
		cout << "---[DEBUG] [Main]--- Input (Type exit to stop): ";
		cin.getline(line, 100, '\n');
		string input(line);
		
		if (input == "exit")
		{			
			cout << "---[DEBUG] [Main]--- Teardown ..." << endl;
			
			/*mrcp_.SendSetParams("Vendor-Specific-Parameters:audio.rtp.SessionState=idle");
			ACE_OS::sleep(2);*/
			mrcp_.SendTeardown(g_recognizer);
			ACE_OS::sleep(1);
			mrcp_.SendTeardown(g_synthesizer);
			ACE_OS::sleep(1);
			rtp_.StopComms();
			rtp_.CloseAudio();
			//ACE_OS::sleep(1);
	//!		dlg_.StopComms();			
			//ACE_OS::sleep(1);	
			mrcp_.StopComms();			
			//ACE_OS::sleep(1);			
			
			return 0;
		}
		
		/*mrcp_.SendSpeak("<speak><break strength=\"x-strong\"/>one six four four <break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/>s.</speak>");*/
		/*mrcp_.SendSpeak("<speak>one two three</speak>");*/
//!		mrcp_.SendSpeak("<speak>" + input + "<break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/>s.</speak>");
//		mrcp_.SendSpeak("<speak>" + input + "</speak>");		
//		ACE_OS::sleep(4);

		if (input == "1")
		{
			mrcp_.SendSpeak("<speak>a</speak>");
			ACE_OS::sleep(4);
		}
			
/*		cout << endl;
		cout << "---[DEBUG] [********************]---" << endl;
		cout << "---[DEBUG] [********************]---" << endl;
		cout << "---[DEBUG] [********************]---" << endl;
		cout << "---[DEBUG] [********************]---" << endl;
		cout << "---[DEBUG] [********************]---" << endl;
		cout << "---[DEBUG] [Speak!]---" << endl;
		cout << "---[DEBUG] [********************]---" << endl;
		cout << "---[DEBUG] [********************]---" << endl;
		cout << "---[DEBUG] [********************]---" << endl;
		cout << "---[DEBUG] [********************]---" << endl;
		cout << "---[DEBUG] [********************]---" << endl;
		
		rtp_.CaptureAudio();*/
			
		mrcp_.SendRecognize(uri_list, parameters);
		ACE_OS::sleep(4);	

		rtp_.SendPackets();
//		rtp_.ClearPacketsBuffer();
		ACE_OS::sleep(4);
		//cout << "---[DEBUG] [Main]---" << endl << mrcp_.GetMessageBody() << endl;
		
	//!	dlg_.SendRecognitionText(input);		
	//!	ACE_OS::sleep(2);
		
	//!	dlg_.SendHelpRequest(input);
	}

/*	mrcp_.SendTeardown(g_recognizer);
	ACE_OS::sleep(2);
	mrcp_.SendTeardown(g_synthesizer);
	ACE_OS::sleep(2);
*/
	ACE_Thread_Manager::instance()->wait();
#endif
/*	int tmp;
	cin >> tmp;
	
	cout << "---[DEBUG] [Main]--- Teardown ..." << endl;
		
	mrcp_.SendTeardown(g_recognizer);
	ACE_OS::sleep(2);
	mrcp_.SendTeardown(g_synthesizer);
	ACE_OS::sleep(2);
	rtp_.StopComms();
	ACE_OS::sleep(2);
	mrcp_.StopComms();
	ACE_OS::sleep(2);*/
/*	
	mrcp_.SendSetup(g_synthesizer);
	
	mrcp_.SendSetParams("audio.rtp.LocalAddress 1343");
			
	mrcp_.SendGetParams("audio.rtp.LocalAddress");
	
	ControllerRegulus reg_("129.194.32.96", 555);
	
	reg_.StartComms();
	reg_.SendHello();
*/

	return 0;
}


/*#include <hildon-widgets/hildon-program.h>
#include <gtk/gtkmain.h>
#include <gtk/gtkbutton.h>
*/

	// Create needed variables
/*	HildonProgram *program;
	HildonWindow *window;
//	GtkWidget *button;
	
	// Initialize the GTK.
	gtk_init(&argc, &argv);
	
	// Create the hildon program and setup the title
	program = HILDON_PROGRAM(hildon_program_get_instance());
	g_set_application_name("MedSLTMobile CommunicationChannel ");
	
	// Create HildonWindow and set it to HildonProgram
	window = HILDON_WINDOW(hildon_window_new());
	hildon_program_add_window(program, window);
	
	// Create button and add it to main view
//	button = gtk_button_new_with_label("Hello!");
//	gtk_container_add(GTK_CONTAINER(window), button);
	
	// Connect signal to X in the upper corner
	g_signal_connect(G_OBJECT(window), "delete_event",
	G_CALLBACK(gtk_main_quit), NULL);
	
	// Begin the main application
	gtk_widget_show_all(GTK_WIDGET(window));
	gtk_main();
*/
