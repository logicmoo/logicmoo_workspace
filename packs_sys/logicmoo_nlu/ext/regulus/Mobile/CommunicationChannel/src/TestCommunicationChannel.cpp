/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	TestCommunicationChannel.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include "TestCommunicationChannel.hpp"

TestCommunicationChannel::TestCommunicationChannel()
	: 	m_result(NULL),
		m_online(true),
		m_recognize_from_tts(false)
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::TestCommunicationChannel()]"));
	
	m_comm_channel = NULL;
	
	// GUI
	// Only first MRCP server (1000)
	m_comm_channel = new CommunicationChannel(*this, 0x08);

	// Calendar without text input
	// All without translation, back translation, second MRCP and TCP servers (1001)
	//m_comm_channel = new CommunicationChannel(*this, 0x9);

	// Calendar with text input
	// All without translation, back translation and TCP servers (11001)
	//m_comm_channel = new CommunicationChannel(*this, 0x19);

	// MedSLT unidirectional
	// All without dialogue, second MRCP and TCP servers (1110)
	//m_comm_channel = new CommunicationChannel(*this, 0xE);

	// MedSLT bidirectional doctor
	// All without dialogue and second MRCP (101110) 
	//m_comm_channel = new CommunicationChannel(*this, 0x2E);

	// MedSLT bidirectional patient
	// All without dialogue, first MRCP and TCP servers (1001110)
	//m_comm_channel = new CommunicationChannel(*this, 0x4E);
	
	// TCP server only (100000)
	//m_comm_channel = new CommunicationChannel(*this, 0x20);
	
	// Dialogue server only (1)
	//m_comm_channel = new CommunicationChannel(*this, 0x1);
	
	// ALL except TCP client (111111)
	//m_comm_channel = new CommunicationChannel(*this, 0x3F);	
		
	return;
}

TestCommunicationChannel::~TestCommunicationChannel()
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::~TestCommunicationChannel()]"));
		
	if (m_comm_channel != NULL)
	{
		delete m_comm_channel;
	}
	
	return;
}

void TestCommunicationChannel::StartComms(string server)
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::StartComms()]"));
	
	if (m_comm_channel != NULL)
	{
		m_comm_channel->SetMrcpServerName(server);
		m_comm_channel->SetMrcpServerPort(554);
		m_comm_channel->SetDlgServerName(server);
		m_comm_channel->SetDlgServerPort(1985);
		//m_comm_channel->SetDlgServerPort(7310);
		m_comm_channel->SetRtpLocalPort(1263);
		m_comm_channel->StartComms();		
	}
	
	return;
}

void TestCommunicationChannel::StopComms()
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::StopComms()]"));
	
	if (m_comm_channel != NULL)
	{
		m_comm_channel->StopComms();		
	}
	
	return;
}

void TestCommunicationChannel::SetRecGrammars()
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::SetRecGrammars()]"));

	//RecognitionGrammar gram1("SLM:public [MAIN_SLM_ENG]", "calendar_slm.grammar", "application/x-nuance-gsl", "");
	//RecognitionGrammar gram2("http://" + m_comm_channel->GetMrcpServerName() + ":9080/Calendar/grammars/recogniser_eng.grammar#MAIN", "calendar_gsl.grammar", "text/uri-list", "");
	RecognitionGrammar gram1("SLM:public [MAIN_SLM_ENG]", "calendar_slm.grammar", "application/x-nuance-gsl", "");
	RecognitionGrammar gram2("http://" + m_comm_channel->GetMrcpServerName() + ":9080/MedSLT/grammars/recogniser_eng.grammar#MAIN", "calendar_gsl.grammar", "text/uri-list", "");
	//RecognitionGrammar gram1("SLM:public [MAIN_SLM_JAP]", "calendar_slm.grammar", "application/x-nuance-gsl", "");
	//RecognitionGrammar gram2("http://" + m_comm_channel->GetMrcpServerName() + ":9080/Calendar/grammars/japanese_recogniser.grammar#MAIN", "calendar_gsl.grammar", "text/uri-list", "");
	//RecognitionGrammar gram2("http://" + m_comm_channel->GetMrcpServerName() + ":9080/MedSLT/grammars/recogniser.grammar#MAIN", "calendar_gsl.grammar", "text/uri-list", "");
	m_comm_channel->SetGrammar(gram1);
	m_comm_channel->SetGrammar(gram2);

	return;
}

void TestCommunicationChannel::StartRecording()
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::StartRecording()]"));
	
	if (m_comm_channel != NULL)
	{
		m_comm_channel->StartRecording();
	}
	
	return;
}

void TestCommunicationChannel::StopRecording()
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::StopRecording()]"));
	
	if (m_comm_channel != NULL)
	{
		m_comm_channel->StopRecording();
	}
	
	return;
}

void TestCommunicationChannel::AllowPlayback(bool choice)
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::AllowPlayback()]"));
	
	if (m_comm_channel != NULL)
	{
		m_comm_channel->AllowPlayback(choice);
	}
	
	return;
}

void TestCommunicationChannel::PlayResult()
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::PlayResult()]"));
	
	//m_result = "Result received.";
	
	if ((m_comm_channel != NULL) && (m_result != NULL))
	{
		m_comm_channel->SpeakSSML("<?xml version=\"1.0\"?><speak><audio src=\"http://" 
									+ m_comm_channel->GetMrcpServerName() 
									+ ":9080/Calendar/audio/blip.wav\"></audio><break/>" 
									+ m_result->GetQueryOutput() + "</speak>", "en-US");
	}
		
	m_result = NULL;
	
	return;
}

void TestCommunicationChannel::FindAndReplace(string &input_str,
												string const &search_str, 
												string const &replace_str)
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::FindAndReplace()]"));
	
	string::size_type pos = 0;
	
	while ((pos = input_str.find(search_str, pos)) != string::npos)
	{		
		input_str.replace(pos, search_str.size(), replace_str);
		pos = pos + replace_str.length();
	}
	
	return;
}

void TestCommunicationChannel::SplitHelpExamples(string& buffer, 
								string& result, const string token)
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::SplitHelpExamples()]"));
	
	string::size_type pos = 0;
	string replace_str("");
	char rm_buff[150];
	
	pos = buffer.find(token, pos);
	
	if (pos	== string::npos)
	{
		return;
	}
	
	++pos;
		
	int length = buffer.copy(rm_buff, pos, 0);
 	rm_buff[length] = '\0';
 	FindAndReplace(buffer, rm_buff, "");

	result = rm_buff;
	FindAndReplace(result, token, "");
	
	return;
}

void TestCommunicationChannel::RecognizeFromUser(const string& grammar, 
													const string& lang, bool online)
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::RecognizeFromUser()]"));
	
	m_online = online;
	SetRecognizeFromTTS(false);
	
	if (m_comm_channel != NULL)
	{
		m_comm_channel->RecognizeFromUser(grammar, lang, online);
	}
	
	return;
}
	
void TestCommunicationChannel::RecognizeFromTTS(const string& input, 
													const string& grammar, const string& lang)
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::RecognizeFromTTS()]"));
	
	m_online = true;
	SetRecognizeFromTTS(true);

	if (m_comm_channel != NULL)
	{
		m_comm_channel->RecognizeFromTTS(input, grammar, lang);
	}
	
	return;
}

void TestCommunicationChannel::SetRecognizeFromTTS(bool choice)
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::SetRecognizeFromTTS()]"));
	
	m_recognize_from_tts = choice;
	
	return;
}

bool TestCommunicationChannel::IsRecognizeFromTTS()
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::IsRecognizeFromTTS()]"));
		
	return m_recognize_from_tts;
}

void TestCommunicationChannel::SetConfidence(int value)
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::SetConfidence()]"));
		
	if (m_comm_channel != NULL)
	{
		m_comm_channel->SetConfidence(value);
	}
		
	return;
}
	
int TestCommunicationChannel::GetConfidence()
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::GetConfidence()]"));
	
	if (m_comm_channel != NULL)
	{
		return m_comm_channel->GetConfidence();
	}
	else
	{
		return -1;
	}
}
void TestCommunicationChannel::InitCompleted(CCStatus status)
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::InitCompleted()]"));
	
	if (m_comm_channel != NULL)
	{
		switch (status)
		{
			case CC_CONNECTED:
				SetRecGrammars();
				//m_comm_channel->SpeakSSML("<?xml version=\"1.0\"?><speak><audio src=\"http://" + m_comm_channel->GetMrcpServerName() + ":9080/Calendar/audio/blip.wav\"></audio><break/>Welcome.</speak>", "en-US");				
				m_comm_channel->SpeakSSML("<?xml version=\"1.0\"?><speak><break/>Bienvenue mon ami.</speak>", "fr-CA");
				//m_comm_channel->Speak("Bienvenue mon ami.", "fr-ca");
				break;
			default:
				ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [TestCommunicationChannel::InitCompleted()] [Error]")));
		}
	}
	
	return;
}

void TestCommunicationChannel::StartOfSpeech()
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::StartOfSpeech()]"));
	
	return;
}

void TestCommunicationChannel::RecognitionCompleted(int status, string& result)
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::RecognitionCompleted()]"));
	
	/*!ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [TestCommunicationChannel::RecognitionCompleted()] "
				"[Status: %d] [Result: %s]\n"), status, result.c_str()));
		*/
	// NO_MATCH
	if (status == 1)
	{
		cout << "Ok!" << endl;
		m_comm_channel->Speak("Ok!", "en-US");
		
/*!		if (m_online == true)
		{ 								
			if (m_comm_channel != NULL)
			{
				//m_comm_channel->SpeakSSML("<?xml version=\"1.0\"?><speak><audio src=\"http://129.194.32.96:9080/Calendar/audio/beep.wav\"></audio><break/></speak>");
				//m_comm_channel->Speak("No match.");
				
				m_online = false;
				SetConfidence(5);
				RecognizeFromUser("calendar_slm.grammar", m_online);				
			}	
		}
		else
		{	
			if (m_comm_channel != NULL)
			{
				m_online = true;
				
				PlayResult();
				
				//m_comm_channel->SpeakSSML("<?xml version=\"1.0\"?><speak><audio src=\"http://129.194.32.96:9080/Calendar/audio/beep.wav\"></audio><break/>No match.</speak>");
				//m_comm_channel->Speak("No match.");
			}			
		}*/
	}
	// E.g. NO_SPEECH_TIMEOUT
	else if (status == 2)
	{	
		cout << "Repeat please!" << endl;
		m_comm_channel->Speak("Repeat please!", "en-US");
		/*!
		if (m_online == true)
		{ 
			if (m_comm_channel != NULL)
			{
				m_comm_channel->SpeakSSML("<?xml version=\"1.0\"?><speak><audio src=\"http://" + m_comm_channel->GetMrcpServerName() + ":9080/Calendar/audio/beep.wav\"></audio><break/>Repeat please.</speak>", "en-US");				
			}						
		}
		else
		{
			m_online = true;
			
			PlayResult();
		}*/
	}
	else
	{	
		cout << "Ok!" << endl;
		m_comm_channel->Speak("Ok!", "en-US");
		/*!
		if (m_online == true)
		{ 	
			m_online = false;
			
			if (m_comm_channel != NULL)
			{
				m_comm_channel->GetQueryData(result);
				SetConfidence(5);		
				RecognizeFromUser("calendar_slm.grammar", m_online);
			}		
		}
		else
		{
			m_online = true;
			SetRecognizeFromTTS(false);
			
			if (m_comm_channel != NULL)
			{	
				m_comm_channel->GetHelpExamples(result);
			}						
		}
		*/	
	}
	
	return;
}

void TestCommunicationChannel::InterpretationCompleted(int status, string& result)
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::InterpretationCompleted()]"));
		
	return;
}

void TestCommunicationChannel::GetFromHelpCompleted(string& understood, string& result)
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::GetFromHelpCompleted()]"));
		
	return;
}

void TestCommunicationChannel::QueryOutputReceived(RegulusDialogueResult* result)
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::QueryOutputReceived()]"));
	
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [TestCommunicationChannel::QueryOutputReceived()] "
				"[Understood: %s] [Result: %s] [Paraphrase: %s]\n"), result->GetSelected().c_str(), 
				result->GetQueryOutput().c_str(), result->GetParaphrase().c_str()));
	
	m_result = result;
		
	return;
}

void TestCommunicationChannel::HelpSentencesReceived(string& buffer)
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::HelpSentencesReceived()]"));
	
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [TestCommunicationChannel::HelpSentencesReceived()] "
				"[Buffer: %s]\n"), buffer.c_str()));

	string result("");
	
	while (buffer.size() > 0)
	{
		SplitHelpExamples(buffer, result, "-");		
//		cout << "--- result: " << result.c_str() << endl;
	}
	
	PlayResult();
	
	return;
}

void TestCommunicationChannel::BackTranslationReceived(RegulusTranslationResult* result)
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::BackTranslationReceived()]"));
		
	return;
}

void TestCommunicationChannel::TranslationReceived(RegulusTranslationResult* result)
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::TranslationReceived()]"));
			
	return;
}

void TestCommunicationChannel::ClientMessageReceived(string& buffer)
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::ClientMessageReceived()]"));
			
	return;
}

void TestCommunicationChannel::ServerMessageReceived(string& buffer)
{
	ACE_TRACE(ACE_TEXT("[TestCommunicationChannel::ServerMessageReceived()]"));
			
	return;
}
