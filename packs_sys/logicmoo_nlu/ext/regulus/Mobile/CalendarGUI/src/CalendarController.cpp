/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	CalendarController.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <string.h>
#include <strings.h>
#include <time.h>

#include "CalendarController.hpp"

CalendarController::CalendarController()
	: 	m_text_input(false),
		m_recognize_from_tts(false),
		m_audio_query_result(""),
		m_server("0.0.0.0"),
		m_online(true),		
		m_ilang(INPUT_LANG_ENG),
		m_use_paraphrase(true),
		m_understood(""),
		m_paraphrase(""),
		m_query("")
{
	ACE_TRACE("[CalendarController::CalendarController()]");
	
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [CalendarController::CalendarController()] "
												"[Use paraphrase: %d]\n"), m_use_paraphrase));
		
	m_calendar_win = NULL;
	m_calendar_win = new CalendarWindow(*this);
	m_comm_channel = NULL;
	// Calendar without text input
	m_comm_channel = new CommunicationChannel(*this, 0x9);

	// Calendar with text input	
	//m_comm_channel = new CommunicationChannel(*this, 0x19);	
	
	return;
}

CalendarController::~CalendarController()
{
	ACE_TRACE("[CalendarController::~CalendarController()]");

	if (m_comm_channel != NULL)
	{	
		//m_comm_channel->StopComms();		
		delete m_comm_channel;
	}
	
	if (m_calendar_win != NULL)
	{	
		delete m_calendar_win;
	}
	
	return;
}

void CalendarController::RunApp()
{
	ACE_TRACE("[CalendarController::RunApp()]");

	if (m_calendar_win == NULL)
	{
		return;
	}
	
	gtk_widget_show_all(GTK_WIDGET(m_calendar_win->GetWindow()));
	m_calendar_win->SetUiState(m_calendar_win->UI_STATE_DISCONNECTED);
	//m_calendar_win->SetUiState(m_calendar_win->UI_STATE_CONNECTED);

#ifdef _MAEMO
	g_signal_connect(G_OBJECT(m_calendar_win->GetWindow()), "key_press_event",
						G_CALLBACK(KeyPressCb), this);
	g_signal_connect(G_OBJECT(m_calendar_win->GetWindow()), "window_state_event",
						G_CALLBACK(WindowStateCb), this);
#endif

	gtk_main();

	return;
}

gboolean CalendarController::WindowStateCb(GtkWidget* widget, 
											GdkEventWindowState* event, 
											CalendarController* controller)
{
	ACE_TRACE("[CalendarController::WindowStateCb()]");

	if (controller != NULL)
	{
		(controller->GetWindow())->SetFullScreen(event->new_window_state &GDK_WINDOW_STATE_FULLSCREEN);
	}
	
	return FALSE;
}

gboolean CalendarController::KeyPressCb(GtkWidget* widget, GdkEventKey* event,
										CalendarController* controller)
{
	ACE_TRACE("[CalendarController::KeyPressCb()]");

	if (controller == NULL)
	{
		return FALSE;
	}

	switch (event->keyval) 
	{
		case GDK_Escape:
			controller->ShowHistory();		
			break;
		case GDK_F6:			
			if ((controller->GetWindow())->IsFullScreen()) 
			{			
				(controller->GetWindow())->Resize(true);
				gtk_window_unfullscreen((controller->GetWindow())->GetWindow());
			} 
			else
			{		
				(controller->GetWindow())->Resize(false);
				gtk_window_fullscreen((controller->GetWindow())->GetWindow());
			}
			
			break;
		case GDK_F7:			
			break;
		case GDK_F8:
			break;
		case GDK_Up:
			(controller->GetWindow())->IterateList(false);
			break;
		case GDK_Down:
			(controller->GetWindow())->IterateList(true);
			break;
		case GDK_Return:
			controller->SetTextInput(false);
			controller->RequestFromHelp((controller->GetWindow())->GetRowData(), 1);
			break;
		case GDK_Left:			
			break;
		case GDK_Right:			
			break;
		case GDK_KP_Enter:
			controller->SetTextInput(true);
			gchar* txt = g_ascii_strdown((controller->GetWindow())->GetText(), -1);
			
			if (txt != NULL)
			{
				controller->RequestFromHelp(txt, 1);
				g_free(txt);
			}			
			break;
		default:
			return FALSE;						
	}

	return TRUE;
}

void CalendarController::StartRecognize()
{
	ACE_TRACE("[CalendarController::StartRecognize()]");

	//SetRecognizeFromTTS(false);

	if (m_calendar_win != NULL)
	{
		m_calendar_win->ResetTextBoxes();
	}
	
	if (m_comm_channel != NULL)
	{
		m_comm_channel->StopTTSAudio();
		//m_comm_channel->AllowPlayback(false);
		//m_comm_channel->RecognizeGLM();
		SetConfidence(30);
		//SetConfidence(0);
		RecognizeFromUser("calendar_gsl.grammar", 
							(GetInputLang() == INPUT_LANG_JAP) ? "jp" : "en-us", true);
		m_comm_channel->StartRecording();
	}

	return;
}

void CalendarController::AbortRecognize()
{
	ACE_TRACE("[CalendarController::AbortRecognize()]");
	
	if (m_comm_channel != NULL)
	{
		m_comm_channel->StopRecording();
		m_comm_channel->StopRecognize();
	}
	
	return;
}

void CalendarController::QuitApp()
{
	ACE_TRACE("[CalendarController::QuitApp()]");
	
	if (m_ui_dlg.ConfirmDialog(m_calendar_win->GetWindow(), 
								"Are you sure you want to exit?", "Confirmation"))
	{
		DisconnectFromRemote();
		
		gtk_main_quit();	
	}
	
	return;
}

CalendarWindow* CalendarController::GetWindow()
{
	ACE_TRACE("[CalendarController::GetWindow()]");

	if (m_calendar_win != NULL)
	{		
		return m_calendar_win;
	}
	else
	{	
		return NULL;
	}
}

void CalendarController::Resize(gboolean is_full_screen)
{
	ACE_TRACE("[CalendarController::Resize()]");

	if (m_calendar_win != NULL)
	{		
		m_calendar_win->Resize(is_full_screen);
	}
	
	return;
}

void CalendarController::ShowError(const char* text)
{
	ACE_TRACE("[CalendarController::ShowError()]");
    
	// Get GTK thread lock
	gdk_threads_enter();
			
	m_ui_dlg.ErrorDialog(m_calendar_win->GetWindow(), text);
			
	gdk_flush();
			
	// Release GTK thread lock
	gdk_threads_leave();
	
	return;
}

void CalendarController::ShowWarning(const char* text)
{
	ACE_TRACE("[CalendarController::ShowWarning()]");
	
	// Get GTK thread lock
	gdk_threads_enter();
			
	m_ui_dlg.WarningDialog(m_calendar_win->GetWindow(), text);
			
	gdk_flush();
			
	// Release GTK thread lock
	gdk_threads_leave();
	
	return;
}

void CalendarController::ShowInfo(const char* text)
{
	ACE_TRACE("[CalendarController::ShowInfo()]");

	// Get GTK thread lock
	gdk_threads_enter();
				
	m_ui_dlg.InfoDialog(m_calendar_win->GetWindow(), text);
				
	gdk_flush();
				
	// Release GTK thread lock
	gdk_threads_leave();	
	
	return;
}

void CalendarController::ShowBanner(const char* text)
{
	ACE_TRACE("[CalendarController::ShowBanner()]");

	// Get GTK thread lock
	gdk_threads_enter();
					
	m_ui_dlg.InfoBanner(m_calendar_win->GetWindow(), text);
					
	gdk_flush();
					
	// Release GTK thread lock
	gdk_threads_leave();	
	
	return;
}

gboolean CalendarController::ConfirmDialog(const char* text, const char* title)
{
	ACE_TRACE("[CalendarController::ConfirmDialog()]");

	gboolean conf = false;
	
	// Get GTK thread lock
	gdk_threads_enter();
						
	conf = m_ui_dlg.ConfirmDialog(m_calendar_win->GetWindow(), text, title);
						
	gdk_flush();
						
	// Release GTK thread lock
	gdk_threads_leave();
		
	return conf;
}

void CalendarController::ConnectToRemote()
{
	ACE_TRACE("[CalendarController::ConnectToRemote()]");

	if (m_comm_channel != NULL)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [CalendarController::ConnectToRemote()] "
						"[--- Wait... ---]\n")));		
		
		m_comm_channel->SetMrcpServerName(m_server);
		m_comm_channel->SetMrcpServerPort(554);
		m_comm_channel->SetDlgServerName(m_server);
		m_comm_channel->SetDlgServerPort(1985);
		m_comm_channel->SetRtpLocalPort(1263);
		m_comm_channel->StartComms();

		if (GetInputLang() == INPUT_LANG_JAP)
		{
			m_comm_channel->SendRegulusMessage("action(execute_regulus_command(\"SET_NOTIONAL_SPEAKER agnes\")).\n");
			m_comm_channel->SendRegulusMessage("action(execute_regulus_command(\"SET_NOTIONAL_TIME 2008-11-17_18-20-43\")).\n");
			
			RecognitionGrammar gram1("SLM:public [MAIN_SLM_JAP]", "calendar_slm.grammar", "application/x-nuance-gsl", "");
			RecognitionGrammar gram2("http://" + m_comm_channel->GetMrcpServerName() + ":9080/Calendar/grammars/recogniser_jap.grammar#MAIN", "calendar_gsl.grammar", "text/uri-list", "");
			
			m_comm_channel->SetGrammar(gram1);
			m_comm_channel->SetGrammar(gram2);			
		}
		else
		{
			m_comm_channel->SendRegulusMessage("action(execute_regulus_command(\"SET_NOTIONAL_SPEAKER agnes\")).\n");
			m_comm_channel->SendRegulusMessage("action(execute_regulus_command(\"SET_NOTIONAL_TIME 2008-11-17_18-20-43\")).\n");
			
			RecognitionGrammar gram1("SLM:public [MAIN_SLM_ENG]", "calendar_slm.grammar", "application/x-nuance-gsl", "");
			RecognitionGrammar gram2("http://" + m_comm_channel->GetMrcpServerName() + ":9080/Calendar/grammars/recogniser_eng.grammar#MAIN", "calendar_gsl.grammar", "text/uri-list", "");
			
			m_comm_channel->SetGrammar(gram1);
			m_comm_channel->SetGrammar(gram2);
		}
	}
	
	return;
}

void CalendarController::DisconnectFromRemote()
{
	ACE_TRACE("[CalendarController::DisconnectFromRemote()]");
    
    if (m_comm_channel != NULL)
	{  
		m_comm_channel->StopRecording();
		m_comm_channel->StopComms();
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [CalendarController::DisconnectFromRemote()] "
						"[--- Goodbye ---]\n")));	
	}
	
	return;
}

void CalendarController::InitializeDialogue()
{
	ACE_TRACE("[CalendarController::InitializeDialogue()]");
    
	if (m_ui_dlg.ConfirmDialog(m_calendar_win->GetWindow(),
			"Do you really want to initialize the dialogue?", "Confirmation") == true)
	{
		if (m_comm_channel != NULL)
		{  
				//m_comm_channel->...();			
		}
	}
	
	return;
}

void CalendarController::ShowHistory()
{
	ACE_TRACE("[CalendarController::ShowHistory()]");
    
	m_ui_dlg.ListDialog(m_calendar_win->GetWindow(), 
								"History",	m_para_list);
	
	return;
}

void CalendarController::RequestFromHelp(gchar* text, bool allowplayback)
{
	ACE_TRACE("[CalendarController::RequestFromHelp()]");

	if ((m_comm_channel != NULL) && (text != NULL))
	{
		//m_comm_channel->AllowPlayback(allowplayback);
		m_comm_channel->RequestFromHelp(text);
	}
		
	return;
}

void CalendarController::SetTextInput(gboolean choice)
{
	ACE_TRACE("[CalendarController::SetTextInput()]");
	
	m_text_input = choice;
		
	return;
}

gboolean CalendarController::IsTextInput()
{
	ACE_TRACE("[CalendarController::IsTextInput()]");
		
	return m_text_input;
}

void CalendarController::SetRecognizeFromTTS(gboolean choice)
{
	ACE_TRACE("[CalendarController::SetRecognizeFromTTS()]");
	
	m_recognize_from_tts = choice;
	
	return;
}

gboolean CalendarController::IsRecognizeFromTTS()
{
	ACE_TRACE("[CalendarController::IsRecognizeFromTTS()]");
		
	return m_recognize_from_tts;
}

void CalendarController::InitCompleted(CCStatus status)
{
	ACE_TRACE("[CalendarController::InitCompleted()]");
	
	if (m_comm_channel != NULL)
	{
		switch (status)
		{
			case CC_CONNECTED:				
				m_comm_channel->SpeakSSML("<?xml version=\"1.0\"?>"
											"<speak><audio src=\"http://" 
											+ m_server + ":9080/Calendar/audio/blip.wav\"></audio>"
											"<break/>Welcome.</speak>", "en-US");				
				
				if (m_calendar_win != NULL)
				{
					m_calendar_win->SetUiState(m_calendar_win->UI_STATE_CONNECTED);
				}
				
				break;
			default:
				ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [CalendarController::InitCompleted()] "
						"[Error]\n")));
		}
	}
	
	return;
}

void CalendarController::StartOfSpeech()
{
	ACE_TRACE("[CalendarController::StartOfSpeech()]");
	
	if ((m_calendar_win != NULL) && (m_online == true))
	{
		m_calendar_win->StartOfSpeech();
	}
	
	return;
}

void CalendarController::RecognitionCompleted(int status, string& result)
{
	ACE_TRACE("[CalendarController::RecognitionCompleted()]");
	
	if (IsRecognizeFromTTS() == false)
	{
		// NO_MATCH
		if (status == 1)
		{
			if (m_online == true)
			{ 								
				if (m_comm_channel != NULL)
				{
					//m_comm_channel->SpeakSSML("<?xml version=\"1.0\"?><speak><audio src=\"http://129.194.32.96:9080/Calendar/audio/beep.wav\"></audio><break/></speak>");
					//m_comm_channel->Speak("No match.");
					
					m_online = false;
				
					if (m_calendar_win != NULL)
					{
						string txt("_NO_MATCH");
						m_calendar_win->RecognitionCompleted();
						m_calendar_win->SetUnderstood(txt);
						m_calendar_win->ShowBusyCursor(true);
					}
					
					SetConfidence(5);
					RecognizeFromUser("calendar_slm.grammar", 
										(GetInputLang() == INPUT_LANG_JAP) ? "jp" : "en-us", m_online);				
				}	
			}
			else
			{	
				if (m_comm_channel != NULL)
				{
					m_online = true;
				
					HandleOutput();
					
					if (m_calendar_win != NULL)
					{		
						m_calendar_win->ShowBusyCursor(false);
					}
					
					PlayResult();
				
					//m_comm_channel->SpeakSSML("<?xml version=\"1.0\"?><speak><audio src=\"http://129.194.32.96:9080/Calendar/audio/beep.wav\"></audio><break/></speak>");
					//m_comm_channel->Speak("No match.");
				}			
			}
		}
		// E.g. NO_SPEECH_TIMEOUT
		else if (status == 2)
		{	
			if (m_online == true)
			{ 
				if (m_calendar_win != NULL)
				{	
					string txt("_REPEAT_PLEASE");
					m_calendar_win->ShowBusyCursor(false);
					m_calendar_win->RecognitionCompleted();
					m_calendar_win->SetUnderstood(txt);
				}
			
				if (m_comm_channel != NULL)
				{
					m_comm_channel->SpeakSSML("<?xml version=\"1.0\"?>"
												"<speak><audio src=\"http://" 
												+ m_server + ":9080/Calendar/audio/beep.wav\"></audio>"
												"<break/>Repeat please.</speak>", "en-US");					
				}					
			}
			else
			{
				m_online = true;
				
				if (m_calendar_win != NULL)
				{			
					m_calendar_win->ShowBusyCursor(false);
				}
				
				PlayResult();
			}		
		}
		else
		{	
			if (m_online == true)
			{ 	
				m_online = false;
			
				if (m_calendar_win != NULL)
				{			
					m_calendar_win->RecognitionCompleted();
					m_calendar_win->ShowBusyCursor(true);
				}
			
				if (m_comm_channel != NULL)
				{
					m_comm_channel->GetQueryData(result);	
					SetConfidence(5);	
					RecognizeFromUser("calendar_slm.grammar", 
										(GetInputLang() == INPUT_LANG_JAP) ? "jp" : "en-us", 
										m_online);
				}		
			}
			else
			{
				m_online = true;
				//SetRecognizeFromTTS(false);	
				
				if (m_calendar_win != NULL)
				{			
					m_calendar_win->RecognitionCompleted();
					m_calendar_win->ShowBusyCursor(false);
				}
				
				if (m_comm_channel != NULL)
				{	
					m_comm_channel->GetHelpExamples(result);
				}						
			}		
		}
	}
	else
	{
		m_calendar_win->ShowBusyCursor(false);
		SetRecognizeFromTTS(false);	
			
		if ((status == 1) || (status == 2))
		{
			if (m_comm_channel != NULL)
			{
				m_comm_channel->SpeakSSML("<?xml version=\"1.0\"?>"
											"<speak><audio src=\"http://" 
											+ m_server + ":9080/Calendar/audio/beep.wav\"></audio>"
											"<break/>Rephrase please.</speak>", "en-US");
				// TODO: Show some examples
								
			}				
		}
		else
		{
			if (m_comm_channel != NULL)
			{	
				m_comm_channel->GetHelpExamples(result);
			}
		}
	}
	
	return;
}

void CalendarController::InterpretationCompleted(int status, string& result)
{
	ACE_TRACE("[CalendarController::InterpretationCompleted()]");
	
	return;
}

void CalendarController::HandleOutput()
{
	ACE_TRACE("[CalendarController::HandleOutput()]");
	
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [CalendarController::HandleOutput()] "
									"[Understood: %s]\n"), m_understood.c_str()));
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [CalendarController::HandleOutput()] "
									"[Paraphrase: %s]\n"), m_paraphrase.c_str()));	
		
	string query_result = m_query;
	string query_paraphrase = m_paraphrase;
	gboolean confirmed = true;

	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [CalendarController::HandleOutput()] "
								"[Understood: %s]\n"), m_understood.c_str()));
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [CalendarController::HandleOutput()] "
								"[Paraphrase: %s]\n"), m_paraphrase.c_str()));	
	
	// Ask confirmation of the paraphrase
	if ((m_use_paraphrase == true) && (m_paraphrase != "") 
			&& (query_paraphrase != "WARNING: PARAPHRASE GENERATION FAILED"))
	{
		FindAndReplace(query_paraphrase, "[ ", "");
		FindAndReplace(query_paraphrase, " ]", "");			
		FormatOutput(query_paraphrase);
		
		if (ConfirmDialog(query_paraphrase.c_str(), "Understood") == false)
		{
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [CalendarController::HandleOutput()] "
											"[Revert Discourse Context]\n")));
			
			confirmed = false;
			
			if (m_comm_channel != NULL)
			{
				m_comm_channel->RevertDiscourseContext();				
			}
		}
	}
	// Ask confirmation of the n-best hypothesis
	else if (m_understood != "")
	{
		if (ConfirmDialog(m_understood.c_str(), "Understood") == false)
		{
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [CalendarController::HandleOutput()] "
														"[Revert Discourse Context]\n")));
			
			confirmed = false;
			
			if (m_comm_channel != NULL)
			{
				m_comm_channel->RevertDiscourseContext();				
			}
		}
	}
	else
	{
		confirmed = false;
	}
	
	// The user confirms the paraphrase or the result comes from the help examples, 
	// so we don't have any paraphrase at all
	if (confirmed == true)
	{
		if (GetInputLang() == INPUT_LANG_JAP)
		{
			//TODO: Recheck
			//query_result = ExtractQueryResult(m_result);
			//m_audio_query_result = ExtractWavList(result);
		}
		else
		{
			m_audio_query_result = query_result;
		}

		if (m_calendar_win != NULL)
		{
			// Present the paraphrase
			if ((m_use_paraphrase == true) && (m_paraphrase != "")
					&& (m_paraphrase != "WARNING: PARAPHRASE GENERATION FAILED"))
			{
				FindAndReplace(m_paraphrase, "[ ", "");
				FindAndReplace(m_paraphrase, " ]", "");
				
				m_para_list.push_back(m_paraphrase);
				m_calendar_win->SetUnderstood(m_paraphrase);
			}
			// Present the n-best hypothesis
			else
			{
				m_para_list.push_back(m_understood);
				m_calendar_win->SetUnderstood(m_understood);
			}
	
			if (query_result == "'Sorry, something went wrong'")						
			{
				string txt("NO_RESULT");			
				m_calendar_win->SetQueryResult(txt);
				m_audio_query_result = "";
			}
			else
			{
				m_calendar_win->SetQueryResult(query_result);
			}
		}
	}

	m_query = "";
	m_paraphrase = "";
	m_understood = "";
		
	return;
}

void CalendarController::QueryOutputReceived(RegulusDialogueResult* result)
{
	ACE_TRACE("[CalendarController::QueryOutputReceived()]");
		
	if (result != NULL)
	{
		m_understood = result->GetSelected();
		m_paraphrase = result->GetParaphrase();
		m_query = result->GetQueryOutput();
		
		//HandleOutput(understood, paraphrase, query);
	}
			
	return;
}

void CalendarController::SetConfidence(int value)
{
	ACE_TRACE("[CalendarController::SetConfidence()]");
	
	if (m_comm_channel != NULL)
	{
		m_comm_channel->SetConfidence(value);
	}
		
	return;
}
	
int CalendarController::GetConfidence()
{
	ACE_TRACE("[CalendarController::GetConfidence()]");
	
	if (m_comm_channel != NULL)
	{
		return m_comm_channel->GetConfidence();
	}
	else
	{
		return -1;
	}
}

void CalendarController::SetRemoteServersIP(string ip)
{
	ACE_TRACE("[CalendarController::SetRemoteServersIP()]");
	
	m_server = ip;
	
	return;
}

CalendarController::InputLang CalendarController::GetInputLang()
{
	ACE_TRACE("[CalendarController::GetInputLang()]");
	
	return m_ilang;
}

void CalendarController::SetInputLang(InputLang lang)
{
	ACE_TRACE("[CalendarController::SetInputLang()]");
	
	m_ilang = lang;
	
	return;
}

void CalendarController::GetFromHelpCompleted(string& understood, string& result)
{
	ACE_TRACE("[CalendarController::GetFromHelpCompleted()]");
		
	m_understood = understood;
	m_paraphrase = "";
	m_query = result;
	
	if (IsTextInput() == false)
	{
		HandleOutput();
		PlayResult();
	}
	else
	{
		if (result == "'Sorry, something went wrong'")						
		{
			//StartRecognizeTTS("<speak>When is the next meeting.<break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/>n.</speak>");
			//RecognizeFromTTS("<speak>" + understood + "<break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/><break strength=\"x-strong\"/>n.</speak>", 1);
			m_calendar_win->ShowBusyCursor(true);
			m_audio_query_result = "";
			SetConfidence(5);
			RecognizeFromTTS(understood, "calendar_slm.grammar", "en-US");
			//SetConfidence(1);
			//RecognizeFromTTS(understood, "calendar_gsl.grammar", "en-US");
		}
		else
		{
			HandleOutput();
			PlayResult();
		}
		
		SetTextInput(false);
	}
	return;
}

void CalendarController::HelpSentencesReceived(string& buffer)
{
	ACE_TRACE("[CalendarController::HelpSentencesReceived()]");
	
	if (m_calendar_win != NULL)
	{	
		HandleOutput();
		m_calendar_win->SetHelpExamples(buffer);
	}
	
	PlayResult();
	
	return;
}

void CalendarController::RecognizeFromUser(const string& grammar, 
											const string& lang, bool online)
{
	ACE_TRACE("[CalendarController::RecognizeFromUser()]");

	m_online = online;
	SetRecognizeFromTTS(false);
	
	if (m_comm_channel != NULL)
	{
		m_comm_channel->RecognizeFromUser(grammar, lang, online);
	}
	
	return;
}

void CalendarController::RecognizeFromTTS(const string& input, 
											const string& grammar, const string& lang)
{
	ACE_TRACE("[CalendarController::RecognizeFromTTS()]");
	
	m_online = true;
	SetRecognizeFromTTS(true);
	
	if (m_comm_channel != NULL)
	{
		m_comm_channel->RecognizeFromTTS(input, grammar, lang);
	}
	
	return;
}

void CalendarController::PlayResult()
{
	ACE_TRACE("[CalendarController::PlayResult()]");
	
	if (m_comm_channel != NULL)
	{
		if (GetInputLang() == INPUT_LANG_ENG)
		{			
			m_comm_channel->SpeakSSML("<?xml version=\"1.0\"?><speak>"
										"<audio src=\"http://" + m_server 
										+ ":9080/Calendar/audio/blip.wav\"></audio><break/>" 
				+ m_audio_query_result + "</speak>", "en-US");
		}
		else if (GetInputLang() == INPUT_LANG_JAP)
		{			
			m_comm_channel->SpeakSSML("<?xml version=\"1.0\"?><speak>"
										"<audio src=\"http://" + m_server 
										+ ":9080/Calendar/audio/blip.wav\"></audio><break/>"
										+ m_audio_query_result + "</speak>", "en-US");
		}
	}
		
	m_audio_query_result = "";
	
	return;
}

void CalendarController::BackTranslationReceived(RegulusTranslationResult* result)
{
	ACE_TRACE("[CalendarController::BackTranslationReceived()]");
		
	return;
}

void CalendarController::TranslationReceived(RegulusTranslationResult* result)
{
	ACE_TRACE("[CalendarController::TranslationReceived()]");

	return;
}

void CalendarController::ClientMessageReceived(string& buffer)
{
	ACE_TRACE("[CalendarController::ClientMessageReceived()]");

	return;
}

void CalendarController::ServerMessageReceived(string& buffer)
{
	ACE_TRACE("[CalendarController::ServerMessageReceived()]");

	return;
}

void CalendarController::FindAndReplace(string &input_str,
										string const &search_str, 
										string const &replace_str)
{
	ACE_TRACE("[CalendarController::FindAndReplace()]");
	
	string::size_type pos = 0;
		
	while ((pos = input_str.find(search_str, pos)) != string::npos)
	{
		input_str.replace(pos, search_str.size(), replace_str);
		pos = pos + replace_str.length();
	}
	
	return;
}

void CalendarController::FormatOutput(string &input_str)
{
	ACE_TRACE("[CalendarController::FormatOutput()]");
	
	string::size_type pos = 0;
	int counter = 0;
	
	// Break the string in different lines
	while ((pos = input_str.find(" ", pos)) != string::npos)
	{
		++counter;
		if ((counter % 8) == 0)
		{
			input_str.replace(pos, 1, "\n");
		}
		
		pos = pos + 1;
	}
	
	return;
}

const string CalendarController::ExtractQueryResult(string const input)
{
	ACE_TRACE("[CalendarController::ExtractQueryResult()]");
	
	string::size_type pos = 0;
	string result = "input";

	if ((pos = input.find(",", 0)) != string::npos)
	{
		result = input.substr(0, pos);		
	}
	
	return result;
}

const string CalendarController::ExtractWavList(string const input)
{
	ACE_TRACE("[CalendarController::ExtractWavList()]");
	
	string::size_type start = 0;
	string::size_type end = 0;
	string list = "";
	string tmp = input;
	
	string pre = "<audio src=\"http://" + m_server + ":9080/Calendar/audio/japanese/";
	string post = "\"></audio>";
	
	FindAndReplace(tmp, "\'", "");

	if ((start = tmp.find(",", 0)) != string::npos)
	{
		start += 1;
		
		while ((end = tmp.find(" ", start)) != string::npos)
		{
			list += pre + tmp.substr(start, end-start) + ".wav" + post;
			start = end + 1;
		}

		if (start < tmp.size())
		{
			list += pre + tmp.substr(start, tmp.size()) + ".wav" + post;
		}	
	}
		
	return list;
}

void CalendarController::GetRecResult(string& result)
{
	ACE_TRACE("[CalendarController::GetRecResult()]");
	
	string::size_type pos = 0;
	
	if ((pos = result.find("<extension", 0)) != string::npos)
	{
		result.replace(pos, result.size(), "");
	}
	
	if ((pos = result.find_last_of(">")) != string::npos)
	{
		result.replace(0, pos+1, "");
	}
	
	size_t startpos = result.find_first_not_of(" \t");

    size_t endpos = result.find_last_not_of(" \t");

    if((string::npos == startpos ) || ( string::npos == endpos))
    {
		result = "";
    }
    else
    {	
    	result = result.substr(startpos, endpos-startpos+1);
    }
    
	return;
}
