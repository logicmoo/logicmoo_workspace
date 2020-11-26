/*
 * Copyright 2007-2008 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MedSLTController.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@issco.unige.ch>
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

#include "MedSLTController.hpp"

MedSLTController::MedSLTController()
	: 	m_rec_result(""),
		m_online(true),
		m_server("0.0.0.0")
{
	ACE_TRACE("[MedSLTController::MedSLTController()]");

	m_medslt_win = NULL;
	m_medslt_win = new MedSLTWindow(*this);
	m_comm_channel = NULL;
	m_comm_channel = new CommunicationChannel(*this);
	
	return;
}

MedSLTController::~MedSLTController()
{
	ACE_TRACE("[MedSLTController::~MedSLTController()]");

	if (m_comm_channel != NULL)
	{	
		//m_comm_channel->StopComms();		
		
		delete m_comm_channel;
	}
	
	if (m_medslt_win != NULL)
	{	
		delete m_medslt_win;
	}
	
	return;
}

void MedSLTController::RunApp()
{
	ACE_TRACE("[MedSLTController::RunApp()]");

	if (m_medslt_win == NULL)
	{
		return;
	}
	
	gtk_widget_show_all(GTK_WIDGET(m_medslt_win->GetWindow()));
	m_medslt_win->SetUiState(m_medslt_win->UI_STATE_DISCONNECTED);
	//m_medslt_win->SetUiState(m_medslt_win->UI_STATE_CONNECTED);

#ifdef _MAEMO
	g_signal_connect(G_OBJECT(m_medslt_win->GetWindow()), "key_press_event",
						G_CALLBACK(KeyPressCb), this);
	g_signal_connect(G_OBJECT(m_medslt_win->GetWindow()), "window_state_event",
						G_CALLBACK(WindowStateCb), this);
#endif

	gtk_main();

	return;
}

gboolean MedSLTController::WindowStateCb(GtkWidget* widget, 
											GdkEventWindowState* event, 
											MedSLTController* controller)
{
	ACE_TRACE("[MedSLTController::WindowStateCb()]");

	if (controller != NULL)
	{
		(controller->GetWindow())->SetFullScreen(event->new_window_state &GDK_WINDOW_STATE_FULLSCREEN);
	}
	
	return FALSE;
}

gboolean MedSLTController::KeyPressCb(GtkWidget* widget, GdkEventKey* event,
										MedSLTController* controller)
{
	ACE_TRACE("[MedSLTController::KeyPressCb()]");

	if (controller == NULL)
	{
		return FALSE;
	}

	switch (event->keyval) 
	{
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
			/*newvol = lastfm_audio_increase_volume(5);
			volume_changed = TRUE;*/
			break;
		case GDK_F8:
			/*newvol = lastfm_audio_increase_volume(-5);
			volume_changed = TRUE;*/
			break;
		case GDK_Up:
			(controller->GetWindow())->IterateList(false);
			break;
		case GDK_Down:
			(controller->GetWindow())->IterateList(true);
			break;
		case GDK_Return:
			controller->GetTranslationFromHelp((controller->GetWindow())->GetRowData());
			//controller->ShowInfo("You pressed enter!");
			break;
		case GDK_Left:
			break;
		case GDK_Right:			
			break;
		case GDK_KP_Enter:
			break;
		default:
			return FALSE;						
	}

	return TRUE;
}

void MedSLTController::StartRecognize()
{
	ACE_TRACE("[MedSLTController::StartRecognize()]");
	
	if (m_medslt_win != NULL)
	{
		m_medslt_win->ResetTextBoxes();
	}
	
	if (m_comm_channel != NULL)
	{
		SetConfidence(30);
		RecognizeFromUser("medslt_gsl.grammar", "en-us", true);
		m_comm_channel->StartRecording();
	}
	
	return;
}

void MedSLTController::AbortRecognize()
{
	ACE_TRACE("[MedSLTController::AbortRecognize()]");
	
	return;
}

void MedSLTController::QuitApp()
{
	ACE_TRACE("[MedSLTController::QuitApp()]");
	
	if (ConfirmDialog("Are you sure you want to exit?"))
	{
		if (m_comm_channel != NULL)
		{		
			m_comm_channel->StopRecording();
		}
		
		gtk_main_quit();	
	}
	
	return;
}

MedSLTWindow* MedSLTController::GetWindow()
{
	ACE_TRACE("[MedSLTController::GetWindow()]");

	if (m_medslt_win != NULL)
	{		
		return m_medslt_win;
	}
	else
	{	
		return NULL;
	}
}

void MedSLTController::Resize(gboolean is_full_screen)
{
	ACE_TRACE("[MedSLTController::Resize()]");

	if (m_medslt_win != NULL)
	{		
		m_medslt_win->Resize(is_full_screen);
	}
	
	return;
}

void MedSLTController::ShowError(const char* text)
{
	ACE_TRACE("[MedSLTController::ShowError()]");	
    
	m_ui_dlg.ErrorDialog(m_medslt_win->GetWindow(), text);
	
	return;
}

void MedSLTController::ShowWarning(const char* text)
{
	ACE_TRACE("[MedSLTController::ShowWarning()]");

	m_ui_dlg.WarningDialog(m_medslt_win->GetWindow(), text);
	
	return;
}

void MedSLTController::ShowInfo(const char* text)
{
	ACE_TRACE("[MedSLTController::ShowInfo()]");

	m_ui_dlg.InfoDialog(m_medslt_win->GetWindow(), text);
	
	return;
}

void MedSLTController::ShowBanner(const char* text)
{
	ACE_TRACE("[MedSLTController::ShowBanner()]");

	m_ui_dlg.InfoBanner(m_medslt_win->GetWindow(), text);
        
	return;
}

gboolean MedSLTController::ConfirmDialog(const char* text)
{
	ACE_TRACE("[MedSLTController::ConfirmDialog()]");
      
	return m_ui_dlg.ConfirmDialog(m_medslt_win->GetWindow(), text);
}

void MedSLTController::ConnectToRemote()
{
	ACE_TRACE("[MedSLTController::ConnectToRemote()]");

	if (m_comm_channel != NULL)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTController::ConnectToRemote()] "
						"[--- Wait... ---]\n")));
		
		m_comm_channel->SetMrcpServerName(m_server);
		m_comm_channel->SetMrcpServerPort(554);
		m_comm_channel->SetDlgServerName(m_server);
		m_comm_channel->SetDlgServerPort(7310);
		m_comm_channel->SetRtpLocalPort(1263);
		m_comm_channel->StartComms();
		//m_comm_channel->SendBackTranslationMessage("action(load_package('$MED_SLT2/EngEng/Prolog/med_role_marked.cfg')).\n");
		//m_comm_channel->SendTranslationMessage("action(load_package('$MED_SLT2/EngFre/Prolog/med_role_marked.cfg')).\n");
		m_comm_channel->SendBackTranslationMessage("action(load_package('c:/medSLT/MedSLT2/EngEng/Prolog/med_role_marked.cfg')).\n");
		m_comm_channel->SendTranslationMessage("action(load_package('c:/medSLT/MedSLT2/EngFre/Prolog/med_role_marked.cfg')).\n");
		m_comm_channel->SendTranslationMessage("action(execute_regulus_command(\"LOAD_HELP\")).\n");
		RecognitionGrammar gram1("SLM:public [MAIN_SLM_headache]", "medslt_slm_headache.grammar", "application/x-nuance-gsl", "");
		RecognitionGrammar gram2("http://" + m_comm_channel->GetMrcpServerName() + ":9080/MedSLT/grammars/recogniser.grammar#MAIN", "medslt_gsl.grammar", "text/uri-list", "");
		m_comm_channel->SetGrammar(gram1);
		m_comm_channel->SetGrammar(gram2);		
	}
	
	return;
}

void MedSLTController::DisconnectFromRemote()
{
	ACE_TRACE("[MedSLTController::DisconnectFromRemote()]");
    
    if (m_comm_channel != NULL)
	{  
		m_comm_channel->StopRecording();
		m_comm_channel->StopComms();		
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTController::DisconnectFromRemote()] "
						"[--- Goodbye ---]\n")));
	}
	
	return;
}

void MedSLTController::GetTranslationFromHelp(gchar* text)
{
	ACE_TRACE("[MedSLTController::GetTranslationFromHelp()]");

	if ((m_comm_channel != NULL) && (m_medslt_win != NULL))
	{	
		string transcription(text);
		m_medslt_win->FindAndReplace(transcription, "?", "");			
		m_comm_channel->GetInterpretation(transcription, "medslt_gsl.grammar");					
	}
		
	return;
}

void MedSLTController::InitCompleted(CCStatus status)
{
	ACE_TRACE("[MedSLTController::InitCompleted()]");
	
	if (m_comm_channel != NULL)
	{
		switch (status)
		{
			case CC_CONNECTED:				
				//m_comm_channel->SpeakSSML("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><speak><audio src=\"http://129.194.32.96:9080/MedSLT/audio/blip.wav\"></audio><break/>Welcome.</speak>", "en-US");
				//m_comm_channel->SpeakSSML("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><speak><audio src=\"http://129.194.32.96:9080/MedSLT/audio/blip.wav\"></audio><break/>Bienvenue.</speak>", "fr-CA");
				m_comm_channel->Speak("Bienvenue.", "fr-CA");
								
				if (m_medslt_win != NULL)
				{
					m_medslt_win->SetUiState(m_medslt_win->UI_STATE_CONNECTED);
				}
				
				break;
			default:
				ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTController::InitCompleted()] "
						"[Error]\n")));				
		}
	}
	
	return;
}
	
void MedSLTController::RecognitionCompleted(int status, string& result)
{
	ACE_TRACE("[MedSLTController::RecognitionCompleted()]");
	
	// NO_MATCH
	if (status == 1)
	{
		if (m_online == true)
		{ 								
			if (m_comm_channel != NULL)
			{
				//m_comm_channel->SpeakSSML("<?xml version=\"1.0\"?><speak><audio src=\"http://129.194.32.96:9080/MedSLT/audio/beep.wav\"></audio><break/></speak>");
				//m_comm_channel->Speak("No match.");
				
				m_online = false;
				
				if (m_medslt_win != NULL)
				{
					string txt("_NO_MATCH");
					m_medslt_win->RecognitionCompleted();
					//txt += "?";
					//m_medslt_win->SetTranslation(txt);
				}
			
				SetConfidence(5);
				RecognizeFromUser("medslt_slm_headache.grammar", "en-us", m_online);			
			}	
		}
		else
		{	
			if (m_comm_channel != NULL)
			{
				m_online = true;
				
				//PlayResult();
				
				//m_comm_channel->SpeakSSML("<?xml version=\"1.0\"?><speak><audio src=\"http://129.194.32.96:9080/MedSLT/audio/beep.wav\"></audio><break/></speak>");
				//m_comm_channel->Speak("No match.");
			}			
		}
	}
	// E.g. NO_SPEECH_TIMEOUT
	else if (status == 2)
	{	
		if (m_online == true)
		{ 
			if (m_medslt_win != NULL)
			{	
				string txt("_REPEAT_PLEASE");			
				m_medslt_win->RecognitionCompleted();
				txt += "?";
				m_medslt_win->SetTranslation(txt);
			}
			
			if (m_comm_channel != NULL)
			{
				m_comm_channel->SpeakSSML("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><speak><audio src=\"http://" + m_server + ":9080/MedSLT/audio/beep.wav\"></audio><break/>Repeat please.</speak>", "en-US");
			}						
		}
		else
		{
			m_online = true;
			
			//PlayResult();
		}		
	}
	else
	{	
		if (m_online == true)
		{ 	
			m_online = false;
			m_rec_result = result;
			
			if (m_medslt_win != NULL)
			{			
				m_medslt_win->RecognitionCompleted();
				//m_medslt_win->SetTranslation(m_rec_result);
			}			
			
			if (m_comm_channel != NULL)
			{
				m_comm_channel->GetBackTranslation(m_rec_result);		
				RecognizeFromUser("medslt_slm_headache.grammar", "en-us", m_online);
			}		
		}
		else
		{
			m_online = true;
							
			if (m_comm_channel != NULL)
			{	
				m_comm_channel->GetHelpExamples(result);
			}						
		}		
	}
	
	return;
}

void MedSLTController::InterpretationCompleted(int status, string& result)
{
	ACE_TRACE("[MedSLTController::InterpretationCompleted()]");
	
	// NO_MATCH
	if (status == 1)
	{
		// TODO: do something
	}
	else
	{	
		if (m_comm_channel != NULL)
		{	
			m_comm_channel->GetTranslation(result);
		}
	}	
	
	return;
}

void MedSLTController::SetConfidence(int value)
{
	ACE_TRACE("[MedSLTController::SetConfidence()]");
	
	if (m_comm_channel != NULL)
	{
		m_comm_channel->SetConfidence(value);
	}
		
	return;
}
	
int MedSLTController::GetConfidence()
{
	ACE_TRACE("[MedSLTController::GetConfidence()]");
	
	if (m_comm_channel != NULL)
	{
		return m_comm_channel->GetConfidence();
	}
	else
	{
		return -1;
	}
}

void MedSLTController::SetRemoteServersIP(string ip)
{
	ACE_TRACE("[MedSLTController::SetRemoteServersIP()]");
	
	m_server = ip;
}

void MedSLTController::QueryOutputReceived(string& understood, string& result)
{
	ACE_TRACE("[MedSLTController::QueryOutputReceived()]");

	return;
}

void MedSLTController::BackTranslationReceived(string& result)
{
	ACE_TRACE("[MedSLTController::BackTranslationReceived()]");
	
	if (m_medslt_win != NULL)
	{
		// No back translation
		if (result != "")		
		{
			m_medslt_win->SetBackTranslation(result);
			//m_medslt_win->SetTranslation(result);
		}		
	}
	
	return;
}

void MedSLTController::TranslationReceived(string& result)
{
	ACE_TRACE("[MedSLTController::TranslationReceived()]");

	if (m_medslt_win != NULL)
	{
		// No translation
		if (result != "")		
		{
			result += "?";
			m_medslt_win->SetTranslation(result);
		}				
	}
	
	PlayResult(result);
	
	return;
}

void MedSLTController::GetFromHelpCompleted(string& understood, string& result)
{
	ACE_TRACE("[MedSLTController::GetFromHelpCompleted()]");
	
	if (m_comm_channel != NULL)
	{	
		//m_comm_channel->GetTranslation(result);
	}		
	
	return;
}

void MedSLTController::HelpSentencesReceived(string& buffer)
{
	ACE_TRACE("[MedSLTController::HelpSentencesReceived()]");
	
	if (m_medslt_win != NULL)
	{	
		m_medslt_win->SetHelpExamples(buffer);
	}
	
	return;
}

void MedSLTController::RecognizeFromUser(const string& grammar, 
											const string& lang, bool online)
{
	ACE_TRACE("[MedSLTController::RecognizeFromUser()]");

	m_online = online;
		
	if (m_comm_channel != NULL)
	{
		m_comm_channel->RecognizeFromUser(grammar, lang, online);
	}
	
	return;
}

void MedSLTController::PlayResult(string& result)
{
	ACE_TRACE("[MedSLTController::PlayResult()]");
	
	if (m_comm_channel != NULL)
	{
		m_comm_channel->SpeakSSML("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><speak><audio src=\"http://" + m_server + ":9080/MedSLT/audio/blip.wav\"></audio><break/>" + result + "</speak>", "fr-CA");		
	}
	
	return;
}
