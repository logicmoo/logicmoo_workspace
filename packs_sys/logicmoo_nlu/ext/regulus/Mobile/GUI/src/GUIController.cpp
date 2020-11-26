/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	GUIController.cpp
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

#include "GUIController.hpp"

GUIController::GUIController()
{
	ACE_TRACE("[GUIController::GUIController()]");

	m_gui_win = NULL;
	m_gui_win = new GUIWindow(*this);
	m_comm_channel = NULL;
	m_comm_channel = new CommunicationChannel(*this, 0x08);	
	
	return;
}

GUIController::~GUIController()
{
	ACE_TRACE("[GUIController::~GUIController()]");

	if (m_comm_channel != NULL)
	{	
		//m_comm_channel->StopComms();	
		
		delete m_comm_channel;
	}
	
	if (m_gui_win != NULL)
	{	
		delete m_gui_win;
	}
	
	return;
}

void GUIController::RunApp()
{
	ACE_TRACE("[GUIController::RunApp()]");

	if (m_gui_win == NULL)
	{
		return;
	}
	
	gtk_widget_show_all(GTK_WIDGET(m_gui_win->GetWindow()));
	m_gui_win->SetUiState(m_gui_win->UI_STATE_DISCONNECTED);

#ifdef _MAEMO
	g_signal_connect(G_OBJECT(m_gui_win->GetWindow()), "key_press_event",
						G_CALLBACK(KeyPressCb), this);
	g_signal_connect(G_OBJECT(m_gui_win->GetWindow()), "window_state_event",
						G_CALLBACK(WindowStateCb), this);
#endif

	gtk_main();

	return;
}

gboolean GUIController::WindowStateCb(GtkWidget* widget, 
										GdkEventWindowState* event, 
										GUIController* controller)
{
	ACE_TRACE("[GUIController::WindowStateCb()]");

	if (controller != NULL)
	{
		(controller->GetWindow())->SetFullScreen(event->new_window_state &GDK_WINDOW_STATE_FULLSCREEN);
	}
	
	return FALSE;
}

gboolean GUIController::KeyPressCb(GtkWidget* widget, GdkEventKey* event,
										GUIController* controller)
{
	ACE_TRACE("[GUIController::KeyPressCb()]");

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
			break;
		case GDK_F8:
			break;
		case GDK_Up:
			break;
		case GDK_Down:			
			break;
		case GDK_Return:			
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
void GUIController::ConnectToRemote()
{
	ACE_TRACE("[GUIController::ConnectToRemote()]");

	if (m_comm_channel != NULL)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [GUIController::ConnectToRemote()] "
										"[--- Wait... ---]\n")));
		
		m_comm_channel->SetMrcpServerName("129.194.32.96");
		m_comm_channel->SetMrcpServerPort(554);
		m_comm_channel->SetDlgServerName("129.194.32.96");
		m_comm_channel->SetDlgServerPort(1985);
		m_comm_channel->SetRtpLocalPort(1263);
		m_comm_channel->StartComms();
		RecognitionGrammar gram("http://" + m_comm_channel->GetMrcpServerName() 
								+ ":9080/Calendar/grammars/recogniser.grammar#MAIN", "calendar_gsl.grammar", 
								"text/uri-list", "");		
		m_comm_channel->SetGrammar(gram);
	}
	
	return;
}

void GUIController::DisconnectFromRemote()
{
	ACE_TRACE("[GUIController::DisconnectFromRemote()]");
    
    if (m_comm_channel != NULL)
	{  
		m_comm_channel->StopRecording();
		m_comm_channel->StopComms();
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [GUIController::DisconnectFromRemote()] "
										"[--- Goodbye ---]\n")));		
	}
	
	return;
}

void GUIController::StartRecognize()
{
	ACE_TRACE("[GUIController::StartRecognize()]");
	
	m_gui_win->ResetTextBoxes();
	
	if (m_comm_channel != NULL)
	{
		RecognizeFromUser("calendar_gsl.grammar", "en-us", true);
		m_comm_channel->StartRecording();
	}
	
	return;
}

void GUIController::AbortRecognize()
{
	ACE_TRACE("[GUIController::AbortRecognize()]");
	
	return;
}

void GUIController::QuitApp()
{
	ACE_TRACE("[GUIController::QuitApp()]");
	
	if (m_ui_dlg.ConfirmDialog(m_gui_win->GetWindow(), "Are you sure you want to exit?"));
	{
		DisconnectFromRemote();
		
		gtk_main_quit();	
	}
	
	return;
}

GUIWindow* GUIController::GetWindow()
{
	ACE_TRACE("[GUIController::GetWindow()]");

	if (m_gui_win != NULL)
	{		
		return m_gui_win;
	}
	else
	{	
		return NULL;
	}
}

void GUIController::Resize(gboolean is_full_screen)
{
	ACE_TRACE("[GUIController::Resize()]");

	if (m_gui_win != NULL)
	{		
		m_gui_win->Resize(is_full_screen);
	}
	
	return;
}

void GUIController::RecognizeFromUser(const string& grammar, 
										const string& lang, bool online)
{
	ACE_TRACE("[GUIController::RecognizeFromUser()]");
	
	if (m_comm_channel != NULL)
	{
		m_comm_channel->RecognizeFromUser(grammar, lang, online);
	}
	
	return;
}

void GUIController::InitCompleted(CCStatus status)
{
	ACE_TRACE("[GUIController::InitCompleted()]");
	
	if (m_comm_channel != NULL)
	{
		switch (status)
		{
			case CC_CONNECTED:				
				m_comm_channel->SpeakSSML("<?xml version=\"1.0\"?><speak>"
											"<audio src=\"http://129.194.32.96:9080/Calendar/audio/blip.wav\"></audio>"
											"<break/>Welcome.</speak>", "en-US");
								
				if (m_gui_win != NULL)
				{
					m_gui_win->SetUiState(m_gui_win->UI_STATE_CONNECTED);
				}
				
				break;
			default:
				ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [GUIController::InitCompleted()] "
						"[Error]\n")));			
		}
	}
	
	return;
}

void GUIController::StartOfSpeech()
{
	ACE_TRACE("[GUIController::StartOfSpeech()]");
	
	if (m_gui_win != NULL)
	{
		m_gui_win->StartOfSpeech();
	}
	
	return;
}

void GUIController::RecognitionCompleted(int status, string& result)
{
	ACE_TRACE("[GUIController::RecognitionCompleted()]");
	
	// NO_MATCH
	if (status == 1)
	{							
		if (m_comm_channel != NULL)
		{
			m_comm_channel->SpeakSSML("<?xml version=\"1.0\"?><speak>"
										"<audio src=\"http://129.194.32.96:9080/Calendar/audio/beep.wav\">"
										"</audio><break/>No match.</speak>", "en-US");

			if (m_gui_win != NULL)
			{
				string txt("_NO_MATCH");
				m_gui_win->RecognitionCompleted();
				m_gui_win->SetRecognized(txt);
			}	
		}
	}
	// E.g. NO_SPEECH_TIMEOUT
	else if (status == 2)
	{	
		if (m_comm_channel != NULL)
		{
			m_comm_channel->SpeakSSML("<?xml version=\"1.0\"?><speak>"
										"<audio src=\"http://129.194.32.96:9080/Calendar/audio/beep.wav\">"
										"</audio><break/>Repeat please.</speak>", "en-US");
		}
		
		if (m_gui_win != NULL)
		{	
			string txt("_REPEAT_PLEASE");			
			m_gui_win->RecognitionCompleted();
			m_gui_win->SetRecognized(txt);
		}				
	}
	else
	{	
		if (m_gui_win != NULL)
		{			
			m_gui_win->RecognitionCompleted();
			m_gui_win->SetRecognized(result);
		}
	}
	
	return;
}

void GUIController::InterpretationCompleted(int status, string& result)
{
	ACE_TRACE("[GUIController::InterpretationCompleted()]");

	return;
}

void GUIController::QueryOutputReceived(RegulusDialogueResult* result)
{
	ACE_TRACE("[GUIController::QueryOutputReceived()]");
	
	return;
}

void GUIController::GetFromHelpCompleted(string& understood, string& result)
{
	ACE_TRACE("[GUIController::GetFromHelpCompleted()]");

	return;
}

void GUIController::HelpSentencesReceived(string& buffer)
{
	ACE_TRACE("[GUIController::HelpSentencesReceived()]");
		
	return;
}

void GUIController::BackTranslationReceived(RegulusTranslationResult* result)
{
	ACE_TRACE("[GUIController::BackTranslationReceived()]");
		
	return;
}

void GUIController::TranslationReceived(RegulusTranslationResult* result)
{
	ACE_TRACE("[GUIController::TranslationReceived()]");
		
	return;
}

void GUIController::ClientMessageReceived(string& buffer)
{
	ACE_TRACE("[GUIController::ClientMessageReceived()]");

	return;
}

void GUIController::ServerMessageReceived(string& buffer)
{
	ACE_TRACE("[GUIController::ServerMessageReceived()]");

	return;
}

void GUIController::ShowError(const char* text)
{
	ACE_TRACE("[GUIController::ShowError()]");
    
	// Get GTK thread lock
	gdk_threads_enter();
				
	m_ui_dlg.ErrorDialog(m_gui_win->GetWindow(), text);
				
	gdk_flush();
				
	// Release GTK thread lock
	gdk_threads_leave();
	
	return;
}

void GUIController::ShowWarning(const char* text)
{
	ACE_TRACE("[GUIController::ShowWarning()]");

	// Get GTK thread lock
	gdk_threads_enter();
					
	m_ui_dlg.WarningDialog(m_gui_win->GetWindow(), text);
					
	gdk_flush();
					
	// Release GTK thread lock
	gdk_threads_leave();
	
	return;
}

void GUIController::ShowInfo(const char* text)
{
	ACE_TRACE("[GUIController::ShowInfo()]");

	// Get GTK thread lock
	gdk_threads_enter();
						
	m_ui_dlg.InfoDialog(m_gui_win->GetWindow(), text);
						
	gdk_flush();
						
	// Release GTK thread lock
	gdk_threads_leave();

	return;
}

void GUIController::ShowBanner(const char* text)
{
	ACE_TRACE("[GUIController::ShowBanner()]");

	// Get GTK thread lock
	gdk_threads_enter();
							
	m_ui_dlg.InfoBanner(m_gui_win->GetWindow(), text);
							
	gdk_flush();
							
	// Release GTK thread lock
	gdk_threads_leave();
    
	return;
}

gboolean GUIController::ConfirmDialog(const char* text)
{
	ACE_TRACE("[GUIController::ConfirmDialog()]");

	gboolean conf = false;
	
	// Get GTK thread lock
	gdk_threads_enter();
								
	conf = m_ui_dlg.ConfirmDialog(m_gui_win->GetWindow(), text);
								
	gdk_flush();
								
	// Release GTK thread lock
	gdk_threads_leave();
		
	return conf; 
}
