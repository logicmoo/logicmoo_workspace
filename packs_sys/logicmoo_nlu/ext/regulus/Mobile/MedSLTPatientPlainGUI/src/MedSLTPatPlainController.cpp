/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MedSLTPatPlainController.cpp
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

#include "MedSLTPatPlainController.hpp"

MedSLTPatPlainController::MedSLTPatPlainController()
	: 	m_rec_result(""),
		m_server("0.0.0.0"),
		m_online(true)
{
	ACE_TRACE("[MedSLTPatPlainController::MedSLTPatPlainController()]");

	m_medslt_win = NULL;
	m_medslt_win = new MedSLTPatPlainWindow(*this);
	m_comm_channel = NULL;
	// MRCP SERVER 1 & TCP CLIENT
	m_comm_channel = new CommunicationChannel(*this, 0x48);
	
	return;
}

MedSLTPatPlainController::~MedSLTPatPlainController()
{
	ACE_TRACE("[MedSLTPatPlainController::~MedSLTPatPlainController()]");

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

void MedSLTPatPlainController::RunApp()
{
	ACE_TRACE("[MedSLTPatPlainController::RunApp()]");

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

gboolean MedSLTPatPlainController::WindowStateCb(GtkWidget* widget, 
												 GdkEventWindowState* event, 
												 MedSLTPatPlainController* controller)
{
	ACE_TRACE("[MedSLTPatPlainController::WindowStateCb()]");

	if (controller != NULL)
	{
		(controller->GetWindow())->SetFullScreen(event->new_window_state &GDK_WINDOW_STATE_FULLSCREEN);
	}
	
	return FALSE;
}

gboolean MedSLTPatPlainController::KeyPressCb(GtkWidget* widget, GdkEventKey* event,
										 MedSLTPatPlainController* controller)
{
	ACE_TRACE("[MedSLTPatPlainController::KeyPressCb()]");

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

void MedSLTPatPlainController::ConnectToRemote()
{
	ACE_TRACE("[MedSLTPatPlainController::ConnectToRemote()]");

	if (m_comm_channel != NULL)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTPatPlainController::ConnectToRemote()] "
						"[--- Wait... ---]\n")));
		
		m_comm_channel->SetMrcpServerName(m_server);
		m_comm_channel->SetMrcpServerPort(556);
		m_comm_channel->SetTransServerName(m_server);
		m_comm_channel->SetTransServerPort(7311);
		m_comm_channel->SetBTransServerName(m_server);
		m_comm_channel->SetBTransServerPort(7338);
		//m_comm_channel->SetLocalServerName("129.194.32.96");
		m_comm_channel->SetLocalServerName("129.194.33.197");
		m_comm_channel->SetLocalServerPort(7120);
		m_comm_channel->SetRtpLocalPort(1263);
		m_comm_channel->StartComms();
		m_comm_channel->SendBackTranslationMessage("action(load_package('c:/medSLT/MedSLT2/SpaSpa/Prolog/med_answers_role_marked.cfg')).\n");
		m_comm_channel->SendBackTranslationMessage("action(execute_regulus_command(\"ANSWER_ELLIPSIS_ON\")).\n");
		m_comm_channel->SendTranslationMessage("action(load_package('c:/medSLT/MedSLT2/SpaEng/Prolog/med_answers_role_marked.cfg')).\n");
		m_comm_channel->SendTranslationMessage("action(execute_regulus_command(\"ANSWER_ELLIPSIS_ON\")).\n");
		m_comm_channel->SendTranslationMessage("action(execute_regulus_command(\"LOAD_HELP\")).\n");
		RecognitionGrammar gram1("SLM:public [MAIN_SLM_SPA_pharyngitis]", "medslt_slm_headache.grammar", "application/x-nuance-gsl", "");
		RecognitionGrammar gram2("http://" + m_comm_channel->GetMrcpServerName() + ":9080/MedSLT/grammars/recogniser_spa.grammar#MAIN__a_sore_throat", "medslt_gsl.grammar", "text/uri-list", "");
		m_comm_channel->SetGrammar(gram1);
		m_comm_channel->SetGrammar(gram2);		
	}
	
	return;
}

void MedSLTPatPlainController::DisconnectFromRemote()
{
	ACE_TRACE("[MedSLTPatPlainController::DisconnectFromRemote()]");
    
    if (m_comm_channel != NULL)
	{  
		m_comm_channel->StopRecording();
		m_comm_channel->StopComms();		
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] "
										"[MedSLTPatPlainController::DisconnectFromRemote()] "
										"[--- Goodbye ---]\n")));
	}
	
	return;
}

void MedSLTPatPlainController::QuitApp()
{
	ACE_TRACE("[MedSLTPatPlainController::QuitApp()]");
	
	if (m_ui_dlg.ConfirmDialog(m_medslt_win->GetWindow(), "Are you sure you want to exit?"))
	{
		DisconnectFromRemote();
		
		gtk_main_quit();	
	}
	
	return;
}

void MedSLTPatPlainController::SendResultToServer(string& result)
{
	ACE_TRACE("[MedSLTPatPlainController::SendResultToServer()]");
	
	if (m_comm_channel != NULL)
	{
		m_comm_channel->SendResultToServer(result);		
	}
	
	return;
}

MedSLTPatPlainWindow* MedSLTPatPlainController::GetWindow() const
{
	ACE_TRACE("[MedSLTPatPlainController::GetWindow()]");

	if (m_medslt_win != NULL)
	{		
		return m_medslt_win;
	}
	else
	{	
		return NULL;
	}
}

void MedSLTPatPlainController::Resize(gboolean is_full_screen) const
{
	ACE_TRACE("[MedSLTPatPlainController::Resize()]");

	if (m_medslt_win != NULL)
	{		
		m_medslt_win->Resize(is_full_screen);
	}
	
	return;
}

void MedSLTPatPlainController::SetRemoteServersIP(string ip)
{
	ACE_TRACE("[MedSLTPatPlainController::SetRemoteServersIP()]");
	
	m_server = ip;
}

void MedSLTPatPlainController::PlayResult(string& result) const
{
	ACE_TRACE("[MedSLTPatPlainController::PlayResult()]");
	
	if (m_comm_channel != NULL)
	{
		m_comm_channel->SpeakSSML("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>"
									"<speak><audio src=\"http://" 
									+ m_server + ":9080/MedSLT/audio/blip.wav\"></audio><break/>" 
									+ result + "</speak>", "fr-CA");		
	}
	
	return;
}

void MedSLTPatPlainController::InitCompleted(CCStatus status)
{
	ACE_TRACE("[MedSLTPatPlainController::InitCompleted()]");
	
	if (m_comm_channel != NULL)
	{
		switch (status)
		{
			case CC_CONNECTED:				
				
				m_comm_channel->Speak("Bienvenue.", "fr-CA");
								
				if (m_medslt_win != NULL)
				{
					m_medslt_win->SetUiState(m_medslt_win->UI_STATE_CONNECTED);
				}
				
				break;
			default:
				ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] "
												"[MedSLTPatPlainController::InitCompleted()] "
												"[Error]\n")));				
		}
	}
	
	return;
}

void MedSLTPatPlainController::StartOfSpeech()
{
	ACE_TRACE("[MedSLTPatPlainController::StartOfSpeech()]");
	
	return;
}

void MedSLTPatPlainController::RecognitionCompleted(int status, string& result)
{
	ACE_TRACE("[MedSLTPatPlainController::RecognitionCompleted()]");
	
	return;
}

void MedSLTPatPlainController::InterpretationCompleted(int status, string& result)
{
	ACE_TRACE("[MedSLTPatPlainController::InterpretationCompleted()]");
	
	return;
}

void MedSLTPatPlainController::QueryOutputReceived(RegulusDialogueResult* result)
{
	ACE_TRACE("[MedSLTPatPlainController::QueryOutputReceived()]");

	return;
}

void MedSLTPatPlainController::GetFromHelpCompleted(string& understood, string& result)
{
	ACE_TRACE("[MedSLTPatPlainController::GetFromHelpCompleted()]");
	
	return;
}

void MedSLTPatPlainController::HelpSentencesReceived(string& buffer)
{
	return;
}

void MedSLTPatPlainController::BackTranslationReceived(RegulusTranslationResult* result)
{
	ACE_TRACE("[MedSLTPatPlainController::BackTranslationReceived()]");
	
	return;
}

void MedSLTPatPlainController::TranslationReceived(RegulusTranslationResult* result)
{
	ACE_TRACE("[MedSLTPatPlainController::TranslationReceived()]");
	
	return;
}

void MedSLTPatPlainController::ClientMessageReceived(string& buffer)
{
	ACE_TRACE(ACE_TEXT("[MedSLTPatPlainController::ClientMessageReceived()]"));
	
	return;
}

void MedSLTPatPlainController::ServerMessageReceived(string& buffer)
{
	ACE_TRACE(ACE_TEXT("[MedSLTPatPlainController::ServerMessageReceived()]"));

	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] "
									"[MedSLTPatPlainController::ServerMessageReceived()]"
									"[%s]\n"), buffer.c_str()));
	
	if (buffer != "")
	{
		if (m_comm_channel != NULL)
		{
			m_comm_channel->RegisterExternalUtterance(buffer);
		}
		
		if (m_medslt_win != NULL)
		{
			m_medslt_win->ExtractOutput(buffer, "text_translation='", "')");
			m_medslt_win->FindAndReplace(buffer, "\\", "");
			buffer += "?";
			
			m_medslt_win->ResetTextBox();
			m_medslt_win->SetDoctorQuestion(buffer);
			
			PlayResult(buffer);
		}
	}

	return;
}

void MedSLTPatPlainController::ShowError(const char* text)
{
	ACE_TRACE("[MedSLTPatPlainController::ShowError()]");	
    
	// Get GTK thread lock
	gdk_threads_enter();
			
	m_ui_dlg.ErrorDialog(m_medslt_win->GetWindow(), text);
			
	gdk_flush();
			
	// Release GTK thread lock
	gdk_threads_leave();
	
	return;
}

void MedSLTPatPlainController::ShowWarning(const char* text)
{
	ACE_TRACE("[MedSLTPatPlainController::ShowWarning()]");

	// Get GTK thread lock
	gdk_threads_enter();
			
	m_ui_dlg.WarningDialog(m_medslt_win->GetWindow(), text);
			
	gdk_flush();
			
	// Release GTK thread lock
	gdk_threads_leave();
	
	return;
}

void MedSLTPatPlainController::ShowInfo(const char* text)
{
	ACE_TRACE("[MedSLTPatPlainController::ShowInfo()]");

	// Get GTK thread lock
	gdk_threads_enter();
				
	m_ui_dlg.InfoDialog(m_medslt_win->GetWindow(), text);
				
	gdk_flush();
				
	// Release GTK thread lock
	gdk_threads_leave();	
	
	return;
}

void MedSLTPatPlainController::ShowBanner(const char* text)
{
	ACE_TRACE("[MedSLTPatPlainController::ShowBanner()]");

	// Get GTK thread lock
	gdk_threads_enter();
					
	m_ui_dlg.InfoBanner(m_medslt_win->GetWindow(), text);
					
	gdk_flush();
					
	// Release GTK thread lock
	gdk_threads_leave();
        
	return;
}

gboolean MedSLTPatPlainController::ConfirmDialog(const char* text)
{
	ACE_TRACE("[MedSLTPatPlainController::ConfirmDialog()]");
      
	gboolean conf = false;
		
	// Get GTK thread lock
	gdk_threads_enter();
							
	conf = m_ui_dlg.ConfirmDialog(m_medslt_win->GetWindow(), text);
							
	gdk_flush();
							
	// Release GTK thread lock
	gdk_threads_leave();
			
	return conf;
}
