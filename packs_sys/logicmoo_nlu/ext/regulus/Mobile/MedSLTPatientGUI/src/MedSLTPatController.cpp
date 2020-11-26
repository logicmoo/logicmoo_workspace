/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MedSLTPatController.cpp
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

#include "MedSLTPatController.hpp"

MedSLTPatController::MedSLTPatController()
	: 	m_rec_result(""),
		m_server("0.0.0.0"),
		m_online(true)
{
	ACE_TRACE("[MedSLTPatController::MedSLTPatController()]");

	m_medslt_win = NULL;
	m_medslt_win = new MedSLTPatWindow(*this);
	m_comm_channel = NULL;
	m_comm_channel = new CommunicationChannel(*this, 0x4E);
	
	return;
}

MedSLTPatController::~MedSLTPatController()
{
	ACE_TRACE("[MedSLTPatController::~MedSLTPatController()]");

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

void MedSLTPatController::RunApp()
{
	ACE_TRACE("[MedSLTPatController::RunApp()]");

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

gboolean MedSLTPatController::WindowStateCb(GtkWidget* widget, 
											GdkEventWindowState* event, 
											MedSLTPatController* controller)
{
	ACE_TRACE("[MedSLTPatController::WindowStateCb()]");

	if (controller != NULL)
	{
		(controller->GetWindow())->SetFullScreen(event->new_window_state &GDK_WINDOW_STATE_FULLSCREEN);
	}
	
	return FALSE;
}

gboolean MedSLTPatController::KeyPressCb(GtkWidget* widget, GdkEventKey* event,
										 MedSLTPatController* controller)
{
	ACE_TRACE("[MedSLTPatController::KeyPressCb()]");

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
			(controller->GetWindow())->IterateList(false);
			break;
		case GDK_Down:
			(controller->GetWindow())->IterateList(true);
			break;
		case GDK_Return:
			controller->GetTranslationFromHelp((controller->GetWindow())->GetRowData());
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

void MedSLTPatController::ConnectToRemote()
{
	ACE_TRACE("[MedSLTPatController::ConnectToRemote()]");

	if (m_comm_channel != NULL)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] "
										"[MedSLTPatController::ConnectToRemote()] "
										"[--- Wait... ---]\n")));
		
		m_comm_channel->SetMrcpServerName(m_server);
		m_comm_channel->SetMrcpServerPort(556);
		m_comm_channel->SetTransServerName(m_server);
		m_comm_channel->SetTransServerPort(7311);
		m_comm_channel->SetBTransServerName(m_server);
		m_comm_channel->SetBTransServerPort(7338);
		//m_comm_channel->SetLocalServerName("129.194.32.96");
		m_comm_channel->SetLocalServerName("129.194.33.196");
		m_comm_channel->SetLocalServerPort(7120);
		m_comm_channel->SetRtpLocalPort(1263);
		m_comm_channel->StartComms();
		//m_comm_channel->SendBackTranslationMessage("action(load_package('$MED_SLT2/EngEng/Prolog/med_role_marked.cfg')).\n");
		//m_comm_channel->SendTranslationMessage("action(load_package('$MED_SLT2/EngFre/Prolog/med_role_marked.cfg')).\n");
		//m_comm_channel->SendBackTranslationMessage("action(load_package('c:/medSLT/MedSLT2/FreFre/Prolog/med_role_marked.cfg')).\n");
		//m_comm_channel->SendTranslationMessage("action(load_package('c:/medSLT/MedSLT2/FreEng/Prolog/med_role_marked.cfg')).\n");
		m_comm_channel->SendBackTranslationMessage("action(load_package('c:/medSLT/MedSLT2/SpaSpa/Prolog/med_answers_role_marked.cfg')).\n");
		m_comm_channel->SendBackTranslationMessage("action(execute_regulus_command(\"ANSWER_ELLIPSIS_ON\")).\n");
		m_comm_channel->SendTranslationMessage("action(load_package('c:/medSLT/MedSLT2/SpaEng/Prolog/med_answers_role_marked.cfg')).\n");
		m_comm_channel->SendTranslationMessage("action(execute_regulus_command(\"ANSWER_ELLIPSIS_ON\")).\n");
		m_comm_channel->SendTranslationMessage("action(execute_regulus_command(\"LOAD_HELP\")).\n");
//		RecognitionGrammar gram1("SLM:public [MAIN_SLM_ENG_headache]", "medslt_slm_headache.grammar", "application/x-nuance-gsl", "");
//		RecognitionGrammar gram2("http://" + m_comm_channel->GetMrcpServerName() + ":9080/MedSLT/grammars/recogniser_eng.grammar#MAIN", "medslt_gsl.grammar", "text/uri-list", "");
//		RecognitionGrammar gram1("SLM:public [MAIN_SLM_FRE_headache]", "medslt_slm_headache.grammar", "application/x-nuance-gsl", "");
//		RecognitionGrammar gram2("http://" + m_comm_channel->GetMrcpServerName() + ":9080/MedSLT/grammars/recogniser_fre.grammar#MAIN", "medslt_gsl.grammar", "text/uri-list", "");
		RecognitionGrammar gram1("SLM:public [MAIN_SLM_SPA_pharyngitis]", "medslt_slm_headache.grammar", "application/x-nuance-gsl", "");
		RecognitionGrammar gram2("http://" + m_comm_channel->GetMrcpServerName() + ":9080/MedSLT/grammars/recogniser_spa.grammar#MAIN__a_sore_throat", "medslt_gsl.grammar", "text/uri-list", "");
		m_comm_channel->SetGrammar(gram1);
		m_comm_channel->SetGrammar(gram2);		
	}
	
	return;
}

void MedSLTPatController::DisconnectFromRemote()
{
	ACE_TRACE("[MedSLTPatController::DisconnectFromRemote()]");
    
    if (m_comm_channel != NULL)
	{  
		m_comm_channel->StopRecording();
		m_comm_channel->StopComms();		
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTPatController::DisconnectFromRemote()] "
						"[--- Goodbye ---]\n")));
	}
	
	return;
}

void MedSLTPatController::StartRecognize()
{
	ACE_TRACE("[MedSLTPatController::StartRecognize()]");
	
	if (m_medslt_win != NULL)
	{
		m_medslt_win->ResetTextList();
		m_medslt_win->ResetTextBox2();
	}
	
	if (m_comm_channel != NULL)
	{
		SetConfidence(30);
		RecognizeFromUser("medslt_gsl.grammar", "es-cala", true);
		m_comm_channel->StartRecording();
	}
	
	return;
}

void MedSLTPatController::AbortRecognize()
{
	ACE_TRACE("[MedSLTPatController::AbortRecognize()]");
	
	if (m_comm_channel != NULL)
	{
		m_comm_channel->StopRecording();
		m_comm_channel->StopRecognize();
	}
	
	return;
}

void MedSLTPatController::GetTranslationFromHelp(gchar* text)
{
	ACE_TRACE("[MedSLTPatController::GetTranslationFromHelp()]");

	if ((m_comm_channel != NULL) && (m_medslt_win != NULL) && (text != NULL))
	{
		GError *error = NULL;
		gchar* iso = NULL;
		
		m_medslt_win->ResetTextBox2();				
		
		iso = g_convert(text, -1, "ISO-8859-1", "UTF-8", NULL, NULL, &error);
			
		if (iso == NULL)
		{
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTDocWindow::SetTranslation()] "
											"[Failed: %s]\n"), error->message));		
		}
		else
		{
			string transcription(iso);
			m_medslt_win->FindAndReplace(transcription, "?", "");
			m_comm_channel->GetInterpretation(transcription, "medslt_gsl.grammar", "es-cala");
		}
			
		g_free(iso);
		g_free(error);					
	}
		
	return;
}

void MedSLTPatController::QuitApp()
{
	ACE_TRACE("[MedSLTPatController::QuitApp()]");
	
	if (m_ui_dlg.ConfirmDialog(m_medslt_win->GetWindow(), "Are you sure you want to exit?"))
	{
		DisconnectFromRemote();
		
		gtk_main_quit();	
	}
	
	return;
}

MedSLTPatWindow* MedSLTPatController::GetWindow() const
{
	ACE_TRACE("[MedSLTPatController::GetWindow()]");

	if (m_medslt_win != NULL)
	{		
		return m_medslt_win;
	}
	else
	{	
		return NULL;
	}
}

void MedSLTPatController::Resize(gboolean is_full_screen) const
{
	ACE_TRACE("[MedSLTPatController::Resize()]");

	if (m_medslt_win != NULL)
	{		
		m_medslt_win->Resize(is_full_screen);
	}
	
	return;
}

void MedSLTPatController::SetConfidence(int value)
{
	ACE_TRACE("[MedSLTPatController::SetConfidence()]");
	
	if (m_comm_channel != NULL)
	{
		m_comm_channel->SetConfidence(value);
	}
		
	return;
}
	
int MedSLTPatController::GetConfidence() const
{
	ACE_TRACE("[MedSLTPatController::GetConfidence()]");
	
	if (m_comm_channel != NULL)
	{
		return m_comm_channel->GetConfidence();
	}
	else
	{
		return -1;
	}
}

void MedSLTPatController::SetRemoteServersIP(string ip)
{
	ACE_TRACE("[MedSLTPatController::SetRemoteServersIP()]");
	
	m_server = ip;
}

void MedSLTPatController::RecognizeFromUser(const string& grammar, 
											const string& lang, bool online)
{
	ACE_TRACE("[MedSLTPatController::RecognizeFromUser()]");

	m_online = online;
		
	if (m_comm_channel != NULL)
	{
		m_comm_channel->RecognizeFromUser(grammar, lang, online);
	}
	
	return;
}

void MedSLTPatController::PlayResult(string& result) const
{
	ACE_TRACE("[MedSLTPatController::PlayResult()]");
	
	if (m_comm_channel != NULL)
	{
		m_comm_channel->SpeakSSML("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>"
									"<speak><audio src=\"http://" 
									+ m_server + ":9080/MedSLT/audio/blip.wav\">"
									"</audio><break/>" + result + "</speak>", "es-CALA");		
	}
	
	return;
}

void MedSLTPatController::SendResultToServer(string& result) const
{
	ACE_TRACE("[MedSLTPatController::SendResultToServer()]");
	
	if (m_comm_channel != NULL)
	{
		m_comm_channel->SendResultToServer(result);		
	}
	
	return;
}

void MedSLTPatController::InitCompleted(CCStatus status)
{
	ACE_TRACE("[MedSLTPatController::InitCompleted()]");
	
	if (m_comm_channel != NULL)
	{
		switch (status)
		{
			case CC_CONNECTED:
				m_comm_channel->Speak("Bienvenida.", "es-CALA");
								
				if (m_medslt_win != NULL)
				{
					m_medslt_win->SetUiState(m_medslt_win->UI_STATE_CONNECTED);
				}
				
				break;
			default:
				ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTPatController::InitCompleted()] "
						"[Error]\n")));				
		}
	}
	
	return;
}

void MedSLTPatController::StartOfSpeech()
{
	ACE_TRACE("[MedSLTPatController::StartOfSpeech()]");
	
	if ((m_medslt_win != NULL) && (m_online == true))
	{
		m_medslt_win->StartOfSpeech();
	}
	
	return;
}

void MedSLTPatController::RecognitionCompleted(int status, string& result)
{
	ACE_TRACE("[MedSLTPatController::RecognitionCompleted()]");
	
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
				RecognizeFromUser("medslt_slm_headache.grammar", "es-cala", m_online);			
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
				m_comm_channel->SpeakSSML("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>"
											"<speak><audio src=\"http://" 
											+ m_server + ":9080/MedSLT/audio/beep.wav\">"
											"</audio><break/>Repeat please.</speak>", "en-US");
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
				GError *error = NULL;
				gchar* iso = NULL;
										
				iso = g_convert(m_rec_result.c_str(), -1, "ISO-8859-1", "UTF-8", NULL, NULL, &error);
							
				if (iso == NULL)
				{
					ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTDocWindow::RecognitionCompleted()] "
							"[Failed: %s]\n"), error->message));		
				}
				else
				{
					string transcription(iso);
					m_comm_channel->GetBackTranslation(transcription);
				}
							
				g_free(iso);
				g_free(error);
							
				//m_comm_channel->GetBackTranslation(m_rec_result);		
				RecognizeFromUser("medslt_slm_headache.grammar", "es-cala", m_online);
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

void MedSLTPatController::InterpretationCompleted(int status, string& result)
{
	ACE_TRACE("[MedSLTPatController::InterpretationCompleted()]");
	
	// NO_MATCH
	if (status == 1)
	{
		// TODO: do something
	}
	else
	{	
		if (m_comm_channel != NULL)
		{	
			GError *error = NULL;
			gchar* iso = NULL;
									
			iso = g_convert(result.c_str(), -1, "ISO-8859-1", "UTF-8", NULL, NULL, &error);
						
			if (iso == NULL)
			{
				ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTDocWindow::SetTranslation()] "
						"[Failed: %s]\n"), error->message));		
			}
			else
			{
				string transcription(iso);
				m_comm_channel->GetTranslation(transcription);
			}
						
			g_free(iso);
			g_free(error);
		}
	}	
	
	return;
}

void MedSLTPatController::QueryOutputReceived(RegulusDialogueResult* result)
{
	ACE_TRACE("[MedSLTPatController::QueryOutputReceived()]");

	return;
}

void MedSLTPatController::BackTranslationReceived(RegulusTranslationResult* result)
{
	ACE_TRACE("[MedSLTPatController::BackTranslationReceived()]");
	
	string translation(result->GetTextTranslation());
	
	if (m_medslt_win != NULL)
	{
		// No back translation
		if (translation != "")		
		{
			m_medslt_win->SetBackTranslation(translation);
			//m_medslt_win->SetTranslation(result);
		}		
	}
	
	return;
}

void MedSLTPatController::TranslationReceived(RegulusTranslationResult* result)
{
	ACE_TRACE("[MedSLTPatController::TranslationReceived()]");

	string translation(result->GetTextTranslation());
	
	if (m_medslt_win != NULL)
	{
		// No translation
		if (result->GetStatus() != RegulusTranslationResult::ERROR)		
		{
			m_medslt_win->FindAndReplace(translation, "\\", "");
			
			m_medslt_win->SetTranslation(translation);
			
			translation = "(translation='";
			translation += result->GetTranslation();
			translation += "')+(text_translation='";
			translation += result->GetTextTranslation();
			translation += "')+(target_lf=";
			translation += result->GetTargetLf();
			translation += ")+(interlingua='";
			translation += result->GetInterlingua();
			translation += "')";

			SendResultToServer(translation);
		}
		else
		{
			translation = "NO_TRANSLATION";
			m_medslt_win->SetTranslation(translation);
		}
	}
	
	return;
}

void MedSLTPatController::ClientMessageReceived(string& buffer)
{
	ACE_TRACE(ACE_TEXT("[MedSLTPatController::ClientMessageReceived()]"));
	
	return;
}

void MedSLTPatController::ServerMessageReceived(string& buffer)
{
	ACE_TRACE(ACE_TEXT("[MedSLTPatController::ServerMessageReceived()]"));

	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTPatController::ServerMessageReceived()]"
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
			
			m_medslt_win->ResetTextBoxes();
			m_medslt_win->SetDoctorQuestion(buffer);
			
			PlayResult(buffer);
		}
	}

	return;
}

void MedSLTPatController::GetFromHelpCompleted(string& understood, string& result)
{
	ACE_TRACE("[MedSLTPatController::GetFromHelpCompleted()]");
	
	if (m_comm_channel != NULL)
	{	
		//m_comm_channel->GetTranslation(result);
	}		
	
	return;
}

void MedSLTPatController::HelpSentencesReceived(string& buffer)
{
	ACE_TRACE("[MedSLTPatController::HelpSentencesReceived()]");
	
	if (m_medslt_win != NULL)
	{	
		m_medslt_win->SetHelpExamples(buffer);
	}
	
	return;
}

void MedSLTPatController::ShowError(const char* text)
{
	ACE_TRACE("[MedSLTPatController::ShowError()]");	
    
	// Get GTK thread lock
	gdk_threads_enter();
			
	m_ui_dlg.ErrorDialog(m_medslt_win->GetWindow(), text);
			
	gdk_flush();
			
	// Release GTK thread lock
	gdk_threads_leave();
	
	return;
}

void MedSLTPatController::ShowWarning(const char* text)
{
	ACE_TRACE("[MedSLTPatController::ShowWarning()]");

	// Get GTK thread lock
	gdk_threads_enter();
			
	m_ui_dlg.WarningDialog(m_medslt_win->GetWindow(), text);
			
	gdk_flush();
			
	// Release GTK thread lock
	gdk_threads_leave();
	
	return;
}

void MedSLTPatController::ShowInfo(const char* text)
{
	ACE_TRACE("[MedSLTPatController::ShowInfo()]");

	// Get GTK thread lock
	gdk_threads_enter();
				
	m_ui_dlg.InfoDialog(m_medslt_win->GetWindow(), text);
				
	gdk_flush();
				
	// Release GTK thread lock
	gdk_threads_leave();	
	
	return;
}

void MedSLTPatController::ShowBanner(const char* text)
{
	ACE_TRACE("[MedSLTPatController::ShowBanner()]");

	// Get GTK thread lock
	gdk_threads_enter();
					
	m_ui_dlg.InfoBanner(m_medslt_win->GetWindow(), text);
					
	gdk_flush();
					
	// Release GTK thread lock
	gdk_threads_leave();
        
	return;
}

gboolean MedSLTPatController::ConfirmDialog(const char* text)
{
	ACE_TRACE("[MedSLTPatController::ConfirmDialog()]");
      
	gboolean conf = false;
		
	// Get GTK thread lock
	gdk_threads_enter();
							
	conf = m_ui_dlg.ConfirmDialog(m_medslt_win->GetWindow(), text);
							
	gdk_flush();
							
	// Release GTK thread lock
	gdk_threads_leave();
			
	return conf;
}
