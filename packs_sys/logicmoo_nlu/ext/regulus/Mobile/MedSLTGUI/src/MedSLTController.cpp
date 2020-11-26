/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MedSLTController.cpp
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

#include "MedSLTController.hpp"

MedSLTController::MedSLTController(InputLang lang)
	: 	m_rec_in_progress(false),
		m_rec_result(""),
		m_server("0.0.0.0"),
		m_connected(false),
		m_online(true),
		m_ilang(lang)
{
	ACE_TRACE("[MedSLTController::MedSLTController()]");
	
	m_medslt_win = NULL;
	m_medslt_win = new MedSLTWindow(*this, m_ilang);
	m_comm_channel = NULL;
	m_comm_channel = new CommunicationChannel(*this, 0xE);
	
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
//	g_signal_connect(G_OBJECT(m_medslt_win->GetWindow()), "window_state_event",
//						G_CALLBACK(WindowStateCb), this);
//	g_signal_connect(G_OBJECT(m_medslt_win->GetWindow()), "key_press_event",
//						G_CALLBACK(KeyPressCb), this);
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
		case GDK_Escape:
			controller->ShowDlgHistory();		
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
			controller->ZoomIO(true);
			break;
		case GDK_F8:
			controller->ZoomIO(false);
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
			if (controller->IsConnected() == true)
			{
				if (controller->GetRecInProgress() == false)
				{
					(controller->GetWindow())->SetUiState((controller->GetWindow())->UI_STATE_START_RECOGNIZE);
					controller->StartRecognize();
				}
				else
				{
					(controller->GetWindow())->SetUiState((controller->GetWindow())->UI_STATE_ABORT_RECOGNIZE);
					controller->AbortRecognize();
				}
			}
			break;
		default:
			return FALSE;						
	}

	return TRUE;
}

void MedSLTController::ConnectToRemote()
{
	ACE_TRACE("[MedSLTController::ConnectToRemote()]");

	if (m_comm_channel != NULL)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTController::ConnectToRemote()] "
										"[--- Wait... ---]\n")));
		
		m_comm_channel->SetMrcpServerName(m_server);
		
		if (GetInputLang() == INPUT_LANG_FRE)
		{	
			m_comm_channel->SetMrcpServerPort(556);
			//m_comm_channel->SetTransServerPort(7311);
			//m_comm_channel->SetBTransServerPort(7338);
			m_comm_channel->SetTransServerPort(7310);
			m_comm_channel->SetBTransServerPort(7313);
		}
		else
		{
			m_comm_channel->SetMrcpServerPort(554);
			m_comm_channel->SetTransServerPort(7310);
			m_comm_channel->SetBTransServerPort(7313);			
		}
		
		m_comm_channel->SetTransServerName(m_server);
		m_comm_channel->SetBTransServerName(m_server);		

		m_comm_channel->SetRtpLocalPort(1263);
		m_comm_channel->StartComms();
		//m_comm_channel->SendBackTranslationMessage("action(load_package('$MED_SLT2/EngEng/Prolog/med_role_marked.cfg')).\n");
		//m_comm_channel->SendTranslationMessage("action(load_package('$MED_SLT2/EngFre/Prolog/med_role_marked.cfg')).\n");
		
		if (GetInputLang() == INPUT_LANG_FRE)
		{		
			m_comm_channel->SendBackTranslationMessage("action(load_package('c:/medSLT/MedSLT2/FreFre/Prolog/med_role_marked.cfg')).\n");
			m_comm_channel->SendTranslationMessage("action(load_package('c:/medSLT/MedSLT2/FreEng/Prolog/med_role_marked.cfg')).\n");
			m_comm_channel->SendTranslationMessage("action(execute_regulus_command(\"LOAD_HELP\")).\n");
		
			RecognitionGrammar gram1("SLM:public [MAIN_SLM_FRE_headache]", "medslt_slm_headache.grammar", "application/x-nuance-gsl", "");
			RecognitionGrammar gram2("http://" + m_comm_channel->GetMrcpServerName() + ":9080/MedSLT/grammars/recogniser_fre.grammar#MAIN", "medslt_gsl.grammar", "text/uri-list", "");
		
			m_comm_channel->SetGrammar(gram1);
			m_comm_channel->SetGrammar(gram2);
		}
		else
		{
			m_comm_channel->SendBackTranslationMessage("action(load_package('c:/medSLT/MedSLT2/EngEng/Prolog/med_role_marked.cfg')).\n");
			m_comm_channel->SendTranslationMessage("action(load_package('c:/medSLT/MedSLT2/EngFre/Prolog/med_role_marked.cfg')).\n");
			m_comm_channel->SendTranslationMessage("action(execute_regulus_command(\"LOAD_HELP\")).\n");
		
			RecognitionGrammar gram1("SLM:public [MAIN_SLM_ENG_headache]", "medslt_slm_headache.grammar", "application/x-nuance-gsl", "");
			RecognitionGrammar gram2("http://" + m_comm_channel->GetMrcpServerName() + ":9080/MedSLT/grammars/recogniser_eng.grammar#MAIN", "medslt_gsl.grammar", "text/uri-list", "");
		
			m_comm_channel->SetGrammar(gram1);
			m_comm_channel->SetGrammar(gram2);
		}
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

gboolean MedSLTController::IsConnected() const
{
	ACE_TRACE("[MedSLTController::IsConnected()]");

	return m_connected;
}

void MedSLTController::SetConnected(gboolean connected)
{
	ACE_TRACE("[MedSLTController::SetConnected()]");
	
	m_connected = connected;
	
	return;
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
		SetRecInProgress(true);
		SetConfidence(30);
		RecognizeFromUser("medslt_gsl.grammar", 
							(GetInputLang() == INPUT_LANG_FRE) ? "fr-ca" : "en-us", true);
		m_comm_channel->StartRecording();
	}
	
	return;
}

void MedSLTController::AbortRecognize()
{
	ACE_TRACE("[MedSLTController::AbortRecognize()]");
	
	if (m_comm_channel != NULL)
	{
		SetRecInProgress(false);
		m_comm_channel->StopRecording();
		m_comm_channel->StopRecognize();
	}
	
	return;
}

void MedSLTController::GetTranslationFromHelp(gchar* text)
{
	ACE_TRACE("[MedSLTController::GetTranslationFromHelp()]");

	if (m_medslt_win != NULL)
	{
		m_medslt_win->ResetTranslationBox();
	}
	
	if ((m_comm_channel != NULL) && (m_medslt_win != NULL) && (text != NULL))
	{	
		GError *error = NULL;
		gchar* iso = NULL;		
		
		iso = g_convert(text, -1, "ISO-8859-1", "UTF-8", NULL, NULL, &error);
		
		// Now its time to free the variable
		g_free(text);
		
		if (iso == NULL)
		{
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTController::SetTranslation()] "
											"[Failed: %s]\n"), error->message));		
		}
		else
		{
			string transcription(iso);
			m_medslt_win->FindAndReplace(transcription, "?", "");
			m_medslt_win->FindAndReplace(transcription, "\n", " ");
			m_comm_channel->GetInterpretation(transcription, "medslt_gsl.grammar", 
												(GetInputLang() == INPUT_LANG_FRE) ? "fr-ca" : "en-us");
			m_dlg_list.push_back("REC:\t" + transcription + "?");
		}
			
		g_free(iso);
		g_free(error);					
	}
		
	return;
}

void MedSLTController::QuitApp()
{
	ACE_TRACE("[MedSLTController::QuitApp()]");
	
	if (m_ui_dlg.ConfirmDialog(m_medslt_win->GetWindow(), "Are you sure you want to exit?"))
	{
		DisconnectFromRemote();
		
		gtk_main_quit();	
	}
	
	return;
}

MedSLTWindow* MedSLTController::GetWindow() const
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

void MedSLTController::Resize(gboolean is_full_screen) const
{
	ACE_TRACE("[MedSLTController::Resize()]");

	if (m_medslt_win != NULL)
	{		
		m_medslt_win->Resize(is_full_screen);
	}
	
	return;
}

void MedSLTController::ZoomIO(gboolean zoom) const
{
	ACE_TRACE("[MedSLTController::ZoomIO()]");

	if (m_medslt_win != NULL)
	{		
		m_medslt_win->ZoomIO(zoom);
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
	
int MedSLTController::GetConfidence() const
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

InputLang MedSLTController::GetInputLang()
{
	ACE_TRACE("[MedSLTController::GetInputLang()]");
	
	return m_ilang;
}

void MedSLTController::SetInputLang(InputLang lang)
{
	ACE_TRACE("[MedSLTController::SetInputLang()]");
	
	m_ilang = lang;
	
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

gboolean MedSLTController::GetRecInProgress()
{
	ACE_TRACE("[MedSLTController::GetRecInProgress()]");
	
	return m_rec_in_progress;
}

void MedSLTController::SetRecInProgress(gboolean in_progress)
{
	ACE_TRACE("[MedSLTController::SetRecInProgress()]");

	m_rec_in_progress = in_progress;
	
	return;
}

void MedSLTController::PlayResult(string& result)
{
	ACE_TRACE("[MedSLTController::PlayResult()]");
	
	if (m_comm_channel != NULL)
	{
		m_comm_channel->SpeakSSML("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>"
									"<speak><audio src=\"http://" 
									+ m_server + ":9080/MedSLT/audio/blip.wav\">"
									"</audio><break/>" + result + "</speak>", 
									(GetInputLang() == INPUT_LANG_ENG) ? "fr-CA" : "en-US");		
	}
	
	return;
}

void MedSLTController::ShowDlgHistory()
{
	ACE_TRACE("[MedSLTController::ShowDlgHistory()]");
    
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTController::ShowDlgHistory()] "
			"[History size: %d]\n"), m_dlg_list.size()));
	
	m_ui_dlg.ListDialog(m_medslt_win->GetWindow(), 
								"History",	m_dlg_list);

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
				
				if (GetInputLang() == INPUT_LANG_FRE)
				{
					m_comm_channel->Speak("Bienvenue.", "fr-CA");
				}
				else
				{
					m_comm_channel->Speak("Welcome.", "en-US");
				}
								
				if (m_medslt_win != NULL)
				{
					m_medslt_win->SetUiState(m_medslt_win->UI_STATE_CONNECTED);
					SetConnected(true);
				}
				
				break;
			default:
				ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTController::InitCompleted()] "
						"[Error]\n")));				
		}
	}
	
	return;
}

void MedSLTController::StartOfSpeech()
{
	ACE_TRACE("[MedSLTController::StartOfSpeech()]");
	
	if ((m_medslt_win != NULL) && (m_online == true))
	{
		m_medslt_win->StartOfSpeech();
	}
	
	return;
}

void MedSLTController::RecognitionCompleted(int status, string& result)
{
	ACE_TRACE("[MedSLTController::RecognitionCompleted()]");
	
	SetRecInProgress(false);
	
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
				RecognizeFromUser("medslt_slm_headache.grammar", 
									(GetInputLang() == INPUT_LANG_FRE) ? "fr-ca" : "en-us", m_online);			
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
					ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTController::RecognitionCompleted()] "
								"[Failed: %s]\n"), error->message));		
				}
				else
				{
					string transcription(iso);
					m_comm_channel->GetBackTranslation(transcription);
				}
							
				g_free(iso);
				g_free(error);
				
				RecognizeFromUser("medslt_slm_headache.grammar", 
									(GetInputLang() == INPUT_LANG_FRE) ? "fr-ca" : "en-us", m_online);
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
		string tmp("");
		PlayResult(tmp);
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
				ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTController::SetTranslation()] "
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

void MedSLTController::QueryOutputReceived(RegulusDialogueResult* result)
{
	ACE_TRACE("[MedSLTController::QueryOutputReceived()]");

	return;
}

void MedSLTController::BackTranslationReceived(RegulusTranslationResult* result)
{
	ACE_TRACE("[MedSLTController::BackTranslationReceived()]");
	
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

void MedSLTController::TranslationReceived(RegulusTranslationResult* result)
{
	ACE_TRACE("[MedSLTController::TranslationReceived()]");

	string translation(result->GetTextTranslation());
	
	if (m_medslt_win != NULL)
	{
		// No translation
		if (result->GetStatus() != RegulusTranslationResult::ERROR)		
		{
			translation += "?";
			m_medslt_win->SetTranslation(translation);			
		}
		
		m_dlg_list.push_back("TRANS:\t" + translation);
		PlayResult(translation);
	}

	return;
}

void MedSLTController::ClientMessageReceived(string& buffer)
{
	ACE_TRACE(ACE_TEXT("[MedSLTController::ClientMessageReceived()]"));
			
	return;
}

void MedSLTController::ServerMessageReceived(string& buffer)
{
	ACE_TRACE(ACE_TEXT("[MedSLTController::ServerMessageReceived()]"));
			
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

void MedSLTController::ShowError(const char* text)
{
	ACE_TRACE("[MedSLTController::ShowError()]");	
    
	// Get GTK thread lock
	gdk_threads_enter();
				
	m_ui_dlg.ErrorDialog(m_medslt_win->GetWindow(), text);
				
	gdk_flush();
				
	// Release GTK thread lock
	gdk_threads_leave();
	
	return;
}

void MedSLTController::ShowWarning(const char* text)
{
	ACE_TRACE("[MedSLTController::ShowWarning()]");

	// Get GTK thread lock
	gdk_threads_enter();
			
	m_ui_dlg.WarningDialog(m_medslt_win->GetWindow(), text);
			
	gdk_flush();
			
	// Release GTK thread lock
	gdk_threads_leave();
	
	return;
}

void MedSLTController::ShowInfo(const char* text)
{
	ACE_TRACE("[MedSLTController::ShowInfo()]");

	// Get GTK thread lock
	gdk_threads_enter();
				
	m_ui_dlg.InfoDialog(m_medslt_win->GetWindow(), text);
				
	gdk_flush();
				
	// Release GTK thread lock
	gdk_threads_leave();	
	
	return;
}

void MedSLTController::ShowBanner(const char* text)
{
	ACE_TRACE("[MedSLTController::ShowBanner()]");

	// Get GTK thread lock
	gdk_threads_enter();
					
	m_ui_dlg.InfoBanner(m_medslt_win->GetWindow(), text);
					
	gdk_flush();
					
	// Release GTK thread lock
	gdk_threads_leave();
        
	return;
}

gboolean MedSLTController::ConfirmDialog(const char* text)
{
	ACE_TRACE("[MedSLTController::ConfirmDialog()]");
	
	gboolean conf = false;
	
	// Get GTK thread lock
	gdk_threads_enter();
						
	conf = m_ui_dlg.ConfirmDialog(m_medslt_win->GetWindow(), text);
						
	gdk_flush();
						
	// Release GTK thread lock
	gdk_threads_leave();     
	
	return conf;
}
