/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MedSLTDocController.cpp
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

#include "MedSLTDocController.hpp"

MedSLTDocController::MedSLTDocController()
	: 	m_rec_in_progress(false),
		m_rec_result(""),
		m_server("0.0.0.0"),
		m_connected(false),
		m_online(true)
{
	ACE_TRACE("[MedSLTDocController::MedSLTDocController()]");

	m_medslt_win = NULL;
	m_medslt_win = new MedSLTDocWindow(*this);
	m_comm_channel = NULL;
	m_comm_channel = new CommunicationChannel(*this, 0x2E);
	
	return;
}

MedSLTDocController::~MedSLTDocController()
{
	ACE_TRACE("[MedSLTDocController::~MedSLTDocController()]");

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

void MedSLTDocController::RunApp()
{
	ACE_TRACE("[MedSLTDocController::RunApp()]");

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

gboolean MedSLTDocController::WindowStateCb(GtkWidget* widget, 
											GdkEventWindowState* event, 
											MedSLTDocController* controller)
{
	ACE_TRACE("[MedSLTDocController::WindowStateCb()]");

	if (controller != NULL)
	{
		(controller->GetWindow())->SetFullScreen(event->new_window_state &GDK_WINDOW_STATE_FULLSCREEN);
	}
	
	return FALSE;
}

gboolean MedSLTDocController::KeyPressCb(GtkWidget* widget, GdkEventKey* event,
										 MedSLTDocController* controller)
{
	ACE_TRACE("[MedSLTDocController::KeyPressCb()]");

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
void MedSLTDocController::ConnectToRemote()
{
	ACE_TRACE("[MedSLTDocController::ConnectToRemote()]");

	if (m_comm_channel != NULL)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTDocController::ConnectToRemote()] "
						"[--- Wait... ---]\n")));
		
		m_comm_channel->SetMrcpServerName(m_server);
		m_comm_channel->SetMrcpServerPort(554);
		m_comm_channel->SetTransServerName(m_server);
		m_comm_channel->SetTransServerPort(7310);
		m_comm_channel->SetBTransServerName(m_server);
		m_comm_channel->SetBTransServerPort(7313);
		m_comm_channel->SetLocalServerPort(7120);
		m_comm_channel->SetRtpLocalPort(1263);
		m_comm_channel->StartComms();
		//m_comm_channel->SendBackTranslationMessage("action(load_package('$MED_SLT2/EngEng/Prolog/med_role_marked.cfg')).\n");
		//m_comm_channel->SendTranslationMessage("action(load_package('$MED_SLT2/EngFre/Prolog/med_role_marked.cfg')).\n");
		m_comm_channel->SendBackTranslationMessage("action(load_package('c:/medSLT/MedSLT2/EngEng/Prolog/med_sore_throat_role_marked.cfg')).\n");
		m_comm_channel->SendTranslationMessage("action(load_package('c:/medSLT/MedSLT2/EngFre/Prolog/med_role_marked.cfg')).\n");
		//m_comm_channel->SendTranslationMessage("action(load_package('c:/medSLT/MedSLT2/EngSpa/Prolog/med_sore_throat_role_marked.cfg')).\n");
		m_comm_channel->SendTranslationMessage("action(execute_regulus_command(\"LOAD_HELP\")).\n");
		RecognitionGrammar gram1("SLM:public [MAIN_SLM_ENG_pharyngitis]", "medslt_slm_headache.grammar", "application/x-nuance-gsl", "");
		RecognitionGrammar gram2("http://" + m_comm_channel->GetMrcpServerName() + ":9080/MedSLT/grammars/recogniser_eng.grammar#MAIN__sore_throat", "medslt_gsl.grammar", "text/uri-list", "");
		m_comm_channel->SetGrammar(gram1);
		m_comm_channel->SetGrammar(gram2);		
	}
	
	return;
}

void MedSLTDocController::DisconnectFromRemote()
{
	ACE_TRACE("[MedSLTDocController::DisconnectFromRemote()]");
    
    if (m_comm_channel != NULL)
	{  
		m_comm_channel->StopRecording();
		m_comm_channel->StopComms();		
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] "
										"[MedSLTDocController::DisconnectFromRemote()] "
										"[--- Goodbye ---]\n")));
	}
	
	return;
}

gboolean MedSLTDocController::IsConnected() const
{
	ACE_TRACE("[MedSLTDocController::IsConnected()]");

	return m_connected;
}

void MedSLTDocController::SetConnected(gboolean connected)
{
	ACE_TRACE("[MedSLTDocController::SetConnected()]");
	
	m_connected = connected;
	
	return;
}

void MedSLTDocController::StartRecognize()
{
	ACE_TRACE("[MedSLTDocController::StartRecognize()]");
	
	if (m_medslt_win != NULL)
	{
		m_medslt_win->ResetTextList();
		m_medslt_win->ResetTextBox1();
		m_medslt_win->ResetTextBox2();
	}
	
	if (m_comm_channel != NULL)
	{
		//m_comm_channel->RevertDiscourseContext();
		SetRecInProgress(true);
		SetConfidence(30);
		RecognizeFromUser("medslt_gsl.grammar", "en-us", true);
		m_comm_channel->StartRecording();
	}
	
	return;
}

void MedSLTDocController::AbortRecognize()
{
	ACE_TRACE("[MedSLTDocController::AbortRecognize()]");
	
	if (m_comm_channel != NULL)
	{
		SetRecInProgress(false);
		m_comm_channel->StopRecording();
		m_comm_channel->StopRecognize();
	}
	
	return;
}

void MedSLTDocController::GetTranslationFromHelp(gchar* text)
{
	ACE_TRACE("[MedSLTDocController::GetTranslationFromHelp()]");

	if ((m_comm_channel != NULL) && (m_medslt_win != NULL) && (text != NULL))
	{	
		m_medslt_win->ResetTextBox1();
		m_medslt_win->ResetTextBox2();
		
		string transcription(text);		
		m_medslt_win->FindAndReplace(transcription, "?", "");
		m_medslt_win->FindAndReplace(transcription, "\n", " ");
		//m_comm_channel->RevertDiscourseContext();
		m_comm_channel->GetInterpretation(transcription, "medslt_gsl.grammar", "en-us");
		m_dlg_list.push_back("REC:\t" + transcription + "?");
	}
	
	return;
}

void MedSLTDocController::QuitApp()
{
	ACE_TRACE("[MedSLTDocController::QuitApp()]");
	
	if (m_ui_dlg.ConfirmDialog(m_medslt_win->GetWindow(), "Are you sure you want to exit?"))
	{
		DisconnectFromRemote();
		
		gtk_main_quit();	
	}
	
	return;
}

MedSLTDocWindow* MedSLTDocController::GetWindow()
{
	ACE_TRACE("[MedSLTDocController::GetWindow()]");

	if (m_medslt_win != NULL)
	{		
		return m_medslt_win;
	}
	else
	{	
		return NULL;
	}
}

void MedSLTDocController::Resize(gboolean is_full_screen)
{
	ACE_TRACE("[MedSLTDocController::Resize()]");

	if (m_medslt_win != NULL)
	{		
		m_medslt_win->Resize(is_full_screen);
	}
	
	return;
}

void MedSLTDocController::ZoomIO(gboolean zoom) const
{
	ACE_TRACE("[MedSLTDocController::ZoomIO()]");

	if (m_medslt_win != NULL)
	{		
		m_medslt_win->ZoomIO(zoom);
	}
	
	return;
}

void MedSLTDocController::SetConfidence(int value)
{
	ACE_TRACE("[MedSLTDocController::SetConfidence()]");
	
	if (m_comm_channel != NULL)
	{
		m_comm_channel->SetConfidence(value);
	}
		
	return;
}
	
int MedSLTDocController::GetConfidence()
{
	ACE_TRACE("[MedSLTDocController::GetConfidence()]");
	
	if (m_comm_channel != NULL)
	{
		return m_comm_channel->GetConfidence();
	}
	else
	{
		return -1;
	}
}

void MedSLTDocController::SetRemoteServersIP(string ip)
{
	ACE_TRACE("[MedSLTDocController::SetRemoteServersIP()]");
	
	m_server = ip;
}

void MedSLTDocController::RecognizeFromUser(const string& grammar, 
											const string& lang, bool online)
{
	ACE_TRACE("[MedSLTDocController::RecognizeFromUser()]");

	m_online = online;
		
	if (m_comm_channel != NULL)
	{
		m_comm_channel->RecognizeFromUser(grammar, lang, online);
	}
	
	return;
}

gboolean MedSLTDocController::GetRecInProgress()
{
	ACE_TRACE("[MedSLTDocController::GetRecInProgress()]");
	
	return m_rec_in_progress;
}

void MedSLTDocController::SetRecInProgress(gboolean in_progress)
{
	ACE_TRACE("[MedSLTDocController::SetRecInProgress()]");

	m_rec_in_progress = in_progress;
	
	return;
}

void MedSLTDocController::PlayResult(string& result)
{
	ACE_TRACE("[MedSLTDocController::PlayResult()]");
	
	if (m_comm_channel != NULL)
	{
		m_comm_channel->SpeakSSML("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>"
									"<speak><audio src=\"http://" 
									+ m_server + ":9080/MedSLT/audio/blip.wav\">"
									"</audio><break/>" + result + "</speak>", "en-US");		
	}
	
	return;
}

void MedSLTDocController::SendResultToClient(string& result)
{
	ACE_TRACE("[MedSLTDocController::SendResultToClient()]");
	
	if (m_comm_channel != NULL)
	{
		m_comm_channel->SendResultToClient(result);		
	}
	
	return;
}

void MedSLTDocController::ShowDlgHistory()
{
	ACE_TRACE("[MedSLTDocController::ShowDlgHistory()]");
    
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTDocController::ShowDlgHistory()] "
			"[History size: %d]\n"), m_dlg_list.size()));
	
	m_ui_dlg.ListDialog(m_medslt_win->GetWindow(), 
								"History",	m_dlg_list);

	return;
}

void MedSLTDocController::InitCompleted(CCStatus status)
{
	ACE_TRACE("[MedSLTDocController::InitCompleted()]");
	
	if (m_comm_channel != NULL)
	{
		switch (status)
		{
			case CC_CONNECTED:				
				
				m_comm_channel->Speak("Welcome.", "en-US");
								
				if (m_medslt_win != NULL)
				{
					m_medslt_win->SetUiState(m_medslt_win->UI_STATE_CONNECTED);
					SetConnected(true);
				}
				
				break;
			default:
				ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTDocController::InitCompleted()] "
						"[Error]\n")));				
		}
	}
	
	return;
}

void MedSLTDocController::StartOfSpeech()
{
	ACE_TRACE("[MedSLTDocController::StartOfSpeech()]");
	
	if ((m_medslt_win != NULL) && (m_online == true))
	{
		m_medslt_win->StartOfSpeech();
	}
	
	return;
}

void MedSLTDocController::RecognitionCompleted(int status, string& result)
{
	ACE_TRACE("[MedSLTDocController::RecognitionCompleted()]");
	
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

void MedSLTDocController::InterpretationCompleted(int status, string& result)
{
	ACE_TRACE("[MedSLTDocController::InterpretationCompleted()]");
	
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

void MedSLTDocController::QueryOutputReceived(RegulusDialogueResult* result)
{
	ACE_TRACE("[MedSLTDocController::QueryOutputReceived()]");

	return;
}

void MedSLTDocController::BackTranslationReceived(RegulusTranslationResult* result)
{
	ACE_TRACE("[MedSLTDocController::BackTranslationReceived()]");
	
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

void MedSLTDocController::GetFromHelpCompleted(string& understood, string& result)
{
	ACE_TRACE("[MedSLTDocController::GetFromHelpCompleted()]");
	
	if (m_comm_channel != NULL)
	{	
		//m_comm_channel->GetTranslation(result);
	}		
	
	return;
}

void MedSLTDocController::HelpSentencesReceived(string& buffer)
{
	ACE_TRACE("[MedSLTDocController::HelpSentencesReceived()]");
	
	if (m_medslt_win != NULL)
	{	
		m_medslt_win->SetHelpExamples(buffer);
	}
	
	return;
}

void MedSLTDocController::TranslationReceived(RegulusTranslationResult* result)
{
	ACE_TRACE("[MedSLTDocController::TranslationReceived()]");

	string translation(result->GetTextTranslation());
	
	if (m_medslt_win != NULL)
	{
		// No translation
		if (result->GetStatus() != RegulusTranslationResult::ERROR)		
		{
			m_medslt_win->FindAndReplace(translation, "\\", "");
			
			translation += "?";
			m_medslt_win->SetTranslation(translation);
			m_dlg_list.push_back("TRANS:\t" + translation);
			
			translation = "(translation='";
			translation += result->GetTranslation();
			translation += "')+(text_translation='";
			translation += result->GetTextTranslation();
			translation += "')+(target_lf=";
			translation += result->GetTargetLf();
			translation += ")+(interlingua='";
			translation += result->GetInterlingua();
			translation += "')";

			SendResultToClient(translation);
		}
		else
		{
			translation = "NO_TRANSLATION";
			m_medslt_win->SetTranslation(translation);
			m_dlg_list.push_back("TRANS:\t" + translation);
		}
	}
	
	return;
}

void MedSLTDocController::ClientMessageReceived(string& buffer)
{
	ACE_TRACE("[MedSLTDocController::ClientMessageReceived()]");
	
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTDocController::ClientMessageReceived()]"
				"[%s]\n"), buffer.c_str()));
	
	if (buffer != "")
	{
		if (m_comm_channel != NULL)
		{
			//m_comm_channel->RegisterExternalUtterance(buffer);
		}
		
		if (m_medslt_win != NULL)
		{
			m_medslt_win->ExtractOutput(buffer, "text_translation='", "')");
			m_medslt_win->FindAndReplace(buffer, "\\", "");
			//m_medslt_win->ResetTextBoxes();
			m_medslt_win->SetPatientResponse(buffer);
			m_dlg_list.push_back("RES:\t" + buffer);
			
			PlayResult(buffer);
		}
	}

	return;
}

void MedSLTDocController::ServerMessageReceived(string& buffer)
{
	ACE_TRACE("[MedSLTDocController::ServerMessageReceived()]");

	return;
}

void MedSLTDocController::ShowError(const char* text)
{
	ACE_TRACE("[MedSLTDocController::ShowError()]");	
    
	// Get GTK thread lock
	gdk_threads_enter();
			
	m_ui_dlg.ErrorDialog(m_medslt_win->GetWindow(), text);
			
	gdk_flush();
			
	// Release GTK thread lock
	gdk_threads_leave();
	
	return;
}

void MedSLTDocController::ShowWarning(const char* text)
{
	ACE_TRACE("[MedSLTDocController::ShowWarning()]");

	// Get GTK thread lock
	gdk_threads_enter();
			
	m_ui_dlg.WarningDialog(m_medslt_win->GetWindow(), text);
			
	gdk_flush();
			
	// Release GTK thread lock
	gdk_threads_leave();
	
	return;
}

void MedSLTDocController::ShowInfo(const char* text)
{
	ACE_TRACE("[MedSLTDocController::ShowInfo()]");

	// Get GTK thread lock
	gdk_threads_enter();
				
	m_ui_dlg.InfoDialog(m_medslt_win->GetWindow(), text);
				
	gdk_flush();
				
	// Release GTK thread lock
	gdk_threads_leave();
	
	return;
}

void MedSLTDocController::ShowBanner(const char* text)
{
	ACE_TRACE("[MedSLTDocController::ShowBanner()]");

	// Get GTK thread lock
	gdk_threads_enter();
						
	m_ui_dlg.InfoBanner(m_medslt_win->GetWindow(), text);
						
	gdk_flush();
						
	// Release GTK thread lock
	gdk_threads_leave();	
		
	return;
}

gboolean MedSLTDocController::ConfirmDialog(const char* text)
{
	ACE_TRACE("[MedSLTDocController::ConfirmDialog()]");

	gboolean conf = false;
	
	// Get GTK thread lock
	gdk_threads_enter();
						
	conf = m_ui_dlg.ConfirmDialog(m_medslt_win->GetWindow(), text);
						
	gdk_flush();
						
	// Release GTK thread lock
	gdk_threads_leave();
		
	return conf;
}
