/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MedSLTController.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#ifndef MEDSLTCONTROLLER_HPP_
#define MEDSLTCONTROLLER_HPP_

#include "MedSLTWindow.hpp"
#include "CommunicationChannel.hpp"

//#include <libosso.h>
#include "MedSLTWindow.hpp"
#include "MedSLTWindowObserver.hpp"
#include "UiDialogs.hpp"
#include "CommunicationChannel.hpp"
#include "CommunicationChannelObserver.hpp"

class MedSLTController : public MedSLTWindowObserver, 
									CommunicationChannelObserver
{
public:
	
	MedSLTController(InputLang lang);
	~MedSLTController();
	
	void RunApp();
	static gboolean WindowStateCb(GtkWidget* widget, 
									GdkEventWindowState* event, 
									MedSLTController* controller);
	static gboolean KeyPressCb(GtkWidget* widget, GdkEventKey* event,
								MedSLTController* controller);
	//static void hw_event_handler(osso_hw_state_t *state, gpointer data);
	
	// From MedSLTWindowObserver.hpp
	// Connect to the MRCP/Regulus remote servers
	void ConnectToRemote();
	// Disconnect from the MRCP/Regulus remote servers
	void DisconnectFromRemote();
	// Is connected to servers
	gboolean IsConnected() const;
	// Set connected to servers
	void SetConnected(gboolean connected);
	// Recognize using user's speech 
	void StartRecognize();	
	void AbortRecognize();
	// Translate from a help sentence
	void GetTranslationFromHelp(gchar* text);
	void QuitApp();
	
	MedSLTWindow* GetWindow() const;
	void Resize(gboolean is_full_screen) const;
	void ZoomIO(gboolean zoom) const;
	
	// Set the recognition confidence value
	void SetConfidence(int value);	
	// Get the recognition confidence value 
	int GetConfidence() const;
	// Set the ip address of the remote peers
	void SetRemoteServersIP(string ip);
	// Get the input language
	InputLang GetInputLang();
	// Set the input language
	void SetInputLang(InputLang lang);
	
	// Recognize using user's speech
	void RecognizeFromUser(const string& grammar, 
							const string& lang, bool online);
	// Get recognition in progress
	gboolean GetRecInProgress();
	// Set recognition in progress
	void SetRecInProgress(gboolean in_progress);
	// Playback the translation result
	void PlayResult(string& result);
	// Show the history of the dialogue
	void ShowDlgHistory();
	
	// From CommunicationChannelObserver
	void InitCompleted(CCStatus status);
	void StartOfSpeech();
	void RecognitionCompleted(int status, string& result);
	void InterpretationCompleted(int status, string& result);
	void QueryOutputReceived(RegulusDialogueResult* result);	
	void GetFromHelpCompleted(string& understood, string& result);
	void HelpSentencesReceived(string& buffer);
	void BackTranslationReceived(RegulusTranslationResult* result);	
	void TranslationReceived(RegulusTranslationResult* result);
	void ClientMessageReceived(string& buffer);
	void ServerMessageReceived(string& buffer);
	
	// Show an error dialog with an OK button
	void ShowError(const char* text);
	// Show a warning dialog with an OK button
	void ShowWarning(const char* text);
	// Show an info dialog with an OK button
	void ShowInfo(const char* text);
	// Show an info banner (with no buttons if possible)
	void ShowBanner(const char* text);
	// Show an OK/cancel dialog to request confirmation from the user
	gboolean ConfirmDialog(const char* text);
	
private:
	
	// Avoid accidental copy or assignment
	MedSLTController(const MedSLTController&);
	MedSLTController& operator = (const MedSLTController&);
	
public:
	
	MedSLTWindow*			m_medslt_win;
	
private:		

	gboolean				m_rec_in_progress;
	string					m_rec_result;
	string					m_server;
	gboolean				m_connected;
	gboolean				m_online;
	InputLang 				m_ilang;
	vector<string> 			m_dlg_list;
	
	UiDialogs				m_ui_dlg;
	
	CommunicationChannel*	m_comm_channel;
};

#endif /*CONTROLLER_HPP_*/
