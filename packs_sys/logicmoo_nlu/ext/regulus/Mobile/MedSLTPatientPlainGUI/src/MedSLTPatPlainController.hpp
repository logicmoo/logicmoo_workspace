/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MedSLTPatPlainController.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#ifndef MEDSLTPATPLAINCONTROLLER_HPP_
#define MEDSLTPATPLAINCONTROLLER_HPP_

#include "MedSLTPatPlainWindow.hpp"
#include "MedSLTPatPlainWindowObserver.hpp"
#include "UiDialogs.hpp"
#include "CommunicationChannel.hpp"
#include "CommunicationChannelObserver.hpp"

class MedSLTPatPlainController : public MedSLTPatPlainWindowObserver, 
										CommunicationChannelObserver
{
public:
	
	MedSLTPatPlainController();
	~MedSLTPatPlainController();
	
	void RunApp();
	static gboolean WindowStateCb(GtkWidget* widget, 
									GdkEventWindowState* event, 
									MedSLTPatPlainController* controller);
	static gboolean KeyPressCb(GtkWidget* widget, GdkEventKey* event,
								MedSLTPatPlainController* controller);
	
	// From MedSLTPatPlainWindowObserver
	// Connect to the MRCP/Regulus remote servers
	void ConnectToRemote();
	// Disconnect from the MRCP/Regulus remote servers
	void DisconnectFromRemote();
	void QuitApp();
	// Send the result to the client
	void SendResultToServer(string& result);
	
	MedSLTPatPlainWindow* GetWindow() const;
	void Resize(gboolean is_full_screen) const;
	
	// Set the ip address of the remote peers
	void SetRemoteServersIP(string ip);
	// Playback the translation result
	void PlayResult(string& result) const;	
			
	// From CommunicationChannelObserver
	void InitCompleted(CCStatus status);
	void StartOfSpeech();
	void RecognitionCompleted(int status, string& result);
	void InterpretationCompleted(int status, string& result);
	void QueryOutputReceived(RegulusDialogueResult*);	
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
	
public:
	
	MedSLTPatPlainWindow*	m_medslt_win;

private:
	
	// Avoid accidental copy or assignment
	MedSLTPatPlainController(const MedSLTPatPlainController&);
	MedSLTPatPlainController& operator = (const MedSLTPatPlainController&);
	
private:		
	
	GtkWindow*				m_window;	
	GtkWidget*				m_progressbar;	
	gboolean				m_recognize_from_tts;
	string					m_rec_result;
	string					m_server;
	bool					m_online;
	
	UiDialogs				m_ui_dlg;
	
	CommunicationChannel*	m_comm_channel;
};

#endif /*MEDSLTPATPLAINCONTROLLER_HPP_*/
