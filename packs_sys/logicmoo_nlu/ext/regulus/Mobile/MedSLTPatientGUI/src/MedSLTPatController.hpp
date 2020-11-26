/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MedSLTPatController.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#ifndef MEDSLTPATCONTROLLER_HPP_
#define MEDSLTPATCONTROLLER_HPP_

#include "MedSLTPatWindow.hpp"
#include "MedSLTPatWindowObserver.hpp"
#include "UiDialogs.hpp"
#include "CommunicationChannel.hpp"
#include "CommunicationChannelObserver.hpp"

class MedSLTPatController : public MedSLTPatWindowObserver, 
									CommunicationChannelObserver
{
public:
	
	MedSLTPatController();
	~MedSLTPatController();
	
	void RunApp();
	static gboolean WindowStateCb(GtkWidget* widget, 
									GdkEventWindowState* event, 
									MedSLTPatController* controller);
	static gboolean KeyPressCb(GtkWidget* widget, GdkEventKey* event,
								MedSLTPatController* controller);
	
	// From MedSLTPatWindowObserver.hpp
	// Connect to the MRCP/Regulus remote servers
	void ConnectToRemote();
	// Disconnect from the MRCP/Regulus remote servers
	void DisconnectFromRemote();	
	// Recognize using user's speech 
	void StartRecognize();	
	void AbortRecognize();
	// Translate from a help sentence
	void GetTranslationFromHelp(gchar* text);
	void QuitApp();
	
	MedSLTPatWindow* GetWindow() const;
	void Resize(gboolean is_full_screen) const;
	
	// Set the recognition confidence value
	void SetConfidence(int value);	
	// Get the recognition confidence value 
	int GetConfidence() const;
	// Set the ip address of the remote peers
	void SetRemoteServersIP(string ip);
	
	// Recognize using user's speech
	void RecognizeFromUser(const string& grammar, 
							const string& lang, bool online);		
	// Playback the translation result
	void PlayResult(string& result) const;
	// Send the result to the client
	void SendResultToServer(string& result) const;	
		
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
	
	MedSLTPatWindow*	m_medslt_win;

private:
	
	// Avoid accidental copy or assignment
	MedSLTPatController(const MedSLTPatController&);
	MedSLTPatController& operator = (const MedSLTPatController&);
	
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

#endif /*MEDSLTPATCONTROLLER_HPP_*/
