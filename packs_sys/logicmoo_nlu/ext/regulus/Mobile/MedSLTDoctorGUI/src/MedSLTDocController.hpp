/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MedSLTDocController.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#ifndef MEDSLTDOCCONTROLLER_HPP_
#define MEDSLTDOCCONTROLLER_HPP_

#include "MedSLTDocWindow.hpp"
#include "MedSLTDocWindowObserver.hpp"
#include "UiDialogs.hpp"
#include "CommunicationChannel.hpp"
#include "CommunicationChannelObserver.hpp"

class MedSLTDocController : public MedSLTDocWindowObserver, 
									CommunicationChannelObserver
{
public:
	
	MedSLTDocController();
	~MedSLTDocController();
	
	void RunApp();
	static gboolean WindowStateCb(GtkWidget* widget, 
									GdkEventWindowState* event, 
									MedSLTDocController* controller);
	static gboolean KeyPressCb(GtkWidget* widget, GdkEventKey* event,
								MedSLTDocController* controller);
	
	// From MedSLTDocWindowObserver
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

	MedSLTDocWindow* GetWindow();
	void Resize(gboolean is_full_screen);		
	void ZoomIO(gboolean zoom) const;
	
	// Set the recognition confidence value
	void SetConfidence(int value);	
	// Get the recognition confidence value 
	int GetConfidence();
	// Set the ip address of the remote peers
	void SetRemoteServersIP(string ip);
	
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
	// Send the result to the client
	void SendResultToClient(string& result);

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
	MedSLTDocController(const MedSLTDocController&);
	MedSLTDocController& operator = (const MedSLTDocController&);
	
public:
	
	MedSLTDocWindow*		m_medslt_win;
	
private:		
	
	GtkWindow*				m_window;	
	gboolean				m_rec_in_progress;
	string					m_rec_result;
	string					m_server;
	gboolean				m_connected;
	bool					m_online;
	vector<string> 			m_dlg_list;
	
	UiDialogs				m_ui_dlg;
	
	CommunicationChannel*	m_comm_channel;
};

#endif /*MEDSLTDOCCONTROLLER_HPP_*/
