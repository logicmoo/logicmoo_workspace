/*
 * Copyright 2007-2008 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MedSLTController.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@issco.unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#ifndef MEDSLTCONTROLLER_HPP_
#define MEDSLTCONTROLLER_HPP_

#include "MedSLTWindow.hpp"
#include "MedSLTWindowObserver.hpp"
#include "UiDialogs.hpp"
#include "CommunicationChannel.hpp"
#include "CommunicationChannelObserver.hpp"

class MedSLTController : public MedSLTWindowObserver, 
									CommunicationChannelObserver
{
public:
	
	MedSLTController();
	~MedSLTController();
	
	void RunApp();
	static gboolean WindowStateCb(GtkWidget* widget, 
									GdkEventWindowState* event, 
									MedSLTController* controller);
	static gboolean KeyPressCb(GtkWidget* widget, GdkEventKey* event,
								MedSLTController* controller);
	
	// Recognize using user's speech 
	void StartRecognize();	
	void AbortRecognize();
	// Recognize using user's speech
	void RecognizeFromUser(const string& grammar, 
							const string& lang, bool online);		
	// Playback the translation result
	void PlayResult(string& result);
	void QuitApp();
	MedSLTWindow* GetWindow();
	void Resize(gboolean is_full_screen);
	
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
	// Connect to the MRCP/Dialogue remote servers
	void ConnectToRemote();
	// Disconnect from the MRCP/Dialogue remote servers
	void DisconnectFromRemote();
	// Translate from a help sentence
	void GetTranslationFromHelp(gchar* text);
	// Set the recognition confidence value
	void SetConfidence(int value);	
	// Get the recognition confidence value 
	int GetConfidence();
	// Set the ip address of the remote peers
	void SetRemoteServersIP(string ip);
		
	// From CommunicationChannelObserver
	void InitCompleted(CCStatus status);
	void RecognitionCompleted(int status, string& result);
	void InterpretationCompleted(int status, string& result);
	void QueryOutputReceived(string& understood, string& result);	
	void GetFromHelpCompleted(string& understood, string& result);
	void HelpSentencesReceived(string& buffer);
	void BackTranslationReceived(string& result);	
	void TranslationReceived(string& result);
	
public:
	
	MedSLTWindow*	m_medslt_win;
	
private:		
	
	GtkWindow* m_window;	
	GtkWidget* m_progressbar;	
	gboolean m_recognize_from_tts;
	string m_rec_result;
	string m_server;
	bool m_online;
	
	UiDialogs	m_ui_dlg;
	
	CommunicationChannel* m_comm_channel;
};

#endif /*CONTROLLER_HPP_*/
