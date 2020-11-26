/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	GUIController.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#ifndef GUICONTROLLER_HPP_
#define GUICONTROLLER_HPP_

#include "GUIWindow.hpp"
#include "GUIWindowObserver.hpp"
#include "UiDialogs.hpp"
#include "CommunicationChannel.hpp"
#include "CommunicationChannelObserver.hpp"

class GUIController : public GUIWindowObserver, 
									CommunicationChannelObserver
{
public:
	
	GUIController();
	~GUIController();
	
	void RunApp();
	static gboolean WindowStateCb(GtkWidget* widget, 
									GdkEventWindowState* event, 
									GUIController* controller);
	static gboolean KeyPressCb(GtkWidget* widget, GdkEventKey* event,
								GUIController* controller);
	
	// From GUIWindowObserver
	// Connect to the MRCP/Regulus remote servers
	void ConnectToRemote();
	// Disconnect from the MRCP/Regulus remote servers
	void DisconnectFromRemote();
	// Recognize using user's speech 
	void StartRecognize();	
	void AbortRecognize();
	void QuitApp();
	
	GUIWindow* GetWindow();
	void Resize(gboolean is_full_screen);
	
	// Recognize using user's speech
	void RecognizeFromUser(const string& grammar, 
							const string& lang, bool online);
	
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
	GUIController(const GUIController&);
	GUIController& operator = (const GUIController&);
	
public:
	
	GUIWindow*	m_gui_win;
	
private:		
	
	GtkWindow*	m_window;
	UiDialogs	m_ui_dlg;
	
	CommunicationChannel* m_comm_channel;
};

#endif /*GUICONTROLLER_HPP_*/
