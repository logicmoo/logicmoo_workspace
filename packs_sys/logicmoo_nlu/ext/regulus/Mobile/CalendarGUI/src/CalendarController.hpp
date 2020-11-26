/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	CalendarController.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#ifndef CALENDARCONTROLLER_HPP_
#define CALENDARCONTROLLER_HPP_

#include <vector>

#include "CalendarWindow.hpp"
#include "CalendarWindowObserver.hpp"
#include "UiDialogs.hpp"
#include "CommunicationChannel.hpp"
#include "CommunicationChannelObserver.hpp"

class CalendarController : public CalendarWindowObserver, 
									CommunicationChannelObserver
{
public:
	
	enum InputLang 
	{
		INPUT_LANG_ENG,
		INPUT_LANG_JAP
	};
	
	CalendarController();
	~CalendarController();
	
	void RunApp();
	static gboolean WindowStateCb(GtkWidget* widget, 
									GdkEventWindowState* event, 
									CalendarController* controller);
	static gboolean KeyPressCb(GtkWidget* widget, GdkEventKey* event,
								CalendarController* controller);
	
	// Recognize using user's speech 
	void StartRecognize();	
	void AbortRecognize();
	// Recognize using user's speech
	void RecognizeFromUser(const string& grammar, 
							const string& lang, bool online);
	// Recognize using speech from the TTS
	void RecognizeFromTTS(const string& input, 
							const string& grammar, const string& lang);	
	// Playback the query result
	void PlayResult();
	void QuitApp();
	CalendarWindow* GetWindow();
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
	gboolean ConfirmDialog(const char* text, const char* title);
	// Connect to the MRCP/Regulus remote servers
	void ConnectToRemote();
	// Disconnect from the MRCP/Regulus remote servers
	void DisconnectFromRemote();
	// Initialize the dialogue
	void InitializeDialogue();
	// Show the history of the paraphrases
	void ShowHistory();
	// Provide a new text input in order to get help sentences
	void RequestFromHelp(gchar* text, bool allowplayback);
	// Set whether or not we have text input
	void SetTextInput(gboolean choice);
	// Get whether or not we have text input
	gboolean IsTextInput();
	// Set whether or not to recognize using the TTS
	void SetRecognizeFromTTS(gboolean choice);
	// Get whether or not to recognize using the TTS
	gboolean IsRecognizeFromTTS();
	// Set the recognition confidence value
	void SetConfidence(int value);	
	// Get the recognition confidence value 
	int GetConfidence();
	// Set the ip address of the remote peers
	void SetRemoteServersIP(string ip);
	// Get the input language
	InputLang GetInputLang();
	// Set the input language
	void SetInputLang(InputLang lang);
	// Handle the output returned from the dialogue server
	void HandleOutput();
	
	const string ExtractQueryResult(string const input);
	const string ExtractWavList(string const input);
	void GetRecResult(string& result);
	void FindAndReplace(string &input_str,
						string const &search_str, 
						string const &replace_str);
	void FormatOutput(string &input_str);
	
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

private:
	
	// Avoid accidental copy or assignment
	CalendarController(const CalendarController&);
	CalendarController& operator = (const CalendarController&);
	
public:
	
	CalendarWindow*	m_calendar_win;
	
private:		
	
	GtkWindow*		m_window;
	gboolean 		m_text_input;
	gboolean 		m_recognize_from_tts;
	string 			m_rec_result;
	string 			m_audio_query_result;
	string 			m_server;
	bool 			m_online;
	InputLang 		m_ilang;
	bool 			m_use_paraphrase;
	string			m_understood;
	string			m_paraphrase;
	string			m_query;
	vector<string> 	m_para_list;
	
	UiDialogs		m_ui_dlg;
	
	CommunicationChannel* m_comm_channel;
};

#endif /*CALENDARCONTROLLER_HPP_*/
