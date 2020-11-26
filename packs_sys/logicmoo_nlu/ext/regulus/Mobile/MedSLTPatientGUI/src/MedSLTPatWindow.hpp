/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MedSLTPatWindow.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#ifndef MEDSLTPATWINDOW_HPP_
#define MEDSLTPATWINDOW_HPP_

#include <string>
#include "GlobalDefs.hpp"
#include "MedSLTPatWindowObserver.hpp"

using namespace std;

class MedSLTPatWindow
{
public:

	enum UiState 
	{
		UI_STATE_CONNECTED,
		UI_STATE_DISCONNECTED,
		UI_STATE_START_OF_SPEECH,
		UI_STATE_START_RECOGNIZE,
		UI_STATE_ABORT_RECOGNIZE,
		UI_STATE_STOPPED,
		UI_STATE_PLAYING,
		UI_STATE_CONNECTING
	};
	
	MedSLTPatWindow(MedSLTPatWindowObserver& observer);
	~MedSLTPatWindow();
	
	GtkWidget* CreateMenu(GtkAccelGroup *accel);
	
	MedSLTPatWindowObserver& GetObserver() const;	

	GtkWindow* GetWindow() const;
	
	void Resize(gboolean is_full_screen) const;
	void SetUiState(UiState state);
	UiState GetUiState() const;
	gboolean IsFullScreen() const;
	void SetFullScreen(gboolean is_full_screen);
	gchar* GetText() const;
	void IterateList(gboolean forward);
	void SetListIndex(guint index);
	guint GetListIndex() const;
	gchar* GetRowData() const;
	
	// Set the back translation
	void SetBackTranslation(string& result);
	// Set the translation
	void SetTranslation(string& result) const;
	// Set the help examples
	void SetHelpExamples(string& result);
	// Set the doctor's question
	void SetDoctorQuestion(string& result) const;
	
	// Start of speech event
	void StartOfSpeech();
	// Recognition completed
	void RecognitionCompleted();
	
	// Reset the content of the text list.
	void ResetTextList();
	// Reset the content of the first text box.
	void ResetTextBox1() const;
	// Reset the content of the second text box.
	void ResetTextBox2() const;
	// Reset the content of the text boxes.
	void ResetTextBoxes();
		
	// Callbacks
	static gboolean CloseApp(GtkWidget* widget, GdkEvent* event, gpointer data);
	static gboolean DeleteEvent(GtkWidget* widget, gpointer data);
	static void RecognizeClicked(GtkWidget* widget, gpointer data);
	static void AbortClicked(GtkWidget* widget, gpointer data);
	static void UpClicked(GtkWidget* widget, gpointer data);
	static void SelectClicked(GtkWidget* widget, gpointer data);
	static void DownClicked(GtkWidget* widget, gpointer data);
	static void ConnectToRemote(GtkWidget* widget, gpointer data);
	static void DisconnectFromRemote(GtkWidget* widget, gpointer data);
	static void ShowAboutDialog(GtkWidget* widget, gpointer data);
	static void ListRowClicked(GtkWidget* widget, gint row);
	static void ListRowSelected(GtkWidget* widget, gint row, gint column,
									GdkEventButton* event, gpointer data);
	
	// Helper methods
	bool FindAndReplace(string &input_str,
						string const &search_str, 
						string const &replace_str) const;
	void SplitHelpExamples(string& buffer, 
							string& result, const string token) const;
	bool ExtractOutput(string& buffer, 
						const string left, 
						const string right) const;

private:
	
	// Avoid accidental copy or assignment
	MedSLTPatWindow(const MedSLTPatWindow&);
	MedSLTPatWindow& operator = (const MedSLTPatWindow&);
	
private:		
	
	MedSLTPatWindowObserver&	m_observer;
		
	GtkWindow*	m_window;
	GtkWidget*	m_understood;
	GtkWidget*	m_question;
	GtkWidget*	m_help;
	GtkWidget*	m_translation;
	GtkWidget*	m_rec;
	GtkWidget*	m_speech;
	GtkWidget*	m_abort;
	GtkWidget*	m_up;
	GtkWidget*	m_select;
	GtkWidget*	m_down;
	GtkWidget*	m_conmenu;
	GtkWidget*	m_disconmenu;
	GtkWidget*	m_progressbar;
	GtkWidget* 	m_list;
	GtkWidget* 	m_txtbox1;
	GtkWidget* 	m_txtbox2;
	guint		m_list_index;
	guint		m_list_items;
	gboolean 	m_is_fullscreen;
	string		m_back_translation;
	UiState		m_ui_state;
};

static const char* app_icon_big = MEDSLTDOC_DATA_DIR "/calendar_48x48.png";
static const char* app_icon = MEDSLTDOC_DATA_DIR "/calendar.png";
static const char* record_icon = MEDSLTDOC_DATA_DIR "/record.png";
static const char* stop_icon = MEDSLTDOC_DATA_DIR "/stop.png";
static const char* speech_icon = MEDSLTDOC_DATA_DIR "/speech.png";
static const char* up_icon = MEDSLTDOC_DATA_DIR "/up.png";
static const char* select_icon = MEDSLTDOC_DATA_DIR "/select.png";
static const char* down_icon = MEDSLTDOC_DATA_DIR "/down.png";

static const char* authors[] = 
{
	"Nikos Tsourakis\n<Nikolaos.Tsourakis@unige.com>",
	NULL
};
static const char* contributors[] = 
{
	"Nikos Tsourakis\n<Nikolaos.Tsourakis@issco.unige.ch>\n",
	"Manny Rayner\n<Emmanuel.Rayner@issco.unige.ch>\n",	
	NULL
};

static const char* appdescr = "MedSLT Patient MRCP/Regulus client for Gnome and Maemo";
static const char* copyright = "(c) 2007-2009 Nikos Tsourakis";
static const char* website = "http://www.issco.unige.ch/staff/tsourakis";
static const char* license =
"MedSLT Patient MRCP/Regulus client is free software: you can\n"
"redistribute it and/or modify it under the terms of\n"
"the GNU General Public License version 3 as published\n"
"by the Free Software Foundation.\n"
"\n"
"MedSL GUI is distributed in the hope that it will\n"
"be useful, but WITHOUT ANY WARRANTY; without even\n"
"the implied warranty of MERCHANTABILITY or FITNESS\n"
"FOR A PARTICULAR PURPOSE. See the GNU General\n"
"Public License for more details.\n"
"\n"
"You should have received a copy of the GNU General\n"
"Public License along with MedSL GUI. If not, see\n"
"http://www.gnu.org/licenses/.\n";

#endif /*MEDSLTPATWINDOW_HPP_*/
