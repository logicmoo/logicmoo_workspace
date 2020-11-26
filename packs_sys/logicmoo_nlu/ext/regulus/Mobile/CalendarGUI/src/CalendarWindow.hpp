/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	CalendarWindow.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#ifndef CALENDARWINDOW_HPP_
#define CALENDARWINDOW_HPP_

#include <string>
#include "GlobalDefs.hpp"
#include "CalendarWindowObserver.hpp"

using namespace std;

class CalendarWindow
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
	
	CalendarWindow(CalendarWindowObserver& observer);
	~CalendarWindow();
	
	GtkWidget* CreateMenu(GtkAccelGroup *accel);
	
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
	static void InitializeDialogue(GtkWidget* widget, gpointer data);
	static void ShowHistory(GtkWidget* widget, gpointer data);
	static void ShowAboutDialog(GtkWidget* widget, gpointer data);
	static void ListRowClicked(GtkWidget* widget, gint row);
	static void ListRowSelected(GtkWidget* widget, gint row, gint column,
									GdkEventButton* event, gpointer data);
	
	void Resize(gboolean is_full_screen);
	void SetUiState(UiState state);
	UiState GetUiState();
	gboolean IsFullScreen();
	void SetFullScreen(gboolean is_full_screen);
	void SetAppImage(const guchar *data, int size);
	gchar* GetText();
	void IterateList(gboolean forward);
	void SetListIndex(guint index);
	guint GetListIndex();
	gchar* GetRowData();
	
	// Set output information on the GUI
	void SetUnderstood(string& result);
	void SetQueryResult(string& result);
	void SetHelpExamples(string& result);
	
	// Start of speech event
	void StartOfSpeech();
	// Recognition started
	void RecognitionStarted();
	// Recognition completed
	void RecognitionCompleted();
	
	// Show a busy cursor
	void ShowBusyCursor(bool choice);
	
	// Reset the content of the text boxes.
	void ResetTextBoxes();
	void FindAndReplaceFirst(string &input_str,
							 string const &search_str, 
							 string const &replace_str);
	void FindAndReplace(string &input_str,
						string const &search_str, 
						string const &replace_str);
	void SplitHelpExamples(string& buffer, 
							string& result, const string token);
														
	CalendarWindowObserver& GetObserver();	

	GtkWindow* GetWindow();

private:
	
	// Avoid accidental copy or assignment
	CalendarWindow(const CalendarWindow&);
	CalendarWindow& operator = (const CalendarWindow&);
	
private:		
	
	CalendarWindowObserver& m_observer;
		
	GtkWindow* 	m_window;
	GtkWidget* 	m_understood;
	GtkWidget* 	m_result;
	GtkWidget* 	m_help;
	GtkWidget* 	m_rec;
	GtkWidget* 	m_speech;
	GtkWidget* 	m_abort;
	GtkWidget* 	m_up;
	GtkWidget* 	m_select;
	GtkWidget* 	m_down;
	GtkWidget* 	m_conmenu;
	GtkWidget* 	m_disconmenu;
	GtkWidget* 	m_toolsmenu;
	GtkWidget* 	m_progressbar;
	GtkWidget* 	m_txtbox1;
	GtkWidget* 	m_txtbox2;	
	GtkWidget* 	m_list;
	guint		m_list_index;
	guint		m_list_items;
	gboolean 	m_is_fullscreen;
	UiState		m_ui_state;
	
	GtkWidget* 	m_cover;
};

static const char* app_icon_big = CALENDAR_DATA_DIR "/calendar_48x48.png";
static const char* app_icon = CALENDAR_DATA_DIR "/calendar.png";
static const char* record_icon = CALENDAR_DATA_DIR "/record.png";
static const char* stop_icon = CALENDAR_DATA_DIR "/stop.png";
static const char* speech_icon = CALENDAR_DATA_DIR "/speech.png";
static const char* up_icon = CALENDAR_DATA_DIR "/up.png";
static const char* select_icon = CALENDAR_DATA_DIR "/select.png";
static const char* down_icon = CALENDAR_DATA_DIR "/down.png";

static const char* authors[] = 
{
	"Nikos Tsourakis\n<Nikolaos.Tsourakis@issco.unige.com>",
	NULL
};
static const char* contributors[] = 
{
	"Nikos Tsourakis\n<Nikolaos.Tsourakis@issco.unige.ch>\n",
	"Manny Rayner\n<Emmanuel.Rayner@issco.unige.ch>\n",	
	NULL
};

static const char* appdescr = "A Calendar MRCP/Regulus client for Gnome and Maemo";
static const char* copyright = "(c) 2007 Nikos Tsourakis";
static const char* website = "http://www.issco.unige.ch/staff/tsourakis";
static const char* license =
"Calendar MRCP/Regulus client is free software: you can\n"
"redistribute it and/or modify it under the terms of\n"
"the GNU General Public License version 3 as published\n"
"by the Free Software Foundation.\n"
"\n"
"Calendar is distributed in the hope that it will\n"
"be useful, but WITHOUT ANY WARRANTY; without even\n"
"the implied warranty of MERCHANTABILITY or FITNESS\n"
"FOR A PARTICULAR PURPOSE. See the GNU General\n"
"Public License for more details.\n"
"\n"
"You should have received a copy of the GNU General\n"
"Public License along with Calendar. If not, see\n"
"http://www.gnu.org/licenses/.\n";

#endif /*CALENDARWINDOW_HPP_*/
