/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	GUIWindow.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#ifndef GUIWINDOW_HPP_
#define GUIWINDOW_HPP_

#include <string>
#include "GlobalDefs.hpp"
#include "GUIWindowObserver.hpp"

using namespace std;

class GUIWindow
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
	
	GUIWindow(GUIWindowObserver& observer);
	~GUIWindow();
	
	GtkWidget* CreateMenu(GtkAccelGroup *accel);
	
	GUIWindowObserver& GetObserver();	

	GtkWindow* GetWindow();
	
	void Resize(gboolean is_full_screen);
	void SetUiState(UiState state);
	UiState GetUiState();
	gboolean IsFullScreen();
	void SetFullScreen(gboolean is_full_screen);
	
	// Set output information on the GUI
	void SetRecognized(string& result);
	
	// Start of speech event
	void StartOfSpeech();
	// Recognition completed
	void RecognitionCompleted();
	
	// Reset the content of the text boxes.
	void ResetTextBoxes();
	
	// Callbacks
	static gboolean CloseApp(GtkWidget* widget, GdkEvent* event, gpointer data);
	static gboolean DeleteEvent(GtkWidget* widget, gpointer data);
	static void RecognizeClicked(GtkWidget* widget, gpointer data);
	static void AbortClicked(GtkWidget* widget, gpointer data);
	static void ConnectToRemote(GtkWidget* widget, gpointer data);
	static void DisconnectFromRemote(GtkWidget* widget, gpointer data);
	static void ShowAboutDialog(GtkWidget* widget, gpointer data);

private:
	
	// Avoid accidental copy or assignment
	GUIWindow(const GUIWindow&);
	GUIWindow& operator = (const GUIWindow&);
	
private:		
	
	GUIWindowObserver& m_observer;
		
	GtkWindow* 	m_window;
	GtkWidget* 	m_recognized;
	
	GtkWidget* 	m_rec;
	GtkWidget* 	m_speech;
	GtkWidget* 	m_abort;
	
	GtkWidget* 	m_conmenu;
	GtkWidget* 	m_disconmenu;
	GtkWidget* 	m_progressbar;
	GtkWidget* 	m_txtbox;
	
	gboolean 	m_is_fullscreen;
	UiState		m_ui_state;		
};

static const char* app_icon_big = GUI_DATA_DIR "/calendar_48x48.png";
static const char* app_icon = GUI_DATA_DIR "/calendar.png";
static const char* record_icon = GUI_DATA_DIR "/record.png";
static const char* stop_icon = GUI_DATA_DIR "/stop.png";
static const char* speech_icon = GUI_DATA_DIR "/speech.png";

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

static const char* appdescr = "A GUI MRCP/Regulus client for Gnome and Maemo";
static const char* copyright = "(c) 2007 Nikos Tsourakis";
static const char* website = "http://www.issco.unige.ch/staff/tsourakis";
static const char* license =
"GUI MRCP/Regulus client is free software: you can\n"
"redistribute it and/or modify it under the terms of\n"
"the GNU General Public License version 3 as published\n"
"by the Free Software Foundation.\n"
"\n"
"GUI is distributed in the hope that it will\n"
"be useful, but WITHOUT ANY WARRANTY; without even\n"
"the implied warranty of MERCHANTABILITY or FITNESS\n"
"FOR A PARTICULAR PURPOSE. See the GNU General\n"
"Public License for more details.\n"
"\n"
"You should have received a copy of the GNU General\n"
"Public License along with GUI. If not, see\n"
"http://www.gnu.org/licenses/.\n";

#endif /*GUIWINDOW_HPP_*/
