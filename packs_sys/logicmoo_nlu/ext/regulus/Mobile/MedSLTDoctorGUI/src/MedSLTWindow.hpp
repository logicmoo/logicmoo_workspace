/*
 * Copyright 2007-2008 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MedSLTWindow.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@issco.unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#ifndef MEDSLTWINDOW_HPP_
#define MEDSLTWINDOW_HPP_

#include <string>
#include "GlobalDefs.hpp"
#include "MedSLTWindowObserver.hpp"

using namespace std;

class MedSLTWindow
{
public:

	enum UiState
	{
		UI_STATE_CONNECTED,
		UI_STATE_DISCONNECTED,
		UI_STATE_START_RECOGNIZE,
		UI_STATE_ABORT_RECOGNIZE,
		UI_STATE_STOPPED,
		UI_STATE_PLAYING,
		UI_STATE_CONNECTING
	};

	MedSLTWindow(MedSLTWindowObserver& observer);
	~MedSLTWindow();

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
	static void ShowAboutDialog(GtkWidget* widget, gpointer data);
	static void ListRowClicked(GtkWidget* widget, gint row);
	static void ListRowSelected(GtkWidget* widget, gint row, gint column,
									GdkEventButton* event, gpointer data);
	//static void ListRowSelectedHelp(GtkWidget* widget, gint row, gint column,
	//								GdkEventButton* event, gpointer data);

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

	// Set the back translation
	void SetBackTranslation(string& result);
	// Set the translation
	void SetTranslation(string& result);
	// Set the help examples
	void SetHelpExamples(string& result);

	// Recognition completed
	void RecognitionCompleted();

	// Reset the content of the text boxes.
	void ResetTextBoxes();
	void FindAndReplace(string &input_str,
						string const &search_str,
						string const &replace_str);
	void SplitHelpExamples(string& buffer,
							string& result, const string token);

	MedSLTWindowObserver& GetObserver();

	GtkWindow* GetWindow();

private:

	MedSLTWindowObserver& m_observer;

	GtkWindow* 	m_window;
	GtkWidget* 	m_understood;
	GtkWidget* 	m_help;
	GtkWidget* 	m_translation;
	GtkWidget* 	m_rec;
	GtkWidget* 	m_abort;
	GtkWidget* 	m_up;
	GtkWidget* 	m_select;
	GtkWidget* 	m_down;
	GtkWidget* 	m_conmenu;
	GtkWidget* 	m_disconmenu;
	GtkWidget* 	m_progressbar;
	GtkWidget* 	m_list;
	GtkWidget* 	m_txtbox1;
	guint		m_list_index;
	guint		m_list_items;
	gboolean 	m_is_fullscreen;
	string		m_back_translation;
	UiState		m_ui_state;

	GtkWidget* m_love;
	GtkWidget* m_album;
	GtkWidget* m_radiomenu, *m_actionsmenu, *m_settings, *m_dload;
	GtkWidget* m_album_cover;

};

static const char* app_icon_big = V_DATA_DIR "/calendar_48x48.png";
static const char* app_icon = V_DATA_DIR "/calendar.png";
static const char* record_icon = V_DATA_DIR "/record.png";
static const char* stop_icon = V_DATA_DIR "/stop.png";
static const char* up_icon = V_DATA_DIR "/up.png";
static const char* select_icon = V_DATA_DIR "/select.png";
static const char* down_icon = V_DATA_DIR "/down.png";

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

static const char* appdescr = "MedSLT Doctor MRCP/Regulus client for Gnome and Maemo";
static const char* copyright = "(c) 2007 Nikos Tsourakis";
static const char* website = "http://www.issco.unige.ch/staff/tsourakis";
static const char* license =
"MedSLT Doctor MRCP/Regulus client is free software: you can\n"
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

#endif /*MEDSLTWINDOW_HPP_*/
