/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MedSLTPatPlainWindow.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#ifndef MEDSLTPATPLAINWINDOW_HPP_
#define MEDSLTPATPLAINWINDOW_HPP_

#include <string>
#include "GlobalDefs.hpp"
#include "MedSLTPatPlainWindowObserver.hpp"

using namespace std;

class MedSLTPatPlainWindow
{
public:

	enum UiState 
	{
		UI_STATE_CONNECTED,
		UI_STATE_DISCONNECTED,		
		UI_STATE_CONNECTING
	};
	
	MedSLTPatPlainWindow(MedSLTPatPlainWindowObserver& observer);
	~MedSLTPatPlainWindow();
	
	GtkWidget* CreateMenu(GtkAccelGroup *accel);
	
	GtkWindow* GetWindow() const;
	
	MedSLTPatPlainWindowObserver& GetObserver() const;	
	
	void Resize(gboolean is_full_screen) const;
	void SetUiState(UiState state);
	UiState GetUiState() const;
	gboolean IsFullScreen() const;
	void SetFullScreen(gboolean is_full_screen);
	// Set the doctor's question
	void SetDoctorQuestion(string& result);
	void ResetTextBox() const;
		
	// Callbacks
	static gboolean CloseApp(GtkWidget* widget, GdkEvent* event, gpointer data);
	static gboolean DeleteEvent(GtkWidget* widget, gpointer data);
	static void YesClicked(GtkWidget* widget, gpointer data);
	static void NoClicked(GtkWidget* widget, gpointer data);
	static void DontKnowClicked(GtkWidget* widget, gpointer data);
	static void FrequentlyClicked(GtkWidget* widget, gpointer data);
	static void RarelyClicked(GtkWidget* widget, gpointer data);
	static void RepeatClicked(GtkWidget* widget, gpointer data);
	static void ConnectToRemote(GtkWidget* widget, gpointer data);
	static void DisconnectFromRemote(GtkWidget* widget, gpointer data);
	static void ShowAboutDialog(GtkWidget* widget, gpointer data);

	//  Helper methods
	void WrapText(string &str);
	bool FindAndReplace(string &input_str,
						string const &search_str, 
						string const &replace_str) const;
	bool ExtractOutput(string& buffer, 
						const string left, 
						const string right) const;

private:
	
	// Avoid accidental copy or assignment
	MedSLTPatPlainWindow(const MedSLTPatPlainWindow&);		
	MedSLTPatPlainWindow& operator = (const MedSLTPatPlainWindow&);		
	
private:		
	
	MedSLTPatPlainWindowObserver&	m_observer;
		
	GtkWindow*	m_window;
	GtkWidget*	m_yes;
	GtkWidget*	m_no;
	GtkWidget*	m_dont_know;
	GtkWidget*	m_frequently;
	GtkWidget*	m_rarely;
	GtkWidget*	m_repeat;
	GtkWidget*	m_conmenu;
	GtkWidget*	m_disconmenu;
	GtkWidget*	m_txtbox;
	gboolean	m_is_fullscreen;
	UiState		m_ui_state;

};

static const char* app_icon_big = MEDSLTDOC_DATA_DIR "/calendar_48x48.png";
static const char* app_icon = MEDSLTDOC_DATA_DIR "/calendar.png";

static const char* authors[] = 
{
	"Nikos Tsourakis\n<Nikolaos.Tsourakis@unige.com>",
	NULL
};
static const char* contributors[] = 
{
	"Nikos Tsourakis\n<Nikolaos.Tsourakis@unige.ch>\n",
	"Manny Rayner\n<Emmanuel.Rayner@unige.ch>\n",	
	NULL
};

static const char* appdescr = "MedSLT Patient Plain MRCP/Regulus client for Gnome and Maemo";
static const char* copyright = "(c) 2007-2009 Nikos Tsourakis";
static const char* website = "http://www.issco.unige.ch/staff/tsourakis";
static const char* license =
"MedSLT Patient Plain MRCP/Regulus client is free software: you can\n"
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

#endif /*MEDSLTPATPLAINWINDOW_HPP_*/
