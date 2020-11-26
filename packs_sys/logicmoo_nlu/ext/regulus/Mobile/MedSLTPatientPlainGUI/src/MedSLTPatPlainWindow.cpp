/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MedSLTPatPlainWindow.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <stdio.h>

//#if defined(MAEMO2) || defined(MAEMO3)
//#include <hildon-widgets/hildon-program.h>
//#elif defined(MAEMO4)
#include <hildon/hildon-program.h>
//#endif

#include "MedSLTPatPlainWindow.hpp"
#include "ace/Log_Msg.h"

MedSLTPatPlainWindow::MedSLTPatPlainWindow(MedSLTPatPlainWindowObserver& observer)
	: 	m_observer(observer),
		m_is_fullscreen(FALSE),
		m_ui_state(UI_STATE_DISCONNECTED)		
{
	ACE_TRACE("[MedSLTPatPlainWindow::MedSLTPatPlainWindow()]");

	GtkBox* 				coverbox;
	GtkBox* 				textbox;
	GtkBox* 				buthbox;
	GtkBox* 				butvbox1;
	GtkBox* 				butvbox2;
	GtkBox* 				butvbox3;
	GtkWidget* 				menu;
	GtkWidget* 				appimage;	
	GtkWidget* 				scroll;
	PangoFontDescription*	font1;
	PangoFontDescription*	font2;
	GtkStyle*				style;
	GtkAccelGroup 			*accel = gtk_accel_group_new();

	// Window
#ifdef _MAEMO
	m_window = GTK_WINDOW(hildon_window_new());	
#else
	m_window = GTK_WINDOW(gtk_window_new(GTK_WINDOW_TOPLEVEL));
	gtk_window_set_default_size(m_window, 450, -1);
#endif
	
	gtk_window_add_accel_group(m_window, accel);	
	gtk_window_set_icon_from_file(m_window, app_icon, NULL);	
	gtk_container_set_border_width(GTK_CONTAINER(m_window), 5);
	
	// Boxes
	coverbox = GTK_BOX(gtk_vbox_new(False, 5));
	textbox = GTK_BOX(gtk_hbox_new(False, 5));
	buthbox = GTK_BOX(gtk_hbox_new(False, 5));
	butvbox1 = GTK_BOX(gtk_vbox_new(False, 5));
	butvbox2 = GTK_BOX(gtk_vbox_new(False, 5));
	butvbox3 = GTK_BOX(gtk_vbox_new(False, 5));
		
	// Image
	appimage = gtk_image_new_from_file(app_icon_big);
	
	// Fonts
	font1 = pango_font_description_from_string("Monospace Bold 40");
	font2 = pango_font_description_from_string("Monospace Bold 30");
	
	// Styles
	style = gtk_style_new();
	style->font_desc = font1;
	gdk_color_parse ("red", &(style->fg[GTK_STATE_NORMAL]));
	
	// Text Boxes
	m_txtbox = gtk_text_view_new();
			
	gtk_widget_modify_font(m_txtbox, font1);
		
	gtk_widget_set_size_request(GTK_WIDGET(buthbox), 550, 250);
		
	gtk_text_view_set_editable(GTK_TEXT_VIEW(m_txtbox), false);
		
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(m_txtbox), GTK_WRAP_WORD_CHAR);
	
	gtk_widget_set_style(m_txtbox, style);
		
	// Scroll bars
	scroll = gtk_scrolled_window_new(NULL, NULL);
	
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll), GTK_POLICY_ALWAYS, GTK_POLICY_ALWAYS);
	
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scroll), GTK_SHADOW_IN);
	
	gtk_container_add(GTK_CONTAINER(scroll), GTK_WIDGET(m_txtbox));
	
	// Buttons	
	m_yes = gtk_button_new();
	m_no = gtk_button_new();
	m_dont_know = gtk_button_new();
	m_frequently = gtk_button_new();
	m_rarely = gtk_button_new();
	m_repeat = gtk_button_new();
	
	gtk_button_set_label(GTK_BUTTON(m_yes), "QUI");
	gtk_button_set_label(GTK_BUTTON(m_no), "NON");
	gtk_button_set_label(GTK_BUTTON(m_dont_know), "JE NE SAIS\n   PAS");
	gtk_button_set_label(GTK_BUTTON(m_frequently), "FREQUEMMENT");
	gtk_button_set_label(GTK_BUTTON(m_rarely), "RAREMENT");
	gtk_button_set_label(GTK_BUTTON(m_repeat), "  REPETEZ \nLA QUESTION");
	
	gtk_widget_modify_font(GTK_BIN (m_yes)->child, font2);
	gtk_widget_modify_font(GTK_BIN (m_no)->child, font2);
	gtk_widget_modify_font(GTK_BIN (m_dont_know)->child, font2);
	gtk_widget_modify_font(GTK_BIN (m_frequently)->child, font2);
	gtk_widget_modify_font(GTK_BIN (m_rarely)->child, font2);
	gtk_widget_modify_font(GTK_BIN (m_repeat)->child, font2);
		
	// Menu
	// Recheck: Is it destroyed somewhere
	menu = CreateMenu(accel);
	
	gtk_box_pack_start(textbox, scroll, TRUE, TRUE, 0);
	
	gtk_box_pack_start(butvbox1, m_yes, TRUE, TRUE, 0);
	gtk_box_pack_start(butvbox1, m_frequently, TRUE, TRUE, 0);
	
	gtk_box_pack_start(butvbox2, m_no, TRUE, TRUE, 0);
	gtk_box_pack_start(butvbox2, m_rarely, TRUE, TRUE, 0);
	
	gtk_box_pack_start(butvbox3, m_dont_know, TRUE, TRUE, 0);
	gtk_box_pack_start(butvbox3, m_repeat, TRUE, TRUE, 0);
	
	gtk_box_pack_start(buthbox, GTK_WIDGET(butvbox1), TRUE, TRUE, 0);
	gtk_box_pack_start(buthbox, GTK_WIDGET(butvbox2), TRUE, TRUE, 0);
	gtk_box_pack_start(buthbox, GTK_WIDGET(butvbox3), TRUE, TRUE, 0);
	
	gtk_box_pack_start(coverbox, GTK_WIDGET(textbox), TRUE, TRUE, 0);
	gtk_box_pack_start(coverbox, GTK_WIDGET(buthbox), TRUE, TRUE, 0);
			
	gtk_container_add(GTK_CONTAINER(m_window), GTK_WIDGET(coverbox));
	
#ifdef _MAEMO
	hildon_window_set_menu(HILDON_WINDOW(m_window), GTK_MENU(menu));	
#else
	gtk_box_pack_start(vbox, menu, FALSE, FALSE, 0);
#endif

	// Signals
	g_signal_connect(G_OBJECT(m_yes), "clicked", 
						G_CALLBACK(YesClicked), this);
	g_signal_connect(G_OBJECT(m_no), "clicked",
						G_CALLBACK(NoClicked), this);
	g_signal_connect(G_OBJECT(m_dont_know), "clicked",
						G_CALLBACK(DontKnowClicked), this);
	g_signal_connect(G_OBJECT(m_frequently), "clicked",
						G_CALLBACK(FrequentlyClicked), this);
	g_signal_connect(G_OBJECT(m_rarely), "clicked",
						G_CALLBACK(RarelyClicked), this);
	g_signal_connect(G_OBJECT(m_repeat), "clicked",
						G_CALLBACK(RepeatClicked), this);
	g_signal_connect(G_OBJECT(m_window), "destroy",
						G_CALLBACK(CloseApp), this);
	g_signal_connect(G_OBJECT(m_window), "delete_event",
						G_CALLBACK(CloseApp), this);

	// Shortcuts
#ifndef _MAEMO
	gtk_widget_add_accelerator(m_yes, "clicked", accel, GDK_space,
								0, 0);
	gtk_widget_add_accelerator(m_no, "clicked", accel, GDK_space,
									0, 0);
	gtk_widget_add_accelerator(m_dont_know, "clicked", accel, GDK_space,
								0, 0);
	gtk_widget_add_accelerator(m_next, "clicked", accel, GDK_Right,
								GDK_CONTROL_MASK, 0);
#endif

	// Initial state
	SetUiState(UI_STATE_DISCONNECTED);
	
	return;
}

MedSLTPatPlainWindow::~MedSLTPatPlainWindow()
{
	ACE_TRACE("[MedSLTPatPlainWindow::~MedSLTPatPlainWindow()]");
			
	return;
}    

GtkWidget* MedSLTPatPlainWindow::CreateMenu(GtkAccelGroup* accel)
{
	ACE_TRACE("[MedSLTPatPlainWindow::CreateMenu()]");

	GtkMenuItem*			connection;
	GtkMenuShell*			connectionsub;
	GtkMenuItem*			help;
	GtkMenuShell*			helpsub;
	GtkWidget*				connect;
	GtkWidget*				disconnect;
	GtkWidget*				quit;
	GtkWidget*				about;
	PangoFontDescription*	font;

#ifdef _MAEMO
	GtkMenuShell* bar = GTK_MENU_SHELL(gtk_menu_new());
#else
	GtkMenuShell* bar = GTK_MENU_SHELL(gtk_menu_bar_new());
#endif

	font = pango_font_description_from_string("Monospace Bold 16");
	
	// Connections
	connection = GTK_MENU_ITEM(gtk_menu_item_new_with_mnemonic("_Connections"));
	connectionsub = GTK_MENU_SHELL(gtk_menu_new());
	connect = gtk_menu_item_new_with_label("Connect to remote...");
	disconnect = gtk_menu_item_new_with_label("Disconnect from remote...");	
	gtk_menu_shell_append(bar, GTK_WIDGET(connection));
	gtk_menu_item_set_submenu(connection, GTK_WIDGET(connectionsub));
	gtk_menu_shell_append(connectionsub, connect);
	gtk_menu_shell_append(connectionsub, disconnect);
	g_signal_connect(G_OBJECT(connect), "activate",
						G_CALLBACK(ConnectToRemote), this);
	g_signal_connect(G_OBJECT(disconnect), "activate",
						G_CALLBACK(DisconnectFromRemote), this);
											
	quit = gtk_menu_item_new_with_label("Quit");
	gtk_widget_add_accelerator(quit, "activate", accel, GDK_q,
								GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE);
	
	gtk_widget_modify_font(gtk_bin_get_child(GTK_BIN(connection)), font);
	gtk_widget_modify_font(gtk_bin_get_child(GTK_BIN(connect)), font);
	gtk_widget_modify_font(gtk_bin_get_child(GTK_BIN(disconnect)), font);
	gtk_widget_modify_font(gtk_bin_get_child(GTK_BIN(quit)), font);	
	
#ifndef _MAEMO
	gtk_menu_shell_append(connectionsub, quit);
#endif

	g_signal_connect(G_OBJECT(quit), "activate",
						G_CALLBACK(DeleteEvent), this);

	// Help
	help = GTK_MENU_ITEM(gtk_menu_item_new_with_mnemonic("_Help"));
	helpsub = GTK_MENU_SHELL(gtk_menu_new());
	about = gtk_menu_item_new_with_label("About...");
	gtk_menu_shell_append(bar, GTK_WIDGET(help));
	gtk_menu_item_set_submenu(help, GTK_WIDGET(helpsub));
	gtk_menu_shell_append(helpsub, about);
	g_signal_connect(G_OBJECT(about), "activate",
						G_CALLBACK(ShowAboutDialog), m_window);	
	gtk_widget_modify_font(gtk_bin_get_child(GTK_BIN(help)), font);	
	gtk_widget_modify_font(gtk_bin_get_child(GTK_BIN(about)), font);
	
#ifdef _MAEMO
	gtk_menu_shell_append(bar, quit);
#endif
	
	m_conmenu = GTK_WIDGET(connect);
	m_disconmenu = GTK_WIDGET(disconnect);
	
	return GTK_WIDGET(bar);
}

GtkWindow* MedSLTPatPlainWindow::GetWindow() const
{
	ACE_TRACE("[MedSLTPatPlainWindow::GetWindow()]");

	if (m_window != NULL)
	{	
		return m_window;
	}
	else
	{	
		return NULL;
	}
}

MedSLTPatPlainWindowObserver& MedSLTPatPlainWindow::GetObserver() const
{
	ACE_TRACE("[MedSLTPatPlainWindow::GetObserver()]");

	return m_observer;
}

void MedSLTPatPlainWindow::Resize(gboolean is_full_screen) const
{
	ACE_TRACE("[MedSLTPatPlainWindow::Resize()]");
	
	if (is_full_screen == true)
	{ 	
		gtk_widget_set_size_request(GTK_WIDGET(m_txtbox), 550, 30);		
	}
	else
	{		
		gtk_widget_set_size_request(GTK_WIDGET(m_txtbox), 600, 40);
	}
	
	return;
}

void MedSLTPatPlainWindow::SetUiState(UiState state)
{
	ACE_TRACE("[MedSLTPatPlainWindow::SetUiState()]");
	
	switch (state) 
	{
		case UI_STATE_DISCONNECTED:		
			gtk_widget_show(m_yes);
			gtk_widget_show(m_no);
			gtk_widget_show(m_dont_know);
			gtk_widget_show(m_frequently);
			gtk_widget_show(m_rarely);
			gtk_widget_show(m_repeat);
			gtk_widget_set_sensitive(m_yes, TRUE);
			gtk_widget_set_sensitive(m_no, TRUE);
			gtk_widget_set_sensitive(m_dont_know, TRUE);
			gtk_widget_set_sensitive(m_frequently, TRUE);			
			gtk_widget_set_sensitive(m_rarely, TRUE);
			gtk_widget_set_sensitive(m_repeat, TRUE);
			gtk_widget_set_sensitive(m_conmenu, TRUE);
			gtk_widget_set_sensitive(m_disconmenu, FALSE);		
			m_ui_state = UI_STATE_DISCONNECTED;
			break;
		case UI_STATE_CONNECTING:
			m_ui_state = UI_STATE_CONNECTING;
			break;
		case UI_STATE_CONNECTED:
			gtk_widget_set_sensitive(m_yes, TRUE);										
			gtk_widget_set_sensitive(m_frequently, TRUE);			
			gtk_widget_set_sensitive(m_rarely, TRUE);
			gtk_widget_set_sensitive(m_repeat, TRUE);
			gtk_widget_set_sensitive(m_conmenu, FALSE);
			gtk_widget_set_sensitive(m_disconmenu, TRUE);
			m_ui_state = UI_STATE_CONNECTED;
			break;
		default:
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTPatPlainWindow::SetUiState()] "
						"[Unknown ui state received: %d]\n"), state));
			break;
	}

	return;
}

MedSLTPatPlainWindow::UiState MedSLTPatPlainWindow::GetUiState() const
{
	ACE_TRACE("[MedSLTPatPlainWindow::GetUiState()]");
	
	return m_ui_state;
}

gboolean MedSLTPatPlainWindow::IsFullScreen() const
{
	ACE_TRACE("[MedSLTPatPlainWindow::IsFullScreen()]");

	return m_is_fullscreen;
}

void MedSLTPatPlainWindow::SetFullScreen(gboolean is_full_screen)
{
	ACE_TRACE("[MedSLTPatPlainWindow::SetFullScreen()]");

	m_is_fullscreen = is_full_screen;
	
	return;
}

void MedSLTPatPlainWindow::SetDoctorQuestion(string& result)
{
	ACE_TRACE("[MedSLTPatPlainWindow::SetDoctorQuestion()]");
	
	GError*	error = NULL;
	gchar*	utf8 = NULL;
	
	WrapText(result);
	
	// Get GTK thread lock
	gdk_threads_enter();
	
	GtkTextBuffer* buf;
			
	buf =  gtk_text_view_get_buffer(GTK_TEXT_VIEW(m_txtbox));
		
	utf8 = g_convert(result.c_str(), -1, "UTF-8", "ISO-8859-1", NULL, NULL, &error);
	
	if (utf8 == NULL)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] "
					"[MedSLTPatPlainWindow::SetDoctorQuestion()] "
					"[Failed: %s]\n"), error->message));		
	}
	else
	{
		gtk_text_buffer_set_text(buf, utf8, -1);
	}
	
	g_free(utf8);
	g_free(error);
	
	gdk_flush();
	
	// Release GTK thread lock
	gdk_threads_leave();
	
	return; 
}

void MedSLTPatPlainWindow::ResetTextBox() const
{
	ACE_TRACE("[MedSLTPatPlainWindow::ResetTextBox()]");
	
	GtkTextBuffer* buf;
	
	buf =  gtk_text_view_get_buffer(GTK_TEXT_VIEW(m_txtbox));
	gtk_text_buffer_set_text(buf, "", -1);
		
	return;
}
gboolean MedSLTPatPlainWindow::CloseApp(GtkWidget* widget, 
										GdkEvent* event, 
										gpointer data)
{
	ACE_TRACE("[MedSLTPatPlainWindow::CloseApp()]");
	
	MedSLTPatPlainWindow* win = (MedSLTPatPlainWindow*)data;
	
	if (win != NULL)
	{	
		(win->GetObserver()).QuitApp();
	}
	
	return TRUE;
}

gboolean MedSLTPatPlainWindow::DeleteEvent(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTPatPlainWindow::DeleteEvent()]");

	MedSLTPatPlainWindow* win = (MedSLTPatPlainWindow*)data;
	
	if (win != NULL)
	{
		(win->GetObserver()).QuitApp();
	}
	
	return TRUE;
}

void MedSLTPatPlainWindow::YesClicked(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTPatPlainWindow::YesClicked()]");

	MedSLTPatPlainWindow* win = (MedSLTPatPlainWindow*)data;
	
	if (win != NULL)
	{
		string response("Yes");
		(win->GetObserver()).SendResultToServer(response);
	}

	return;
}

void MedSLTPatPlainWindow::NoClicked(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTPatPlainWindow::NoClicked()]");

	MedSLTPatPlainWindow* win = (MedSLTPatPlainWindow*)data;
	
	if (win != NULL)
	{
		string response("No");
		(win->GetObserver()).SendResultToServer(response);
	}

	return;
}

void MedSLTPatPlainWindow::DontKnowClicked(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTPatPlainWindow::DontKnowClicked()]");
	
	MedSLTPatPlainWindow* win = (MedSLTPatPlainWindow*)data;
	
	if (win != NULL)
	{
		string response("I don't know");
		(win->GetObserver()).SendResultToServer(response);
	}
	
	return;
}

void MedSLTPatPlainWindow::FrequentlyClicked(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTPatPlainWindow::FrequentlyClicked()]");
	
	MedSLTPatPlainWindow* win = (MedSLTPatPlainWindow*)data;
	
	if (win != NULL)
	{
		string response("Frequently");
		(win->GetObserver()).SendResultToServer(response);
	}
	
	return;
}

void MedSLTPatPlainWindow::RarelyClicked(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTPatPlainWindow::RarelyClicked()]");
	
	MedSLTPatPlainWindow* win = (MedSLTPatPlainWindow*)data;
	
	if (win != NULL)
	{
		string response("Rarely");
		(win->GetObserver()).SendResultToServer(response);
	}
	
	return;
}

void MedSLTPatPlainWindow::RepeatClicked(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTPatPlainWindow::RepeatClicked()]");
	
	MedSLTPatPlainWindow* win = (MedSLTPatPlainWindow*)data;
	
	if (win != NULL)
	{
		string response("Repeat");
		(win->GetObserver()).SendResultToServer(response);
	}
	
	return;
}

void MedSLTPatPlainWindow::ConnectToRemote(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTPatPlainWindow::ConnectToRemote()]");
	
	MedSLTPatPlainWindow* win = (MedSLTPatPlainWindow*)data;
		
	if (win != NULL)
	{	
		(win->GetObserver()).ConnectToRemote();	
		//win->SetUiState(UI_STATE_CONNECTED);
	}	
	
	return;
}

void MedSLTPatPlainWindow::DisconnectFromRemote(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTPatPlainWindow::DisconnectFromRemote()]");
	
	MedSLTPatPlainWindow* win = (MedSLTPatPlainWindow*)data;
	
	if (win != NULL)
	{
		(win->GetObserver()).DisconnectFromRemote();
		win->SetUiState(UI_STATE_DISCONNECTED);		
	}

	return;
}

void MedSLTPatPlainWindow::ShowAboutDialog(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTPatPlainWindow::ShowAboutDialog()]");

	GtkWindow *win = GTK_WINDOW(data);
	GdkPixbuf *logo = gdk_pixbuf_new_from_file(app_icon, NULL);
	gtk_show_about_dialog(win, "name", APP_NAME, "authors", authors,
							"comments", appdescr, "copyright", copyright,
							"license", license, "version", APP_VERSION,
							"website", website, "documenters", contributors,
							"logo", logo, NULL);
	
	g_object_unref(G_OBJECT(logo));
	
	return;
}

void MedSLTPatPlainWindow::WrapText(string &str)
{
	ACE_TRACE("[MedSLTPatPlainWindow::WrapText()]");

	string::size_type	pos = 0;
	int 				count = 0;
	
	while ((pos = str.find(" ", pos)) != string::npos)
	{	
		++count;

		if ((count % 5) == 0)
		{
			str.replace(pos, 1, "\n");
			pos = pos + 2;
			
			break;
		}
		
		++pos;
	}
	
	return;
}

bool MedSLTPatPlainWindow::FindAndReplace(string &input_str,
									 		string const &search_str, 
									 		string const &replace_str) const
{
	string::size_type	pos = 0;
	bool				found = false;
	
	while ((pos = input_str.find(search_str, pos)) != string::npos)
	{
		input_str.replace(pos, search_str.size(), replace_str);
		pos = pos + replace_str.length();
		
		found = true;
	}
	
	return found;
}

bool MedSLTPatPlainWindow::ExtractOutput(string& buffer, 
										 const string left, 
										 const string right) const
{
	ACE_TRACE(ACE_TEXT("[MedSLTPatPlainWindow::ExtractOutput()]"));
		
	string::size_type	pos = 0;
	string				replace_str("");
	char				rm_buff[1000];
	
	pos = buffer.find(left, pos);
	
	if (pos	== string::npos)
	{
		return false;
	}
	
	pos = pos + left.size();

	if (pos >= 1000)
 	{
 		pos = 1000 - 1;
 	}
	
	int length = buffer.copy(rm_buff, pos, 0);
	
	if (length >= 1000)
 	{
 		length = 1000 - 1;
 	}
 	 	
 	rm_buff[length] = '\0';
 	 	
 	if (FindAndReplace(buffer, rm_buff, replace_str) == false)
 	{
 		return false;
 	}
 	
 	pos = buffer.find(right, 0);
	
	if (pos	== string::npos)
	{
		return false;
	}
	
	if (pos >= 1000)
 	{
 		pos = 1000 - 1;
 	}
	
	length = buffer.copy(rm_buff, pos, 0);
	
	if (length >= 1000)
 	{
 		length = 1000 - 1;
 	}
 	
 	rm_buff[length] = '\0';
 	
	if (FindAndReplace(buffer, rm_buff, replace_str) == false)
 	{
 		return false;
 	}
 	
	buffer = rm_buff;
		
#if _DEBUG
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTPatPlainWindow::ExtractOutput()] "
			"[Ouput: %s]\n"), buffer.c_str()));				
#endif
	
	return true;
}
