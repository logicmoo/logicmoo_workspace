/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	GUIWindow.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <stdio.h>
#include "ace/Log_Msg.h"

//#if defined(MAEMO2) || defined(MAEMO3)
//#include <hildon-widgets/hildon-program.h>
//#elif defined(MAEMO4)
#include <hildon/hildon-program.h>
//#endif

#include "GUIWindow.hpp"

GUIWindow::GUIWindow(GUIWindowObserver& observer)
	: 	m_observer(observer),		
		m_is_fullscreen(FALSE),
		m_ui_state(UI_STATE_DISCONNECTED)		
{
	ACE_TRACE("[GUIWindow::GUIWindow()]");

	GtkBox*					hbox;	
	GtkBox*					coverbox;
	GtkBox*					textbox;
	GtkBox*					buthbox;
	GtkWidget*				menu;
	GtkWidget*				appimage;	
	GtkWidget*				scroll;	
	PangoFontDescription*	font1;	
	PangoFontDescription*	font2;	
	GtkAccelGroup*			accel = gtk_accel_group_new();

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
	hbox = GTK_BOX(gtk_hbox_new(False, 5));	
	textbox = GTK_BOX(gtk_vbox_new(False, 5));
	buthbox = GTK_BOX(gtk_vbox_new(False, 5));
			
	// Image
	appimage = gtk_image_new_from_file(app_icon_big);
	
	// Fonts	
	font1 = pango_font_description_from_string("Monospace Bold 20");
	font2 = pango_font_description_from_string("Monospace Bold 16");
		
	// Text Boxes
	m_txtbox = gtk_text_view_new();
			
	gtk_widget_modify_font(m_txtbox, font1);
		
	gtk_widget_set_size_request(GTK_WIDGET(m_txtbox), 550, 310);
		
	gtk_text_view_set_editable(GTK_TEXT_VIEW(m_txtbox), false);
		
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(m_txtbox), GTK_WRAP_WORD_CHAR);
	
	// Scroll bars
	scroll = gtk_scrolled_window_new(NULL, NULL);
	
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll), GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);
	
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scroll), GTK_SHADOW_IN);
	
	gtk_container_add(GTK_CONTAINER(scroll), GTK_WIDGET(m_txtbox));	
				
	// Text labels
	m_recognized = gtk_label_new("Recognized");
	
	gtk_widget_modify_font(m_recognized, font2);
	
	gtk_label_set_justify(GTK_LABEL(m_recognized), GTK_JUSTIFY_LEFT);
		
	gtk_label_set_ellipsize(GTK_LABEL(m_recognized), PANGO_ELLIPSIZE_END);
	
	// Buttons
	m_rec = gtk_button_new();
	m_speech = gtk_button_new();
	m_abort = gtk_button_new();
		
	gtk_button_set_image(GTK_BUTTON(m_rec),
							gtk_image_new_from_file(record_icon));
	gtk_button_set_image(GTK_BUTTON(m_speech),
									gtk_image_new_from_file(speech_icon));
	gtk_button_set_image(GTK_BUTTON(m_abort),
							gtk_image_new_from_file(stop_icon));
							
	// Menu
	// Recheck: Is it destroyed somewhere
	menu = CreateMenu(accel);
	
	// Progress bar
	m_progressbar = gtk_progress_bar_new();
	gtk_progress_set_text_alignment(GTK_PROGRESS(m_progressbar),
									0.5, 0.5);
	gtk_widget_set_size_request(GTK_WIDGET(m_progressbar), 150, 50);	
	//gtk_container_add(GTK_CONTAINER(m_window), GTK_WIDGET(vbox));	
		
	gtk_box_pack_start(textbox, m_recognized, TRUE, TRUE, 0);
	gtk_box_pack_start(textbox, scroll, TRUE, TRUE, 0);
		
	//gtk_box_pack_start(buthbox, appimage, TRUE, TRUE, 0);
	gtk_box_pack_start(buthbox, m_rec, TRUE, TRUE, 0);
	gtk_box_pack_start(buthbox, m_speech, TRUE, TRUE, 0);
	gtk_box_pack_start(buthbox, m_abort, TRUE, TRUE, 0);
		
	gtk_box_pack_start(hbox, GTK_WIDGET(textbox), TRUE, TRUE, 0);
	gtk_box_pack_start(hbox, GTK_WIDGET(buthbox), TRUE, TRUE, 0);
	
	gtk_box_pack_start(coverbox, GTK_WIDGET(hbox), TRUE, TRUE, 0);
	gtk_box_pack_start(coverbox, m_progressbar, TRUE, TRUE, 0);
			
	gtk_container_add(GTK_CONTAINER(m_window), GTK_WIDGET(coverbox));

	
#ifdef _MAEMO
	hildon_window_set_menu(HILDON_WINDOW(m_window), GTK_MENU(menu));	
#else
	gtk_box_pack_start(vbox, menu, FALSE, FALSE, 0);
#endif

	// Signals
	g_signal_connect(G_OBJECT(m_rec), "clicked", 
						G_CALLBACK(RecognizeClicked), this);
	g_signal_connect(G_OBJECT(m_speech), "clicked",
							G_CALLBACK(AbortClicked), this);
	g_signal_connect(G_OBJECT(m_abort), "clicked",
						G_CALLBACK(AbortClicked), this);
	g_signal_connect(G_OBJECT(m_window), "destroy",
						G_CALLBACK(CloseApp), this);
	g_signal_connect(G_OBJECT(m_window), "delete_event",
						G_CALLBACK(CloseApp), this);

	
#if 0
	
#ifdef _MAEMO
	hildon_window_set_menu(HILDON_WINDOW(m_window), GTK_MENU(menu));	
#else
	gtk_box_pack_start(vbox, menu, FALSE, FALSE, 0);
#endif
        
#endif

	// Shortcuts
#ifndef _MAEMO
	gtk_widget_add_accelerator(m_rec, "clicked", accel, GDK_space,
								0, 0);
	gtk_widget_add_accelerator(m_speech, "clicked", accel, GDK_space,
									0, 0);
	gtk_widget_add_accelerator(m_abort, "clicked", accel, GDK_space,
								0, 0);
#endif

	// Initial state
	SetUiState(UI_STATE_DISCONNECTED);
	
	return;
}

GUIWindow::~GUIWindow()
{
	ACE_TRACE("[GUIWindow::~GUIWindow()]");
			
	return;
}    

GtkWidget* GUIWindow::CreateMenu(GtkAccelGroup* accel)
{
	ACE_TRACE("[GUIWindow::CreateMenu()]");

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

GUIWindowObserver& GUIWindow::GetObserver()
{
	ACE_TRACE("[GUIWindow::GetObserver()]");

	return m_observer;
}

GtkWindow* GUIWindow::GetWindow()
{
	ACE_TRACE("[GUIWindow::GetWindow()]");

	if (m_window != NULL)
	{	
		return m_window;
	}
	else
	{	
		return NULL;
	}
}

void GUIWindow::Resize(gboolean is_full_screen)
{
	ACE_TRACE("[GUIWindow::Resize()]");
	
	if (is_full_screen == true)
	{ 
		gtk_widget_set_size_request(GTK_WIDGET(m_txtbox), 550, 310);		
	}
	else
	{
		gtk_widget_set_size_request(GTK_WIDGET(m_txtbox), 600, 390);
	}
	
	return;
}

void GUIWindow::SetUiState(UiState state)
{
	ACE_TRACE("[GUIWindow::SetUiState()]");
		
	switch (state) 
	{
		case UI_STATE_DISCONNECTED:			
					
			gtk_progress_bar_set_text(GTK_PROGRESS_BAR(m_progressbar),
										"Status:DISCONNECTED");			
			gtk_widget_show(m_rec);
			gtk_widget_hide(m_speech);
			gtk_widget_hide(m_abort);
			gtk_widget_set_sensitive(m_rec, FALSE);
			gtk_widget_set_sensitive(m_conmenu, TRUE);
			gtk_widget_set_sensitive(m_disconmenu, FALSE);		
//			gtk_window_set_title(m_window, APP_NAME);			
			m_ui_state = UI_STATE_DISCONNECTED;
			break;
		case UI_STATE_CONNECTING:
			gtk_progress_bar_set_text(GTK_PROGRESS_BAR(m_progressbar),
										"Status:CONNECTING...");
			m_ui_state = UI_STATE_CONNECTING;
			break;
		case UI_STATE_CONNECTED:
			gtk_progress_bar_set_text(GTK_PROGRESS_BAR(m_progressbar),
										"Status:CONNECTED");
			gtk_widget_set_sensitive(m_rec, TRUE);
			gtk_widget_set_sensitive(m_conmenu, FALSE);
			gtk_widget_set_sensitive(m_disconmenu, TRUE);
			m_ui_state = UI_STATE_CONNECTED;
			break;
		case UI_STATE_START_OF_SPEECH:
			gtk_progress_bar_set_text(GTK_PROGRESS_BAR(m_progressbar),
														"Status:START_OF_SPEECH");
			gtk_widget_hide(m_rec);
			gtk_widget_hide(m_abort);
			gtk_widget_show(m_speech);
			gtk_widget_set_sensitive(m_abort, FALSE);
			gtk_widget_set_sensitive(m_speech, TRUE);			
			m_ui_state = UI_STATE_START_OF_SPEECH;					
			break;
		case UI_STATE_START_RECOGNIZE:
			gtk_progress_bar_set_text(GTK_PROGRESS_BAR(m_progressbar),
										"Status:START_RECOGNIZE");
			gtk_widget_hide(m_rec);
			gtk_widget_hide(m_speech);
			gtk_widget_show(m_abort);
			gtk_widget_set_sensitive(m_rec, FALSE);
			gtk_widget_set_sensitive(m_speech, FALSE);
			gtk_widget_set_sensitive(m_abort, TRUE);
			m_ui_state = UI_STATE_START_RECOGNIZE;					
			break;
		case UI_STATE_ABORT_RECOGNIZE:
			gtk_progress_bar_set_text(GTK_PROGRESS_BAR(m_progressbar),
										"Status:ABORT_RECOGNIZE");
			gtk_widget_show(m_rec);
			gtk_widget_hide(m_speech);
			gtk_widget_hide(m_abort);
			gtk_widget_set_sensitive(m_rec, TRUE);
			m_ui_state = UI_STATE_CONNECTED;
		break;
		default:
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [GUIWindow::SetUiState()] "
						"[Unknown ui state received: %d]\n"), state));
			//g_critical("Unknown ui state received: %d", state);
			break;
	}
	
	return;
}

GUIWindow::UiState GUIWindow::GetUiState()
{
	ACE_TRACE("[GUIWindow::GetUiState()]");
	
	return m_ui_state;
}

gboolean GUIWindow::IsFullScreen()
{
	ACE_TRACE("[GUIWindow::IsFullScreen()]");

	return m_is_fullscreen;
}

void GUIWindow::SetFullScreen(gboolean is_full_screen)
{
	ACE_TRACE("[GUIWindow::SetFullScreen()]");

	m_is_fullscreen = is_full_screen;
	
	return;
}

void GUIWindow::SetRecognized(string& result)
{
	ACE_TRACE("[GUIWindow::SetRecognized()]");
	
	// Get GTK thread lock
	gdk_threads_enter();
	
	GtkTextBuffer* buf;
			
	buf =  gtk_text_view_get_buffer(GTK_TEXT_VIEW(m_txtbox));
	gtk_text_buffer_set_text(buf, result.c_str(), -1);

	gdk_flush();
	
	// Release GTK thread lock
	gdk_threads_leave();
	
	return; 
}

void GUIWindow::StartOfSpeech()
{
	ACE_TRACE("[GUIWindow::StartOfSpeech()]");

	// Get GTK thread lock
	gdk_threads_enter();
	
	SetUiState(UI_STATE_START_OF_SPEECH);
		
	gdk_flush();
	
	// Release GTK thread lock
	gdk_threads_leave();
	
	return; 
}

void GUIWindow::RecognitionCompleted()
{
	ACE_TRACE("[GUIWindow::RecognitionCompleted()]");

	// Get GTK thread lock
	gdk_threads_enter();
	
	SetUiState(UI_STATE_ABORT_RECOGNIZE);
		
	gdk_flush();
	
	// Release GTK thread lock
	gdk_threads_leave();
	
	return; 
}

void GUIWindow::ResetTextBoxes()
{
	ACE_TRACE("[GUIWindow::ResetTextBoxes()]");
	
	GtkTextBuffer* buf;
	
	buf =  gtk_text_view_get_buffer(GTK_TEXT_VIEW(m_txtbox));
	gtk_text_buffer_set_text(buf, "", -1);
	
	return;
}

gboolean GUIWindow::CloseApp(GtkWidget* widget, 
							 GdkEvent* event, 
							 gpointer data)
{
	ACE_TRACE("[GUIWindow::CloseApp()]");
	
	GUIWindow* win = (GUIWindow*)data;
	
	if (win != NULL)
	{	
		(win->GetObserver()).QuitApp();
	}
	
	return TRUE;
}

gboolean GUIWindow::DeleteEvent(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[GUIWindow::DeleteEvent()]");

	GUIWindow* win = (GUIWindow*)data;
	
	if (win != NULL)
	{
		(win->GetObserver()).QuitApp();
	}
	
	return TRUE;
}

void GUIWindow::RecognizeClicked(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[GUIWindow::RecognizeClicked()]");

	GUIWindow* win = (GUIWindow*)data;
	
	if (win != NULL)
	{
		win->SetUiState(UI_STATE_START_RECOGNIZE);
		(win->GetObserver()).StartRecognize();
	}
	
	return;
}

void GUIWindow::AbortClicked(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[GUIWindow::AbortClicked()]");

	GUIWindow* win = (GUIWindow*)data;

	if (win != NULL)
	{
		win->SetUiState(UI_STATE_ABORT_RECOGNIZE);
		(win->GetObserver()).AbortRecognize();
	}
	
	return;
}

void GUIWindow::ConnectToRemote(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[GUIWindow::ConnectToRemote()]");
	
	GUIWindow* win = (GUIWindow*)data;

	if (win != NULL)
	{	
		(win->GetObserver()).ConnectToRemote();	
		//win->SetUiState(UI_STATE_CONNECTED);		
	}	
	
	return;
}

void GUIWindow::DisconnectFromRemote(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[GUIWindow::DisconnectFromRemote()]");
	
	GUIWindow* win = (GUIWindow*)data;
	
	if (win != NULL)
	{
		(win->GetObserver()).DisconnectFromRemote();
		win->SetUiState(UI_STATE_DISCONNECTED);		
	}

	return;
}

void GUIWindow::ShowAboutDialog(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[GUIWindow::ShowAboutDialog()]");

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
