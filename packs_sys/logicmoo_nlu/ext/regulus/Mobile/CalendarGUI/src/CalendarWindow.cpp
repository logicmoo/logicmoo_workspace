/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	CalendarWindow.cpp
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

#include "CalendarWindow.hpp"
#include "ace/Log_Msg.h"

#ifdef MAEMO
static const int album_cover_size = 3;
#else
static const int album_cover_size = 3;
#endif

CalendarWindow::CalendarWindow(CalendarWindowObserver& observer)
	: 	m_observer(observer),
		m_list_index(0),
		m_list_items(0),
		m_is_fullscreen(FALSE),
		m_ui_state(UI_STATE_DISCONNECTED)		
{
	ACE_TRACE("[CalendarWindow::CalendarWindow()]");

	GtkBox*					hbox;	
	GtkBox*					coverbox;
	GtkBox*					textbox;
	GtkBox*					buthbox;
	GtkWidget*				menu;
	GtkWidget*				appimage;	
	GtkWidget*				scroll1;
	GtkWidget*				scroll2;
	GtkWidget*				scroll3;
	PangoFontDescription*	font1;
	PangoFontDescription*	font2;
	PangoFontDescription*	font3;
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
	//hbox2 = GTK_BOX(gtk_hbox_new(False, 5));
		
	// Image
	appimage = gtk_image_new_from_file(app_icon_big);
	
	// Fonts	
	font1 = pango_font_description_from_string("Monospace Bold 20");
	font2 = pango_font_description_from_string("Monospace Bold 16");
	font3 = pango_font_description_from_string("Monospace Bold 18");
	
	// Text Boxes
	m_txtbox1 = gtk_text_view_new();
	m_txtbox2 = gtk_text_view_new();
		
	gtk_widget_modify_font(m_txtbox1, font1);
	gtk_widget_modify_font(m_txtbox2, font1);
	
	gtk_widget_set_size_request(GTK_WIDGET(m_txtbox1), 550, 80);
	gtk_widget_set_size_request(GTK_WIDGET(m_txtbox2), 550, 100);
	
	gtk_text_view_set_editable(GTK_TEXT_VIEW(m_txtbox1), true);
	gtk_text_view_set_editable(GTK_TEXT_VIEW(m_txtbox2), false);
	
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(m_txtbox1), GTK_WRAP_WORD_CHAR);
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(m_txtbox2), GTK_WRAP_WORD_CHAR);

	// List
	m_list = gtk_clist_new(1);
		 
	gtk_widget_set_size_request(GTK_WIDGET(m_list), 550, 110);
	gtk_widget_modify_font(m_list, font3);	
	
	gchar* text[3][1] = {	{"when is the next meeting?"},
							{"will mike attend a meeting on monday?"},
							{"what is john 's phone number?"}};	
	
	gtk_clist_append((GtkCList*)m_list, text[0]);
	gtk_clist_append((GtkCList*)m_list, text[1]);
	gtk_clist_append((GtkCList*)m_list, text[2]);
	
	m_list_items = 3;
		
	gtk_clist_select_row((GtkCList*)m_list, 0, 0);
	
	// Scroll bars
	scroll1 = gtk_scrolled_window_new(NULL, NULL);
	scroll2 = gtk_scrolled_window_new(NULL, NULL);
	scroll3 = gtk_scrolled_window_new(NULL, NULL);
		
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll1), GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll2), GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll3), GTK_POLICY_ALWAYS, GTK_POLICY_ALWAYS);
		
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scroll1), GTK_SHADOW_IN);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scroll2), GTK_SHADOW_IN);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scroll3), GTK_SHADOW_IN);
		
	gtk_container_add(GTK_CONTAINER(scroll1), GTK_WIDGET(m_txtbox1));	
	gtk_container_add(GTK_CONTAINER(scroll2), GTK_WIDGET(m_txtbox2));
	gtk_container_add(GTK_CONTAINER(scroll3), GTK_WIDGET(m_list));
					
	// Text labels
	m_understood = gtk_label_new("Input");
	m_result = gtk_label_new("Result");
	m_help = gtk_label_new("Help");

	gtk_widget_modify_font(m_understood, font2);
	gtk_widget_modify_font(m_result, font2);
	gtk_widget_modify_font(m_help, font2);
	
	gtk_label_set_justify(GTK_LABEL(m_understood), GTK_JUSTIFY_LEFT);
	gtk_label_set_justify(GTK_LABEL(m_result), GTK_JUSTIFY_LEFT);
	gtk_label_set_justify(GTK_LABEL(m_help), GTK_JUSTIFY_LEFT);
	
	gtk_label_set_ellipsize(GTK_LABEL(m_understood), PANGO_ELLIPSIZE_END);
	gtk_label_set_ellipsize(GTK_LABEL(m_result), PANGO_ELLIPSIZE_END);
	gtk_label_set_ellipsize(GTK_LABEL(m_help), PANGO_ELLIPSIZE_END);
	
	// Buttons
	m_rec = gtk_button_new();
	m_speech = gtk_button_new();
	m_abort = gtk_button_new();
	m_up = gtk_button_new();
	m_select = gtk_button_new();
	m_down = gtk_button_new();
	
	gtk_button_set_image(GTK_BUTTON(m_rec),
							gtk_image_new_from_file(record_icon));
	gtk_button_set_image(GTK_BUTTON(m_speech),
								gtk_image_new_from_file(speech_icon));
	gtk_button_set_image(GTK_BUTTON(m_abort),
							gtk_image_new_from_file(stop_icon));
	gtk_button_set_image(GTK_BUTTON(m_up),
							gtk_image_new_from_file(up_icon));
	gtk_button_set_image(GTK_BUTTON(m_select),
							gtk_image_new_from_file(select_icon));
	gtk_button_set_image(GTK_BUTTON(m_down),
							gtk_image_new_from_file(down_icon));
		
	// Menu
	// Recheck: Is it destroyed somewhere
	menu = CreateMenu(accel);
	
	// Progress bar
	m_progressbar = gtk_progress_bar_new();
	gtk_progress_set_text_alignment(GTK_PROGRESS(m_progressbar),
									0.5, 0.5);
	gtk_widget_set_size_request(GTK_WIDGET(m_progressbar), 150, 50);	
	//gtk_container_add(GTK_CONTAINER(m_window), GTK_WIDGET(vbox));	
		
	gtk_box_pack_start(textbox, m_understood, TRUE, TRUE, 0);
	gtk_box_pack_start(textbox, scroll1, TRUE, TRUE, 0);
	gtk_box_pack_start(textbox, m_result, TRUE, TRUE, 0);
	gtk_box_pack_start(textbox, scroll2, TRUE, TRUE, 0);
	gtk_box_pack_start(textbox, m_help, TRUE, TRUE, 0);
	gtk_box_pack_start(textbox, scroll3, TRUE, TRUE, 0);
		
	//gtk_box_pack_start(buthbox, appimage, TRUE, TRUE, 0);
	gtk_box_pack_start(buthbox, m_rec, TRUE, TRUE, 0);
	gtk_box_pack_start(buthbox, m_speech, TRUE, TRUE, 0);
	gtk_box_pack_start(buthbox, m_abort, TRUE, TRUE, 0);
	gtk_box_pack_start(buthbox, m_up, TRUE, TRUE, 0);
	gtk_box_pack_start(buthbox, m_select, TRUE, TRUE, 0);
	gtk_box_pack_start(buthbox, m_down, TRUE, TRUE, 0);
	
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
	g_signal_connect(G_OBJECT(m_up), "clicked",
						G_CALLBACK(UpClicked), this);
	g_signal_connect(G_OBJECT(m_select), "clicked",
						G_CALLBACK(SelectClicked), this);
	g_signal_connect(G_OBJECT(m_down), "clicked",
						G_CALLBACK(DownClicked), this);
	g_signal_connect(G_OBJECT(m_list), "select_row",
						G_CALLBACK(ListRowSelected), this);
	g_signal_connect(G_OBJECT(m_window), "destroy",
						G_CALLBACK(CloseApp), this);
	g_signal_connect(G_OBJECT(m_window), "delete_event",
						G_CALLBACK(CloseApp), this);

	// Shortcuts
#ifndef _MAEMO
	gtk_widget_add_accelerator(m_rec, "clicked", accel, GDK_space,
								0, 0);
	gtk_widget_add_accelerator(m_speech, "clicked", accel, GDK_space,
									0, 0);
	gtk_widget_add_accelerator(m_abort, "clicked", accel, GDK_space,
								0, 0);
	gtk_widget_add_accelerator(m_next, "clicked", accel, GDK_Right,
								GDK_CONTROL_MASK, 0);
#endif

	// Initial state
	SetUiState(UI_STATE_DISCONNECTED);
	
	return;
}

CalendarWindow::~CalendarWindow()
{
	ACE_TRACE("[CalendarWindow::~CalendarWindow()]");
				
	return;
}    

GtkWidget* CalendarWindow::CreateMenu(GtkAccelGroup* accel)
{
	ACE_TRACE("[CalendarWindow::CreateMenu()]");

	GtkMenuItem*			connection;
	GtkMenuShell*			connectionsub;
	GtkMenuItem*			tools;
	GtkMenuShell*			toolssub;
	GtkMenuItem*			help;
	GtkMenuShell*			helpsub;
	GtkWidget*				connect;
	GtkWidget*				disconnect;
	GtkWidget*				initialize;
	GtkWidget*				history;
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
	
	gtk_widget_modify_font(gtk_bin_get_child(GTK_BIN(connection)), font);
	gtk_widget_modify_font(gtk_bin_get_child(GTK_BIN(connect)), font);
	gtk_widget_modify_font(gtk_bin_get_child(GTK_BIN(disconnect)), font);

	// Tools
	tools = GTK_MENU_ITEM(gtk_menu_item_new_with_mnemonic("_Tools"));
	toolssub = GTK_MENU_SHELL(gtk_menu_new());
	initialize = gtk_menu_item_new_with_label("Initialize Dialogue...");
	history = gtk_menu_item_new_with_label("Show History...");	
	gtk_menu_shell_append(bar, GTK_WIDGET(tools));
	gtk_menu_item_set_submenu(tools, GTK_WIDGET(toolssub));
	gtk_menu_shell_append(toolssub, initialize);
	gtk_menu_shell_append(toolssub, history);
	g_signal_connect(G_OBJECT(initialize), "activate",
						G_CALLBACK(InitializeDialogue), this);
	g_signal_connect(G_OBJECT(history), "activate",
						G_CALLBACK(ShowHistory), this);
											
	quit = gtk_menu_item_new_with_label("Quit");
	gtk_widget_add_accelerator(quit, "activate", accel, GDK_q,
								GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE);
	
	gtk_widget_modify_font(gtk_bin_get_child(GTK_BIN(tools)), font);
	gtk_widget_modify_font(gtk_bin_get_child(GTK_BIN(initialize)), font);
	gtk_widget_modify_font(gtk_bin_get_child(GTK_BIN(history)), font);

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
	
	quit = gtk_menu_item_new_with_label("Quit");
	gtk_widget_add_accelerator(quit, "activate", accel, GDK_q,
								GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE);
				
	gtk_widget_modify_font(gtk_bin_get_child(GTK_BIN(quit)), font);	
		
	g_signal_connect(G_OBJECT(quit), "activate",
							G_CALLBACK(DeleteEvent), this);

#ifdef _MAEMO
	gtk_menu_shell_append(bar, quit);
#endif
	
	m_conmenu = GTK_WIDGET(connect);	
	m_disconmenu = GTK_WIDGET(disconnect);
	m_toolsmenu = GTK_WIDGET(tools);
	
	return GTK_WIDGET(bar);
}

gboolean CalendarWindow::IsFullScreen()
{
	ACE_TRACE("[CalendarWindow::IsFullScreen()]");

	return m_is_fullscreen;
}

void CalendarWindow::SetFullScreen(gboolean is_full_screen)
{
	ACE_TRACE("[CalendarWindow::SetFullScreen()]");

	m_is_fullscreen = is_full_screen;
	
	return;
}

void CalendarWindow::SetAppImage(const guchar *data, int size)
{
	ACE_TRACE("[CalendarWindow::SetAppImage()]");

	GdkPixbufLoader	*ldr = NULL;
	GdkPixbuf		*pixbuf = NULL;
	
	if (data != NULL) 
	{
		g_return_if_fail(size > 0);
		GError *err = NULL;
		ldr = gdk_pixbuf_loader_new();
		gdk_pixbuf_loader_set_size(ldr, album_cover_size,
									album_cover_size);
		gdk_pixbuf_loader_write(ldr, data, size, NULL);
		gdk_pixbuf_loader_close(ldr, &err);
		
		if (err != NULL) 
		{
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [CalendarWindow::SetAppImage()] "
						"[Error loading image: %s]\n"), 
						err->message ? err->message : "unknown]"));
			g_error_free(err);
			g_object_unref(G_OBJECT(ldr));
			ldr = NULL;
		}
		else
		{
			pixbuf = gdk_pixbuf_loader_get_pixbuf(ldr);
		}
	}
	
	gtk_image_set_from_pixbuf(GTK_IMAGE(m_cover), pixbuf);
	gtk_widget_set_sensitive(m_cover, TRUE);
	
	if (ldr != NULL) 
	{
		g_object_unref(G_OBJECT(ldr));
	}
	
	return;
}

CalendarWindowObserver& CalendarWindow::GetObserver()
{
	ACE_TRACE("[CalendarWindow::GetObserver()]");

	return m_observer;
}

gboolean CalendarWindow::CloseApp(GtkWidget* widget, 
									GdkEvent* event, 
									gpointer data)
{
	ACE_TRACE("[CalendarWindow::CloseApp()]");
	
	CalendarWindow* win = (CalendarWindow*)data;
	
	if (win != NULL)
	{	
		(win->GetObserver()).QuitApp();
	}
	
	return TRUE;
}

gboolean CalendarWindow::DeleteEvent(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[CalendarWindow::DeleteEvent()]");

	CalendarWindow* win = (CalendarWindow*)data;
	
	if (win != NULL)
	{
		(win->GetObserver()).QuitApp();
	}
	
	return TRUE;
}

void CalendarWindow::RecognizeClicked(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[CalendarWindow::RecognizeClicked()]");

	CalendarWindow* win = (CalendarWindow*)data;
	
	if (win != NULL)
	{
		//win->RecognitionStarted();
		win->SetUiState(UI_STATE_START_RECOGNIZE);
		(win->GetObserver()).StartRecognize();
	}
	
	return;
}

void CalendarWindow::AbortClicked(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[CalendarWindow::AbortClicked()]");

	CalendarWindow* win = (CalendarWindow*)data;

	if (win != NULL)
	{
		//win->RecognitionCompleted();
		win->SetUiState(UI_STATE_ABORT_RECOGNIZE);
		(win->GetObserver()).AbortRecognize();
	}
	
	return;
}

void CalendarWindow::UpClicked(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[CalendarWindow::UpClicked()]");
	
	CalendarWindow* win = (CalendarWindow*)data;
	
	if (win != NULL)
	{	
		win->IterateList(false);
	}
	
	return;
}

void CalendarWindow::SelectClicked(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[CalendarWindow::SelectClicked()]");
	
	CalendarWindow* win = (CalendarWindow*)data;
	
	if (win != NULL)
	{		
		(win->GetObserver()).RequestFromHelp(win->GetRowData(), 1);
	}
	
	return;
}

void CalendarWindow::DownClicked(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[CalendarWindow::DownClicked()]");
	
	CalendarWindow* win = (CalendarWindow*)data;
	
	if (win != NULL)
	{	
		win->IterateList(true);
	}
	
	return;
}

void CalendarWindow::Resize(gboolean is_full_screen)
{
	ACE_TRACE("[CalendarWindow::Resize()]");
	
	if (is_full_screen == true)
	{ 
		gtk_widget_set_size_request(GTK_WIDGET(m_txtbox1), 550, 80);
		gtk_widget_set_size_request(GTK_WIDGET(m_txtbox2), 550, 100);
		gtk_widget_set_size_request(GTK_WIDGET(m_list), 550, 110);
	}
	else
	{
		gtk_widget_set_size_request(GTK_WIDGET(m_txtbox1), 600, 90);
		gtk_widget_set_size_request(GTK_WIDGET(m_txtbox2), 600, 120);
		gtk_widget_set_size_request(GTK_WIDGET(m_list), 600, 110);
	}
	
	return;
}

void CalendarWindow::ConnectToRemote(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[CalendarWindow::ConnectToRemote()]");
	
	CalendarWindow* win = (CalendarWindow*)data;
		
	if (win != NULL)
	{	
		(win->GetObserver()).ConnectToRemote();	
		//win->SetUiState(UI_STATE_CONNECTED);		
	}	
	
	return;
}

void CalendarWindow::DisconnectFromRemote(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[CalendarWindow::DisconnectFromRemote()]");
	
	CalendarWindow* win = (CalendarWindow*)data;
	
	if (win != NULL)
	{
		(win->GetObserver()).DisconnectFromRemote();
		win->SetUiState(UI_STATE_DISCONNECTED);		
	}

	return;
}

void CalendarWindow::InitializeDialogue(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[CalendarWindow::InitializeDialogue()]");
	
	CalendarWindow* win = (CalendarWindow*)data;
		
	if (win != NULL)
	{	
		(win->GetObserver()).InitializeDialogue();				
	}	
	
	return;
}

void CalendarWindow::ShowHistory(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[CalendarWindow::ConnectToRemote()]");
	
	CalendarWindow* win = (CalendarWindow*)data;
		
	if (win != NULL)
	{	
		(win->GetObserver()).ShowHistory();	
	}	
	
	return;
}

void CalendarWindow::ShowAboutDialog(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[CalendarWindow::ShowAboutDialog()]");

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

// TODO: deprecated?
void CalendarWindow::ListRowClicked(GtkWidget* widget, gint row)
{
	ACE_TRACE("[CalendarWindow::ListRowClicked()]");

	return;
}
									
void CalendarWindow::ListRowSelected(GtkWidget* widget, gint row, gint column,
										GdkEventButton* event, gpointer data)
{
	ACE_TRACE("[CalendarWindow::ListRowSelected()]");
	
	CalendarWindow* win = (CalendarWindow*)data;
	
	if (win != NULL)
	{
		win->SetListIndex(row);
		
		if (event != NULL)
		{
			if (event->type == GDK_2BUTTON_PRESS)
			{
				ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [CalendarWindow::ListRowSelected()] "
						"[List index: %d]\n"), win->GetListIndex()));	
				
				(win->GetObserver()).RequestFromHelp(win->GetRowData(), 1);
			}
		}
	}
	
	return;
}
									
void CalendarWindow::SetUiState(UiState state)
{
	ACE_TRACE("[CalendarWindow::SetUiState()]");
	
	// Get GTK thread lock
//	gdk_threads_enter();
		
	switch (state) 
	{
		case UI_STATE_DISCONNECTED:			
					
			gtk_progress_bar_set_text(GTK_PROGRESS_BAR(m_progressbar),
										"Status:DISCONNECTED");			
			gtk_widget_show(m_rec);
			gtk_widget_hide(m_speech);
			gtk_widget_hide(m_abort);
			gtk_widget_set_sensitive(m_rec, FALSE);
			gtk_widget_set_sensitive(m_up, FALSE);			
			gtk_widget_set_sensitive(m_select, FALSE);
			gtk_widget_set_sensitive(m_down, FALSE);
			gtk_widget_set_sensitive(m_conmenu, TRUE);
			gtk_widget_set_sensitive(m_toolsmenu, FALSE);
			gtk_widget_set_sensitive(m_disconmenu, FALSE);		
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
			gtk_widget_set_sensitive(m_up, TRUE);			
			gtk_widget_set_sensitive(m_select, TRUE);
			gtk_widget_set_sensitive(m_down, TRUE);
			gtk_widget_set_sensitive(m_conmenu, FALSE);
			gtk_widget_set_sensitive(m_toolsmenu, TRUE);
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
			g_critical("Unknown ui state received: %d", state);
			break;
	}
	    
	return;
}

CalendarWindow::UiState CalendarWindow::GetUiState()
{
	ACE_TRACE("[CalendarWindow::GetUiState()]");
	
	return m_ui_state;
}

GtkWindow* CalendarWindow::GetWindow()
{
	ACE_TRACE("[CalendarWindow::GetWindow()]");

	if (m_window != NULL)
	{	
		return m_window;
	}
	else
	{	
		return NULL;
	}
}

gchar* CalendarWindow::GetText()
{
	ACE_TRACE("[CalendarWindow::GetText()]");
	
	if (GetUiState() != UI_STATE_CONNECTED)
	{
		return NULL;
	}
 	
	char* result = NULL;
	GtkTextBuffer* buf;
	GtkTextIter start, end;
		
	buf =  gtk_text_view_get_buffer(GTK_TEXT_VIEW(m_txtbox1));
	gtk_text_buffer_get_bounds(buf, &start, &end);
	result = gtk_text_buffer_get_text(buf,  &start, &end, FALSE);
	
	if (result == NULL)
	{	
		g_debug("[CalendarWindow::GetText()]  NULL");
	}

	return result;
}

void CalendarWindow::IterateList(gboolean forward)
{
	ACE_TRACE("[CalendarWindow::GetWindow()]");

	if (GetUiState() != UI_STATE_CONNECTED)
	{
		return;
	}

	if (forward == true)
	{
		if (m_list_index < m_list_items - 1)
		{
			++m_list_index;
		}
		else
		{
			m_list_index = 0;			
		}
		
		gtk_clist_select_row((GtkCList*)m_list, m_list_index, 0);
	}
	else
	{
		if (m_list_index > 0)
		{
			--m_list_index;			
		}
		else
		{
			m_list_index = m_list_items - 1;			
		}
		
		gtk_clist_select_row((GtkCList*)m_list, m_list_index, 0);
	}
	
	return;
}

void CalendarWindow::SetListIndex(guint index)
{
	ACE_TRACE("[CalendarWindow::SetListIndex()]");

	m_list_index = index; 
	
	return;
}

guint CalendarWindow::GetListIndex()
{
	ACE_TRACE("[CalendarWindow::GetListIndex()]");

	return m_list_index; 
}

gchar* CalendarWindow::GetRowData()
{
	ACE_TRACE("[CalendarWindow::GetRowData()]");
	
	if (GetUiState() != UI_STATE_CONNECTED)
	{
		return NULL;
	}
			
	char* result = NULL;
	
	gtk_clist_get_text(GTK_CLIST(m_list), m_list_index, 0, &result);
	
	if (result == NULL)
	{	
		g_debug("[CalendarWindow::GetRowData()]  NULL");
	}
	
	g_debug("[CalendarWindow::GetRowData()]  %d", m_list_index);
		
	return result;
}

void CalendarWindow::SetUnderstood(string& result)
{
	ACE_TRACE("[CalendarWindow::SetUnderstood()]");
	
	// Get GTK thread lock
	gdk_threads_enter();
	
	GtkTextBuffer* buf;
			
	buf =  gtk_text_view_get_buffer(GTK_TEXT_VIEW(m_txtbox1));
	gtk_text_buffer_set_text(buf, result.c_str(), -1);

	gdk_flush();
	
	// Release GTK thread lock
	gdk_threads_leave();
	
	return; 
}

void CalendarWindow::SetQueryResult(string& result)
{
	ACE_TRACE("[CalendarWindow::SetQueryResult()]");

	// Get GTK thread lock
	gdk_threads_enter();
	
	GtkTextBuffer* buf;
		
	buf =  gtk_text_view_get_buffer(GTK_TEXT_VIEW(m_txtbox2));
	gtk_text_buffer_set_text(buf, result.c_str(), -1);
	
	gdk_flush();
	
	// Release GTK thread lock
	gdk_threads_leave();

	return; 
}

void CalendarWindow::SetHelpExamples(string& result)
{
	ACE_TRACE("[CalendarWindow::SetHelpExamples()]");

	// Get GTK thread lock
	gdk_threads_enter();
	
	gtk_clist_clear((GtkCList*)m_list);
	
	string buffer("");
	
	m_list_items = 0;

	while (result.size() > 0)
	{
		SplitHelpExamples(result, buffer, "_");
		
		if ((buffer == "") || (buffer == "error."))
		{
			break;
		}
		else
		{
			buffer += "?";	
			gchar* item = g_strdup(buffer.c_str());
			gtk_clist_append((GtkCList*)m_list, &item);	
			++m_list_items;	
			free(item);
			buffer = "";
		}
	}
	
	if (buffer.size() > 0)
	{
		gtk_clist_select_row((GtkCList*)m_list, 0, 0);
		SetListIndex(0);
	}
		
	gdk_flush();
	
	// Release GTK thread lock
	gdk_threads_leave();
	
	return; 
}

void CalendarWindow::RecognitionStarted()
{
	ACE_TRACE("[CalendarWindow::RecognitionStarted()]");

	// Get GTK thread lock
	gdk_threads_enter();
	
	SetUiState(UI_STATE_START_RECOGNIZE);
		
	gdk_flush();
	
	// Release GTK thread lock
	gdk_threads_leave();
	
	return; 
}

void CalendarWindow::StartOfSpeech()
{
	ACE_TRACE("[CalendarWindow::StartOfSpeech()]");

	// Get GTK thread lock
	gdk_threads_enter();
	
	SetUiState(UI_STATE_START_OF_SPEECH);
		
	gdk_flush();
	
	// Release GTK thread lock
	gdk_threads_leave();
	
	return; 
}

void CalendarWindow::RecognitionCompleted()
{
	ACE_TRACE("[CalendarWindow::RecognitionCompleted()]");

	// Get GTK thread lock
	gdk_threads_enter();
	
	SetUiState(UI_STATE_ABORT_RECOGNIZE);
		
	gdk_flush();
	
	// Release GTK thread lock
	gdk_threads_leave();
	
	return; 
}

void CalendarWindow::ShowBusyCursor(bool choice)
{
	ACE_TRACE("[CalendarWindow::ShowBusyCursor()]");

	GdkDisplay* disp;
	GdkScreen* screen;
	
	if (choice == false)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [CalendarWindow::ShowBusyCursor()] "
										"[Hide busy cursor]\n")));
		
		gdk_window_set_cursor((GTK_WIDGET(m_window)->window), NULL);
	}
	else
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [CalendarWindow::ShowBusyCursor()] "
										"[Show busy cursor]\n")));
		
		GdkCursor* cursor = gdk_cursor_new(GDK_WATCH);
				
		gdk_window_set_cursor((GTK_WIDGET(m_window)->window), cursor);	
	}
	
	disp = gdk_display_get_default();
	screen = gdk_display_get_default_screen(disp);
	gdk_display_warp_pointer(disp, screen, 200, 140);
	
	gtk_widget_show(GTK_WIDGET(m_window));

	return;
}

void CalendarWindow::ResetTextBoxes()
{
	ACE_TRACE("[CalendarWindow::ResetTextBoxes()]");
	
	GtkTextBuffer* buf1;
	GtkTextBuffer* buf2;
	
	buf1 =  gtk_text_view_get_buffer(GTK_TEXT_VIEW(m_txtbox1));
	gtk_text_buffer_set_text(buf1, "", -1);
	
	buf2 =  gtk_text_view_get_buffer(GTK_TEXT_VIEW(m_txtbox2));
	gtk_text_buffer_set_text(buf2, "", -1);
	
	return;
}

void CalendarWindow::FindAndReplaceFirst(string &input_str,
									string const &search_str, 
									string const &replace_str)
{
	ACE_TRACE("[CalendarWindow::FindAndReplace()]");

	string::size_type pos = 0;
	
	if ((pos = input_str.find(search_str, pos)) != string::npos)
	{		
		input_str.replace(pos, search_str.size(), replace_str);
		pos = pos + replace_str.length();
	}
	
	return;
}

void CalendarWindow::FindAndReplace(string &input_str,
									string const &search_str, 
									string const &replace_str)
{
	ACE_TRACE("[CalendarWindow::FindAndReplace()]");

	string::size_type pos = 0;
	
	while ((pos = input_str.find(search_str, pos)) != string::npos)
	{		
		input_str.replace(pos, search_str.size(), replace_str);
		pos = pos + replace_str.length();
	}
	
	return;
}

void CalendarWindow::SplitHelpExamples(string& buffer, 
						string& result, const string token)
{
	ACE_TRACE("[CalendarWindow::SplitHelpExamples()]");

	string::size_type pos = 0;
	string replace_str("");
	char rm_buff[150];
	
	pos = buffer.find(token, pos);
	
	if (pos	== string::npos)
	{
		return;
	}
	
	++pos;
	
	if (pos >= 150)
 	{
 		pos = 150 - 1;
 	}
		
	int length = buffer.copy(rm_buff, pos, 0);
	
	if (length >= 150)
 	{
 		length = 150 - 1;
 	}
 	 	
 	rm_buff[length] = '\0';
 		
 	FindAndReplaceFirst(buffer, rm_buff, "");

	result = rm_buff;
	FindAndReplace(result, token, "");
	
	return;
}
