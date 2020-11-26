/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MedSLTPatWindow.cpp
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

#include "MedSLTPatWindow.hpp"
#include "ace/Log_Msg.h"

MedSLTPatWindow::MedSLTPatWindow(MedSLTPatWindowObserver& observer)
	: 	m_observer(observer),
		m_list_index(0),
		m_list_items(0),
		m_is_fullscreen(FALSE),
		m_back_translation(""),
		m_ui_state(UI_STATE_DISCONNECTED)		
{
	ACE_TRACE("[MedSLTPatWindow::MedSLTPatWindow()]");

	GtkBox*					hbox;
	GtkBox*					coverbox;
	GtkBox*					textbox;
	GtkBox*					buthbox;
	GtkWidget*				menu;
	GtkWidget*				appimage;	
	GtkWidget*				scroll1;
	GtkWidget* 				scroll2;
	GtkWidget* 				scroll3;
	PangoFontDescription*	font1;
	PangoFontDescription*	font2;	
	GtkStyle*				style; 
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
	
	// Styles
	style = gtk_style_new();
	style->font_desc = font1;
	gdk_color_parse ("red", &(style->fg[GTK_STATE_NORMAL]));
	
	// Lists
	m_list = gtk_clist_new(1);
	 
	gtk_widget_set_size_request(GTK_WIDGET(m_list), 550, 130);
	gtk_widget_modify_font(m_list, font1);
		
	/*gchar* text[3][1] = {	{"où avez vous mal?"},		
							{"avez vous mal sous le devant de la tête?"},
							{"la douleur survient elle le matin?"}};	*/
	
	gchar* text[3][1] = {	{"sí mucho"},		
								{"no sé"},
								{"la cabeza"}};
	
	gtk_clist_append((GtkCList*)m_list, text[0]);
	gtk_clist_append((GtkCList*)m_list, text[1]);
	gtk_clist_append((GtkCList*)m_list, text[2]);
	
	m_list_items = 3;
	
	gtk_clist_select_row((GtkCList*)m_list, 0, 0);
	
	// Text Boxes
	m_txtbox1 = gtk_text_view_new();
			
	gtk_widget_modify_font(m_txtbox1, font1);
		
	gtk_widget_set_size_request(GTK_WIDGET(m_txtbox1), 550, 50);
		
	gtk_text_view_set_editable(GTK_TEXT_VIEW(m_txtbox1), false);
		
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(m_txtbox1), GTK_WRAP_WORD_CHAR);
	
	gtk_widget_set_style (m_txtbox1, style);
	
	m_txtbox2 = gtk_text_view_new();
				
	gtk_widget_modify_font(m_txtbox2, font1);
			
	gtk_widget_set_size_request(GTK_WIDGET(m_txtbox2), 550, 50);
			
	gtk_text_view_set_editable(GTK_TEXT_VIEW(m_txtbox2), false);
		
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(m_txtbox2), GTK_WRAP_WORD_CHAR);
		
	// Scroll bars
	scroll1 = gtk_scrolled_window_new(NULL, NULL);
	scroll2 = gtk_scrolled_window_new(NULL, NULL);
	scroll3 = gtk_scrolled_window_new(NULL, NULL);
	
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll1), GTK_POLICY_ALWAYS, GTK_POLICY_ALWAYS);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll2), GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll3), GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);
	
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scroll1), GTK_SHADOW_IN);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scroll2), GTK_SHADOW_IN);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scroll3), GTK_SHADOW_IN);
	
	gtk_container_add(GTK_CONTAINER(scroll1), GTK_WIDGET(m_list));
	gtk_container_add(GTK_CONTAINER(scroll2), GTK_WIDGET(m_txtbox1));
	gtk_container_add(GTK_CONTAINER(scroll3), GTK_WIDGET(m_txtbox2));
			
	// Text labels
	m_understood = gtk_label_new("Understood");
	m_question = gtk_label_new("Question");
	m_translation = gtk_label_new("Translation");

	gtk_widget_modify_font(m_understood, font2);
	gtk_widget_modify_font(m_question, font2);
	gtk_widget_modify_font(m_translation, font2);
	
	gtk_label_set_justify(GTK_LABEL(m_understood), GTK_JUSTIFY_LEFT);
	gtk_label_set_justify(GTK_LABEL(m_question), GTK_JUSTIFY_LEFT);
	gtk_label_set_justify(GTK_LABEL(m_translation), GTK_JUSTIFY_LEFT);
		
	gtk_label_set_ellipsize(GTK_LABEL(m_understood), PANGO_ELLIPSIZE_END);	
	gtk_label_set_ellipsize(GTK_LABEL(m_question), PANGO_ELLIPSIZE_END);
	gtk_label_set_ellipsize(GTK_LABEL(m_translation), PANGO_ELLIPSIZE_END);
		
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
	gtk_box_pack_start(textbox, m_question, TRUE, TRUE, 0);
	gtk_box_pack_start(textbox, scroll2, TRUE, TRUE, 0);
	gtk_box_pack_start(textbox, m_translation, TRUE, TRUE, 0);
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

MedSLTPatWindow::~MedSLTPatWindow()
{
	ACE_TRACE("[MedSLTPatWindow::~MedSLTPatWindow()]");
			
	return;
}    

GtkWidget* MedSLTPatWindow::CreateMenu(GtkAccelGroup* accel)
{
	ACE_TRACE("[MedSLTPatWindow::CreateMenu()]");

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

MedSLTPatWindowObserver& MedSLTPatWindow::GetObserver() const
{
	ACE_TRACE("[MedSLTPatWindow::GetObserver()]");

	return m_observer;
}

GtkWindow* MedSLTPatWindow::GetWindow() const
{
	ACE_TRACE("[MedSLTPatWindow::GetWindow()]");

	if (m_window != NULL)
	{	
		return m_window;
	}
	else
	{	
		return NULL;
	}
}

void MedSLTPatWindow::Resize(gboolean is_full_screen) const
{
	ACE_TRACE("[MedSLTPatWindow::Resize()]");
	
	if (is_full_screen == true)
	{ 
		gtk_widget_set_size_request(GTK_WIDGET(m_list), 550, 130);		
		gtk_widget_set_size_request(GTK_WIDGET(m_txtbox1), 550, 50);
		gtk_widget_set_size_request(GTK_WIDGET(m_txtbox2), 550, 50);
	}
	else
	{
		gtk_widget_set_size_request(GTK_WIDGET(m_list), 600, 150);		
		gtk_widget_set_size_request(GTK_WIDGET(m_txtbox1), 600, 40);		
		gtk_widget_set_size_request(GTK_WIDGET(m_txtbox2), 600, 40);
	}
	
	return;
}

void MedSLTPatWindow::SetUiState(UiState state)
{
	ACE_TRACE("[MedSLTPatWindow::SetUiState()]");
	
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
			gtk_widget_set_sensitive(m_up, TRUE);			
			gtk_widget_set_sensitive(m_select, TRUE);
			gtk_widget_set_sensitive(m_down, TRUE);
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
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTPatWindow::SetUiState()] "
						"[Unknown ui state received: %d]\n"), state));
			break;
	}

	return;
}

MedSLTPatWindow::UiState MedSLTPatWindow::GetUiState() const
{
	ACE_TRACE("[MedSLTPatWindow::GetUiState()]");
	
	return m_ui_state;
}

gboolean MedSLTPatWindow::IsFullScreen() const
{
	ACE_TRACE("[MedSLTPatWindow::IsFullScreen()]");

	return m_is_fullscreen;
}

void MedSLTPatWindow::SetFullScreen(gboolean is_full_screen)
{
	ACE_TRACE("[MedSLTPatWindow::SetFullScreen()]");

	m_is_fullscreen = is_full_screen;
	
	return;
}

gchar* MedSLTPatWindow::GetText() const
{
	ACE_TRACE("[MedSLTPatWindow::GetText()]");
	
	if (GetUiState() != UI_STATE_CONNECTED)
	{
		return NULL;
	}
 		
	char* result = NULL;
	GtkTextBuffer* buf;
	GtkTextIter start, end;
		
	buf =  gtk_text_view_get_buffer(GTK_TEXT_VIEW(m_txtbox2));
	gtk_text_buffer_get_bounds(buf, &start, &end);
	result = gtk_text_buffer_get_text(buf,  &start, &end, FALSE);
	
	if (result == NULL)
	{	
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTPatWindow::GetText()] [NULL]\n")));
	}

	return result;
}

void MedSLTPatWindow::IterateList(gboolean forward)
{
	ACE_TRACE("[MedSLTPatWindow::IterateList()]");

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

void MedSLTPatWindow::SetListIndex(guint index)
{
	ACE_TRACE("[MedSLTPatWindow::SetListIndex()]");

	m_list_index = index; 
	
	return;
}

guint MedSLTPatWindow::GetListIndex() const
{
	ACE_TRACE("[MedSLTPatWindow::GetListIndex()]");

	return m_list_index; 
}

gchar* MedSLTPatWindow::GetRowData() const
{
	ACE_TRACE("[MedSLTPatWindow::GetRowData()]");
	
	if (GetUiState() != UI_STATE_CONNECTED)
	{
		return NULL;
	}
			
	char* result = NULL;
	
	gtk_clist_get_text(GTK_CLIST(m_list), m_list_index, 0, &result);
	
	if (result == NULL)
	{	
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTPatWindow::GetRowData()] [NULL]\n")));
	}
	
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTPatWindow::GetRowData()] "
				"[List Index: %d]\n"), m_list_index));
		
	return result;
}

void MedSLTPatWindow::SetBackTranslation(string& result)
{
	ACE_TRACE("[MedSLTPatWindow::SetBackTranslation()]");
	
	m_back_translation = result;
	
	return; 
}

void MedSLTPatWindow::SetTranslation(string& result) const
{
	ACE_TRACE("[MedSLTPatWindow::SetTranslation()]");
	
	GError *error = NULL;
	gchar* utf8 = NULL;
	
	// Get GTK thread lock
	gdk_threads_enter();
	
	GtkTextBuffer* buf;
			
	buf =  gtk_text_view_get_buffer(GTK_TEXT_VIEW(m_txtbox2));
		
	utf8 = g_convert(result.c_str(), -1, "UTF-8", "ISO-8859-1", NULL, NULL, &error);
	
	if (utf8 == NULL)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTPatWindow::SetTranslation()] "
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

void MedSLTPatWindow::SetHelpExamples(string& result)
{
	ACE_TRACE("[MedSLTPatWindow::SetHelpExamples()]");
	
	// Get GTK thread lock
	gdk_threads_enter();
	
	string buffer("");
	
	m_list_items = 0;
	
	if ((m_back_translation != "") && (m_back_translation != "error."))
	{
		GError *error = NULL;
		
		buffer = m_back_translation;
		gchar* item = g_convert(buffer.c_str(), -1, "UTF-8", "ISO-8859-1", NULL, NULL, &error);
		
		if (item == NULL)
		{
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTPatWindow::SetHelpExamples()] "
					"[Failed: %s]\n"), error->message));		
		}
		else
		{
			gtk_clist_append((GtkCList*)m_list, &item);
		}		
		
		g_free(item);
		g_free(error);
		++m_list_items;
	}
	
	buffer = "";
	
	while (result.size() > 0)
	{
		SplitHelpExamples(result, buffer, "_");
		
		GError *error = NULL;
		
		if ((buffer == "") || (buffer == "error."))
		{
			break;
		}
		
		if (buffer != m_back_translation)
		{
			gchar* item = g_convert(buffer.c_str(), -1, "UTF-8", "ISO-8859-1", NULL, NULL, &error);
			
			if (item == NULL)
			{
				ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTPatWindow::SetHelpExamples()] "
							"[Failed: %s]\n"), error->message));		
			}
			else
			{
				gtk_clist_append((GtkCList*)m_list, &item);
			}		
					
			g_free(item);			
			
			buffer = "";			
			++m_list_items;
		}		
		
		g_free(error);
	}
	
	if (m_list_items != 0)
	{
		gtk_clist_select_row((GtkCList*)m_list, 0, 0);
		SetListIndex(0);
	}
		
	gdk_flush();
	
	// Release GTK thread lock
	gdk_threads_leave();
	
	return; 
}

void MedSLTPatWindow::SetDoctorQuestion(string& result) const
{
	ACE_TRACE("[MedSLTPatWindow::SetDoctorQuestion()]");
	
	GError *error = NULL;
	gchar* utf8 = NULL;
	
	// Get GTK thread lock
	gdk_threads_enter();
	
	GtkTextBuffer* buf;
			
	buf =  gtk_text_view_get_buffer(GTK_TEXT_VIEW(m_txtbox1));
		
	utf8 = g_convert(result.c_str(), -1, "UTF-8", "ISO-8859-1", NULL, NULL, &error);
	
	if (utf8 == NULL)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTPatWindow::SetDoctorQuestion()] "
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

void MedSLTPatWindow::StartOfSpeech()
{
	ACE_TRACE("[MedSLTPatWindow::StartOfSpeech()]");

	// Get GTK thread lock
	gdk_threads_enter();
	
	SetUiState(UI_STATE_START_OF_SPEECH);
		
	gdk_flush();
	
	// Release GTK thread lock
	gdk_threads_leave();
	
	return; 
}

void MedSLTPatWindow::RecognitionCompleted() 
{
	ACE_TRACE("[MedSLTPatWindow::RecognitionCompleted()]");

	// Get GTK thread lock
	gdk_threads_enter();
	
	SetUiState(UI_STATE_ABORT_RECOGNIZE);
		
	gdk_flush();
	
	// Release GTK thread lock
	gdk_threads_leave();
	
	return; 
}

void MedSLTPatWindow::ResetTextList()
{
	ACE_TRACE("[MedSLTPatWindow::ResetTextList()]");
		
	gtk_clist_clear((GtkCList*)m_list);
	m_list_items = 0;
	
	m_back_translation = "";
	
	return;
}

void MedSLTPatWindow::ResetTextBox1() const
{
	ACE_TRACE("[MedSLTPatWindow::ResetTextBox1()]");
	
	GtkTextBuffer* buf;
	
	buf =  gtk_text_view_get_buffer(GTK_TEXT_VIEW(m_txtbox1));
	gtk_text_buffer_set_text(buf, "", -1);
		
	return;
}

void MedSLTPatWindow::ResetTextBox2() const
{
	ACE_TRACE("[MedSLTPatWindow::ResetTextBox2()]");
	
	GtkTextBuffer* buf;
		
	buf =  gtk_text_view_get_buffer(GTK_TEXT_VIEW(m_txtbox2));
	gtk_text_buffer_set_text(buf, "", -1);
	
	return;
}

void MedSLTPatWindow::ResetTextBoxes()
{
	ACE_TRACE("[MedSLTPatWindow::ResetTextBoxes()]");
	
	ResetTextList();
	ResetTextBox1();
	ResetTextBox2();
	
	return;
}

gboolean MedSLTPatWindow::CloseApp(GtkWidget* widget, 
									GdkEvent* event, 
									gpointer data)
{
	ACE_TRACE("[MedSLTPatWindow::CloseApp()]");
	
	MedSLTPatWindow* win = (MedSLTPatWindow*)data;
	
	if (win != NULL)
	{	
		(win->GetObserver()).QuitApp();
	}
	
	return TRUE;
}

gboolean MedSLTPatWindow::DeleteEvent(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTPatWindow::DeleteEvent()]");

	MedSLTPatWindow* win = (MedSLTPatWindow*)data;
	
	if (win != NULL)
	{
		(win->GetObserver()).QuitApp();
	}
	
	return TRUE;
}
void MedSLTPatWindow::RecognizeClicked(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTPatWindow::RecognizeClicked()]");

	MedSLTPatWindow* win = (MedSLTPatWindow*)data;
	
	if (win != NULL)
	{
		win->SetUiState(UI_STATE_START_RECOGNIZE);
		(win->GetObserver()).StartRecognize();
	}
	
	return;
}

void MedSLTPatWindow::AbortClicked(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTPatWindow::AbortClicked()]");

	MedSLTPatWindow* win = (MedSLTPatWindow*)data;

	if (win != NULL)
	{
		win->SetUiState(UI_STATE_ABORT_RECOGNIZE);
		(win->GetObserver()).AbortRecognize();
	}
	
	return;
}

void MedSLTPatWindow::UpClicked(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTPatWindow::UpClicked()]");
	
	MedSLTPatWindow* win = (MedSLTPatWindow*)data;
	
	if (win != NULL)
	{	
		win->IterateList(false);
	}
	
	return;
}

void MedSLTPatWindow::SelectClicked(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTPatWindow::SelectClicked()]");
	
	MedSLTPatWindow* win = (MedSLTPatWindow*)data;
	
	if (win != NULL)
	{
		(win->GetObserver()).GetTranslationFromHelp(win->GetRowData());
	}
	
	return;
}

void MedSLTPatWindow::DownClicked(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTPatWindow::DownClicked()]");
	
	MedSLTPatWindow* win = (MedSLTPatWindow*)data;
	
	if (win != NULL)
	{	
		win->IterateList(true);
	}
	
	return;
}

void MedSLTPatWindow::ConnectToRemote(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTPatWindow::ConnectToRemote()]");
	
	MedSLTPatWindow* win = (MedSLTPatWindow*)data;
		
	if (win != NULL)
	{	
		(win->GetObserver()).ConnectToRemote();	
		//win->SetUiState(UI_STATE_CONNECTED);		
	}	
	
	return;
}

void MedSLTPatWindow::DisconnectFromRemote(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTPatWindow::DisconnectFromRemote()]");
	
	MedSLTPatWindow* win = (MedSLTPatWindow*)data;
	
	if (win != NULL)
	{
		(win->GetObserver()).DisconnectFromRemote();
		win->SetUiState(UI_STATE_DISCONNECTED);		
	}

	return;
}

void MedSLTPatWindow::ShowAboutDialog(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTPatWindow::ShowAboutDialog()]");

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
void MedSLTPatWindow::ListRowClicked(GtkWidget* widget, gint row)
{
	ACE_TRACE("[MedSLTPatWindow::ListRowClicked()]");

	return;
}
									
void MedSLTPatWindow::ListRowSelected(GtkWidget* widget, gint row, gint column,
										GdkEventButton* event, gpointer data)
{
	ACE_TRACE("[MedSLTPatWindow::ListRowSelected()]");
	
	MedSLTPatWindow* win = (MedSLTPatWindow*)data;
	
	if (win != NULL)
	{
		win->SetListIndex(row);
		
		if (event != NULL)
		{
			if (event->type == GDK_2BUTTON_PRESS)
			{
				ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTPatWindow::ListRowSelected()] "
						"[List index: %d]\n"), win->GetListIndex()));
										
				(win->GetObserver()).GetTranslationFromHelp(win->GetRowData());
			}
		}
	}
	
	return;
}

bool MedSLTPatWindow::FindAndReplace(string &input_str,
									 string const &search_str, 
									 string const &replace_str) const
{
	string::size_type pos = 0;
	bool found = false;
	
	while ((pos = input_str.find(search_str, pos)) != string::npos)
	{
		input_str.replace(pos, search_str.size(), replace_str);
		pos = pos + replace_str.length();
		
		found = true;
	}
	
	return found;
}

void MedSLTPatWindow::SplitHelpExamples(string& buffer, 
										string& result, 
										const string token) const
{
	ACE_TRACE("[MedSLTPatWindow::SplitHelpExamples()]");

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
 	
 	FindAndReplace(buffer, rm_buff, "");

	result = rm_buff;
	FindAndReplace(result, token, "");
	
	return;
}

bool MedSLTPatWindow::ExtractOutput(string& buffer, 
									const string left, 
									const string right) const
{
	ACE_TRACE(ACE_TEXT("[MedSLTPatWindow::ExtractOutput()]"));
		
	string::size_type pos = 0;
	string replace_str("");
	char rm_buff[1000];
	
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
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTPatWindow::ExtractOutput()] "
			"[Ouput: %s]\n"), buffer.c_str()));				
#endif
	
	return true;
}
