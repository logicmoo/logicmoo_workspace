/*
 * Copyright 2007-2008 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MedSLTWindow.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@issco.unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <stdio.h>

//#if defined(MAEMO2) || defined(MAEMO3)
#include <hildon-widgets/hildon-program.h>
//#elif defined(MAEMO4)
//#include <hildon/hildon-program.h>
//#endif

#include "MedSLTWindow.hpp"
#include "ace/Log_Msg.h"

#ifdef MAEMO
static const int album_cover_size = 3;
#else
static const int album_cover_size = 3;
#endif

MedSLTWindow::MedSLTWindow(MedSLTWindowObserver& observer)
	: 	m_observer(observer),
		m_list_index(0),
		m_list_items(0),
		m_is_fullscreen(FALSE),
		m_back_translation(""),
		m_ui_state(UI_STATE_DISCONNECTED)		
{
	ACE_TRACE("[MedSLTWindow::MedSLTWindow()]");

	GtkBox* hbox;
	//GtkBox* hbox2; // deprecated	
	GtkBox* coverbox;
	GtkBox* textbox;
	GtkBox* buthbox;
	GtkWidget* menu;
	GtkWidget* appimage;	
	GtkWidget* scroll1;
	GtkWidget* scroll2;	
	PangoFontDescription* font1;
	PangoFontDescription* font2;	
	GtkStyle*		style; 
	//GtkTextBuffer* buf1;
	//GtkTextBuffer* buf2;
	GtkAccelGroup *accel = gtk_accel_group_new();

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
	font1 = pango_font_description_from_string("Monospace 20");
	font2 = pango_font_description_from_string("Monospace 16");
	
	// Styles
	//style = gtk_style_new();
	//gdk_color_parse ("orange", &(style->fg[GTK_STATE_NORMAL]));
	
	// Lists
	m_list = gtk_clist_new(1);
	//m_list = gtk_list_store_new();
	 
	gtk_widget_set_size_request(GTK_WIDGET(m_list), 550, 200);
	gtk_widget_modify_font(m_list, font1);
	
	//gtk_clist_set_row_style(GTK_CLIST(m_list), 1, style);
	//gtk_widget_set_style (m_list, style);
		
	gchar* text[3][1] = {	{"where is the pain?"},
							{"is the pain in the front of the head?"},
							/*{"is the pain severe?"},*/
							{"do you have headaches in the morning?"}};	
	
	gtk_clist_append((GtkCList*)m_list, text[0]);
	gtk_clist_append((GtkCList*)m_list, text[1]);
	gtk_clist_append((GtkCList*)m_list, text[2]);
	
	m_list_items = 3;

	/*GtkStyle* style1 = gtk_style_new();
	
	GdkColor col1;
	GdkColor col2;

	col1.red   = 0;
	col1.green = 56000;
	col1.blue  = 0;
 	col2.red   = 32000;
	col2.green = 0;
	col2.blue  = 56000;

	style1 = gtk_style_copy (GTK_WIDGET (m_list)->style);
	style1->base[GTK_STATE_NORMAL] = col1;
	style1->base[GTK_STATE_SELECTED] = col2;
	style1->font_desc = font1;
		
	gtk_clist_set_cell_style(GTK_CLIST (m_list), 0, 0, style1);*/	
	
	gtk_clist_select_row((GtkCList*)m_list, 0, 0);
	
	// Text Boxes
	m_txtbox1 = gtk_text_view_new();
			
	gtk_widget_modify_font(m_txtbox1, font1);
		
	gtk_widget_set_size_request(GTK_WIDGET(m_txtbox1), 550, 50);
		
	gtk_text_view_set_editable(GTK_TEXT_VIEW(m_txtbox1), false);
		
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(m_txtbox1), GTK_WRAP_WORD_CHAR);
	
	//buf1 =  gtk_text_view_get_buffer(GTK_TEXT_VIEW(m_txtbox1));
	//buf2 =  gtk_text_view_get_buffer(GTK_TEXT_VIEW(m_txtbox2));   
	    
	//gtk_text_buffer_set_text(buf1, "When is the next meeting?", -1);		 
	//gtk_text_buffer_set_text(buf2, "Meeting and Pierrette's room on September 3rd 9:00 am at IDIAP.", -1);
		
	// Scroll bars
	scroll1 = gtk_scrolled_window_new(NULL, NULL);
	scroll2 = gtk_scrolled_window_new(NULL, NULL);
	
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll1), GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll2), GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);
	
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scroll1), GTK_SHADOW_IN);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scroll2), GTK_SHADOW_IN);
	
	gtk_container_add(GTK_CONTAINER(scroll1), GTK_WIDGET(m_list));
	gtk_container_add(GTK_CONTAINER(scroll2), GTK_WIDGET(m_txtbox1));
			
	// Text labels
	m_understood = gtk_label_new("Understood");	
	m_translation = gtk_label_new("Translation");

	gtk_widget_modify_font(m_understood, font2);	
	gtk_widget_modify_font(m_translation, font2);
	
	gtk_label_set_justify(GTK_LABEL(m_understood), GTK_JUSTIFY_LEFT);	
	gtk_label_set_justify(GTK_LABEL(m_translation), GTK_JUSTIFY_LEFT);
		
	gtk_label_set_ellipsize(GTK_LABEL(m_understood), PANGO_ELLIPSIZE_END);	
	gtk_label_set_ellipsize(GTK_LABEL(m_translation), PANGO_ELLIPSIZE_END);
		
	// Buttons
	m_rec = gtk_button_new();
	m_abort = gtk_button_new();
	m_up = gtk_button_new();
	m_select = gtk_button_new();
	m_down = gtk_button_new();
	
	gtk_button_set_image(GTK_BUTTON(m_rec),
							gtk_image_new_from_file(record_icon));
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
	gtk_box_pack_start(textbox, m_translation, TRUE, TRUE, 0);
	gtk_box_pack_start(textbox, scroll2, TRUE, TRUE, 0);
	
	//gtk_box_pack_start(buthbox, appimage, TRUE, TRUE, 0);
	gtk_box_pack_start(buthbox, m_rec, TRUE, TRUE, 0);
	gtk_box_pack_start(buthbox, m_abort, TRUE, TRUE, 0);
	gtk_box_pack_start(buthbox, m_up, TRUE, TRUE, 0);
	gtk_box_pack_start(buthbox, m_select, TRUE, TRUE, 0);
	gtk_box_pack_start(buthbox, m_down, TRUE, TRUE, 0);
	
	gtk_box_pack_start(hbox, GTK_WIDGET(textbox), TRUE, TRUE, 0);
	gtk_box_pack_start(hbox, GTK_WIDGET(buthbox), TRUE, TRUE, 0);
	
	gtk_box_pack_start(coverbox, GTK_WIDGET(hbox), TRUE, TRUE, 0);
	gtk_box_pack_start(coverbox, m_progressbar, TRUE, TRUE, 0);
	
	//gtk_container_add(GTK_CONTAINER(hbox2), appimage);
	//gtk_container_add(GTK_CONTAINER(hbox2), GTK_WIDGET(coverbox));
	//gtk_container_add(GTK_CONTAINER(m_window), GTK_WIDGET(hbox2));	
	gtk_container_add(GTK_CONTAINER(m_window), GTK_WIDGET(coverbox));
	
#ifdef _MAEMO
	hildon_window_set_menu(HILDON_WINDOW(m_window), GTK_MENU(menu));	
#else
	gtk_box_pack_start(vbox, menu, FALSE, FALSE, 0);
#endif

	// Signals
	g_signal_connect(G_OBJECT(m_rec), "clicked", 
						G_CALLBACK(RecognizeClicked), this);
	g_signal_connect(G_OBJECT(m_abort), "clicked",
						G_CALLBACK(AbortClicked), this);
	g_signal_connect(G_OBJECT(m_up), "clicked",
						G_CALLBACK(UpClicked), this);
	g_signal_connect(G_OBJECT(m_select), "clicked",
						G_CALLBACK(SelectClicked), this);
	g_signal_connect(G_OBJECT(m_down), "clicked",
						G_CALLBACK(DownClicked), this);
//	g_signal_connect(G_OBJECT(m_list), "click_column",
//						G_CALLBACK(ListRowClicked), NULL);
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
	gtk_widget_add_accelerator(m_abort, "clicked", accel, GDK_space,
								0, 0);
	gtk_widget_add_accelerator(m_next, "clicked", accel, GDK_Right,
								GDK_CONTROL_MASK, 0);
#endif

	// Initial state
	SetUiState(UI_STATE_DISCONNECTED);
	
	return;
}

MedSLTWindow::~MedSLTWindow()
{
	ACE_TRACE("[MedSLTWindow::~MedSLTWindow()]");
			
	return;
}    

GtkWidget* MedSLTWindow::CreateMenu(GtkAccelGroup* accel)
{
	ACE_TRACE("[MedSLTWindow::CreateMenu()]");

	GtkMenuItem* connection;
	GtkMenuShell* connectionsub;
	GtkMenuItem* help;
	GtkMenuShell* helpsub;
	GtkWidget* connect;
	GtkWidget* disconnect;
	GtkWidget* quit;
	GtkWidget* about;
	PangoFontDescription* font;

#ifdef _MAEMO
	GtkMenuShell* bar = GTK_MENU_SHELL(gtk_menu_new());
#else
	GtkMenuShell* bar = GTK_MENU_SHELL(gtk_menu_bar_new());
#endif

	font = pango_font_description_from_string("Monospace 16");
	
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

#if 0
	// Radio
	radio = GTK_MENU_ITEM(gtk_menu_item_new_with_mnemonic("Play _Radio"));
	radiosub = GTK_MENU_SHELL(gtk_menu_new());
	user = GTK_MENU_ITEM(gtk_menu_item_new_with_label("My radios"));
	others = GTK_MENU_ITEM(gtk_menu_item_new_with_label(
							"Others' radios"));
	group = gtk_menu_item_new_with_label("Group radio...");
	globaltag = gtk_menu_item_new_with_label("Music tagged...");
	similarartist = gtk_menu_item_new_with_label("Artists similar to...");
	urlradio = gtk_menu_item_new_with_label("Enter URL...");
	gtk_menu_shell_append(bar, GTK_WIDGET(radio));
	gtk_menu_item_set_submenu(radio, GTK_WIDGET(radiosub));
	gtk_menu_shell_append(radiosub, GTK_WIDGET(user));
	gtk_menu_shell_append(radiosub, GTK_WIDGET(others));
	gtk_menu_shell_append(radiosub, group);
	gtk_menu_shell_append(radiosub, globaltag);
	gtk_menu_shell_append(radiosub, similarartist);
	gtk_menu_shell_append(radiosub, urlradio);
	g_signal_connect(G_OBJECT(group), "activate",
						G_CALLBACK(GroupRadioSelected), NULL);

	// Radio -> My radios
	usersub = GTK_MENU_SHELL(gtk_menu_new());
	gtk_menu_item_set_submenu(user, GTK_WIDGET(usersub));
	personal = gtk_menu_item_new_with_label("My personal radio");
	neigh = gtk_menu_item_new_with_label("My neighbours");
	loved = gtk_menu_item_new_with_label("My loved tracks");
	playlist = gtk_menu_item_new_with_label("My playlist");
	recomm = gtk_menu_item_new_with_label("My recommendations");
	usertag = gtk_menu_item_new_with_label("My music tagged...");
	gtk_menu_shell_append(usersub, personal);
	gtk_menu_shell_append(usersub, neigh);
	gtk_menu_shell_append(usersub, loved);
	gtk_menu_shell_append(usersub, playlist);
	gtk_menu_shell_append(usersub, recomm);
	gtk_menu_shell_append(usersub, usertag);

	// Radio -> Others' radios
	othersub = GTK_MENU_SHELL(gtk_menu_new());
	gtk_menu_item_set_submenu(others, GTK_WIDGET(othersub));
	personal2 = gtk_menu_item_new_with_label("Personal...");
	neigh2 = gtk_menu_item_new_with_label("Neighbours...");
	loved2 = gtk_menu_item_new_with_label("Loved tracks...");
	playlist2 = gtk_menu_item_new_with_label("Playlist...");
	gtk_menu_shell_append(othersub, personal2);
	gtk_menu_shell_append(othersub, neigh2);
	gtk_menu_shell_append(othersub, loved2);
	gtk_menu_shell_append(othersub, playlist2);

	// Actions
	actions = GTK_MENU_ITEM(gtk_menu_item_new_with_mnemonic("_Actions"));
	actionssub = GTK_MENU_SHELL(gtk_menu_new());
	tagsub = GTK_MENU_SHELL(gtk_menu_new());
	dorecommsub = GTK_MENU_SHELL(gtk_menu_new());
	love = gtk_menu_item_new_with_label("Love this track");
	ban = gtk_menu_item_new_with_label("Ban this track");
	addtopls = gtk_menu_item_new_with_label("Add to playlist");
	dload = gtk_menu_item_new_with_label("Download this track");
	tag = gtk_menu_item_new_with_label("Tag");
	dorecomm = gtk_menu_item_new_with_label("Recommend");
	tagartist = gtk_menu_item_new_with_label("This artist...");
	tagtrack = gtk_menu_item_new_with_label("This track...");
	tagalbum = gtk_menu_item_new_with_label("This album...");
	recommartist = gtk_menu_item_new_with_label("This artist...");
	recommtrack = gtk_menu_item_new_with_label("This track...");
	recommalbum = gtk_menu_item_new_with_label("This album...");
	gtk_menu_shell_append(bar, GTK_WIDGET(actions));
	gtk_menu_item_set_submenu(actions, GTK_WIDGET(actionssub));
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(tag), GTK_WIDGET(tagsub));
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(dorecomm),
								GTK_WIDGET(dorecommsub));
	gtk_menu_shell_append(actionssub, love);
	gtk_menu_shell_append(actionssub, ban);
	gtk_menu_shell_append(actionssub, addtopls);
	gtk_menu_shell_append(actionssub, dload);
	gtk_menu_shell_append(actionssub, tag);
	gtk_menu_shell_append(actionssub, dorecomm);
	gtk_menu_shell_append(tagsub, tagartist);
	gtk_menu_shell_append(tagsub, tagtrack);
	gtk_menu_shell_append(tagsub, tagalbum);
	gtk_menu_shell_append(dorecommsub, recommartist);
	gtk_menu_shell_append(dorecommsub, recommtrack);
	gtk_menu_shell_append(dorecommsub, recommalbum);

#endif

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

gboolean MedSLTWindow::IsFullScreen()
{
	ACE_TRACE("[MedSLTWindow::IsFullScreen()]");

	return m_is_fullscreen;
}

void MedSLTWindow::SetFullScreen(gboolean is_full_screen)
{
	ACE_TRACE("[MedSLTWindow::SetFullScreen()]");

	m_is_fullscreen = is_full_screen;
	
	return;
}

MedSLTWindowObserver& MedSLTWindow::GetObserver()
{
	ACE_TRACE("[MedSLTWindow::GetObserver()]");

	return m_observer;
}

gboolean MedSLTWindow::CloseApp(GtkWidget* widget, 
								GdkEvent* event, 
								gpointer data)
{
	ACE_TRACE("[MedSLTWindow::CloseApp()]");
	
	MedSLTWindow* win = (MedSLTWindow*)data;
	
	if (win != NULL)
	{	
		(win->GetObserver()).QuitApp();
	}
	
	return TRUE;
}

gboolean MedSLTWindow::DeleteEvent(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTWindow::DeleteEvent()]");

	MedSLTWindow* win = (MedSLTWindow*)data;
	
	if (win != NULL)
	{
		(win->GetObserver()).QuitApp();
	}
	
	return TRUE;
}

void MedSLTWindow::RecognizeClicked(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTWindow::RecognizeClicked()]");

	MedSLTWindow* win = (MedSLTWindow*)data;
	
	if (win != NULL)
	{
		win->SetUiState(UI_STATE_START_RECOGNIZE);
		(win->GetObserver()).StartRecognize();
	}
	
	return;
}

void MedSLTWindow::AbortClicked(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTWindow::AbortClicked()]");

	MedSLTWindow* win = (MedSLTWindow*)data;

	if (win != NULL)
	{
		win->SetUiState(UI_STATE_ABORT_RECOGNIZE);
		(win->GetObserver()).AbortRecognize();
	}
	
	return;
}

void MedSLTWindow::UpClicked(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTWindow::UpClicked()]");
	
	MedSLTWindow* win = (MedSLTWindow*)data;
	
	if (win != NULL)
	{	
		win->IterateList(false);
	}
	
	return;
}

void MedSLTWindow::SelectClicked(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTWindow::SelectClicked()]");
	
	MedSLTWindow* win = (MedSLTWindow*)data;
	
	if (win != NULL)
	{		
		(win->GetObserver()).GetTranslationFromHelp(win->GetRowData());
	}
	
	return;
}

void MedSLTWindow::DownClicked(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTWindow::DownClicked()]");
	
	MedSLTWindow* win = (MedSLTWindow*)data;
	
	if (win != NULL)
	{	
		win->IterateList(true);
	}
	
	return;
}

void MedSLTWindow::Resize(gboolean is_full_screen)
{
	ACE_TRACE("[MedSLTWindow::Resize()]");
	
	if (is_full_screen == true)
	{ 
		gtk_widget_set_size_request(GTK_WIDGET(m_list), 550, 200);		
		gtk_widget_set_size_request(GTK_WIDGET(m_txtbox1), 550, 50);
	}
	else
	{
		gtk_widget_set_size_request(GTK_WIDGET(m_list), 600, 270);		
		gtk_widget_set_size_request(GTK_WIDGET(m_txtbox1), 600, 60);		
	}
	
	return;
}

void MedSLTWindow::ConnectToRemote(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTWindow::ConnectToRemote()]");
	
	MedSLTWindow* win = (MedSLTWindow*)data;
		
	if (win != NULL)
	{	
		(win->GetObserver()).ConnectToRemote();	
		//win->SetUiState(UI_STATE_CONNECTED);		
	}	
	
	return;
}

void MedSLTWindow::DisconnectFromRemote(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTWindow::DisconnectFromRemote()]");
	
	MedSLTWindow* win = (MedSLTWindow*)data;
	
	if (win != NULL)
	{
		(win->GetObserver()).DisconnectFromRemote();
		win->SetUiState(UI_STATE_DISCONNECTED);		
	}

	return;
}

void MedSLTWindow::ShowAboutDialog(GtkWidget* widget, gpointer data)
{
	ACE_TRACE("[MedSLTWindow::ShowAboutDialog()]");

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
void MedSLTWindow::ListRowClicked(GtkWidget* widget, gint row)
{
	ACE_TRACE("[MedSLTWindow::ListRowClicked()]");

	return;
}
									
void MedSLTWindow::ListRowSelected(GtkWidget* widget, gint row, gint column,
										GdkEventButton* event, gpointer data)
{
	ACE_TRACE("[MedSLTWindow::ListRowSelected()]");
	
	MedSLTWindow* win = (MedSLTWindow*)data;
	
	if (win != NULL)
	{
		win->SetListIndex(row);
		
		if (event != NULL)
		{
			if (event->type == GDK_2BUTTON_PRESS)
			{
				ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTWindow::ListRowSelected()] "
						"[List index: %d]\n"), win->GetListIndex()));
										
				(win->GetObserver()).GetTranslationFromHelp(win->GetRowData());
			}
		}
	}
	
	return;
}

void MedSLTWindow::SetUiState(UiState state)
{
	ACE_TRACE("[MedSLTWindow::SetUiState()]");
	
	// Get GTK thread lock
//	gdk_threads_enter();
		
	switch (state) 
	{
		case UI_STATE_DISCONNECTED:			
					
			gtk_progress_bar_set_text(GTK_PROGRESS_BAR(m_progressbar),
										"Status:DISCONNECTED");			
			gtk_widget_show(m_rec);
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
		case UI_STATE_START_RECOGNIZE:
			gtk_progress_bar_set_text(GTK_PROGRESS_BAR(m_progressbar),
										"Status:START_RECOGNIZE");
			gtk_widget_hide(m_rec);
			gtk_widget_show(m_abort);
			gtk_widget_set_sensitive(m_rec, FALSE);
			m_ui_state = UI_STATE_START_RECOGNIZE;					
			break;
		case UI_STATE_ABORT_RECOGNIZE:
			gtk_progress_bar_set_text(GTK_PROGRESS_BAR(m_progressbar),
										"Status:ABORT_RECOGNIZE");
			gtk_widget_show(m_rec);
			gtk_widget_hide(m_abort);
			gtk_widget_set_sensitive(m_rec, TRUE);
			m_ui_state = UI_STATE_CONNECTED;
		break;
		default:
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTWindow::SetUiState()] "
						"[Unknown ui state received: %d]\n"), state));
			//g_critical("Unknown ui state received: %d", state);
			break;
	}

	return;
}

MedSLTWindow::UiState MedSLTWindow::GetUiState()
{
	ACE_TRACE("[MedSLTWindow::GetUiState()]");
	
	return m_ui_state;
}

GtkWindow* MedSLTWindow::GetWindow()
{
	ACE_TRACE("[MedSLTWindow::GetWindow()]");

	if (m_window != NULL)
	{	
		return m_window;
	}
	else
	{	
		return NULL;
	}
}

gchar* MedSLTWindow::GetText()
{
	ACE_TRACE("[MedSLTWindow::GetText()]");
	
	if (GetUiState() != UI_STATE_CONNECTED)
	{
		return NULL;
	}
	/*
	int length = buffer.copy(rm_buff, pos, 0);
	char buffer[150];
	
	if (length >= 150)
 	{
 		length = 150 - 1;
 	}
 	 	
 	buffer[length] = '\0';*/
 	
 		
	char* result = NULL;
	GtkTextBuffer* buf;
	GtkTextIter start, end;
		
	buf =  gtk_text_view_get_buffer(GTK_TEXT_VIEW(m_txtbox1));
	gtk_text_buffer_get_bounds(buf, &start, &end);
	result = gtk_text_buffer_get_text(buf,  &start, &end, FALSE);
	
	if (result == NULL)
	{	
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTWindow::GetText()] [NULL]\n")));
	}

	return result;
}

void MedSLTWindow::IterateList(gboolean forward)
{
	ACE_TRACE("[MedSLTWindow::GetWindow()]");

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

void MedSLTWindow::SetListIndex(guint index)
{
	ACE_TRACE("[MedSLTWindow::SetListIndex()]");

	m_list_index = index; 
	
	return;
}

guint MedSLTWindow::GetListIndex()
{
	ACE_TRACE("[MedSLTWindow::GetListIndex()]");

	return m_list_index; 
}

gchar* MedSLTWindow::GetRowData()
{
	ACE_TRACE("[MedSLTWindow::GetRowData()]");
	
	if (GetUiState() != UI_STATE_CONNECTED)
	{
		return NULL;
	}
			
	char* result = NULL;
	
	//result  = (gchar*)gtk_clist_get_row_data((GtkCList*)m_list, m_list_index);	
	gtk_clist_get_text(GTK_CLIST(m_list), m_list_index, 0, &result);
	
	if (result == NULL)
	{	
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTWindow::GetRowDataHelp()] [NULL]\n")));
	}
	
	ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTWindow::GetRowDataHelp()] "
				"[List Index: %d]\n"), m_list_index));
		
	return result;
}

void MedSLTWindow::SetTranslation(string& result)
{
	ACE_TRACE("[MedSLTWindow::SetTranslation()]");
	
	GError *error = NULL;
	gchar* utf8 = NULL;
	
	// Get GTK thread lock
	gdk_threads_enter();
	
	GtkTextBuffer* buf;
			
	buf =  gtk_text_view_get_buffer(GTK_TEXT_VIEW(m_txtbox1));
		
	utf8 = g_convert(result.c_str(), -1, "UTF-8", "ISO-8859-1", NULL, NULL, &error);
	
	if (utf8 == NULL)
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTWindow::SetTranslation()] "
				"[Failed: %s]\n"), error->message));		
	}
	else
	{
		gtk_text_buffer_set_text(buf, utf8, -1);
	}
	
	g_free(utf8);
	g_free(error);
	
	//gtk_text_buffer_set_text(buf, result.c_str(), -1);

	gdk_flush();
	
	// Release GTK thread lock
	gdk_threads_leave();
	
	return; 
}

void MedSLTWindow::SetBackTranslation(string& result)
{
	ACE_TRACE("[MedSLTWindow::SetBackTranslation()]");
	
	m_back_translation = result;
	
	return; 
}

void MedSLTWindow::SetHelpExamples(string& result)
{
	ACE_TRACE("[MedSLTWindow::SetHelpExamples()]");

	// Get GTK thread lock
	gdk_threads_enter();
	
//	gtk_clist_clear((GtkCList*)m_list);
	
	string buffer("");
	
	m_list_items = 0;

	if ((m_back_translation != "") && (m_back_translation != "error."))
	{
		buffer = m_back_translation + "?";
		gchar* item = g_strdup(buffer.c_str());
		gtk_clist_append((GtkCList*)m_list, &item);
		free(item);
		++m_list_items;
	}
	
	buffer = "";
	
	while (result.size() > 0)
	{
		SplitHelpExamples(result, buffer, "-");
		
		if ((buffer != "") && (buffer != "error.") 
			&& (buffer != m_back_translation))
		{
			buffer += "?";	
			gchar* item = g_strdup(buffer.c_str());
			gtk_clist_append((GtkCList*)m_list, &item);
			free(item);	
			++m_list_items;
		}		
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

void MedSLTWindow::RecognitionCompleted()
{
	ACE_TRACE("[MedSLTWindow::RecognitionCompleted()]");

	// Get GTK thread lock
	gdk_threads_enter();
	
	SetUiState(UI_STATE_ABORT_RECOGNIZE);
		
	gdk_flush();
	
	// Release GTK thread lock
	gdk_threads_leave();
	
	return; 
}

void MedSLTWindow::ResetTextBoxes()
{
	ACE_TRACE("[MedSLTWindow::ResetTextBoxes()]");
	
	GtkTextBuffer* buf1;
	
	buf1 =  gtk_text_view_get_buffer(GTK_TEXT_VIEW(m_txtbox1));
	gtk_text_buffer_set_text(buf1, "", -1);
		
	gtk_clist_clear((GtkCList*)m_list);
	m_list_items = 0;
	
	m_back_translation = "";
	
	return;
}

void MedSLTWindow::FindAndReplace(string &input_str,
									string const &search_str, 
									string const &replace_str)
{
	ACE_TRACE("[MedSLTWindow::FindAndReplace()]");

	string::size_type pos = 0;
	
	while ((pos = input_str.find(search_str, pos)) != string::npos)
	{		
		input_str.replace(pos, search_str.size(), replace_str);
		pos = pos + replace_str.length();
	}
	
	return;
}

void MedSLTWindow::SplitHelpExamples(string& buffer, 
						string& result, const string token)
{
	ACE_TRACE("[MedSLTWindow::SplitHelpExamples()]");

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
