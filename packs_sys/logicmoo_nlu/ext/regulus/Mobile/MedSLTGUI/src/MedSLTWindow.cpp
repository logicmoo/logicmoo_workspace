/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MedSLTWindow.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <stdio.h>
#include <sstream>

//#if defined(MAEMO2) || defined(MAEMO3)
//#include <hildon-widgets/hildon-program.h>
//#elif defined(MAEMO4)
#include <hildon/hildon-program.h>
//#endif

#include "MedSLTWindow.hpp"
#include "ace/Log_Msg.h"

MedSLTWindow::MedSLTWindow(MedSLTWindowObserver& observer, InputLang lang)
	: 	m_observer(observer),
		m_is_fullscreen(FALSE),
		m_back_translation(""),
		m_ui_state(UI_STATE_DISCONNECTED),
		m_wrap_position(5)
{
	ACE_TRACE("[MedSLTWindow::MedSLTWindow()]");
	
	GtkBox*					hbox;
	GtkBox*					coverbox;
	GtkBox*					textbox;
	GtkBox*					buthbox;
	GtkWidget*				menu;
	GtkWidget*				appimage;	
	GtkWidget*				scroll1;
	GtkWidget*				scroll2;	
	PangoFontDescription*	font1;
	PangoFontDescription*	font2;
	GtkAccelGroup*			accel = gtk_accel_group_new();
	GtkTreeViewColumn*		col;
	GtkCellRenderer*		renderer;	
	GtkTreeModel*			model;


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
	font1 = pango_font_description_from_string("Monospace Bold 25");
	font2 = pango_font_description_from_string("Monospace Bold 16");
	
	// Styles
	//GtkStyle* style;
	//style = gtk_style_new();
	//gdk_color_parse("red", &(style->fg[GTK_STATE_NORMAL]));
	//gdk_color_parse("orange", &(style->bg[GTK_STATE_NORMAL]));
	
	// TreeViews
	m_list = gtk_tree_view_new();
	gtk_widget_set_size_request(GTK_WIDGET(m_list), 550, 200);
		
	col = gtk_tree_view_column_new();
	
	gtk_tree_view_column_set_title(col, "First Name");

	// pack tree view column into tree view
	gtk_tree_view_append_column(GTK_TREE_VIEW(m_list), col);

	renderer = gtk_cell_renderer_text_new();

	// pack cell renderer into tree view column
	gtk_tree_view_column_pack_start(col, renderer, TRUE);

	// connect 'text' property of the cell renderer to
	// model column that contains the first name
	gtk_tree_view_column_add_attribute(col, renderer, "text", 0);
	
	// set 'cell-background' property of the cell renderer
	g_object_set(renderer,
					"height", 75,
					/*"cell-background", "Red",
					"cell-background-set", FALSE,*/
					NULL);

	model = CreateFillModel(lang);

	gtk_tree_view_set_model(GTK_TREE_VIEW(m_list), model);

	// destroy model automatically with view
	g_object_unref(model);
	
	gtk_tree_selection_set_mode(gtk_tree_view_get_selection(GTK_TREE_VIEW(m_list)),
								GTK_SELECTION_SINGLE);

	gtk_widget_modify_font(m_list, font1);
	gtk_tree_view_set_rules_hint(GTK_TREE_VIEW(m_list), TRUE);
		
/*	
	GdkColormap *colormap;
	GdkColor color;
	
	colormap = gtk_widget_get_colormap(m_list);
	color.red = color_map[0][0];
	color.green = color_map[0][1];
	color.blue = color_map[0][2];
	gdk_color_alloc(colormap, &color);
	gtk_clist_set_background(GTK_CLIST(m_list), 0, &color);

	color.red = color_map[1][0];
	color.green = color_map[1][1];
	color.blue = color_map[1][2];
	gdk_color_alloc(colormap, &color);
	gtk_clist_set_background(GTK_CLIST(m_list), 1, &color);
	
	color.red = color_map[2][0];
	color.green = color_map[2][1];
	color.blue = color_map[2][2];
	gdk_color_alloc(colormap, &color);
	gtk_clist_set_background(GTK_CLIST(m_list), 2, &color);
		
	gtk_clist_select_row((GtkCList*)m_list, 0, 0);
	*/
	
	// Text Boxes
	m_txtbox1 = gtk_text_view_new();
			
	gtk_widget_modify_font(m_txtbox1, font1);
		
	gtk_widget_set_size_request(GTK_WIDGET(m_txtbox1), 550, 50);
		
	gtk_text_view_set_editable(GTK_TEXT_VIEW(m_txtbox1), false);
		
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(m_txtbox1), GTK_WRAP_WORD_CHAR);
		
	// Scroll bars
	scroll1 = gtk_scrolled_window_new(NULL, NULL);
	scroll2 = gtk_scrolled_window_new(NULL, NULL);
	
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll1), GTK_POLICY_ALWAYS, GTK_POLICY_ALWAYS);
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
	gtk_widget_set_size_request(GTK_WIDGET(m_progressbar), 800, 50);	
			
	gtk_box_pack_start(textbox, m_understood, TRUE, TRUE, 0);
	gtk_box_pack_start(textbox, scroll1, TRUE, TRUE, 0);	
	gtk_box_pack_start(textbox, m_translation, TRUE, TRUE, 0);
	gtk_box_pack_start(textbox, scroll2, TRUE, TRUE, 0);
	
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
	g_signal_connect(G_OBJECT(m_list), "row-activated",
							G_CALLBACK(ListRowSelected), this);
	g_signal_connect(G_OBJECT(m_window), "destroy",
						G_CALLBACK(CloseApp), this);
	g_signal_connect(G_OBJECT(m_window), "delete_event",
						G_CALLBACK(CloseApp), this);

#ifdef _MAEMO
	g_signal_connect(G_OBJECT(m_window), "window_state_event",
							G_CALLBACK(WindowStateCb), this);
	g_signal_connect(G_OBJECT(m_window), "key_press_event",
						G_CALLBACK(KeyPressCb), this);
#endif
	
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
	//!FullScreen();

	return;
}

MedSLTWindow::~MedSLTWindow()
{
	ACE_TRACE("[MedSLTWindow::~MedSLTWindow()]");
			
	return;
}

GtkTreeModel* MedSLTWindow::CreateFillModel(InputLang lang)
{
	ACE_TRACE("[MedSLTWindow::CreateFillModel()]");
	
	GtkTreeStore*	treestore;
	GtkTreeIter		toplevel;

	treestore = gtk_tree_store_new(1, G_TYPE_STRING);

	// Append a top level row, and fill it with some data
	gtk_tree_store_append(treestore, &toplevel, NULL);
	gtk_tree_store_set(treestore, &toplevel, 0, 
						(lang == INPUT_LANG_FRE) ? "où avez vous mal?"
						: "where is the pain?", -1);

	// Append a second top level row, and fill it with some data
	gtk_tree_store_append(treestore, &toplevel, NULL);
	gtk_tree_store_set(treestore, &toplevel, 0, 
						(lang == INPUT_LANG_FRE) ? "avez vous mal sous le\ndevant de la tête?" 
						 : "is the pain in the\nfront of the head?", -1);

	// Append a third top level row, and fill it with some data
	gtk_tree_store_append(treestore, &toplevel, NULL);
	gtk_tree_store_set(treestore, &toplevel, 0, 
						(lang == INPUT_LANG_FRE) ? "la douleur survient elle le\nmatin?"
						: "do you have headaches in\nthe morning?", -1);
	
	return GTK_TREE_MODEL(treestore);
}
/*
void MedSLTWindow::CellDataFunc(GtkTreeViewColumn	*col,
								GtkCellRenderer   	*renderer,
								GtkTreeModel      	*model,
								GtkTreeIter       	*iter,
								gpointer           	user_data)
{
	ACE_TRACE("[MedSLTWindow::AgeCellDataFunc()]");
	
	guint  year_born;
	guint  year_now = 2003;
	gchar  buf[64];

	gtk_tree_model_get(model, iter, COL_YEAR_BORN, &year_born, -1);

	if (year_born <= year_now && year_born > 0)
	{
		guint age = year_now - year_born;
	
		g_snprintf(buf, sizeof(buf), "%u years old", age);
		g_object_set(renderer, "foreground-set", FALSE, NULL);
	}
	else
	{
		g_snprintf(buf, sizeof(buf), "age unknown");

		g_object_set(renderer, "foreground", "Red", "foreground-set", TRUE, NULL);
	}

	g_object_set(renderer, "text", buf, NULL);
	
	return;
}
*/
GtkWidget* MedSLTWindow::CreateMenu(GtkAccelGroup* accel)
{
	ACE_TRACE("[MedSLTWindow::CreateMenu()]");

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

gboolean MedSLTWindow::WindowStateCb(GtkWidget* widget, 
										 GdkEventWindowState* event, 
										 gpointer data)
{
	ACE_TRACE("[MedSLTWindow::WindowStateCb()]");
	
	MedSLTWindow* win = (MedSLTWindow*)data;
	
	if (win == NULL)
	{	
		return FALSE;
	}	
	else
	{
		win->SetFullScreen(event->new_window_state &GDK_WINDOW_STATE_FULLSCREEN);
	}
	
	return TRUE;
}

gboolean MedSLTWindow::KeyPressCb(GtkWidget* widget, 
									GdkEventKey* event,
									gpointer data)
{
	ACE_TRACE("[MedSLTWindow::KeyPressCb()]");

	g_assert(widget != NULL);
	g_assert(event != NULL);
	
	MedSLTWindow* win = (MedSLTWindow*)data;
		
	if (win == NULL)
	{	
		return FALSE;
	}

	switch (event->keyval) 
	{
		case GDK_Escape:
			(win->GetObserver()).ShowDlgHistory();
			break;
		case GDK_F6:
			if (win->IsFullScreen() == false)
			{
				win->Resize(false);
				win->FullScreen();				
			}
			else
			{
				win->Resize(true);
				win->UnfullScreen();				
			}
			break;
		case GDK_F7:
			win->ZoomIO(true);			
			break;
		case GDK_F8:
			win->ZoomIO(false);
			break;
		case GDK_Up:
			win->IterateList(false);
			break;
		case GDK_Down:
			win->IterateList(true);
			break;
		case GDK_Return:
			(win->GetObserver()).GetTranslationFromHelp(win->GetRowData());
			break;
		case GDK_Left:
			break;
		case GDK_Right:		
			break;
		case GDK_KP_Enter:
			if ((win->GetObserver()).IsConnected() == true)
			{
				if ((win->GetObserver()).GetRecInProgress() == false)
				{
					win->SetUiState(win->UI_STATE_START_RECOGNIZE);
					(win->GetObserver()).StartRecognize();
				}
				else
				{
					win->SetUiState(win->UI_STATE_ABORT_RECOGNIZE);
					(win->GetObserver()).AbortRecognize();
				}
			}
			break;
		default:
			return FALSE;						
	}

	return TRUE;
}

void MedSLTWindow::FullScreen()
{
	ACE_TRACE("[MedSLTWindow::FullScreen()]");
	
	gtk_window_fullscreen(m_window);
   
	return;
}
 
void MedSLTWindow::UnfullScreen()
{
	ACE_TRACE("[MedSLTWindow::UnfullScreen()]");
	
	gtk_window_unfullscreen(m_window);
   
	return;
}

MedSLTWindowObserver& MedSLTWindow::GetObserver() const
{
	ACE_TRACE("[MedSLTWindow::GetObserver()]");

	return m_observer;
}

GtkWindow* MedSLTWindow::GetWindow() const
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

void MedSLTWindow::Resize(gboolean is_full_screen)
{
	ACE_TRACE("[MedSLTWindow::Resize()]");
	
	m_wrap_position = 5;
	
	if (is_full_screen == true)
	{		
		gtk_widget_set_size_request(GTK_WIDGET(m_list), 550, 200);	
		gtk_widget_set_size_request(GTK_WIDGET(m_txtbox1), 550, 50);
		gtk_widget_set_size_request(GTK_WIDGET(m_progressbar), 800, 50);
	}
	else
	{
		gtk_widget_set_size_request(GTK_WIDGET(m_list), 600, 250);		
		gtk_widget_set_size_request(GTK_WIDGET(m_txtbox1), 600, 100);
		gtk_widget_set_size_request(GTK_WIDGET(m_progressbar), 800, 60);
	}
	
	return;
}

void MedSLTWindow::ZoomIO(gboolean zoom)
{
	ACE_TRACE("[MedSLTWindow::ZoomIO()]");
	
	//if (m_is_fullscreen == true)	
	{
		if (zoom == true)
		{
			m_wrap_position = 7;
			
			SetUiState(UI_STATE_ZOOM_IN);
			
			gtk_widget_set_size_request(GTK_WIDGET(m_list), 800, 270);		
			gtk_widget_set_size_request(GTK_WIDGET(m_txtbox1), 800, 130);
			gtk_widget_set_size_request(GTK_WIDGET(m_progressbar), 800, 80);
		}
		else
		{			
			m_wrap_position = 5;
			
			SetUiState(UI_STATE_ZOOM_OUT);
			
			gtk_widget_set_size_request(GTK_WIDGET(m_list), 600, 250);		
			gtk_widget_set_size_request(GTK_WIDGET(m_txtbox1), 600, 100);
			gtk_widget_set_size_request(GTK_WIDGET(m_progressbar), 800, 60);
		}
	}
	
	return;
}

void MedSLTWindow::SetUiState(UiState state)
{
	ACE_TRACE("[MedSLTWindow::SetUiState()]");
	
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
		case UI_STATE_ZOOM_IN:
			gtk_widget_hide(m_understood);
			gtk_widget_hide(m_translation);
			break;
		case UI_STATE_ZOOM_OUT:
			gtk_widget_show(m_understood);
			gtk_widget_show(m_translation);
			break;
		default:
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTWindow::SetUiState()] "
						"[Unknown ui state received: %d]\n"), state));
			break;
	}

	return;
}

MedSLTWindow::UiState MedSLTWindow::GetUiState() const
{
	ACE_TRACE("[MedSLTWindow::GetUiState()]");
	
	return m_ui_state;
}

gboolean MedSLTWindow::IsFullScreen() const
{
	ACE_TRACE("[MedSLTWindow::IsFullScreen()]");
	
	return m_is_fullscreen;
}

void MedSLTWindow::SetFullScreen(gboolean is_full_screen)
{
	ACE_TRACE("[MedSLTWindow::SetFullScreen()]");
	
	m_is_fullscreen = (is_full_screen == 0) ? false : true;
	
	return;
}

gchar* MedSLTWindow::GetText() const
{
	ACE_TRACE("[MedSLTWindow::GetText()]");
	
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
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTWindow::GetText()] [NULL]\n")));
	}
	
	// The varibele "result" should be freed later
	//g_free(result);
	
	return result;
}

void MedSLTWindow::IterateList(gboolean forward)
{
	ACE_TRACE("[MedSLTWindow::IterateList()]");

	if (GetUiState() != UI_STATE_CONNECTED)
	{
		return;
	}
	
	GtkTreeIter iter;
	GtkTreeSelection* selection;
    GtkTreeModel* model;
	GtkTreePath* path;
	
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(m_list));
	model = gtk_tree_view_get_model(GTK_TREE_VIEW(m_list));
	
	gtk_tree_selection_get_selected(selection, &model, &iter);
	path = gtk_tree_model_get_path(model, &iter);
	
	if (forward == true)
	{
		gtk_tree_path_next(path);
		gtk_tree_view_set_cursor(GTK_TREE_VIEW(m_list), path, NULL, FALSE);
		gtk_tree_path_free(path);
	}
	else
	{	
		if (gtk_tree_path_prev(path)) 
		{
			gtk_tree_view_set_cursor(GTK_TREE_VIEW(m_list), path, NULL, FALSE);
		}
		
		gtk_tree_path_free(path);
	}
	
	return;
}

gchar* MedSLTWindow::GetRowData() const
{
	ACE_TRACE("[MedSLTWindow::GetRowData()]");
	
	if (GetUiState() != UI_STATE_CONNECTED)
	{
		return NULL;
	}
	
	GtkTreeSelection*	selection;
	GtkTreeModel*		model;
	GtkTreeIter			iter;
	
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(m_list));
	
	if (gtk_tree_selection_get_selected(selection, &model, &iter))
	{
		gchar* result;
		
		gtk_tree_model_get(model, &iter, 0, &result, -1);

		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTWindow::GetRowData()] "
								"[selected row is: %s]\n"), result));

		// The varibele "name" should be freed later

		return result;
		
	}
	else
	{
		ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTWindow::GetRowData()] "
								"[no row selected.]\n")));
		
		return NULL;
	}
}

void MedSLTWindow::SetBackTranslation(string& result)
{
	ACE_TRACE("[MedSLTWindow::SetBackTranslation()]");
	
	m_back_translation = result;
	
	return; 
}

void MedSLTWindow::SetTranslation(string& result) const
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
	
	gdk_flush();
	
	// Release GTK thread lock
	gdk_threads_leave();
	
	return; 
}

void MedSLTWindow::SetHelpExamples(string& result)
{
	ACE_TRACE("[MedSLTWindow::SetHelpExamples()]");

	// Get GTK thread lock
	gdk_threads_enter();
	
	GtkTreeView*	treeview = GTK_TREE_VIEW(m_list);	
	GtkTreeModel*	model = gtk_tree_view_get_model(treeview);
	GtkTreeIter		iter;
	gboolean		selected = false;

	gtk_tree_store_clear(GTK_TREE_STORE(model));

	string buffer("");
					
	if ((m_back_translation != "") && (m_back_translation != "error."))
	{
		GError *error = NULL;
		
		buffer = m_back_translation;
		WrapText(buffer);
		buffer += "?";
		gchar* item = g_convert(buffer.c_str(), -1, "UTF-8", "ISO-8859-1", NULL, NULL, &error);
		
		if (item == NULL)
		{
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTWindow::SetHelpExamples()] "
					"[Failed: %s]\n"), error->message));		
		}
		else
		{	
			gtk_tree_store_append(GTK_TREE_STORE(model), &iter, NULL);
			gtk_tree_store_set(GTK_TREE_STORE(model), &iter, 0, item, -1);
			
			gtk_tree_selection_select_iter(gtk_tree_view_get_selection(GTK_TREE_VIEW(m_list)), &iter);
			
			selected = true;
		}		
		
		g_free(item);
		g_free(error);
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
			WrapText(buffer);
			buffer += "?";
			
			gchar* item = g_convert(buffer.c_str(), -1, "UTF-8", "ISO-8859-1", NULL, NULL, &error);
			
			if (item == NULL)
			{
				ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [MedSLTWindow::SetHelpExamples()] "
							"[Failed: %s]\n"), error->message));		
			}
			else
			{
				gtk_tree_store_append(GTK_TREE_STORE(model), &iter, NULL);
				gtk_tree_store_set(GTK_TREE_STORE(model), &iter, 0, item, -1);
				
				// If we don't have backtranslation
				if (selected == false)
				{
					gtk_tree_selection_select_iter(gtk_tree_view_get_selection(GTK_TREE_VIEW(m_list)), &iter);
					selected = true;
				}
			}		
					
			g_free(item);			
			
			buffer = "";
		}		
		
		g_free(error);
	}
	
	gdk_flush();
	
	// Release GTK thread lock
	gdk_threads_leave();
		
	return;
}

void MedSLTWindow::StartOfSpeech()
{
	ACE_TRACE("[MedSLTWindow::StartOfSpeech()]");

	// Get GTK thread lock
	gdk_threads_enter();
	
	SetUiState(UI_STATE_START_OF_SPEECH);
		
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

void MedSLTWindow::ResetTranslationBox()
{
	ACE_TRACE("[MedSLTWindow::ResetTranslationBox()]");
	
	GtkTextBuffer* buf1;
	
	buf1 =  gtk_text_view_get_buffer(GTK_TEXT_VIEW(m_txtbox1));
	gtk_text_buffer_set_text(buf1, "", -1);
	
	return;
}

void MedSLTWindow::ResetTextBoxes()
{
	ACE_TRACE("[MedSLTWindow::ResetTextBoxes()]");
	
	GtkTreeView*	treeview = GTK_TREE_VIEW(m_list);	
	GtkTreeModel*	model = gtk_tree_view_get_model(treeview);	

	gtk_tree_store_clear(GTK_TREE_STORE(model));

	ResetTranslationBox();
	
	m_back_translation = "";
	
	return;
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
									
void MedSLTWindow::ListRowSelected(GtkTreeView* treeview, GtkTreePath* path,
									GtkTreeViewColumn* col, gpointer userdata)
{
	ACE_TRACE("[MedSLTWindow::ListRowSelected()]");
	
	GtkTreeModel*	model;
	GtkTreeIter		iter;

	model = gtk_tree_view_get_model(treeview);
	
	if (gtk_tree_model_get_iter(model, &iter, path))
	{
	   gchar* result;
	
	   gtk_tree_model_get(model, &iter, 0, &result, -1);
	
		MedSLTWindow* win = (MedSLTWindow*)userdata;
		
		if (win != NULL)
		{
			(win->GetObserver()).GetTranslationFromHelp(result);
		}
	}

	return;
}

void MedSLTWindow::WrapText(string &str)
{
	ACE_TRACE("[MedSLTWindow::WrapText()]");

	string::size_type	pos = 0;
	int 				count = 0;
	
	while ((pos = str.find(" ", pos)) != string::npos)
	{	
		++count;

		if ((count % m_wrap_position) == 0)
		{
			str.replace(pos, 1, "\n");
			pos = pos + 2;
			
			break;
		}
		
		++pos;
	}
	
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
