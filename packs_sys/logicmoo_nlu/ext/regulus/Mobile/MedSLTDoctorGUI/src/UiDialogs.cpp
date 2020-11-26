/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	UiDialogs.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include "UiDialogs.hpp"
#include "ace/Log_Msg.h"

#include <hildon/hildon-program.h>
#include <hildon/hildon-banner.h>

UiDialogs::UiDialogs()
{
	ACE_TRACE("[UiDialogs::UiDialogs()]");
	
	return;
}

UiDialogs::~UiDialogs()
{
	ACE_TRACE("[UiDialogs::~UiDialogs()]");

	return;
}

void UiDialogs::FlushUiEvents(void)
{
	ACE_TRACE("[UiDialogs::FlushUiEvents()]");

	while (gtk_events_pending())
	{
		gtk_main_iteration();
	}
	
	return;
}

void UiDialogs::ShowDialog(GtkWindow* parent, const char* text, GtkMessageType type)
{
	ACE_TRACE("[UiDialogs::ShowDialog()]");

	g_return_if_fail(text != NULL);
	
	PangoFontDescription* font = pango_font_description_from_string("Monospace Bold 16");
	
	GtkDialogFlags flags = (GtkDialogFlags)(GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT);
	GtkWidget *dialog = gtk_message_dialog_new(parent, flags, type,
												GTK_BUTTONS_OK,
												"%s", text);
	gtk_widget_modify_font(dialog, font);
	gtk_dialog_run (GTK_DIALOG (dialog));
	gtk_widget_destroy (dialog);
	
	return;
}

void UiDialogs::InfoDialog(GtkWindow* parent, const char* text)
{
	ACE_TRACE("[UiDialogs::InfoDialog()]");

	ShowDialog(parent, text, GTK_MESSAGE_INFO);
	
	return;
}

void UiDialogs::WarningDialog(GtkWindow* parent, const char* text)
{
	ACE_TRACE("[UiDialogs::WarningDialog()]");

	ShowDialog(parent, text, GTK_MESSAGE_WARNING);
	
	return;
}

void UiDialogs::ErrorDialog(GtkWindow* parent, const char* text)
{
	ACE_TRACE("[UiDialogs::ErrorDialog()]");

	ShowDialog(parent, text, GTK_MESSAGE_ERROR);
	
	return;
}

void UiDialogs::InfoBanner(GtkWindow* parent, const char* text)
{
	ACE_TRACE("[UiDialogs::InfoBanner()]");

	g_return_if_fail(parent != NULL && text != NULL);
	
#ifdef MAEMO
	hildon_banner_show_information(GTK_WIDGET(parent), NULL, text);
#else
	// TODO: Implement a notification banner for Gnome
#endif

	return;
}

GtkDialog* UiDialogs::BaseDialog(GtkWindow* parent, const char* title, bool cancel)
{
	ACE_TRACE("[UiDialogs::BaseDialog()]");

	GtkWidget*				dialog;
	PangoFontDescription*	font = pango_font_description_from_string("Monospace Bold 16");
	
	dialog = gtk_dialog_new_with_buttons(title, parent,
										 (GtkDialogFlags)(GTK_DIALOG_MODAL |
										 GTK_DIALOG_DESTROY_WITH_PARENT),
										 GTK_STOCK_OK,
										 GTK_RESPONSE_ACCEPT,
										 (cancel == true) ? GTK_STOCK_CANCEL : 0,
										 (cancel == true) ? GTK_RESPONSE_REJECT: 0,
										 NULL);
	
	gtk_widget_modify_font(dialog, font);
	
	return GTK_DIALOG(dialog);
}

gboolean UiDialogs::ConfirmDialog(GtkWindow* parent, const char* text)
{
	ACE_TRACE("[UiDialogs::ConfirmDialog()]");
	
	g_return_val_if_fail(text != NULL, FALSE);
	
	gint					response;
	PangoFontDescription*	font = pango_font_description_from_string("Monospace Bold 16");
	GtkDialog*				dialog = BaseDialog(parent, "Confirmation", true);
	GtkWidget* 				label = gtk_label_new(text);
	
	gtk_widget_modify_font(label, font);
	gtk_box_pack_start(GTK_BOX(dialog->vbox), label, FALSE, FALSE, 10);
	gtk_widget_show_all(GTK_WIDGET(dialog));
	response = gtk_dialog_run(dialog);
	gtk_widget_destroy(GTK_WIDGET(dialog));
	
	return (response == GTK_RESPONSE_ACCEPT);
}

char* UiDialogs::InputDialog(GtkWindow* parent, const char* title,
								const char* text, const char* value)
{
	ACE_TRACE("[UiDialogs::InputDialog()]");

	GtkDialog*	dialog;
	GtkWidget*	label;
	GtkEntry*	entry;
	char*		retvalue = NULL;
	
	dialog = BaseDialog(parent, title, true);
	label = gtk_label_new(text);
	entry = GTK_ENTRY(gtk_entry_new());
	gtk_box_pack_start(GTK_BOX(dialog->vbox), label, FALSE, FALSE, 10);
	gtk_box_pack_start(GTK_BOX(dialog->vbox),
						GTK_WIDGET(entry), FALSE, FALSE, 10);

	if (value != NULL)
	{
		gtk_entry_set_text(entry, value);
	}
	
	gtk_widget_show_all(GTK_WIDGET(dialog));

	if (gtk_dialog_run(dialog) == GTK_RESPONSE_ACCEPT) 
	{
		retvalue = g_strstrip(g_strdup(gtk_entry_get_text(entry)));
	}
	
	gtk_widget_destroy(GTK_WIDGET(dialog));
	
	return retvalue;
}

void UiDialogs::ListDialog(GtkWindow* parent, const char* title, vector<string>& strlist)
{
	ACE_TRACE("[UiDialogs::ListDialog()]");

	GtkDialog*	dialog;	
	GtkWidget*	list;
	GdkColor	color;
		
	dialog = BaseDialog(parent, title, false);	
	list =  gtk_clist_new(1);
	gtk_widget_set_size_request(GTK_WIDGET(list), 550, 280);
	
	GdkColormap *colormap = gtk_widget_get_colormap(list);	
	
	for (unsigned int i = 0; i < strlist.size(); ++i)
	{
		GError *error = NULL;		
		gchar* item = g_convert(strlist[i].c_str(), -1, "UTF-8", "ISO-8859-1", NULL, NULL, &error);
		
		if (item == NULL)
		{
			ACE_DEBUG((LM_DEBUG, ACE_TEXT("%I[DEBUG] [UiDialogs::ListDialog()] "
					"[Failed: %s]\n"), error->message));		
		}
		else
		{	
			gtk_clist_append((GtkCList*)list, &item);
			
			if (strlist[i].find("REC:") == 0)
			{
				color.red = color_map[0][0];
				color.green = color_map[0][1];
				color.blue = color_map[0][2];
			}
			else if (strlist[i].find("TRANS:") == 0)
			{
				color.red = color_map[1][0];
				color.green = color_map[1][1];
				color.blue = color_map[1][2];
			}
			else
			{
				color.red = color_map[2][0];
				color.green = color_map[2][1];
				color.blue = color_map[2][2];
			}
			
			gdk_color_alloc(colormap, &color);
			gtk_clist_set_background(GTK_CLIST(list), i, &color);			
		}		
		
		g_free(item);
		g_free(error);
	}
	
	GtkWidget* scroll = gtk_scrolled_window_new(NULL, NULL);
			
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll), GTK_POLICY_ALWAYS, GTK_POLICY_ALWAYS);			
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scroll), GTK_SHADOW_IN);
	
	gtk_container_add(GTK_CONTAINER(scroll), GTK_WIDGET(list));	
	
	gtk_box_pack_start(GTK_BOX(dialog->vbox),
							GTK_WIDGET(scroll), TRUE, TRUE, 10);
	
	gtk_widget_show_all(GTK_WIDGET(dialog));

	gtk_dialog_run(dialog);
	
	gtk_widget_destroy(GTK_WIDGET(dialog));
	
	return;
}
