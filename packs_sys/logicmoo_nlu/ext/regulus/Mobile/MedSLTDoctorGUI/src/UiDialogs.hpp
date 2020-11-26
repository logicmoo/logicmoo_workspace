/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	UiDialogs.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#ifndef UIDIALOGS_HPP_
#define UIDIALOGS_HPP_

#include <gtk/gtk.h>
#include <string>
#include <vector>
#include "GlobalDefs.hpp"

using namespace std;

class UiDialogs
{
public:
	
	UiDialogs();
	~UiDialogs();
	
	void InfoBanner(GtkWindow* parent, const char* text);
	void InfoDialog(GtkWindow* parent, const char* text);
	void WarningDialog(GtkWindow* parent, const char* text);
	void ErrorDialog(GtkWindow* parent, const char* text);
	char* InputDialog(GtkWindow* parent, const char* title,
						const char* text, const char* value);
	gboolean ConfirmDialog(GtkWindow* parent, const char* text);
	void ListDialog(GtkWindow* parent, const char* title, vector<string>& strlist);
	void FlushUiEvents(void);
	
	static void ShowDialog(GtkWindow* parent, 
								const char* text, GtkMessageType type);
	static GtkDialog* BaseDialog(GtkWindow* parent, const char* title, bool cancel);

};

#endif /*UIDIALOGS_HPP_*/
