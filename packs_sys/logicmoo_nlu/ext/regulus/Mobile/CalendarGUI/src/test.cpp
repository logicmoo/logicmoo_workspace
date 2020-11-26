/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	test.cpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */

#include <gtk/gtk.h>
#include <string.h>
#include <signal.h>

#include <hildon/hildon-program.h>
#include <gtk/gtkmain.h>

#include "ace/streams.h"
#include "GlobalDefs.hpp"
#include "CalendarController.hpp"
#include "CalendarWindow.hpp"

int main(int argc, char *argv[])
{	
	string server("129.194.32.96");
	CalendarController::InputLang lang(CalendarController::INPUT_LANG_ENG);
	
	if (argc >= 2)
	{	
		string arg = argv[1];
		
		if (arg == "ENG")
		{
			lang = CalendarController::INPUT_LANG_ENG;			
		}
		else if (arg == "JAP")
		{
			lang = CalendarController::INPUT_LANG_JAP;		
		}
		else
		{
			lang = CalendarController::INPUT_LANG_ENG;
		}
	}
	
	if (argc == 3)
	{	
		server = argv[2];
	}

	const struct tm* tm;
	size_t len;
	time_t now;
	char* s;
	
	now = time(NULL);
	tm = localtime(&now);
	
	s = new char[40];
	len = strftime(s, 40, "%Y-%m-%d.%H-%M-%S", tm);
	string str(s);
	str = "../logs/test_calendar_gui." + str + ".output";	
	delete s;
	
	ACE_OSTREAM_TYPE* output = 
		new std::ofstream(str.c_str());
	ACE_LOG_MSG->msg_ostream(output, 0);
	ACE_LOG_MSG->set_flags(ACE_Log_Msg::OSTREAM);
	ACE_LOG_MSG->set_flags(ACE_Log_Msg::VERBOSE_LITE);
	
	ACE_LOG_MSG->priority_mask(LM_TRACE|LM_DEBUG, ACE_Log_Msg::PROCESS);
	
	g_thread_init(NULL);
	gdk_threads_init();
	gdk_threads_enter();
	gtk_init(&argc, &argv);
	g_set_application_name(APP_NAME);
	
	CalendarController calendarctl;
	calendarctl.SetRemoteServersIP(server);
	calendarctl.SetInputLang(lang);
	calendarctl.RunApp();

	ACE_LOG_MSG->clr_flags(ACE_Log_Msg::OSTREAM);
	delete output;
	
	gdk_threads_leave();
	
	return 0;    
}
