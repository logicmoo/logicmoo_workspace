/*
 *	file:		interrupt.c
 *	version:	1.0
 *	date:		March 9, 1990
 *	creation:	March 9, 1990
 *	author:		Uwe Waldmann (uwe)
 *
 *	description:
 *	C functions to be used in connection with interrupt.pl
 *
 *	history:
 *	900322	js	removed absolute pathname for qpaction.h,
 *			use -I option for cc.
 *
 *	Copyright (C) 1990
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

#include <stdio.h>
#include <signal.h>
#include "quintus.h"

#ifdef QPROLOG_VER_242
#define QP_flush QP_FLUSH
#endif

#define DISABLED 0
#define ENABLED  1
#define USED     2

static long int quit_completion = DISABLED;
static long int rg_version = 0;

/* the following two functions are modified from quintus/library/critical.c */

/*----------------------------------------------------------------------+
|   get_char() reads a (case-folded) character from stdin.		|
+----------------------------------------------------------------------*/

static int get_char()
    {
	char c, command;

	fflush(stdin);
	command = '\n';		/* have not seen a printable character yet */
	read(0, &c, 1);
	while (c != EOF && c != '\n') {
	    if (c > ' ' && c < 127 && command == '\n') {
		command = c;	/* first printable character */
	    }
	    read(0, &c, 1);
	}
	fflush(stdin);
	if (command >= 'A' && command <= 'Z') command += 'a'-'A';
	return command;
    }


static void give_menu()
    {
	for (;;) {
	    if (quit_completion == ENABLED)
	      { printf("\nCompletion interruption (h for help)? "); }
	    else
	      { printf( "\nProlog interruption (h for help)? "); };
	    switch (get_char()) {
		case 'c':
		    /* fflush(stderr) */; return;
		case 'q':
		    if (quit_completion == DISABLED) {
		        printf( "\
\"q\" option disabled\n");
		    } else if (quit_completion == ENABLED) {
		        quit_completion = USED;
			printf("\
\n\n\nThe completion process will be aborted at the next consistent state.\n");
			return;
		    } else {
		        printf( "\
\"q\" option has already been used\n");
		        return;
		    }
		    break;
		case 't':
		    if(!rg_version) QP_action(QP_TRACE);
		    return;
		case 'd':
		    if(!rg_version) QP_action(QP_DEBUG);
		    return;
		case EOF:	/* better solution needed... */
		case 'a':
		    if (quit_completion == ENABLED)
		      { quit_completion = USED;
			printf("\
\n\n\nThe completion process will be aborted at the next consistent state.\n");
			return;
		      }
		    else 
		      { quit_completion = DISABLED;
			QP_action(QP_ABORT);
		      }
		case 'e':
		    QP_action(QP_EXIT);
		case 'h':
		case '?':
		    printf( "\n\
Prolog interrupt options:\n");
		    if (quit_completion == ENABLED) {
		        printf( "\
    q    quit     - quit cec completion\n");
		    }
		    printf( "\
    c    continue - do nothing\n");
		    if( !rg_version) {
			printf( "\
    t    trace    - debugger will start creeping\n\
    d    debug    - debugger will start leaping\n");
		    }
		    printf( "\
    a    abort    - cause a Prolog abort\n\
    e    exit     - irreversible exit from Prolog\n\
    h    help     - this list\n");
		    break;
		default:
		    break;
	    }
	}
    }



void establish_handler(v)
long int v;
{
    rg_version = v;
    signal(SIGINT, give_menu);
}


long int completion_interrupted()
{
    return (quit_completion);
}


void disable_q_option()
{
    quit_completion = DISABLED;
}


void enable_q_option()
{
    quit_completion = ENABLED;
}
