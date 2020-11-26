/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	MrcpControllerDefs.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef MRCPCONTROLLERDEFS_HPP_
#define MRCPCONTROLLERDEFS_HPP_

#include <string>

#define MAX_SERVER_NAME_LEN 64
#define MAX_FILENAME_LEN 256

using namespace std;

const static string g_recognizer  = "recognizer";
const static string g_synthesizer = "synthesizer";

const static string CRLF( "\r\n" );
const static string LF( "\n" );
const static string SP( " " );

string const G_digit_grammar =
"    <rule id=\"digit\">\n"
"        <one-of>\n"
"            <item> 1 </item>\n"
"            <item> 2 </item>\n"
"            <item> 3 </item>\n"
"            <item> 4 </item>\n"
"            <item> 5 </item>\n"
"            <item> 6 </item>\n"
"            <item> 7 </item>\n"
"            <item> 8 </item>\n"
"            <item> 9 </item>\n"
"            <item> 0 </item>\n"
"        </one-of>\n"
"    </rule>\n"
"    <rule id=\"four-digits\" scope=\"public\">\n"
"        <item>\n"
"            <ruleref uri=\"#digit\"/>\n"
"            <ruleref uri=\"#digit\"/>\n"
"            <ruleref uri=\"#digit\"/>\n"
"            <ruleref uri=\"#digit\"/>\n"
"        </item>\n"
"    </rule>\n";

string const G_speech_digit_grammar =
    "<?xml version=\"1.0\"?>\n"
    "<grammar xmlns=\"http://www.w3.org/2001/06/grammar\"\n"
    " xml:lang=\"en-US\" version=\"1.0\" root=\"four-digits\">\n" +
    G_digit_grammar +
    "</grammar>\n";

string const G_dtmf_digit_grammar =
    "<?xml version=\"1.0\"?>\n"
    "<grammar xmlns=\"http://www.w3.org/2001/06/grammar\"\n"
    "         xml:lang=\"en-US\" version=\"1.0\" mode=\"dtmf\"\n"
    "         root=\"four-digits\">\n" +
    G_digit_grammar +
    "</grammar>\n";

//string const G_slm_grammar =
//    "SLM:public [MAIN_SLM]";

string const G_slm_grammar =
		"SLM:public [MAIN_SLM]";
		
#endif /*MRCPCONTROLLERDEFS_HPP_*/
