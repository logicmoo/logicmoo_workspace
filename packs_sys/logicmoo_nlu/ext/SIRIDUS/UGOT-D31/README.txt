*******************************************************

TALK UGOT WP3 task 3.1 implementation accompanying D3.1

Extended Information State Modelling

    Stina Ericsson, 19th January, 2006

*******************************************************

Requirements
============

Sicstus Prolog 3.11 or newer
Emacs with Prolog mode


Directory structure
===================

The code distributed here with D3.1 contains an alpha version of
TrindiKit 4, in the directory trindikit4-0.1alpha/. TrindiKit 4 is
also available from SourceForge:
http://sourceforge.net/projects/trindikit/

The code also consists of a directory godis/ which contains all the
non-application specific parts of GoDiS. Of particular relevance to
present concerns are the files in the directory godis/godis-aod/.

There is also a directory godis-apps/ that contains application
specific code. Of interest here is the directory
godis-apps/domain-tram/ where the code for the tram information
application can be found.



Installing TrindiKit and GoDiS
==============================

* Unix/Linux: 

TrindiKit and Windows are installed on Unix/Linux by setting TRINDIKIT
and GODIS in the environment file to the trindikit4-0.1alpha directory
and the godis directory, respectively. For instance:

setenv TRINDIKIT ~/TALK/UGOT-D31/trindikit4-0.1alpha
setenv GODIS ~/TALK/UGOT-D31/godis


* Windows:

TrindiKit and Windows are installed on Windows by setting the
environment variables in Settings -> Control panel -> System ->
Advanced. A system variable TRINDIKIT is set to the
trindikit4-0.1alpha directory. A system variable GODIS is set to the
godis directory.

For instance:

TRINDIKIT     C:\TALK\UGOT-D31\trindikit4-0.1alpha
GODIS         C:\TALK\UGOT-D31\godis



Running the tram information application
========================================

Open the file start-tram-text-t4.pl in godis-apps/domain-tram/ using
Emacs. Consult the buffer. Type "quiet." to see only the dialogue when
the system is running, or type "verb." to see the information state
and the application of the update rules ("verb." is the
default). Using "verb." enables the inspection of changes to the
extended information state during the dialogue. Then type "run." or
one of: "rundriving.", "runtelephone.", "runmeeting.",
"runathome.". The dialogues described in TALK D3.1 for GoDiS can then
be tried.

Say "bye" to the system to quit the dialogue, and type C-c C-c to
abort Prolog.

