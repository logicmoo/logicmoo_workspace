 
OAA Agent for a song database, version 1.0 (April 2005)

1. WHAT IS THIS?

This is a simple database application using a text file as
database. The application searches a given directory for music files
and makes a database file from the meta information. 


==============================================================


2. REQUIREMENTS.

In order to run this agent you need the following software together with the
appropriate CLASSPATHs and PATHs.


2.1 OAA 2.3

The OAA agent requires OAA to run. If you do not allready have it installed
on your system, download and install it.

Download from: http://www.ai.sri.com/oaa/distribution/v2.3/


2.2 JlGuiPlayer 

In order to get the meta information from the music files, this
application uses JlGuiPlayer. The necessary jar files will be included
in DbaseAgentFiles.jar.

Download JlGui from: http://www.javazoom.net/jlgui/jlgui.html


2.3 Extra jarfiles

Extract all the files in the DbaseAgentFiles.jar file in a chosen
directory. Then extract the jlGuiJars.jar file which contains the
JlGui files needed for this application. 

Make sure the CLASSPATH environment variable points to those jar
files. Either in the java call, or as pre-defined environment
variables.


==============================================================


3. RUNNING THE APPLICATION

3.1 STARTING UP

3.1.1 Starting OAA

An OAA facilitator must be started in order to run the
application. Start an OAA facilitator in accordance with the OAA
specifications. 


3.1.2 Starting the database agent

For a short help text, run the application with the '-h' flag.

java -classpath ${CLASSPATH}:dbaseAgent.jar DbaseAgent -h


The agent could be used in two ways; making a new database file or
using an existing one. When making a new file the user must give a
directory and a file name as arguments in the command line. The flags
used are '-dir' and '-dbfile'.

java -classpath ${CLASSPATH}:dbaseAgent.jar DbaseAgent 
		-dir PathToMusicDir -dbfile PathToPutDbFile 
 
When there exists a database file, no arguments are needed except
OAA arguments(if needed).

java -classpath ${CLASSPATH}:dbaseAgent.jar DbaseAgent {optional OAA arguments} 



The user can also slightly modify the database file when it is created. If the
meta information of a song contains a "'", it cannot be parsed
correctly in the database application. For example, if a song is
called "Hello, I'm here". Open the database file in a text editor and
just put ** first on the relevant line. This will make the application
ignore the line.


3.2 THE SOLVABLES

searchDbase(+DbaseFile,+Query,-Answer)

This is the only solvable available in this application. The user
specifies what database file to use and the search query. So far there
is only one sort of query available which uses unification in order to
find the answer/-s. The query's form is song(ListOfEntries). The
returning Answer is in the form of a list. I.e. an IclList.

The example below shows a query. The order of the entries must be as
shown. All arguments starting with an upper case letter, is a variable
that will unify with the database. The user can instantiate how many
variables he wants when searching.

song([ artist(Artist),
       title(Title),
       ablbum(Album),
       length(SongLengthInSeconds),
       track(TrackNo),
       genre(Genre),
       year(Year),
       bitrate(BitRate),
       samplingrate(SamplingRate),
       comment(Comment),
       path(PathToSong) ] )

The DbaseFileName and the arguments in the query are all IclStr. That
is, Java Strings or Prolog atoms.


Example query 1: returning a list with all songs by Tomas Ledin

searchDbase( 'path_to_dbase',
	     song([ artist('Tomas Ledin'),
		    title(Tit),
		    album(Alb),
		    length(Len),
		    track(Tra),
		    genre(Gen),
		    year(Year),
		    bitrate(Bit),
		    samplingrate(Samp),
		    comment(Com),
		    path(Path)]),
	     Answer)


Example query 2: returning a list with all songs in the genre rock
		 made by the artist Uno Svenningson

searchDbase( 'path_to_dbase',
	     song([ artist('Uno Svenningsson'),
		    title(Tit),
		    album(Alb),
		    length(Len),
		    track(Tra),
		    genre('Rock'),
		    year(Year),
		    bitrate(Bit),
		    samplingrate(Samp),
		    comment(Com),
		    path(Path)]),
	     Answer)

