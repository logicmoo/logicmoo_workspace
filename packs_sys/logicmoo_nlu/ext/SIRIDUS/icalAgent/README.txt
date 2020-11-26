OAA wrapper for handling calendar files

Andreas Wallentin

Göteborg University
Department of Linguistics
under the TALK project

December 2004


OAA wrapper for handling calendar(iCal,KOrganizer) files

This OAA wrapper handles files for calendar applications such as iCal
and KOrganizer. It builds new calendar files or updates existing
ones. The wrapper does not interact with a database, instead it saves
the necessary information as a binary file. From this binary file one
can build new calendar files in VCalendar format (*.ics).
If the use has existing calendar files, the wrapper can take one of
those and load it into the application, giving the user the chance to
update it.


Over all functionality

The user uses OAA when updating his calendar files. These files are then
used in other stand alone calendar applications. As default suffix to
these file we will use ``.ics''. When using this wrapper, a default
database is used. The database is updated automatically every time an
entry is added or deleted. The binary database file will be placed in,
logically from the directory of the java call, dbs/db.sav.

So far the user can add events, todos and alarms into the
calendar using the add/3 and add/4 methods. In the near future there
will only be events to add, using the addEvent/5 method. 
When adding to the database an Id number is also added automatically
into the entry. This is used to simplify the delete method. When the
user has added his information, the database entries will look like
the following examples:  
entry( event(Summary,Id,startDate(StartDate),endDate(EndDate)) ) or
entry( todo(Summary,Id,startDate(StartDate)) )
For format of the input, see the solvables below.

The wrapper will probably not be able to parse all type of calendar
files. The ones used, both when parsing and generating, are very
simple with less information that is customary. However, the parser is
to be updated shortly.


For additional information, see the API for the wrapper classes or the
source files. There will be examples of test runs further down.

These are the solvables that are available to the wrapper:
 - add(+Type,+Summary,+StartDate)
 - add(+Type,+Summary,+StartDate,+StopDate)
 - addEvent(+Type,+Summary,+StartDatime,+EndDatime,+Location)
 - entry(?X)
 - delete(+Id)
 - buildCal(+CalendarFileName)
 - buildDb(+ExistingCalendarFileName)


The add solvable comes in three versions. The first,
add(Type,Summary,StartDate), is 
handling the todos and alarms. What this solvable needs is
representation of what type it is (todo or alarm), summary of the
entry and a start date. All representation must be strings, even the
date. The date format takes the YearMonthDay and the time of day
which will look like YYYYMMDDTHHMM, that is, '20041215T1530'.
The add/4 solvable will naturally look the same, only adding an end
time as the fourth argument.

As mentioned earlier the addEvent solvable is not used yet. This will
be used when the application only will handle events in a
calendar. This solvable will differ slightly from the add
solvable. The type needed here is what kind of event it is. That is,
meeting, birthday, appointment and so forth. The summary is the same
as before, just a reminder for the user what he is supposed to do. As
last argument a location of the event is required. The biggest
difference will be the time representation. The representation used
here will be like the prolog predicate datime/6,
datime(YYYY,MM,DD,HH,MM,SS). 

Since it uses a database like structure, the user is able to search
for entries. The search method is so far rather simple, using the
built-in unification when using OAA. The entry/1 solvable is used to
search the database. As argument the user must provide what he is
looking for. If the user is is looking for all entries in the
database, he uses the ``all entries'' search, entry(X). The user can
also search for all events or todos in the same way with a method call
like entry(event(Summary,Id,StartDate,EndDate)). Since the database
uses unification, all entries that are an event will be
returned. Naturally when using unification the user can provide more
information if he wants specific entries. The search
entry(todo(_,_,'20041215T123000')) is searching for all todos that are
to start 12:30 the 15th of December, 2004.

In addition to adding entries, the user is also able to delete entries
from the database. This is a very simple delete function using the
delete(X) solvable where X is the ID number of the entry to
delete. The ID number is shown when the user is searching for an
entry. For example, if the user makes the query
entry(todo(Summary,ID,'20041215T123000')), he will get an answer like
entry(todo('summary here',id(3),'20041215T123000')). 

The buildCal(CalendarFileName) solvable is rather straight
forward. When calling this solvable the user must give string
representing the name of the *.ics file. The application will convert
the database entries to text strings, writing them to the file.

The last solvable is buildDb(ExistingCalendarFileName). This is used
when the user want so update his existing calendar files. The user
must give the name of an existing file. The application parses the
calendar file, making a database of with corresponding entries. Then
the user is free to update his calendar and, perhaps, building a new
calendar file.


How to start the agent

Extract the icalAgent.tar file and it should be enough to get started.

How to start the wrapper with the following call:

  java -cp calendar_agent.jar:ical4j.jar:$CLASSPATH FindAgent

It is assumed that the $CLASSPATH variable contains paths to all jar files
necessary for running OAA and so forth. The ical4j.jar file must be in
the same directory you start the program, or be in a correct path.

If you want to handle test some files, there are some calendar files
in the 'icsFiles' directory.



Test run examples

Starts with building a database from an existing file:
 - oaa_Solve(buildDb('test.ics'), [])

Checks all entries. The result is displayed (now) in the debugger
window: 
 - oaa_Solve(entry(X), [])

If the user wants to see all todos:
 - oaa_Solve(entry(todo(_,_,_)), [])

The user wants to see all event starting a certain time:
 - oaa_Solve(entry(event(_,_,startdate('20041215T153000'),_)), [])

Adding a new TODO to the database:
 - oaa_Solve(add(todo,'testing todo','20041215T153000'), [])

Building a calendar file from the existing database:
 - oaa_Solve(buildCal('test2.ics'), [])

Deleting an entry, with ID no 2, from the database:
 - oaa_Solve(delete(2), [])

