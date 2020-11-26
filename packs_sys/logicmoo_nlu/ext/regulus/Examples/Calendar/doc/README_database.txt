
CREATING A VERSION OF THE APPLICATION FOR A NEW DATABASE

The app assumes that the database will be in Prolog format. Name
entries in the Regulus grammar are automatically extracted from the
database by a script. There is an example of a correctly formatted
database in the file Prolog/database_im2.pl.

This document should contain all the information you need if you want
to create a version of the system using a new database. Specifically,
it describes 1) the format of the database, 2) which other files
need to be edited or created.

FORMAT OF DATABASE

The database needs to define a module called 'database' exporting the
following predicates:

meeting/7
person/6
attends/2
location/5

NOTE: THE EXACT FORM OF THE FIELDS CAN BE IMPORTANT, SINCE 
SOME OF THEM ARE USED TO EXTRACT ENTRIES FOR THE REGULUS GRAMMAR.

FIELDS CONTAINING INFORMATION THAT WILL APPEAR IN THE LEXICON MUST BE
PROLOG ATOMS WHOSE PRINT NAMES CONTAIN ONLY LOWER-CASE LETTERS AND
UNDERSCORES.

In detail, the format of the database relations is as follows:

meeting/7
Pattern: meeting(ID, Day, Month, Year, StartTime, EndTime, LocID).
Example: meeting(meeting_1, 29, 5, 2007, 9:30, 12:0, nikos_room_1).

 - ID is an arbitrary Prolog atom
 - Day is a number (1-31) representing day of month
 - Month is a number (1-12) representing a month
 - Year is a number (2000-2100) representing a year
 - StartTime is two numbers (0-23, 0-59) separated by a colon, representing a time
 - EndTime is two numbers (0-23, 0-59) separated by a colon, representing a time
 - LocID is an arbitrary Prolog atom.

person/6
Pattern: person(ID, FirstName, LastName, Affiliation, Phone, Email).
Example: person(pierrette_bouillon, pierrette, bouillon, geneva, "+41 22 379 8679", "Pierrette.Bouillon@issco.unige.ch").

 - ID is an arbitrary Prolog atom
 - FirstName is a Prolog atom whose print name may only contain letters and underscores. 
 - LastName is a Prolog atom whose print name may only contain letters and underscores. 
 - Affiliation is a Prolog atom whose print name may only contain letters and underscores. 
 - Phone is an arbitrary Prolog string
 - Email is an arbitrary Prolog string

attends/2
Pattern: attends(PersonID, MeetingID).
Example: attends(pierrette_bouillon, meeting_1).

 - PersonID is a Prolog atom that appears in the first field of some person record
 - MeetingID is a Prolog atom that appears in the first field of some meeting record

location/5
Pattern: location(ID, Name, Country, City, Organisation).
Example: location(nikos_room_1, nikos_s_room, switzerland, geneva, geneva_university).

 - ID is an arbitrary Prolog atom
 - Name is a Prolog atom whose print name may only contain letters and underscores. 
 - Country is a Prolog atom whose print name may only contain letters and underscores. 
 - City is a Prolog atom whose print name may only contain letters and underscores. 
 - Organisation is a Prolog atom whose print name may only contain letters and underscores. 

DECLARING THE NEW DATABASE 

You need to create or edit the following additional files:

1. A top-level config file. Use the one in $REGULUS/Examples/Calendar/scripts/calendar_im2.cfg
as a model. Edit all the fields that include the string 'im2' and replace them with
corresponding values. 

2. scripts/Makefile: add make targets corresponding to the ones for im2. 
This will include adding a file corresponding to
scripts/build_name_lexicon_im2, to create the new name lexicon.

3. scripts directory: create files corresponding to all those containing
the string 'im2'. Modify the content accordingly.
