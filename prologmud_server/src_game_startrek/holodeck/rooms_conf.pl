/*
Room Flags
Door Flags:

A single number, with the following meaning:
0) No door        - An unrestricted exit that has no door, or a special door
                    that cannot be opened or closed with the ``open'' and ``close''
                    commands.  The latter is useful for secret doors, trap doors,
                    or other doors that are opened and closed by something other
                    than the normal commands, like a special procedure or script
                    assigned to the room or an tObj in the room.

1) Closeable door - Normal doors that can be opened, closed, locked, unlocked,
                    and picked.

2) Pickproof      - if locked, can be opened only with the key.


Room Flags:

A short mudDescription of the roomflags:

A bitvector, with the following values:
1) DARK       - self explanatory
2) DEATH      - room is a deathtrap - a forced quit
3) NO_MOB     - mobs will not enter this room
4) INDOORS    - weather has no effect
5) PEACEFUL   - no violence will work here 
6) SOUNDPROOF - tell, gossip, shout, holler will not be heard here.
7) NO_TRACK   - track will never find a path through this room
8) NO_MAGIC   - no magic will work here.
9) TUNNEL     - only one person allowed at one time
10) PRIVATE   - cannot enter if more than two persons there
11) GODROOM   - only for GODS of level 33 or above
12) HOUSE     - DO NOT USE 
13) HCRSH     - DO NOT USE
14) ATRIUM    - DO NOT USE
15) OLC       - DO NOT USE
16) *         - DO NOT USE

            
Sector Type

     A single number (not a bitvector)
defining the tCol of terrain in the room.  Note that this value
is not the number of movement points needed but just a number to
identify the sector tCol.  It can be any of the following:

0    INSIDE         Indoors - typically small number of move points needed.
1    CITY           The streets of a city.
2    FIELD          An open field.
3    FOREST         A dense forest.
4    HILLS          Low foothills.
5    MOUNTAIN       Steep mountain regions.
6    WATER_SWIM     Water (swimmable).
7    WATER_NOSWIM   Unswimmable water - boat required for passage.
8    FLYING         The name says it all.
9    UNDERWATER     Underwater - currently has no effect..
*/
