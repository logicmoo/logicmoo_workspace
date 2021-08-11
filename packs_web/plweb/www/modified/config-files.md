# New config file structure

Starting with SWI-Prolog 8.1.15,  the   locations  for  finding personal
configuration files and  storing  extensions   (packs)  has  changed  to
statisfy the free  desktop  standards  (XDG)   and  reach  at  a  common
structure for all platforms.

Now,  everything  except  for  packs  is    installed   in  a  directory
`swi-prolog` below a base directory  for _configuration_ files depending
on the OS. Packs are install in   a  directory `swi-prolog` below a base
directory for _data_ files.  The base directories are:

 - Config
   - Windows: CSIDL directory CSIDL_APPDATA (see win_folder/2)
   - Otherwise:
   - $XDG_CONFIG_HOME, defaulting to $HOME/.config
 - Data
   - Windows: CSIDL directory CSIDL_LOCAL_APPDATA (see win_folder/2)
   - Otherwise:
   - $XDG_DATA_HOME, defaulting to $HOME/.local/share

In the _config_ directory we find:

 - init.pl (previously .swiplrc or swipl.ini (Windows) <br>
   Personal initialization file
 - lib (previously ~/lib/prolog) <br>
   Personal library
 - xpce (previously ~/.xpce) <br>
   Directory holding xpce application data

In the _data_ directory we find:

 - pack (previously ~/lib/swipl/pack) <br>
   Installed add-ons.

The  system  prints  a  warning  on  startup    if  the  old  init  file
(``~/.swiplrc`` or `<appdata>\swipl.ini`) was  found   and  there  is no
`init.pl` in the new location, pointing at this page.

If you use multiple versions we suggest  to create the new hierarchy and
make symbolic links from the old  locations. Windows users should either
make copies or create a file at  the   old  location that loads the data
from the new location.

@see https://swi-prolog.discourse.group/t/new-config-file-structure/1360/1
