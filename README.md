# aleph
Porting of Aleph for SWI-Prolog.

Aleph is an Inductive Logic Programming system developed by Ashwin Srinivasan:

http://www.cs.ox.ac.uk/activities/machlearn/Aleph/

This  pack is a porting of Aleph version 5 to SWI-Prolog. The porting was done  by Fabrizio Riguzzi.

Example of use
---------------

    $ cd <pack>/aleph/prolog/examples/trains
    $ swipl
    ?- [library(aleph)].
    ?- read_all(train).
    ?- induce.

## Manual
The manual can be found at http://www.cs.ox.ac.uk/activities/machlearn/Aleph/.

## Examples
The examples have been downloaded from http://www.comlab.ox.ac.uk/oucl/research/areas/machlearn/Aleph/misc/examples.zip and ported as well to SWI-Prolog.
