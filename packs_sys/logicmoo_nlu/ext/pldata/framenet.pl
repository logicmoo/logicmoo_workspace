/*
SWI-Prolog version of the Berkeley FrameNet database v. 1.4, which was distributed in the SemEval 10 shared task on "Linking events and their participants"

- The file "valence_patterns" lists for every lexical unit in a frame the valence patterns ecoded 
in the FrameNet database, including the coreness status and the GF/PT label for each frame element 
as well as the pattern occurrencies in the database.

Every entry has the following structure:

fsr('Lexical_unit'-Lu_pos,Lowercased_frame,[List_of_valence_patterns]).

The list of valence patterns includes in turn every valence pattern for the given LU represented as a list of FEs plus the number of occurrences of that pattern in the FN database.
Every FE label is followed by -c(ore), -p(eripheral) or -e(xtra thematic) and comes also with the phrase type and the grammatical function assigned by FN annotators. 
If a FE is annotated as a target (ex. the "Artist" Frame Element in "Behind_the_scenes" frame), it is represented in the pattern as bearing the "target" PT and GF, for example: [artist-c,target,target].
If a FE has been annotated as null instantiated, it bears the cni/ini/dni label as PT and the "null" label as GF, for example: [ground-c,dni,null].
All other GF and PT labels are those reported in the FN database.

If the valence pattern list is empty, it means that no example sentences were available in the FN database.

- The file "semtypes" contains the list of semantic types associated to every frame element as Prolog "semtype" predicates.
The format is:
semtype(Frame_name, Frame_element, Semantic_type).

(For more information about FN semantic types, please refer to the project documentation).

- The file "frame_to_frame" contains the list of all frame-to-frame relations represented as "frel" Prolog predicates.
The format is:
frel(Relation_type, Frame1, Frame2).

- The file "fe_to_fe" contains the list of all frame element pairs that are put into relation through a frame-to-frame relation. This information is represented as "frels" Prolog predicates.
The format is:
frels(Relation_type, Frame1, Frame2, FE_frame1, FE_frame2).

*/
:- module(framenet,[fnpattern/4,fsr/3,semtype/3,frel/3,frels/5]).

:- consult('Prolog_FNet/valence_patterns'). % fsr/3
:- consult('Prolog_FNet/semtypes'). % semtype/3
:- consult('Prolog_FNet/frame_to_frame.txt'). % frel/3
:- consult('Prolog_FNet/fe_to_fe.txt'). % frels/5

%fn_verb_frame1(Doer,Verb,ISA, fnpattern(Verb, _, ISA, [ Arg1Type : Arg1, Arg2Type: Arg2]).

%fn_verb_frame1(Doer,Verb,ISA, fnpattern(Verb, _, ISA, [ 'agent' : Doer, Arg2Type: Arg2]).

fnpattern(immerse, 9010000, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(immerse, 9010000, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(lodge, 9010000, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(lodge, 9010000, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(position, 9010000, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(position, 9010000, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(situate, 9010000, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(situate, 9010000, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(deposit, 9010100, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(deposit, 9010100, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(insert, 9010100, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(insert, 9010100, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(stash, 9010100, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(stash, 9010100, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(stow, 9010100, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(stow, 9010100, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(place, 9010200, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(place, 9010200, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(put, 9010200, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(put, 9010200, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(set, 9010200, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(set, 9010200, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(lay, 9020000, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(lay, 9020000, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(hang, 9020100, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(hang, 9020100, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(lean, 9020100, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(lean, 9020100, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(perch, 9020100, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(perch, 9020100, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(rest, 9020100, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(rest, 9020100, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(sit, 9020100, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(sit, 9020100, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(stand, 9020100, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(stand, 9020100, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(tuck, 9030200, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme', 'cause': 'cause']).
fnpattern(ram, 9030210, 'cause_impact', ['agent': 'agent', 'theme': 'impactor', 'destination': 'impactee']).
fnpattern(dribble, 9050000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'goal']).
fnpattern(dribble, 9050000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'area']).
fnpattern(drip, 9050000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'goal']).
fnpattern(drip, 9050000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'area']).
fnpattern(spew, 9050000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'goal']).
fnpattern(spew, 9050000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'area']).
fnpattern(spill, 9050000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'goal']).
fnpattern(spill, 9050000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'area']).
fnpattern(spin, 9060000, 'cause_to_move_in_place', ['agent': 'agent', 'theme': 'theme', 'location': 'fixed_location']).
fnpattern(spin, 9060000, 'cause_to_move_in_place', ['agent': 'agent', 'theme': 'body_part', 'location': 'fixed_location']).
fnpattern(spin, 9060000, 'cause_to_move_in_place', ['agent': 'cause', 'theme': 'theme', 'location': 'fixed_location']).
fnpattern(spin, 9060000, 'cause_to_move_in_place', ['agent': 'cause', 'theme': 'body_part', 'location': 'fixed_location']).
fnpattern(twirl, 9060100, 'cause_to_move_in_place', ['agent': 'agent', 'theme': 'theme', 'location': 'fixed_location']).
fnpattern(twirl, 9060100, 'cause_to_move_in_place', ['agent': 'agent', 'theme': 'body_part', 'location': 'fixed_location']).
fnpattern(twirl, 9060100, 'cause_to_move_in_place', ['agent': 'cause', 'theme': 'theme', 'location': 'fixed_location']).
fnpattern(twirl, 9060100, 'cause_to_move_in_place', ['agent': 'cause', 'theme': 'body_part', 'location': 'fixed_location']).
fnpattern(brush, 9070100, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(brush, 9070100, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(brush, 9070100, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(drizzle, 9070100, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(drizzle, 9070100, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(drizzle, 9070100, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(hang, 9070100, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(hang, 9070100, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(hang, 9070100, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(plaster, 9070100, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(pump, 9070100, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(rub, 9070100, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(rub, 9070100, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(scatter, 9070100, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(seed, 9070100, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(shower, 9070100, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(shower, 9070100, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(shower, 9070100, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(smear, 9070100, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(smear, 9070100, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(smear, 9070100, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(sow, 9070100, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(spatter, 9070100, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(splash, 9070100, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(splatter, 9070100, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(spray, 9070100, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(spread, 9070100, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(sprinkle, 9070100, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(squirt, 9070100, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(stick, 9070100, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(stick, 9070100, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(strew, 9070100, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(wrap, 9070100, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(wrap, 9070100, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(wrap, 9070100, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(cram, 9070110, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(cram, 9070110, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(cram, 9070110, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(crowd, 9070110, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(jam, 9070110, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(jam, 9070110, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(jam, 9070110, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(pack, 9070110, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(pack, 9070110, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(pack, 9070110, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(pile, 9070110, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(pile, 9070110, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(pile, 9070110, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(dab, 9070200, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(dab, 9070200, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(dab, 9070200, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(daub, 9070200, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(daub, 9070200, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(daub, 9070200, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(drape, 9070200, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(drape, 9070200, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(drape, 9070200, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(dust, 9070200, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(dust, 9070200, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(dust, 9070200, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(heap, 9070200, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(heap, 9070200, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(heap, 9070200, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(load, 9070200, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(load, 9070200, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(load, 9070200, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(adorn, 9080000, 'adorning', ['destination': 'location', 'theme': 'theme']).
fnpattern(adorn, 9080000, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(anoint, 9080000, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(blanket, 9080000, 'adorning', ['destination': 'location', 'theme': 'theme']).
fnpattern(cloak, 9080000, 'adorning', ['destination': 'location', 'theme': 'theme']).
fnpattern(coat, 9080000, 'adorning', ['destination': 'location', 'theme': 'theme']).
fnpattern(coat, 9080000, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(cover, 9080000, 'adorning', ['destination': 'location', 'theme': 'theme']).
fnpattern(cover, 9080000, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(deck, 9080000, 'adorning', ['destination': 'location', 'theme': 'theme']).
fnpattern(decorate, 9080000, 'adorning', ['destination': 'location', 'theme': 'theme']).
fnpattern(dot, 9080000, 'adorning', ['destination': 'location', 'theme': 'theme']).
fnpattern(douse, 9080000, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(embellish, 9080000, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(encircle, 9080000, 'adorning', ['destination': 'location', 'theme': 'theme']).
fnpattern(festoon, 9080000, 'adorning', ['destination': 'location', 'theme': 'theme']).
fnpattern(fill, 9080000, 'adorning', ['destination': 'location', 'theme': 'theme']).
fnpattern(fill, 9080000, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(flood, 9080000, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(inject, 9080000, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(inject, 9080000, 'placing', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(inject, 9080000, 'placing', ['agent': 'cause', 'destination': 'goal', 'theme': 'theme']).
fnpattern(line, 9080000, 'adorning', ['destination': 'location', 'theme': 'theme']).
fnpattern(pave, 9080000, 'adorning', ['destination': 'location', 'theme': 'theme']).
fnpattern(pave, 9080000, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(stud, 9080000, 'adorning', ['destination': 'location', 'theme': 'theme']).
fnpattern(suffuse, 9080000, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(tile, 9080000, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(asphalt, 9090000, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(blanket, 9090000, 'adorning', ['destination': 'location', 'theme': 'theme']).
fnpattern(butter, 9090000, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(cloak, 9090000, 'adorning', ['destination': 'location', 'theme': 'theme']).
fnpattern(panel, 9090000, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(plaster, 9090000, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(seed, 9090000, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(ticket, 9090000, 'fining', ['agent': 'speaker', 'destination': 'payer', 'theme': 'fine']).
fnpattern(tile, 9090000, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(wallpaper, 9090000, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(wreathe, 9090000, 'adorning', ['destination': 'location', 'theme': 'theme']).
fnpattern(archive, 9100000, 'placing', ['agent': 'agent', 'location': 'goal', 'theme': 'theme']).
fnpattern(archive, 9100000, 'placing', ['agent': 'cause', 'location': 'goal', 'theme': 'theme']).
fnpattern(bag, 9100000, 'placing', ['agent': 'agent', 'location': 'goal', 'theme': 'theme']).
fnpattern(bag, 9100000, 'placing', ['agent': 'cause', 'location': 'goal', 'theme': 'theme']).
fnpattern(billet, 9100000, 'placing', ['agent': 'agent', 'location': 'goal', 'theme': 'theme']).
fnpattern(billet, 9100000, 'placing', ['agent': 'cause', 'location': 'goal', 'theme': 'theme']).
fnpattern(bin, 9100000, 'placing', ['agent': 'agent', 'location': 'goal', 'theme': 'theme']).
fnpattern(bin, 9100000, 'placing', ['agent': 'cause', 'location': 'goal', 'theme': 'theme']).
fnpattern(bottle, 9100000, 'placing', ['agent': 'agent', 'location': 'goal', 'theme': 'theme']).
fnpattern(bottle, 9100000, 'placing', ['agent': 'cause', 'location': 'goal', 'theme': 'theme']).
fnpattern(box, 9100000, 'placing', ['agent': 'agent', 'location': 'goal', 'theme': 'theme']).
fnpattern(box, 9100000, 'placing', ['agent': 'cause', 'location': 'goal', 'theme': 'theme']).
fnpattern(cage, 9100000, 'placing', ['agent': 'agent', 'location': 'goal', 'theme': 'theme']).
fnpattern(cage, 9100000, 'placing', ['agent': 'cause', 'location': 'goal', 'theme': 'theme']).
fnpattern(crate, 9100000, 'placing', ['agent': 'agent', 'location': 'goal', 'theme': 'theme']).
fnpattern(crate, 9100000, 'placing', ['agent': 'cause', 'location': 'goal', 'theme': 'theme']).
fnpattern(file, 9100000, 'placing', ['agent': 'agent', 'location': 'goal', 'theme': 'theme']).
fnpattern(file, 9100000, 'placing', ['agent': 'cause', 'location': 'goal', 'theme': 'theme']).
fnpattern(garage, 9100000, 'placing', ['agent': 'agent', 'location': 'goal', 'theme': 'theme']).
fnpattern(garage, 9100000, 'placing', ['agent': 'cause', 'location': 'goal', 'theme': 'theme']).
fnpattern(imprison, 9100000, 'cause_confinement', ['agent': 'agent', 'location': 'goal', 'theme': 'theme']).
fnpattern(imprison, 9100000, 'cause_confinement', ['agent': 'cause', 'location': 'goal', 'theme': 'theme']).
fnpattern(pocket, 9100000, 'placing', ['agent': 'agent', 'location': 'goal', 'theme': 'theme']).
fnpattern(pocket, 9100000, 'placing', ['agent': 'cause', 'location': 'goal', 'theme': 'theme']).
fnpattern(pot, 9100000, 'placing', ['agent': 'agent', 'location': 'goal', 'theme': 'theme']).
fnpattern(pot, 9100000, 'placing', ['agent': 'cause', 'location': 'goal', 'theme': 'theme']).
fnpattern(sheathe, 9100000, 'placing', ['agent': 'agent', 'location': 'goal', 'theme': 'theme']).
fnpattern(sheathe, 9100000, 'placing', ['agent': 'cause', 'location': 'goal', 'theme': 'theme']).
fnpattern(shelve, 9100000, 'placing', ['agent': 'agent', 'location': 'goal', 'theme': 'theme']).
fnpattern(shelve, 9100000, 'placing', ['agent': 'cause', 'location': 'goal', 'theme': 'theme']).
fnpattern(shoulder, 9100000, 'placing', ['agent': 'agent', 'location': 'goal', 'theme': 'theme']).
fnpattern(shoulder, 9100000, 'placing', ['agent': 'cause', 'location': 'goal', 'theme': 'theme']).
fnpattern(warehouse, 9100000, 'placing', ['agent': 'agent', 'location': 'goal', 'theme': 'theme']).
fnpattern(warehouse, 9100000, 'placing', ['agent': 'cause', 'location': 'goal', 'theme': 'theme']).
fnpattern(lodge, 9100100, 'placing', ['agent': 'agent', 'location': 'goal', 'theme': 'theme']).
fnpattern(lodge, 9100100, 'placing', ['agent': 'cause', 'location': 'goal', 'theme': 'theme']).
fnpattern(stable, 9100100, 'placing', ['agent': 'agent', 'location': 'goal', 'theme': 'theme']).
fnpattern(stable, 9100100, 'placing', ['agent': 'cause', 'location': 'goal', 'theme': 'theme']).
fnpattern(depose, 10010000, 'change_of_leadership', ['agent': 'selector', 'theme': 'old_leader', 'source': 'role']).
fnpattern(depose, 10010000, 'change_of_leadership', ['agent': 'selector', 'theme': 'old_order', 'source': 'role']).
fnpattern(dislodge, 10010000, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(dislodge, 10010000, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(eject, 10010000, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(eject, 10010000, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(eliminate, 10010000, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(eliminate, 10010000, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(evict, 10010000, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(evict, 10010000, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(excise, 10010000, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(excise, 10010000, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(expel, 10010000, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(expel, 10010000, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(extract, 10010000, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(extract, 10010000, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(oust, 10010000, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(oust, 10010000, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(remove, 10010000, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(remove, 10010000, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(withdraw, 10010000, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(withdraw, 10010000, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(evacuate, 10020000, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source', 'destination': 'goal']).
fnpattern(evacuate, 10020000, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source', 'destination': 'goal']).
fnpattern(expel, 10020000, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source', 'destination': 'goal']).
fnpattern(expel, 10020000, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source', 'destination': 'goal']).
fnpattern(remove, 10020000, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source', 'destination': 'goal']).
fnpattern(remove, 10020000, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source', 'destination': 'goal']).
fnpattern(clear, 10030100, 'emptying', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(clear, 10030100, 'emptying', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(clear, 10030100, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(clear, 10030100, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(drain, 10030100, 'emptying', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(drain, 10030100, 'emptying', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(drain, 10030100, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(drain, 10030100, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(empty, 10030100, 'emptying', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(empty, 10030100, 'emptying', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(empty, 10030100, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(empty, 10030100, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(expunge, 10041000, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(expunge, 10041000, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(purge, 10041000, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(purge, 10041000, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(shave, 10041000, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(shave, 10041000, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(skim, 10041000, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(skim, 10041000, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(dust, 10041100, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(dust, 10041100, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(pluck, 10041100, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(pluck, 10041100, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(rinse, 10041100, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(rinse, 10041100, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(strip, 10041100, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(strip, 10041100, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(wash, 10041100, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(wash, 10041100, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(abduct, 10050000, 'kidnapping', ['agent': 'perpetrator', 'theme': 'victim']).
fnpattern(confiscate, 10050000, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source', 'beneficiary': 'goal']).
fnpattern(confiscate, 10050000, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source', 'beneficiary': 'goal']).
fnpattern(embezzle, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'source', 'beneficiary': 'purpose']).
fnpattern(embezzle, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'source', 'beneficiary': 'reason']).
fnpattern(embezzle, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'victim', 'beneficiary': 'purpose']).
fnpattern(embezzle, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'victim', 'beneficiary': 'reason']).
fnpattern(filch, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'source', 'beneficiary': 'purpose']).
fnpattern(filch, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'source', 'beneficiary': 'reason']).
fnpattern(filch, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'victim', 'beneficiary': 'purpose']).
fnpattern(filch, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'victim', 'beneficiary': 'reason']).
fnpattern(kidnap, 10050000, 'kidnapping', ['agent': 'perpetrator', 'theme': 'victim']).
fnpattern(lift, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'source', 'beneficiary': 'purpose']).
fnpattern(lift, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'source', 'beneficiary': 'reason']).
fnpattern(lift, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'victim', 'beneficiary': 'purpose']).
fnpattern(lift, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'victim', 'beneficiary': 'reason']).
fnpattern(misappropriate, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'source', 'beneficiary': 'purpose']).
fnpattern(misappropriate, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'source', 'beneficiary': 'reason']).
fnpattern(misappropriate, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'victim', 'beneficiary': 'purpose']).
fnpattern(misappropriate, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'victim', 'beneficiary': 'reason']).
fnpattern(pilfer, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'source', 'beneficiary': 'purpose']).
fnpattern(pilfer, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'source', 'beneficiary': 'reason']).
fnpattern(pilfer, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'victim', 'beneficiary': 'purpose']).
fnpattern(pilfer, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'victim', 'beneficiary': 'reason']).
fnpattern(pinch, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'source', 'beneficiary': 'purpose']).
fnpattern(pinch, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'source', 'beneficiary': 'reason']).
fnpattern(pinch, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'victim', 'beneficiary': 'purpose']).
fnpattern(pinch, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'victim', 'beneficiary': 'reason']).
fnpattern(pirate, 10050000, 'piracy', ['agent': 'perpetrator', 'theme': 'vehicle']).
fnpattern(purloin, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'source', 'beneficiary': 'purpose']).
fnpattern(purloin, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'source', 'beneficiary': 'reason']).
fnpattern(purloin, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'victim', 'beneficiary': 'purpose']).
fnpattern(purloin, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'victim', 'beneficiary': 'reason']).
fnpattern(smuggle, 10050000, 'smuggling', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'source', 'beneficiary': 'goal']).
fnpattern(snatch, 10050000, 'kidnapping', ['agent': 'perpetrator', 'theme': 'victim']).
fnpattern(snatch, 10050000, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source', 'beneficiary': 'goal']).
fnpattern(snatch, 10050000, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source', 'beneficiary': 'goal']).
fnpattern(snatch, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'source', 'beneficiary': 'purpose']).
fnpattern(snatch, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'source', 'beneficiary': 'reason']).
fnpattern(snatch, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'victim', 'beneficiary': 'purpose']).
fnpattern(snatch, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'victim', 'beneficiary': 'reason']).
fnpattern(steal, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'source', 'beneficiary': 'purpose']).
fnpattern(steal, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'source', 'beneficiary': 'reason']).
fnpattern(steal, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'victim', 'beneficiary': 'purpose']).
fnpattern(steal, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'victim', 'beneficiary': 'reason']).
fnpattern(swipe, 10050000, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source', 'beneficiary': 'goal']).
fnpattern(swipe, 10050000, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source', 'beneficiary': 'goal']).
fnpattern(swipe, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'source', 'beneficiary': 'purpose']).
fnpattern(swipe, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'source', 'beneficiary': 'reason']).
fnpattern(swipe, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'victim', 'beneficiary': 'purpose']).
fnpattern(swipe, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'victim', 'beneficiary': 'reason']).
fnpattern(take, 10050000, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source', 'beneficiary': 'goal']).
fnpattern(take, 10050000, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source', 'beneficiary': 'goal']).
fnpattern(thieve, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'source', 'beneficiary': 'purpose']).
fnpattern(thieve, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'source', 'beneficiary': 'reason']).
fnpattern(thieve, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'victim', 'beneficiary': 'purpose']).
fnpattern(thieve, 10050000, 'theft', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'victim', 'beneficiary': 'reason']).
fnpattern(cure, 10060000, 'cure', ['agent': 'healer', 'theme': 'affliction', 'source': 'patient']).
fnpattern(cure, 10060000, 'cure', ['agent': 'healer', 'theme': 'body_part', 'source': 'patient']).
fnpattern(denude, 10060000, 'emptying', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(denude, 10060000, 'emptying', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(divest, 10060000, 'emptying', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(divest, 10060000, 'emptying', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(purge, 10060000, 'emptying', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(purge, 10060000, 'emptying', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(purge, 10060000, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(purge, 10060000, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(rid, 10060000, 'emptying', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(rid, 10060000, 'emptying', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(rob, 10060000, 'robbery', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'victim']).
fnpattern(rob, 10060000, 'robbery', ['agent': 'perpetrator', 'theme': 'goods', 'source': 'source']).
fnpattern(strip, 10060000, 'emptying', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(strip, 10060000, 'emptying', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(strip, 10060000, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(strip, 10060000, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(void, 10060000, 'emptying', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(void, 10060000, 'emptying', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(drain, 10060100, 'emptying', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(drain, 10060100, 'emptying', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(drain, 10060100, 'removing', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(drain, 10060100, 'removing', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(ease, 10060100, 'cure', ['agent': 'healer', 'theme': 'affliction', 'source': 'patient']).
fnpattern(ease, 10060100, 'cure', ['agent': 'healer', 'theme': 'body_part', 'source': 'patient']).
fnpattern(bone, 10070000, 'emptying', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(bone, 10070000, 'emptying', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(core, 10070000, 'emptying', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(core, 10070000, 'emptying', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(gut, 10070000, 'emptying', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(gut, 10070000, 'emptying', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(peel, 10070000, 'emptying', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(peel, 10070000, 'emptying', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(scalp, 10070000, 'emptying', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(scalp, 10070000, 'emptying', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(skin, 10070000, 'emptying', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(skin, 10070000, 'emptying', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(debug, 10080000, 'emptying', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(debug, 10080000, 'emptying', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(deforest, 10080000, 'emptying', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(deforest, 10080000, 'emptying', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(defrost, 10080000, 'emptying', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(defrost, 10080000, 'emptying', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(degrease, 10080000, 'emptying', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(degrease, 10080000, 'emptying', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(delouse, 10080000, 'emptying', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(delouse, 10080000, 'emptying', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(descale, 10080000, 'emptying', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(descale, 10080000, 'emptying', ['agent': 'cause', 'theme': 'theme', 'source': 'source']).
fnpattern(dispatch, 11010000, 'sending', ['agent': 'sender', 'theme': 'theme', 'destination': 'goal']).
fnpattern(dispatch, 11010000, 'sending', ['agent': 'sender', 'theme': 'theme', 'destination': 'recipient']).
fnpattern(post, 11010000, 'sending', ['agent': 'sender', 'theme': 'theme', 'destination': 'goal']).
fnpattern(post, 11010000, 'sending', ['agent': 'sender', 'theme': 'theme', 'destination': 'recipient']).
fnpattern(forward, 11010100, 'sending', ['agent': 'sender', 'theme': 'theme', 'destination': 'goal']).
fnpattern(forward, 11010100, 'sending', ['agent': 'sender', 'theme': 'theme', 'destination': 'recipient']).
fnpattern(mail, 11010100, 'sending', ['agent': 'sender', 'theme': 'theme', 'destination': 'goal']).
fnpattern(mail, 11010100, 'sending', ['agent': 'sender', 'theme': 'theme', 'destination': 'recipient']).
fnpattern(send, 11010100, 'sending', ['agent': 'sender', 'theme': 'theme', 'destination': 'goal']).
fnpattern(send, 11010100, 'sending', ['agent': 'sender', 'theme': 'theme', 'destination': 'recipient']).
fnpattern(ship, 11010100, 'sending', ['agent': 'sender', 'theme': 'theme', 'destination': 'goal']).
fnpattern(ship, 11010100, 'sending', ['agent': 'sender', 'theme': 'theme', 'destination': 'recipient']).
fnpattern(drag, 11040000, 'cause_motion', ['agent': 'agent', 'theme': 'theme', 'source': 'source', 'destination': 'goal']).
fnpattern(drag, 11040000, 'cause_motion', ['agent': 'cause', 'theme': 'theme', 'source': 'source', 'destination': 'goal']).
fnpattern(tug, 11040000, 'cause_motion', ['agent': 'agent', 'theme': 'theme', 'source': 'source', 'destination': 'goal']).
fnpattern(tug, 11040000, 'cause_motion', ['agent': 'cause', 'theme': 'theme', 'source': 'source', 'destination': 'goal']).
fnpattern(push, 11040110, 'cause_motion', ['agent': 'agent', 'theme': 'theme', 'source': 'source', 'destination': 'goal']).
fnpattern(push, 11040110, 'cause_motion', ['agent': 'cause', 'theme': 'theme', 'source': 'source', 'destination': 'goal']).
fnpattern(shove, 11040110, 'cause_motion', ['agent': 'agent', 'theme': 'theme', 'source': 'source', 'destination': 'goal']).
fnpattern(shove, 11040110, 'cause_motion', ['agent': 'cause', 'theme': 'theme', 'source': 'source', 'destination': 'goal']).
fnpattern(pull, 12000100, 'manipulation', ['agent': 'agent', 'theme': 'entity']).
fnpattern(tug, 12000100, 'manipulation', ['agent': 'agent', 'theme': 'entity']).
fnpattern(yank, 12000100, 'manipulation', ['agent': 'agent', 'theme': 'entity']).
fnpattern(push, 12000110, 'manipulation', ['agent': 'agent', 'theme': 'entity']).
fnpattern(thrust, 12000110, 'cause_motion', ['agent': 'agent', 'theme': 'theme']).
fnpattern(pass, 13010000, 'giving', ['agent': 'donor', 'theme': 'theme', 'recipient': 'recipient']).
fnpattern(give, 13010100, 'giving', ['agent': 'donor', 'theme': 'theme', 'recipient': 'recipient']).
fnpattern(lease, 13010100, 'commerce_buy', ['agent': 'buyer', 'theme': 'goods']).
fnpattern(lease, 13010100, 'commerce_sell', ['agent': 'seller', 'theme': 'goods']).
fnpattern(rent, 13010100, 'commerce_buy', ['agent': 'buyer', 'theme': 'goods']).
fnpattern(rent, 13010100, 'commerce_sell', ['agent': 'seller', 'theme': 'goods']).
fnpattern(sell, 13010100, 'commerce_sell', ['agent': 'seller', 'theme': 'goods']).
fnpattern(pay, 13010110, 'commerce_pay', ['agent': 'seller', 'recipient': 'buyer', 'theme': 'goods', 'asset': 'money']).
fnpattern(donate, 13020110, 'giving', ['agent': 'donor', 'theme': 'theme', 'recipient': 'recipient']).
fnpattern(donate, 13020110, 'giving', ['cause': 'donor', 'theme': 'theme', 'recipient': 'recipient']).
fnpattern(disburse, 13020200, 'commerce_pay', ['agent': 'seller', 'recipient': 'buyer', 'theme': 'goods']).
fnpattern(disburse, 13020200, 'commerce_pay', ['agent': 'cause', 'recipient': 'buyer', 'theme': 'goods']).
fnpattern(bequeath, 13030000, 'giving', ['agent': 'donor', 'theme': 'theme', 'recipient': 'recipient']).
fnpattern(charge, 13042000, 'commerce_collect', ['agent': 'buyer', 'theme': 'goods']).
fnpattern(buy, 13051000, 'commerce_buy', ['agent': 'buyer', 'theme': 'goods']).
fnpattern(call, 13051000, 'claim_ownership', ['agent': 'claimant', 'theme': 'property', 'beneficiary': 'beneficiary']).
fnpattern(hire, 13051000, 'hiring', ['agent': 'employer', 'theme': 'employee']).
fnpattern(lease, 13051000, 'commerce_buy', ['agent': 'buyer', 'theme': 'goods']).
fnpattern(order, 13051000, 'request', ['agent': 'speaker', 'theme': 'message', 'source': 'addressee']).
fnpattern(pick, 13051000, 'choosing', ['agent': 'cognizer', 'theme': 'chosen']).
fnpattern(rent, 13051000, 'commerce_buy', ['agent': 'buyer', 'theme': 'goods']).
fnpattern(secure, 13051000, 'getting', ['agent': 'recipient', 'theme': 'theme', 'source': 'source']).
fnpattern(gain, 13051100, 'getting', ['agent': 'recipient', 'theme': 'theme', 'source': 'source']).
fnpattern(get, 13051100, 'getting', ['agent': 'recipient', 'theme': 'theme', 'source': 'source']).
fnpattern(accept, 13052000, 'receiving', ['agent': 'donor', 'theme': 'theme']).
fnpattern(collect, 13052000, 'commerce_collect', ['agent': 'buyer', 'source': 'seller', 'theme': 'goods', 'asset': 'money']).
fnpattern(receive, 13052000, 'receiving', ['agent': 'donor', 'theme': 'theme']).
fnpattern(seize, 13052000, 'taking', ['agent': 'agent', 'theme': 'theme', 'source': 'source']).
fnpattern(select, 13052000, 'choosing', ['agent': 'cognizer', 'theme': 'chosen']).
fnpattern(acquire, 13052100, 'getting', ['agent': 'recipient', 'theme': 'theme', 'source': 'source']).
fnpattern(obtain, 13052100, 'getting', ['agent': 'recipient', 'theme': 'theme', 'source': 'source']).
fnpattern(purchase, 13052100, 'commerce_buy', ['agent': 'buyer', 'theme': 'goods']).
fnpattern(replace, 13060000, 'replacing', ['agent': 'agent', 'theme': 'old', 'theme2': 'new', 'beneficiary': 'purpose']).
fnpattern(replace, 13060000, 'replacing', ['agent': 'agent', 'theme': 'old', 'theme2': 'new', 'beneficiary': 'reason']).
fnpattern(replace, 13060000, 'replacing', ['agent': 'agent', 'theme1': 'old', 'theme2': 'new', 'beneficiary': 'purpose']).
fnpattern(replace, 13060000, 'replacing', ['agent': 'agent', 'theme1': 'old', 'theme2': 'new', 'beneficiary': 'reason']).
fnpattern(acquire, 14000000, 'getting', ['agent': 'recipient', 'topic': 'theme', 'source': 'source']).
fnpattern(read, 14000100, 'reading', ['agent': 'reader', 'topic': 'text']).
fnpattern(study, 14000100, 'education_teaching', ['agent': 'student', 'topic': 'subject', 'source': 'teacher']).
fnpattern(read, 14000210, 'reading', ['agent': 'reader', 'topic': 'text']).
fnpattern(clasp, 15010100, 'manipulation', ['agent': 'agent', 'theme': 'entity']).
fnpattern(clutch, 15010100, 'manipulation', ['agent': 'agent', 'theme': 'entity']).
fnpattern(grasp, 15010100, 'manipulation', ['agent': 'agent', 'theme': 'entity']).
fnpattern(grip, 15010100, 'manipulation', ['agent': 'agent', 'theme': 'entity']).
fnpattern(block, 16000000, 'eclipse', ['agent': 'obstruction', 'patient': 'eclipsed']).
fnpattern(conceal, 16000000, 'eclipse', ['agent': 'obstruction', 'patient': 'eclipsed']).
fnpattern(hide, 16000000, 'eclipse', ['agent': 'obstruction', 'patient': 'eclipsed']).
fnpattern(screen, 16000000, 'eclipse', ['agent': 'obstruction', 'patient': 'eclipsed']).
fnpattern(hide, 16000100, 'eclipse', ['agent': 'obstruction', 'patient': 'eclipsed']).
fnpattern(discard, 17010000, 'removing', ['agent': 'agent', 'cause': 'cause', 'theme': 'theme', 'source': 'source', 'destination': 'goal']).
fnpattern(chuck, 17010100, 'cause_motion', ['agent': 'agent', 'cause': 'cause', 'theme': 'theme', 'source': 'source', 'destination': 'goal']).
fnpattern(fire, 17010100, 'shoot_projectiles', ['agent': 'agent', 'theme': 'projectile', 'source': 'firearm']).
fnpattern(fling, 17010100, 'cause_motion', ['agent': 'agent', 'cause': 'cause', 'theme': 'theme', 'source': 'source', 'destination': 'goal']).
fnpattern(hit, 17010100, 'cause_impact', ['agent': 'agent', 'theme': 'impactor', 'destination': 'impactee']).
fnpattern(hit, 17010100, 'cause_impact', ['cause': 'agent', 'theme': 'impactor', 'destination': 'impactee']).
fnpattern(hurl, 17010100, 'cause_motion', ['agent': 'agent', 'cause': 'cause', 'theme': 'theme', 'source': 'source', 'destination': 'goal']).
fnpattern(nudge, 17010100, 'cause_motion', ['agent': 'agent', 'cause': 'cause', 'theme': 'theme', 'source': 'source', 'destination': 'goal']).
fnpattern(pitch, 17010100, 'cause_motion', ['agent': 'agent', 'cause': 'cause', 'theme': 'theme', 'source': 'source', 'destination': 'goal']).
fnpattern(shoot, 17010100, 'shoot_projectiles', ['agent': 'agent', 'theme': 'projectile', 'source': 'firearm']).
fnpattern(shove, 17010100, 'cause_motion', ['agent': 'agent', 'cause': 'cause', 'theme': 'theme', 'source': 'source', 'destination': 'goal']).
fnpattern(slam, 17010100, 'cause_impact', ['agent': 'agent', 'theme': 'impactor', 'destination': 'impactee']).
fnpattern(slam, 17010100, 'cause_impact', ['cause': 'agent', 'theme': 'impactor', 'destination': 'impactee']).
fnpattern(slap, 17010100, 'cause_impact', ['agent': 'agent', 'theme': 'impactor', 'destination': 'impactee']).
fnpattern(slap, 17010100, 'cause_impact', ['cause': 'agent', 'theme': 'impactor', 'destination': 'impactee']).
fnpattern(catapult, 17010110, 'cause_motion', ['agent': 'agent', 'cause': 'cause', 'theme': 'theme', 'source': 'source', 'destination': 'goal']).
fnpattern(throw, 17010110, 'cause_motion', ['agent': 'agent', 'cause': 'cause', 'theme': 'theme', 'source': 'source', 'destination': 'goal']).
fnpattern(toss, 17010110, 'cause_motion', ['agent': 'agent', 'cause': 'cause', 'theme': 'theme', 'source': 'source', 'destination': 'goal']).
fnpattern(buffet, 17020000, 'cause_harm', ['agent': 'agent', 'theme': 'victim']).
fnpattern(buffet, 17020000, 'cause_harm', ['agent': 'agent', 'theme': 'body_part']).
fnpattern(buffet, 17020000, 'cause_harm', ['agent': 'cause', 'theme': 'victim']).
fnpattern(buffet, 17020000, 'cause_harm', ['agent': 'cause', 'theme': 'body_part']).
fnpattern(pelt, 17020000, 'cause_harm', ['agent': 'agent', 'theme': 'victim']).
fnpattern(pelt, 17020000, 'cause_harm', ['agent': 'agent', 'theme': 'body_part']).
fnpattern(pelt, 17020000, 'cause_harm', ['agent': 'cause', 'theme': 'victim']).
fnpattern(pelt, 17020000, 'cause_harm', ['agent': 'cause', 'theme': 'body_part']).
fnpattern(stone, 17020000, 'cause_harm', ['agent': 'agent', 'theme': 'victim']).
fnpattern(stone, 17020000, 'cause_harm', ['agent': 'agent', 'theme': 'body_part']).
fnpattern(stone, 17020000, 'cause_harm', ['agent': 'cause', 'theme': 'victim']).
fnpattern(stone, 17020000, 'cause_harm', ['agent': 'cause', 'theme': 'body_part']).
fnpattern(bang, 18010100, 'cause_impact', ['agent': 'agent', 'instrument': 'impactor', 'patient': 'impactee']).
fnpattern(bang, 18010100, 'impact', ['instrument': 'impactor', 'patient': 'impactee']).
fnpattern(bash, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(bash, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(bash, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(bash, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(batter, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(batter, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(batter, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(batter, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(beat, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(beat, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(beat, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(beat, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(bump, 18010100, 'impact', ['instrument': 'impactor', 'patient': 'impactee']).
fnpattern(butt, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(butt, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(butt, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(butt, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(click, 18010100, 'cause_impact', ['agent': 'agent', 'instrument': 'impactor', 'patient': 'impactee']).
fnpattern(click, 18010100, 'impact', ['instrument': 'impactor', 'patient': 'impactee']).
fnpattern(hammer, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(hammer, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(hammer, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(hammer, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(hit, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(hit, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(hit, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(hit, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(hit, 18010100, 'cause_impact', ['agent': 'agent', 'instrument': 'impactor', 'patient': 'impactee']).
fnpattern(hit, 18010100, 'hit_target', ['agent': 'agent', 'patient': 'target']).
fnpattern(hit, 18010100, 'hit_target', ['instrument': 'agent', 'patient': 'target']).
fnpattern(hit, 18010100, 'impact', ['instrument': 'impactor', 'patient': 'impactee']).
fnpattern(kick, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(kick, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(kick, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(kick, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(knock, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(knock, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(knock, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(knock, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(lash, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(lash, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(lash, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(lash, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(rap, 18010100, 'cause_impact', ['agent': 'agent', 'instrument': 'impactor', 'patient': 'impactee']).
fnpattern(rap, 18010100, 'impact', ['instrument': 'impactor', 'patient': 'impactee']).
fnpattern(slap, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(slap, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(slap, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(slap, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(slap, 18010100, 'cause_impact', ['agent': 'agent', 'instrument': 'impactor', 'patient': 'impactee']).
fnpattern(slap, 18010100, 'impact', ['instrument': 'impactor', 'patient': 'impactee']).
fnpattern(smack, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(smack, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(smack, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(smack, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(smack, 18010100, 'cause_impact', ['agent': 'agent', 'instrument': 'impactor', 'patient': 'impactee']).
fnpattern(smack, 18010100, 'impact', ['instrument': 'impactor', 'patient': 'impactee']).
fnpattern(strike, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(strike, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(strike, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(strike, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(strike, 18010100, 'cause_impact', ['agent': 'agent', 'instrument': 'impactor', 'patient': 'impactee']).
fnpattern(strike, 18010100, 'impact', ['instrument': 'impactor', 'patient': 'impactee']).
fnpattern(thump, 18010100, 'cause_impact', ['agent': 'agent', 'instrument': 'impactor', 'patient': 'impactee']).
fnpattern(thump, 18010100, 'impact', ['instrument': 'impactor', 'patient': 'impactee']).
fnpattern(thwack, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(thwack, 18010100, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(thwack, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(thwack, 18010100, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(claw, 18020000, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(claw, 18020000, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(claw, 18020000, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(claw, 18020000, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(punch, 18020000, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(punch, 18020000, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(punch, 18020000, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(punch, 18020000, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(stab, 18020000, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(stab, 18020000, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(stab, 18020000, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(stab, 18020000, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(belt, 18030000, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(belt, 18030000, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(belt, 18030000, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(belt, 18030000, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(bludgeon, 18030000, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(bludgeon, 18030000, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(bludgeon, 18030000, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(bludgeon, 18030000, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(club, 18030000, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(club, 18030000, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(club, 18030000, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(club, 18030000, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(cudgel, 18030000, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(cudgel, 18030000, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(cudgel, 18030000, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(cudgel, 18030000, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(cuff, 18030000, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(cuff, 18030000, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(cuff, 18030000, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(cuff, 18030000, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(knife, 18030000, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(knife, 18030000, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(knife, 18030000, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(knife, 18030000, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(pummel, 18030000, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(pummel, 18030000, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(pummel, 18030000, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(pummel, 18030000, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(whip, 18030000, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(whip, 18030000, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(whip, 18030000, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(whip, 18030000, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(hit, 18040000, 'impact', ['theme': 'impactor', 'location': 'impactee']).
fnpattern(bang, 18040100, 'impact', ['theme': 'impactor', 'location': 'impactee']).
fnpattern(bump, 18040100, 'impact', ['theme': 'impactor', 'location': 'impactee']).
fnpattern(crash, 18040100, 'impact', ['theme': 'impactor', 'location': 'impactee']).
fnpattern(slam, 18040100, 'impact', ['theme': 'impactor', 'location': 'impactee']).
fnpattern(smash, 18040100, 'impact', ['theme': 'impactor', 'location': 'impactee']).
fnpattern(thud, 18040100, 'impact', ['theme': 'impactor', 'location': 'impactee']).
fnpattern(jab, 19000000, 'cause_harm', ['agent': 'agent', 'destination': 'victim']).
fnpattern(jab, 19000000, 'cause_harm', ['agent': 'agent', 'destination': 'body_part']).
fnpattern(jab, 19000000, 'cause_harm', ['agent': 'cause', 'destination': 'victim']).
fnpattern(jab, 19000000, 'cause_harm', ['agent': 'cause', 'destination': 'body_part']).
fnpattern(graze, 20000100, 'cause_impact', ['agent': 'agent', 'instrument': 'impactor', 'experiencer': 'impactee']).
fnpattern(graze, 20000100, 'impact', ['instrument': 'impactor', 'experiencer': 'impactee']).
fnpattern(chip, 21010100, 'damaging', ['agent': 'agent', 'patient': 'patient']).
fnpattern(cut, 21010100, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(cut, 21010100, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(cut, 21010100, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(cut, 21010100, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(cut, 21010100, 'experience_bodily_harm', ['agent': 'agent', 'patient': 'patient', 'instrument': 'injuring_entity']).
fnpattern(scratch, 21010100, 'damaging', ['agent': 'agent', 'patient': 'patient']).
fnpattern(bruise, 21020100, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(bruise, 21020100, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(bruise, 21020100, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(bruise, 21020100, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(chip, 21020100, 'damaging', ['agent': 'agent', 'patient': 'patient']).
fnpattern(crush, 21020100, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(crush, 21020100, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(crush, 21020100, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(crush, 21020100, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(crush, 21020100, 'grinding', ['agent': 'grinder', 'patient': 'undergoer', 'instrument': 'instrument']).
fnpattern(dent, 21020100, 'damaging', ['agent': 'agent', 'patient': 'patient']).
fnpattern(gash, 21020100, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(gash, 21020100, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(gash, 21020100, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(gash, 21020100, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(grate, 21020100, 'grinding', ['agent': 'grinder', 'patient': 'undergoer', 'instrument': 'instrument']).
fnpattern(grind, 21020100, 'grinding', ['agent': 'grinder', 'patient': 'undergoer', 'instrument': 'instrument']).
fnpattern(nick, 21020100, 'damaging', ['agent': 'agent', 'patient': 'patient']).
fnpattern(pulverize, 21020100, 'grinding', ['agent': 'grinder', 'patient': 'undergoer', 'instrument': 'instrument']).
fnpattern(shred, 21020100, 'cause_to_fragment', ['agent': 'agent', 'patient': 'whole_patient']).
fnpattern(shred, 21020100, 'grinding', ['agent': 'grinder', 'patient': 'undergoer', 'instrument': 'instrument']).
fnpattern(slice, 21020100, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(slice, 21020100, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(slice, 21020100, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(slice, 21020100, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(squash, 21020100, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(squash, 21020100, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(squash, 21020100, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(squash, 21020100, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(chop, 21020200, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(chop, 21020200, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(chop, 21020200, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(chop, 21020200, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(mill, 21020200, 'grinding', ['agent': 'grinder', 'patient': 'undergoer', 'instrument': 'instrument']).
fnpattern(punch, 21020200, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(punch, 21020200, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(punch, 21020200, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(punch, 21020200, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(spear, 21020200, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(spear, 21020200, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(spear, 21020200, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(spear, 21020200, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(blend, 22010110, 'amalgamation', ['patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(blend, 22010110, 'cause_to_amalgamate', ['agent': 'agent', 'patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(combine, 22010110, 'amalgamation', ['patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(combine, 22010110, 'cause_to_amalgamate', ['agent': 'agent', 'patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(commingle, 22010110, 'amalgamation', ['patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(commingle, 22010110, 'cause_to_amalgamate', ['agent': 'agent', 'patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(fuse, 22010110, 'amalgamation', ['patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(fuse, 22010110, 'cause_to_amalgamate', ['agent': 'agent', 'patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(merge, 22010110, 'amalgamation', ['patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(merge, 22010110, 'cause_to_amalgamate', ['agent': 'agent', 'patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(mix, 22010110, 'cause_to_amalgamate', ['agent': 'agent', 'patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(connect, 22010210, 'make_cognitive_connection', ['agent': 'cognizer', 'patient1': 'concept_1', 'patient2': 'concept_2']).
fnpattern(join, 22010210, 'cause_to_amalgamate', ['agent': 'agent', 'patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(link, 22010210, 'make_cognitive_connection', ['agent': 'cognizer', 'patient1': 'concept_1', 'patient2': 'concept_2']).
fnpattern(match, 22020100, 'compatibility', ['patient1': 'item_1', 'patient2': 'item_2', 'patient': 'items']).
fnpattern(amalgamate, 22020110, 'amalgamation', ['patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(amalgamate, 22020110, 'cause_to_amalgamate', ['agent': 'agent', 'patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(harmonize, 22020210, 'compatibility', ['patient1': 'item_1', 'patient2': 'item_2', 'patient': 'items']).
fnpattern(rhyme, 22020210, 'compatibility', ['patient1': 'item_1', 'patient2': 'item_2', 'patient': 'items']).
fnpattern(unify, 22020210, 'amalgamation', ['patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(unify, 22020210, 'cause_to_amalgamate', ['agent': 'agent', 'patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(unite, 22020210, 'amalgamation', ['patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(unite, 22020210, 'cause_to_amalgamate', ['agent': 'agent', 'patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(lump, 22030100, 'cause_to_amalgamate', ['agent': 'agent', 'patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(fuse, 22030110, 'amalgamation', ['patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(fuse, 22030110, 'cause_to_amalgamate', ['agent': 'agent', 'patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(sew, 22030200, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(stick, 22030200, 'inchoative_attaching', ['patient1': 'connector', 'patient2': 'goal']).
fnpattern(attach, 22030210, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(attach, 22030210, 'inchoative_attaching', ['patient1': 'connector', 'patient2': 'goal']).
fnpattern(bind, 22030210, 'inchoative_attaching', ['patient1': 'connector', 'patient2': 'goal']).
fnpattern(bond, 22030210, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(fasten, 22030210, 'inchoative_attaching', ['patient1': 'connector', 'patient2': 'goal']).
fnpattern(moor, 22030210, 'inchoative_attaching', ['patient1': 'connector', 'patient2': 'goal']).
fnpattern(weld, 22030210, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(anchor, 22040000, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(cement, 22040000, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(chain, 22040000, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(fetter, 22040000, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(glue, 22040000, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(handcuff, 22040000, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(hitch, 22040000, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(hook, 22040000, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(lash, 22040000, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(manacle, 22040000, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(moor, 22040000, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(nail, 22040000, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(pin, 22040000, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(plaster, 22040000, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(rivet, 22040000, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(shackle, 22040000, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(solder, 22040000, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(staple, 22040000, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(tack, 22040000, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(tape, 22040000, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(tether, 22040000, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(tie, 22040000, 'attaching', ['agent': 'agent', 'patient1': 'connector', 'patient2': 'goal']).
fnpattern(divide, 23010100, 'separation', ['agent': 'agent', 'patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(segregate, 23010100, 'separation', ['agent': 'agent', 'patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(part, 23010200, 'separation', ['agent': 'agent', 'patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(separate, 23010200, 'separation', ['agent': 'agent', 'patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(break, 23020000, 'cause_to_fragment', ['agent': 'agent', 'patient1': 'whole_patient']).
fnpattern(rip, 23020000, 'cause_to_fragment', ['agent': 'agent', 'patient1': 'whole_patient']).
fnpattern(split, 23020000, 'separation', ['patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(tear, 23020000, 'cause_to_fragment', ['agent': 'agent', 'patient1': 'whole_patient']).
fnpattern(detach, 23030000, 'attaching', ['agent': 'agent', 'patient1': 'item', 'patient2': 'goal']).
fnpattern(dismantle, 23030000, 'destroying', ['agent': 'agent', 'patient': 'undergoer']).
fnpattern(dismantle, 23030000, 'destroying', ['agent': 'destroyer', 'patient': 'undergoer']).
fnpattern(partition, 23030000, 'separation', ['agent': 'agent', 'patient1': 'part_1', 'patient2': 'part_2']).
fnpattern(unbuckle, 23030000, 'closure', ['agent': 'agent', 'patient1': 'fastener', 'patient2': 'containing_object']).
fnpattern(unfasten, 23030000, 'closure', ['agent': 'agent', 'patient1': 'fastener', 'patient2': 'containing_object']).
fnpattern(unscrew, 23030000, 'closure', ['agent': 'agent', 'patient1': 'fastener', 'patient2': 'containing_object']).
fnpattern(unzip, 23030000, 'closure', ['agent': 'agent', 'patient1': 'fastener', 'patient2': 'containing_object']).
fnpattern(dye, 24000000, 'processing_materials', ['agent': 'agent', 'theme': 'material']).
fnpattern(glaze, 24000000, 'filling', ['agent': 'agent', 'theme': 'theme']).
fnpattern(paint, 24000000, 'filling', ['agent': 'agent', 'theme': 'theme']).
fnpattern(stain, 24000000, 'processing_materials', ['agent': 'agent', 'theme': 'material']).
fnpattern(varnish, 24000000, 'filling', ['agent': 'agent', 'theme': 'theme']).
fnpattern(etch, 25010000, 'processing_materials', ['agent': 'agent', 'destination': 'material']).
fnpattern(copy, 25020000, 'duplication', ['theme': 'copy', 'agent': 'creator']).
fnpattern(forge, 25020000, 'imitation', ['theme': 'text', 'agent': 'author']).
fnpattern(misspell, 25020000, 'spelling_and_pronouncing', ['theme': 'formal_realization', 'agent': 'speaker', 'destination': 'context']).
fnpattern(print, 25020000, 'text_creation', ['theme': 'text', 'agent': 'author']).
fnpattern(spell, 25020000, 'spelling_and_pronouncing', ['theme': 'formal_realization', 'agent': 'speaker', 'destination': 'context']).
fnpattern(type, 25020000, 'text_creation', ['theme': 'text', 'agent': 'author']).
fnpattern(write, 25020000, 'text_creation', ['theme': 'text', 'agent': 'author']).
fnpattern(gild, 25030000, 'filling', ['agent': 'agent', 'destination': 'goal', 'theme': 'theme']).
fnpattern(chronicle, 25040000, 'text_creation', ['theme': 'text', 'agent': 'author', 'destination': 'addressee']).
fnpattern(chronicle, 25040000, 'text_creation', ['theme': 'text', 'agent': 'author', 'destination': 'place']).
fnpattern(copy, 25040000, 'duplication', ['theme': 'copy', 'agent': 'creator']).
fnpattern(forge, 25040000, 'imitation', ['theme': 'copy', 'agent': 'creator']).
fnpattern(photocopy, 25040000, 'duplication', ['theme': 'copy', 'agent': 'creator']).
fnpattern(type, 25040000, 'text_creation', ['theme': 'text', 'agent': 'author', 'destination': 'addressee']).
fnpattern(type, 25040000, 'text_creation', ['theme': 'text', 'agent': 'author', 'destination': 'place']).
fnpattern(assemble, 26010000, 'building', ['material': 'components', 'product': 'created_entity', 'agent': 'agent']).
fnpattern(bake, 26010000, 'cooking_creation', ['product': 'produced_food', 'agent': 'cook']).
fnpattern(cook, 26010000, 'cooking_creation', ['product': 'produced_food', 'agent': 'cook']).
fnpattern(cook_up, 26010000, 'invention', ['product': 'invention', 'agent': 'cognizer']).
fnpattern(formulate, 26010000, 'invention', ['product': 'invention', 'agent': 'cognizer']).
fnpattern(hatch, 26010000, 'invention', ['product': 'invention', 'agent': 'cognizer']).
fnpattern(build, 26010100, 'building', ['material': 'components', 'product': 'created_entity', 'agent': 'agent']).
fnpattern(make, 26010100, 'building', ['material': 'components', 'product': 'created_entity', 'agent': 'agent']).
fnpattern(make, 26010100, 'cooking_creation', ['product': 'produced_food', 'agent': 'cook']).
fnpattern(make, 26010100, 'intentionally_create', ['product': 'created_entity', 'agent': 'creator']).
fnpattern(make, 26010100, 'manufacturing', ['product': 'product', 'agent': 'manufacturer']).
fnpattern(develop, 26020000, 'coming_to_be', ['product': 'entity', 'material': 'components']).
fnpattern(hatch, 26020000, 'invention', ['product': 'invention', 'agent': 'cognizer']).
fnpattern(bake, 26030100, 'cooking_creation', ['product': 'produced_food', 'agent': 'cook']).
fnpattern(cook, 26030100, 'cooking_creation', ['product': 'produced_food', 'agent': 'cook']).
fnpattern(light, 26030100, 'setting_fire', ['product': 'flamables', 'agent': 'kindler']).
fnpattern(prepare, 26030100, 'cooking_creation', ['product': 'produced_food', 'agent': 'cook']).
fnpattern(coin, 26040000, 'achieving_first', ['product': 'new_idea', 'agent': 'cognizer']).
fnpattern(coin, 26040000, 'invention', ['product': 'invention', 'agent': 'cognizer']).
fnpattern(compose, 26040000, 'text_creation', ['product': 'text', 'agent': 'author']).
fnpattern(concoct, 26040000, 'cooking_creation', ['product': 'produced_food', 'material': 'ingredients', 'agent': 'cook']).
fnpattern(concoct, 26040000, 'invention', ['product': 'invention', 'agent': 'cognizer']).
fnpattern(construct, 26040000, 'building', ['product': 'created_entity', 'material': 'components', 'agent': 'agent']).
fnpattern(contrive, 26040000, 'invention', ['product': 'invention', 'agent': 'cognizer']).
fnpattern(create, 26040000, 'intentionally_create', ['product': 'created_entity', 'agent': 'creator']).
fnpattern(invent, 26040000, 'achieving_first', ['product': 'new_idea', 'agent': 'cognizer']).
fnpattern(invent, 26040000, 'invention', ['product': 'invention', 'agent': 'cognizer']).
fnpattern(manufacture, 26040000, 'manufacturing', ['product': 'product', 'material': 'resource', 'agent': 'manufacturer']).
fnpattern(produce, 26040000, 'manufacturing', ['product': 'product', 'material': 'resource', 'agent': 'manufacturer']).
fnpattern(design, 26040100, 'invention', ['product': 'invention', 'agent': 'cognizer']).
fnpattern(stage, 26040100, 'behind_the_scenes', ['product': 'production', 'material': 'medium', 'agent': 'artist']).
fnpattern(bend, 26050000, 'reshaping', ['product': 'result', 'material': 'undergoer', 'agent': 'deformer']).
fnpattern(fold, 26050000, 'reshaping', ['product': 'result', 'material': 'undergoer', 'agent': 'deformer']).
fnpattern(knead, 26050000, 'manipulation', ['agent': 'agent', 'product': 'entity']).
fnpattern(squash, 26050000, 'reshaping', ['product': 'result', 'material': 'undergoer', 'agent': 'deformer']).
fnpattern(squeeze, 26050000, 'manipulation', ['agent': 'agent', 'product': 'entity']).
fnpattern(squish, 26050000, 'reshaping', ['product': 'result', 'material': 'undergoer', 'agent': 'deformer']).
fnpattern(deform, 26061000, 'reshaping', ['agent': 'deformer', 'material': 'undergoer', 'product': 'result']).
fnpattern(translate, 26061000, 'categorization', ['agent': 'cognizer', 'patient': 'item', 'product': 'category']).
fnpattern(switch, 26062100, 'replacing', ['agent': 'agent', 'source': 'old', 'destination': 'new']).
fnpattern(direct, 26070100, 'behind_the_scenes', ['agent': 'artist', 'theme': 'production', 'beneficiary': 'studio']).
fnpattern(improvise, 26070100, 'invention', ['agent': 'cognizer', 'theme': 'invention']).
fnpattern(play, 26070110, 'performers_and_roles', ['agent': 'performer', 'theme': 'performance', 'beneficiary': 'audience']).
fnpattern(compose, 26070200, 'text_creation', ['agent': 'author', 'theme': 'text', 'beneficiary': 'purpose']).
fnpattern(produce, 26070200, 'behind_the_scenes', ['agent': 'artist', 'theme': 'production', 'beneficiary': 'studio']).
fnpattern(write, 26070210, 'text_creation', ['agent': 'author', 'theme': 'text', 'beneficiary': 'purpose']).
fnpattern(cause, 27000000, 'causation', ['theme1': 'cause', 'theme2': 'effect']).
fnpattern(create, 27000000, 'cause_to_start', ['theme1': 'cause', 'theme2': 'effect']).
fnpattern(engender, 27000000, 'cause_to_start', ['theme1': 'cause', 'theme2': 'effect']).
fnpattern(generate, 27000000, 'cause_to_start', ['theme1': 'cause', 'theme2': 'effect']).
fnpattern(sire, 27000000, 'birth', ['theme1': 'mother', 'theme2': 'child']).
fnpattern(calve, 28000000, 'birth', ['agent': 'parents', 'patient': 'child']).
fnpattern(spawn, 28000000, 'birth', ['agent': 'parents', 'patient': 'child']).
fnpattern(whelp, 28000000, 'birth', ['agent': 'parents', 'patient': 'child']).
fnpattern(appoint, 29010100, 'change_of_leadership', ['agent': 'selector', 'theme': 'new_leader', 'predicate': 'role']).
fnpattern(elect, 29010100, 'change_of_leadership', ['agent': 'selector', 'theme': 'new_leader', 'predicate': 'role']).
fnpattern(name, 29010100, 'name_conferral', ['agent': 'agent', 'theme': 'entity', 'predicate': 'name']).
fnpattern(imagine, 29010200, 'awareness', ['agent': 'cognizer', 'theme': 'content']).
fnpattern(reckon, 29010200, 'awareness', ['agent': 'cognizer', 'theme': 'content']).
fnpattern(depict, 29020000, 'communicate_categorization', ['agent': 'speaker', 'theme': 'item', 'predicate': 'category']).
fnpattern(cite, 29020100, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'role']).
fnpattern(class, 29020100, 'categorization', ['agent': 'cognizer', 'theme': 'item', 'predicate': 'category']).
fnpattern(classify, 29020100, 'categorization', ['agent': 'cognizer', 'theme': 'item', 'predicate': 'category']).
fnpattern(count, 29020100, 'categorization', ['agent': 'cognizer', 'theme': 'item', 'predicate': 'category']).
fnpattern(address, 29020200, 'speak_on_topic', ['agent': 'speaker', 'theme': 'topic']).
fnpattern(address, 29020200, 'topic', ['agent': 'communicator', 'theme': 'topic']).
fnpattern(appreciate, 29020200, 'judgment', ['agent': 'cognizer', 'theme': 'evaluee', 'predicate': 'role']).
fnpattern(characterize, 29020200, 'communicate_categorization', ['agent': 'speaker', 'theme': 'item', 'predicate': 'category']).
fnpattern(know, 29020200, 'awareness', ['agent': 'cognizer', 'theme': 'content']).
fnpattern(perceive, 29020200, 'categorization', ['agent': 'cognizer', 'theme': 'item', 'predicate': 'category']).
fnpattern(recollect, 29020200, 'memory', ['agent': 'cognizer', 'theme': 'content']).
fnpattern(regard, 29020200, 'categorization', ['agent': 'cognizer', 'theme': 'item', 'predicate': 'category']).
fnpattern(remember, 29020200, 'memory', ['agent': 'cognizer', 'theme': 'content']).
fnpattern(treat, 29020200, 'communicate_categorization', ['agent': 'speaker', 'theme': 'item', 'predicate': 'category']).
fnpattern(treat, 29020200, 'topic', ['agent': 'communicator', 'theme': 'topic']).
fnpattern(value, 29020200, 'judgment', ['agent': 'cognizer', 'theme': 'evaluee', 'predicate': 'role']).
fnpattern(portray, 29020300, 'communicate_categorization', ['agent': 'speaker', 'theme': 'item', 'predicate': 'category']).
fnpattern(praise, 29020300, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'role']).
fnpattern(represent, 29020300, 'communicate_categorization', ['agent': 'speaker', 'theme': 'item', 'predicate': 'category']).
fnpattern(stigmatize, 29020300, 'judgment', ['agent': 'cognizer', 'theme': 'evaluee', 'predicate': 'role']).
fnpattern(characterize, 29020400, 'communicate_categorization', ['agent': 'speaker', 'theme': 'item', 'predicate': 'category']).
fnpattern(define, 29020400, 'communicate_categorization', ['agent': 'speaker', 'theme': 'item', 'predicate': 'category']).
fnpattern(describe, 29020400, 'communicate_categorization', ['agent': 'speaker', 'theme': 'item', 'predicate': 'category']).
fnpattern(dub, 29030000, 'name_conferral', ['agent': 'speaker', 'theme': 'entity', 'predicate': 'name']).
fnpattern(baptize, 29030100, 'name_conferral', ['agent': 'speaker', 'theme': 'entity', 'predicate': 'name']).
fnpattern(christen, 29030100, 'name_conferral', ['agent': 'speaker', 'theme': 'entity', 'predicate': 'name']).
fnpattern(name, 29030100, 'name_conferral', ['agent': 'speaker', 'theme': 'entity', 'predicate': 'name']).
fnpattern(nickname, 29030100, 'name_conferral', ['agent': 'speaker', 'theme': 'entity', 'predicate': 'name']).
fnpattern(avow, 29040100, 'statement', ['agent': 'speaker', 'theme': 'addressee', 'predicate': 'message']).
fnpattern(confess, 29040100, 'statement', ['agent': 'speaker', 'theme': 'addressee', 'predicate': 'message']).
fnpattern(declare, 29040100, 'statement', ['agent': 'speaker', 'theme': 'addressee', 'predicate': 'message']).
fnpattern(profess, 29040100, 'statement', ['agent': 'speaker', 'theme': 'addressee', 'predicate': 'message']).
fnpattern(believe, 29040200, 'awareness', ['agent': 'cognizer', 'theme': 'content']).
fnpattern(find, 29040200, 'becoming_aware', ['agent': 'cognizer', 'theme': 'phenomenon', 'predicate': 'state']).
fnpattern(presume, 29040200, 'awareness', ['agent': 'cognizer', 'theme': 'content']).
fnpattern(think, 29040200, 'awareness', ['agent': 'cognizer', 'theme': 'content']).
fnpattern(conjecture, 29050100, 'statement', ['agent': 'speaker', 'theme': 'addressee', 'predicate': 'message']).
fnpattern(doubt, 29050100, 'certainty', ['agent': 'cognizer', 'theme': 'content']).
fnpattern(foresee, 29050100, 'expectation', ['agent': 'cognizer', 'theme': 'phenomenon']).
fnpattern(foresee, 29050100, 'expectation', ['agent': 'cognizer', 'theme': 'topic']).
fnpattern(foretell, 29050100, 'predicting', ['agent': 'speaker', 'theme': 'eventuality']).
fnpattern(guess, 29050100, 'coming_to_believe', ['agent': 'cognizer', 'theme': 'content']).
fnpattern(know, 29050100, 'awareness', ['agent': 'cognizer', 'theme': 'content']).
fnpattern(prophesy, 29050100, 'predicting', ['agent': 'speaker', 'theme': 'eventuality']).
fnpattern(realize, 29050100, 'coming_to_believe', ['agent': 'cognizer', 'theme': 'content']).
fnpattern(recognize, 29050100, 'becoming_aware', ['agent': 'cognizer', 'theme': 'phenomenon', 'predicate': 'state']).
fnpattern(surmise, 29050100, 'coming_to_believe', ['agent': 'cognizer', 'theme': 'content']).
fnpattern(suspect, 29050100, 'awareness', ['agent': 'cognizer', 'theme': 'content']).
fnpattern(vaticinate, 29050100, 'predicting', ['agent': 'speaker', 'theme': 'eventuality']).
fnpattern(admit, 29050200, 'statement', ['agent': 'speaker', 'theme': 'addressee', 'predicate': 'message']).
fnpattern(assert, 29050200, 'statement', ['agent': 'speaker', 'theme': 'addressee', 'predicate': 'message']).
fnpattern(discover, 29050200, 'becoming_aware', ['agent': 'cognizer', 'theme': 'phenomenon', 'predicate': 'state']).
fnpattern(maintain, 29050200, 'statement', ['agent': 'speaker', 'theme': 'addressee', 'predicate': 'message']).
fnpattern(rate, 29060000, 'assessing', ['agent': 'assessor']).
fnpattern(volunteer, 29080000, 'commitment', ['agent': 'speaker', 'theme': 'addressee']).
fnpattern(bully, 29080100, 'manipulate_into_doing', ['agent': 'manipulator', 'theme': 'victim']).
fnpattern(butcher, 29080100, 'killing', ['agent': 'killer', 'theme': 'victim']).
fnpattern(clerk, 29080100, 'being_employed', ['agent': 'employee', 'theme': 'task']).
fnpattern(escort, 29080100, 'cotheme', ['agent': 'source', 'theme': 'new_leader']).
fnpattern(host, 29080100, 'social_event', ['agent': 'host', 'theme': 'social_event']).
fnpattern(partner, 29080100, 'collaboration', ['agent': 'partners', 'theme': 'undertaking']).
fnpattern(pioneer, 29080100, 'achieving_first', ['agent': 'cognizer', 'theme': 'new_idea']).
fnpattern(shepherd, 29080100, 'cotheme', ['agent': 'source', 'theme': 'new_leader']).
fnpattern(tutor, 29080100, 'education_teaching', ['agent': 'teacher', 'theme': 'student']).
fnpattern(usher, 29080100, 'cotheme', ['agent': 'source', 'theme': 'new_leader']).
fnpattern(volunteer, 29080100, 'commitment', ['agent': 'speaker', 'theme': 'addressee']).
fnpattern(star, 29080110, 'performers_and_roles', ['agent': 'performer', 'theme': 'audience']).
fnpattern(consider, 29090111, 'categorization', ['agent': 'cognizer', 'theme': 'item', 'predicate': 'category']).
fnpattern(consider, 29090111, 'cogitation', ['agent': 'cognizer', 'theme': 'topic']).
fnpattern(detect, 30010000, 'perception_experience', ['experiencer': 'perceiver', 'stimulus': 'phenomenon']).
fnpattern(discern, 30010000, 'becoming_aware', ['experiencer': 'cognizer', 'stimulus': 'phenomenon']).
fnpattern(feel, 30010000, 'perception_experience', ['experiencer': 'perceiver', 'stimulus': 'phenomenon']).
fnpattern(notice, 30010000, 'becoming_aware', ['experiencer': 'cognizer', 'stimulus': 'phenomenon']).
fnpattern(see, 30010000, 'perception_experience', ['experiencer': 'perceiver', 'stimulus': 'phenomenon']).
fnpattern(sense, 30010000, 'perception_experience', ['experiencer': 'perceiver', 'stimulus': 'phenomenon']).
fnpattern(smell, 30010000, 'perception_experience', ['experiencer': 'perceiver', 'stimulus': 'phenomenon']).
fnpattern(taste, 30010000, 'perception_experience', ['experiencer': 'perceiver', 'stimulus': 'phenomenon']).
fnpattern(hear, 30010100, 'perception_experience', ['experiencer': 'perceiver', 'stimulus': 'phenomenon']).
fnpattern(descry, 30020000, 'becoming_aware', ['experiencer': 'cognizer', 'stimulus': 'phenomenon']).
fnpattern(discover, 30020000, 'becoming_aware', ['experiencer': 'cognizer', 'stimulus': 'phenomenon']).
fnpattern(espy, 30020000, 'becoming_aware', ['experiencer': 'cognizer', 'stimulus': 'phenomenon']).
fnpattern(experience, 30020000, 'feeling', ['experiencer': 'experiencer', 'stimulus': 'emotion']).
fnpattern(experience, 30020000, 'feeling', ['experiencer': 'experiencer', 'stimulus': 'emotional_state']).
fnpattern(note, 30020000, 'becoming_aware', ['experiencer': 'cognizer', 'stimulus': 'phenomenon']).
fnpattern(observe, 30020000, 'perception_active', ['experiencer': 'perceiver_agentive', 'stimulus': 'phenomenon']).
fnpattern(overhear, 30020000, 'perception_experience', ['experiencer': 'perceiver_passive', 'stimulus': 'phenomenon']).
fnpattern(perceive, 30020000, 'perception_experience', ['experiencer': 'perceiver_passive', 'stimulus': 'phenomenon']).
fnpattern(peruse, 30020000, 'reading', ['experiencer': 'reader', 'stimulus': 'text']).
fnpattern(peruse, 30020000, 'scrutiny', ['experiencer': 'cognizer', 'stimulus': 'phenomenon']).
fnpattern(recognize, 30020000, 'becoming_aware', ['experiencer': 'cognizer', 'stimulus': 'phenomenon']).
fnpattern(sniff, 30020000, 'perception_active', ['experiencer': 'perceiver_agentive', 'stimulus': 'phenomenon']).
fnpattern(spot, 30020000, 'becoming_aware', ['experiencer': 'cognizer', 'stimulus': 'phenomenon']).
fnpattern(spy, 30020000, 'perception_active', ['experiencer': 'perceiver_agentive', 'stimulus': 'phenomenon']).
fnpattern(view, 30020000, 'perception_active', ['experiencer': 'perceiver_agentive', 'stimulus': 'phenomenon']).
fnpattern(watch, 30020000, 'perception_active', ['experiencer': 'perceiver_agentive', 'stimulus': 'phenomenon']).
fnpattern(gaze, 30030000, 'perception_active', ['experiencer': 'perceiver_agentive', 'stimulus': 'phenomenon']).
fnpattern(glance, 30030000, 'perception_active', ['experiencer': 'perceiver_agentive', 'stimulus': 'phenomenon']).
fnpattern(listen, 30030000, 'perception_active', ['experiencer': 'perceiver_agentive', 'stimulus': 'phenomenon']).
fnpattern(look, 30030000, 'perception_active', ['experiencer': 'perceiver_agentive', 'stimulus': 'phenomenon']).
fnpattern(peek, 30030000, 'perception_active', ['experiencer': 'perceiver_agentive', 'stimulus': 'phenomenon']).
fnpattern(peep, 30030000, 'perception_active', ['experiencer': 'perceiver_agentive', 'stimulus': 'phenomenon']).
fnpattern(peer, 30030000, 'perception_active', ['experiencer': 'perceiver_agentive', 'stimulus': 'phenomenon']).
fnpattern(sniff, 30030000, 'perception_active', ['experiencer': 'perceiver_agentive', 'stimulus': 'phenomenon']).
fnpattern(squint, 30030000, 'perception_active', ['experiencer': 'perceiver_agentive', 'stimulus': 'phenomenon']).
fnpattern(stare, 30030000, 'perception_active', ['experiencer': 'perceiver_agentive', 'stimulus': 'phenomenon']).
fnpattern(feel, 30040000, 'appearance', ['experiencer': 'perceiver_passive', 'stimulus': 'phenomenon']).
fnpattern(feel, 30040000, 'perception_active', ['experiencer': 'perceiver_agentive', 'stimulus': 'phenomenon']).
fnpattern(look, 30040000, 'appearance', ['experiencer': 'perceiver_passive', 'stimulus': 'phenomenon']).
fnpattern(look, 30040000, 'perception_active', ['experiencer': 'perceiver_agentive', 'stimulus': 'phenomenon']).
fnpattern(smell, 30040000, 'appearance', ['experiencer': 'perceiver_passive', 'stimulus': 'phenomenon']).
fnpattern(smell, 30040000, 'perception_active', ['experiencer': 'perceiver_agentive', 'stimulus': 'phenomenon']).
fnpattern(sound, 30040000, 'appearance', ['experiencer': 'perceiver_passive', 'stimulus': 'phenomenon']).
fnpattern(taste, 30040000, 'appearance', ['experiencer': 'perceiver_passive', 'stimulus': 'phenomenon']).
fnpattern(taste, 30040000, 'perception_active', ['experiencer': 'perceiver_agentive', 'stimulus': 'phenomenon']).
fnpattern(abash, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(aggravate, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(aggrieve, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(alarm, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(amaze, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(anger, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(annoy, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(antagonize, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(astonish, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(astound, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(baffle, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(beguile, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(bewilder, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(bewitch, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(bore, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(calm, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(captivate, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(charm, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(cheer, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(comfort, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(confuse, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(console, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(dazzle, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(delight, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(depress, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(disappoint, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(discomfit, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(disconcert, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(discourage, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(dishearten, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(displease, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(distress, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(disturb, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(embarrass, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(enchant, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(enrage, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(entertain, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(enthrall, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(exasperate, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(excite, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(exhilarate, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(fascinate, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(faze, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(flabbergast, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(floor, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(fluster, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(frighten, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(frustrate, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(gall, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(gladden, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(gratify, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(grieve, 31010000, 'experiencer_subj', ['experiencer': 'experiencer', 'cause': 'content']).
fnpattern(hearten, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(humiliate, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(impress, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(incense, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(infuriate, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(interest, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(intimidate, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(intrigue, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(irk, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(irritate, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(madden, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(mollify, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(mortify, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(mystify, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(nettle, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(offend, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(pacify, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(perplex, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(perturb, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(placate, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(puzzle, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(rankle, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(reassure, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(repel, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(revolt, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(rile, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(sadden, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(satisfy, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(scare, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(shake, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(shame, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(shock, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(sicken, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(sober, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(solace, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(soothe, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(spook, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(startle, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(stimulate, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(sting, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(stir, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(stun, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(stupefy, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(surprise, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(terrify, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(thrill, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(tickle, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(torment, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(torture, 31010000, 'cause_harm', ['experiencer': 'victim', 'cause': 'agent']).
fnpattern(torture, 31010000, 'cause_harm', ['experiencer': 'victim', 'cause': 'cause']).
fnpattern(trouble, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(unnerve, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(unsettle, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(vex, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(wow, 31010000, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(abhor, 31020000, 'experiencer_subj', ['experiencer': 'experiencer', 'theme': 'content']).
fnpattern(admire, 31020000, 'judgment', ['experiencer': 'cognizer', 'theme': 'evaluee', 'predicate': 'role']).
fnpattern(adore, 31020000, 'experiencer_subj', ['experiencer': 'experiencer', 'theme': 'content']).
fnpattern(affirm, 31020000, 'statement', ['experiencer': 'speaker', 'theme': 'message', 'predicate': 'topic']).
fnpattern(affirm, 31020000, 'statement', ['experiencer': 'speaker', 'theme': 'message', 'predicate': 'medium']).
fnpattern(appreciate, 31020000, 'judgment', ['experiencer': 'cognizer', 'theme': 'evaluee', 'predicate': 'role']).
fnpattern(deify, 31020000, 'judgment', ['experiencer': 'cognizer', 'theme': 'evaluee', 'predicate': 'role']).
fnpattern(deplore, 31020000, 'judgment', ['experiencer': 'cognizer', 'theme': 'evaluee', 'predicate': 'role']).
fnpattern(despise, 31020000, 'experiencer_subj', ['experiencer': 'experiencer', 'theme': 'content']).
fnpattern(detest, 31020000, 'experiencer_subj', ['experiencer': 'experiencer', 'theme': 'content']).
fnpattern(disdain, 31020000, 'judgment', ['experiencer': 'cognizer', 'theme': 'evaluee', 'predicate': 'role']).
fnpattern(dislike, 31020000, 'experiencer_subj', ['experiencer': 'experiencer', 'theme': 'content']).
fnpattern(dread, 31020000, 'experiencer_subj', ['experiencer': 'experiencer', 'theme': 'content']).
fnpattern(enjoy, 31020000, 'experiencer_subj', ['experiencer': 'experiencer', 'theme': 'content']).
fnpattern(envy, 31020000, 'experiencer_subj', ['experiencer': 'experiencer', 'theme': 'content']).
fnpattern(esteem, 31020000, 'judgment', ['experiencer': 'cognizer', 'theme': 'evaluee', 'predicate': 'role']).
fnpattern(exalt, 31020000, 'judgment', ['experiencer': 'cognizer', 'theme': 'evaluee', 'predicate': 'role']).
fnpattern(execrate, 31020000, 'judgment_communication', ['experiencer': 'communicator', 'theme': 'evaluee']).
fnpattern(fancy, 31020000, 'experiencer_subj', ['experiencer': 'experiencer', 'theme': 'content']).
fnpattern(fear, 31020000, 'experiencer_subj', ['experiencer': 'experiencer', 'theme': 'content']).
fnpattern(hate, 31020000, 'experiencer_subj', ['experiencer': 'experiencer', 'theme': 'content']).
fnpattern(like, 31020000, 'experiencer_subj', ['experiencer': 'experiencer', 'theme': 'content']).
fnpattern(loathe, 31020000, 'experiencer_subj', ['experiencer': 'experiencer', 'theme': 'content']).
fnpattern(love, 31020000, 'experiencer_subj', ['experiencer': 'experiencer', 'theme': 'content']).
fnpattern(mourn, 31020000, 'experiencer_subj', ['experiencer': 'experiencer', 'theme': 'content']).
fnpattern(pity, 31020000, 'experiencer_subj', ['experiencer': 'experiencer', 'theme': 'content']).
fnpattern(prefer, 31020000, 'partiality', ['experiencer': 'judge', 'theme': 'dispute', 'predicate': 'side_2']).
fnpattern(prefer, 31020000, 'partiality', ['experiencer': 'judge', 'theme': 'dispute', 'predicate': 'manifestation_of_bias']).
fnpattern(prefer, 31020000, 'partiality', ['experiencer': 'judge', 'theme': 'sides', 'predicate': 'side_2']).
fnpattern(prefer, 31020000, 'partiality', ['experiencer': 'judge', 'theme': 'sides', 'predicate': 'manifestation_of_bias']).
fnpattern(prefer, 31020000, 'partiality', ['experiencer': 'judge', 'theme': 'side_1', 'predicate': 'side_2']).
fnpattern(prefer, 31020000, 'partiality', ['experiencer': 'judge', 'theme': 'side_1', 'predicate': 'manifestation_of_bias']).
fnpattern(prefer, 31020000, 'preference', ['experiencer': 'experiencer', 'theme': 'event', 'predicate': 'location_of_event']).
fnpattern(prefer, 31020000, 'preference', ['experiencer': 'experiencer', 'theme': 'event', 'predicate': 'contrast']).
fnpattern(prefer, 31020000, 'preference', ['experiencer': 'experiencer', 'theme': 'focal_participant', 'predicate': 'location_of_event']).
fnpattern(prefer, 31020000, 'preference', ['experiencer': 'experiencer', 'theme': 'focal_participant', 'predicate': 'contrast']).
fnpattern(prize, 31020000, 'judgment', ['experiencer': 'cognizer', 'theme': 'evaluee', 'predicate': 'role']).
fnpattern(reaffirm, 31020000, 'statement', ['experiencer': 'speaker', 'theme': 'message', 'predicate': 'topic']).
fnpattern(reaffirm, 31020000, 'statement', ['experiencer': 'speaker', 'theme': 'message', 'predicate': 'medium']).
fnpattern(regret, 31020000, 'experiencer_subj', ['experiencer': 'experiencer', 'theme': 'content']).
fnpattern(relish, 31020000, 'experiencer_subj', ['experiencer': 'experiencer', 'theme': 'content']).
fnpattern(resent, 31020000, 'experiencer_subj', ['experiencer': 'experiencer', 'theme': 'content']).
fnpattern(respect, 31020000, 'judgment', ['experiencer': 'cognizer', 'theme': 'evaluee', 'predicate': 'role']).
fnpattern(revere, 31020000, 'judgment', ['experiencer': 'cognizer', 'theme': 'evaluee', 'predicate': 'role']).
fnpattern(rue, 31020000, 'experiencer_subj', ['experiencer': 'experiencer', 'theme': 'content']).
fnpattern(value, 31020000, 'judgment', ['experiencer': 'cognizer', 'theme': 'evaluee', 'predicate': 'role']).
fnpattern(cheer, 31030100, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(gladden, 31030100, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(madden, 31030100, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(sicken, 31030100, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(thrill, 31030100, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(fear, 31030300, 'experiencer_subj', ['experiencer': 'experiencer', 'cause': 'content']).
fnpattern(grieve, 31030300, 'experiencer_subj', ['experiencer': 'experiencer', 'cause': 'content']).
fnpattern(mourn, 31030300, 'experiencer_subj', ['experiencer': 'experiencer', 'cause': 'content']).
fnpattern(delight, 31030500, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(delight, 31030500, 'experiencer_subj', ['experiencer': 'experiencer', 'cause': 'content']).
fnpattern(luxuriate, 31030500, 'experiencer_subj', ['experiencer': 'experiencer', 'cause': 'content']).
fnpattern(despair, 31030600, 'experiencer_subj', ['experiencer': 'experiencer', 'cause': 'content']).
fnpattern(disapprove, 31030600, 'judgment', ['experiencer': 'cognizer', 'cause': 'evaluee']).
fnpattern(sicken, 31030600, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(anger, 31030800, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(delight, 31030800, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(delight, 31030800, 'experiencer_subj', ['experiencer': 'experiencer', 'cause': 'content']).
fnpattern(grieve, 31030800, 'experiencer_subj', ['experiencer': 'experiencer', 'cause': 'content']).
fnpattern(meditate, 31030800, 'cogitation', ['experiencer': 'cognizer', 'cause': 'topic']).
fnpattern(mourn, 31030800, 'experiencer_subj', ['experiencer': 'experiencer', 'cause': 'content']).
fnpattern(muse, 31030800, 'cogitation', ['experiencer': 'cognizer', 'cause': 'topic']).
fnpattern(puzzle, 31030800, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(reflect, 31030800, 'cogitation', ['experiencer': 'cognizer', 'cause': 'topic']).
fnpattern(ruminate, 31030800, 'cogitation', ['experiencer': 'cognizer', 'cause': 'topic']).
fnpattern(thrill, 31030900, 'experiencer_obj', ['experiencer': 'experiencer', 'cause': 'stimulus']).
fnpattern(covet, 32010000, 'desiring', ['experiencer': 'experiencer', 'event': 'theme']).
fnpattern(covet, 32010000, 'experiencer_subj', ['experiencer': 'experiencer', 'event': 'content', 'attribute': 'reason']).
fnpattern(crave, 32010000, 'desiring', ['experiencer': 'experiencer', 'event': 'theme']).
fnpattern(crave, 32010000, 'experiencer_subj', ['experiencer': 'experiencer', 'event': 'content', 'attribute': 'reason']).
fnpattern(desire, 32010000, 'desiring', ['experiencer': 'experiencer', 'event': 'theme']).
fnpattern(desire, 32010000, 'experiencer_subj', ['experiencer': 'experiencer', 'event': 'content', 'attribute': 'reason']).
fnpattern(fancy, 32010000, 'experiencer_subj', ['experiencer': 'experiencer', 'event': 'content', 'attribute': 'reason']).
fnpattern(want, 32010000, 'desiring', ['experiencer': 'experiencer', 'event': 'theme']).
fnpattern(want, 32010000, 'experiencer_subj', ['experiencer': 'experiencer', 'event': 'content', 'attribute': 'reason']).
fnpattern(prefer, 32010100, 'partiality', ['experiencer': 'judge', 'theme': 'dispute', 'attribute': 'side_2', 'predicate': 'manifestation_of_bias']).
fnpattern(prefer, 32010100, 'partiality', ['experiencer': 'judge', 'theme': 'sides', 'attribute': 'side_2', 'predicate': 'manifestation_of_bias']).
fnpattern(prefer, 32010100, 'partiality', ['experiencer': 'judge', 'theme': 'side_1', 'attribute': 'side_2', 'predicate': 'manifestation_of_bias']).
fnpattern(prefer, 32010100, 'preference', ['experiencer': 'experiencer', 'theme': 'event', 'predicate': 'location_of_event']).
fnpattern(prefer, 32010100, 'preference', ['experiencer': 'experiencer', 'theme': 'event', 'predicate': 'contrast']).
fnpattern(prefer, 32010100, 'preference', ['experiencer': 'experiencer', 'theme': 'focal_participant', 'predicate': 'location_of_event']).
fnpattern(prefer, 32010100, 'preference', ['experiencer': 'experiencer', 'theme': 'focal_participant', 'predicate': 'contrast']).
fnpattern(crave, 32020100, 'desiring', ['experiencer': 'experiencer', 'event': 'theme']).
fnpattern(crave, 32020100, 'experiencer_subj', ['experiencer': 'experiencer', 'event': 'content']).
fnpattern(hanker, 32020100, 'desiring', ['experiencer': 'experiencer', 'event': 'theme']).
fnpattern(hanker, 32020100, 'experiencer_subj', ['experiencer': 'experiencer', 'event': 'content']).
fnpattern(hope, 32020100, 'desiring', ['experiencer': 'experiencer', 'event': 'theme']).
fnpattern(hunger, 32020100, 'desiring', ['experiencer': 'experiencer', 'event': 'theme']).
fnpattern(long, 32020100, 'desiring', ['experiencer': 'experiencer', 'event': 'theme']).
fnpattern(long, 32020100, 'experiencer_subj', ['experiencer': 'experiencer', 'event': 'content']).
fnpattern(lust, 32020100, 'desiring', ['experiencer': 'experiencer', 'event': 'theme']).
fnpattern(pine, 32020100, 'experiencer_subj', ['experiencer': 'experiencer', 'event': 'content']).
fnpattern(thirst, 32020100, 'desiring', ['experiencer': 'experiencer', 'event': 'theme']).
fnpattern(wish, 32020100, 'desiring', ['experiencer': 'experiencer', 'event': 'theme']).
fnpattern(yearn, 32020100, 'desiring', ['experiencer': 'experiencer', 'event': 'theme']).
fnpattern(yearn, 32020100, 'experiencer_subj', ['experiencer': 'experiencer', 'event': 'content']).
fnpattern(hanker, 32020200, 'desiring', ['experiencer': 'experiencer', 'event': 'theme']).
fnpattern(lust, 32020200, 'desiring', ['experiencer': 'experiencer', 'event': 'theme']).
fnpattern(thirst, 32020200, 'desiring', ['experiencer': 'experiencer', 'event': 'theme']).
fnpattern(yearn, 32020200, 'experiencer_subj', ['experiencer': 'experiencer', 'event': 'content']).
fnpattern(acclaim, 33000000, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(applaud, 33000000, 'judgment', ['agent': 'cognizer', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(assault, 33000000, 'attack', ['agent': 'assailant', 'theme': 'victim', 'predicate': 'reason']).
fnpattern(assault, 33000000, 'attack', ['agent': 'assailant', 'theme': 'victim', 'predicate': 'purpose']).
fnpattern(attack, 33000000, 'attack', ['agent': 'assailant', 'theme': 'victim', 'predicate': 'reason']).
fnpattern(attack, 33000000, 'attack', ['agent': 'assailant', 'theme': 'victim', 'predicate': 'purpose']).
fnpattern(belittle, 33000000, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(blame, 33000000, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(castigate, 33000000, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(censure, 33000000, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(chastise, 33000000, 'judgment_direct_address', ['agent': 'communicator', 'theme': 'addressee', 'predicate': 'reason']).
fnpattern(chide, 33000000, 'judgment_direct_address', ['agent': 'communicator', 'theme': 'addressee', 'predicate': 'reason']).
fnpattern(commend, 33000000, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(compliment, 33000000, 'judgment_direct_address', ['agent': 'communicator', 'theme': 'addressee', 'predicate': 'reason']).
fnpattern(condemn, 33000000, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(condone, 33000000, 'forgiveness', ['agent': 'judge', 'theme': 'evaluee', 'predicate': 'offense']).
fnpattern(criticize, 33000000, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(decry, 33000000, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(denigrate, 33000000, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(denounce, 33000000, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(deprecate, 33000000, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(deride, 33000000, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(disparage, 33000000, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(doubt, 33000000, 'certainty', ['agent': 'cognizer', 'theme': 'content']).
fnpattern(doubt, 33000000, 'certainty', ['agent': 'cognizer', 'theme': 'topic']).
fnpattern(excoriate, 33000000, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(excuse, 33000000, 'forgiveness', ['agent': 'judge', 'theme': 'evaluee', 'predicate': 'offense']).
fnpattern(extol, 33000000, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(fault, 33000000, 'judgment', ['agent': 'cognizer', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(forgive, 33000000, 'forgiveness', ['agent': 'judge', 'theme': 'evaluee', 'predicate': 'offense']).
fnpattern(gibe, 33000000, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(herald, 33000000, 'heralding', ['agent': 'communicator', 'theme': 'event']).
fnpattern(herald, 33000000, 'heralding', ['agent': 'communicator', 'theme': 'individual']).
fnpattern(indict, 33000000, 'notification_of_charges', ['agent': 'arraign_authority', 'theme': 'accused', 'predicate': 'charges']).
fnpattern(indict, 33000000, 'notification_of_charges', ['agent': 'arraign_authority', 'theme': 'accused', 'predicate': 'containing_event']).
fnpattern(laud, 33000000, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(mock, 33000000, 'judgment', ['agent': 'cognizer', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(mock, 33000000, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(pardon, 33000000, 'forgiveness', ['agent': 'judge', 'theme': 'evaluee', 'predicate': 'offense']).
fnpattern(praise, 33000000, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(punish, 33000000, 'rewards_and_punishments', ['agent': 'agent', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(rebuke, 33000000, 'judgment_direct_address', ['agent': 'communicator', 'theme': 'addressee', 'predicate': 'reason']).
fnpattern(recompense, 33000000, 'rewards_and_punishments', ['agent': 'agent', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(reprimand, 33000000, 'judgment_direct_address', ['agent': 'communicator', 'theme': 'addressee', 'predicate': 'reason']).
fnpattern(reproach, 33000000, 'judgment_direct_address', ['agent': 'communicator', 'theme': 'addressee', 'predicate': 'reason']).
fnpattern(reprove, 33000000, 'judgment_direct_address', ['agent': 'communicator', 'theme': 'addressee', 'predicate': 'reason']).
fnpattern(reward, 33000000, 'rewards_and_punishments', ['agent': 'agent', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(ridicule, 33000000, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(sanction, 33000000, 'grant_permission', ['agent': 'grantor', 'theme': 'grantee', 'predicate': 'action']).
fnpattern(scold, 33000000, 'judgment_direct_address', ['agent': 'communicator', 'theme': 'addressee', 'predicate': 'reason']).
fnpattern(scorn, 33000000, 'judgment', ['agent': 'cognizer', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(thank, 33000000, 'judgment_direct_address', ['agent': 'communicator', 'theme': 'addressee', 'predicate': 'reason']).
fnpattern(upbraid, 33000000, 'judgment_direct_address', ['agent': 'communicator', 'theme': 'addressee', 'predicate': 'reason']).
fnpattern(analyse, 34000000, 'scrutiny', ['agent': 'cognizer', 'theme': 'ground', 'attribute': 'phenomenon']).
fnpattern(assess, 34000000, 'assessing', ['agent': 'assessor', 'theme': 'phenomenon', 'attribute': 'feature']).
fnpattern(evaluate, 34000000, 'assessing', ['agent': 'assessor', 'theme': 'phenomenon', 'attribute': 'feature']).
fnpattern(scrutinize, 34000000, 'scrutiny', ['agent': 'cognizer', 'theme': 'ground', 'attribute': 'phenomenon']).
fnpattern(study, 34000000, 'scrutiny', ['agent': 'cognizer', 'theme': 'ground', 'attribute': 'phenomenon']).
fnpattern(feel, 35010000, 'seeking', ['agent': 'cognizer', 'location': 'ground', 'theme': 'sought_entity']).
fnpattern(hunt, 35010000, 'seeking', ['agent': 'cognizer', 'location': 'ground', 'theme': 'sought_entity']).
fnpattern(check, 35020000, 'scrutiny', ['agent': 'cognizer', 'location': 'ground', 'theme': 'phenomenon']).
fnpattern(comb, 35020000, 'seeking', ['agent': 'cognizer', 'location': 'ground', 'theme': 'sought_entity']).
fnpattern(probe, 35020000, 'scrutiny', ['agent': 'cognizer', 'location': 'ground', 'theme': 'phenomenon']).
fnpattern(scour, 35020000, 'seeking', ['agent': 'cognizer', 'location': 'ground', 'theme': 'sought_entity']).
fnpattern(scout, 35020000, 'scrutiny', ['agent': 'cognizer', 'location': 'ground', 'theme': 'phenomenon']).
fnpattern(search, 35020000, 'scrutiny', ['agent': 'cognizer', 'location': 'ground', 'theme': 'phenomenon']).
fnpattern(sift, 35020000, 'scrutiny', ['agent': 'cognizer', 'location': 'ground', 'theme': 'phenomenon']).
fnpattern(watch, 35020000, 'seeking', ['agent': 'cognizer', 'location': 'ground', 'theme': 'sought_entity']).
fnpattern(examine, 35040000, 'scrutiny', ['agent': 'cognizer', 'location': 'ground', 'theme': 'phenomenon']).
fnpattern(frisk, 35040000, 'seeking', ['agent': 'cognizer', 'location': 'ground', 'theme': 'sought_entity']).
fnpattern(inspect, 35040000, 'scrutiny', ['agent': 'cognizer', 'location': 'ground', 'theme': 'phenomenon']).
fnpattern(investigate, 35040000, 'scrutiny', ['agent': 'cognizer', 'location': 'ground', 'theme': 'phenomenon']).
fnpattern(ransack, 35040000, 'seeking', ['agent': 'cognizer', 'location': 'ground', 'theme': 'sought_entity']).
fnpattern(scan, 35040000, 'scrutiny', ['agent': 'cognizer', 'location': 'ground', 'theme': 'phenomenon']).
fnpattern(scrutinize, 35040000, 'scrutiny', ['agent': 'cognizer', 'location': 'ground', 'theme': 'phenomenon']).
fnpattern(survey, 35040000, 'scrutiny', ['agent': 'cognizer', 'location': 'ground', 'theme': 'phenomenon']).
fnpattern(test, 35040000, 'operational_testing', ['agent': 'tester', 'location': 'product', 'theme': 'unwanted_characteristics']).
fnpattern(test, 35040000, 'operational_testing', ['agent': 'tester', 'location': 'tested_property', 'theme': 'unwanted_characteristics']).
fnpattern(forage, 35050000, 'seeking', ['agent': 'cognizer', 'location': 'ground', 'theme': 'sought_entity']).
fnpattern(fumble, 35050000, 'seeking', ['agent': 'cognizer', 'location': 'ground', 'theme': 'sought_entity']).
fnpattern(grope, 35050000, 'seeking', ['agent': 'cognizer', 'location': 'ground', 'theme': 'sought_entity']).
fnpattern(listen, 35050000, 'seeking', ['agent': 'cognizer', 'location': 'ground', 'theme': 'sought_entity']).
fnpattern(look, 35050000, 'scrutiny', ['agent': 'cognizer', 'location': 'ground', 'theme': 'phenomenon']).
fnpattern(rummage, 35050000, 'scrutiny', ['agent': 'cognizer', 'location': 'ground', 'theme': 'phenomenon']).
fnpattern(nose, 35060000, 'seeking', ['agent': 'cognizer', 'location': 'ground', 'theme': 'sought_entity']).
fnpattern(seek, 35060000, 'seeking', ['agent': 'cognizer', 'location': 'ground', 'theme': 'sought_entity']).
fnpattern(intermix, 36010000, 'amalgamation', ['actor1': 'parts', 'actor2': 'part_2', 'theme': 'whole']).
fnpattern(intermix, 36010000, 'amalgamation', ['actor1': 'part_1', 'actor2': 'part_2', 'theme': 'whole']).
fnpattern(agree, 36010100, 'compatibility', ['actor1': 'item1', 'actor2': 'item2', 'theme': 'parameter']).
fnpattern(collaborate, 36010100, 'collaboration', ['actor1': 'partner1', 'actor2': 'partner2', 'theme': 'undertaking']).
fnpattern(cooperate, 36010100, 'collaboration', ['actor1': 'partner1', 'actor2': 'partner2', 'theme': 'undertaking']).
fnpattern(argue, 36010200, 'quarreling', ['actor1': 'arguer1', 'actor2': 'arguer2', 'theme': 'issue']).
fnpattern(bicker, 36010200, 'quarreling', ['actor1': 'arguer1', 'actor2': 'arguer2', 'theme': 'issue']).
fnpattern(brawl, 36010200, 'hostile_encounter', ['actor1': 'side1', 'actor2': 'side2', 'theme': 'issue']).
fnpattern(clash, 36010200, 'hostile_encounter', ['actor1': 'side1', 'actor2': 'side2', 'theme': 'issue']).
fnpattern(compete, 36010200, 'competition', ['actor1': 'participant1', 'actor2': 'participant2', 'theme': 'competition']).
fnpattern(duel, 36010200, 'hostile_encounter', ['actor1': 'side1', 'actor2': 'side2', 'theme': 'issue']).
fnpattern(quarrel, 36010200, 'quarreling', ['actor1': 'arguer1', 'actor2': 'arguer2', 'theme': 'issue']).
fnpattern(quibble, 36010200, 'quarreling', ['actor1': 'arguer1', 'actor2': 'arguer2', 'theme': 'issue']).
fnpattern(scuffle, 36010200, 'hostile_encounter', ['actor1': 'side1', 'actor2': 'side2', 'theme': 'issue']).
fnpattern(skirmish, 36010200, 'hostile_encounter', ['actor1': 'side1', 'actor2': 'side2', 'theme': 'issue']).
fnpattern(squabble, 36010200, 'quarreling', ['actor1': 'arguer1', 'actor2': 'arguer2', 'theme': 'issue']).
fnpattern(war, 36010200, 'hostile_encounter', ['actor1': 'side1', 'actor2': 'side2', 'theme': 'issue']).
fnpattern(wrangle, 36010200, 'quarreling', ['actor1': 'arguer1', 'actor2': 'arguer2', 'theme': 'issue']).
fnpattern(court, 36020000, 'personal_relationship', ['actor1': 'partner1', 'actor2': 'partner2']).
fnpattern(date, 36020000, 'personal_relationship', ['actor1': 'partner1', 'actor2': 'partner2']).
fnpattern(divorce, 36020000, 'forming_relationships', ['actor1': 'partner1', 'actor2': 'partner2']).
fnpattern(marry, 36020000, 'forming_relationships', ['actor1': 'partner1', 'actor2': 'partner2']).
fnpattern(play, 36030100, 'competition', ['actor1': 'participant1', 'actor2': 'participant2']).
fnpattern(battle, 36030200, 'hostile_encounter', ['actor1': 'side1', 'actor2': 'side2']).
fnpattern(fight, 36030200, 'hostile_encounter', ['actor1': 'side1', 'actor2': 'side2']).
fnpattern(fight, 36030200, 'quarreling', ['actor1': 'arguer1', 'actor2': 'arguer2']).
fnpattern(communicate, 36040100, 'communication', ['actor': 'communicator', 'actor2': 'addressee', 'topic': 'message']).
fnpattern(communicate, 36040100, 'communication', ['actor': 'communicator', 'actor2': 'addressee', 'topic': 'topic']).
fnpattern(communicate, 36040100, 'communication', ['actor1': 'communicator', 'actor2': 'addressee', 'topic': 'message']).
fnpattern(communicate, 36040100, 'communication', ['actor1': 'communicator', 'actor2': 'addressee', 'topic': 'topic']).
fnpattern(demonstrate, 37010000, 'reasoning', ['agent': 'arguer', 'topic': 'content', 'recipient': 'addressee']).
fnpattern(explain, 37010000, 'justifying', ['agent': 'agent', 'topic': 'state_of_affairs']).
fnpattern(explain, 37010000, 'justifying', ['agent': 'agent', 'topic': 'act']).
fnpattern(explain, 37010000, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(explain, 37010000, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(preach, 37010000, 'speak_on_topic', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'audience']).
fnpattern(preach, 37010000, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(preach, 37010000, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(read, 37010100, 'hear', ['agent': 'speaker', 'topic': 'message', 'recipient': 'hearer']).
fnpattern(read, 37010100, 'hear', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'hearer']).
fnpattern(show, 37010110, 'reasoning', ['agent': 'arguer', 'topic': 'content', 'recipient': 'addressee']).
fnpattern(tell, 37010111, 'reporting', ['agent': 'informer', 'topic': 'behavior', 'recipient': 'authorities']).
fnpattern(tell, 37010111, 'request', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(tell, 37010111, 'request', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(tell, 37010111, 'telling', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(tell, 37010111, 'telling', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(write, 37010111, 'contacting', ['agent': 'communicator', 'topic': 'communication', 'recipient': 'addressee']).
fnpattern(write, 37010111, 'contacting', ['agent': 'communicator', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(write, 37010111, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(write, 37010111, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(ask, 37010111, 'questioning', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(ask, 37010111, 'questioning', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(ask, 37010111, 'request', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(ask, 37010111, 'request', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(inquire, 37012000, 'questioning', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(inquire, 37012000, 'questioning', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(tell, 37020000, 'reporting', ['agent': 'informer', 'topic': 'behavior', 'recipient': 'authorities']).
fnpattern(tell, 37020000, 'request', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(tell, 37020000, 'request', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(tell, 37020000, 'telling', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(tell, 37020000, 'telling', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(babble, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(babble, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(bark, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(bark, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(bawl, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(bawl, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(bellow, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(bellow, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(bleat, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(bleat, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(bray, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(bray, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(burble, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(burble, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(cackle, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(cackle, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(chant, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(chant, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(chatter, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(chatter, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(chirp, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(chirp, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(cluck, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(cluck, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(coo, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(coo, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(croak, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(croak, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(croon, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(croon, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(crow, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(crow, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(cry, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(cry, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(drawl, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(drawl, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(drone, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(drone, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(gabble, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(gabble, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(gibber, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(gibber, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(groan, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(groan, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(growl, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(growl, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(grumble, 37030000, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(grumble, 37030000, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(grunt, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(grunt, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(hiss, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(hiss, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(hoot, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(hoot, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(howl, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(howl, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(jabber, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(jabber, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(lisp, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(lisp, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(moan, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(moan, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(mumble, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(mumble, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(murmur, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(murmur, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(mutter, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(mutter, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(prattle, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(prattle, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(purr, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(purr, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(rasp, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(rasp, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(roar, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(roar, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(rumble, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(rumble, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(scream, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(scream, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(screech, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(screech, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(shout, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(shout, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(shriek, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(shriek, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(sing, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(sing, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(snarl, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(snarl, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(splutter, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(splutter, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(squawk, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(squawk, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(squeak, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(squeak, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(squeal, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(squeal, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(stammer, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(stammer, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(stutter, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(stutter, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(thunder, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(thunder, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(trill, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(trill, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(trumpet, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(trumpet, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(twitter, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(twitter, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(wail, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(wail, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(warble, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(warble, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(wheeze, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(wheeze, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(whimper, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(whimper, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(whine, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(whine, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(whisper, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(whisper, 37030000, 'communication_manner', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(whoop, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(whoop, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(yell, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(yell, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(yelp, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(yelp, 37030000, 'communication_noise', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(cable, 37040000, 'communication_means', ['agent': 'communicator', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(cable, 37040000, 'communication_means', ['agent': 'communicator', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(cable, 37040000, 'contacting', ['agent': 'communicator', 'topic': 'communication', 'recipient': 'addressee']).
fnpattern(cable, 37040000, 'contacting', ['agent': 'communicator', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(e-mail, 37040000, 'contacting', ['agent': 'communicator', 'topic': 'communication', 'recipient': 'addressee']).
fnpattern(e-mail, 37040000, 'contacting', ['agent': 'communicator', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(fax, 37040000, 'contacting', ['agent': 'communicator', 'topic': 'communication', 'recipient': 'addressee']).
fnpattern(fax, 37040000, 'contacting', ['agent': 'communicator', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(phone, 37040000, 'communication_means', ['agent': 'communicator', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(phone, 37040000, 'communication_means', ['agent': 'communicator', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(phone, 37040000, 'contacting', ['agent': 'communicator', 'topic': 'communication', 'recipient': 'addressee']).
fnpattern(phone, 37040000, 'contacting', ['agent': 'communicator', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(radio, 37040000, 'communication_means', ['agent': 'communicator', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(radio, 37040000, 'communication_means', ['agent': 'communicator', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(radio, 37040000, 'contacting', ['agent': 'communicator', 'topic': 'communication', 'recipient': 'addressee']).
fnpattern(radio, 37040000, 'contacting', ['agent': 'communicator', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(semaphore, 37040000, 'communication_means', ['agent': 'communicator', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(semaphore, 37040000, 'communication_means', ['agent': 'communicator', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(telegraph, 37040000, 'communication_means', ['agent': 'communicator', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(telegraph, 37040000, 'communication_means', ['agent': 'communicator', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(telegraph, 37040000, 'contacting', ['agent': 'communicator', 'topic': 'communication', 'recipient': 'addressee']).
fnpattern(telegraph, 37040000, 'contacting', ['agent': 'communicator', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(telephone, 37040000, 'communication_means', ['agent': 'communicator', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(telephone, 37040000, 'communication_means', ['agent': 'communicator', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(telephone, 37040000, 'contacting', ['agent': 'communicator', 'topic': 'communication', 'recipient': 'addressee']).
fnpattern(telephone, 37040000, 'contacting', ['agent': 'communicator', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(telex, 37040000, 'communication_means', ['agent': 'communicator', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(telex, 37040000, 'communication_means', ['agent': 'communicator', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(telex, 37040000, 'contacting', ['agent': 'communicator', 'topic': 'communication', 'recipient': 'addressee']).
fnpattern(telex, 37040000, 'contacting', ['agent': 'communicator', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(wire, 37040000, 'communication_means', ['agent': 'communicator', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(wire, 37040000, 'communication_means', ['agent': 'communicator', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(speak, 37050000, 'statement', ['actor1': 'speaker', 'topic': 'message', 'actor2': 'addressee']).
fnpattern(speak, 37050000, 'statement', ['actor1': 'speaker', 'topic': 'topic', 'actor2': 'addressee']).
fnpattern(talk, 37050000, 'statement', ['actor1': 'speaker', 'topic': 'message', 'actor2': 'addressee']).
fnpattern(talk, 37050000, 'statement', ['actor1': 'speaker', 'topic': 'topic', 'actor2': 'addressee']).
fnpattern(chat, 37060000, 'chatting', ['actor1': 'interlocutor1', 'actor2': 'interlocutor2', 'topic': 'topic']).
fnpattern(converse, 37060000, 'chatting', ['actor1': 'interlocutor1', 'actor2': 'interlocutor2', 'topic': 'topic']).
fnpattern(gab, 37060000, 'chatting', ['actor1': 'interlocutor1', 'actor2': 'interlocutor2', 'topic': 'topic']).
fnpattern(gossip, 37060000, 'chatting', ['actor1': 'interlocutor1', 'actor2': 'interlocutor2', 'topic': 'topic']).
fnpattern(announce, 37070000, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(announce, 37070000, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(claim, 37070000, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(claim, 37070000, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(confess, 37070000, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(confess, 37070000, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(confide, 37070000, 'telling', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(confide, 37070000, 'telling', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(convey, 37070000, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(convey, 37070000, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(declare, 37070000, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(declare, 37070000, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(disclose, 37070000, 'reveal_secret', ['agent': 'speaker', 'topic': 'information', 'recipient': 'addressee']).
fnpattern(exclaim, 37070000, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(exclaim, 37070000, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(insist, 37070000, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(insist, 37070000, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(mention, 37070000, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(mention, 37070000, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(proclaim, 37070000, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(proclaim, 37070000, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(propose, 37070000, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(propose, 37070000, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(recount, 37070000, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(recount, 37070000, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(reiterate, 37070000, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(reiterate, 37070000, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(relate, 37070000, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(relate, 37070000, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(reply, 37070000, 'communication_response', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(reply, 37070000, 'communication_response', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(reply, 37070000, 'communication_response', ['agent': 'speaker', 'topic': 'trigger', 'recipient': 'addressee']).
fnpattern(respond, 37070000, 'communication_response', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(respond, 37070000, 'communication_response', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(respond, 37070000, 'communication_response', ['agent': 'speaker', 'topic': 'trigger', 'recipient': 'addressee']).
fnpattern(retort, 37070000, 'communication_response', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(retort, 37070000, 'communication_response', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(retort, 37070000, 'communication_response', ['agent': 'speaker', 'topic': 'trigger', 'recipient': 'addressee']).
fnpattern(reveal, 37070000, 'reveal_secret', ['agent': 'speaker', 'topic': 'information', 'recipient': 'addressee']).
fnpattern(say, 37070000, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(say, 37070000, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(state, 37070000, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(state, 37070000, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(suggest, 37070000, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(suggest, 37070000, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(utter, 37070000, 'text_creation', ['agent': 'author', 'topic': 'text', 'recipient': 'addressee']).
fnpattern(voice, 37070000, 'expressing_publicly', ['agent': 'communucator', 'topic': 'content', 'recipient': 'addressee']).
fnpattern(remark, 37070100, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(remark, 37070100, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(report, 37070100, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(report, 37070100, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(boast, 37080000, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(boast, 37080000, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(brag, 37080000, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(brag, 37080000, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(complain, 37080000, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(complain, 37080000, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(gripe, 37080000, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(gripe, 37080000, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(grumble, 37080000, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(grumble, 37080000, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(assure, 37090000, 'telling', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(assure, 37090000, 'telling', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(inform, 37090000, 'telling', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(inform, 37090000, 'telling', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(notify, 37090000, 'telling', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(notify, 37090000, 'telling', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(admonish, 37090100, 'attempt_suasion', ['agent': 'speaker', 'topic': 'content', 'recipient': 'addressee']).
fnpattern(admonish, 37090100, 'attempt_suasion', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(advise, 37090100, 'telling', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(advise, 37090100, 'telling', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(caution, 37090100, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(caution, 37090100, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(lecture, 37110100, 'speak_on_topic', ['agent': 'speaker', 'topic': 'content', 'recipient': 'audience']).
fnpattern(comment, 37110110, 'statement', ['agent': 'speaker', 'topic': 'message', 'recipient': 'addressee']).
fnpattern(comment, 37110110, 'statement', ['agent': 'speaker', 'topic': 'topic', 'recipient': 'addressee']).
fnpattern(bark, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(bark, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(bark, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(bark, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(bark, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(bark, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(bellow, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(bellow, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(bellow, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(bellow, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(bellow, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(bellow, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(bleat, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(bleat, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(bleat, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(bleat, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(bleat, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(bleat, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(bray, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(bray, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(bray, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(bray, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(bray, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(bray, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(cackle, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(cackle, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(cackle, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(cackle, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(cackle, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(cackle, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(caw, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(caw, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(chatter, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(chatter, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(cheep, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(cheep, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(chirp, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(chirp, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(chirp, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(chirp, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(chirp, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(chirp, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(chirrup, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(chirrup, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(chirrup, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(chirrup, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(cluck, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(cluck, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(cluck, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(cluck, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(coo, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(coo, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(coo, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(coo, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(coo, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(coo, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(croak, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(croak, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(croak, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(croak, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(croak, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(croak, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(crow, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(crow, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(crow, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(crow, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(drone, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(drone, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(drone, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(drone, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(drone, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(drone, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(gobble, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(gobble, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(groan, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(groan, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(groan, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(groan, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(growl, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(growl, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(growl, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(growl, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(growl, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(growl, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(grunt, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(grunt, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(grunt, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(grunt, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(grunt, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(grunt, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(hiss, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(hiss, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(hiss, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(hiss, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(hiss, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(hiss, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(hoot, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(hoot, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(hoot, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(hoot, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(hoot, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(hoot, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(howl, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(howl, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(howl, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(howl, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(howl, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(howl, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(mew, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(mew, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(moan, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(moan, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(moan, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(moan, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(moan, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(moan, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(moo, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(moo, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(neigh, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(neigh, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(peep, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(peep, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(purr, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(purr, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(purr, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(purr, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(purr, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(purr, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(quack, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(quack, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(rattle, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(rattle, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(rattle, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(rattle, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(rattle, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(rattle, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(roar, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(roar, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(roar, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(roar, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(roar, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(roar, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(rumble, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(rumble, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(rumble, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(rumble, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(squawk, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(squawk, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(squawk, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(squawk, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(squawk, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(squawk, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(squeak, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(squeak, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(squeak, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(squeak, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(squeak, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(squeak, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(squeal, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(squeal, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(squeal, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(squeal, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(squeal, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(squeal, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(thunder, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(thunder, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(thunder, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(thunder, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(thunder, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(thunder, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(trill, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(trill, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(trill, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(trill, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(tweet, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(tweet, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(twitter, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(twitter, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(twitter, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(twitter, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(twitter, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(twitter, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(ululate, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(ululate, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(wail, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(wail, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(wail, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(wail, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(wail, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(wail, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(warble, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(warble, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(warble, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(warble, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(wheeze, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(wheeze, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(wheeze, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(wheeze, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(whimper, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(whimper, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(whimper, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(whimper, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(whimper, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(whimper, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(whine, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(whine, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(whine, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(whine, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(whine, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(whine, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(whinny, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(whinny, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(whistle, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(whistle, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(yap, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(yap, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(yell, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(yell, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(yell, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(yell, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(yelp, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(yelp, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(yelp, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(yelp, 38000000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(yelp, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(yelp, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(yowl, 38000000, 'make_noise', ['agent': 'noisy_event', 'theme': 'sound']).
fnpattern(yowl, 38000000, 'make_noise', ['agent': 'sound_source', 'theme': 'sound']).
fnpattern(eat, 39010100, 'ingestion', ['agent': 'ingestor', 'instrument': 'instrument', 'source': 'ingestibles']).
fnpattern(drink, 39010200, 'ingestion', ['agent': 'ingestor', 'instrument': 'instrument', 'source': 'ingestibles']).
fnpattern(masticate, 39020100, 'grinding', ['agent': 'grinder', 'patient': 'undergoer']).
fnpattern(munch, 39020100, 'ingestion', ['agent': 'ingestor', 'patient': 'ingestibles']).
fnpattern(nibble, 39020100, 'ingestion', ['agent': 'ingestor', 'patient': 'ingestibles']).
fnpattern(sip, 39020200, 'ingestion', ['agent': 'ingestor', 'patient': 'ingestibles']).
fnpattern(slurp, 39020200, 'ingestion', ['agent': 'ingestor', 'patient': 'ingestibles']).
fnpattern(gobble, 39030100, 'ingestion', ['agent': 'ingestor', 'patient': 'ingestibles']).
fnpattern(gulp, 39030200, 'ingestion', ['agent': 'ingestor', 'patient': 'ingestibles']).
fnpattern(guzzle, 39030200, 'ingestion', ['agent': 'ingestor', 'patient': 'ingestibles']).
fnpattern(quaff, 39030200, 'ingestion', ['agent': 'ingestor', 'patient': 'ingestibles']).
fnpattern(swig, 39030200, 'ingestion', ['agent': 'ingestor', 'patient': 'ingestibles']).
fnpattern(consume, 39040100, 'ingestion', ['agent': 'ingestor', 'patient': 'ingestibles']).
fnpattern(devour, 39040100, 'ingestion', ['agent': 'ingestor', 'patient': 'ingestibles']).
fnpattern(ingest, 39040100, 'ingestion', ['agent': 'ingestor', 'patient': 'ingestibles']).
fnpattern(imbibe, 39040200, 'ingestion', ['agent': 'ingestor', 'patient': 'ingestibles']).
fnpattern(swill, 39040200, 'ingestion', ['agent': 'ingestor', 'patient': 'ingestibles']).
fnpattern(breakfast, 39050000, 'ingestion', ['agent': 'ingestor', 'patient': 'ingestibles']).
fnpattern(dine, 39050000, 'ingestion', ['agent': 'ingestor', 'patient': 'ingestibles']).
fnpattern(feast, 39050000, 'ingestion', ['agent': 'ingestor', 'patient': 'ingestibles']).
fnpattern(lunch, 39050000, 'ingestion', ['agent': 'ingestor', 'patient': 'ingestibles']).
fnpattern(nosh, 39050000, 'ingestion', ['agent': 'ingestor', 'patient': 'ingestibles']).
fnpattern(snack, 39050000, 'ingestion', ['agent': 'ingestor', 'patient': 'ingestibles']).
fnpattern(sup, 39050000, 'ingestion', ['agent': 'ingestor', 'patient': 'ingestibles']).
fnpattern(feed, 39060000, 'ingestion', ['agent': 'ingestor', 'patient': 'ingestibles']).
fnpattern(feed, 39070000, 'ingestion', ['agent': 'ingestor', 'patient': 'ingestibles']).
fnpattern(belch, 40011000, 'excreting', ['agent': 'excreter']).
fnpattern(burp, 40011000, 'excreting', ['agent': 'excreter']).
fnpattern(fart, 40011000, 'excreting', ['agent': 'excreter']).
fnpattern(dribble, 40012000, 'fluidic_motion', ['theme': 'fluid']).
fnpattern(puke, 40012000, 'excreting', ['agent': 'excreter', 'theme': 'excreta']).
fnpattern(sweat, 40012000, 'excreting', ['agent': 'excreter', 'theme': 'excreta']).
fnpattern(vomit, 40012000, 'excreting', ['agent': 'excreter', 'theme': 'excreta']).
fnpattern(breathe, 40012100, 'breathing', ['agent': 'agent', 'theme': 'air']).
fnpattern(defecate, 40012100, 'excreting', ['agent': 'excreter', 'theme': 'excreta']).
fnpattern(retch, 40012100, 'excreting', ['agent': 'excreter', 'theme': 'excreta']).
fnpattern(exhale, 40013100, 'breathing', ['agent': 'agent', 'theme': 'air']).
fnpattern(exhale, 40013100, 'emitting', ['agent': 'source_emitter', 'theme': 'emission']).
fnpattern(inhale, 40013200, 'breathing', ['agent': 'agent', 'theme': 'air']).
fnpattern(cackle, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(cackle, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(cackle, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(cackle, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(chuckle, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(chuckle, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(chuckle, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(chuckle, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(cry, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(cry, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(cry, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(cry, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(frown, 40020000, 'making_faces', ['agent': 'agent', 'cause': 'internal_cause']).
fnpattern(gasp, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(gasp, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(gasp, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(gasp, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(grimace, 40020000, 'making_faces', ['agent': 'agent', 'cause': 'internal_cause']).
fnpattern(grin, 40020000, 'making_faces', ['agent': 'agent', 'cause': 'internal_cause']).
fnpattern(groan, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(groan, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(groan, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(groan, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(growl, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(growl, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(growl, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(growl, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(howl, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(howl, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(howl, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(howl, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(moan, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(moan, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(moan, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(moan, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(pout, 40020000, 'making_faces', ['agent': 'agent', 'cause': 'internal_cause']).
fnpattern(scoff, 40020000, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'cause': 'topic', 'recipient': 'addressee']).
fnpattern(scoff, 40020000, 'judgment_communication', ['agent': 'communicator', 'theme': 'evaluee', 'cause': 'reason', 'recipient': 'addressee']).
fnpattern(scowl, 40020000, 'making_faces', ['agent': 'agent', 'cause': 'internal_cause']).
fnpattern(simper, 40020000, 'communication_manner', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(simper, 40020000, 'communication_manner', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(simper, 40020000, 'communication_manner', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(simper, 40020000, 'communication_manner', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(smile, 40020000, 'making_faces', ['agent': 'agent', 'cause': 'internal_cause']).
fnpattern(smirk, 40020000, 'making_faces', ['agent': 'agent', 'cause': 'internal_cause']).
fnpattern(snort, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(snort, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(snort, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(snort, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(titter, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'cause': 'addressee']).
fnpattern(titter, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(titter, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'cause': 'addressee']).
fnpattern(titter, 40020000, 'communication_noise', ['agent': 'speaker', 'theme': 'topic', 'recipient': 'addressee']).
fnpattern(wag, 40031000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(beckon, 40031100, 'gesture', ['agent': 'communicator', 'patient': 'body_part', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(beckon, 40031100, 'gesture', ['agent': 'communicator', 'patient': 'body_part', 'theme': 'indicated_entity', 'recipient': 'addressee']).
fnpattern(blink, 40031100, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(clap, 40031100, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(gesture, 40031100, 'gesture', ['agent': 'communicator', 'patient': 'body_part', 'theme': 'message', 'recipient': 'addressee']).
fnpattern(gesture, 40031100, 'gesture', ['agent': 'communicator', 'patient': 'body_part', 'theme': 'indicated_entity', 'recipient': 'addressee']).
fnpattern(nod, 40031100, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(shrug, 40031100, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(wave, 40031100, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(wink, 40031100, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(arch, 40032000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(bat, 40032000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(cock, 40032000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(crane, 40032000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(cross, 40032000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(flap, 40032000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(flex, 40032000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(flutter, 40032000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(gnash, 40032000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(grind, 40032000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(hang, 40032000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(hunch, 40032000, 'posture', ['agent': 'agent']).
fnpattern(pucker, 40032000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(purse, 40032000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(roll, 40032000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(shake, 40032000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(shuffle, 40032000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(smack, 40032000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(stamp, 40032000, 'self_motion', ['agent': 'self_mover']).
fnpattern(stretch, 40032000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(toss, 40032000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(twitch, 40032000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(wag, 40032000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(waggle, 40032000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(wiggle, 40032000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(wrinkle, 40032000, 'body_movement', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(bob, 40033000, 'body_movement', ['agent': 'agent']).
fnpattern(kneel, 40033000, 'body_movement', ['agent': 'agent']).
fnpattern(catnap, 40040000, 'sleep', ['agent': 'sleeper']).
fnpattern(doze, 40040000, 'sleep', ['agent': 'sleeper']).
fnpattern(drowse, 40040000, 'sleep', ['agent': 'sleeper']).
fnpattern(nap, 40040000, 'sleep', ['agent': 'sleeper']).
fnpattern(sleep, 40040000, 'sleep', ['agent': 'sleeper']).
fnpattern(slumber, 40040000, 'sleep', ['agent': 'sleeper']).
fnpattern(snooze, 40040000, 'sleep', ['agent': 'sleeper']).
fnpattern(shake, 40060000, 'body_movement', ['experiencer': 'agent']).
fnpattern(shiver, 40060000, 'body_movement', ['experiencer': 'agent']).
fnpattern(shudder, 40060000, 'body_movement', ['experiencer': 'agent']).
fnpattern(writhe, 40060000, 'body_movement', ['experiencer': 'agent']).
fnpattern(asphyxiate, 40070000, 'death', ['agent': 'cause', 'theme': 'protagonist']).
fnpattern(drown, 40070000, 'death', ['agent': 'cause', 'theme': 'protagonist']).
fnpattern(starve, 40070000, 'death', ['agent': 'cause', 'theme': 'protagonist']).
fnpattern(starve, 40070000, 'killing', ['agent': 'cause', 'theme': 'victim']).
fnpattern(starve, 40070000, 'killing', ['agent': 'killer', 'theme': 'victim']).
fnpattern(suffocate, 40070000, 'death', ['agent': 'cause', 'theme': 'protagonist']).
fnpattern(hurt, 40081000, 'perception_body', ['experiencer': 'experiencer', 'patient': 'body_part']).
fnpattern(itch, 40081000, 'perception_body', ['experiencer': 'experiencer', 'patient': 'body_part']).
fnpattern(ache, 40082000, 'perception_body', ['experiencer': 'experiencer', 'patient': 'body_part']).
fnpattern(burn, 40082000, 'perception_body', ['experiencer': 'experiencer', 'patient': 'body_part']).
fnpattern(prickle, 40082000, 'perception_body', ['experiencer': 'experiencer', 'patient': 'body_part']).
fnpattern(smart, 40082000, 'perception_body', ['experiencer': 'experiencer', 'patient': 'body_part']).
fnpattern(sting, 40082000, 'perception_body', ['experiencer': 'experiencer', 'patient': 'body_part']).
fnpattern(tickle, 40082000, 'perception_body', ['experiencer': 'experiencer', 'patient': 'body_part']).
fnpattern(tingle, 40082000, 'perception_body', ['experiencer': 'experiencer', 'patient': 'body_part']).
fnpattern(stub, 40083100, 'experience_bodily_harm', ['experiencer': 'experiencer', 'patient': 'body_part']).
fnpattern(break, 40083110, 'experience_bodily_harm', ['experiencer': 'experiencer', 'patient': 'body_part']).
fnpattern(pull, 40083110, 'experience_bodily_harm', ['experiencer': 'experiencer', 'patient': 'body_part']).
fnpattern(sprain, 40083110, 'experience_bodily_harm', ['experiencer': 'experiencer', 'patient': 'body_part']).
fnpattern(twist, 40083110, 'experience_bodily_harm', ['experiencer': 'experiencer', 'patient': 'body_part']).
fnpattern(burn, 40083200, 'experience_bodily_harm', ['experiencer': 'experiencer', 'patient': 'body_part']).
fnpattern(cut, 40083200, 'experience_bodily_harm', ['experiencer': 'experiencer', 'patient': 'body_part']).
fnpattern(hurt, 40083200, 'experience_bodily_harm', ['experiencer': 'experiencer', 'patient': 'body_part']).
fnpattern(injure, 40083200, 'experience_bodily_harm', ['experiencer': 'experiencer', 'patient': 'body_part']).
fnpattern(strain, 40083200, 'experience_bodily_harm', ['experiencer': 'experiencer', 'patient': 'body_part']).
fnpattern(bathe, 41011000, 'grooming', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(bathe, 41011000, 'grooming', ['agent': 'agent', 'patient': 'patient']).
fnpattern(shave, 41011000, 'grooming', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(shave, 41011000, 'grooming', ['agent': 'agent', 'patient': 'patient']).
fnpattern(shower, 41011000, 'grooming', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(shower, 41011000, 'grooming', ['agent': 'agent', 'patient': 'patient']).
fnpattern(wash, 41011000, 'grooming', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(wash, 41011000, 'grooming', ['agent': 'agent', 'patient': 'patient']).
fnpattern(groom, 41012000, 'grooming', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(groom, 41012000, 'grooming', ['agent': 'agent', 'patient': 'patient']).
fnpattern(floss, 41021000, 'grooming', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(floss, 41021000, 'grooming', ['agent': 'agent', 'patient': 'patient']).
fnpattern(shave, 41021000, 'grooming', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(shave, 41021000, 'grooming', ['agent': 'agent', 'patient': 'patient']).
fnpattern(wash, 41021000, 'grooming', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(wash, 41021000, 'grooming', ['agent': 'agent', 'patient': 'patient']).
fnpattern(comb, 41022000, 'grooming', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(comb, 41022000, 'grooming', ['agent': 'agent', 'patient': 'patient']).
fnpattern(curl, 41022000, 'hair_configuration', ['patient': 'hair']).
fnpattern(manicure, 41022000, 'grooming', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(manicure, 41022000, 'grooming', ['agent': 'agent', 'patient': 'patient']).
fnpattern(plait, 41022000, 'grooming', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(plait, 41022000, 'grooming', ['agent': 'agent', 'patient': 'patient']).
fnpattern(pluck, 41022000, 'grooming', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(pluck, 41022000, 'grooming', ['agent': 'agent', 'patient': 'patient']).
fnpattern(shampoo, 41022000, 'grooming', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(shampoo, 41022000, 'grooming', ['agent': 'agent', 'patient': 'patient']).
fnpattern(soap, 41022000, 'grooming', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(soap, 41022000, 'grooming', ['agent': 'agent', 'patient': 'patient']).
fnpattern(assassinate, 42010000, 'killing', ['agent': 'killer', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(assassinate, 42010000, 'killing', ['agent': 'cause', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(butcher, 42010000, 'killing', ['agent': 'killer', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(butcher, 42010000, 'killing', ['agent': 'cause', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(eliminate, 42010000, 'killing', ['agent': 'killer', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(eliminate, 42010000, 'killing', ['agent': 'cause', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(execute, 42010000, 'execution', ['agent': 'executioner', 'patient': 'executed']).
fnpattern(liquidate, 42010000, 'killing', ['agent': 'killer', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(liquidate, 42010000, 'killing', ['agent': 'cause', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(massacre, 42010000, 'killing', ['agent': 'killer', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(massacre, 42010000, 'killing', ['agent': 'cause', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(murder, 42010000, 'killing', ['agent': 'killer', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(murder, 42010000, 'killing', ['agent': 'cause', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(slaughter, 42010000, 'killing', ['agent': 'killer', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(slaughter, 42010000, 'killing', ['agent': 'cause', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(slay, 42010000, 'killing', ['agent': 'killer', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(slay, 42010000, 'killing', ['agent': 'cause', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(kill, 42010100, 'killing', ['agent': 'killer', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(kill, 42010100, 'killing', ['agent': 'cause', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(asphyxiate, 42020000, 'killing', ['agent': 'killer', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(asphyxiate, 42020000, 'killing', ['agent': 'cause', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(crucify, 42020000, 'killing', ['agent': 'killer', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(crucify, 42020000, 'killing', ['agent': 'cause', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(drown, 42020000, 'killing', ['agent': 'killer', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(drown, 42020000, 'killing', ['agent': 'cause', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(electrocute, 42020000, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(electrocute, 42020000, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(electrocute, 42020000, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(electrocute, 42020000, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(garrotte, 42020000, 'killing', ['agent': 'killer', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(garrotte, 42020000, 'killing', ['agent': 'cause', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(hang, 42020000, 'execution', ['agent': 'executioner', 'patient': 'executed']).
fnpattern(knife, 42020000, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(knife, 42020000, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(knife, 42020000, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(knife, 42020000, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(smother, 42020000, 'killing', ['agent': 'killer', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(smother, 42020000, 'killing', ['agent': 'cause', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(stab, 42020000, 'cause_harm', ['agent': 'agent', 'patient': 'victim']).
fnpattern(stab, 42020000, 'cause_harm', ['agent': 'agent', 'patient': 'body_part']).
fnpattern(stab, 42020000, 'cause_harm', ['agent': 'cause', 'patient': 'victim']).
fnpattern(stab, 42020000, 'cause_harm', ['agent': 'cause', 'patient': 'body_part']).
fnpattern(suffocate, 42020000, 'killing', ['agent': 'killer', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(suffocate, 42020000, 'killing', ['agent': 'cause', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(flame, 43010000, 'light_movement', ['agent': 'emitter', 'theme': 'beam', 'location': 'source']).
fnpattern(flame, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'figure', 'location': 'ground']).
fnpattern(flame, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'light', 'location': 'ground']).
fnpattern(flare, 43010000, 'light_movement', ['agent': 'emitter', 'theme': 'beam', 'location': 'source']).
fnpattern(flare, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'figure', 'location': 'ground']).
fnpattern(flare, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'light', 'location': 'ground']).
fnpattern(flash, 43010000, 'light_movement', ['agent': 'emitter', 'theme': 'beam', 'location': 'source']).
fnpattern(flash, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'figure', 'location': 'ground']).
fnpattern(flash, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'light', 'location': 'ground']).
fnpattern(flicker, 43010000, 'light_movement', ['agent': 'emitter', 'theme': 'beam', 'location': 'source']).
fnpattern(flicker, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'figure', 'location': 'ground']).
fnpattern(flicker, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'light', 'location': 'ground']).
fnpattern(gleam, 43010000, 'light_movement', ['agent': 'emitter', 'theme': 'beam', 'location': 'source']).
fnpattern(gleam, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'figure', 'location': 'ground']).
fnpattern(gleam, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'light', 'location': 'ground']).
fnpattern(glimmer, 43010000, 'light_movement', ['agent': 'emitter', 'theme': 'beam', 'location': 'source']).
fnpattern(glimmer, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'figure', 'location': 'ground']).
fnpattern(glimmer, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'light', 'location': 'ground']).
fnpattern(glint, 43010000, 'light_movement', ['agent': 'emitter', 'theme': 'beam', 'location': 'source']).
fnpattern(glint, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'figure', 'location': 'ground']).
fnpattern(glint, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'light', 'location': 'ground']).
fnpattern(glisten, 43010000, 'light_movement', ['agent': 'emitter', 'theme': 'beam', 'location': 'source']).
fnpattern(glisten, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'figure', 'location': 'ground']).
fnpattern(glisten, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'light', 'location': 'ground']).
fnpattern(glitter, 43010000, 'light_movement', ['agent': 'emitter', 'theme': 'beam', 'location': 'source']).
fnpattern(glitter, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'figure', 'location': 'ground']).
fnpattern(glitter, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'light', 'location': 'ground']).
fnpattern(glow, 43010000, 'light_movement', ['agent': 'emitter', 'theme': 'beam', 'location': 'source']).
fnpattern(glow, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'figure', 'location': 'ground']).
fnpattern(glow, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'light', 'location': 'ground']).
fnpattern(scintillate, 43010000, 'light_movement', ['agent': 'emitter', 'theme': 'beam', 'location': 'source']).
fnpattern(scintillate, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'figure', 'location': 'ground']).
fnpattern(scintillate, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'light', 'location': 'ground']).
fnpattern(shimmer, 43010000, 'light_movement', ['agent': 'emitter', 'theme': 'beam', 'location': 'source']).
fnpattern(shimmer, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'figure', 'location': 'ground']).
fnpattern(shimmer, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'light', 'location': 'ground']).
fnpattern(shine, 43010000, 'light_movement', ['agent': 'emitter', 'theme': 'beam', 'location': 'source']).
fnpattern(shine, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'figure', 'location': 'ground']).
fnpattern(shine, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'light', 'location': 'ground']).
fnpattern(sparkle, 43010000, 'light_movement', ['agent': 'emitter', 'theme': 'beam', 'location': 'source']).
fnpattern(sparkle, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'figure', 'location': 'ground']).
fnpattern(sparkle, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'light', 'location': 'ground']).
fnpattern(twinkle, 43010000, 'light_movement', ['agent': 'emitter', 'theme': 'beam', 'location': 'source']).
fnpattern(twinkle, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'figure', 'location': 'ground']).
fnpattern(twinkle, 43010000, 'location_of_light', ['agent': 'cause_to_shine', 'theme': 'light', 'location': 'ground']).
fnpattern(bang, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(buzz, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(chug, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(clack, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(clang, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(clank, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(clatter, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(click, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(clink, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(clump, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(clunk, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(crackle, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(crash, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(creak, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(crunch, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(fizz, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(gurgle, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(hiss, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(howl, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(patter, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(ping, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(purr, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(putter, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(roar, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(rumble, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(rustle, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(screech, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(splash, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(splutter, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(squelch, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(swish, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(thud, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(thump, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(thunder, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(wheeze, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(whine, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(whir, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(zing, 43020000, 'motion_noise', ['theme': 'theme', 'location': 'area']).
fnpattern(reek, 43030000, 'chemical-sense_description', ['theme': 'sensory_attribute', 'location': 'source']).
fnpattern(smell, 43030000, 'chemical-sense_description', ['theme': 'sensory_attribute', 'location': 'source']).
fnpattern(stink, 43030000, 'chemical-sense_description', ['theme': 'sensory_attribute', 'location': 'source']).
fnpattern(belch, 43040000, 'excreting', ['theme': 'excreta', 'source': 'excreter']).
fnpattern(bubble, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'goal']).
fnpattern(bubble, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'area']).
fnpattern(dribble, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'goal']).
fnpattern(dribble, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'area']).
fnpattern(drip, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'goal']).
fnpattern(drip, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'area']).
fnpattern(emanate, 43040000, 'emanating', ['theme': 'emission', 'source': 'source', 'location': 'goal']).
fnpattern(emanate, 43040000, 'emanating', ['theme': 'emission', 'source': 'source', 'location': 'path']).
fnpattern(exude, 43040000, 'emanating', ['theme': 'emission', 'source': 'source', 'location': 'goal']).
fnpattern(exude, 43040000, 'emanating', ['theme': 'emission', 'source': 'source', 'location': 'path']).
fnpattern(exude, 43040000, 'emitting', ['theme': 'emission', 'source': 'source_emitter', 'location': 'goal']).
fnpattern(exude, 43040000, 'emitting', ['theme': 'emission', 'source': 'source_emitter', 'location': 'path']).
fnpattern(gush, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'goal']).
fnpattern(gush, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'area']).
fnpattern(leak, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'goal']).
fnpattern(leak, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'area']).
fnpattern(ooze, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'goal']).
fnpattern(ooze, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'area']).
fnpattern(pour, 43040000, 'mass_motion', ['theme': 'mass_theme', 'source': 'source', 'location': 'goal']).
fnpattern(pour, 43040000, 'mass_motion', ['theme': 'mass_theme', 'source': 'source', 'location': 'area']).
fnpattern(pour, 43040000, 'mass_motion', ['theme': 'mass_theme', 'source': 'source', 'location': 'path']).
fnpattern(radiate, 43040000, 'emanating', ['theme': 'emission', 'source': 'source', 'location': 'goal']).
fnpattern(radiate, 43040000, 'emanating', ['theme': 'emission', 'source': 'source', 'location': 'path']).
fnpattern(radiate, 43040000, 'emitting', ['theme': 'emission', 'source': 'source_emitter', 'location': 'goal']).
fnpattern(radiate, 43040000, 'emitting', ['theme': 'emission', 'source': 'source_emitter', 'location': 'path']).
fnpattern(seep, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'goal']).
fnpattern(seep, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'area']).
fnpattern(spew, 43040000, 'excreting', ['theme': 'excreta', 'source': 'excreter']).
fnpattern(spew, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'goal']).
fnpattern(spew, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'area']).
fnpattern(spill, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'goal']).
fnpattern(spill, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'area']).
fnpattern(spout, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'goal']).
fnpattern(spout, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'area']).
fnpattern(spurt, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'goal']).
fnpattern(spurt, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'area']).
fnpattern(squirt, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'goal']).
fnpattern(squirt, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'area']).
fnpattern(stream, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'goal']).
fnpattern(stream, 43040000, 'fluidic_motion', ['theme': 'fluid', 'source': 'source', 'location': 'area']).
fnpattern(stream, 43040000, 'mass_motion', ['theme': 'mass_theme', 'source': 'source', 'location': 'goal']).
fnpattern(stream, 43040000, 'mass_motion', ['theme': 'mass_theme', 'source': 'source', 'location': 'area']).
fnpattern(stream, 43040000, 'mass_motion', ['theme': 'mass_theme', 'source': 'source', 'location': 'path']).
fnpattern(sweat, 43040000, 'excreting', ['theme': 'excreta', 'source': 'excreter']).
fnpattern(annihilate, 44000000, 'destroying', ['agent': 'cause', 'patient': 'undergoer']).
fnpattern(annihilate, 44000000, 'destroying', ['agent': 'destroyer', 'patient': 'undergoer']).
fnpattern(damage, 44000000, 'damaging', ['agent': 'agent', 'patient': 'patient']).
fnpattern(damage, 44000000, 'damaging', ['agent': 'cause', 'patient': 'patient']).
fnpattern(demolish, 44000000, 'destroying', ['agent': 'cause', 'patient': 'undergoer']).
fnpattern(demolish, 44000000, 'destroying', ['agent': 'destroyer', 'patient': 'undergoer']).
fnpattern(destroy, 44000000, 'destroying', ['agent': 'cause', 'patient': 'undergoer']).
fnpattern(destroy, 44000000, 'destroying', ['agent': 'destroyer', 'patient': 'undergoer']).
fnpattern(devastate, 44000000, 'destroying', ['agent': 'cause', 'patient': 'undergoer']).
fnpattern(devastate, 44000000, 'destroying', ['agent': 'destroyer', 'patient': 'undergoer']).
fnpattern(obliterate, 44000000, 'destroying', ['agent': 'cause', 'patient': 'undergoer']).
fnpattern(obliterate, 44000000, 'destroying', ['agent': 'destroyer', 'patient': 'undergoer']).
fnpattern(raze, 44000000, 'destroying', ['agent': 'cause', 'patient': 'undergoer']).
fnpattern(raze, 44000000, 'destroying', ['agent': 'destroyer', 'patient': 'undergoer']).
fnpattern(shatter, 44000000, 'cause_to_fragment', ['agent': 'agent', 'patient': 'whole_patient']).
fnpattern(break, 45010000, 'cause_to_fragment', ['agent': 'agent', 'patient': 'patient']).
fnpattern(chip, 45010000, 'damaging', ['agent': 'agent', 'patient': 'patient']).
fnpattern(fracture, 45010000, 'cause_to_fragment', ['agent': 'agent', 'patient': 'patient']).
fnpattern(rip, 45010000, 'cause_to_fragment', ['agent': 'agent', 'patient': 'patient']).
fnpattern(shatter, 45010000, 'cause_to_fragment', ['agent': 'agent', 'patient': 'patient']).
fnpattern(smash, 45010000, 'cause_to_fragment', ['agent': 'agent', 'patient': 'patient']).
fnpattern(snap, 45010000, 'cause_to_fragment', ['agent': 'agent', 'patient': 'patient']).
fnpattern(splinter, 45010000, 'cause_to_fragment', ['agent': 'agent', 'patient': 'patient']).
fnpattern(split, 45010000, 'separation', ['agent': 'agent', 'patient': 'whole', 'oblique': 'parts']).
fnpattern(split, 45010000, 'separation', ['agent': 'cause', 'patient': 'whole', 'oblique': 'parts']).
fnpattern(tear, 45010000, 'cause_to_fragment', ['agent': 'agent', 'patient': 'patient']).
fnpattern(bend, 45020000, 'reshaping', ['agent': 'cause', 'patient': 'undergoer', 'instrument': 'instrument']).
fnpattern(bend, 45020000, 'reshaping', ['agent': 'deformer', 'patient': 'undergoer', 'instrument': 'instrument']).
fnpattern(crumple, 45020000, 'reshaping', ['agent': 'cause', 'patient': 'undergoer', 'instrument': 'instrument']).
fnpattern(crumple, 45020000, 'reshaping', ['agent': 'deformer', 'patient': 'undergoer', 'instrument': 'instrument']).
fnpattern(fold, 45020000, 'reshaping', ['agent': 'cause', 'patient': 'undergoer', 'instrument': 'instrument']).
fnpattern(fold, 45020000, 'reshaping', ['agent': 'deformer', 'patient': 'undergoer', 'instrument': 'instrument']).
fnpattern(bake, 45030000, 'absorb_heat', ['agent': 'heat_source', 'patient': 'entity']).
fnpattern(bake, 45030000, 'apply_heat', ['agent': 'cook', 'patient': 'food', 'instrument': 'heating_instrument']).
fnpattern(bake, 45030000, 'cooking_creation', ['agent': 'cook', 'patient': 'ingredients', 'instrument': 'heating_instrument']).
fnpattern(blanch, 45030000, 'apply_heat', ['agent': 'cook', 'patient': 'food', 'instrument': 'heating_instrument']).
fnpattern(boil, 45030000, 'absorb_heat', ['agent': 'heat_source', 'patient': 'entity']).
fnpattern(boil, 45030000, 'apply_heat', ['agent': 'cook', 'patient': 'food', 'instrument': 'heating_instrument']).
fnpattern(braise, 45030000, 'apply_heat', ['agent': 'cook', 'patient': 'food', 'instrument': 'heating_instrument']).
fnpattern(broil, 45030000, 'apply_heat', ['agent': 'cook', 'patient': 'food', 'instrument': 'heating_instrument']).
fnpattern(brown, 45030000, 'apply_heat', ['agent': 'cook', 'patient': 'food', 'instrument': 'heating_instrument']).
fnpattern(coddle, 45030000, 'apply_heat', ['agent': 'cook', 'patient': 'food', 'instrument': 'heating_instrument']).
fnpattern(cook, 45030000, 'absorb_heat', ['agent': 'heat_source', 'patient': 'entity']).
fnpattern(cook, 45030000, 'apply_heat', ['agent': 'cook', 'patient': 'food', 'instrument': 'heating_instrument']).
fnpattern(cook, 45030000, 'cooking_creation', ['agent': 'cook', 'patient': 'ingredients', 'instrument': 'heating_instrument']).
fnpattern(fry, 45030000, 'absorb_heat', ['agent': 'heat_source', 'patient': 'entity']).
fnpattern(fry, 45030000, 'apply_heat', ['agent': 'cook', 'patient': 'food', 'instrument': 'heating_instrument']).
fnpattern(grill, 45030000, 'apply_heat', ['agent': 'cook', 'patient': 'food', 'instrument': 'heating_instrument']).
fnpattern(microwave, 45030000, 'apply_heat', ['agent': 'cook', 'patient': 'food', 'instrument': 'heating_instrument']).
fnpattern(parboil, 45030000, 'apply_heat', ['agent': 'cook', 'patient': 'food', 'instrument': 'heating_instrument']).
fnpattern(poach, 45030000, 'apply_heat', ['agent': 'cook', 'patient': 'food', 'instrument': 'heating_instrument']).
fnpattern(reheat, 45030000, 'cause_temperature_change', ['agent': 'agent', 'patient': 'item', 'instrument': 'hot_cold_source']).
fnpattern(reheat, 45030000, 'cause_temperature_change', ['agent': 'cause', 'patient': 'item', 'instrument': 'hot_cold_source']).
fnpattern(reheat, 45030000, 'inchoative_change_of_temperature', ['agent': 'cause', 'patient': 'item', 'instrument': 'hot_cold_source']).
fnpattern(roast, 45030000, 'apply_heat', ['agent': 'cook', 'patient': 'food', 'instrument': 'heating_instrument']).
fnpattern(saute, 45030000, 'apply_heat', ['agent': 'cook', 'patient': 'food', 'instrument': 'heating_instrument']).
fnpattern(scald, 45030000, 'apply_heat', ['agent': 'cook', 'patient': 'food', 'instrument': 'heating_instrument']).
fnpattern(simmer, 45030000, 'absorb_heat', ['agent': 'heat_source', 'patient': 'entity']).
fnpattern(simmer, 45030000, 'apply_heat', ['agent': 'cook', 'patient': 'food', 'instrument': 'heating_instrument']).
fnpattern(steam, 45030000, 'apply_heat', ['agent': 'cook', 'patient': 'food', 'instrument': 'heating_instrument']).
fnpattern(stew, 45030000, 'absorb_heat', ['agent': 'heat_source', 'patient': 'entity']).
fnpattern(stew, 45030000, 'apply_heat', ['agent': 'cook', 'patient': 'food', 'instrument': 'heating_instrument']).
fnpattern(toast, 45030000, 'apply_heat', ['agent': 'cook', 'patient': 'food', 'instrument': 'heating_instrument']).
fnpattern(abrade, 45040000, 'experience_bodily_harm', ['agent': 'experiencer', 'patient': 'body_part', 'instrument': 'injuring_entity']).
fnpattern(advance, 45040000, 'change_position_on_a_scale', ['patient': 'attribute']).
fnpattern(awake, 45040000, 'waking_up', ['patient': 'sleeper']).
fnpattern(awaken, 45040000, 'cause_to_wake', ['agent': 'agent', 'patient': 'sleeper', 'instrument': 'instrument']).
fnpattern(awaken, 45040000, 'cause_to_wake', ['agent': 'cause', 'patient': 'sleeper', 'instrument': 'instrument']).
fnpattern(bisect, 45040000, 'separation', ['agent': 'agent', 'patient': 'whole', 'instrument': 'instrument']).
fnpattern(bisect, 45040000, 'separation', ['agent': 'cause', 'patient': 'whole', 'instrument': 'instrument']).
fnpattern(botch, 45040000, 'bungling', ['agent': 'agent', 'patient': 'patient', 'instrument': 'instrument']).
fnpattern(botch, 45040000, 'bungling', ['agent': 'agent', 'patient': 'action', 'instrument': 'instrument']).
fnpattern(categorize, 45040000, 'categorization', ['agent': 'cognizer', 'patient': 'item']).
fnpattern(chill, 45040000, 'cause_temperature_change', ['agent': 'cause', 'patient': 'item', 'instrument': 'hot_cold_source']).
fnpattern(chill, 45040000, 'cause_temperature_change', ['agent': 'agent', 'patient': 'item', 'instrument': 'hot_cold_source']).
fnpattern(chill, 45040000, 'inchoative_change_of_temperature', ['patient': 'item']).
fnpattern(circumcise, 45040000, 'rite', ['agent': 'leader', 'patient': 'member', 'instrument': 'instrument']).
fnpattern(circumcise, 45040000, 'rite', ['agent': 'leader', 'patient': 'object', 'instrument': 'instrument']).
fnpattern(circumcise, 45040000, 'rite', ['agent': 'guardian', 'patient': 'member', 'instrument': 'instrument']).
fnpattern(circumcise, 45040000, 'rite', ['agent': 'guardian', 'patient': 'object', 'instrument': 'instrument']).
fnpattern(clear, 45040000, 'emptying', ['agent': 'agent', 'instrument': 'theme', 'patient': 'source']).
fnpattern(clear, 45040000, 'emptying', ['agent': 'cause', 'instrument': 'theme', 'patient': 'source']).
fnpattern(coagulate, 45040000, 'change_of_consistency', ['patient': 'undergoer']).
fnpattern(conciliate, 45040000, 'experiencer_obj', ['agent': 'stimulus', 'patient': 'experiencer', 'instrument': 'means']).
fnpattern(condense, 45040000, 'change_of_phase', ['patient': 'undergoer']).
fnpattern(contract, 45040000, 'expansion', ['patient': 'item']).
fnpattern(cool, 45040000, 'cause_temperature_change', ['agent': 'cause', 'patient': 'item', 'instrument': 'hot_cold_source']).
fnpattern(cool, 45040000, 'cause_temperature_change', ['agent': 'agent', 'patient': 'item', 'instrument': 'hot_cold_source']).
fnpattern(cool, 45040000, 'inchoative_change_of_temperature', ['patient': 'item']).
fnpattern(corrode, 45040000, 'corroding', ['patient': 'undergoer']).
fnpattern(curdle, 45040000, 'cause_change_of_consistency', ['agent': 'agent', 'patient': 'undergoer', 'instrument': 'instrument']).
fnpattern(curdle, 45040000, 'cause_change_of_consistency', ['agent': 'agent', 'patient': 'undergoer', 'instrument': 'change_agent']).
fnpattern(curdle, 45040000, 'cause_change_of_consistency', ['agent': 'agent', 'patient': 'undergoer', 'instrument': 'means']).
fnpattern(curdle, 45040000, 'cause_change_of_consistency', ['agent': 'cause', 'patient': 'undergoer', 'instrument': 'instrument']).
fnpattern(curdle, 45040000, 'cause_change_of_consistency', ['agent': 'cause', 'patient': 'undergoer', 'instrument': 'change_agent']).
fnpattern(curdle, 45040000, 'cause_change_of_consistency', ['agent': 'cause', 'patient': 'undergoer', 'instrument': 'means']).
fnpattern(curdle, 45040000, 'change_of_consistency', ['patient': 'undergoer']).
fnpattern(dampen, 45040000, 'cause_to_be_wet', ['agent': 'agent', 'instrument': 'liquid', 'patient': 'undergoer']).
fnpattern(dampen, 45040000, 'cause_to_be_wet', ['agent': 'cause', 'instrument': 'liquid', 'patient': 'undergoer']).
fnpattern(decompose, 45040000, 'rotting', ['patient': 'undergoer']).
fnpattern(decrease, 45040000, 'cause_change_of_scalar_position', ['agent': 'agent', 'patient': 'attribute']).
fnpattern(decrease, 45040000, 'cause_change_of_scalar_position', ['agent': 'cause', 'patient': 'attribute']).
fnpattern(decrease, 45040000, 'change_position_on_a_scale', ['patient': 'attribute']).
fnpattern(defrost, 45040000, 'cause_change_of_phase', ['agent': 'agent', 'patient': 'undergoer']).
fnpattern(defrost, 45040000, 'cause_change_of_phase', ['agent': 'cause', 'patient': 'undergoer']).
fnpattern(defrost, 45040000, 'change_of_phase', ['patient': 'undergoer']).
fnpattern(dehumidify, 45040000, 'cause_to_be_dry', ['agent': 'agent', 'instrument': 'instrument', 'patient': 'dryee']).
fnpattern(dehumidify, 45040000, 'cause_to_be_dry', ['agent': 'cause', 'instrument': 'instrument', 'patient': 'dryee']).
fnpattern(dehydrate, 45040000, 'cause_to_be_dry', ['agent': 'agent', 'instrument': 'instrument', 'patient': 'dryee']).
fnpattern(dehydrate, 45040000, 'cause_to_be_dry', ['agent': 'cause', 'instrument': 'instrument', 'patient': 'dryee']).
fnpattern(desiccate, 45040000, 'cause_to_be_dry', ['agent': 'agent', 'instrument': 'instrument', 'patient': 'dryee']).
fnpattern(desiccate, 45040000, 'cause_to_be_dry', ['agent': 'cause', 'instrument': 'instrument', 'patient': 'dryee']).
fnpattern(diminish, 45040000, 'change_position_on_a_scale', ['patient': 'attribute']).
fnpattern(disperse, 45040000, 'dispersal', ['agent': 'agent', 'patient': 'individuals', 'instrument': 'means']).
fnpattern(disperse, 45040000, 'dispersal', ['agent': 'cause', 'patient': 'individuals', 'instrument': 'means']).
fnpattern(divide, 45040000, 'separation', ['agent': 'agent', 'patient': 'whole', 'instrument': 'instrument']).
fnpattern(divide, 45040000, 'separation', ['agent': 'cause', 'patient': 'whole', 'instrument': 'instrument']).
fnpattern(double, 45040000, 'change_position_on_a_scale', ['patient': 'attribute']).
fnpattern(drain, 45040000, 'emptying', ['agent': 'agent', 'instrument': 'theme', 'patient': 'source']).
fnpattern(drain, 45040000, 'emptying', ['agent': 'cause', 'instrument': 'theme', 'patient': 'source']).
fnpattern(dry, 45040000, 'cause_to_be_dry', ['agent': 'agent', 'instrument': 'instrument', 'patient': 'dryee']).
fnpattern(dry, 45040000, 'cause_to_be_dry', ['agent': 'cause', 'instrument': 'instrument', 'patient': 'dryee']).
fnpattern(ease, 45040000, 'cure', ['agent': 'healer', 'instrument': 'treatment', 'patient': 'affliction']).
fnpattern(ease, 45040000, 'cure', ['agent': 'healer', 'instrument': 'treatment', 'patient': 'patient']).
fnpattern(ease, 45040000, 'cure', ['agent': 'healer', 'instrument': 'treatment', 'patient': 'body_part']).
fnpattern(ease, 45040000, 'cure', ['agent': 'healer', 'instrument': 'medication', 'patient': 'affliction']).
fnpattern(ease, 45040000, 'cure', ['agent': 'healer', 'instrument': 'medication', 'patient': 'patient']).
fnpattern(ease, 45040000, 'cure', ['agent': 'healer', 'instrument': 'medication', 'patient': 'body_part']).
fnpattern(embalm, 45040000, 'preserving', ['agent': 'agent', 'patient': 'undergoer', 'instrument': 'medium']).
fnpattern(embitter, 45040000, 'experiencer_obj', ['agent': 'stimulus', 'patient': 'experiencer', 'instrument': 'means']).
fnpattern(empty, 45040000, 'emptying', ['agent': 'agent', 'instrument': 'theme', 'patient': 'source']).
fnpattern(empty, 45040000, 'emptying', ['agent': 'cause', 'instrument': 'theme', 'patient': 'source']).
fnpattern(enlarge, 45040000, 'cause_expansion', ['agent': 'agent', 'patient': 'item']).
fnpattern(enlarge, 45040000, 'cause_expansion', ['agent': 'cause', 'patient': 'item']).
fnpattern(enlarge, 45040000, 'expansion', ['patient': 'item']).
fnpattern(evaporate, 45040000, 'cause_change_of_phase', ['agent': 'agent', 'patient': 'undergoer']).
fnpattern(evaporate, 45040000, 'cause_change_of_phase', ['agent': 'cause', 'patient': 'undergoer']).
fnpattern(evaporate, 45040000, 'change_of_phase', ['patient': 'undergoer']).
fnpattern(expand, 45040000, 'expansion', ['patient': 'item']).
fnpattern(explode, 45040000, 'change_position_on_a_scale', ['patient': 'attribute']).
fnpattern(fill, 45040000, 'filling', ['agent': 'agent', 'patient': 'goal', 'instrument': 'theme']).
fnpattern(flatten, 45040000, 'reshaping', ['agent': 'deformer', 'instrument': 'instrument', 'patient': 'undergoer']).
fnpattern(flatten, 45040000, 'reshaping', ['agent': 'cause', 'instrument': 'instrument', 'patient': 'undergoer']).
fnpattern(flood, 45040000, 'filling', ['agent': 'agent', 'patient': 'goal', 'instrument': 'theme']).
fnpattern(freeze, 45040000, 'cause_change_of_phase', ['agent': 'agent', 'patient': 'undergoer']).
fnpattern(freeze, 45040000, 'cause_change_of_phase', ['agent': 'cause', 'patient': 'undergoer']).
fnpattern(freeze, 45040000, 'change_of_phase', ['patient': 'undergoer']).
fnpattern(fuse, 45040000, 'amalgamation', ['patient': 'parts']).
fnpattern(fuse, 45040000, 'cause_to_amalgamate', ['agent': 'agent', 'patient': 'parts']).
fnpattern(grow, 45040000, 'change_position_on_a_scale', ['patient': 'attribute']).
fnpattern(harden, 45040000, 'cause_change_of_consistency', ['agent': 'agent', 'patient': 'undergoer', 'instrument': 'instrument']).
fnpattern(harden, 45040000, 'cause_change_of_consistency', ['agent': 'agent', 'patient': 'undergoer', 'instrument': 'change_agent']).
fnpattern(harden, 45040000, 'cause_change_of_consistency', ['agent': 'agent', 'patient': 'undergoer', 'instrument': 'means']).
fnpattern(harden, 45040000, 'cause_change_of_consistency', ['agent': 'cause', 'patient': 'undergoer', 'instrument': 'instrument']).
fnpattern(harden, 45040000, 'cause_change_of_consistency', ['agent': 'cause', 'patient': 'undergoer', 'instrument': 'change_agent']).
fnpattern(harden, 45040000, 'cause_change_of_consistency', ['agent': 'cause', 'patient': 'undergoer', 'instrument': 'means']).
fnpattern(harden, 45040000, 'change_of_consistency', ['patient': 'undergoer']).
fnpattern(heal, 45040000, 'cure', ['agent': 'healer', 'instrument': 'treatment', 'patient': 'affliction']).
fnpattern(heal, 45040000, 'cure', ['agent': 'healer', 'instrument': 'treatment', 'patient': 'patient']).
fnpattern(heal, 45040000, 'cure', ['agent': 'healer', 'instrument': 'treatment', 'patient': 'body_part']).
fnpattern(heal, 45040000, 'cure', ['agent': 'healer', 'instrument': 'medication', 'patient': 'affliction']).
fnpattern(heal, 45040000, 'cure', ['agent': 'healer', 'instrument': 'medication', 'patient': 'patient']).
fnpattern(heal, 45040000, 'cure', ['agent': 'healer', 'instrument': 'medication', 'patient': 'body_part']).
fnpattern(heat, 45040000, 'cause_temperature_change', ['agent': 'cause', 'patient': 'item', 'instrument': 'hot_cold_source']).
fnpattern(heat, 45040000, 'cause_temperature_change', ['agent': 'agent', 'patient': 'item', 'instrument': 'hot_cold_source']).
fnpattern(heat, 45040000, 'inchoative_change_of_temperature', ['patient': 'item']).
fnpattern(humidify, 45040000, 'cause_to_be_wet', ['agent': 'agent', 'instrument': 'liquid', 'patient': 'undergoer']).
fnpattern(humidify, 45040000, 'cause_to_be_wet', ['agent': 'cause', 'instrument': 'liquid', 'patient': 'undergoer']).
fnpattern(hush, 45040000, 'become_silent', ['patient': 'expressor']).
fnpattern(hush, 45040000, 'become_silent', ['patient': 'speaker']).
fnpattern(hush, 45040000, 'silencing', ['agent': 'agent', 'patient': 'speaker', 'instrument': 'instrument']).
fnpattern(hush, 45040000, 'silencing', ['agent': 'agent', 'patient': 'speaker', 'instrument': 'means']).
fnpattern(hush, 45040000, 'silencing', ['agent': 'agent', 'patient': 'expressor', 'instrument': 'instrument']).
fnpattern(hush, 45040000, 'silencing', ['agent': 'agent', 'patient': 'expressor', 'instrument': 'means']).
fnpattern(ignite, 45040000, 'setting_fire', ['agent': 'kindler', 'patients': 'flame', 'patient': 'flammables']).
fnpattern(ignite, 45040000, 'setting_fire', ['agent': 'cause', 'patients': 'flame', 'patient': 'flammables']).
fnpattern(increase, 45040000, 'cause_change_of_scalar_position', ['agent': 'agent', 'patient': 'attribute']).
fnpattern(increase, 45040000, 'cause_change_of_scalar_position', ['agent': 'cause', 'patient': 'attribute']).
fnpattern(increase, 45040000, 'change_position_on_a_scale', ['patient': 'attribute']).
fnpattern(inflate, 45040000, 'cause_expansion', ['agent': 'agent', 'patient': 'item']).
fnpattern(inflate, 45040000, 'cause_expansion', ['agent': 'cause', 'patient': 'item']).
fnpattern(kindle, 45040000, 'setting_fire', ['agent': 'kindler', 'patients': 'flame', 'patient': 'flammables']).
fnpattern(kindle, 45040000, 'setting_fire', ['agent': 'cause', 'patients': 'flame', 'patient': 'flammables']).
fnpattern(lengthen, 45040000, 'cause_expansion', ['agent': 'agent', 'patient': 'item']).
fnpattern(lengthen, 45040000, 'cause_expansion', ['agent': 'cause', 'patient': 'item']).
fnpattern(light, 45040000, 'setting_fire', ['agent': 'kindler', 'patients': 'flame', 'patient': 'flammables']).
fnpattern(light, 45040000, 'setting_fire', ['agent': 'cause', 'patients': 'flame', 'patient': 'flammables']).
fnpattern(liquefy, 45040000, 'cause_change_of_phase', ['agent': 'agent', 'patient': 'undergoer']).
fnpattern(liquefy, 45040000, 'cause_change_of_phase', ['agent': 'cause', 'patient': 'undergoer']).
fnpattern(liquefy, 45040000, 'change_of_phase', ['patient': 'undergoer']).
fnpattern(magnify, 45040000, 'cause_expansion', ['agent': 'agent', 'patient': 'item']).
fnpattern(magnify, 45040000, 'cause_expansion', ['agent': 'cause', 'patient': 'item']).
fnpattern(melt, 45040000, 'cause_change_of_phase', ['agent': 'agent', 'patient': 'undergoer']).
fnpattern(melt, 45040000, 'cause_change_of_phase', ['agent': 'cause', 'patient': 'undergoer']).
fnpattern(melt, 45040000, 'change_of_phase', ['patient': 'undergoer']).
fnpattern(moisten, 45040000, 'cause_to_be_wet', ['agent': 'agent', 'instrument': 'liquid', 'patient': 'undergoer']).
fnpattern(moisten, 45040000, 'cause_to_be_wet', ['agent': 'cause', 'instrument': 'liquid', 'patient': 'undergoer']).
fnpattern(narrow, 45040000, 'cause_expansion', ['agent': 'agent', 'patient': 'item']).
fnpattern(narrow, 45040000, 'cause_expansion', ['agent': 'cause', 'patient': 'item']).
fnpattern(obscure, 45040000, 'eclipse', ['agent': 'obstruction', 'patient': 'eclipsed']).
fnpattern(oxidize, 45040000, 'corroding_caused', ['agent': 'cause', 'patient': 'undergoer']).
fnpattern(putrefy, 45040000, 'rotting', ['patient': 'undergoer']).
fnpattern(quiet, 45040000, 'become_silent', ['patient': 'expressor']).
fnpattern(quiet, 45040000, 'become_silent', ['patient': 'speaker']).
fnpattern(quiet, 45040000, 'silencing', ['agent': 'agent', 'patient': 'speaker', 'instrument': 'instrument']).
fnpattern(quiet, 45040000, 'silencing', ['agent': 'agent', 'patient': 'speaker', 'instrument': 'means']).
fnpattern(quiet, 45040000, 'silencing', ['agent': 'agent', 'patient': 'expressor', 'instrument': 'instrument']).
fnpattern(quiet, 45040000, 'silencing', ['agent': 'agent', 'patient': 'expressor', 'instrument': 'means']).
fnpattern(reproduce, 45040000, 'duplication', ['agent': 'copy', 'patient': 'original']).
fnpattern(reproduce, 45040000, 'duplication', ['agent': 'creator', 'patient': 'original']).
fnpattern(shrink, 45040000, 'cause_expansion', ['agent': 'agent', 'patient': 'item']).
fnpattern(shrink, 45040000, 'cause_expansion', ['agent': 'cause', 'patient': 'item']).
fnpattern(shrink, 45040000, 'expansion', ['patient': 'item']).
fnpattern(silence, 45040000, 'become_silent', ['patient': 'expressor']).
fnpattern(silence, 45040000, 'become_silent', ['patient': 'speaker']).
fnpattern(silence, 45040000, 'killing', ['agent': 'killer', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(silence, 45040000, 'killing', ['agent': 'cause', 'patient': 'victim', 'instrument': 'instrument']).
fnpattern(silence, 45040000, 'silencing', ['agent': 'agent', 'patient': 'speaker', 'instrument': 'instrument']).
fnpattern(silence, 45040000, 'silencing', ['agent': 'agent', 'patient': 'speaker', 'instrument': 'means']).
fnpattern(silence, 45040000, 'silencing', ['agent': 'agent', 'patient': 'expressor', 'instrument': 'instrument']).
fnpattern(silence, 45040000, 'silencing', ['agent': 'agent', 'patient': 'expressor', 'instrument': 'means']).
fnpattern(soak, 45040000, 'cause_to_be_wet', ['agent': 'agent', 'instrument': 'liquid', 'patient': 'undergoer']).
fnpattern(soak, 45040000, 'cause_to_be_wet', ['agent': 'cause', 'instrument': 'liquid', 'patient': 'undergoer']).
fnpattern(soften, 45040000, 'cause_change_of_consistency', ['agent': 'agent', 'patient': 'undergoer', 'instrument': 'instrument']).
fnpattern(soften, 45040000, 'cause_change_of_consistency', ['agent': 'agent', 'patient': 'undergoer', 'instrument': 'change_agent']).
fnpattern(soften, 45040000, 'cause_change_of_consistency', ['agent': 'agent', 'patient': 'undergoer', 'instrument': 'means']).
fnpattern(soften, 45040000, 'cause_change_of_consistency', ['agent': 'cause', 'patient': 'undergoer', 'instrument': 'instrument']).
fnpattern(soften, 45040000, 'cause_change_of_consistency', ['agent': 'cause', 'patient': 'undergoer', 'instrument': 'change_agent']).
fnpattern(soften, 45040000, 'cause_change_of_consistency', ['agent': 'cause', 'patient': 'undergoer', 'instrument': 'means']).
fnpattern(soften, 45040000, 'change_of_consistency', ['patient': 'undergoer']).
fnpattern(solidify, 45040000, 'change_of_phase', ['patient': 'undergoer']).
fnpattern(stretch, 45040000, 'expansion', ['patient': 'item']).
fnpattern(thaw, 45040000, 'cause_change_of_phase', ['agent': 'agent', 'patient': 'undergoer']).
fnpattern(thaw, 45040000, 'cause_change_of_phase', ['agent': 'cause', 'patient': 'undergoer']).
fnpattern(thaw, 45040000, 'change_of_phase', ['patient': 'undergoer']).
fnpattern(thicken, 45040000, 'cause_change_of_consistency', ['agent': 'agent', 'patient': 'undergoer', 'instrument': 'instrument']).
fnpattern(thicken, 45040000, 'cause_change_of_consistency', ['agent': 'agent', 'patient': 'undergoer', 'instrument': 'change_agent']).
fnpattern(thicken, 45040000, 'cause_change_of_consistency', ['agent': 'agent', 'patient': 'undergoer', 'instrument': 'means']).
fnpattern(thicken, 45040000, 'cause_change_of_consistency', ['agent': 'cause', 'patient': 'undergoer', 'instrument': 'instrument']).
fnpattern(thicken, 45040000, 'cause_change_of_consistency', ['agent': 'cause', 'patient': 'undergoer', 'instrument': 'change_agent']).
fnpattern(thicken, 45040000, 'cause_change_of_consistency', ['agent': 'cause', 'patient': 'undergoer', 'instrument': 'means']).
fnpattern(thicken, 45040000, 'change_of_consistency', ['patient': 'undergoer']).
fnpattern(thin, 45040000, 'cause_change_of_consistency', ['agent': 'agent', 'patient': 'undergoer', 'instrument': 'instrument']).
fnpattern(thin, 45040000, 'cause_change_of_consistency', ['agent': 'agent', 'patient': 'undergoer', 'instrument': 'change_agent']).
fnpattern(thin, 45040000, 'cause_change_of_consistency', ['agent': 'agent', 'patient': 'undergoer', 'instrument': 'means']).
fnpattern(thin, 45040000, 'cause_change_of_consistency', ['agent': 'cause', 'patient': 'undergoer', 'instrument': 'instrument']).
fnpattern(thin, 45040000, 'cause_change_of_consistency', ['agent': 'cause', 'patient': 'undergoer', 'instrument': 'change_agent']).
fnpattern(thin, 45040000, 'cause_change_of_consistency', ['agent': 'cause', 'patient': 'undergoer', 'instrument': 'means']).
fnpattern(triple, 45040000, 'change_position_on_a_scale', ['patient': 'attribute']).
fnpattern(vaporize, 45040000, 'change_of_phase', ['patient': 'undergoer']).
fnpattern(wake, 45040000, 'cause_to_wake', ['agent': 'agent', 'patient': 'sleeper', 'instrument': 'instrument']).
fnpattern(wake, 45040000, 'cause_to_wake', ['agent': 'cause', 'patient': 'sleeper', 'instrument': 'instrument']).
fnpattern(wake, 45040000, 'waking_up', ['patient': 'sleeper']).
fnpattern(warm, 45040000, 'cause_temperature_change', ['agent': 'cause', 'patient': 'item', 'instrument': 'hot_cold_source']).
fnpattern(warm, 45040000, 'cause_temperature_change', ['agent': 'agent', 'patient': 'item', 'instrument': 'hot_cold_source']).
fnpattern(warm, 45040000, 'inchoative_change_of_temperature', ['patient': 'item']).
fnpattern(warp, 45040000, 'reshaping', ['agent': 'deformer', 'instrument': 'instrument', 'patient': 'undergoer']).
fnpattern(warp, 45040000, 'reshaping', ['agent': 'cause', 'instrument': 'instrument', 'patient': 'undergoer']).
fnpattern(wet, 45040000, 'cause_to_be_wet', ['agent': 'agent', 'instrument': 'liquid', 'patient': 'undergoer']).
fnpattern(wet, 45040000, 'cause_to_be_wet', ['agent': 'cause', 'instrument': 'liquid', 'patient': 'undergoer']).
fnpattern(widen, 45040000, 'cause_expansion', ['agent': 'agent', 'patient': 'item']).
fnpattern(widen, 45040000, 'cause_expansion', ['agent': 'cause', 'patient': 'item']).
fnpattern(corrode, 45050000, 'corroding', ['patient': 'undergoer']).
fnpattern(corrode, 45050000, 'corroding_caused', ['patient': 'undergoer']).
fnpattern(decay, 45050000, 'rotting', ['patient': 'undergoer']).
fnpattern(rot, 45050000, 'rotting', ['patient': 'undergoer']).
fnpattern(rust, 45050000, 'corroding', ['patient': 'undergoer']).
fnpattern(spoil, 45050000, 'rotting', ['patient': 'undergoer']).
fnpattern(swell, 45050000, 'cause_change_of_scalar_position', ['patient': 'attribute']).
fnpattern(swell, 45050000, 'cause_expansion', ['patient': 'item']).
fnpattern(tarnish, 45050000, 'corroding_caused', ['patient': 'undergoer']).
fnpattern(decline, 45060100, 'change_position_on_a_scale', ['patient': 'item', 'attribute': 'attribute', 'extent': 'difference']).
fnpattern(decrease, 45060100, 'cause_change_of_scalar_position', ['patient': 'item', 'attribute': 'attribute', 'extent': 'difference']).
fnpattern(decrease, 45060100, 'change_position_on_a_scale', ['patient': 'item', 'attribute': 'attribute', 'extent': 'difference']).
fnpattern(diminish, 45060100, 'change_position_on_a_scale', ['patient': 'item', 'attribute': 'attribute', 'extent': 'difference']).
fnpattern(drop, 45060100, 'change_position_on_a_scale', ['patient': 'item', 'attribute': 'attribute', 'extent': 'difference']).
fnpattern(fall, 45060100, 'change_position_on_a_scale', ['patient': 'item', 'attribute': 'attribute', 'extent': 'difference']).
fnpattern(fluctuate, 45060100, 'change_position_on_a_scale', ['patient': 'item', 'attribute': 'attribute', 'extent': 'difference']).
fnpattern(gain, 45060100, 'change_position_on_a_scale', ['patient': 'item', 'attribute': 'attribute', 'extent': 'difference']).
fnpattern(grow, 45060100, 'change_position_on_a_scale', ['patient': 'item', 'attribute': 'attribute', 'extent': 'difference']).
fnpattern(increase, 45060100, 'cause_change_of_scalar_position', ['patient': 'item', 'attribute': 'attribute', 'extent': 'difference']).
fnpattern(increase, 45060100, 'change_position_on_a_scale', ['patient': 'item', 'attribute': 'attribute', 'extent': 'difference']).
fnpattern(jump, 45060100, 'change_position_on_a_scale', ['patient': 'item', 'attribute': 'attribute', 'extent': 'difference']).
fnpattern(lower, 45060100, 'cause_change_of_scalar_position', ['patient': 'item', 'attribute': 'attribute', 'extent': 'difference']).
fnpattern(plummet, 45060100, 'change_position_on_a_scale', ['patient': 'item', 'attribute': 'attribute', 'extent': 'difference']).
fnpattern(rise, 45060100, 'change_position_on_a_scale', ['patient': 'item', 'attribute': 'attribute', 'extent': 'difference']).
fnpattern(rocket, 45060100, 'change_position_on_a_scale', ['patient': 'item', 'attribute': 'attribute', 'extent': 'difference']).
fnpattern(soar, 45060100, 'change_position_on_a_scale', ['patient': 'item', 'attribute': 'attribute', 'extent': 'difference']).
fnpattern(swell, 45060100, 'cause_change_of_scalar_position', ['patient': 'item', 'attribute': 'attribute', 'extent': 'difference']).
fnpattern(swell, 45060100, 'cause_expansion', ['patient': 'item', 'attribute': 'dimension', 'extent': 'size_change']).
fnpattern(tumble, 45060100, 'change_position_on_a_scale', ['patient': 'item', 'attribute': 'attribute', 'extent': 'difference']).
fnpattern(camp, 46000000, 'residence', ['theme': 'resident', 'location': 'goal']).
fnpattern(camp, 46000000, 'residence', ['theme': 'resident', 'location': 'location']).
fnpattern(camp, 46000000, 'residence', ['theme': 'co-resident', 'location': 'goal']).
fnpattern(camp, 46000000, 'residence', ['theme': 'co-resident', 'location': 'location']).
fnpattern(dwell, 46000000, 'residence', ['theme': 'resident', 'location': 'goal']).
fnpattern(dwell, 46000000, 'residence', ['theme': 'resident', 'location': 'location']).
fnpattern(dwell, 46000000, 'residence', ['theme': 'co-resident', 'location': 'goal']).
fnpattern(dwell, 46000000, 'residence', ['theme': 'co-resident', 'location': 'location']).
fnpattern(live, 46000000, 'residence', ['theme': 'resident', 'location': 'goal']).
fnpattern(live, 46000000, 'residence', ['theme': 'resident', 'location': 'location']).
fnpattern(live, 46000000, 'residence', ['theme': 'co-resident', 'location': 'goal']).
fnpattern(live, 46000000, 'residence', ['theme': 'co-resident', 'location': 'location']).
fnpattern(lodge, 46000000, 'residence', ['theme': 'resident', 'location': 'goal']).
fnpattern(lodge, 46000000, 'residence', ['theme': 'resident', 'location': 'location']).
fnpattern(lodge, 46000000, 'residence', ['theme': 'co-resident', 'location': 'goal']).
fnpattern(lodge, 46000000, 'residence', ['theme': 'co-resident', 'location': 'location']).
fnpattern(reside, 46000000, 'residence', ['theme': 'resident', 'location': 'goal']).
fnpattern(reside, 46000000, 'residence', ['theme': 'resident', 'location': 'location']).
fnpattern(reside, 46000000, 'residence', ['theme': 'co-resident', 'location': 'goal']).
fnpattern(reside, 46000000, 'residence', ['theme': 'co-resident', 'location': 'location']).
fnpattern(stay, 46000000, 'residence', ['theme': 'resident', 'location': 'goal']).
fnpattern(stay, 46000000, 'residence', ['theme': 'resident', 'location': 'location']).
fnpattern(stay, 46000000, 'residence', ['theme': 'co-resident', 'location': 'goal']).
fnpattern(stay, 46000000, 'residence', ['theme': 'co-resident', 'location': 'location']).
fnpattern(exist, 47010100, 'existence', ['theme': 'entity']).
fnpattern(persist, 47010100, 'process_continue', ['theme': 'event', 'location': 'goal']).
fnpattern(persist, 47010100, 'process_continue', ['theme': 'event', 'location': 'place']).
fnpattern(cascade, 47020000, 'fluidic_motion', ['theme': 'fluid', 'location': 'goal']).
fnpattern(cascade, 47020000, 'fluidic_motion', ['theme': 'fluid', 'location': 'area']).
fnpattern(corrode, 47020000, 'corroding', ['theme': 'undergoer']).
fnpattern(decay, 47020000, 'rotting', ['theme': 'undergoer']).
fnpattern(decompose, 47020000, 'rotting', ['theme': 'undergoer']).
fnpattern(fester, 47020000, 'rotting', ['theme': 'undergoer']).
fnpattern(fizz, 47020000, 'motion_noise', ['location': 'area', 'theme': 'theme']).
fnpattern(fizz, 47020000, 'motion_noise', ['location': 'goal', 'theme': 'theme']).
fnpattern(flow, 47020000, 'fluidic_motion', ['theme': 'fluid', 'location': 'goal']).
fnpattern(flow, 47020000, 'fluidic_motion', ['theme': 'fluid', 'location': 'area']).
fnpattern(rot, 47020000, 'rotting', ['theme': 'undergoer']).
fnpattern(rust, 47020000, 'corroding', ['theme': 'undergoer']).
fnpattern(spread, 47020000, 'dispersal', ['theme': 'individuals', 'location': 'source']).
fnpattern(spread, 47020000, 'dispersal', ['theme': 'individuals', 'location': 'goal_area']).
fnpattern(stream, 47020000, 'fluidic_motion', ['theme': 'fluid', 'location': 'goal']).
fnpattern(stream, 47020000, 'fluidic_motion', ['theme': 'fluid', 'location': 'area']).
fnpattern(tarnish, 47020000, 'corroding_caused', ['theme': 'undergoer']).
fnpattern(trickle, 47020000, 'fluidic_motion', ['theme': 'fluid', 'location': 'goal']).
fnpattern(trickle, 47020000, 'fluidic_motion', ['theme': 'fluid', 'location': 'area']).
fnpattern(bob, 47030000, 'body_movement', ['agent': 'agent', 'theme': 'body_part', 'location': 'goal']).
fnpattern(bob, 47030000, 'body_movement', ['agent': 'agent', 'theme': 'body_part', 'location': 'area']).
fnpattern(creep, 47030000, 'self_motion', ['location': 'area', 'theme': 'self_mover']).
fnpattern(creep, 47030000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(dance, 47030000, 'self_motion', ['location': 'area', 'theme': 'self_mover']).
fnpattern(dance, 47030000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(drift, 47030000, 'motion', ['location': 'area', 'theme': 'self_mover']).
fnpattern(drift, 47030000, 'motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(flap, 47030000, 'body_movement', ['agent': 'agent', 'theme': 'body_part', 'location': 'goal']).
fnpattern(flap, 47030000, 'body_movement', ['agent': 'agent', 'theme': 'body_part', 'location': 'area']).
fnpattern(float, 47030000, 'motion', ['location': 'area', 'theme': 'self_mover']).
fnpattern(float, 47030000, 'motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(flutter, 47030000, 'body_movement', ['agent': 'agent', 'theme': 'body_part', 'location': 'goal']).
fnpattern(flutter, 47030000, 'body_movement', ['agent': 'agent', 'theme': 'body_part', 'location': 'area']).
fnpattern(jiggle, 47030000, 'body_movement', ['agent': 'agent', 'theme': 'body_part', 'location': 'goal']).
fnpattern(jiggle, 47030000, 'body_movement', ['agent': 'agent', 'theme': 'body_part', 'location': 'area']).
fnpattern(rotate, 47030000, 'cause_to_move_in_place', ['agent': 'agent', 'theme': 'theme', 'location': 'fixed_location']).
fnpattern(rotate, 47030000, 'cause_to_move_in_place', ['agent': 'agent', 'theme': 'body_part', 'location': 'fixed_location']).
fnpattern(rotate, 47030000, 'cause_to_move_in_place', ['agent': 'cause', 'theme': 'theme', 'location': 'fixed_location']).
fnpattern(rotate, 47030000, 'cause_to_move_in_place', ['agent': 'cause', 'theme': 'body_part', 'location': 'fixed_location']).
fnpattern(rotate, 47030000, 'moving_in_place', ['theme': 'theme', 'location': 'fixed_location']).
fnpattern(shake, 47030000, 'body_movement', ['agent': 'agent', 'theme': 'body_part', 'location': 'goal']).
fnpattern(shake, 47030000, 'body_movement', ['agent': 'agent', 'theme': 'body_part', 'location': 'area']).
fnpattern(shake, 47030000, 'cause_to_move_in_place', ['agent': 'agent', 'theme': 'theme', 'location': 'fixed_location']).
fnpattern(shake, 47030000, 'cause_to_move_in_place', ['agent': 'agent', 'theme': 'body_part', 'location': 'fixed_location']).
fnpattern(shake, 47030000, 'cause_to_move_in_place', ['agent': 'cause', 'theme': 'theme', 'location': 'fixed_location']).
fnpattern(shake, 47030000, 'cause_to_move_in_place', ['agent': 'cause', 'theme': 'body_part', 'location': 'fixed_location']).
fnpattern(shake, 47030000, 'moving_in_place', ['theme': 'theme', 'location': 'fixed_location']).
fnpattern(totter, 47030000, 'self_motion', ['location': 'area', 'theme': 'self_mover']).
fnpattern(totter, 47030000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(vibrate, 47030000, 'cause_to_move_in_place', ['agent': 'agent', 'theme': 'theme', 'location': 'fixed_location']).
fnpattern(vibrate, 47030000, 'cause_to_move_in_place', ['agent': 'agent', 'theme': 'body_part', 'location': 'fixed_location']).
fnpattern(vibrate, 47030000, 'cause_to_move_in_place', ['agent': 'cause', 'theme': 'theme', 'location': 'fixed_location']).
fnpattern(vibrate, 47030000, 'cause_to_move_in_place', ['agent': 'cause', 'theme': 'body_part', 'location': 'fixed_location']).
fnpattern(vibrate, 47030000, 'moving_in_place', ['theme': 'theme', 'location': 'fixed_location']).
fnpattern(wave, 47030000, 'body_movement', ['agent': 'agent', 'theme': 'body_part', 'location': 'goal']).
fnpattern(wave, 47030000, 'body_movement', ['agent': 'agent', 'theme': 'body_part', 'location': 'area']).
fnpattern(wiggle, 47030000, 'body_movement', ['agent': 'agent', 'theme': 'body_part', 'location': 'goal']).
fnpattern(wiggle, 47030000, 'body_movement', ['agent': 'agent', 'theme': 'body_part', 'location': 'area']).
fnpattern(writhe, 47030000, 'body_movement', ['agent': 'agent', 'theme': 'body_part', 'location': 'goal']).
fnpattern(writhe, 47030000, 'body_movement', ['agent': 'agent', 'theme': 'body_part', 'location': 'area']).
fnpattern(echo, 47040000, 'sound_movement', ['theme': 'sound', 'location': 'sound_source']).
fnpattern(resound, 47040000, 'sound_movement', ['theme': 'sound', 'location': 'sound_source']).
fnpattern(reverberate, 47040000, 'sound_movement', ['theme': 'sound', 'location': 'sound_source']).
fnpattern(bustle, 47051100, 'self_motion', ['location': 'goal', 'theme': 'self_mover']).
fnpattern(bustle, 47051100, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(crawl, 47051100, 'abundance', ['theme': 'theme', 'location': 'location']).
fnpattern(crawl, 47051100, 'self_motion', ['location': 'goal', 'theme': 'self_mover']).
fnpattern(crawl, 47051100, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(creep, 47051100, 'self_motion', ['location': 'goal', 'theme': 'self_mover']).
fnpattern(creep, 47051100, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(run, 47051100, 'self_motion', ['location': 'goal', 'theme': 'self_mover']).
fnpattern(run, 47051100, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(swarm, 47051100, 'abundance', ['theme': 'theme', 'location': 'location']).
fnpattern(swarm, 47051100, 'mass_motion', ['theme': 'mass_theme', 'location': 'goal']).
fnpattern(swarm, 47051100, 'mass_motion', ['theme': 'mass_theme', 'location': 'location']).
fnpattern(swim, 47051100, 'self_motion', ['location': 'goal', 'theme': 'self_mover']).
fnpattern(swim, 47051100, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(teem, 47051100, 'abundance', ['theme': 'theme', 'location': 'location']).
fnpattern(teem, 47051100, 'mass_motion', ['theme': 'mass_theme', 'location': 'goal']).
fnpattern(teem, 47051100, 'mass_motion', ['theme': 'mass_theme', 'location': 'location']).
fnpattern(throng, 47051100, 'abundance', ['theme': 'theme', 'location': 'location']).
fnpattern(throng, 47051200, 'mass_motion', ['theme': 'mass_theme', 'location': 'goal']).
fnpattern(throng, 47051200, 'mass_motion', ['theme': 'mass_theme', 'location': 'location']).
fnpattern(assemble, 47052000, 'congregating', ['theme': 'individuals']).
fnpattern(assemble, 47052000, 'gathering_up', ['agent': 'agent', 'theme': 'individuals']).
fnpattern(collect, 47052000, 'commerce_collect', ['agent': 'buyer', 'theme': 'goods']).
fnpattern(congregate, 47052000, 'congregating', ['theme': 'individuals']).
fnpattern(convene, 47052000, 'congregating', ['theme': 'individuals']).
fnpattern(convene, 47052000, 'gathering_up', ['agent': 'agent', 'theme': 'individuals']).
fnpattern(flock, 47052000, 'mass_motion', ['theme': 'mass_theme']).
fnpattern(gather, 47052000, 'congregating', ['theme': 'individuals']).
fnpattern(gather, 47052000, 'gathering_up', ['agent': 'agent', 'theme': 'individuals']).
fnpattern(herd, 47052000, 'gathering_up', ['agent': 'agent', 'theme': 'individuals']).
fnpattern(bend, 47060000, 'posture', ['theme': 'theme', 'location': 'location']).
fnpattern(crouch, 47060000, 'posture', ['theme': 'theme', 'location': 'location']).
fnpattern(kneel, 47060000, 'posture', ['theme': 'theme', 'location': 'location']).
fnpattern(lean, 47060000, 'posture', ['theme': 'theme', 'location': 'location']).
fnpattern(lie, 47060000, 'posture', ['theme': 'theme', 'location': 'location']).
fnpattern(sit, 47060000, 'posture', ['theme': 'theme', 'location': 'location']).
fnpattern(slouch, 47060000, 'posture', ['theme': 'theme', 'location': 'location']).
fnpattern(sprawl, 47060000, 'posture', ['theme': 'theme', 'location': 'location']).
fnpattern(squat, 47060000, 'posture', ['theme': 'theme', 'location': 'location']).
fnpattern(stand, 47060000, 'posture', ['theme': 'theme', 'location': 'location']).
fnpattern(stoop, 47060000, 'posture', ['theme': 'theme', 'location': 'location']).
fnpattern(cascade, 47070000, 'fluidic_motion', ['theme': 'fluid', 'location': 'goal']).
fnpattern(cascade, 47070000, 'fluidic_motion', ['theme': 'fluid', 'location': 'area']).
fnpattern(climb, 47070000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(climb, 47070000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(crawl, 47070000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(crawl, 47070000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(drop, 47070000, 'motion_directional', ['theme': 'theme', 'location': 'goal']).
fnpattern(drop, 47070000, 'motion_directional', ['theme': 'theme', 'location': 'area']).
fnpattern(drop, 47070000, 'motion_directional', ['theme': 'theme', 'location': 'path']).
fnpattern(drop, 47070000, 'path_shape', ['theme': 'road', 'location': 'path']).
fnpattern(drop, 47070000, 'path_shape', ['theme': 'road', 'location': 'area']).
fnpattern(go, 47070000, 'motion', ['theme': 'theme', 'location': 'goal']).
fnpattern(go, 47070000, 'motion', ['theme': 'theme', 'location': 'area']).
fnpattern(meander, 47070000, 'path_shape', ['theme': 'road', 'location': 'path']).
fnpattern(meander, 47070000, 'path_shape', ['theme': 'road', 'location': 'area']).
fnpattern(meander, 47070000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(meander, 47070000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(plunge, 47070000, 'motion_directional', ['theme': 'theme', 'location': 'goal']).
fnpattern(plunge, 47070000, 'motion_directional', ['theme': 'theme', 'location': 'area']).
fnpattern(plunge, 47070000, 'motion_directional', ['theme': 'theme', 'location': 'path']).
fnpattern(run, 47070000, 'fluidic_motion', ['theme': 'fluid', 'location': 'goal']).
fnpattern(run, 47070000, 'fluidic_motion', ['theme': 'fluid', 'location': 'area']).
fnpattern(run, 47070000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(run, 47070000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(straggle, 47070000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(straggle, 47070000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(stretch, 47070000, 'expansion', ['theme': 'item']).
fnpattern(swerve, 47070000, 'path_shape', ['theme': 'road', 'location': 'path']).
fnpattern(swerve, 47070000, 'path_shape', ['theme': 'road', 'location': 'area']).
fnpattern(veer, 47070000, 'path_shape', ['theme': 'road', 'location': 'path']).
fnpattern(veer, 47070000, 'path_shape', ['theme': 'road', 'location': 'area']).
fnpattern(wander, 47070000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(wander, 47070000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(weave, 47070000, 'path_shape', ['theme': 'road', 'location': 'path']).
fnpattern(weave, 47070000, 'path_shape', ['theme': 'road', 'location': 'area']).
fnpattern(wind, 47070000, 'path_shape', ['theme': 'road', 'location': 'path']).
fnpattern(wind, 47070000, 'path_shape', ['theme': 'road', 'location': 'area']).
fnpattern(blanket, 47080000, 'adorning', ['theme1': 'theme', 'theme2': 'location']).
fnpattern(cap, 47080000, 'closure', ['theme1': 'fastener', 'theme2': 'containing_object']).
fnpattern(contain, 47080000, 'containing', ['theme1': 'container', 'theme2': 'contents']).
fnpattern(cover, 47080000, 'adorning', ['theme1': 'theme', 'theme2': 'location']).
fnpattern(encircle, 47080000, 'adorning', ['theme1': 'theme', 'theme2': 'location']).
fnpattern(fill, 47080000, 'adorning', ['theme1': 'theme', 'theme2': 'location']).
fnpattern(follow, 47080000, 'cotheme', ['theme1': 'theme', 'theme2': 'cotheme']).
fnpattern(line, 47080000, 'adorning', ['theme1': 'theme', 'theme2': 'location']).
fnpattern(traverse, 47080000, 'path_shape', ['theme1': 'theme', 'theme2': 'road']).
fnpattern(appear, 48011000, 'coming_to_be', ['theme': 'entity', 'location': 'place']).
fnpattern(appear, 48011000, 'coming_to_be', ['theme': 'entity', 'location': 'time']).
fnpattern(arise, 48011000, 'coming_to_be', ['theme': 'entity', 'location': 'place']).
fnpattern(arise, 48011000, 'coming_to_be', ['theme': 'entity', 'location': 'time']).
fnpattern(develop, 48011000, 'coming_to_be', ['theme': 'entity', 'location': 'place']).
fnpattern(develop, 48011000, 'coming_to_be', ['theme': 'entity', 'location': 'time']).
fnpattern(emerge, 48011000, 'coming_to_be', ['theme': 'entity', 'location': 'place']).
fnpattern(emerge, 48011000, 'coming_to_be', ['theme': 'entity', 'location': 'time']).
fnpattern(erupt, 48011000, 'process_start', ['theme': 'event', 'location': 'place']).
fnpattern(erupt, 48011000, 'process_start', ['theme': 'event', 'location': 'time']).
fnpattern(form, 48011000, 'coming_to_be', ['theme': 'entity', 'location': 'place']).
fnpattern(form, 48011000, 'coming_to_be', ['theme': 'entity', 'location': 'time']).
fnpattern(materialize, 48011000, 'coming_to_be', ['theme': 'entity', 'location': 'place']).
fnpattern(materialize, 48011000, 'coming_to_be', ['theme': 'entity', 'location': 'time']).
fnpattern(assert, 48012000, 'statement', ['theme': 'topic', 'agent': 'speaker', 'recipient': 'addressee']).
fnpattern(assert, 48012000, 'statement', ['theme': 'message', 'agent': 'speaker', 'recipient': 'addressee']).
fnpattern(declare, 48012000, 'statement', ['theme': 'topic', 'agent': 'speaker', 'recipient': 'addressee']).
fnpattern(declare, 48012000, 'statement', ['theme': 'message', 'agent': 'speaker', 'recipient': 'addressee']).
fnpattern(expose, 48012000, 'reveal_secret', ['theme': 'topic', 'agent': 'speaker', 'recipient': 'addressee']).
fnpattern(expose, 48012000, 'reveal_secret', ['theme': 'information', 'agent': 'speaker', 'recipient': 'addressee']).
fnpattern(express, 48012000, 'statement', ['theme': 'topic', 'agent': 'speaker', 'recipient': 'addressee']).
fnpattern(express, 48012000, 'statement', ['theme': 'message', 'agent': 'speaker', 'recipient': 'addressee']).
fnpattern(suggest, 48012000, 'statement', ['theme': 'topic', 'agent': 'speaker', 'recipient': 'addressee']).
fnpattern(suggest, 48012000, 'statement', ['theme': 'message', 'agent': 'speaker', 'recipient': 'addressee']).
fnpattern(die, 48020000, 'death', ['theme': 'protagonist']).
fnpattern(disappear, 48020000, 'departing', ['theme': 'theme', 'location': 'source']).
fnpattern(expire, 48020000, 'death', ['theme': 'protagonist']).
fnpattern(perish, 48020000, 'death', ['theme': 'protagonist']).
fnpattern(vanish, 48020000, 'departing', ['theme': 'theme', 'location': 'source']).
fnpattern(befall, 48030000, 'catastrophe', ['theme': 'undesirable_event', 'location': 'place']).
fnpattern(befall, 48030000, 'catastrophe', ['theme': 'undesirable_event', 'location': 'time']).
fnpattern(chance, 48030000, 'daring', ['theme': 'action', 'location': 'place']).
fnpattern(chance, 48030000, 'daring', ['theme': 'action', 'location': 'time']).
fnpattern(happen, 48030000, 'event', ['theme': 'event', 'location': 'place']).
fnpattern(happen, 48030000, 'event', ['theme': 'event', 'location': 'time']).
fnpattern(occur, 48030000, 'event', ['theme': 'event', 'location': 'place']).
fnpattern(occur, 48030000, 'event', ['theme': 'event', 'location': 'time']).
fnpattern(fidget, 49000000, 'body_movement', ['agent': 'entity', 'patient': 'body_part']).
fnpattern(flap, 49000000, 'body_movement', ['agent': 'entity', 'patient': 'body_part']).
fnpattern(totter, 49000000, 'self_motion', ['patient': 'self_mover']).
fnpattern(twitch, 49000000, 'body_movement', ['agent': 'entity', 'patient': 'body_part']).
fnpattern(waggle, 49000000, 'body_movement', ['agent': 'entity', 'patient': 'body_part']).
fnpattern(wiggle, 49000000, 'body_movement', ['agent': 'entity', 'patient': 'body_part']).
fnpattern(wriggle, 49000000, 'body_movement', ['agent': 'entity', 'patient': 'body_part']).
fnpattern(wriggle, 49000000, 'self_motion', ['patient': 'self_mover']).
fnpattern(bend, 50000000, 'change_posture', ['agent': 'protagonist']).
fnpattern(crouch, 50000000, 'change_posture', ['agent': 'protagonist']).
fnpattern(kneel, 50000000, 'change_posture', ['agent': 'protagonist']).
fnpattern(lean, 50000000, 'change_posture', ['agent': 'protagonist']).
fnpattern(lie, 50000000, 'change_posture', ['agent': 'protagonist']).
fnpattern(rise, 50000000, 'change_posture', ['agent': 'protagonist']).
fnpattern(sit, 50000000, 'change_posture', ['agent': 'protagonist']).
fnpattern(slouch, 50000000, 'change_posture', ['agent': 'protagonist']).
fnpattern(sprawl, 50000000, 'change_posture', ['agent': 'protagonist']).
fnpattern(squat, 50000000, 'change_posture', ['agent': 'protagonist']).
fnpattern(stand, 50000000, 'change_posture', ['agent': 'protagonist']).
fnpattern(stoop, 50000000, 'change_posture', ['agent': 'protagonist']).
fnpattern(ascend, 51010100, 'path_shape', ['theme': 'theme', 'location': 'path']).
fnpattern(ascend, 51010100, 'path_shape', ['theme': 'theme', 'location': 'road']).
fnpattern(climb, 51010100, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(climb, 51010100, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(cross, 51010100, 'path_shape', ['theme': 'theme', 'location': 'path']).
fnpattern(cross, 51010100, 'path_shape', ['theme': 'theme', 'location': 'road']).
fnpattern(depart, 51010100, 'departing', ['theme': 'theme', 'location': 'source']).
fnpattern(depart, 51010100, 'departing', ['theme': 'theme', 'location': 'path']).
fnpattern(descend, 51010100, 'path_shape', ['theme': 'theme', 'location': 'path']).
fnpattern(descend, 51010100, 'path_shape', ['theme': 'theme', 'location': 'road']).
fnpattern(enter, 51010100, 'arriving', ['theme': 'theme', 'location': 'goal']).
fnpattern(enter, 51010100, 'arriving', ['theme': 'theme', 'location': 'path']).
fnpattern(enter, 51010100, 'path_shape', ['theme': 'theme', 'location': 'path']).
fnpattern(enter, 51010100, 'path_shape', ['theme': 'theme', 'location': 'road']).
fnpattern(escape, 51010100, 'departing', ['theme': 'theme', 'location': 'source']).
fnpattern(escape, 51010100, 'departing', ['theme': 'theme', 'location': 'path']).
fnpattern(escape, 51010100, 'escaping', ['theme': 'theme', 'location': 'undesirable_location']).
fnpattern(exit, 51010100, 'departing', ['theme': 'theme', 'location': 'source']).
fnpattern(exit, 51010100, 'departing', ['theme': 'theme', 'location': 'path']).
fnpattern(exit, 51010100, 'path_shape', ['theme': 'theme', 'location': 'path']).
fnpattern(exit, 51010100, 'path_shape', ['theme': 'theme', 'location': 'road']).
fnpattern(flee, 51010100, 'escaping', ['theme': 'theme', 'location': 'undesirable_location']).
fnpattern(flee, 51010100, 'evading', ['theme': 'evader']).
fnpattern(leave, 51010100, 'departing', ['theme': 'theme', 'location': 'source']).
fnpattern(leave, 51010100, 'departing', ['theme': 'theme', 'location': 'path']).
fnpattern(leave, 51010100, 'path_shape', ['theme': 'theme', 'location': 'path']).
fnpattern(leave, 51010100, 'path_shape', ['theme': 'theme', 'location': 'road']).
fnpattern(vacate, 51010100, 'departing', ['theme': 'theme', 'location': 'source']).
fnpattern(vacate, 51010100, 'departing', ['theme': 'theme', 'location': 'path']).
fnpattern(vacate, 51010100, 'quitting_a_place', ['theme': 'theme', 'location': 'source']).
fnpattern(vacate, 51010100, 'quitting_a_place', ['theme': 'theme', 'location': 'path']).
fnpattern(approach, 51010200, 'arriving', ['theme': 'theme', 'location': 'goal']).
fnpattern(approach, 51010200, 'arriving', ['theme': 'theme', 'location': 'path']).
fnpattern(come, 51010200, 'arriving', ['theme': 'theme', 'location': 'goal']).
fnpattern(come, 51010200, 'arriving', ['theme': 'theme', 'location': 'path']).
fnpattern(fall, 51010200, 'motion_directional', ['location': 'area', 'theme': 'theme']).
fnpattern(fall, 51010200, 'motion_directional', ['theme': 'theme', 'location': 'goal']).
fnpattern(go, 51010200, 'motion', ['location': 'area', 'theme': 'theme']).
fnpattern(go, 51010200, 'motion', ['theme': 'theme', 'location': 'goal']).
fnpattern(plunge, 51010200, 'motion_directional', ['location': 'area', 'theme': 'theme']).
fnpattern(plunge, 51010200, 'motion_directional', ['theme': 'theme', 'location': 'goal']).
fnpattern(return, 51010200, 'arriving', ['theme': 'theme', 'location': 'goal']).
fnpattern(return, 51010200, 'arriving', ['theme': 'theme', 'location': 'path']).
fnpattern(rise, 51010200, 'path_shape', ['theme': 'theme', 'location': 'path']).
fnpattern(rise, 51010200, 'path_shape', ['theme': 'theme', 'location': 'road']).
fnpattern(arrive, 51010210, 'arriving', ['theme': 'theme', 'location': 'goal']).
fnpattern(arrive, 51010210, 'arriving', ['theme': 'theme', 'location': 'path']).
fnpattern(abandon, 51020000, 'departing', ['theme': 'theme', 'source': 'source']).
fnpattern(desert, 51020100, 'departing', ['theme': 'theme', 'source': 'source']).
fnpattern(leave, 51020100, 'departing', ['theme': 'theme', 'source': 'source']).
fnpattern(drift, 51031000, 'motion', ['location': 'area', 'theme': 'theme']).
fnpattern(drift, 51031000, 'motion', ['theme': 'theme', 'location': 'goal']).
fnpattern(drop, 51031000, 'motion_directional', ['location': 'area', 'theme': 'theme']).
fnpattern(drop, 51031000, 'motion_directional', ['theme': 'theme', 'location': 'goal']).
fnpattern(drop, 51031000, 'path_shape', ['theme': 'theme', 'location': 'path']).
fnpattern(drop, 51031000, 'path_shape', ['theme': 'theme', 'location': 'road']).
fnpattern(float, 51031000, 'motion', ['location': 'area', 'theme': 'theme']).
fnpattern(float, 51031000, 'motion', ['theme': 'theme', 'location': 'goal']).
fnpattern(glide, 51031000, 'motion', ['location': 'area', 'theme': 'theme']).
fnpattern(glide, 51031000, 'motion', ['theme': 'theme', 'location': 'goal']).
fnpattern(move, 51031000, 'motion', ['location': 'area', 'theme': 'theme']).
fnpattern(move, 51031000, 'motion', ['theme': 'theme', 'location': 'goal']).
fnpattern(roll, 51031000, 'motion', ['location': 'area', 'theme': 'theme']).
fnpattern(roll, 51031000, 'motion', ['theme': 'theme', 'location': 'goal']).
fnpattern(rotate, 51031000, 'cause_to_move_in_place', ['agent': 'agent', 'theme': 'theme', 'location': 'fixed_location']).
fnpattern(rotate, 51031000, 'cause_to_move_in_place', ['agent': 'agent', 'theme': 'body_part', 'location': 'fixed_location']).
fnpattern(rotate, 51031000, 'cause_to_move_in_place', ['agent': 'cause', 'theme': 'theme', 'location': 'fixed_location']).
fnpattern(rotate, 51031000, 'cause_to_move_in_place', ['agent': 'cause', 'theme': 'body_part', 'location': 'fixed_location']).
fnpattern(rotate, 51031000, 'moving_in_place', ['theme': 'theme', 'location': 'fixed_location']).
fnpattern(spin, 51031000, 'cause_to_move_in_place', ['agent': 'agent', 'theme': 'theme', 'location': 'fixed_location']).
fnpattern(spin, 51031000, 'cause_to_move_in_place', ['agent': 'agent', 'theme': 'body_part', 'location': 'fixed_location']).
fnpattern(spin, 51031000, 'cause_to_move_in_place', ['agent': 'cause', 'theme': 'theme', 'location': 'fixed_location']).
fnpattern(spin, 51031000, 'cause_to_move_in_place', ['agent': 'cause', 'theme': 'body_part', 'location': 'fixed_location']).
fnpattern(swing, 51031000, 'path_shape', ['theme': 'theme', 'location': 'path']).
fnpattern(swing, 51031000, 'path_shape', ['theme': 'theme', 'location': 'road']).
fnpattern(turn, 51031000, 'cause_to_move_in_place', ['agent': 'agent', 'theme': 'theme', 'location': 'fixed_location']).
fnpattern(turn, 51031000, 'cause_to_move_in_place', ['agent': 'agent', 'theme': 'body_part', 'location': 'fixed_location']).
fnpattern(turn, 51031000, 'cause_to_move_in_place', ['agent': 'cause', 'theme': 'theme', 'location': 'fixed_location']).
fnpattern(turn, 51031000, 'cause_to_move_in_place', ['agent': 'cause', 'theme': 'body_part', 'location': 'fixed_location']).
fnpattern(twirl, 51031000, 'cause_to_move_in_place', ['agent': 'agent', 'theme': 'theme', 'location': 'fixed_location']).
fnpattern(twirl, 51031000, 'cause_to_move_in_place', ['agent': 'agent', 'theme': 'body_part', 'location': 'fixed_location']).
fnpattern(twirl, 51031000, 'cause_to_move_in_place', ['agent': 'cause', 'theme': 'theme', 'location': 'fixed_location']).
fnpattern(twirl, 51031000, 'cause_to_move_in_place', ['agent': 'cause', 'theme': 'body_part', 'location': 'fixed_location']).
fnpattern(twirl, 51031000, 'moving_in_place', ['theme': 'theme', 'location': 'fixed_location']).
fnpattern(wind, 51031000, 'path_shape', ['theme': 'theme', 'location': 'path']).
fnpattern(wind, 51031000, 'path_shape', ['theme': 'theme', 'location': 'road']).
fnpattern(amble, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(amble, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(amble, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(bolt, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(bolt, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(bolt, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(bound, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(bound, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(bound, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(canter, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(canter, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(canter, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(clamber, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(clamber, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(clamber, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(climb, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(climb, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(climb, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(coast, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(coast, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(coast, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(crawl, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(crawl, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(crawl, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(creep, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(creep, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(creep, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(dart, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(dart, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(dart, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(dash, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(dash, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(dash, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(drift, 51032000, 'motion', ['location': 'area', 'theme': 'theme']).
fnpattern(drift, 51032000, 'motion', ['theme': 'theme', 'location': 'goal']).
fnpattern(flit, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(flit, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(flit, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(float, 51032000, 'motion', ['location': 'area', 'theme': 'theme']).
fnpattern(float, 51032000, 'motion', ['theme': 'theme', 'location': 'goal']).
fnpattern(fly, 51032000, 'motion', ['location': 'area', 'theme': 'theme']).
fnpattern(fly, 51032000, 'motion', ['theme': 'theme', 'location': 'goal']).
fnpattern(frolic, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(frolic, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(frolic, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(gambol, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(gambol, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(gambol, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(glide, 51032000, 'motion', ['location': 'area', 'theme': 'theme']).
fnpattern(glide, 51032000, 'motion', ['theme': 'theme', 'location': 'goal']).
fnpattern(hasten, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(hasten, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(hasten, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(hike, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(hike, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(hike, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(hobble, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(hobble, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(hobble, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(hurry, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(hurry, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(hurry, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(jog, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(jog, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(jog, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(jump, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(jump, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(jump, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(leap, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(leap, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(leap, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(limp, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(limp, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(limp, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(lope, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(lope, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(lope, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(lumber, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(lumber, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(lumber, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(lurch, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(lurch, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(lurch, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(march, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(march, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(march, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(meander, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(meander, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(meander, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(mince, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(mince, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(mince, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(mosey, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(mosey, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(mosey, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(pad, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(pad, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(pad, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(parade, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(parade, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(parade, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(plod, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(plod, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(plod, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(prance, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(prance, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(prance, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(promenade, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(promenade, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(promenade, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(prowl, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(prowl, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(prowl, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(roam, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(roam, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(roam, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(roll, 51032000, 'motion', ['location': 'area', 'theme': 'theme']).
fnpattern(roll, 51032000, 'motion', ['theme': 'theme', 'location': 'goal']).
fnpattern(romp, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(romp, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(romp, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(run, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(run, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(run, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(rush, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(rush, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(rush, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(sashay, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(sashay, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(sashay, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(saunter, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(saunter, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(saunter, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(scamper, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(scamper, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(scamper, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(scoot, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(scoot, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(scoot, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(scramble, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(scramble, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(scramble, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(scurry, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(scurry, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(scurry, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(scuttle, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(scuttle, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(scuttle, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(shuffle, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(shuffle, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(shuffle, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(sidle, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(sidle, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(sidle, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(skip, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(skip, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(skip, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(skulk, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(skulk, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(skulk, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(sleepwalk, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(sleepwalk, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(sleepwalk, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(slink, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(slink, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(slink, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(slither, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(slither, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(slither, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(slog, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(slog, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(slog, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(sneak, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(sneak, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(sneak, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(stagger, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(stagger, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(stagger, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(step, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(step, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(step, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(stomp, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(stomp, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(stomp, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(stride, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(stride, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(stride, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(stroll, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(stroll, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(stroll, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(strut, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(strut, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(strut, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(stumble, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(stumble, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(stumble, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(swagger, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(swagger, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(swagger, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(swim, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(swim, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(swim, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(tack, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(tack, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(tack, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(tiptoe, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(tiptoe, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(tiptoe, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(toddle, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(toddle, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(toddle, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(totter, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(totter, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(totter, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(tour, 51032000, 'travel', ['theme': 'traveler', 'location': 'area']).
fnpattern(tour, 51032000, 'travel', ['theme': 'traveler', 'location': 'goal']).
fnpattern(tour, 51032000, 'travel', ['theme': 'traveler', 'location': 'path']).
fnpattern(traipse, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(traipse, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(traipse, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(tramp, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(tramp, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(tramp, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(trek, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(trek, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(trek, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(troop, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(troop, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(troop, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(trot, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(trot, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(trot, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(trudge, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(trudge, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(trudge, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(trundle, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(trundle, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(trundle, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(vault, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(vault, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(vault, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(waddle, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(waddle, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(waddle, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(wade, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(wade, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(wade, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(walk, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(walk, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(walk, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(wander, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(wander, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'path']).
fnpattern(wander, 51032000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(balloon, 51041000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'area']).
fnpattern(balloon, 51041000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'goal']).
fnpattern(boat, 51041000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'area']).
fnpattern(boat, 51041000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'goal']).
fnpattern(caravan, 51041000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'area']).
fnpattern(caravan, 51041000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'goal']).
fnpattern(jet, 51041000, 'ride_vehicle', ['agent': 'vehicle', 'theme': 'theme', 'location': 'area']).
fnpattern(jet, 51041000, 'ride_vehicle', ['agent': 'vehicle', 'theme': 'theme', 'location': 'goal']).
fnpattern(motor, 51041000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'area']).
fnpattern(motor, 51041000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'goal']).
fnpattern(parachute, 51041000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'area']).
fnpattern(parachute, 51041000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'goal']).
fnpattern(punt, 51041000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'area']).
fnpattern(punt, 51041000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'goal']).
fnpattern(sledge, 51041000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'area']).
fnpattern(sledge, 51041000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'goal']).
fnpattern(taxi, 51041000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'area']).
fnpattern(taxi, 51041000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'goal']).
fnpattern(taxi, 51041000, 'ride_vehicle', ['agent': 'vehicle', 'theme': 'theme', 'location': 'area']).
fnpattern(taxi, 51041000, 'ride_vehicle', ['agent': 'vehicle', 'theme': 'theme', 'location': 'goal']).
fnpattern(bicycle, 51041100, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'area']).
fnpattern(bicycle, 51041100, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'goal']).
fnpattern(bike, 51041100, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'area']).
fnpattern(bike, 51041100, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'goal']).
fnpattern(canoe, 51041100, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'area']).
fnpattern(canoe, 51041100, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'goal']).
fnpattern(cycle, 51041100, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'area']).
fnpattern(cycle, 51041100, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'goal']).
fnpattern(raft, 51041100, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'area']).
fnpattern(raft, 51041100, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'goal']).
fnpattern(skate, 51041100, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'area']).
fnpattern(skate, 51041100, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'goal']).
fnpattern(toboggan, 51041100, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'area']).
fnpattern(toboggan, 51041100, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'goal']).
fnpattern(cruise, 51042000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'area']).
fnpattern(cruise, 51042000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'goal']).
fnpattern(cruise, 51042000, 'ride_vehicle', ['agent': 'vehicle', 'theme': 'theme', 'location': 'area']).
fnpattern(cruise, 51042000, 'ride_vehicle', ['agent': 'vehicle', 'theme': 'theme', 'location': 'goal']).
fnpattern(drive, 51042000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'area']).
fnpattern(drive, 51042000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'goal']).
fnpattern(fly, 51042000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'area']).
fnpattern(fly, 51042000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'goal']).
fnpattern(fly, 51042000, 'ride_vehicle', ['agent': 'vehicle', 'theme': 'theme', 'location': 'area']).
fnpattern(fly, 51042000, 'ride_vehicle', ['agent': 'vehicle', 'theme': 'theme', 'location': 'goal']).
fnpattern(paddle, 51042000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'area']).
fnpattern(paddle, 51042000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'goal']).
fnpattern(pedal, 51042000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'area']).
fnpattern(pedal, 51042000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'goal']).
fnpattern(ride, 51042000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'area']).
fnpattern(ride, 51042000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'goal']).
fnpattern(row, 51042000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'area']).
fnpattern(row, 51042000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'goal']).
fnpattern(sail, 51042000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'area']).
fnpattern(sail, 51042000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'goal']).
fnpattern(sail, 51042000, 'ride_vehicle', ['agent': 'vehicle', 'theme': 'theme', 'location': 'area']).
fnpattern(sail, 51042000, 'ride_vehicle', ['agent': 'vehicle', 'theme': 'theme', 'location': 'goal']).
fnpattern(tack, 51042000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'area']).
fnpattern(tack, 51042000, 'operate_vehicle', ['agent': 'driver', 'theme': 'vehicle', 'location': 'goal']).
fnpattern(voyage, 51042000, 'travel', ['agent': 'traveler', 'theme': 'co-participant', 'location': 'area']).
fnpattern(voyage, 51042000, 'travel', ['agent': 'traveler', 'theme': 'co-participant', 'location': 'goal']).
fnpattern(voyage, 51042000, 'travel', ['agent': 'traveler', 'theme': 'co-participant', 'location': 'path']).
fnpattern(voyage, 51042000, 'travel', ['agent': 'traveler', 'theme': 'baggage', 'location': 'area']).
fnpattern(voyage, 51042000, 'travel', ['agent': 'traveler', 'theme': 'baggage', 'location': 'goal']).
fnpattern(voyage, 51042000, 'travel', ['agent': 'traveler', 'theme': 'baggage', 'location': 'path']).
fnpattern(bop, 51050000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(bop, 51050000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(dance, 51050000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(dance, 51050000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(shuffle, 51050000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(shuffle, 51050000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(waltz, 51050000, 'self_motion', ['theme': 'self_mover', 'location': 'area']).
fnpattern(waltz, 51050000, 'self_motion', ['theme': 'self_mover', 'location': 'goal']).
fnpattern(chase, 51060000, 'cotheme', ['agent': 'theme', 'theme': 'cotheme']).
fnpattern(follow, 51060000, 'cotheme', ['agent': 'theme', 'theme': 'cotheme']).
fnpattern(pursue, 51060000, 'cotheme', ['agent': 'theme', 'theme': 'cotheme']).
fnpattern(shadow, 51060000, 'cotheme', ['agent': 'theme', 'theme': 'cotheme']).
fnpattern(tail, 51060000, 'cotheme', ['agent': 'theme', 'theme': 'cotheme']).
fnpattern(track, 51060000, 'cotheme', ['agent': 'theme', 'theme': 'cotheme']).
fnpattern(trail, 51060000, 'cotheme', ['agent': 'theme', 'theme': 'cotheme']).
fnpattern(accompany, 51070000, 'cotheme', ['agent': 'theme', 'theme': 'cotheme']).
fnpattern(conduct, 51070000, 'cotheme', ['agent': 'theme', 'theme': 'cotheme']).
fnpattern(escort, 51070000, 'cotheme', ['agent': 'theme', 'theme': 'cotheme']).
fnpattern(guide, 51070000, 'cotheme', ['agent': 'theme', 'theme': 'cotheme']).
fnpattern(lead, 51070000, 'cotheme', ['agent': 'theme', 'theme': 'cotheme']).
fnpattern(shepherd, 51070000, 'cotheme', ['agent': 'theme', 'theme': 'cotheme']).
fnpattern(reach, 51080000, 'arriving', ['theme': 'theme', 'location': 'goal']).
fnpattern(reach, 51080000, 'arriving', ['theme': 'theme', 'location': 'path']).
fnpattern(avoid, 52000000, 'avoiding', ['agent': 'agent', 'theme': 'undesirable_situation', 'location': 'place']).
fnpattern(avoid, 52000000, 'avoiding', ['agent': 'agent', 'location': 'undesirable_situation']).
fnpattern(avoid, 52000000, 'preventing', ['agent': 'preventing_cause', 'theme': 'event']).
fnpattern(dodge, 52000000, 'avoiding', ['agent': 'agent', 'theme': 'undesirable_situation', 'location': 'place']).
fnpattern(dodge, 52000000, 'avoiding', ['agent': 'agent', 'location': 'undesirable_situation']).
fnpattern(duck, 52000000, 'avoiding', ['agent': 'agent', 'theme': 'undesirable_situation', 'location': 'place']).
fnpattern(duck, 52000000, 'avoiding', ['agent': 'agent', 'location': 'undesirable_situation']).
fnpattern(elude, 52000000, 'evading', ['agent': 'evader', 'theme': 'pursuer']).
fnpattern(eschew, 52000000, 'avoiding', ['agent': 'agent', 'theme': 'undesirable_situation', 'location': 'place']).
fnpattern(eschew, 52000000, 'avoiding', ['agent': 'agent', 'location': 'undesirable_situation']).
fnpattern(evade, 52000000, 'avoiding', ['agent': 'agent', 'theme': 'undesirable_situation', 'location': 'place']).
fnpattern(evade, 52000000, 'avoiding', ['agent': 'agent', 'location': 'undesirable_situation']).
fnpattern(evade, 52000000, 'evading', ['agent': 'evader', 'theme': 'pursuer']).
fnpattern(shun, 52000000, 'avoiding', ['agent': 'agent', 'theme': 'undesirable_situation', 'location': 'place']).
fnpattern(shun, 52000000, 'avoiding', ['agent': 'agent', 'location': 'undesirable_situation']).
fnpattern(sidestep, 52000000, 'avoiding', ['agent': 'agent', 'theme': 'undesirable_situation', 'location': 'place']).
fnpattern(sidestep, 52000000, 'avoiding', ['agent': 'agent', 'location': 'undesirable_situation']).
fnpattern(equivocate, 53010000, 'prevarication', ['agent': 'speaker', 'theme': 'topic']).
fnpattern(delay, 53010100, 'hindering', ['agent': 'agent', 'theme': 'action']).
fnpattern(measure, 54010000, 'dimension', ['theme': 'object', 'value': 'measurement']).
fnpattern(total, 54010000, 'adding_up', ['agent': 'cognizer', 'theme': 'numbers', 'value': 'result']).
fnpattern(total, 54010000, 'amounting_to', ['theme': 'attribute', 'value': 'value']).
fnpattern(weigh, 54010000, 'dimension', ['theme': 'object', 'value': 'measurement']).
fnpattern(cost, 54020000, 'expensiveness', ['theme': 'goods', 'value': 'asset', 'benefactor': 'payer']).
fnpattern(contain, 54030000, 'containing', ['location': 'container', 'theme': 'contents']).
fnpattern(hold, 54030000, 'containing', ['location': 'container', 'theme': 'contents']).
fnpattern(assess, 54040000, 'assessing', ['agent': 'assessor', 'theme': 'feature', 'value': 'value']).
fnpattern(peg, 54040000, 'categorization', ['agent': 'cognizer', 'theme': 'item', 'value': 'category']).
fnpattern(rate, 54040000, 'assessing', ['agent': 'assessor', 'theme': 'feature', 'value': 'value']).
fnpattern(scrimp, 54050000, 'frugality', ['agent': 'resouce_controller', 'theme': 'resouce']).
fnpattern(begin, 55010100, 'activity_start', ['agent': 'agent', 'theme': 'activity', 'time': 'time']).
fnpattern(resume, 55010100, 'process_resume', ['theme': 'process', 'time': 'time']).
fnpattern(start, 55010100, 'activity_start', ['agent': 'agent', 'theme': 'activity', 'time': 'time']).
fnpattern(start, 55010100, 'process_start', ['theme': 'event', 'time': 'time']).
fnpattern(keep, 55010210, 'activity_ongoing', ['agent': 'agent', 'theme': 'activity']).
fnpattern(continue, 55010221, 'activity_ongoing', ['agent': 'agent', 'theme': 'activity']).
fnpattern(continue, 55010221, 'process_continue', ['theme': 'event', 'time': 'time']).
fnpattern(end, 55010300, 'process_end', ['theme': 'process']).
fnpattern(finish, 55010300, 'activity_finish', ['agent': 'agent', 'theme': 'activity']).
fnpattern(discontinue, 55020000, 'process_stop', ['theme': 'process']).
fnpattern(quit, 55020000, 'process_stop', ['theme': 'process']).
fnpattern(halt, 55040000, 'activity_stop', ['agent': 'agent', 'theme': 'activity', 'instrument': 'means']).
fnpattern(halt, 55040000, 'halt', ['theme': 'activity', 'instrument': 'means']).
fnpattern(terminate, 55040000, 'activity_finish', ['agent': 'agent', 'theme': 'activity', 'instrument': 'means']).
fnpattern(terminate, 55040000, 'activity_stop', ['agent': 'agent', 'theme': 'activity', 'instrument': 'means']).
fnpattern(terminate, 55040000, 'firing', ['agent': 'employer', 'theme': 'employee']).
fnpattern(terminate, 55040000, 'killing', ['agent': 'killer', 'theme': 'victim', 'instrument': 'instrument']).
fnpattern(terminate, 55040000, 'killing', ['agent': 'killer', 'theme': 'victim', 'instrument': 'means']).
fnpattern(terminate, 55040000, 'killing', ['agent': 'cause', 'theme': 'victim', 'instrument': 'instrument']).
fnpattern(terminate, 55040000, 'killing', ['agent': 'cause', 'theme': 'victim', 'instrument': 'means']).
fnpattern(drizzle, 57000000, 'precipitation', ['theme': 'precipitation']).
fnpattern(hail, 57000000, 'precipitation', ['theme': 'precipitation']).
fnpattern(rain, 57000000, 'precipitation', ['theme': 'precipitation']).
fnpattern(sleet, 57000000, 'precipitation', ['theme': 'precipitation']).
fnpattern(snow, 57000000, 'precipitation', ['theme': 'precipitation']).
fnpattern(sprinkle, 57000000, 'precipitation', ['theme': 'precipitation']).
fnpattern(storm, 57000000, 'weather', ['': '']).
fnpattern(implore, 58000000, 'request', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'message']).
fnpattern(implore, 58000000, 'request', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'topic']).
fnpattern(persuade, 58000000, 'suasion', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'content']).
fnpattern(persuade, 58000000, 'suasion', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'text']).
fnpattern(persuade, 58000000, 'suasion', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'topic']).
fnpattern(urge, 58000000, 'request', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'message']).
fnpattern(urge, 58000000, 'request', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'topic']).
fnpattern(commission, 59000000, 'employing', ['agent': 'employer', 'patient': 'employee', 'proposition': 'field']).
fnpattern(commission, 59000000, 'employing', ['agent': 'employer', 'patient': 'employee', 'proposition': 'position']).
fnpattern(commission, 59000000, 'employing', ['agent': 'employer', 'patient': 'employee', 'proposition': 'task']).
fnpattern(commission, 59000000, 'hiring', ['agent': 'employer', 'patient': 'employee', 'proposition': 'position']).
fnpattern(commission, 59000000, 'hiring', ['agent': 'employer', 'patient': 'employee', 'proposition': 'task']).
fnpattern(dare, 59000000, 'daring', ['agent': 'agent', 'proposition': 'action']).
fnpattern(incite, 59000000, 'talking_into', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'content']).
fnpattern(incite, 59000000, 'talking_into', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'goods']).
fnpattern(induce, 59000000, 'talking_into', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'content']).
fnpattern(induce, 59000000, 'talking_into', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'goods']).
fnpattern(mislead, 59000000, 'prevarication', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'topic']).
fnpattern(press, 59000000, 'attempt_suasion', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'content']).
fnpattern(press, 59000000, 'attempt_suasion', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'topic']).
fnpattern(pressure, 59000000, 'attempt_suasion', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'content']).
fnpattern(pressure, 59000000, 'attempt_suasion', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'topic']).
fnpattern(spur, 59000000, 'attempt_suasion', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'content']).
fnpattern(spur, 59000000, 'attempt_suasion', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'topic']).
fnpattern(tempt, 59000000, 'attempt_suasion', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'content']).
fnpattern(tempt, 59000000, 'attempt_suasion', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'topic']).
fnpattern(cajole, 59000100, 'attempt_suasion', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'content']).
fnpattern(cajole, 59000100, 'attempt_suasion', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'topic']).
fnpattern(deceive, 59000100, 'prevarication', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'topic']).
fnpattern(fool, 59000100, 'prevarication', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'topic']).
fnpattern(hoodwink, 59000100, 'prevarication', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'topic']).
fnpattern(wheedle, 59000100, 'attempt_suasion', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'content']).
fnpattern(wheedle, 59000100, 'attempt_suasion', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'topic']).
fnpattern(permit, 60000000, 'grant_permission', ['agent': 'grantor', 'patient': 'grantee', 'proposition': 'action']).
fnpattern(permit, 60000000, 'permitting', ['agent': 'principle', 'patient': 'state_of_affairs']).
fnpattern(permit, 60000000, 'permitting', ['agent': 'principle', 'proposition': 'state_of_affairs']).
fnpattern(command, 60000100, 'request', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'message']).
fnpattern(demand, 60000100, 'request', ['agent': 'speaker', 'patient': 'addressee', 'proposition': 'message']).
fnpattern(aim, 62000000, 'purpose', ['experiencer': 'agent', 'theme': 'goal']).
fnpattern(aim, 62000000, 'purpose', ['experiencer': 'agent', 'theme': 'attribute']).
fnpattern(aim, 62000000, 'purpose', ['experiencer': 'agent', 'theme': 'value']).
fnpattern(expect, 62000000, 'expectation', ['experiencer': 'cognizer', 'theme': 'phenomenon']).
fnpattern(expect, 62000000, 'expectation', ['experiencer': 'cognizer', 'theme': 'topic']).
fnpattern(intend, 62000000, 'purpose', ['experiencer': 'agent', 'theme': 'goal']).
fnpattern(intend, 62000000, 'purpose', ['experiencer': 'agent', 'theme': 'attribute']).
fnpattern(intend, 62000000, 'purpose', ['experiencer': 'agent', 'theme': 'value']).
fnpattern(allow, 64000000, 'permitting', ['agent': 'principle', 'theme': 'state_of_affairs']).
fnpattern(permit, 64000000, 'permitting', ['agent': 'principle', 'theme': 'state_of_affairs']).
fnpattern(allow, 65000000, 'permitting', ['agent': 'principle', 'theme': 'state_of_affairs']).
fnpattern(allow, 65000000, 'permitting', ['agent': 'principle', 'location': 'state_of_affairs']).
fnpattern(permit, 65000000, 'grant_permission', ['agent': 'grantor', 'theme': 'grantee', 'location': 'action']).
fnpattern(permit, 65000000, 'grant_permission', ['agent': 'grantor', 'theme': 'grantee', 'location': 'place']).
fnpattern(permit, 65000000, 'permitting', ['agent': 'principle', 'theme': 'state_of_affairs']).
fnpattern(permit, 65000000, 'permitting', ['agent': 'principle', 'location': 'state_of_affairs']).
fnpattern(waste, 66000100, 'frugality', ['agent': 'resource_controller', 'theme': 'resource']).
fnpattern(waste, 66000100, 'frugality', ['agent': 'resource_controller', 'theme': 'behavior']).
fnpattern(ban, 67000000, 'prohibiting', ['agent': 'principle', 'theme': 'state_of_affairs']).
fnpattern(dissuade, 67000000, 'suasion', ['agent': 'speaker', 'theme': 'addressee']).
fnpattern(dissuade, 67000000, 'suasion', ['agent': 'speaker', 'theme': 'content']).
fnpattern(dissuade, 67000000, 'suasion', ['agent': 'speaker', 'theme': 'topic']).
fnpattern(dissuade, 67000000, 'suasion', ['agent': 'text', 'theme': 'addressee']).
fnpattern(dissuade, 67000000, 'suasion', ['agent': 'text', 'theme': 'content']).
fnpattern(dissuade, 67000000, 'suasion', ['agent': 'text', 'theme': 'topic']).
fnpattern(forbid, 67000000, 'deny_permission', ['agent': 'authority', 'theme': 'protagonist']).
fnpattern(forbid, 67000000, 'deny_permission', ['agent': 'authority', 'theme': 'action']).
fnpattern(forbid, 67000000, 'prohibiting', ['agent': 'principle', 'theme': 'state_of_affairs']).
fnpattern(prevent, 67000000, 'preventing', ['agent': 'preventing_cause', 'theme': 'event']).
fnpattern(prevent, 67000000, 'thwarting', ['agent': 'preventing_cause', 'theme': 'protagonist']).
fnpattern(prevent, 67000000, 'thwarting', ['agent': 'preventing_cause', 'theme': 'action']).
fnpattern(prohibit, 67000000, 'deny_permission', ['agent': 'authority', 'theme': 'protagonist']).
fnpattern(prohibit, 67000000, 'deny_permission', ['agent': 'authority', 'theme': 'action']).
fnpattern(prohibit, 67000000, 'preventing', ['agent': 'preventing_cause', 'theme': 'event']).
fnpattern(prohibit, 67000000, 'prohibiting', ['agent': 'principle', 'theme': 'state_of_affairs']).
fnpattern(waste, 68000000, 'frugality', ['agent': 'resource_controller', 'asset': 'resource', 'theme': 'behavior']).
fnpattern(desist, 69000000, 'avoiding', ['agent': 'agent', 'theme': 'undesirable_situation']).
fnpattern(desist, 69000000, 'process_stop', ['agent': 'cause', 'theme': 'process']).
fnpattern(desist, 69000000, 'process_stop', ['agent': 'reason', 'theme': 'process']).
fnpattern(depend, 70000000, 'reliance', ['agent': 'protagonist', 'theme': 'means']).
fnpattern(depend, 70000000, 'reliance', ['agent': 'protagonist', 'theme': 'instrument']).
fnpattern(depend, 70000000, 'reliance', ['agent': 'protagonist', 'theme': 'intermediary']).
fnpattern(depend, 70000000, 'reliance', ['agent': 'protagonist', 'theme': 'benefit']).
fnpattern(depend, 70000000, 'reliance', ['agent': 'protagonist', 'theme': 'purpose']).
fnpattern(rely, 70000000, 'reliance', ['agent': 'protagonist', 'theme': 'means']).
fnpattern(rely, 70000000, 'reliance', ['agent': 'protagonist', 'theme': 'instrument']).
fnpattern(rely, 70000000, 'reliance', ['agent': 'protagonist', 'theme': 'intermediary']).
fnpattern(rely, 70000000, 'reliance', ['agent': 'protagonist', 'theme': 'benefit']).
fnpattern(rely, 70000000, 'reliance', ['agent': 'protagonist', 'theme': 'purpose']).
fnpattern(collude, 71000000, 'collaboration', ['actor': 'partners', 'actor1': 'partner_1', 'actor2': 'partner_2', 'theme': 'undertaking']).
fnpattern(conspire, 71000000, 'collaboration', ['actor': 'partners', 'actor1': 'partner_1', 'actor2': 'partner_2', 'theme': 'undertaking']).
fnpattern(retaliate, 71000000, 'revenge', ['actor': 'avenger', 'actor2': 'offender', 'theme': 'punishment']).
fnpattern(retaliate, 71000000, 'revenge', ['actor': 'avenger', 'actor2': 'injury', 'theme': 'punishment']).
fnpattern(retaliate, 71000000, 'revenge', ['actor': 'avenger', 'actor2': 'injured_party', 'theme': 'punishment']).
fnpattern(retaliate, 71000000, 'revenge', ['actor': 'avenger', 'actor2': 'instrument', 'theme': 'punishment']).
fnpattern(retaliate, 71000000, 'revenge', ['actor1': 'avenger', 'actor2': 'offender', 'theme': 'punishment']).
fnpattern(retaliate, 71000000, 'revenge', ['actor1': 'avenger', 'actor2': 'injury', 'theme': 'punishment']).
fnpattern(retaliate, 71000000, 'revenge', ['actor1': 'avenger', 'actor2': 'injured_party', 'theme': 'punishment']).
fnpattern(retaliate, 71000000, 'revenge', ['actor1': 'avenger', 'actor2': 'instrument', 'theme': 'punishment']).
fnpattern(sin, 71000000, 'misdeed', ['actor': 'wrongdoer', 'actor2': 'injured_party', 'theme': 'misdeed']).
fnpattern(sin, 71000000, 'misdeed', ['actor1': 'wrongdoer', 'actor2': 'injured_party', 'theme': 'misdeed']).
fnpattern(team_up, 71000000, 'collaboration', ['actor': 'partners', 'actor1': 'partner_1', 'actor2': 'partner_2', 'theme': 'undertaking']).
fnpattern(succor, 72000000, 'assistance', ['agent': 'helper', 'beneficiary': 'benefitted_party', 'theme': 'goal']).
fnpattern(succor, 72000000, 'assistance', ['agent': 'helper', 'beneficiary': 'benefitted_party', 'theme': 'focal_entity']).
fnpattern(abet, 72000100, 'assistance', ['agent': 'helper', 'beneficiary': 'benefitted_party', 'theme': 'goal']).
fnpattern(abet, 72000100, 'assistance', ['agent': 'helper', 'beneficiary': 'benefitted_party', 'theme': 'focal_entity']).
fnpattern(assist, 72000100, 'assistance', ['agent': 'helper', 'beneficiary': 'benefitted_party', 'theme': 'goal']).
fnpattern(assist, 72000100, 'assistance', ['agent': 'helper', 'beneficiary': 'benefitted_party', 'theme': 'focal_entity']).
fnpattern(help, 72000100, 'assistance', ['agent': 'helper', 'beneficiary': 'benefitted_party', 'theme': 'goal']).
fnpattern(help, 72000100, 'assistance', ['agent': 'helper', 'beneficiary': 'benefitted_party', 'theme': 'focal_entity']).
fnpattern(participate, 73000200, 'participation', ['actor': 'participants', 'actor1': 'participant_1', 'actor2': 'participant_2', 'theme': 'event']).
fnpattern(participate, 73000200, 'participation', ['actor': 'participants', 'actor1': 'participant_1', 'actor2': 'participant_2', 'theme': 'institution']).
fnpattern(confine, 76000000, 'cause_confinement', ['cause': 'agent', 'patient': 'theme', 'proposition': 'goal']).
fnpattern(confine, 76000000, 'cause_confinement', ['cause': 'cause', 'patient': 'theme', 'proposition': 'goal']).
fnpattern(constrain, 76000000, 'hindering', ['cause': 'agent', 'patient': 'item', 'proposition': 'attribute']).
fnpattern(constrain, 76000000, 'hindering', ['cause': 'agent', 'patient': 'item', 'proposition': 'path']).
fnpattern(constrain, 76000000, 'hindering', ['cause': 'agent', 'patient': 'item', 'proposition': 'value_1']).
fnpattern(constrain, 76000000, 'hindering', ['cause': 'agent', 'patient': 'item', 'proposition': 'value_2']).
fnpattern(constrain, 76000000, 'hindering', ['cause': 'agent', 'patient': 'item', 'proposition': 'means']).
fnpattern(constrain, 76000000, 'hindering', ['cause': 'cause', 'patient': 'item', 'proposition': 'attribute']).
fnpattern(constrain, 76000000, 'hindering', ['cause': 'cause', 'patient': 'item', 'proposition': 'path']).
fnpattern(constrain, 76000000, 'hindering', ['cause': 'cause', 'patient': 'item', 'proposition': 'value_1']).
fnpattern(constrain, 76000000, 'hindering', ['cause': 'cause', 'patient': 'item', 'proposition': 'value_2']).
fnpattern(constrain, 76000000, 'hindering', ['cause': 'cause', 'patient': 'item', 'proposition': 'means']).
fnpattern(restrict, 76000000, 'cause_confinement', ['cause': 'agent', 'patient': 'theme', 'proposition': 'goal']).
fnpattern(restrict, 76000000, 'cause_confinement', ['cause': 'cause', 'patient': 'theme', 'proposition': 'goal']).
fnpattern(understand, 77000000, 'awareness', ['agent': 'cognizer', 'proposition': 'content']).
fnpattern(understand, 77000000, 'awareness', ['agent': 'cognizer', 'proposition': 'topic']).
fnpattern(imply, 78000000, 'evidence', ['cause': 'support', 'recipient': 'cognizer', 'topic': 'proposition']).
fnpattern(predict, 78000000, 'predicting', ['cause': 'evidence', 'topic': 'eventuality']).
fnpattern(predict, 78000000, 'predicting', ['cause': 'speaker', 'topic': 'eventuality']).
fnpattern(indicate, 78000100, 'communication', ['cause': 'communicator', 'recipient': 'addressee', 'topic': 'message']).
fnpattern(indicate, 78000100, 'communication', ['cause': 'communicator', 'recipient': 'addressee', 'topic': 'topic']).
fnpattern(indicate, 78000100, 'evidence', ['cause': 'support', 'recipient': 'cognizer', 'topic': 'proposition']).
fnpattern(indicate, 78000100, 'sign', ['cause': 'indicator', 'topic': 'indicated']).
fnpattern(confirm, 78000110, 'evidence', ['cause': 'support', 'recipient': 'cognizer', 'topic': 'proposition']).
fnpattern(confirm, 78000110, 'statement', ['cause': 'speaker', 'recipient': 'addressee', 'topic': 'message']).
fnpattern(confirm, 78000110, 'statement', ['cause': 'speaker', 'recipient': 'addressee', 'topic': 'topic']).
fnpattern(confirm, 78000110, 'verification', ['cause': 'inspector', 'topic': 'unconfirmed_content']).
fnpattern(confirm, 78000110, 'verification', ['cause': 'medium', 'topic': 'unconfirmed_content']).
fnpattern(prove, 78000110, 'evidence', ['cause': 'support', 'recipient': 'cognizer', 'topic': 'proposition']).
fnpattern(prove, 78000110, 'reasoning', ['cause': 'support', 'recipient': 'addressee', 'topic': 'content']).
fnpattern(prove, 78000110, 'reasoning', ['cause': 'arguer', 'recipient': 'addressee', 'topic': 'content']).
fnpattern(acquit, 80000100, 'verdict', ['cause': 'judge', 'source': 'defendant', 'theme': 'charges']).
fnpattern(acquit, 80000100, 'verdict', ['cause': 'reason', 'source': 'defendant', 'theme': 'charges']).
fnpattern(accuse, 81000000, 'judgement', ['agent': 'cognizer', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(accuse, 81000000, 'judgement_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'reason']).
fnpattern(accuse, 81000000, 'judgement_communication', ['agent': 'communicator', 'theme': 'evaluee', 'predicate': 'topic']).
fnpattern(accuse, 81000000, 'notification_of_charges', ['agent': 'arraign_authority', 'theme': 'accused', 'predicate': 'charges']).
fnpattern(retire, 82000300, 'quitting', ['agent': 'employee', 'source': 'position']).
fnpattern(retire, 82000300, 'quitting', ['agent': 'employee', 'source': 'field']).
fnpattern(retreat, 82000300, 'departing', ['agent': 'theme', 'source': 'source']).
fnpattern(retreat, 82000300, 'quitting_a_place', ['agent': 'self_mover', 'source': 'source']).
fnpattern(learn, 84000110, 'becoming_aware', ['agent': 'cognizer', 'theme': 'phenomenon', 'source': 'means']).
fnpattern(learn, 84000110, 'becoming_aware', ['agent': 'cognizer', 'theme': 'phenomenon', 'source': 'instrument']).
fnpattern(learn, 84000110, 'becoming_aware', ['agent': 'cognizer', 'theme': 'topic', 'source': 'means']).
fnpattern(learn, 84000110, 'becoming_aware', ['agent': 'cognizer', 'theme': 'topic', 'source': 'instrument']).
fnpattern(learn, 84000110, 'coming_to_believe', ['agent': 'cognizer', 'theme': 'content', 'source': 'evidence']).
fnpattern(learn, 84000110, 'coming_to_believe', ['agent': 'cognizer', 'theme': 'content', 'source': 'means']).
fnpattern(learn, 84000110, 'coming_to_believe', ['agent': 'cognizer', 'theme': 'topic', 'source': 'evidence']).
fnpattern(learn, 84000110, 'coming_to_believe', ['agent': 'cognizer', 'theme': 'topic', 'source': 'means']).
fnpattern(learn, 84000110, 'education_teaching', ['agent': 'student', 'theme': 'subject', 'source': 'teacher']).
fnpattern(learn, 84000110, 'education_teaching', ['agent': 'student', 'theme': 'subject', 'source': 'institution']).
fnpattern(learn, 84000110, 'education_teaching', ['agent': 'student', 'theme': 'subject', 'source': 'course']).
fnpattern(learn, 84000110, 'education_teaching', ['agent': 'student', 'theme': 'subject', 'source': 'material']).
fnpattern(learn, 84000110, 'education_teaching', ['agent': 'student', 'theme': 'skill', 'source': 'teacher']).
fnpattern(learn, 84000110, 'education_teaching', ['agent': 'student', 'theme': 'skill', 'source': 'institution']).
fnpattern(learn, 84000110, 'education_teaching', ['agent': 'student', 'theme': 'skill', 'source': 'course']).
fnpattern(learn, 84000110, 'education_teaching', ['agent': 'student', 'theme': 'skill', 'source': 'material']).
fnpattern(learn, 84000110, 'education_teaching', ['agent': 'student', 'theme': 'role', 'source': 'teacher']).
fnpattern(learn, 84000110, 'education_teaching', ['agent': 'student', 'theme': 'role', 'source': 'institution']).
fnpattern(learn, 84000110, 'education_teaching', ['agent': 'student', 'theme': 'role', 'source': 'course']).
fnpattern(learn, 84000110, 'education_teaching', ['agent': 'student', 'theme': 'role', 'source': 'material']).
fnpattern(learn, 84000110, 'education_teaching', ['agent': 'student', 'theme': 'precept', 'source': 'teacher']).
fnpattern(learn, 84000110, 'education_teaching', ['agent': 'student', 'theme': 'precept', 'source': 'institution']).
fnpattern(learn, 84000110, 'education_teaching', ['agent': 'student', 'theme': 'precept', 'source': 'course']).
fnpattern(learn, 84000110, 'education_teaching', ['agent': 'student', 'theme': 'precept', 'source': 'material']).
fnpattern(learn, 84000110, 'education_teaching', ['agent': 'student', 'theme': 'fact', 'source': 'teacher']).
fnpattern(learn, 84000110, 'education_teaching', ['agent': 'student', 'theme': 'fact', 'source': 'institution']).
fnpattern(learn, 84000110, 'education_teaching', ['agent': 'student', 'theme': 'fact', 'source': 'course']).
fnpattern(learn, 84000110, 'education_teaching', ['agent': 'student', 'theme': 'fact', 'source': 'material']).
fnpattern(learn, 84000110, 'memorization', ['agent': 'cognizer', 'theme': 'pattern']).
fnpattern(defend, 85000000, 'defend', ['agent': 'defender', 'theme': 'victim', 'proposition': 'assailant']).
fnpattern(defend, 85000000, 'defend', ['agent': 'defender', 'proposition': 'victim']).
fnpattern(defend, 85000000, 'justifying', ['agent': 'agent', 'theme': 'act', 'proposition': 'state_of_affairs']).
fnpattern(defend, 85000000, 'justifying', ['agent': 'agent', 'proposition': 'act']).
fnpattern(wonder, 88000100, 'cogitation', ['eperiencer': 'cognizer', 'cause': 'topic']).
fnpattern(cohere, 89000000, 'compatibility', ['actor': 'item_1', 'proposition': 'item_2']).
fnpattern(cohere, 89000000, 'compatibility', ['actor': 'item_1', 'proposition': 'parameter']).
fnpattern(cohere, 89000000, 'compatibility', ['actor': 'items', 'proposition': 'item_2']).
fnpattern(cohere, 89000000, 'compatibility', ['actor': 'items', 'proposition': 'parameter']).
fnpattern(jibe, 89000000, 'compatibility', ['actor': 'item_1', 'proposition': 'item_2']).
fnpattern(jibe, 89000000, 'compatibility', ['actor': 'item_1', 'proposition': 'parameter']).
fnpattern(jibe, 89000000, 'compatibility', ['actor': 'items', 'proposition': 'item_2']).
fnpattern(jibe, 89000000, 'compatibility', ['actor': 'items', 'proposition': 'parameter']).
fnpattern(square, 89000000, 'compatibility', ['actor': 'item_1', 'proposition': 'item_2']).
fnpattern(square, 89000000, 'compatibility', ['actor': 'item_1', 'proposition': 'parameter']).
fnpattern(square, 89000000, 'compatibility', ['actor': 'items', 'proposition': 'item_2']).
fnpattern(square, 89000000, 'compatibility', ['actor': 'items', 'proposition': 'parameter']).
fnpattern(outstrip, 90000000, 'surpassing', ['theme1': 'profiled_item', 'theme2': 'standard_item', 'attribute': 'attribute']).
fnpattern(outstrip, 90000000, 'surpassing', ['theme1': 'profiled_item', 'theme2': 'standard_item', 'attribute': 'extent']).
fnpattern(outstrip, 90000000, 'surpassing', ['theme1': 'profiled_item', 'theme2': 'standard_item', 'attribute': 'standard_attribute']).
fnpattern(outstrip, 90000000, 'surpassing', ['theme1': 'profiled_item', 'theme2': 'standard_item', 'attribute': 'profiled_attribute']).
fnpattern(surpass, 90000000, 'surpassing', ['theme1': 'profiled_item', 'theme2': 'standard_item', 'attribute': 'attribute']).
fnpattern(surpass, 90000000, 'surpassing', ['theme1': 'profiled_item', 'theme2': 'standard_item', 'attribute': 'extent']).
fnpattern(surpass, 90000000, 'surpassing', ['theme1': 'profiled_item', 'theme2': 'standard_item', 'attribute': 'standard_attribute']).
fnpattern(surpass, 90000000, 'surpassing', ['theme1': 'profiled_item', 'theme2': 'standard_item', 'attribute': 'profiled_attribute']).
fnpattern(commit, 92000100, 'institutionalization', ['agent': 'authority', 'theme': 'patient', 'destination': 'facility']).
fnpattern(confine, 92000100, 'cause_confinement', ['agent': 'agent', 'theme': 'theme', 'destination': 'goal']).
fnpattern(confine, 92000100, 'cause_confinement', ['agent': 'cause', 'theme': 'theme', 'destination': 'goal']).
fnpattern(venture, 94000000, 'daring', ['agent': 'agent', 'theme': 'action']).
fnpattern(venture, 94000000, 'statement', ['agent': 'speaker', 'theme': 'message']).
fnpattern(venture, 94000000, 'statement', ['agent': 'speaker', 'theme': 'medium']).
fnpattern(venture, 94000000, 'statement', ['agent': 'speaker', 'theme': 'topic']).
fnpattern(chance, 94000100, 'daring', ['agent': 'agent', 'theme': 'action']).
fnpattern(hazard, 94000100, 'daring', ['agent': 'agent', 'theme': 'action']).
fnpattern(hazard, 94000100, 'statement', ['agent': 'speaker', 'theme': 'message']).
fnpattern(hazard, 94000100, 'statement', ['agent': 'speaker', 'theme': 'medium']).
fnpattern(hazard, 94000100, 'statement', ['agent': 'speaker', 'theme': 'topic']).
fnpattern(risk, 94000100, 'daring', ['agent': 'agent', 'theme': 'action']).
