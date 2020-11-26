This package contains the TALK unimodal grammar library and various programs needed to use the grammars in a dialogue system.

Contents:

GF_GoDiS: The GF grammars that make up the TALK unimodal grammar library, described in TALK deliverable D1.1

trindikit4 - A pre-release of trindikit4.

godis - core parts of the GoDiS dialogue system, i.e. the GoDiS libraries. Contains code shared by GoDiS applications

delux - the GoDiS deLux application. Uses the embedded GF interpreter for interpretation and generation. 

djgodis - the DJ GoDiS application. Uses the embedded GF interpreter for interpretation and generation. 

jars - various jar files, including gfc2java.jar which contains the embedded GF interpreter, 

------------------------------------------------------------------------------
 Requirements
------------------------------------------------------------------------------

To use the combined GF-Trindikit package the following software is needed:

- SICStus Prolog version 3.9 or later. The sicstus binary needs to be in your PATH. 
- Java development kit version 1.5 or later. java and javac executables need to be in your PATH.
- OAA 2.3.0 or later. Environment variable OAA_HOME must  be set to point at the OAA installation.
- Optional: Nuance ASR and TTS for running with speech
- Grammatical Framework version 2.2 or later. The gf binary needs to be in your PATH. GF can be downloaded from http:/www.cs.chalmers.se/~aarne/GF


 ------------------------------------------------------------------------------
Usage
------------------------------------------------------------------------------

Given that the requirements are met, the systems can be run by using the OAA Startit agent.


For windows:

- Generate the gfcm and mcfg grammars by running export_grammars_to_godis.bat

- If using Nuance ASR
  - compile the generated GSL grammars by running compile_nuance_grammars.bat in the djgodis and delux directories
  - start a nuance license manager

- run run.bat in the djgodis or delux directory

- from the Projects menu, select text-mode or speech-mode

- click on the blue "Start" button

For Unix/Linux:

- Generate the gfcm and mcfg grammars by running export_grammars_to_godis.sh

- run run.sh in the djgodis or delux directory

- from the Projects menu, select text-mode (should be pre-selected)

- click on the blue "Start" button

--- 

The grammars can also be tested within GF, see Appendix A.3 in TALK deliverable D1.1

