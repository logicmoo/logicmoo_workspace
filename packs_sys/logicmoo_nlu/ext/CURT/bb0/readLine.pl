/*************************************************************************

         name: readLine.pl
      version: March 31, 1998
  description: Converting input line to list of atoms, suitable for
               DCG input.
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(readLine,[readLine/1]).

/*========================================================================

   Read Predicates
   ---------------

readLine(-WordList)
   Outputs a prompt, reads a sequence of characters from the standard
   input and converts this to WordList, a list of strings. Punctuation 
   is stripped.

readWords(-WordList)
   Reads in a sequence of characters, until a return is registered, 
   and converts this to WordList a list of strings. 

readWord(+Char,-Chars,?State)
   Read a word coded as Chars (a list of ascii values), starting 
   with with ascii value Char, and determine the State of input
   (`ended' = end of line, `notended' = not end of line).
   Blanks and full stops split words, a return ends a line.

checkWords(+OldWordList,-NewWordList)
   Check if all words are unquoted atoms, if not convert them 
   into atoms.

convertWord(+OldWord,-NewWord)
   OldWord and NewWord are words represented as lists of ascii values.
   Converts upper into lower case characters, and eliminates
   non-alphabetic characters.

========================================================================*/

readLine(WordList):-
   nl, write('> '),
   readWords(Words),
   checkWords(Words,WordList).

readWords([Word|Rest]):-
   get0(Char),
   readWord(Char,Chars,State),
   name(Word,Chars),
   readRest(Rest,State).

readRest([],ended).
readRest(Rest,notended):-
   readWords(Rest).

readWord(32,[],notended):-!.     %%% blank
readWord(46,[],notended):-!.     %%% full stop
readWord(10,[],ended):-!.        %%% return
readWord(Code,[Code|Rest],State):-
   get0(Char),
   readWord(Char,Rest,State).

checkWords([],[]):- !.
checkWords([''|Rest1],Rest2):-
   checkWords(Rest1,Rest2).
checkWords([Atom|Rest1],[Atom2|Rest2]):-
   name(Atom,Word1),
   convertWord(Word1,Word2),
   name(Atom2,Word2),
   checkWords(Rest1,Rest2).

convertWord([],[]):- !.
convertWord([Capital|Rest1],[Small|Rest2]):-
   Capital > 64, Capital < 91, !,
   Small is Capital + 32,
   convertWord(Rest1,Rest2).
convertWord([Weird|Rest1],Rest2):-
   (Weird < 97; Weird > 122), !,
   convertWord(Rest1,Rest2).
convertWord([Char|Rest1],[Char|Rest2]):-
   convertWord(Rest1,Rest2).
