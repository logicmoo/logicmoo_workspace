/****************************************************
% File Name: pronto_morph_engine.pl
% Author: Jason Schlachter (ai@uga.edu)(www.arches.uga.edu/~ai)

% Released: May 8th, 2003
% Artificial Intelligence Center (www.ai.uga.edu)
% ***see pronto_morph.pdf for documentation

% Morphological Analyzer to be used with
% ProNTo (Prolog Natural Language Toolkit),
% created at the Artificial Intelligence Center
% of The University of Georgia

% Modified from: POEM.PL - Part Of English Morphology
% M. Covington
% 2003 February 12
****************************************************/

:- module(pronto_morph_engine,[morph_atoms/2]).


% *******************************************************************
% Be sure to install these files in the same directory as morph_engine.pl
% *******************************************************************
%:- include( 'pronto_morph_tokenizer.pl' ).
:- include( 'pronto_morph_spelling_rules.pl').
:- include( 'pronto_morph_irreg_adj.pl' ).
:- include( 'pronto_morph_irreg_adv.pl' ).
:- include( 'pronto_morph_irreg_noun.pl' ).
:- include( 'pronto_morph_irreg_verb.pl' ).
:- multifile( irregular_form/3 ).

% How inflectional suffixes are named:
%
% - All are marked with the prefix operator -/1.
%   (In Prolog, -x is equivalent to -(x).)
%   This makes it easy to distinguish suffixes
%   from words.
%
% - The morphological analyzer makes the distinctions
%   that it can make without a lexicon.
%   We use the ambiguous symbols -s and -ed for morphemes
%   that cannot be unambiguously identified.
%
% -s     Suffix -s, could be either noun plural or verb 3sg
% -pl    Suffix definitely denoting a noun plural (e.g., oxen = ox -pl)
% -sg3   Suffix definitely denoting a verb 3rd person singular (e.g., has = have -sg3)
%
% -ing   Verb ending, never ambiguous
%
% -ed    Suffix -ed, denoting the past or -en form of a regular verb
% -past  Suffix definitely denoting a verb past tense form (e.g., ran = run -past)
% -en    Suffix definitely denoting an -en form of a verb  (e.g., eaten = eat -en)
%
% -er    Suffix denoting the comparative form of the adjective or adverb
% -est   Suffix denoting the superlative form of the adjective or adverb




% morph_tokens(+Tokens,-List)
%  Converts the output of pronto_morph_tokenizer.pl (also et.pl) to a list of morphemes.
%   (i.e. w([t,e,s,t,i,n,g] --> [[test,-ing]]
%  OR
%   (i.e. [w([t,e,s,t,i,n,g])] -->  [[test,-ing]]
%  OR
%   (i.e. [w([t,e,s,t,i,n,g]),w([i,t])] -->  [[test,-ing],[it]]
 
morph_tokens([w(Chars)|Tokens],Morphs) :-    % handles list of token(s) as input
   !,
   morph(Chars,Rest,Morphs),    
   morph_tokens(Tokens,Rest).

morph_tokens([_|Tokens],Morphs) :-           % handles list of token(s) as input
   % numeric or special-character token
   morph_tokens(Tokens,Morphs).

morph_tokens(Token,Morphs) :-                % handles single token as input
   \+ is_list(Token),
   morph_tokens([Token],Morphs).

morph_tokens([],[]).


% morph_tokens_bag(+Tokens,-List)
%  Same as morph_tokens/2 except that it returns every alternative
%  analysis in a list of lists
%   (i.e. )
%   [[[[testing]], [[teste, -ing]], [[test, -ing]]], [[[more]]]] 

morph_tokens_bag(Token,List) :-
   \+ is_list(Token),
   findall(Alternative,
	   morph_tokens(Token,Alternative),
	   List).
morph_tokens_bag([First|RestTokens],[List|RestList]) :-
   findall(Alternative ,
	   morph_tokens(First,Alternative),
	   List),
   morph_tokens_bag(RestTokens,RestList).
morph_tokens_bag([],[]).


% morph_atoms(+AtomWord,-List)
%  Converts an atom to a list of morphemes
%   (i.e. testing --> [[test,-ing]]
%  OR
%   (i.e. [testing] --> [[test,-ing]]
%  OR
%  Converts a list of atoms to a list of morpheme lists
%   (i.e. [testing,one,two,three] --> [[test,-ing],[one],[two],[three]]

morph_atoms([AtomWord|Rest],List) :-	   % handles list of atom(s) as input
   atom_chars(AtomWord,RawList),
   morph(RawList,RestResult,List),
   morph_atoms(Rest,RestResult).

morph_atoms(SingleAtom,List) :-            % handles single atoms as input
   \+ is_list(SingleAtom),
   morph_atoms([SingleAtom],List).

morph_atoms([],[]).


% morph_atoms_bag(+Atoms,-List)
%  Same as morph_atoms/2 except that it returns evey alternative
%  analysis in a list of lists
%  i.e.)
%  [[[[testing]], [[teste, -ing]], [[test, -ing]]], [[[more]]]]

morph_atoms_bag(Token,List) :-
   \+ is_list(Token),
   findall(Alternative,
	   morph_atoms(Token,Alternative),
	   List).
morph_atoms_bag([First|RestTokens],[List|RestList]) :-
    findall(Alternative ,
	    morph_atoms(First,Alternative),
	    List),
    morph_atoms_bag(RestTokens,RestList).
morph_atoms_bag([],[]).



% morph(+Characters,-List)
%  Converts a list of characters to a list of morphemes.
%   (i.e. [r,u,n,n,i,n,g] --> [run,-ing]
%  OR
%  Converts a list of character lists to a list of morpheme lists
%   (i.e. [[r,u,n,n,i,n,g],[f,a,s,t,e,r]] --> [[run,-ing],[fast,-er]]

morph_chars([Chars|Rest],List) :-          % handles list of character lists as input
   is_list(Chars),
   morph(Chars,RestResult,List),
   morph_chars(Rest,RestResult).

morph_chars([Chars|Rest],List) :-          % handles a single list of characters as input
   \+ is_list(Chars),
   morph_chars([[Chars|Rest]],List).

morph_chars([],[]).


% morph_chars_bag(+Tokens,-List) :-
%  Same as morph_chars/2 except that it returns every alternative
%  analysis as a list of lists
% i.e.
% [[[[testing]], [[teste, -ing]], [[test, -ing]]], [[[more]]]] 

morph_chars_bag([C|CharList],List) :-
    \+ is_list(C),
    findall(Alternatives,
	    morph_chars(CharList,Alternatives),
	    List).
morph_chars_bag([First|RestTokens],[List|RestList]) :-
    findall(Alternatives,
	    morph_chars(First,Alternatives),
	    List),
    morph_chars_bag(RestTokens,RestList).
morph_chars_bag([],[]).

% morph(+Characters,-Tail,-OpenList)
%  Like morph/2, but creates an open list ending with Tail.
%  This is where the real work is done.

morph(Chars,Tail,[[Root,Suffix]|Tail]) :-
   atom_chars(Atom,Chars),                   % quicker to look up an atom than a list
   irregular_form(Atom,Tail,[Root,Suffix|Tail]),         % check to see if word is irregular
   !.                                        

morph(Chars,Tail,[[Word]|Tail]) :-
   atom_chars(Word,Chars).                   % always an option that word is root

morph(Chars,Tail,[[RootWord,Suffix]|Tail]) :-  % tries to break up word into root and suffix
   find_suffix(Chars,Root,Suffix),
   atom_chars(RootWord,Root).



% find_suffix(+Characters,-Root,-Suffix)
%  Applies split_suffix to a word at all positions.

find_suffix(Chars,Root,Suffix) :-
   split_suffix(Chars,Root,Suffix).

find_suffix([C|Chars],[C|Root],Suffix) :-
   find_suffix(Chars,Root,Suffix).

   


% suffix(?Chars,?Morpheme)
%  If Chars is a suffix, Morpheme is the description of it.
%  Note that the suffix -s is also hard-coded into split_suffix
%  in various places.
%
suffix([s],-s).         
suffix([e,d],-ed).
suffix([i,n,g],-ing).
suffix([e,r],-er).
suffix([e,s,t],-est).


% vowel(?Char)
%  Char is a vowel.
%
vowel(a).
vowel(e).
vowel(i).
vowel(o).
vowel(u).
vowel(y).












