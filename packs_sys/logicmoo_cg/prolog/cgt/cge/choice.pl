% $Id: util.pl,v 1.7 90/04/23 19:31:32 spa Exp $

:- use_module(library(cgt/cge/swi_apeal)).
:- set_prolog_flag(swi_apeal,true).

% =============================================================================
%
% choice(Prompt, ListOfItems, ChosenItem)
% choice(Prompt, ListOfItems, DefaultItem, ChosenItem)
%
% $Log:	util.pl,v $
%

choice(Name, [],     Choice) :- choice(Name, [], 0, Choice).
choice(Name, [I|Is], Choice) :- choice(Name, [I|Is], I, Choice).

choice(Name, Items, Default, Choice) :-
	( nth0(DefaultIndex, Items, Default) -> true ; DefaultIndex=0 ),
	get_choice_translations(Tr),
	shell widget choice_dialog(S, Items, DefaultIndex, Name, Tr, List),
	next_event(E),
	choice_action(E, List, Choice, Items, Goal),
	S wproc destroy, !,
	Goal.

choice_action(cancel, _,    _,      _Items, fail) :- !.
choice_action(ok,     List, Choice, Items, Choice=Item) :-
	List wproc show_current(_:I),
	nth0(I, Items, Item).


shell widget choice_dialog(S, Items, Default, Label, Tr, List) :-
  choice: S=
  transientShell / [
    title('Modal Dialog'),
    geometry('+350+320')
  ] - [
    choice: box / [
      vSpace(2), hSpace(2)
    ] - [
      form: Form= form / [
        borderWidth(2)
      ] - [
	choose: LBL= label / [
	  bottom(top),
	  label(Label)
	],

	box: BBox= cuTbl / [
	  right(left), bottom(top), fromVert(LBL),
	  formatString([ [c] ]), borderWidth(0)
	] - [
	  ok: cuCommand / [ callback(t(ok)), label('OK') ],
	  space(1, 8),
	  cancel: cuCommand / [ callback(t(cancel)), label('Cancel') ]
	],

	vp: Port= viewport / [
	  allowVert(true), forceBars(true),
	  resizable(false),
	  height(100),
	  fromVert(LBL), fromHoriz(BBox)
	] - [
	  List= list / [
	    list(Items),
	    defaultColumns(1), forceColumns(true),
	    longest(200), borderWidth(0),
	    translations(Tr)
	  ] + [
	    List wproc highlight(Default)
	  ]
	]
      ]
    ]
  ].

get_choice_translations(Tr) :- recorded('$_choice_translations', Tr, _), !.
get_choice_translations(Tr) :-
	xt_parse( [ btn(down):		'Set',
		    btn(1)/btn(motion):	'Set',
		    btn(2)/btn(motion):	'Set',
		    btn(3)/btn(motion):	'Set',
		    btn(up)*2:		term(t(ok)) ], Tr),
	recorda('$_choice_translations', Tr, _).


:- set_prolog_flag(swi_apeal,false).

