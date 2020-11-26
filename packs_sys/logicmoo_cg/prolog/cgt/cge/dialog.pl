% 90/11/30 mw: corrected some bugs and simplified the code a little bit

% -----------------------------------------------------------------------------
%
% confirm(PromptText): puts up a modal dialog with a label with the text
% specified as the argument and two command buttons: one saying 'OK' and
% the other saying 'Cancel'. Succeeds if the user clicks 'OK' and fails
% if the user clicks 'Cancel'.
%


acknowledge(Msg):- wdmsg(Msg).
acknowledge(Text) :-
        process_confirm_dialog(ack_dialog, Text, _).

confirm(Text) :-
        process_confirm_dialog(confirm_dialog, Text, _).

confirm(Text, Choice) :-
        process_confirm_dialog(choice_dialog, Text, Choice).

ask(Question, Answer) :-
        ask(Question, Answer, '').

ask(Question, Answer, Default) :-
        shell widget ask_dialog(Dialog, Question, Default),
        repeat, next_event(Dialog-Text),
        Text wproc [get_last_pos(LP), get(0, LP, Answer)],
        Dialog wproc destroy, !.

% -----------------------------------------------------------------------------

process_confirm_dialog(Kind, Message, Action) :-
        Widget =.. [Kind, Dialog, Message],
        shell widget Widget,
	repeat, next_event(Dialog-Action),
	confirm_action(Action, Goal),
	Dialog wproc destroy, !,
	Goal.

        
% -----------------------------------------------------------------------------

confirm_action(ok,     true).
confirm_action(yes,    true).
confirm_action(no,     true).
confirm_action(cancel, fail).


% -----------------------------------------------------------------------------

display_confirm_text(WID, Text) :-
	WID wproc stream(OS),
	current_output(COS), set_output(OS),
	write_confirm_text(Text),
	set_output(COS), close(OS).


%------------------------------------------------------------------------------

write_confirm_text([H]) :-
        write_confirm_text(H).
write_confirm_text([H|T]) :- 
        write_confirm_text(H),
	write_confirm_text(T).

write_confirm_text(A) :- 
        atomic(A), write(A).

write_confirm_text(G) :- call(G).
write_confirm_text(_).


% -----------------------------------------------------------------------------
:- use_module(library(cgt/cge/swi_apeal)).

shell widget ack_dialog(Dialog, Text) :-
  ack_dialog: Dialog= transientShell / [
    title('Modal Dialog'),
    geometry('+400+400'),
    allowShellResize(false)
  ] - [
    box / [
      hSpace(2), vSpace(2),
      background(white)
    ] - [
       cuTbl / [
	interWidth(4), interHeight(4),
	internalWidth(2), internalHeight(2),
	formatString([ [c, <, <], [@(c), c, @(c)] ]),
	borderWidth(2)
      ] - [
	confirm_text: Prompt= asciiText / [
	  textOptions([wordBreak, resizeHeight]),
	  width(200), height(30),
	  editType(edit), sensitive(false),
	  borderWidth(0), displayCaret(false)
	] + [
	  display_confirm_text(Prompt, Text)
	],

	space(1, 1),

	confirm_ok: cuCommand / [
	  label('OK'),
	  callback(t(Dialog-ok))
	],

	space(1,1)
      ]
    ]
  ].

shell widget confirm_dialog(Dialog, Text) :-
  confirm_dialog: Dialog= transientShell / [
    title('Modal Dialog'),
    geometry('+400+400')
  ] - [
    box / [
      hSpace(2), vSpace(2),
      background(white)
    ] - [
      cuTbl / [
	interWidth(4), interHeight(4),
	internalWidth(2), internalHeight(2),
	formatString([ [c, <, <], [c, @(c), c] ]),
	borderWidth(2)
      ] - [
	confirm_text: Prompt= asciiText / [
	  textOptions([wordBreak, resizeHeight]),
	  width(200), height(30),
	  editType(edit), sensitive(false),
	  borderWidth(0), displayCaret(false)
	] + [
	  display_confirm_text(Prompt, Text)
	],

	confirm_ok: cuCommand / [
	  label('OK'),
	  callback(t(Dialog-ok))
	],

	space(1, 1),

	confirm_cancel: cuCommand / [
	  label('Cancel'),
	  callback(t(Dialog-cancel))
	]
      ]
    ]
  ].

shell widget choice_dialog(Dialog, Text) :-
  choice_dialog: Dialog= transientShell / [
    title('Modal Dialog'),
    geometry('+400+400')
  ] - [
    box / [
      hSpace(2), vSpace(2),
      background(white)
    ] - [
      cuTbl / [
	interWidth(4), interHeight(4),
	internalWidth(4), internalHeight(4),
	formatString([ [c, <, <], [c, @(c), c, @(c), c] ]),
	borderWidth(2)
      ] - [
	confirm_text: Prompt= asciiText / [
	  textOptions([wordBreak, resizeHeight]),
	  width(200), height(30),
	  %font(courier-[size(pixel)=12, bold, slant=r]),
	  editType(edit), sensitive(false),
	  borderWidth(0), displayCaret(false)
	] + [
	  display_confirm_text(Prompt, Text)
	],

	confirm_yes: cuCommand / [
	  label('Yes'),
	  callback(t(Dialog-yes))
	],
	
	space(1, 1),

	confirm_no: cuCommand / [
	  label('No'),
	  callback(t(Dialog-no))
	],

	space(1, 1),

	confirm_cancel: cuCommand / [
	  label('Cancel'),
	  callback(t(Dialog-cancel))
	]
      ]
    ]
  ].

shell widget ask_dialog(Dialog, Question, DefaultAnswer) :-
  ask_dialog: Dialog= transientShell / [
    title('Modal Dialog'),
    geometry('+400+400')
  ] - [
    box / [
      hSpace(2), vSpace(2),
      background(white)
    ] - [
      unmanaged cuTbl / [
	interWidth(4), interHeight(4),
	internalWidth(4), internalHeight(4),
	formatString([ [l], [c, <], [c, @(c)] ]),
	borderWidth(2)
      ] - [
	confirm_text: Q= asciiText / [
	  textOptions([wordBreak, resizeHeight]),
	  editType(edit), sensitive(false),
	  borderWidth(0), displayCaret(false)
	] + [
	  display_confirm_text(Q, Question)
	],

	confirm_input: A= asciiText / [
	  textOptions([wordBreak/*, resizeHeight*/]),
	  editType(edit), borderWidth(1), scrollVertical(always),
	  height(50), width(150)
	] + [
	  display_confirm_text(A, DefaultAnswer),
          xt_parse(meta/key-'Return' : term(t(Dialog-A)), Shortcuts),
          override_translations(Shortcuts)
	],

	confirm_ok: cuCommand / [
	  label('OK'),
	  callback(t(Dialog-A))
	],

	space(1, 1)
      ] + [
	manage
      ]
    ] + [
      focus(A)
    ]
  ].
