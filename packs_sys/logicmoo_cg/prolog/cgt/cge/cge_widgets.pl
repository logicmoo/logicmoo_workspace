/* COPYRIGHT ************************************************************

Conceptual Graph Editor (CGE) - an X-Windows graphical interface to CGT
Copyright (C) 1990 Miguel Alexandre Wermelinger

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

************************************************************************/

/* AUTHOR(S) ************************************************************

Michel Wermelinger
Dept. de Informatica, Univ. Nova de Lisboa, Quinta da Torre
P - 2825 Monte da Caparica, PORTUGAL
Phone: (+351) (1) 295 44 64 ext. 1360  Internet: mw@fct.unl.pt

************************************************************************/

/* GENERALITIES *********************************************************
 
File Name       : WIDGETS.PL
Creation Date   : 90/11/20 
Author(s)       : Michel Wermelinger (mw)
Description     : Widgets for the Conceptual Graph Editor
 
************************************************************************/

/* HISTORY **************************************************************

0.0     90/11/29  mw    Displays graphs but doesn't show coreference links
0.1	90/12/07  mw	graph and node menus
1.0	90/12/14  mw	editor menu, icons, multiple editors
1.01    91/02/19  mw    minor improvements and debugging

************************************************************************/


:- use_module(library(cgt/cge/swi_apeal)).

/* CONTENTS *************************************************************

shell widget cge/4	the editor window widget

************************************************************************/
:- set_prolog_flag(swi_apeal,false).

_Parent widget Graph= graphViewer(Width, Height, Editor)  <->
    Graph= graphGraph
      / [
	width(Width), height(Height),
        borderWidth(0),
	%destroyCallback( g(cge_destroy(cge_graph, Graph)) ),
	layoutStyle(top_down),				% initial layout style
        defaultHorizDistance(30),			% hz distance of arcs
        defaultVertDistance(30)				% vt distance of arcs
        ]
      + [
	recorda(cge_graph, Graph-none+Editor, _)	% remember graph's WID
        ].


shell widget cge(DisplayWidth, MiniatureFraction, LinearHeight, Editor) <->
    ( recorded(cge_num, _-N, _), succ(N, Number) ; Number = 1 ),
    name('Conceptual Graph Editor #', Ascii1), name(Number, Ascii2),
    conc(Ascii1, Ascii2, Ascii3), name(Title, Ascii3),
    name('CG Editor #', Ascii4),
    conc(Ascii4, Ascii2, Ascii5), name(IconName, Ascii5), 
    MiniatureWidth is DisplayWidth // MiniatureFraction,
    MiniatureDist is DisplayWidth - MiniatureWidth,
    cge: Editor= topLevelShell 
      / [
	iconName(IconName),
	title(Title),
	destroyCallback(g(cge_destroy(Editor)))
	]
      - [
        form
          - [
            Palette= cuTbl		% widget with easy access commands
              / [         
                right(left), bottom(top),	% in upper left corner of editor
                formatString([ 
		  [@(c), c, @(c)],		% buttons for commands without
		  [  ^,  c,   ^ ],		% keyboard shortcuts
		  [c]      			% layout command box is centered
		]),
                borderWidth(0)
                ]
              - [
		space(1, 1),
		RestrictType= command
		  / [
	    	    callback(g(cge_action(indiv, cge_restrict_type, Editor, only-concept, multiple))),
		    shapeStyle(rectangle)
		    ]
		  + [
		    xt_parse(enter : set, ComTrans),
		    override_translations(ComTrans),
		    read_bitmap('rest_type.icon', RestTypeIcon),
		    RestrictType wset bitmap(RestTypeIcon)
		    ],
		space(1, 1),
		RestrictRef= command
		  / [
	    	    callback(g(cge_action(indiv, cge_restrict_ref, Editor, only-concept, multiple))),
		    shapeStyle(rectangle)
		    ]
		  + [
		    override_translations(ComTrans),
		    read_bitmap('rest_ref.icon', RestRefIcon),
		    RestrictRef wset bitmap(RestRefIcon)
		    ],
		space(1, 15),
		box
		  / [
		    hSpace(0),
		    vSpace(0),
		    borderWidth(1)
		    ]
		  - [
                    cuTbl			% commands for layout control
                      / [
		    	formatString([ [c] ]),
		    	internalHeight(3),	% vt dist betw. cuTbl & buttons
		    	internalWidth(3),	% hz dist betw. cuTbl & buttons
		    	interHeight(2)		% vt dist between buttons
                    	]
                      - [
			Mode= cuButton
			  / [
			    label('Auto'),	% initial mode is automatic
			    set(true),		% no graph in manual mode
			    highlightColor(white),
			    callback(g(toggle_mode(Mode)))
			    ],
			space(1, 8),
                        'Hierarchy': Hier= cuButton
                          / [highlightColor(white),
                            set(true)                   % initial layout mode
                            ],
                        'Spring': Spring= cuButton
			  / [
			    highlightColor(white)
			    ],
                        'Tree': Tree= cuButton
			  / [
			    highlightColor(white)
			    ],
			space(1, 8),
			cuTbl				% style buttons
			  / [
			    formatString([
				[@(c), @(c), c, <, @(c), @(c)],
				[ ^  ,   c , <, c,  <  ,  ^  ],
				[ ^  , @(c), c, <, @(c),  ^  ]
			    ])
			    ]
			  - [
			    space(1, 1),
			    space(1, 1),
			    style: BU= toggle
			      / [
				callback(g(cge_style(Editor, BU, bottom_up)))
				]
 			      + [
				read_bitmap('bu.icon', BUIcon),
			    	BU wset bitmap(BUIcon)
			    	],
			    space(1, 1),
			    space(1, 1),
			    style: RL= toggle
			      / [
				callback(g(cge_style(Editor, RL, right_left))),
				radioGroup(BU)
				]
			      + [
				read_bitmap('rl.icon', RLIcon),
			    	RL wset bitmap(RLIcon)
			    	],
			    style: LR= toggle
			      / [
				callback(g(cge_style(Editor, LR, left_right))),
				radioGroup(BU)
				]
			      + [
				read_bitmap('lr.icon', LRIcon),
			    	LR wset bitmap(LRIcon)
			    	],
			    space(1, 1),
			    style: TD= toggle
			      / [
				callback(g(cge_style(Editor, TD, top_down))),
				radioGroup(BU), state(true)	% initial style
				]
			      + [
			    	read_bitmap('td.icon', TDIcon),
			    	TD wset bitmap(TDIcon)
			    	],
			    space(1, 1)
			    ]
			  + [
			    [LR, RL, TD, BU] wset [
				shapeStyle(rectangle), highlightThickness(1)
			    ]
			    ]
                        ]
		      + [
			[Mode, Hier, Spring, Tree] wset
			    [ shadow(false), dimFacetColor(gray40), 
			      brightFacetColor(lightGray), 
			      highlightColor(white)
			    ],
			xt_parse(btn(up) : goal(true), NoUpTrans),
			[Mode, Hier, Spring, Tree] wproc
			    override_translations(NoUpTrans)
			]
		    ],
                    Layout= cuBmgr			% outside of box
                      / [
			setCallback(g((next_event(E), cge_layout(Editor, E)))),
                        bmgrType(one_of_many)       	% only one layout mode
                        ]
                      + [
                        Layout wproc manage(
			    [Tree,    Hier, 	       Spring],
			    [t(tree), t(hierarchical), t(spring)]
			)
                        ]
                    ],
                Header= asciiText	% describes kind of graph (schema, etc.)
                  / [
    		    font(courier-[bold, size(pixel)=10, slant=r]),
                    bottom(top),        	% fixed dist from top of editor
                    right(left),        	% fixed dist from left of editor
                    fromHoriz(Palette),		% to the right of Palette
		    editType(edit),		% displayed text may be edited
		    sensitive(false),		% but not by the user
		    resize(width),		% Header may get wider
		    resizable(true),		% Form allows Header to resize
		    displayCaret(false),	% don't show insertion point
		    borderWidth(0)%,
		    %cursor(68)			% northwest arrow (XC_left_ptr)
                    ],
		Display= paned 
		  / [
                    fromVert(Header),		% under the graph description
                    fromHoriz(Palette)		% to the right of the palette
		    ]
		  - [
		    _Graphical= form 
		      / [
			defaultDistance(0)
			]
		      - [
		    	Shadow= core		% miniature of whole graph
		      	  / [
			    top(rubber), left(rubber),	% proportionally in
			    bottom(bottom), right(right),% bottom right corner
			    mappedWhenManaged(false),	% initially invisible
			    width(MiniatureWidth), horizDistance(MiniatureDist),
			    height(MiniatureWidth), vertDistance(MiniatureDist)
			    ],
                    	viewport
                      	  / [
			    borderWidth(0),
                    	    forceBars(true),	% scrollbars always present
                    	    allowHoriz(true),   % allow displayed graph to widen
                    	    allowVert(true),    % allow displayed graph to grow
                    	    width(DisplayWidth), height(DisplayWidth)
              		    ]
                       	  - [
                    	    Graph= graphViewer(DisplayWidth, DisplayWidth,
					       Editor)
			      + [
				Graph wset shadowWidget(Shadow)
				]
                    	    ]
			],
		    Linear= asciiText
		      / [
			editType(edit),		% displayed text may be edited
			scrollVertical(always),	% vt scrollbar always present
			scrollHorizontal(whenNeeded),
			dataCompression(true),	% save memory space
			width(DisplayWidth), 
			height(LinearHeight),
			skipAdjust(true)	% try to resize Graphical first
			]
		    ],
                _MenuBar= box
                  / [
                    top(bottom),        	% don't change size vertically
                    right(left),        	% don't change size horizontally
                    fromVert(Display),		% under the Display
                    fromHoriz(Palette), 	% to the right of the palette
                    background(white),  	% white bar
		    orientation(horizontal),	% menu options side by side
                    vSpace(0), hSpace(0)	% no space between menu options
                    ]
		  - [
		    MainMB= menuButton
		      / [
			label('Editor'), menuName(mainMenu)
			],
		    GraphMB= menuButton
		      / [
			label('Graph'), menuName(graphMenu)
			],
		    NodeMB= menuButton
		      / [
			label('Node'), menuName(nodeMenu)
			]
		    ]
		  + [
		    [MainMB, GraphMB, NodeMB] wset borderWidth(0),
		    xt_parse([enter: set], ButtonTrans),
		    [MainMB, GraphMB, NodeMB] wproc
			override_translations(ButtonTrans)
		    ],
                modified: Modified= label       % appears upon changes
                  / [
                    fromVert(Display),		% under the display
                    top(bottom), right(left)	% in bottom left corner of Form
                    ]
		  + [
		    ( recorded(get_db_modif, Yes, _)	% was database modified?
		    ; Yes = false
		    ),
		    Modified wset mappedWhenManaged(Yes)% map label iff modified
		    ]
                ]
        ]
      + [
	read_bitmap('cge.icon', Icon),
	Editor wset iconPixmap(Icon),
	popup(Editor) widget mainMenu,
	popup(Editor) widget graphMenu,
	popup(Editor) widget nodeMenu,
        xt_parse([
	    key-'Up'   : goal(Style wproc set_child(BU)),
	    key-'Down' : goal(Style wproc set_child(TD)),
	    key-'Left' : goal(Style wproc set_child(RL)),
	    key-'Right': goal(Style wproc set_child(LR)),

	    % editor menu

	    control/key-l :
	      goal(cge_load(Editor)),
	    control/key-s :
	      goal(cge_save_gr(Editor)),
	    key-'Delete' :
	      goal(cge_clear_editor(Editor)),
	    control/key-o :
	      goal(cge_open_db(Editor)),
	    control/key-u :
	      goal(cge_save_db(Editor)),
	    control/key-h :
	      goal(cge_help),
	    control/key-q :
	      goal(cge_quit(Editor)),

	    % graph menu

	    meta/key-c :
	      goal(cge_action(indiv, cge_copy, Editor, graph, multiple)),
	    meta/key-o :
	      goal(cge_action(conj, cge_join_on, Editor, only-concept, multiple)),
	    meta/key-j :
	      goal(cge_action(conj, cge_join, Editor, graph, multiple)),
	    [meta, shift]/key-j :
	      goal(cge_action(conj, cge_max_join, Editor, graph, multiple)),
	    meta/key-s :
	      goal(cge_action(indiv, cge_simplify, Editor, graph, multiple)),
	    meta/key-e :
	      goal(cge_action(indiv, cge_erasure, Editor, graph, multiple)),
	    meta/key-i :
	      goal(cge_action(indiv, cge_insertion, Editor, graph, single)),
	    [meta, shift]/key-a :
	      goal(cge_action(indiv, cge_iteration, Editor, graph, single)),
	    meta/key-a :
	      goal(cge_action(indiv, cge_deiteration, Editor, graph, multiple)),
	    [meta, shift]/key-d :
	      goal(cge_draw_dn(Editor)),
	    meta/key-d :
	      goal(cge_action(indiv, cge_erase_dn, Editor, graph, multiple)),

	    % node menu

	    [meta, shift]/key-t : 
	      goal(cge_action(indiv, cge_max_exp, Editor, only-concept, multiple)),
	    meta/key-t :
	      goal(cge_action(indiv, cge_min_exp, Editor, only-concept, multiple)),
	    [meta, shift]/key-r :
	      goal(cge_action(indiv, cge_rel_exp, Editor, only-relation, multiple)),
	    [meta, shift]/key-m :
	      goal(cge_action(indiv, cge_meas_exp, Editor, only-concept, multiple)),
	    meta/key-m :
	      goal(cge_action(indiv, cge_meas_ctr, Editor, only-relation, multiple)),
	    [meta, shift]/key-n :
	      goal(cge_action(indiv, cge_name_exp, Editor, only-concept, multiple)),
	    meta/key-n :
	      goal(cge_action(indiv, cge_name_ctr, Editor, only-relation, multiple)),
	    [meta, shift]/key-q :
	      goal(cge_action(indiv, cge_qty_exp, Editor, only-concept, multiple)),
	    meta/key-q :
	      goal(cge_action(indiv, cge_qty_ctr, Editor, only-relation, multiple)),
	    [meta, shift]/key-u :
	      goal(cge_action(indiv, cge_univ_exp, Editor, only-concept, multiple))

	    ], Shortcuts),
            override_translations(Shortcuts),
            recorda(cg_editor, 
		    Editor-Header-Graph-Linear-Mode/[Hier, Spring, Tree]/[LR, RL, TD, BU],
		    _),
	    recorda(cge_shadow, Editor-Shadow, _),
	    recorda(cge_modif, Editor-Modified, _),
	    recorda(cge_num, Editor-Number, _)
            ].

popup(Editor) widget mainMenu <->
    mainMenu: simpleMenu
      - [
	'Load graph...      C-L': smeBSB
	  / [
	    callback(g(cge_load(Editor)))
	    ],
	'Store graph...     C-S': smeBSB
	  / [
	    callback(g(cge_save_gr(Editor)))
	    ],
	'Clear graph(s)  Delete': smeBSB
	  / [
	    callback(g(cge_clear_editor(Editor)))
	    ],
	smeLine,
	'Open database...   C-O': smeBSB
	  / [
	    callback(g(cge_open_db(Editor)))
	    ],
	'Update database... C-U': smeBSB
	  / [
	    callback(g(cge_save_db(Editor)))
	    ],
	smeLine,
	'HEEEEEELP!!!       C-H': smeBSB
	  / [
	    callback(g(cge_help))
	    ],
	ShadowEntry= smeBSB
	  / [
	    label('Show Miniature'),
	    callback(g(toggle_shadow(ShadowEntry, Editor)))
	    ],
	'Quit               C-Q': smeBSB
	  / [
	    callback(g(cge_quit(Editor)))
	    ]
	].

popup(Editor) widget graphMenu <->
    graphMenu: simpleMenu
      - [
	'Copy             M-C': smeBSB
	  / [
	    callback(g(cge_action(indiv, cge_copy, Editor, graph, multiple)))
	    ],
	'Join on          M-O': smeBSB
	  / [
	    callback(g(cge_action(conj, cge_join_on, Editor, only-concept, multiple)))
	    ],
	'Join             M-J': smeBSB
	  / [
	    callback(g(cge_action(conj, cge_join, Editor, graph, multiple)))
	    ],
	'Maximal Join    MS-J': smeBSB
	  / [
	    callback(g(cge_action(conj, cge_max_join, Editor, graph, multiple)))
	    ],
	'Simplify         M-S': smeBSB
	  / [
	    callback(g(cge_action(indiv, cge_simplify, Editor, graph, multiple)))
	    ],
	smeLine,
	'Erasure          M-E': smeBSB
	  / [
	    callback(g(cge_action(indiv, cge_erasure, Editor, graph, multiple)))
	    ],
	'Insertion        M-I': smeBSB
	  / [
	    callback(g(cge_action(indiv, cge_insertion, Editor, graph, single)))
	    ],
	'Iteration       MS-A': smeBSB
	  / [
	    callback(g(cge_action(indiv, cge_iteration, Editor, graph, single)))
	    ],
	'Deiteration      M-A': smeBSB
	  / [
	    callback(g(cge_action(indiv, cge_deiteration, Editor, graph, multiple)))
	    ],
	'Draw Double Neg MS-D': smeBSB
	  / [
	    callback(g(cge_draw_dn(Editor)))
	    ],
	'Erase Double Neg M-D': smeBSB
	  / [
	    callback(g(cge_action(indiv, cge_erase_dn, Editor, graph, multiple)))
	    ],
	smeLine,
	'Compare             ': smeBSB
	  / [
	    callback(g(cge_action(indiv, cge_compare, Editor, graph, single)))
	    ],
	'Depth               ': smeBSB
	  / [
	    callback(g(cge_action(indiv, cge_depth, Editor, graph, single)))
	    ]
	].

popup(Editor) widget nodeMenu <->
    nodeMenu: simpleMenu
      - [
	'Restrict Type              ': smeBSB
	  / [
	    callback(g(cge_action(indiv, cge_restrict_type, Editor, only-concept, multiple)))
	    ],
	'Restrict Referent          ': smeBSB
	  / [
	    callback(g(cge_action(indiv, cge_restrict_ref, Editor, only-concept, multiple)))
	    ],
	smeLine,
	'Max Type Expansion     MS-T': smeBSB
	  / [
	    callback(g(cge_action(indiv, cge_max_exp, Editor, only-concept, multiple)))
	    ],
	'Min Type Expansion      M-T': smeBSB
	  / [
	    callback(g(cge_action(indiv, cge_min_exp, Editor, only-concept, multiple)))
	    ],
	'Relational Expansion   MS-R': smeBSB
	  / [
	    callback(g(cge_action(indiv, cge_rel_exp, Editor, only-relation, multiple)))
	    ],
	smeLine,
	'Measure Expansion      MS-M': smeBSB
	  / [
	    callback(g(cge_action(indiv, cge_meas_exp, Editor, only-concept, single)))
	    ],
	'Measure Contraction     M-M': smeBSB
	  / [
	    callback(g(cge_action(indiv, cge_meas_ctr, Editor, only-relation, single)))
	    ],
	'Name Expansion         MS-N': smeBSB
	  / [
	    callback(g(cge_action(indiv, cge_name_exp, Editor, only-concept, single)))
	    ],
	'Name Contraction        M-N': smeBSB
	  / [
	    callback(g(cge_action(indiv, cge_name_ctr, Editor, only-relation, single)))
	    ],
	'Quantity Expansion     MS-Q': smeBSB
	  / [
	    callback(g(cge_action(indiv, cge_qty_exp, Editor, only-concept, single)))
	    ],
	'Quantity Contraction    M-Q': smeBSB
	  / [
	    callback(g(cge_action(indiv, cge_qty_ctr, Editor, only-relation, single)))
	    ],
	'Expand Univ Quantifier MS-U': smeBSB
	  / [
	    callback(g(cge_action(indiv, cge_univ_exp, Editor, only-concept, single)))
	    ]
	].

Graph= graphGraph widget Relation= relation(Label, Links) <->
    recorded(cge_graph, Graph-_+Editor, _),	% get associated Editor
    Label: Relation= toggle
      / [
        linkedNodes(Links),			% concepts linked to Relation
        resizable(true),			% Relation may grow
	%destroyCallback( g(cge_destroy(cge_relation, Relation)) ),
        font(courier-[bold, size(pixel)=10, slant=r])
        ]
      + [
	xt_parse([
	    btn(right, down) : goal(cge_move(Relation)),
	    [meta, ctrl]/btn(left, down) :
		goal(toggle_sel(sec, relation, Relation, Graph, Editor)),
	    btn(left, down)*2: goal((Graph wproc unmap, cge_rel_exp(Relation), Graph wproc map, update_linear(Editor))),
	    btn(left, down)  :
		goal(toggle_sel(prim, relation, Relation, Graph, Editor))
	], RelTrans),
	override_translations(RelTrans),
	recorda(cge_relation, Relation-_+Editor, _)
	].

Graph= graphGraph widget Concept= concept(Links) <->
    recorded(cge_graph, Graph-_+Editor, _),	% get associated Editor
    Concept= form
      / [
	defaultDistance(0),
	%destroyCallback( g(cge_destroy(cge_concept, Concept)) ),
	width(10), height(10),
        linkedNodes(Links),         % relations linked to Concept
        resizable(true)            % Concept may grow
        ]
      + [
	xt_accelerators([
	    btn(right, down) : goal(cge_move(Concept)),
	    [meta, ctrl]/btn(left, down) :
		goal(toggle_sel(sec, concept, Concept, Graph, Editor)),
	    btn(left, down)*2: goal((Graph wproc unmap, cge_max_exp(Concept), Graph wproc map, update_linear(Editor))),
	    btn(left, down) :
		goal(toggle_sel(prim, concept, Concept, Graph, Editor))
	], ConAcc),
	Concept wset accelerators(ConAcc),
	recorda(cge_concept, Concept-none/none+Editor, _)
        ].

Concept= form widget Type= typeField(TypeLabel) <->
    Type= unmanaged form
      / [
	%destroyCallback( g(cge_destroy(cge_type, Type)) ),
	%bottom(top), right(left),	% in upper left corner of Concept
        borderWidth(0),
	defaultDistance(0),		% children all together
	resizable(true)
        ]
      - [
	Label= label
	  / [
	    %borderWidth(1),
            font(courier-[bold,size(pixel)=10,slant=r]), label(TypeLabel),
	    %bottom(top), right(left),
	    resizable(true)
	    ]
	]
      + [
	install_accelerators(Concept),
	manage,
	( recorded(cge_concept, Concept-none/Ref+Editor, DbR), erase(DbR),
	  recorda(cge_concept, Concept-Type/Ref+Editor, _)
	; recorded(cge_context, Concept-none/Ref/Graph+Editor, DbR), erase(DbR),
	  recorda(cge_context, Concept-Type/Ref/Graph+Editor, _)
	),
	recorda(cge_type, Type-none/Label/none+Editor, _)
	].

Concept= form widget Type= typeField(TypeLabel, Graph) <->
    ( recorded(cge_concept, Concept-_+Editor, _)
    ; recorded(cge_context, Concept-_+Editor, _)
    ),
    Type= unmanaged form
      / [
	%destroyCallback( g(cge_destroy(cge_type, Type)) ),
        borderWidth(0),
	defaultDistance(0),			% children all together
	resizable(true)
        ]
      - [
	l: Lambda= label
	  / [
	    borderWidth(0),
	    font(symbol-[size(pixel)=10]),	% display lambda letter
	    bottom(top), right(left)
	    ],
	TypeLabel: Label= label
	  / [
	    borderWidth(0),
            font(courier-[bold,size(pixel)=10,slant=r]),
	    fromHoriz(Lambda),
	    %bottom(top), right(left),
	    resizable(true)
	    ],
        Graph= graphViewer(1, 1, Editor)
	  + [
	    Graph wset [resizable(true), fromVert(Lambda)]
	    ]
        ]
      + [
	install_accelerators(Concept),
	manage,
	( recorded(cge_concept, Concept-none/Ref+Editor, DbR), erase(DbR),
	  recorda(cge_concept, Concept-Type/Ref+Editor, _)
	; recorded(cge_context, Concept-none/Ref/Graph+Editor, DbR), erase(DbR),
	  recorda(cge_context, Concept-Type/Ref/Graph+Editor, _)
	),
	recorda(cge_type, Type-Lambda/Label/Graph+Editor, _)
	].

Concept= form widget Ref= refField(Label) <->
    recorded(cge_concept, Concept-Type/none+Editor, DbR), 
    Ref= asciiText
      / [
	%top(bottom), left(right),
	fromHoriz(Type),
	font(courier-[bold,size(pixel)=10,slant=r]),
	editType(edit),			% displayed text may be edited
	sensitive(false),		% but not by the user
	resize(width),			% Ref may get wider
	width(1),			% to stretch only as necessary
	resizable(true),		% Concept allows it actually to resize
	displayCaret(false),
	borderWidth(0)%,
	%cursor(68)
	]
      + [
	install_accelerators(Concept),
	replace_text(Ref, Label),
	erase(DbR), recorda(cge_concept, Concept-Type/Ref+Editor, _)
	].

Graph= graphGraph widget Context= context(Links) <->
    recorded(cge_graph, Graph-_+Editor, _),
    Context= form
      / [
	defaultDistance(0),
	width(10), height(10),
        linkedNodes(Links),         % relations linked to Concept
        resizable(true)            % Concept may grow
	]
      + [
	xt_accelerators([
	    btn(right, down) : goal(cge_move(Context)),
	    [meta, ctrl]/btn(left, down) :
		goal(toggle_sel(sec, context, Context, Graph, Editor)),
	    btn(left, down)*2: goal((Graph wproc unmap, cge_max_exp(Context), Graph wproc map, update_linear(Editor))),
	    btn(left, down) :
		goal(toggle_sel(prim, context, Context, Graph, Editor))
	], EnvAcc),
	Context wset accelerators(EnvAcc),
	recorda(cge_context, Context-none/none/none+Editor, _)
	].

Context= form widget Graph= graphs <->
    recorded(cge_context, Context-Type/Ref/none+Editor, DbR),
    Graph= graphViewer(1, 1, Editor)
      + [
	install_accelerators(Context),
	Graph wset resizable(true),
	erase(DbR), recorda(cge_context, Context-Type/Ref/Graph+Editor, _)
	].

shell widget graphLoader(S, LIST, Items) <->
  S= transientShell / [
    title('Modal Dialog'),
    geometry('+400+400')
  ] - [
    box / [
      vSpace(2), hSpace(2)
    ] - [
      form / [
	borderWidth(2), defaultDistance(2)
      ] - [
	OK= cuCommand / [ label('OK'), callback(t(LIST-ok)) ],
	
	CANCEL= cuCommand / [
	  fromVert(OK), label('Cancel'), callback(t(LIST-cancel))
	],

	viewport / [
	  fromHoriz(CANCEL),
	  forceBars(true), allowVert(true),
	  height(100), width(200)
	] - [
	  LIST= list / [
	    list(Items), 
	    borderWidth(0), 
	    forceColumns(true)			% show the initial two columns
	  ] + [
	    LIST wproc highlight(0),		% highlight first item
	    xt_parse([btn(down) : 'Set',
		      btn(up)*2 : term(t(LIST-ok)) ], Tr),
	    override_translations(Tr)
	  ]
	]
      ]
    ]
  ].

:- set_prolog_flag(swi_apeal,false).
