% FILE xm.pl


:- ensure_loaded(dmiles).

% IMPORTS
:- use_module_if_exists(library(proxt)).
:- use_module_if_exists(library(ctypes)).
:- use_module_if_exists(library(strings)).
:- use_module_if_exists(library(basics)).
:- consult(miles).
%%%:- use_module(show_utils).
:- consult(xmiles_functions).



%***********************************************************************
%*	
%* file: xm.pl        					
%*									
%* author: T. Volz
%*									
%* changed:								
%*									
%* description:	X interface for MILES. Load this directly into prolog
%*        (NOT qui!!), and call `xm.' to create the interface and start
%*        the event loop. Use the `Quit XMILES'-button to suspend the
%*        event loop, if you want to access the prolog-prompt directly.
%*        Calling `xm' again restarts the event loop.
%*        
%* see also:	 
%*									
%***********************************************************************

runtime_entry(start) :- xm.

:- dynamic listItems/1, my_exit_loop/1, toplevel/1.

my_exit_loop(no).





%************************************************************************
%*
%* predicate: xm		main predicate
%*
%* syntax: -
%*
%* args: -
%*
%* description: Creates the whole xmiles interface.
%*
%************************************************************************


xm :-
        (   toplevel(Widget) ->
            XMiles = Widget,
	    open('xmProtocol.tmp',write,F),
	    recordz(messages,file(F),_),
            updateEvaluationLabel,
	    refreshKnowledgeList(_Widget,rules,_Calldata),
	    refreshKnowledgeList(_Widget,examples,_Calldata)
	;   xtToolkitInitialize,
            xtInitialize('X-MILES',xMILES,XMiles),
            assert(toplevel(XMiles)),
	    clear_kb,

	    xmCreateRowColumn(XMiles,xMilesRow,
	       [xmNorientation(xmHORIZONTAL),
	        xmNpacking(xmPACK_TIGHT),
	        xmNrowColumnType(xmWORK_AREA)],
	       XMilesRow),
	    xtManageChild(XMilesRow),

	    xmCreateRowColumn(XMilesRow,xMilesColumn1,
	       [xmNorientation(xmVERTICAL),
	        xmNpacking(xmPACK_TIGHT),
	        xmNrowColumnType(xmWORK_AREA)],
	       XMilesColumn1),	
	    xtManageChild(XMilesColumn1),

	    createCommandArea(XMilesColumn1),
	    createFunctionArea(XMilesColumn1),
	    createArgumentArea(XMilesColumn1),
	    createMessageArea(XMilesColumn1),

	    xmCreateRowColumn(XMilesRow,xMilesColumn2,
	       [xmNorientation(xmVERTICAL),
	        xmNpacking(xmPACK_TIGHT),
	        xmNrowColumnType(xmWORK_AREA)],
	        XMilesColumn2),	
	    xtManageChild(XMilesColumn2),

	    createEditorArea(XMilesColumn2),
	    createKnowledgeBaseArea(XMilesColumn2),

	    xtRealizeWidget(XMiles)
        ),

        my_main_loop(XMiles).

my_main_loop(Shell):-
   (   my_exit_loop(yes) ->
       retract(my_exit_loop(yes)),
       assert(my_exit_loop(no))
   ;   xtNextEvent(Event),
       xtDispatchEvent(Event),
       my_main_loop(Shell)
   ).


%************************************************************************
%*
%* predicate: createKnowledgeBaseArea/1
%*
%* syntax: createKnowledgeBaseArea(+Parent)
%*
%* args: Parent... specifies the parentwidget of KbRowColumn 
%*
%* description: Creates the knowledge base area of the X user interface
%*		This area consists of two scrolled windows. One window
%*		is used to browse the rules of the knowledge base, the
%*		other window browses the examples. Used by xm/0.
%*
%************************************************************************

createKnowledgeBaseArea(Parent) :-
	xmCreateFrame(Parent,'KnowledgeBaseFrame',
	  [],KnowledgeBaseFrame),
	xtManageChild(KnowledgeBaseFrame),

	xmCreateRowColumn(KnowledgeBaseFrame,'KnowledgeBaseColumn',
	  [xmNadjustLast(true),
	   xmNorientation(xmVERTICAL),
	   xmNpacking(xmPACK_TIGHT),
	   xmNrowColumnType(xmWORK_AREA)],
	  KnowledgeBaseColumn),	
	xtManageChild(KnowledgeBaseColumn),

	xmCreateRowColumn(KnowledgeBaseColumn,titleRC,
	[xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN)],
	    TitleRowColumn),
	xtManageChild(TitleRowColumn),

	proxtGetDefaultCharset(DCharset),
	proxtStringToCharPtr('Knowledge Base',KbCP),
	xmStringCreate(KbCP,DCharset,StatusStr),
	xmCreateLabelGadget(TitleRowColumn,titleLabel,
	  [xmNlabelType(xmSTRING),xmNlabelString(StatusStr)],
	  KbLabel),
	xtManageChild(KbLabel),

        createEvaluationString(EString),
	proxtStringToCharPtr(EString,KbCP1),
	xmStringCreate(KbCP1,DCharset,StatusStr1),
	xmCreateLabelGadget(TitleRowColumn,titleEvaluation,
	  [xmNlabelType(xmSTRING),xmNlabelString(StatusStr1)],
	  KbLabel1),
	xtManageChild(KbLabel1),
        recordz(irene,KbLabel1,_),

	xmCreateRowColumn(KnowledgeBaseColumn,kbRowColumn,
	  [xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN)],
	  KnowledgeBaseRow),
	xtManageChild(KnowledgeBaseRow),

	createRuleArea(KnowledgeBaseRow),
	createExampleArea(KnowledgeBaseRow),
	createKbButtons(KnowledgeBaseColumn).


%************************************************************************
%*
%* predicate: createRuleArea/1 
%*
%* syntax: createRuleArea(+Parent)
%*
%* args: Parent... specifies the parent widget of RuleFrame
%*
%* description: Creates the rule browser of the knowledge base. The rule
%*	        browser consists of a label, a scrolled rule list and
%*		a refresh button, which looks up for changes of the
%*		knowledge base. Used by createKbRowColumn/1
%*
%************************************************************************

createRuleArea(Parent):-
	xmCreateFrame(Parent,'RuleFrame',
	  [],RuleFrame),
	xtManageChild(RuleFrame),

	xmCreateRowColumn(RuleFrame,'RuleRowColumn',
	  [xmNwidth(300),xmNadjustLast(true),
	   xmNorientation(xmVERTICAL),
	   xmNpacking(xmPACK_TIGHT),
	   xmNrowColumnType(xmWORK_AREA)],
	  RuleColumn),	
	xtManageChild(RuleColumn),

	xmCreateRowColumn(RuleColumn,ruleRow,
	  [xmNorientation(xmHORIZONTAL),xmNmarginHeight(0),
	   xmNpacking(xmPACK_TIGHT)],
	  RuleRow),
	xtManageChild(RuleRow),

	proxtGetDefaultCharset(DCharset),
	proxtStringToCharPtr('Rules               ',RuleCP),
	xmStringCreate(RuleCP,DCharset,RuleStr),
	xmCreateLabelGadget(RuleRow,ruleLabel,
	  [xmNlabelType(xmSTRING),xmNlabelString(RuleStr)],
	  RuleLabel),
	xtManageChild(RuleLabel),

	xmCreatePushButton(RuleRow,'Examine ...',
	  [],Examine),
	xtManageChild(Examine),
	createExamineRulesPopup(Examine),

	createKnowledgeList(RuleColumn,rules),
	createKbSubButtons(RuleColumn,rules).




%************************************************************************
%*
%* predicate: createExampleArea/1
%*
%* syntax: createExampleArea(+Parent)
%*
%* args: +Parent		Widget 
%*
%* description: Creates the example browser of the knowledge base. The
%*		example browser consists of a label, a scrolled rule list
%*		and a refresh button, which looks up for changes of the
%*		knowledge base.
%*
%************************************************************************

createExampleArea(Parent) :-
	xmCreateFrame(Parent,'ExampleFrame',
	  [],ExampleFrame),
	xtManageChild(ExampleFrame),

	xmCreateRowColumn(ExampleFrame,'ExampleRowColumn',
	  [xmNwidth(300),xmNadjustLast(true),
	   xmNorientation(xmVERTICAL),
	   xmNpacking(xmPACK_TIGHT),
	   xmNrowColumnType(xmWORK_AREA)],
	  ExampleRowColumn),	
	xtManageChild(ExampleRowColumn),

	proxtGetDefaultCharset(DCharset),
	proxtStringToCharPtr('Examples',ExampleCP),
	xmStringCreate(ExampleCP,DCharset,ExampleStr),
	xmCreateLabelGadget(ExampleRowColumn,exampleLabel,
	  [xmNlabelType(xmSTRING),xmNlabelString(ExampleStr)],
	  ExampleLabel),
	xtManageChild(ExampleLabel),

	createKnowledgeList(ExampleRowColumn,examples),
	createKbSubButtons(ExampleRowColumn,examples).


%************************************************************************
%*
%* predicate: createKnowledgeList/3
%*
%* syntax: createKnowledgeList(+Parent,-KnowledgeRC,+KindOfKnowledge)
%*
%* args:       +Parent			Widget
%*	       -KnowledgeRC		Widget
%*	       +KindOfKnowledge		'rules' or 'examples'	
%*
%* description: Creates a List either containing all rules or all examples
%*
%************************************************************************

createKnowledgeList(Parent,KindOfKnowledge) :-
	xmCreateScrolledWindow(Parent,knowledgelistSW,
	  [xmNheight(300),xmNwidth(300),
	   xmNlistSizePolicy(xmCONSTANT),
	   xmNscrollBarDisplayPolicy(xmSTATIC),
	   xmNscrollingPolicy(xmAUTOMATIC)],
	  KL),
	xtManageChild(KL),

	xmCreateRowColumn(KL,knowledgelistRC,
	  [xmNadjustLast(false),
	   xmNorientation(xmVERTICAL),
	   xmNpacking(xmPACK_TIGHT),
	   xmNrowColumnType(xmWORK_AREA)],
	  KnowledgeRC),
	recordz(KindOfKnowledge,knowledgeList(KnowledgeRC),_Ref),
	xtGetValues(KnowledgeRC,[xmNbackground(B)]),
	xtGetValues(KL,[xmNclipWindow(CW)]),
	xtSetValues(CW,[xmNbackground(B)]),
	addKnowledgeListItems(KindOfKnowledge),
	xtManageChild(KnowledgeRC).


%************************************************************************
%*
%* predicate: addKnowledgeListItems/1
%*
%* syntax: addKnowledgeListItems(+KindOfKnowledge)
%*
%* args: +KindOfKnowledge		rules or examples
%*
%* description:  Adds a label for each Rule or Example existing in the kb
%*		 to the parent widget stored in
%*			 KindOfKnowledge, knowledgeList(X)	
%*
%************************************************************************

addKnowledgeListItems(rules) :-
	getNextId(Id),
	get_clause(Id,H,B,S,O),
	addRuleItem(Id,H,B,S,O),
	fail.
addKnowledgeListItems(examples) :-
	getNextId(Id),
	get_example(Id,F,C),
	addExampleItem(Id,F,C),
	fail.
addKnowledgeListItems(_) :- !.


%************************************************************************
%*
%* predicate: getNextId/1
%*
%* syntax: getNextId(+Id)
%*
%* args: +Id			Integer <= id_count(X)
%*
%* description: Counts from zero to id_count
%*
%************************************************************************

getNextId(Id) :-
	id_count(MaxId),
	repeat,
	(   recorded(knowledgeList,xm_id_count(I_old),Ref) ->
	    erase(Ref),
	    Id is I_old + 1,
	    recordz(knowledgeList,xm_id_count(Id),_) 
	;   Id = 1,
	    recordz(knowledgeList,xm_id_count(Id),_)
	),
	(   MaxId < Id -> 
	    recorded(knowledgeList,xm_id_count(_),Ref2),
	    erase(Ref2),!,fail 
	;   otherwise
        ).


%************************************************************************
%*
%* predicate: deleteKnowledgeListItems/1
%*
%* syntax: deleteKnowledgeListItems(+KindOfKnowledge)
%*
%* args: +KindOfKnowledge		rules or examples
%*
%* description: Deletes all Labels representing rules or examples
%*		of the knowledgelist. Leaves the knowledgebase unchanged
%*
%************************************************************************

deleteKnowledgeListItems(rules) :-
	recorded(current,clause(_,Label,_),Ref),
	xtDestroyWidget(Label),
	erase(Ref),!,
	deleteKnowledgeListItems(rules).
deleteKnowledgeListItems(examples) :-
	recorded(current,example(_,Label,_),Ref),
	xtDestroyWidget(Label),
	erase(Ref),!,
	deleteKnowledgeListItems(examples).
deleteKnowledgeListItems(_) :- !.


%************************************************************************
%*
%* predicate: addRuleItem/4
%*
%* syntax: addRuleItem(+I,+H,+B,+O)
%*
%* args:       +I		Integer, spezifies the ID of the rule
%*	       +H		Head of the rule	
%*	       +B		Body of the rule
%*	       +O		Label	
%*
%* description: Creates a label to show the rule spezified by the
%*		arguments. The Label widget is recorded:
%*	            recordz(current,clause(I,Label,notselected),_Ref) 
%*		If the Label allready exists, then only the text changes.
%*
%************************************************************************

addRuleItem(I,H,B,_S,O):-
     (   recorded(rules,viewMin(MinText),_) ->
	 xmTextGetString(MinText,MinCP),
	 proxtCharPtrToString(MinCP,MinStr),
	 atom_chars(MinStr,MinC),
	 number_chars(Min,MinC)
     ),
	
     (   recorded(rules,viewMax(MaxText),_) ->
	 xmTextGetString(MaxText,MaxCP),
	 proxtCharPtrToString(MaxCP,MaxStr),
	 atom_chars(MaxStr,MaxC),
	 number_chars(Max,MaxC)
     ),

     (   recorded(rules,view(all),_)
     ;   (  recorded(rules,view(labels,LabelList),_),
	    recorded(rules,view(clause_heads,CHList),_),
	    I >= Min,
	    I =< Max,
	    functor(H,CH,_),
	    (  LabelList=[] 
	    ;  member(O,LabelList)
            ),
	    (  CHList=[] 
	    ;  member(CH,CHList)
	    )
	 )
     ),

     proxtGetDefaultCharset(DCharset),
     recorded(rules,knowledgeList(KL),_),
     xxmWriteToString(show_kb_clause(I,H,B,O),DCharset,XmS),

     (   recorded(current,clause(I,W,_),_) ->
	 xtSetValues(W, [xmNlabelString(XmS)]) 
     ;   xmCreateLabel(KL,anyClause,
	    [xmNalignment(xmALIGNMENT_BEGINNING),
	     xmNlabelString(XmS),xmNlabelType(xmSTRING)],
	     Label),
	 xtManageChild(Label),
	 recordz(current,clause(I,Label,notselected),_Ref),
	 xtAddEventHandler(Label,[buttonReleaseMask],
	                     false,selectClause,rules),
	 xtAddEventHandler(Label,[buttonPressMask],false,
	                     firstClauseClick,rules)
     ),!.

addRuleItem(_,_,_,_,_) :- !.



%************************************************************************
%*
%* predicate: addExampleItem/3
%*
%* syntax: addExampleItem(+I,+F,+C)
%*
%* args:       +I		Integer, spezifies the ID of the example
%*	       +F		Example predicate	
%*	       +C		+ or -, classification of the example	
%*
%* description: Creates a label to show the example spezified by the
%*		arguments. The Label widget is recorded:
%*		    recordz(current,example(I,Label,notselected),_Ref)
%*
%************************************************************************

addExampleItem(I,F,C):-
    (    recorded(examples,viewMin(MinText),_) ->
	 xmTextGetString(MinText,MinCP),
	 proxtCharPtrToString(MinCP,MinStr),
	 atom_chars(MinStr,MinC),
	 number_chars(Min,MinC)
    ),
	
    (    recorded(examples,viewMax(MaxText),_) ->
	 xmTextGetString(MaxText,MaxCP),
	 proxtCharPtrToString(MaxCP,MaxStr),
	 atom_chars(MaxStr,MaxC),
	 number_chars(Max,MaxC)
     ),

     (   recorded(examples,view(all),_)
     ;	 (  recorded(examples,view(clause_heads,CHList),_),
	    functor(F,CH,_),
	    I >= Min,
	    I =< Max,
	    (  CHList=[] 
            ;  member(CH,CHList)
	    )
	 )
     ),
	
     proxtGetDefaultCharset(DCharset),
     recorded(examples,knowledgeList(KL),_),

     number_chars(I,S1),
     append(S1," (",S2),
     atom_chars(C,S3),
     append(S2,S3,S4),
     append(S4,"): ",S5),
     atom_chars(A5,S5),
     proxtStringToCharPtr(A5,CP5),
     xmStringCreateLtoR(CP5,DCharset,XmS1),

     xxmWriteToString(write(F),DCharset,XmS2),
     xmStringConcat(XmS1,XmS2,XmS3),
     xmStringSeparatorCreate(XmSep),
     xmStringConcat(XmS3,XmSep,XmS),

     (   recorded(current,example(I,W,_),_) ->
	 xtSetValues(W,[xmNlabelString(XmS)])
     ;   xmCreateLabel(KL,anyExample,
	    [xmNalignment(xmALIGNMENT_BEGINNING),
	     xmNlabelString(XmS),xmNlabelType(xmSTRING)],
	     Label),
	 xtManageChild(Label),
	 xtAddEventHandler(Label,[buttonReleaseMask],false,
	                      selectClause,examples),
	 xtAddEventHandler(Label,[buttonPressMask],false,
	                      firstClauseClick,examples),
	 recordz(current,example(I,Label,notselected),_Ref)
     ),!.


%************************************************************************
%*
%* predicate: refreshKnowledgeList/3		CallbackProcedure
%*
%* syntax: refreshKnowledgeList(_Widget,+KindOfKnowledge,_Calldata)
%*
%* args:       _Widget			calling Widget
%*             +KindOfKnowledge		rules or examples	
%*	       _Calldata		Event
%*
%* description: Ensures that the knowledge displayed is the same than
%*		the knowledge in the knowledge base.	
%*
%************************************************************************

refreshKnowledgeList(_Widget,KindOfKnowledge,_Calldata) :-
	deleteKnowledgeListItems(KindOfKnowledge),
	addKnowledgeListItems(KindOfKnowledge),
        updateEvaluationLabel.


%************************************************************************
%*
%* predicate: selectClause/3			CallbackProcedure
%*
%* syntax: selectClause(+Widget,+KindOfKnowledge,_Calldata)
%*
%* args:       +Widget			calling Widget
%*	       +KindOfKnowledge		rules or examples
%*	       _Calldata		Event
%*
%* description: Toggles selection state of the knowledge item specified
%*		by Widget. Visual effect is the exchange of foreground
%*		and background color.
%*
%************************************************************************

selectClause(Widget,rules,_CallData) :-
	recorded(current,clause(I,Widget,notselected),Ref),
	xtGetValues(Widget,[xmNbackground(B),xmNforeground(F)]),
	xtSetValues(Widget,[xmNbackground(F),xmNforeground(B)]),
	erase(Ref),
	recordz(current,clause(I,Widget,selected),_),
	recorded(state,getId(GetId),_),
	xtGetValues(GetId,[xmNset(GetState)]),
	(   GetState = true ->
	    copyId(I,1) 
	;   otherwise
        ).

selectClause(Widget,rules,_CallData) :-
	recorded(current,clause(I,Widget,selected),Ref),
	xtGetValues(Widget,[xmNbackground(B),xmNforeground(F)]),
	xtSetValues(Widget,[xmNbackground(F),xmNforeground(B)]),
	erase(Ref),
	recordz(current,clause(I,Widget,notselected),_Ref),
	recorded(state,getId(GetId),_),
	xtGetValues(GetId,[xmNset(GetState)]),
	(   GetState = true ->
	    copyId(I,1) 
	;   otherwise
        ).

selectClause(Widget,examples,_CallData) :-
	recorded(current,example(I,Widget,notselected),Ref),
	xtGetValues(Widget,[xmNbackground(B),xmNforeground(F)]),
	xtSetValues(Widget,[xmNbackground(F),xmNforeground(B)]),
	erase(Ref),
	recordz(current,example(I,Widget,selected),_Ref),
	recorded(state,getId(GetId),_),
	xtGetValues(GetId,[xmNset(GetState)]),
	(   GetState = true ->
	    copyId(I,1) 
	;   otherwise
        ).

selectClause(Widget,examples,_CallData) :-
	recorded(current,example(I,Widget,selected),Ref),
	xtGetValues(Widget,[xmNbackground(B),xmNforeground(F)]),
	xtSetValues(Widget,[xmNbackground(F),xmNforeground(B)]),
	erase(Ref),
	recordz(current,example(I,Widget,notselected),_Ref),
	recorded(state,getId(GetId),_),
	xtGetValues(GetId,[xmNset(GetState)]),
	(   GetState = true ->
	    copyId(I,1) 
	;   otherwise
        ).


%************************************************************************
%*
%* predicate: firstClauseClick/3			CallbackProcedure
%*
%* syntax: firstClauseClick(+Widget,_ClientData,_Calldata)
%*
%* args:       +Widget			calling Widget
%*	       _ClientData				
%*	       _Calldata		Event
%*
%* description: If another click occures in the next two seconds, the
%*		knowledgebase item specified by Widget is copied into
%*		the editor.
%*
%*
%************************************************************************

firstClauseClick(Widget,_ClientData,_CallData) :-
	xtAddEventHandler(Widget,[buttonPressMask],false,copyClause,_),
	xtAddTimeOut(1000,noDoubleClick,Widget,_ID).


%************************************************************************
%*
%* predicate: copyClause/3			CallbackProcedure
%*
%* syntax: copyClause(+Widget,_ClientData,_Calldata)
%*
%* args:       +Widget			calling Widget
%*             _ClientData		rules or examples
%*	       _Calldata		Event	
%*
%* description: Copies a knowledge base item spezified by Widget to the
%*	        editor text.
%*
%************************************************************************

copyClause(Widget,_ClientData,_CallData) :-
	recorded(editor,label(Label),_),
	proxtGetDefaultCharset(DCharset),
	(recorded(editor,editing(_,_),Ref) -> erase(Ref),
	    proxtStringToCharPtr('Editor',LblCP1),
	    xmStringCreate(LblCP1,DCharset,LblS1),
	    xtSetValues(Label,[xmNlabelString(LblS1)]) |
         otherwise),
	(recorded(current,clause(Id,Widget,_),_) ->
	    get_clause(Id,H,B,_,_),
	    xxmWriteToCharPtr(portray_clause((H:-B)),CP),
	    recorded(editor,textWidget(Editor),_),
	    xmTextSetString(Editor,CP),
	    recordz(editor,editing(rules,Id),_),
	    number_chars(Id,IdCS),
	    append("Editor     editing rule ",IdCS,LblCS),
	    atom_chars(LblAS,LblCS),
	    proxtStringToCharPtr(LblAS,LblCP),
	    xmStringCreate(LblCP,DCharset,LblS),
	    xtSetValues(Label,[xmNlabelString(LblS)]) |
	 recorded(current,example(Id,Widget,_),_) ->
	    get_example(Id,F,_),
	    xxmWriteToCharPtr(writeFullstop(F),CP),
	    recorded(editor,textWidget(Editor),_),
	    xmTextSetString(Editor,CP),
	    recordz(editor,editing(examples,Id),_),
	    number_chars(Id,IdCS),
	    append("Editor     editing example ",IdCS,LblCS),
	    atom_chars(LblAS,LblCS),
	    proxtStringToCharPtr(LblAS,LblCP),
	    xmStringCreate(LblCP,DCharset,LblS),
	    xtSetValues(Label,[xmNlabelString(LblS)])).


%************************************************************************
%*
%* predicate: noDoubleClick/2			TimeOutProcedure
%*
%* syntax: noDoubleClick(+Widget,_IntervallID)
%*
%* args:       +Widget			knowledge label
%*	       _IntervallId		calling Intervall
%*
%* description: Removes event handler for copying a knowledge base item
%*		into the editor. This predicate is avoked if no second	
%*		button press has occured within two seconds.
%*
%************************************************************************

noDoubleClick(Widget,_IntervallId) :-
	xtRemoveEventHandler(Widget,[buttonPressMask],false,copyClause,_).


%************************************************************************
%*
%* predicate: selectAll/3		Callback procedure	
%*
%* syntax: selectAll(_Widget,+KindOfKnowledge,_CallData)
%*
%* args:       _Widget			calling widget	
%*	       KindOfKnowledge		rules or examples	
%*	       _CallData		event	
%*
%* description: Selects all rules or examples of the knowledge	base. The
%*		rules or examples have to satisfy the current display
%*	        restricion.
%*
%************************************************************************

selectAll(_Widget,rules,_CallData) :-
	recorded(current,clause(I,Widget,notselected),Ref),!,
	xtGetValues(Widget,[xmNbackground(B),xmNforeground(F)]),
	xtSetValues(Widget,[xmNbackground(F),xmNforeground(B)]),
	erase(Ref),
	recordz(current,clause(I,Widget,selected),_Ref),
	selectAll(_Widget,rules,_CallData).

selectAll(_Widget,examples,_CallData) :-
	recorded(current,example(I,Widget,notselected),Ref),!,
	xtGetValues(Widget,[xmNbackground(B),xmNforeground(F)]),
	xtSetValues(Widget,[xmNbackground(F),xmNforeground(B)]),
	erase(Ref),
	recordz(current,example(I,Widget,selected),_Ref),
	selectAll(_Widget,examples,_CallData).

selectAll(_,_,_).


%************************************************************************
%*
%* predicate: unselectAll/3		Callback procedure	
%*
%* syntax: unselectAll(_Widget,+KindOfKnowledge,_CallData)
%*
%* args:       _Widget			calling widget	
%*	       KindOfKnowledge		rules or examples	
%*	       _CallData		event	
%*
%* description: Unselects all rules or examples of the knowledge base. The
%*      	rules or examples have to satisfy the current display
%*		 restricion.
%*
%************************************************************************

unselectAll(_Widget,rules,_CallData) :-
	recorded(current,clause(I,Widget,selected),Ref),!,
	xtGetValues(Widget,[xmNbackground(B),xmNforeground(F)]),
	xtSetValues(Widget,[xmNbackground(F),xmNforeground(B)]),
	erase(Ref),
	recordz(current,clause(I,Widget,notselected),_Ref),
	unselectAll(_Widget,rules,_CallData).

unselectAll(_Widget,examples,_CallData) :-
	recorded(current,example(I,Widget,selected),Ref),!,
	xtGetValues(Widget,[xmNbackground(B),xmNforeground(F)]),
	xtSetValues(Widget,[xmNbackground(F),xmNforeground(B)]),
	erase(Ref),
	recordz(current,example(I,Widget,notselected),_Ref),
	unselectAll(_Widget,examples,_CallData).

unselectAll(_,_,_).


%************************************************************************
%*
%* predicate: deleteKnowledge/3		Callback procedure	
%*
%* syntax: deleteKnowledge(_Widget,+KindOfKnowledge,_CallData)
%*
%* args:       _Widget			calling widget	
%*	       KindOfKnowledge		rules or examples	
%*	       _CallData		event	
%*
%* description: Deletes all selected rules or examples.	
%*
%************************************************************************

deleteKnowledge(_Widget,rules,_CallData) :-
	recorded(current,clause(I,Widget,selected),Ref),!,
	(recorded(editor,editing(rules,I),_) -> clearEditor(_,_,_)|
         otherwise),
	xtGetValues(Widget,[xmNbackground(B),xmNforeground(F)]),
	xtSetValues(Widget,[xmNbackground(F),xmNforeground(B)]),
	erase(Ref),
	xtDestroyWidget(Widget),
	delete_clause(I),
	writeMessage(':- delete_clause('),
	writeMessage(I),
	writelnMessage(').'),
	deleteKnowledge(_Widget,rules,_CallData).
	
deleteKnowledge(_Widget,examples,_CallData) :-
	(recorded(editor,editing(examples,I),_) -> clearEditor(_,_,_)|
         otherwise),
	recorded(current,example(I,Widget,selected),Ref),!,
	xtGetValues(Widget,[xmNbackground(B),xmNforeground(F)]),
	xtSetValues(Widget,[xmNbackground(F),xmNforeground(B)]),
	erase(Ref),
	xtDestroyWidget(Widget),
	delete_example(I),
	writeMessage(':- delete_example('),
	writeMessage(I),
	writelnMessage(').'),
	deleteKnowledge(_Widget,examples,_CallData).

deleteKnowledge(_,KindOfKnowledge,_) :- 
	writeMessage('% selected '),
        writeMessage(KindOfKnowledge),
	writelnMessage(' deleted.'),
        updateEvaluationLabel.


%************************************************************************
%*
%* predicate: deleteAllKnowledge/3		Callback procedure	
%*
%* syntax: deleteKnowledge(_Widget,+KindOfKnowledge,_CallData)
%*
%* args:       _Widget			calling widget	
%*	       KindOfKnowledge		rules or examples	
%*	       _CallData		event	
%*
%* description: Deletes all rules or examples within the current display
%*	        restriction.	
%*
%************************************************************************

deleteAllKnowledge(_Widget,rules,_CallData) :-
	(recorded(editor,editing(rules,_),_) -> clearEditor(_,_,_)|
         otherwise),
	recorded(current,clause(I,Widget,_),Ref),!,
	xtGetValues(Widget,[xmNbackground(B),xmNforeground(F)]),
	xtSetValues(Widget,[xmNbackground(F),xmNforeground(B)]),
	erase(Ref),
	xtDestroyWidget(Widget),
	delete_clause(I),
	writeMessage(':- delete_clause('),
	writeMessage(I),
	writelnMessage(').'),
	deleteAllKnowledge(_Widget,rules,_CallData).
	
deleteAllKnowledge(_Widget,examples,_CallData) :-
	(recorded(editor,editing(examples,_),_) -> clearEditor(_,_,_)|
         otherwise),
	recorded(current,example(I,Widget,_),Ref),!,
	xtGetValues(Widget,[xmNbackground(B),xmNforeground(F)]),
	xtSetValues(Widget,[xmNbackground(F),xmNforeground(B)]),
	erase(Ref),
	xtDestroyWidget(Widget),
	delete_example(I),
	writeMessage(':- delete_example('),
	writeMessage(I),
	writelnMessage(').'),
	deleteAllKnowledge(_Widget,examples,_CallData).

deleteAllKnowledge(_,KindOfKnowledge,_) :- 
	writeMessage('% all '),
        writeMessage(KindOfKnowledge),
	writelnMessage(' deleted.').


%************************************************************************
%*
%* predicate: saveKnowledgeBegin/3		Callback procedure
%*
%* syntax: saveKnowledgeBegin(_Widget,+FileSelectionDialog,_CallData)
%*
%* args:       _Widget			calling widget	
%*	       +FileSelectionDialog	FileSelectionDialogWidget	
%*	       _CallData		event	
%*
%* description: Pops up a fileselection dialog.	
%*
%************************************************************************

saveKnowledgeBegin(_Widget,FileSelectionDialog,_CallData) :-
	xtManageChild(FileSelectionDialog),
	xtAddCallback(FileSelectionDialog,xmNokCallback,
	  saveKnowledgeEnd,FileSelectionDialog),
	xtAddCallback(FileSelectionDialog,xmNcancelCallback,
	  cancelSaveFileSelect,FileSelectionDialog).



%************************************************************************
%*
%* predicate: saveKnowledgeEnd/3		Callback procedure
%*
%* syntax: saveKnowledgeEnd(_Widget,+FileSelectionDialog,+CallData)
%*
%* args:       _Widget			calling widget	
%*	       +FileSelectionDialog	FileSelectionDialogWidget	
%*	       +CallData		event	
%*
%* description: Saves the selected file.
%*
%************************************************************************

saveKnowledgeEnd(_Widget,FileSelectionDialog,CallData) :-
	xtUnmanageChild(FileSelectionDialog),
	xtRemoveCallback(FileSelectionDialog,xmNokCallback,
	  saveKnowledgeEnd,FileSelectionDialog),
	xtRemoveCallback(FileSelectionDialog,xmNcancelCallback,
	  cancelSaveFileSelect,FileSelectionDialog),
	sucheInListe(value(FileNameString),CallData),
	proxtGetDefaultCharset(DCharset),
	xmStringGetLtoR(FileNameString,DCharset,FileNameCP,_),
	proxtCharPtrToString(FileNameCP,FileName),
	((midstring(FileName,_,'.qof',0) ->
	    save_kb(FileName),
	    writeMessage(':- save_kb('),
	    writeMessage(FileName),
	    writelnMessage(').'))|
	 (otherwise ->
	    print_kb(FileName),
	    writeMessage(':- print_kb('),
	    writeMessage(FileName),
	    writelnMessage(').'))),
	writeMessage('% file "'),
	writeMessage(FileName),
	writelnMessage('" saved.').


%************************************************************************
%*
%* predicate: loadKnowledgeBegin/3		Callback procedure
%*
%* syntax: loadKnowledgeBegin(_Widget,+FileSelectionDialog,_CallData)
%*
%* args:       _Widget			calling widget	
%*	       +FileSelectionDialog	FileSelectionDialogWidget	
%*	       _CallData		event	
%*
%* description: Pops up a fileselection dialog.	
%*
%************************************************************************

loadKnowledgeBegin(_Widget,FileSelectionDialog,_CallData) :-
	xtManageChild(FileSelectionDialog),
	xtAddCallback(FileSelectionDialog,xmNokCallback,
	  loadKnowledgeEnd,FileSelectionDialog),
	xtAddCallback(FileSelectionDialog,xmNcancelCallback,
	  cancelLoadFileSelect,FileSelectionDialog).


%************************************************************************
%*
%* predicate: loadKnowledgeEnd/3		Callback procedure
%*
%* syntax: loadKnowledgeEnd(_Widget,+FileSelectionDialog,+CallData)
%*
%* args:       _Widget			calling widget	
%*	       +FileSelectionDialog	FileSelectionDialogWidget	
%*	       _CallData		event	
%*
%* description: Loads the selected file.
%*
%************************************************************************

loadKnowledgeEnd(_Widget,FileSelectionDialog,CallData) :-
	xtUnmanageChild(FileSelectionDialog),
	xtRemoveCallback(FileSelectionDialog,xmNokCallback,
	  loadKnowledgeEnd,FileSelectionDialog),
	xtRemoveCallback(FileSelectionDialog,xmNcancelCallback,
	  cancelLoadFileSelect,FileSelectionDialog),
	sucheInListe(value(FileNameString),CallData),
	proxtGetDefaultCharset(DCharset),
	xmStringGetLtoR(FileNameString,DCharset,FileNameCP,_),
	proxtCharPtrToString(FileNameCP,FileName),
	((midstring(FileName,_,'.qof',0) ->
	    consult_kb(FileName),
	    writeMessage(':- consult_kb('),
	    writeMessage(FileName),
	    writelnMessage(').'))|
	 (otherwise ->
	    init_kb(FileName),
	    writeMessage(':- init_kb('),
	    writeMessage(FileName),
	    writelnMessage(').'))),
	writeMessage('% file "'),
	writeMessage(FileName),
	writelnMessage('" consulted.'),
	refreshKnowledgeList(_Widget,rules,_Calldata),
	refreshKnowledgeList(_Widget,examples,_Calldata),
        updateEvaluationLabel.


%************************************************************************
%*
%* predicate: clearAllKnowledge/3 	callback procedure
%*
%* syntax: clearAllKnowledge(_Widget,_ClientData,_CallData)
%*
%* args:       _Widget			calling widget
%*	       _ClientData		no client data
%*	       _CallData		event
%*
%* description:  deletes all rules and examples	
%*
%************************************************************************

clearAllKnowledge(_Widget,_ClientData,_CallData) :-
	deleteAllKnowledge(_Widget,rules,_CallData),
	deleteAllKnowledge(_Widget,examples,_CallData),
	clearEditor(_,_,_),
	writelnMessage(':- clear_kb.'),
	writelnMessage('% knowledgebase cleared.'),
	clear_kb,
        updateEvaluationLabel.

%************************************************************************
%*
%* predicate: cancelLoadFileSelect/3 	callback procedure
%*
%* syntax: cancelLoadFileSelect(_Widget,_ClientData,_CallData)
%*
%* args:       _Widget			calling widget
%*	       _ClientData		no client data
%*	       _CallData		event
%*
%* description: Pops down the file selection dialog without loading any
%*		file.
%*
%************************************************************************

cancelLoadFileSelect(_Widget,FileSelectionDialog,_CallData) :-
	xtRemoveCallback(FileSelectionDialog,xmNokCallback,
	  loadKnowledgeEnd,FileSelectionDialog),
	xtRemoveCallback(FileSelectionDialog,xmNcancelCallback,
	  cancelLoadFileSelect,FileSelectionDialog),
	xtUnmanageChild(FileSelectionDialog).


%************************************************************************
%*
%* predicate: cancelSaveFileSelect/3 	callback procedure
%*
%* syntax: cancelSaveFileSelect(_Widget,_ClientData,_CallData)
%*
%* args:       _Widget			calling widget
%*	       _ClientData		no client data
%*	       _CallData		event
%*
%* description: Pops down the file selection dialog without saving any
%*		file.
%*
%************************************************************************

cancelSaveFileSelect(_Widget,FileSelectionDialog,_CallData) :-
	xtRemoveCallback(FileSelectionDialog,xmNokCallback,
	  saveKnowledgeEnd,FileSelectionDialog),
	xtRemoveCallback(FileSelectionDialog,xmNcancelCallback,
	  cancelSaveFileSelect,FileSelectionDialog),
	xtUnmanageChild(FileSelectionDialog).


%************************************************************************
%*
%* predicate: createKbButtons/1
%*
%* syntax: createKbButtons(+Parent)
%*
%* args: Parent			Widget
%*
%* description: Creates the push buttons 'Save', 'Load', 'Clear'. These
%*		buttons are effecting the whole knowledge base (rules and
%*	        examples).
%*
%************************************************************************

createKbButtons(Parent) :-
	xmCreateRowColumn(Parent,'ButtonRowColumn',
	  [xmNisAligned(true),xmNentryAlignment(xmALIGNMENT_CENTER),
	   xmNnumColumns(1),
	   xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN)],
	  ButtonRowColumn),
	xtManageChild(ButtonRowColumn),

	xmCreateFileSelectionDialog(ButtonRowColumn,'KBFileSelect',
	  [],KbFileSelectionDialog),

	xmCreatePushButton(ButtonRowColumn,'Load',
	  [],Load),
	xtManageChild(Load),
	xtAddCallback(Load,xmNactivateCallback,
	              loadKnowledgeBegin,KbFileSelectionDialog),

	xmCreatePushButton(ButtonRowColumn,'Save',
	  [],Save),
	xtManageChild(Save),
	xtAddCallback(Save,xmNactivateCallback,
	              saveKnowledgeBegin,KbFileSelectionDialog),

	xmCreatePushButton(ButtonRowColumn,'Clear',
	  [],Clear),
	xtManageChild(Clear),
	createYesNoPopup(Clear,_YesNoPopup,clearAllKnowledge,_).


%************************************************************************
%*
%* predicate: createKbSubButtons/2
%*
%* syntax: createKbSubButtons(+Parent,+KindOfKnowledge)
%*
%* args:        +Parent			Widget		
%*		+KindOfKnowledge	examples or rules
%*
%* description: Creates the push buttons 'SelectAll', 'Unselect All',
%* 	         'Refresh', 'Delete', 'Delete All' and 'Label' or '+', '-'
%* 	         '?'. These buttons are efecting the KindOfKnowledge
%*		 (rules and examples).
%*
%************************************************************************

createKbSubButtons(Parent,KindOfKnowledge) :-
	xmCreateRowColumn(Parent,'ButtonRowColumn',
	  [xmNisAligned(true),xmNentryAlignment(xmALIGNMENT_CENTER),
	   xmNnumColumns(2),
	   xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN)],
	  ButtonRowColumn),
	xtManageChild(ButtonRowColumn),

	xmCreatePushButton(ButtonRowColumn,'View ...',
	  [],View),
	xtManageChild(View),

	xmCreatePushButton(ButtonRowColumn,'Select All',
	  [],Select),
	xtManageChild(Select),
	xtAddCallback(Select,xmNactivateCallback,
	              selectAll,KindOfKnowledge),
	
	xmCreatePushButton(ButtonRowColumn,'Unselect All',
	  [],Unselect),
	xtManageChild(Unselect),
	xtAddCallback(Unselect,xmNactivateCallback,
	              unselectAll,KindOfKnowledge),
	
	xmCreatePushButton(ButtonRowColumn,'Refresh',
	  [],Refresh),
	xtManageChild(Refresh),
	xtAddCallback(Refresh,xmNactivateCallback,
	              refreshKnowledgeList,KindOfKnowledge),

	xmCreatePushButton(ButtonRowColumn,'Delete',
	  [],Delete),
	xtManageChild(Delete),
	xtAddCallback(Delete,xmNactivateCallback,
	              deleteKnowledge,KindOfKnowledge),

	(KindOfKnowledge = rules ->
	                   createViewRulesPopup(View),
	                   createLabelChangeButton(ButtonRowColumn) |
	 KindOfKnowledge = examples ->
	                   createViewExamplesPopup(View),
	                   createClassChangeButtons(ButtonRowColumn)).


%************************************************************************
%*
%* predicate: createLabelChangeButton/1
%*
%* syntax: createLabelChangeButton(+Parent)
%*
%* args:
%*
%* description: Creates a push button and a editing Dialog for changing
%*		 the label of the selected rules.	
%*
%************************************************************************

createLabelChangeButton(Parent) :-
	xmCreatePushButton(Parent,'Label',
	  [],Label),
	xtManageChild(Label),
	
	xmCreateBulletinBoardDialog(Label,'Label Change',
	 [],LabelChangeDialog),
	xtAddCallback(Label,xmNactivateCallback,
	              popupDialog,LabelChangeDialog),
	xmCreateFrame(LabelChangeDialog,'LabelFrame',
	  [],LabelFrame),
	xtManageChild(LabelFrame),

	xmCreateRowColumn(LabelFrame,labelChangeColumn,
	  [xmNorientation(xmVERTICAL),xmNpacking(xmPACK_COLUMN)],
	  LabelChangeColumn),
	xtManageChild(LabelChangeColumn),

	proxtGetDefaultCharset(DCharset),
	proxtStringToCharPtr(
	    'Change Label of all selected rules to:',LCP),
	xmStringCreate(LCP,DCharset,LabelStr),
	xmCreateLabelGadget(LabelChangeColumn,labelchange,
	  [xmNlabelType(xmSTRING),xmNlabelString(LabelStr)],
	  KStatus),
	xtManageChild(KStatus),
	
	xmCreateText(LabelChangeColumn,labelChangeText,
	  [xmNeditable(true),xmNeditMode(xmSINGLE_LINE_EDIT),
	   xmNwidth(150),
	   xmNautoShowCursorPosition(true)],LabelChangeText),
	xtManageChild(LabelChangeText),
	xtAddActions([action(label_ok,changeSelectedLabels,LabelChangeDialog)]),
	proxtStringToCharPtr('<Key>Return: label_ok()',TranslationString),
	xtParseTranslationTable(TranslationString,TranslationTable),
	xtOverrideTranslations(LabelChangeText,TranslationTable),

	xmCreateRowColumn(LabelChangeColumn,buttonrow,
	  [xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN),
	   xmNisAligned(true),xmNentryAlignment(xmALIGNMENT_CENTER)],
	  ButtonRow),
	xtManageChild(ButtonRow),

	xmCreatePushButton(ButtonRow,'OK!',
	  [],OK),
	xtManageChild(OK),
	xtAddCallback(OK,xmNactivateCallback,
	      changeSelectedLabels,[LabelChangeDialog,LabelChangeText]),

	xmCreatePushButton(ButtonRow,'Cancel',
	  [],Cancel),
	xtManageChild(Cancel),
	xtAddCallback(Cancel,xmNactivateCallback,
	              cancelLabelChange,LabelChangeDialog).


%************************************************************************
%*
%* predicate: createSelectRulesPopup/1
%*
%* syntax: createSelectRulesPopup(+Parent)
%*
%* args:
%*
%* description:  Creates a push button and a editing Dialog for changing
%*		 the label of the selected rules.
%*
%************************************************************************

createSelectRulesPopup(Parent) :-
	
	xmCreateBulletinBoardDialog(Parent,'SelectRules',
	 [],SelectRulesDialog),
	xtAddCallback(Parent,xmNactivateCallback,
	              popupDialog,SelectRulesDialog),
	xmCreateFrame(SelectRulesDialog,'SelectRulesFrame',
	  [],SelectRulesFrame),
	xtManageChild(SelectRulesFrame),

	xmCreateRowColumn(SelectRulesFrame,selectRulesColumn,
	  [xmNorientation(xmVERTICAL),xmNpacking(xmPACK_COLUMN)],
	  SelectRulesColumn),
	xtManageChild(SelectRulesColumn),

	proxtGetDefaultCharset(DCharset),
	proxtStringToCharPtr('Select rules:',TitleCP),
	xmStringCreate(TitleCP,DCharset,TitleStr),
	xmCreateLabelGadget(SelectRulesColumn,title,
	  [xmNlabelType(xmSTRING),xmNlabelString(TitleStr)],
	  TitleLabel),
	xtManageChild(TitleLabel),
	
	xmCreateText(SelectRulesColumn,selectRulesText,
	  [xmNeditable(true),xmNeditMode(xmSINGLE_LINE_EDIT),
	   xmNwidth(150),
	   xmNautoShowCursorPosition(true)],SelectRulesText),
	xtManageChild(SelectRulesText),
	xtAddActions([action(select_ok,selectRules,SelectRulesDialog)]),
	proxtStringToCharPtr('<Key>Return: select_ok()',TranslationString),
	xtParseTranslationTable(TranslationString,TranslationTable),
	xtOverrideTranslations(SelectRulesText,TranslationTable),

	xmCreateRowColumn(SelectRulesColumn,buttonrow,
	  [xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN),
	   xmNisAligned(true),xmNentryAlignment(xmALIGNMENT_CENTER)],
	  ButtonRow),
	xtManageChild(ButtonRow),

	xmCreatePushButton(ButtonRow,'Select',
	  [],Select),
	xtManageChild(Select),
	xtAddCallback(Select,xmNactivateCallback,
	      selectRules,SelectRulesDialog),

	xmCreatePushButton(ButtonRow,'Select All',
	  [],SelectAll),
	xtManageChild(SelectAll),
	xtAddCallback(SelectAll,xmNactivateCallback,
	      selectRules,[rules,SelectRulesDialog]),

	xmCreatePushButton(ButtonRow,'Cancel',
	  [],Cancel),
	xtManageChild(Cancel),
	xtAddCallback(Cancel,xmNactivateCallback,
	              yesNoPopdown,SelectRulesDialog).


%************************************************************************
%*
%* predicate: createExamineRulesPopup/1
%*
%* syntax: createExamineRulesPopup(+Parent)
%*
%* args:
%*
%* description: Creates a push button and a Dialog for examining
%*		the selected rules.
%*
%************************************************************************

createExamineRulesPopup(Parent) :-
	
	xmCreateBulletinBoardDialog(Parent,'ExamineRules',
	 [],ExamineRulesDialog),
	xtAddCallback(Parent,xmNactivateCallback,
	              popupExamineRules,_),
	recordz(rules,examine(dialog,ExamineRulesDialog),_),

	xmCreateFrame(ExamineRulesDialog,'ExamineRulesFrame',
	  [],ExamineRulesFrame),
	xtManageChild(ExamineRulesFrame),

	xmCreateRowColumn(ExamineRulesFrame,examineRulesColumn,
	  [xmNorientation(xmVERTICAL),xmNpacking(xmPACK_TIGHT)],
	  ExamineRulesColumn),
	xtManageChild(ExamineRulesColumn),

	xmCreateRowColumn(ExamineRulesColumn,titlerow,
	  [xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN),
	   xmNisAligned(true),xmNentryAlignment(xmALIGNMENT_CENTER)],
	  TitleRow),
	xtManageChild(TitleRow),

	proxtGetDefaultCharset(DCharset),
	proxtStringToCharPtr('Examine rule:',TitleCP),
	xmStringCreate(TitleCP,DCharset,TitleStr),
	xmCreateLabelGadget(TitleRow,title,
	  [xmNlabelType(xmSTRING),xmNlabelString(TitleStr)],
	  TitleLabel),
	xtManageChild(TitleLabel),

	xmCreateScrolledWindow(ExamineRulesColumn,ruleSW,
	  [xmNheight(200),xmNwidth(250),
	   xmNlistSizePolicy(xmCONSTANT),
	   xmNscrollBarDisplayPolicy(xmSTATIC),
	   xmNscrollingPolicy(xmAUTOMATIC)],
	  RuleSW),
	xtManageChild(RuleSW),

	xmCreateRowColumn(RuleSW,ruleRC,
	  [xmNadjustLast(false),
	   xmNorientation(xmVERTICAL),
	   xmNpacking(xmPACK_TIGHT),
	   xmNrowColumnType(xmWORK_AREA)],
	  RuleRC),
	recordz(rules,examineRC(RuleRC),_Ref),
	xtGetValues(RuleRC,[xmNbackground(B)]),
	xtGetValues(RuleSW,[xmNclipWindow(CW)]),
	xtSetValues(CW,[xmNbackground(B)]),
	xtManageChild(RuleRC),

	xmCreateRowColumn(ExamineRulesColumn,buttonrow,
	  [xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN),
	   xmNisAligned(true),xmNentryAlignment(xmALIGNMENT_CENTER)],
	  ButtonRow),
	xtManageChild(ButtonRow),

	xmCreatePushButton(ButtonRow,'Examine',
	  [],Examine),
	xtManageChild(Examine),
	xtAddCallback(Examine,xmNactivateCallback,
	      showExaminedRule,_),
	showExaminedRule(_,_,_),

	xmCreatePushButton(ButtonRow,'Cancel',
	  [],Cancel),
	xtManageChild(Cancel),
	xtAddCallback(Cancel,xmNactivateCallback,
	              cancelExamineRules,_).	




%************************************************************************
%*
%* predicate: showExaminedRule/1
%*
%* syntax: showExaminedRule(_Widget,_RuleNoText,_CallData)
%*
%* description: Displays Info on the (first) selected rule
%*
%************************************************************************

showExaminedRule(_Widget, _RuleNoText, _CallData) :-
	proxtGetDefaultCharset(DCharset),
	(recorded(rules,examineText(OldText),Ref) ->
	    erase(Ref),
	    xtDestroyWidget(OldText)|
	 otherwise),
	recorded(rules,examineRC(ExamineRC),_),

	(   ( recorded(current,clause(RuleNo,_,selected),_),
	      get_clause(RuleNo,H,B,_S,O)) ->
	    get_evaluation(RuleNo,Evaluation),
	    Evaluation = evaluation(E1,E2,E3,E4,E5,E6,E7,E8,E9),
	    ( type_restriction(H,R)
	    ; R='No type restriction found!'
	    ),
	    numbervars((H,B),0,_), %%%%Irene
	    xxmWriteToString((show_kb_clause(RuleNo,H,B,O),
	                      nl,write('type_restriction:'),
                              nl,write(R),nl),
 	                      DCharset,XmS3),
	    xxmWriteToString((nl,write('evaluation:'),nl,
		              write(E1),nl,write(E2),nl,write(E3),nl,
	                      write(E4),nl,write(E5),nl,write(E6),nl,
	                      write(E7),nl,write(E8),nl,write(E9),nl),
                              DCharset,XmS4),
	    xmStringConcat(XmS3,XmS4,XmS),

	    xmCreateLabel(ExamineRC,anyClause,
	               [xmNalignment(xmALIGNMENT_BEGINNING),
	                xmNlabelString(XmS),xmNlabelType(xmSTRING)],
	                Label)
		    
	;   atom_chars(ASno,"No rule selected!"),
	    proxtStringToCharPtr(ASno,CPno),
	    xmStringCreateLtoR(CPno,DCharset,XmSno),
	    xmCreateLabel(ExamineRC,anyClause,
	             [xmNalignment(xmALIGNMENT_BEGINNING),
	             xmNlabelString(XmSno),xmNlabelType(xmSTRING)],
	             Label)
        ),

	xtManageChild(Label),
	recordz(rules,examineText(Label),_),!.


%************************************************************************
%*
%* predicate: createViewRulesPopup/1
%*
%* syntax: createViewRulesPopup(+Parent)
%*
%* description: creates Dialog for Viewing Rules
%*
%************************************************************************

createViewRulesPopup(Parent) :-
	
	xmCreateBulletinBoardDialog(Parent,'ViewRules',
	 [],ViewRulesDialog),
	xtAddCallback(Parent,xmNactivateCallback,
	              popupViewRules,_),

	xmCreateFrame(ViewRulesDialog,'ViewRulesFrame',
	  [],ViewRulesFrame),
	xtManageChild(ViewRulesFrame),

	xmCreateRowColumn(ViewRulesFrame,viewRulesColumn,
	  [xmNorientation(xmVERTICAL),xmNpacking(xmPACK_TIGHT)],
	  ViewRulesColumn),
	xtManageChild(ViewRulesColumn),

	proxtGetDefaultCharset(DCharset),
	proxtStringToCharPtr('View rules:',TitleCP),
	xmStringCreate(TitleCP,DCharset,TitleStr),
	xmCreateLabelGadget(ViewRulesColumn,title,
	  [xmNlabelType(xmSTRING),xmNlabelString(TitleStr)],
	  TitleLabel),
	xtManageChild(TitleLabel),
	
	xmCreateFrame(ViewRulesColumn,'ViewLabelsFrame',
	  [],ViewLabelFrame),
	xtManageChild(ViewLabelFrame),

	xmCreateRowColumn(ViewLabelFrame,labelrow,
	  [xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN),
	   xmNisAligned(true),xmNentryAlignment(xmALIGNMENT_CENTER)],
	  LabelRow),
	xtManageChild(LabelRow),

	xmCreateRowColumn(LabelRow,exLabColumn,
	  [xmNorientation(xmVERTICAL),xmNpacking(xmPACK_TIGHT)],
	  ExLabColumn),
	xtManageChild(ExLabColumn),

	xmCreateRowColumn(LabelRow,viewLabColumn,
	  [xmNorientation(xmVERTICAL),xmNpacking(xmPACK_TIGHT)],
	  ViewLabColumn),
	xtManageChild(ViewLabColumn),

	proxtStringToCharPtr('existing labels',ExLabCP),
	xmStringCreate(ExLabCP,DCharset,ExLabStr),
	xmCreateLabelGadget(ExLabColumn,subtitle,
	  [xmNlabelType(xmSTRING),xmNlabelString(ExLabStr)],
	  ExLabLabel),
	xtManageChild(ExLabLabel),
	
	proxtStringToCharPtr('viewed labels',ViewLabCP),
	xmStringCreate(ViewLabCP,DCharset,ViewLabStr),
	xmCreateLabelGadget(ViewLabColumn,subtitle,
	  [xmNlabelType(xmSTRING),xmNlabelString(ViewLabStr)],
	  ViewLabLabel),
	xtManageChild(ViewLabLabel),
	
	xmCreateScrolledWindow(ExLabColumn,exLabSW,
	  [xmNheight(150),xmNwidth(140),
	   xmNlistSizePolicy(xmCONSTANT),
	   xmNscrollBarDisplayPolicy(xmSTATIC),
	   xmNscrollingPolicy(xmAUTOMATIC)],
	  ExLabSW),
	xtManageChild(ExLabSW),
	xmCreateRowColumn(ExLabSW,exLabRC,
	  [xmNadjustLast(false),
	   xmNorientation(xmVERTICAL),
	   xmNpacking(xmPACK_TIGHT),
	   xmNrowColumnType(xmWORK_AREA)],
	  ExLabRC),
	xtGetValues(ExLabRC,[xmNbackground(ExLabB)]),
	xtGetValues(ExLabSW,[xmNclipWindow(ExLabCW)]),
	xtSetValues(ExLabCW,[xmNbackground(ExLabB)]),
	xtManageChild(ExLabRC),

	xmCreateScrolledWindow(ViewLabColumn,viewedLabSW,
	  [xmNheight(150),xmNwidth(140),
	   xmNlistSizePolicy(xmCONSTANT),
	   xmNscrollBarDisplayPolicy(xmSTATIC),
	   xmNscrollingPolicy(xmAUTOMATIC)],
	  ViewLabSW),
	xtManageChild(ViewLabSW),
	xmCreateRowColumn(ViewLabSW,viewLabRC,
	  [xmNadjustLast(false),
	   xmNorientation(xmVERTICAL),
	   xmNpacking(xmPACK_TIGHT),
	   xmNrowColumnType(xmWORK_AREA)],
	  ViewLabRC),
	xtGetValues(ViewLabRC,[xmNbackground(ViewLabB)]),
	xtGetValues(ViewLabSW,[xmNclipWindow(ViewLabCW)]),
	xtSetValues(ViewLabCW,[xmNbackground(ViewLabB)]),
	xtManageChild(ViewLabRC),

	xmCreateFrame(ViewRulesColumn,'ViewClauseHeadFrame',
	  [],ViewClauseHeadFrame),
	xtManageChild(ViewClauseHeadFrame),

	xmCreateRowColumn(ViewClauseHeadFrame,clauseHeadrow,
	  [xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN),
	   xmNisAligned(true),xmNentryAlignment(xmALIGNMENT_CENTER)],
	  ClauseHeadRow),
	xtManageChild(ClauseHeadRow),

	xmCreateRowColumn(ClauseHeadRow,exCHColumn,
	  [xmNorientation(xmVERTICAL),xmNpacking(xmPACK_TIGHT)],
	  ExCHColumn),
	xtManageChild(ExCHColumn),

	xmCreateRowColumn(ClauseHeadRow,viewCHColumn,
	  [xmNorientation(xmVERTICAL),xmNpacking(xmPACK_TIGHT)],
	  ViewCHColumn),
	xtManageChild(ViewCHColumn),

	proxtStringToCharPtr('existing clause heads',ExCHCP),
	xmStringCreate(ExCHCP,DCharset,ExCHStr),
	xmCreateLabelGadget(ExCHColumn,subtitle,
	  [xmNlabelType(xmSTRING),xmNlabelString(ExCHStr)],
	  ExCHLabel),
	xtManageChild(ExCHLabel),
	
	proxtStringToCharPtr('viewed clause heads',ViewCHCP),
	xmStringCreate(ViewCHCP,DCharset,ViewCHStr),
	xmCreateLabelGadget(ViewCHColumn,subtitle,
	  [xmNlabelType(xmSTRING),xmNlabelString(ViewCHStr)],
	  ViewCHLabel),
	xtManageChild(ViewCHLabel),
	
	xmCreateScrolledWindow(ExCHColumn,existingCHSW,
	  [xmNheight(150),xmNwidth(140),
	   xmNlistSizePolicy(xmCONSTANT),
	   xmNscrollBarDisplayPolicy(xmSTATIC),
	   xmNscrollingPolicy(xmAUTOMATIC)],
	  ExCHSW),
	xtManageChild(ExCHSW),
	xmCreateRowColumn(ExCHSW,exCHRC,
	  [xmNadjustLast(false),
	   xmNorientation(xmVERTICAL),
	   xmNpacking(xmPACK_TIGHT),
	   xmNrowColumnType(xmWORK_AREA)],
	  ExCHRC),
	xtGetValues(ExCHRC,[xmNbackground(ExCHB)]),
	xtGetValues(ExCHSW,[xmNclipWindow(ExCHCW)]),
	xtSetValues(ExCHCW,[xmNbackground(ExCHB)]),
	xtManageChild(ExCHRC),

	xmCreateScrolledWindow(ViewCHColumn,viewedCHSW,
	  [xmNheight(150),xmNwidth(140),
	   xmNlistSizePolicy(xmCONSTANT),
	   xmNscrollBarDisplayPolicy(xmSTATIC),
	   xmNscrollingPolicy(xmAUTOMATIC)],
	  ViewCHSW),
	xtManageChild(ViewCHSW),
	xmCreateRowColumn(ViewCHSW,viewCHRC,
	  [xmNadjustLast(false),
	   xmNorientation(xmVERTICAL),
	   xmNpacking(xmPACK_TIGHT),
	   xmNrowColumnType(xmWORK_AREA)],
	  ViewCHRC),
	xtGetValues(ViewCHRC,[xmNbackground(ViewCHB)]),
	xtGetValues(ViewCHSW,[xmNclipWindow(ViewCHCW)]),
	xtSetValues(ViewCHCW,[xmNbackground(ViewCHB)]),
	xtManageChild(ViewCHRC),

	xmCreateRowColumn(ViewRulesColumn,minMaxRow,
	  [xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN),
	   xmNisAligned(true),xmNentryAlignment(xmALIGNMENT_CENTER)],
	  MinMaxRow),
	xtManageChild(MinMaxRow),

	proxtStringToCharPtr( 'Min: ', MinLabelChrPtr),
	xmStringCreate(MinLabelChrPtr,DCharset,MinLabelStr),
	xmCreateLabelGadget(MinMaxRow,minLabel,
	[xmNlabelType(xmSTRING),xmNlabelString(MinLabelStr)],
	MinLabel),
	xtManageChild(MinLabel),

	xmCreateRowColumn(MinMaxRow,'Min',
	[xmNadjustLast(true), xmNmarginHeight(0),
	xmNorientation(xmHORIZONTAL),
	xmNpacking(xmPACK_TIGHT)],
	MinRow),	
	xtManageChild(MinRow),

	xmCreatePushButton(MinRow,'<',
	[],Lower),
	xtManageChild(Lower),

	xmCreateText(MinRow,argumentText,
	[xmNeditable(true),xmNeditMode(xmSINGLE_LINE_EDIT),
	xmNwidth(30),
	xmNautoShowCursorPosition(true)],MinText),
	proxtStringToCharPtr('1',MinChrPtr),
	xmTextSetString(MinText,MinChrPtr),
	xtManageChild(MinText),
	recordz(rules,viewMin(MinText),_),

	xtAddCallback(Lower,xmNactivateCallback,lowerMin,MinText),
	xmCreatePushButton(MinRow,'>',
	[],Raise),
	xtManageChild(Raise),
	xtAddCallback(Raise,xmNactivateCallback,raiseMin,MinText),

	proxtStringToCharPtr( 'Max: ', MaxLabelChrPtr),
	xmStringCreate(MaxLabelChrPtr,DCharset,MaxLabelStr),
	xmCreateLabelGadget(MinMaxRow,maxLabel,
	[xmNlabelType(xmSTRING),xmNlabelString(MaxLabelStr)],
	MaxLabel),
	xtManageChild(MaxLabel),

	xmCreateRowColumn(MinMaxRow,'Max',
	[xmNadjustLast(true), xmNmarginHeight(0),
	xmNorientation(xmHORIZONTAL),
	xmNpacking(xmPACK_TIGHT)],
	MaxRow),	
	xtManageChild(MaxRow),

	xmCreatePushButton(MaxRow,'<',
	[],LowerMax),
	xtManageChild(LowerMax),

	xmCreateText(MaxRow,argumentText,
	[xmNeditable(true),xmNeditMode(xmSINGLE_LINE_EDIT),
	xmNwidth(30),
	xmNautoShowCursorPosition(true)],MaxText),
	proxtStringToCharPtr('100',MaxChrPtr),
	xmTextSetString(MaxText,MaxChrPtr),
	xtManageChild(MaxText),
	recordz(rules,viewMax(MaxText),_),

	xtAddCallback(LowerMax,xmNactivateCallback,lowerMax,MaxText),
	xmCreatePushButton(MaxRow,'>',
	[],RaiseMax),
	xtManageChild(RaiseMax),
	xtAddCallback(RaiseMax,xmNactivateCallback,raiseMax,MaxText),



	recordz(rules,view(all),_),
	recordz(rules,view(labels,[]),_),
	recordz(rules,view(clause_heads,[]),_),
	recordz(rules,view(dialog,ViewRulesDialog),_),
	recordz(rules,view(exLabRC,ExLabRC),_),
	recordz(rules,view(viewLabRC,ViewLabRC),_),
	recordz(rules,view(exCHRC,ExCHRC),_),
	recordz(rules,view(viewCHRC,ViewCHRC),_),

	xmCreateRowColumn(ViewRulesColumn,buttonrow,
	  [xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN),
	   xmNisAligned(true),xmNentryAlignment(xmALIGNMENT_CENTER)],
	  ButtonRow),
	xtManageChild(ButtonRow),

	xmCreatePushButton(ButtonRow,'View',
	  [],View),
	xtManageChild(View),
	xtAddCallback(View,xmNactivateCallback,
	      viewRules,_),

	xmCreatePushButton(ButtonRow,'View All',
	  [],ViewAll),
	xtManageChild(ViewAll),
	xtAddCallback(ViewAll,xmNactivateCallback,
	      viewAllRules,_),

	xmCreatePushButton(ButtonRow,'Cancel',
	  [],Cancel),
	xtManageChild(Cancel),
	xtAddCallback(Cancel,xmNactivateCallback,
	              cancelViewRules,_).




%************************************************************************
%*
%* predicate: createViewExamplesPopup/1
%*
%* syntax: createViewExamplesPopup(+parent)
%*
%* description: Creates creates facilities for selecting the viewed
%*		examples.	
%*
%************************************************************************

createViewExamplesPopup(Parent) :-
	
	xmCreateBulletinBoardDialog(Parent,'ViewExamples',
	 [],ViewExamplesDialog),
	xtAddCallback(Parent,xmNactivateCallback,
	              popupViewExamples,_),

	xmCreateFrame(ViewExamplesDialog,'ViewExamplesFrame',
	  [],ViewExamplesFrame),
	xtManageChild(ViewExamplesFrame),

	xmCreateRowColumn(ViewExamplesFrame,viewExamplesColumn,
	  [xmNorientation(xmVERTICAL),xmNpacking(xmPACK_TIGHT)],
	  ViewExamplesColumn),
	xtManageChild(ViewExamplesColumn),

	proxtGetDefaultCharset(DCharset),
	proxtStringToCharPtr('View examples:',TitleCP),
	xmStringCreate(TitleCP,DCharset,TitleStr),
	xmCreateLabelGadget(ViewExamplesColumn,title,
	  [xmNlabelType(xmSTRING),xmNlabelString(TitleStr)],
	  TitleLabel),
	xtManageChild(TitleLabel),

	xmCreateFrame(ViewExamplesColumn,'ViewClauseHeadFrame',
	  [],ViewClauseHeadFrame),
	xtManageChild(ViewClauseHeadFrame),

	xmCreateRowColumn(ViewClauseHeadFrame,clauseHeadrow,
	  [xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN),
	   xmNisAligned(true),xmNentryAlignment(xmALIGNMENT_CENTER)],
	  ClauseHeadRow),
	xtManageChild(ClauseHeadRow),

	xmCreateRowColumn(ClauseHeadRow,exCHColumn,
	  [xmNorientation(xmVERTICAL),xmNpacking(xmPACK_TIGHT)],
	  ExCHColumn),
	xtManageChild(ExCHColumn),

	xmCreateRowColumn(ClauseHeadRow,viewCHColumn,
	  [xmNorientation(xmVERTICAL),xmNpacking(xmPACK_TIGHT)],
	  ViewCHColumn),
	xtManageChild(ViewCHColumn),

	proxtStringToCharPtr('existing clause heads',ExCHCP),
	xmStringCreate(ExCHCP,DCharset,ExCHStr),
	xmCreateLabelGadget(ExCHColumn,subtitle,
	  [xmNlabelType(xmSTRING),xmNlabelString(ExCHStr)],
	  ExCHLabel),
	xtManageChild(ExCHLabel),
	
	proxtStringToCharPtr('viewed clause heads',ViewCHCP),
	xmStringCreate(ViewCHCP,DCharset,ViewCHStr),
	xmCreateLabelGadget(ViewCHColumn,subtitle,
	  [xmNlabelType(xmSTRING),xmNlabelString(ViewCHStr)],
	  ViewCHLabel),
	xtManageChild(ViewCHLabel),

	xmCreateScrolledWindow(ExCHColumn,existingCHSW,
	  [xmNheight(150),xmNwidth(140),
	   xmNlistSizePolicy(xmCONSTANT),
	   xmNscrollBarDisplayPolicy(xmSTATIC),
	   xmNscrollingPolicy(xmAUTOMATIC)],
	  ExCHSW),
	xtManageChild(ExCHSW),
	xmCreateRowColumn(ExCHSW,exCHRC,
	  [xmNadjustLast(false),
	   xmNorientation(xmVERTICAL),
	   xmNpacking(xmPACK_TIGHT),
	   xmNrowColumnType(xmWORK_AREA)],
	  ExCHRC),
	xtGetValues(ExCHRC,[xmNbackground(ExCHB)]),
	xtGetValues(ExCHSW,[xmNclipWindow(ExCHCW)]),
	xtSetValues(ExCHCW,[xmNbackground(ExCHB)]),
	xtManageChild(ExCHRC),

	xmCreateScrolledWindow(ViewCHColumn,viewedCHSW,
	  [xmNheight(150),xmNwidth(140),
	   xmNlistSizePolicy(xmCONSTANT),
	   xmNscrollBarDisplayPolicy(xmSTATIC),
	   xmNscrollingPolicy(xmAUTOMATIC)],
	  ViewCHSW),
	xtManageChild(ViewCHSW),
	xmCreateRowColumn(ViewCHSW,viewCHRC,
	  [xmNadjustLast(false),
	   xmNorientation(xmVERTICAL),
	   xmNpacking(xmPACK_TIGHT),
	   xmNrowColumnType(xmWORK_AREA)],
	  ViewCHRC),
	xtGetValues(ViewCHRC,[xmNbackground(ViewCHB)]),
	xtGetValues(ViewCHSW,[xmNclipWindow(ViewCHCW)]),
	xtSetValues(ViewCHCW,[xmNbackground(ViewCHB)]),
	xtManageChild(ViewCHRC),

	xmCreateRowColumn(ViewExamplesColumn,minMaxRow,
	  [xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN),
	   xmNisAligned(true),xmNentryAlignment(xmALIGNMENT_CENTER)],
	  MinMaxRow),
	xtManageChild(MinMaxRow),

	proxtStringToCharPtr( 'Min: ', MinLabelChrPtr),
	xmStringCreate(MinLabelChrPtr,DCharset,MinLabelStr),
	xmCreateLabelGadget(MinMaxRow,minLabel,
	[xmNlabelType(xmSTRING),xmNlabelString(MinLabelStr)],
	MinLabel),
	xtManageChild(MinLabel),

	xmCreateRowColumn(MinMaxRow,'Min',
	[xmNadjustLast(true), xmNmarginHeight(0),
	xmNorientation(xmHORIZONTAL),
	xmNpacking(xmPACK_TIGHT)],
	MinRow),	
	xtManageChild(MinRow),

	xmCreatePushButton(MinRow,'<',
	[],Lower),
	xtManageChild(Lower),

	xmCreateText(MinRow,argumentText,
	[xmNeditable(true),xmNeditMode(xmSINGLE_LINE_EDIT),
	xmNwidth(30),
	xmNautoShowCursorPosition(true)],MinText),
	proxtStringToCharPtr('1',MinChrPtr),
	xmTextSetString(MinText,MinChrPtr),
	xtManageChild(MinText),
	recordz(examples,viewMin(MinText),_),

	xtAddCallback(Lower,xmNactivateCallback,lowerMin,MinText),
	xmCreatePushButton(MinRow,'>',
	[],Raise),
	xtManageChild(Raise),
	xtAddCallback(Raise,xmNactivateCallback,raiseMin,MinText),

	proxtStringToCharPtr( 'Max: ', MaxLabelChrPtr),
	xmStringCreate(MaxLabelChrPtr,DCharset,MaxLabelStr),
	xmCreateLabelGadget(MinMaxRow,maxLabel,
	[xmNlabelType(xmSTRING),xmNlabelString(MaxLabelStr)],
	MaxLabel),
	xtManageChild(MaxLabel),

	xmCreateRowColumn(MinMaxRow,'Max',
	[xmNadjustLast(true), xmNmarginHeight(0),
	xmNorientation(xmHORIZONTAL),
	xmNpacking(xmPACK_TIGHT)],
	MaxRow),	
	xtManageChild(MaxRow),

	xmCreatePushButton(MaxRow,'<',
	[],LowerMax),
	xtManageChild(LowerMax),

	xmCreateText(MaxRow,argumentText,
	[xmNeditable(true),xmNeditMode(xmSINGLE_LINE_EDIT),
	xmNwidth(30),
	xmNautoShowCursorPosition(true)],MaxText),
	proxtStringToCharPtr('100',MaxChrPtr),
	xmTextSetString(MaxText,MaxChrPtr),
	xtManageChild(MaxText),
	recordz(examples,viewMax(MaxText),_),

	xtAddCallback(LowerMax,xmNactivateCallback,lowerMax,MaxText),
	xmCreatePushButton(MaxRow,'>',
	[],RaiseMax),
	xtManageChild(RaiseMax),
	xtAddCallback(RaiseMax,xmNactivateCallback,raiseMax,MaxText),


	recordz(examples,view(all),_),
	recordz(examples,view(clause_heads,[]),_),
	recordz(examples,view(dialog,ViewExamplesDialog),_),
	recordz(examples,view(exCHRC,ExCHRC),_),
	recordz(examples,view(viewCHRC,ViewCHRC),_),

	xmCreateRowColumn(ViewExamplesColumn,buttonrow,
	  [xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN),
	   xmNisAligned(true),xmNentryAlignment(xmALIGNMENT_CENTER)],
	  ButtonRow),
	xtManageChild(ButtonRow),

	xmCreatePushButton(ButtonRow,'View',
	  [],View),
	xtManageChild(View),
	xtAddCallback(View,xmNactivateCallback,
	      viewExamples,_),

	xmCreatePushButton(ButtonRow,'View All',
	  [],ViewAll),
	xtManageChild(ViewAll),
	xtAddCallback(ViewAll,xmNactivateCallback,
	      viewAllExamples,_),

	xmCreatePushButton(ButtonRow,'Cancel',
	  [],Cancel),
	xtManageChild(Cancel),
	xtAddCallback(Cancel,xmNactivateCallback,
	              cancelViewExamples,_).


%************************************************************************
%*
%* predicate: changeSelectedLabels/1 		callback procedure
%*
%* syntax: changeSelectedLabels(_Widget,+[LabelChangeDialog,LabelText],_CallData)
%*
%* args:       _Widget				calling widget
%*	       +[LabelChangeDialog,LabelText]	widgets		
%*	       _CallData			event	
%*
%* description: Removes the LabelChange Dialog from the display, and
%*		 changes the Label of all selected rules.	
%*
%************************************************************************

changeSelectedLabels(_Widget,[LabelChangeDialog,LabelText],_CallData) :-
	xtUnmanageChild(LabelChangeDialog),
	xmTextGetString(LabelText,LabelCP),
	proxtCharPtrToString(LabelCP,LabelAS),
	setSelectedLabels(LabelAS).


%************************************************************************
%*
%* predicate: setSelectedLabels/1
%*
%* syntax: setSelectedLabels(+Label)
%*
%* args: +Label			new labelname
%*
%* description: Sets the label of all selected rules to Label.	
%*
%************************************************************************

setSelectedLabels(Label) :-
	  recorded(current,clause(Id,Widget,selected),Ref),
	  erase(Ref),
	  get_clause(Id,H,B,CL,_),
	  delete_clause(Id),
	  store_clause(_,CL,Label,Id),
	  setSelectedLabels(Label),
	  recorda(current,clause(Id,Widget,selected),_),
	  addRuleItem(Id,H,B,CL,Label),
          updateEvaluationLabel.
setSelectedLabels(_) :- !.


%************************************************************************
%*
%* predicate: cancelLabelChange/3	callback procedure
%*
%* syntax: cancelLabelChange(_Widget,+LabelChangeDialog)
%*
%* args:       _Widget			calling widget
%*	       +LabelChangeDialog	widget		
%*	       _CallData		event	
%*
%* description: Removes the LabelChangeDialog from the display.
%*
%************************************************************************

cancelLabelChange(_Widget,LabelChangeDialog,_CallData) :-
	xtUnmanageChild(LabelChangeDialog).


%************************************************************************
%*
%* predicate: createClassChangeButtons/1
%*
%* syntax: createClassChangeButtons(+Parent)
%*
%* description: Creates three buttons '+', '-', '?' for changing the	
%*		class of the selected examples.	
%*
%************************************************************************

createClassChangeButtons(Parent) :-
	xmCreateRowColumn(Parent,'Class Change',
	[xmNadjustLast(true),xmNmarginHeight(0),
	xmNorientation(xmHORIZONTAL),
	xmNpacking(xmPACK_COLUMN),
	xmNrowColumnType(xmWORK_AREA)],
	ClassChangeRow),	
	xtManageChild(ClassChangeRow),

	xmCreatePushButton(ClassChangeRow,'+',
	[],Positive),
	xtManageChild(Positive),
	xtAddCallback(Positive,xmNactivateCallback,
	classChangeSelected,'+'),

	xmCreatePushButton(ClassChangeRow,'-',
	[],Negative),
	xtManageChild(Negative),
	xtAddCallback(Negative,xmNactivateCallback,
	classChangeSelected,'-'),

	xmCreatePushButton(ClassChangeRow,'?',
	[],Quest),
	xtManageChild(Quest),
	xtAddCallback(Quest,xmNactivateCallback,
	classChangeSelected,'?').


%************************************************************************
%*
%* predicate: classChangeSelected/3		Callback procedure
%*
%* syntax: classChangeSelected(_Widget,+Classification,_CallData)
%*
%* args:       _Widget			calling widget	
%*	       +Classification		'+' or '-' or '?'
%*	       _CallData		event	
%*
%* description: Sets the classification of the selected examples to 
%*		 Classification.
%*
%************************************************************************

classChangeSelected(_Widget,Classification,_CallData) :-
	recorded(current,example(Id,Label,selected),Ref),
	erase(Ref),
	get_example(Id,Fact,_),
	delete_example(Id),
	store_ex(Fact,Classification,Id),
	classChangeSelected(_Widget,Classification,_CallData),
	recorda(current,example(Id,Label,selected),_),
	addExampleItem(Id,Fact,Classification),
        updateEvaluationLabel.
classChangeSelected(_,_,_) :- !.


%************************************************************************
%*
%* predicate: createArgumentArea/1
%*
%* syntax: createArgumentArea(+Parent)
%*
%* description: Creates the argument area with five fields for arguments,
%*		one toggle button for copying rule ids into argument	 
%*		fields when selecting or deselecting rules or examples, 
%*		and a clear push button.
%*
%************************************************************************

createArgumentArea(Parent) :-
	xmCreateFrame(Parent,'Arguments',
	[],ArgumentFrame),
	xtManageChild(ArgumentFrame),

	xmCreateRowColumn(ArgumentFrame,'Arguments',
	[xmNwidth(300),xmNadjustLast(true),
	xmNorientation(xmVERTICAL),
	xmNpacking(xmPACK_TIGHT),
	xmNrowColumnType(xmWORK_AREA)],
	ArgumentColumn),	
	xtManageChild(ArgumentColumn),

	proxtStringToCharPtr(
	'Arguments',
	ArgumentChrPtr),
	proxtGetDefaultCharset(DCharset),
	xmStringCreate(ArgumentChrPtr,DCharset,ArgumentStr),
	xmCreateLabelGadget(ArgumentColumn,argumentLabel,
	[xmNlabelType(xmSTRING),xmNlabelString(ArgumentStr)],
	ArgumentLabel),
	xtManageChild(ArgumentLabel),

	xmCreateRowColumn(ArgumentColumn,kommandoRC,
	[xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN),
	xmNnumColumns(2)],
	ArgumentRow),
	xtManageChild(ArgumentRow),

	createArguments(ArgumentRow,12),

	xmCreateRowColumn(ArgumentColumn,buttonrow,
	[xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_TIGHT),
	xmNisAligned(true),xmNentryAlignment(xmALIGNMENT_CENTER)],
	ButtonRow),
	xtManageChild(ButtonRow),

	xmCreateToggleButton(ButtonRow,'Get Id       ',
	[],GetId),
	xtManageChild(GetId),
	recordz(state,getId(GetId),_),

	xmCreatePushButton(ButtonRow,'Clear',
	[],Clear),
	xtManageChild(Clear),
	xtAddCallback(Clear,xmNactivateCallback,clearArguments,12),

	proxtStringToCharPtr( '         Depth: ', DepthChrPtr),
	xmStringCreate(DepthChrPtr,DCharset,DepthStr),
	xmCreateLabelGadget(ButtonRow,depthLabel,
	[xmNlabelType(xmSTRING),xmNlabelString(DepthStr)],
	DepthLabel),
	xtManageChild(DepthLabel),

	xmCreateRowColumn(ButtonRow,'Depth',
	[xmNadjustLast(true), xmNmarginHeight(0),
	xmNorientation(xmHORIZONTAL),
	xmNpacking(xmPACK_TIGHT)],
	DepthRow),	
	xtManageChild(DepthRow),

	xmCreatePushButton(DepthRow,'<',
	[],Lower),
	xtManageChild(Lower),

	xmCreateText(DepthRow,argumentText,
	[xmNeditable(true),xmNeditMode(xmSINGLE_LINE_EDIT),
	xmNwidth(30),
	xmNautoShowCursorPosition(true)],DText),
	proxtStringToCharPtr('5',DChrPtr),
	xmTextSetString(DText,DChrPtr),
	xtManageChild(DText),
	recordz(state,argumentWidget(depth,DText),_),

	xtAddCallback(Lower,xmNactivateCallback,lowerDepth,DText),
	xmCreatePushButton(DepthRow,'>',
	[],Raise),
	xtManageChild(Raise),
	xtAddCallback(Raise,xmNactivateCallback,raiseDepth,DText).


%************************************************************************
%*
%* predicate: createArguments/2
%*
%* syntax: createArguments(+Parent,+Count)
%*
%* args:       +Parent		parent widget	
%*	       +Count		number of argument fields to create	
%*
%* description: Creates Count text widgets for the arguments.
%*
%************************************************************************

createArguments(_,0) :- !.
createArguments(Parent,Count) :-
	C1 is Count - 1,
	createArguments(Parent,C1),

	xmCreateText(Parent,argumentText,
	[xmNeditable(true),xmNeditMode(xmSINGLE_LINE_EDIT),
	xmNwidth(50),
	xmNautoShowCursorPosition(true)],ArgumentText),
	xtManageChild(ArgumentText),
	recordz(state,argumentWidget(Count,ArgumentText),_).


%************************************************************************
%*
%* predicate: clearArguments/3		callback procedure
%*
%* syntax: clearArguments(_Widget,+Count,_CallData)
%*
%* args:       _Widget		calling widget	
%*	       Count		Number of arguments to clear (5)	
%*	       _CallData	
%*
%* description: Clears Count argument text widgets
%*
%************************************************************************

clearArguments(_Widget,0,_CallData).
clearArguments(_Widget,Count,_CallData) :-
	C1 is Count - 1,
	clearArguments(_Widget,C1,_CallData),
	recorded(state,argumentWidget(Count,Text),_),
	proxtStringToCharPtr('',EmptyChrPtr),
	xmTextSetString(Text,EmptyChrPtr).


%************************************************************************
%*
%* predicate: lowerDepth/3		callback procedure	
%*
%* syntax:
%*
%* args:       _Widget		calling widget	
%*	       +DepthText	TextWidget of Depth	
%*	       _CallData	
%*
%* description: 1 decrements xmDepth.
%*
%* see also: raiseDepth	
%*
%************************************************************************

lowerDepth(_Widget,DepthText,_CallData) :-
	xmTextGetString(DepthText,OldDepthCP),
	proxtCharPtrToString(OldDepthCP,OldDepthStr),
	atom_chars(OldDepthStr,OldDepthC),
	number_chars(OldDepth,OldDepthC),
	(OldDepth = 0 -> NewDepth is 100|
    otherwise -> NewDepth is OldDepth - 1),
	number_chars(NewDepth,NewDepthC),
	atom_chars(NewDepthStr,NewDepthC),
	proxtStringToCharPtr(NewDepthStr,NewDepthCP),
	xmTextSetString(DepthText,NewDepthCP).


%************************************************************************
%*
%* predicate: lowerID/3		callback procedure
%*
%* syntax:
%*
%* args:       _Widget		calling widget	
%*	       +DepthText	TextWidget containing ID
%*	       _CallData	
%*
%* description:  1 decrements ID. 
%*
%* see also:  raiseID
%*
%************************************************************************

lowerID(_Widget,IDText,_CallData) :-
	xmTextGetString(IDText,OldIDCP),
	proxtCharPtrToString(OldIDCP,OldIDStr),
	atom_chars(OldIDStr,OldIDC),
	number_chars(OldID,OldIDC),
	(OldID = 0 -> NewID is 9999|
    otherwise -> NewID is OldID - 1),
	number_chars(NewID,NewIDC),
	atom_chars(NewIDStr,NewIDC),
	proxtStringToCharPtr(NewIDStr,NewIDCP),
	xmTextSetString(IDText,NewIDCP).


%************************************************************************
%*
%* predicate: lowerMin/3		callback procedure
%*
%* syntax:
%*
%* args:       _Widget		calling widget	
%*	       +DepthText	TextWidget of Depth	
%*	       _CallData
%*
%* description: 1 decrements MinId of viewed rules or examples.
%*
%************************************************************************

lowerMin(_Widget,MinText,_CallData) :-
	(recorded(rules,viewMin(MinText),_) -> KindOfKnowledge = rules|
	 otherwise -> KindOfKnowledge = examples),
	xmTextGetString(MinText,OldMinCP),
	proxtCharPtrToString(OldMinCP,OldMinStr),
	atom_chars(OldMinStr,OldMinC),
	number_chars(OldMin,OldMinC),
	recorded(KindOfKnowledge,viewMax(MaxText),_),
	xmTextGetString(MaxText,MaxCP),
	proxtCharPtrToString(MaxCP,MaxStr),
	atom_chars(MaxStr,MaxC),
	number_chars(Max,MaxC),

	(OldMin =< 1 -> NewMin is Max|
    otherwise -> NewMin is OldMin - 1),
	number_chars(NewMin,NewMinC),
	atom_chars(NewMinStr,NewMinC),
	proxtStringToCharPtr(NewMinStr,NewMinCP),
	xmTextSetString(MinText,NewMinCP).


%************************************************************************
%*
%* predicate: lowerMax/3		callback procedure
%*
%* syntax:
%*
%* args:       _Widget		calling widget
%*	       +DepthText	TextWidget of Depth
%*	       _CallData	
%*
%* description: 1 decrements MaxId of viewed rules or examples.
%*
%************************************************************************

lowerMax(_Widget,MaxText,_CallData) :-
	(recorded(rules,viewMin(MinText),_) -> KindOfKnowledge = rules|
	 otherwise -> KindOfKnowledge = examples),
	xmTextGetString(MaxText,OldMaxCP),
	proxtCharPtrToString(OldMaxCP,OldMaxStr),
	atom_chars(OldMaxStr,OldMaxC),
	number_chars(OldMax,OldMaxC),
	recorded(KindOfKnowledge,viewMin(MinText),_),
	xmTextGetString(MinText,MinCP),
	proxtCharPtrToString(MinCP,MinStr),
	atom_chars(MinStr,MinC),
	number_chars(Min,MinC),

	(OldMax =< Min -> NewMax is 9999|
    otherwise -> NewMax is (OldMax - 1)),
	number_chars(NewMax,NewMaxC),
	atom_chars(NewMaxStr,NewMaxC),
	proxtStringToCharPtr(NewMaxStr,NewMaxCP),
	xmTextSetString(MaxText,NewMaxCP).


%************************************************************************
%*
%* predicate: raiseDepth/3		callback procedure	
%*
%* syntax:
%*
%* args:       _Widget		calling widget
%*	       +DepthText	TextWidget of Depth
%*	       _CallData	
%*
%* description: 1 inrements xmDepth.
%*
%************************************************************************

raiseDepth(_Widget,DepthText,_CallData) :-
	xmTextGetString(DepthText,OldDepthCP),
	proxtCharPtrToString(OldDepthCP,OldDepthStr),
	atom_chars(OldDepthStr,OldDepthC),
	number_chars(OldDepth,OldDepthC),
	(OldDepth = 100 -> NewDepth is 0|
    otherwise -> NewDepth is OldDepth + 1),
	number_chars(NewDepth,NewDepthC),
	atom_chars(NewDepthStr,NewDepthC),
	proxtStringToCharPtr(NewDepthStr,NewDepthCP),
	xmTextSetString(DepthText,NewDepthCP).


%************************************************************************
%*
%* predicate: raiseID/3		callback procedure	
%*
%* syntax:
%*
%* args:       _Widget		calling widget
%*	       +DepthText	TextWidget containing ID	
%*	       _CallData	
%*
%* description: 1 increments ID. 
%*
%************************************************************************

raiseID(_Widget,IDText,_CallData) :-
	xmTextGetString(IDText,OldIDCP),
	proxtCharPtrToString(OldIDCP,OldIDStr),
	atom_chars(OldIDStr,OldIDC),
	number_chars(OldID,OldIDC),
	(OldID = 9999 -> NewID is 0|
    otherwise -> NewID is OldID + 1),
	number_chars(NewID,NewIDC),
	atom_chars(NewIDStr,NewIDC),
	proxtStringToCharPtr(NewIDStr,NewIDCP),
	xmTextSetString(IDText,NewIDCP).

%************************************************************************
%*
%* predicate: raiseMin/3		callback procedure	
%*
%* syntax:
%*
%* args:       _Widget		calling widget
%*	       +DepthText	TextWidget of Depth	
%*	       _CallData	
%*
%* description: 1 increments MinId of viewed rules or examples.
%*
%************************************************************************

raiseMin(_Widget,MinText,_CallData) :-
	(recorded(rules,viewMin(MinText),_) -> KindOfKnowledge = rules|
	 otherwise -> KindOfKnowledge = examples),
	xmTextGetString(MinText,OldMinCP),
	proxtCharPtrToString(OldMinCP,OldMinStr),
	atom_chars(OldMinStr,OldMinC),
	number_chars(OldMin,OldMinC),
	recorded(KindOfKnowledge,viewMax(MaxText),_),
	xmTextGetString(MaxText,MaxCP),
	proxtCharPtrToString(MaxCP,MaxStr),
	atom_chars(MaxStr,MaxC),
	number_chars(Max,MaxC),

	(OldMin >= Max -> NewMin is 1|
    otherwise -> NewMin is (OldMin + 1)),
	number_chars(NewMin,NewMinC),
	atom_chars(NewMinStr,NewMinC),
	proxtStringToCharPtr(NewMinStr,NewMinCP),
	xmTextSetString(MinText,NewMinCP).



%************************************************************************
%*
%* predicate: raiseMax/3		callback procedure	
%*
%* syntax:
%*
%* args:       _Widget		calling widget
%*	       +DepthText	TextWidget of Depth	
%*	       _CallData	
%*
%* description: 1 increments MaxId of viewed rules or examples.
%*
%************************************************************************

raiseMax(_Widget,MaxText,_CallData) :-
	(recorded(rules,viewMin(MinText),_) -> KindOfKnowledge = rules|
	 otherwise -> KindOfKnowledge = examples),
	xmTextGetString(MaxText,OldMaxCP),
	proxtCharPtrToString(OldMaxCP,OldMaxStr),
	atom_chars(OldMaxStr,OldMaxC),
	number_chars(OldMax,OldMaxC),
	recorded(KindOfKnowledge,viewMin(MinText),_),
	xmTextGetString(MinText,MinCP),
	proxtCharPtrToString(MinCP,MinStr),
	atom_chars(MinStr,MinC),
	number_chars(Min,MinC),

	(OldMax >= 9999 -> NewMax is Min|
    otherwise -> NewMax is (OldMax + 1)),
	number_chars(NewMax,NewMaxC),
	atom_chars(NewMaxStr,NewMaxC),
	proxtStringToCharPtr(NewMaxStr,NewMaxCP),
	xmTextSetString(MaxText,NewMaxCP).


%************************************************************************
%*
%* predicate: copyId/2
%*
%* syntax: copyId(+Id,+Count)
%*
%* args:       +Id		Id of rule or example to copy
%*	       +Count		First argument to proove if empty	
%*
%*
%* description: Copies the specified rule or example to the textwidget
%*		of the count argument.
%*
%************************************************************************

copyId(_,13) :- !.
copyId(Id,Count) :-
	recorded(state,argumentWidget(Count,Text),_),
	xmTextGetLastPosition(Text,Length),
	Length = 0, !,
	number_chars(Id,IdChars),
	atom_chars(IdStr,IdChars),
	proxtStringToCharPtr(IdStr,IdCharPtr),
	xmTextSetString(Text,IdCharPtr).
copyId(Id,Count) :-
	C1 is Count + 1,
	copyId(Id,C1).


%************************************************************************
%*
%* predicate: createCommandArea/1
%*
%* syntax: createCommandArea(+Parent)
%*
%* description: Creates the area for direct command input.	
%*
%************************************************************************

createCommandArea(Parent) :-
	xmCreateFrame(Parent,'Controll',
	[],ControllFrame),
	xtManageChild(ControllFrame),

	xmCreateRowColumn(ControllFrame,controll,
	[xmNwidth(300),xmNadjustLast(true),
	xmNorientation(xmVERTICAL),
	xmNpacking(xmPACK_TIGHT),
	xmNrowColumnType(xmWORK_AREA)],
	ControllRowColumn),	
	xtManageChild(ControllRowColumn),

	xmCreateRowColumn(ControllRowColumn,kommandoRC,
	[xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN)],
	KommandoRowColumn),
	xtManageChild(KommandoRowColumn),

	proxtStringToCharPtr('Comand:',KommandoChrPtr),
	proxtGetDefaultCharset(DCharset),
	xmStringCreate(KommandoChrPtr,DCharset,KommandoStr),
	xmCreateLabelGadget(KommandoRowColumn,kommandoLabel,
	[xmNlabelType(xmSTRING),xmNlabelString(KommandoStr)],
	Klabel),
	xtManageChild(Klabel),

	proxtStringToCharPtr('          Input',StatusCP),
	xmStringCreate(StatusCP,DCharset,StatusStr),
	xmCreateLabelGadget(KommandoRowColumn,kommandoStatus,
	[xmNlabelType(xmSTRING),xmNlabelString(StatusStr)],
	KStatus),
	xtManageChild(KStatus),
	
	xmCreateText(ControllRowColumn,kommandotext,
	[xmNeditable(true), xmNeditMode(xmSINGLE_LINE_EDIT),
	xmNwidth(290),
	xmNautoShowCursorPosition(true)],Ktext),
	xtManageChild(Ktext),
	xtAddActions([action(ok,doKommando,[Ktext,KStatus])]),
	proxtStringToCharPtr('<Key>Return: ok()',TranslationString),
	xtParseTranslationTable(TranslationString,TranslationTable),
	xtOverrideTranslations(Ktext,TranslationTable),

	xmCreateRowColumn(ControllRowColumn,buttonrow,
	[xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN),
	xmNisAligned(true),xmNentryAlignment(xmALIGNMENT_CENTER)],
	ButtonRow),
	xtManageChild(ButtonRow),

	xmCreatePushButton(ButtonRow,'OK!',
	[],OK),
	xtManageChild(OK),

	xmCreatePushButton(ButtonRow,'Clear',
	[],Clear),
	xtManageChild(Clear),

	xmCreatePushButton(ButtonRow,'Quit X-Miles',
	[],Beenden),
	createYesNoPopup(Beenden,_YesNoPopup,beenden,_),
	xtManageChild(Beenden),

	xtSetValues(ControllRowColumn,[xmNdefaultButton(OK)]),
	xtAddCallback(OK,xmNactivateCallback,doKommando,[Ktext,KStatus]),
	xtAddCallback(Clear,xmNactivateCallback,doEmptyKommando,Ktext).


%************************************************************************
%*
%* predicate: createFunctionArea/1
%*
%* syntax: createFunctionArea(+Parent)
%*
%* description: Creates the area for the pulldown menus for calling the
%*		learning functions of miles. 
%*
%* see also: file: 'xmiles_functions.pl'	
%*
%************************************************************************

createFunctionArea(Parent) :-
	xmCreateFrame(Parent,'Functions',
	[],FunctionFrame),
	xtManageChild(FunctionFrame),

	xmCreateRowColumn(FunctionFrame,'Function',
	[xmNwidth(300),xmNadjustLast(true),
	xmNorientation(xmVERTICAL),
	xmNpacking(xmPACK_TIGHT),
	xmNrowColumnType(xmWORK_AREA)],
	FunctionColumn),	
	xtManageChild(FunctionColumn),

	proxtStringToCharPtr('Learning Operators',FunctionChrPtr),
	proxtGetDefaultCharset(DCharset),
	xmStringCreate(FunctionChrPtr,DCharset,FunctionStr),
	xmCreateLabelGadget(FunctionColumn,functionLabel,
	[xmNlabelType(xmSTRING),xmNlabelString(FunctionStr)],
	FunctionLabel),
	xtManageChild(FunctionLabel),

	xmCreateRowColumn(FunctionColumn,functionRC,
	[xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN),
	xmNnumColumns(2),
	xmNisAligned(true),xmNentryAlignment(xmALIGNMENT_CENTER)],
	FunctionRowColumn),
	xtManageChild(FunctionRowColumn),

	groups(ListOfGroups),
	createFunctionGroups(FunctionRowColumn,ListOfGroups).


%************************************************************************
%*
%* predicate: createFunctionGroups/2
%*
%* syntax: createFunctionGroups(+Parent,+[Groupname|ListOfGroups])
%*
%* args:       +Parent				parent widget
%*	       +[Groupname|ListOfGroups]	list of groupnames
%*
%* description: Creates one pull down menu for every group of learning
%*		functions.
%*
%************************************************************************

createFunctionGroups(_,[]).
createFunctionGroups(Parent,[Groupname|ListOfGroups]) :-
	xmCreatePushButton(Parent,Groupname,
	  [],ButtonWidget),
	xtManageChild(ButtonWidget),

	xmCreatePopupMenu(ButtonWidget,'Learning functions',
	  [xmNorientation(xmVERTICAL),
	   xmNpacking(xmPACK_COLUMN),
	   xmNisAligned(true),xmNentryAlignment(xmALIGNMENT_CENTER)], 
	  LearningFunctions),

	xtAddEventHandler(LearningFunctions,[buttonReleaseMask],false,
	                  functionsPopdown,_),
	xtAddCallback(ButtonWidget,xmNactivateCallback,
	              popupFunctions,LearningFunctions),

	groupdef(Groupname,ListOfButtons),
	createFunctionButtons(LearningFunctions,ListOfButtons),
	createFunctionGroups(Parent,ListOfGroups).


%************************************************************************
%*
%* predicate: createFunctionButtons/2
%*
%* syntax: createFunctionButtons(+Parent,+[Button|ListOfButtons])
%*
%* args:       +Parent			parent widget
%*	       +[Button|ListOfButtons]	list of buttonnames	
%*
%* description: Creates one pushbutton for every button name in the second
%*		argument.	
%*
%************************************************************************

createFunctionButtons(_,[]).
createFunctionButtons(Parent,[Button|ListOfButtons]) :-
	xmCreatePushButton(Parent,Button,
	  [],ButtonWidget),
	xtManageChild(ButtonWidget),
	xtAddCallback(ButtonWidget,xmNactivateCallback,
	              callFunction,Button),
	createFunctionButtons(Parent,ListOfButtons).


%************************************************************************
%*
%* predicate: callFunction/3		callback procedure	
%*
%* syntax:
%*
%* args:       _Widget		calling widget	
%*	       +Buttonname	determines the function	
%*	       _CallData	event
%*
%* description: calls the function defined for the specified button
%*		in the file xmiles_functions.pl	
%*
%************************************************************************

callFunction(_Widget,Buttonname,_CallData) :-
	recorded(learnfuncs,popedup(PopupShell),Ref),
	erase(Ref),
	xtUnmanageChild(PopupShell),
	operatordef(Buttonname,Funcname,InOutPattern,InChecks,XOuts,Refresh),
	writeMessage(':- '),writeMessage(Funcname),
	(InOutPattern=[] -> writelnMessage('.'),
	    Fails=[], Args=[]|
	 otherwise -> writeMessage('('),
	    replaceXmVars(InOutPattern,InChecks,Args,Fails)),
	flushErrorBuffer,
	(Fails=[] ->
	    F =.. [Funcname|Args],
	    ((call(F) -> writelnMessage('% yes'),
	                 outVars(Args,InOutPattern,XOuts),
			 refresh(Refresh))|
	      otherwise -> writelnMessage('% no'))|
	 otherwise -> 
           writelnMessage('% Error occured, function not executed!')),
         updateEvaluationLabel.


%************************************************************************
%*
%* predicate: replaceXmVars/3
%*
%* syntax:
%*
%* args:      +[Pat|InOutPattern]	List of argument patterns
%*	       +InChecks		List of checking functions
%*	       -Args			arguments the learning function is
%*					to be called with	
%*
%* description: replaces the InOutPattern through constant values for
%*		input arguments and unbound variables for output arguments
%*
%************************************************************************

replaceXmVars([],_,[],[]):- !, writelnMessage(').').
replaceXmVars([Pat|InOutPattern],InChecks,Args,Fails) :-
	(atom(Pat) -> atom_chars(Pat,PatC)|
	 otherwise -> PatC = "constant"),
	append("xmarg",[M1],Argstr),
	append("xmarg",[M3,M4],Argstr2),
	append("xmopt",[M2],Optstr),
	append("xmopt",[M5,M6],Optstr2),
	append("xmout",[_],Outstr),
	(PatC = "xmdepth" -> recorded(state,argumentWidget(depth,Text),_),
			  xmTextGetString(Text,ArgCP),
			  proxtCharPtrToString(ArgCP,ArgStr),
			  atom_chars(ArgStr,ArgChars),
			  (number_chars(Value,ArgChars) | Value = ArgStr),
	                  InChecks = [IC|ICs],
			  C =.. [IC,Value],
			  (call(C) -> Fail=[]|
		           Fail=[0]),
			  writeMessage(ArgStr),
			  (InOutPattern=[]|writeMessage(','))|
	 PatC = Argstr -> number_chars(N,[M1]),
	                  recorded(state,argumentWidget(N,Text),_),
			  xmTextGetString(Text,ArgCP),
			  proxtCharPtrToString(ArgCP,ArgStr),
			  atom_chars(ArgStr,ArgChars),
			  (number_chars(Value,ArgChars) | Value = ArgStr),
	                  InChecks = [IC|ICs],
			  C =.. [IC,Value],
			  (call(C) -> Fail=[]|
		           Fail=[N]),
			  writeMessage(ArgStr),
			  (InOutPattern=[]|writeMessage(','))|
	 PatC = Argstr2 -> number_chars(N,[M3,M4]),
	                  recorded(state,argumentWidget(N,Text),_),
			  xmTextGetString(Text,ArgCP),
			  proxtCharPtrToString(ArgCP,ArgStr),
			  atom_chars(ArgStr,ArgChars),
			  (number_chars(Value,ArgChars) | Value = ArgStr),
	                  InChecks = [IC|ICs],
			  C =.. [IC,Value],
			  (call(C) -> Fail=[]|
		           Fail=[N]),
			  writeMessage(ArgStr),
			  (InOutPattern=[]|writeMessage(','))|
	 PatC = "xmoptdepth" -> recorded(state,argumentWidget(depth,Text),_),
			  xmTextGetString(Text,ArgCP),
			  proxtCharPtrToString(ArgCP,ArgStr),
			  atom_chars(ArgStr,ArgChars),
	                  InChecks = [IC|ICs],
			  (ArgChars = [] ->
			    Value = novalue|
			   otherwise ->
			    (number_chars(Value,ArgChars) | Value = ArgStr),
			    C =.. [IC,Value],
			    (call(C) -> Fail=[]|
		             Fail=[0]),
			    writeMessage(ArgStr),
			    (InOutPattern=[]|writeMessage(',')))|
	 PatC = Optstr -> number_chars(N,[M2]),
	                  recorded(state,argumentWidget(N,Text),_),
			  xmTextGetString(Text,ArgCP),
			  proxtCharPtrToString(ArgCP,ArgStr),
			  atom_chars(ArgStr,ArgChars),
	                  InChecks = [IC|ICs],
			  (ArgChars = [] ->
			    Value = novalue|
			   otherwise ->
			    (number_chars(Value,ArgChars) | Value = ArgStr),
			    C =.. [IC,Value],
			    (call(C) -> Fail=[]|
		             Fail=[N]),
			    writeMessage(ArgStr),
			    (InOutPattern=[]|writeMessage(',')))|
	 PatC = Optstr2 -> number_chars(N,[M5,M6]),
	                  recorded(state,argumentWidget(N,Text),_),
			  xmTextGetString(Text,ArgCP),
			  proxtCharPtrToString(ArgCP,ArgStr),
			  atom_chars(ArgStr,ArgChars),
	                  InChecks = [IC|ICs],
			  (ArgChars = [] ->
			    Value = novalue|
			   otherwise ->
			    (number_chars(Value,ArgChars) | Value = ArgStr),
			    C =.. [IC,Value],
			    (call(C) -> Fail=[]|
		             Fail=[N]),
			    writeMessage(ArgStr),
			    (InOutPattern=[]|writeMessage(',')))|
	 PatC = Outstr -> InChecks = ICs,
	                  PatC = [_|P1],
			  append("X",P1,P2),
			  atom_chars(P3,P2),
	                  writeMessage(P3),
			  Fail=[],
			  Value = variable,
			  (InOutPattern=[]|writeMessage(','))|
	 otherwise -> Value = Pat,
	                  InChecks = ICs,
	                  Fail=[],
	                  writeMessage(Pat),	
			  (InOutPattern=[]|writeMessage(','))),!,
	replaceXmVars(InOutPattern,ICs,Values,F),
	append(Fail,F,Fails),
	(Value = novalue -> Args = Values|
	 Value = variable -> Args = [_Var|Values]|
	 otherwise -> Args = [Value|Values]).


%************************************************************************
%*
%* predicate: outVars/3
%*
%* syntax: 
%*
%* args:       +[Arg1|Args]		arguments returned by a learnin f.
%*	       +[Pat|InOutPattern]	list of InOutPatterns	
%*	       +XOuts			list of output functions
%*
%* description: Calls the spezified output functions with the values
%*		returned by a learning function.	
%*
%************************************************************************

outVars(_,_,[]).
outVars([Arg1|Args],[Pat|InOutPattern],XOuts) :-
	atom_chars(Pat,PatC),
	append("xmout",[_],Outstr),
	(PatC = Outstr -> XOuts = [XO|XOs],
			  C =.. [XO,Arg1],
			  call(C)|
	 otherwise -> XOuts = XOs),!,
	outVars(Args,InOutPattern,XOs).


%************************************************************************
%*
%* predicate: refresh/1
%*
%* syntax:
%*
%* args: +[KindOfKnowledge|R]	
%*
%* description: Refreshes the visual part of the knowledge base of the
%*		specified kinds of knowledge.
%*
%************************************************************************

refresh([]).
refresh([KindOfKnowledge|R]) :- 
	refreshKnowledgeList(_,KindOfKnowledge,_),
	refresh(R).

%************************************************************************
%*
%* predicate: createYesNoPopup/4
%*
%* syntax:
%*
%* args:       +Parent		parent widget, which is a push button	
%*	       -YesNoPopup	created widget, popupMenu		
%*	       +YesPred		callback procedure for Yes button	
%*	       +YesArg		argument for callback procedure	
%*
%* description: Creates a popupMenu which is poped up, whenever the	
%*		parent button is pressed. This popupMenu consists of two
%*		push buttons 'Yes' and 'No'.
%*
%************************************************************************

createYesNoPopup( Parent,YesNoPopup,YesPred,YesArg ) :-
	xmCreatePopupMenu(Parent,'YesNo',
	  [xmNorientation(xmVERTICAL),
	   xmNpacking(xmPACK_COLUMN),
	   xmNisAligned(true),xmNentryAlignment(xmALIGNMENT_CENTER)], 
	  YesNoPopup),
	xmCreatePushButton(YesNoPopup,'Yes',
	  [],Yes),
	xmCreatePushButton(YesNoPopup,'No',
	  [],No),
	xtAddCallback(Parent,xmNactivateCallback,popupDialog,
	  YesNoPopup),
	xtAddCallback(Yes,xmNactivateCallback,YesPred,YesArg),
	xtAddCallback(No,xmNactivateCallback,yesNoPopdown,YesNoPopup),
	xtAddEventHandler(YesNoPopup,[buttonReleaseMask],false,
	                  yesNoPopdown,YesNoPopup),
	xtManageChildren([Yes,No]).


%************************************************************************
%*
%* predicate: popupDialog/3	callback procedure
%*
%* syntax:
%*
%* args:       +Widget		calling widget
%*	       +PopupShell	widget, to manage
%*	       _CallData	event	
%*
%* description: Manages the specified widget beneath the calling widget.
%*
%************************************************************************

popupDialog(Widget,PopupShell,_CallData) :-
	xtGetValues(Widget,[xmNwidth(Xs)]),
	xtTranslateCoords(Widget,Xs,0,X,Y),
	xtSetValues(PopupShell,[xmNx(X),xmNy(Y)]),
	xtManageChild(PopupShell).


%************************************************************************
%*
%* predicate: popupViewRules/3  	callback procedure	
%*
%* syntax:
%*
%* args:       +Widget		calling widget	
%*	       _ClientData	client data	
%*	       _CallData	event	
%*
%* description: Manages the specified widget beneath the calling widget
%*
%************************************************************************

popupViewRules(Widget,_ClientData,_CallData) :-
	recorded(rules,view(dialog,VRDialog),_),
	xtGetValues(Widget,[xmNwidth(Xs)]),
	xtTranslateCoords(Widget,Xs,0,X,Y),
	xtSetValues(VRDialog,[xmNx(X),xmNy(Y)]),
	xtManageChild(VRDialog),

	fillExistingLabels,
	fillViewedLabels,
	fillExistingClauseHeads,
	fillViewedClauseHeads.




%************************************************************************
%*
%* predicate: popupExamineRules/3  	callback procedure
%*
%* syntax:
%*
%* args:       +Widget		calling widget	
%*	       _ClientData	client data	
%*	       _CallData	event	
%*
%* description: Manages the specified widget beneath the calling widget.
%*
%************************************************************************

popupExamineRules(Widget,_ClientData,_CallData) :-
	recorded(rules,examine(dialog,ERDialog),_),
	xtGetValues(Widget,[xmNwidth(Xs)]),
	xtTranslateCoords(Widget,Xs,0,X,Y),
	xtSetValues(ERDialog,[xmNx(X),xmNy(Y)]),
        showExaminedRule(_,_,_),
	xtManageChild(ERDialog).



%************************************************************************
%*
%* predicate: popupViewExamples/3  	callback procedure
%*
%* syntax:
%*
%* args:       +Widget		calling widget	
%*	       _ClientData	client data	
%*	       _CallData	event	
%*
%* description: Manages the specified widget beneath the calling widget.
%*
%************************************************************************

popupViewExamples(Widget,_ClientData,_CallData) :-
	recorded(examples,view(dialog,VRDialog),_),
	xtGetValues(Widget,[xmNwidth(Xs)]),
	xtTranslateCoords(Widget,Xs,0,X,Y),
	xtSetValues(VRDialog,[xmNx(X),xmNy(Y)]),
	xtManageChild(VRDialog),

	fillExistingExampleCHs,
	fillViewedExampleCHs.


%************************************************************************
%*
%* predicate: fillExistingLabels/0	
%*
%* syntax:
%*
%* args: -
%*
%* description: Finds all existing Labels.
%*
%************************************************************************

fillExistingLabels :-
	recorded(rules,view(exLabRC,ExLabRC),_),
	listLabels(LabelList),
	createLabelWidgets(ExLabRC,LabelList).


%************************************************************************
%*
%* predicate: fillViewedLabels/0	
%*
%* syntax:
%*
%* args: -
%*
%* description: Finds all viewed Labels.
%*
%************************************************************************

fillViewedLabels:-
	recorded(rules,view(viewLabRC,ViewLabRC),_),
	recorded(rules,view(labels,LabelList),_),
	createLabelWidgets(ViewLabRC,LabelList).


%************************************************************************
%*
%* predicate: fillExistingClauseHeads/0
%*
%* syntax:
%*
%* args: -
%*
%* description:
%*
%************************************************************************

fillExistingClauseHeads:-
	recorded(rules,view(exCHRC,ExCHRC),_),
	listClauseHeads(LabelList),
	createLabelWidgets(ExCHRC,LabelList).


%************************************************************************
%*
%* predicate: fillViewedClauseHeads/0
%*
%* syntax:
%*
%* args: -
%*
%* description: Finds all existing Labels
%*
%************************************************************************

fillViewedClauseHeads:-
	recorded(rules,view(viewCHRC,ViewCHRC),_),
	recorded(rules,view(clause_heads,CHList),_),
	createLabelWidgets(ViewCHRC,CHList).


%************************************************************************
%*
%* predicate: fillExistingExampleCHs/0
%*
%* syntax:
%*
%* args: -
%*
%* description:  Finds all existing Labels.	
%*
%************************************************************************

fillExistingExampleCHs:-
	recorded(examples,view(exCHRC,ExCHRC),_),
	listExampleCHs(LabelList),
	createLabelWidgets(ExCHRC,LabelList).


%************************************************************************
%*
%* predicate: fillViewedExampleCs/0
%*
%* syntax:
%*
%* args: -
%*
%* description: Finds all existing Clause Heads of examples.
%*
%************************************************************************

fillViewedExampleCHs:-
	recorded(examples,view(viewCHRC,ViewCHRC),_),
	recorded(examples,view(clause_heads,CHList),_),
	createLabelWidgets(ViewCHRC,CHList).


%************************************************************************
%*
%* predicate: createLabelWidgets/2
%*
%* syntax:
%*
%* args:       +Parent		widget	
%*	       +NameList		atomList
%*
%* description: creates a label for each name in NameList.
%*
%************************************************************************

createLabelWidgets(Parent,[Name|NameList]) :-
	proxtStringToCharPtr(Name,NameChrPtr),
	proxtGetDefaultCharset(DCharset),
	xmStringCreate(NameChrPtr,DCharset,NameStr),
	xmCreateLabel(Parent,nameLabel,
	  [xmNalignment(xmALIGNMENT_BEGINNING),
	   xmNlabelType(xmSTRING),xmNlabelString(NameStr)],
	  NameLabel),
	xtManageChild(NameLabel),
	recordz(labels,parent_child(Parent,NameLabel,NameStr),_),
	xtAddEventHandler(NameLabel,[buttonReleaseMask],
	                  false,selectLabel,_),!,
	createLabelWidgets(Parent,NameList).
createLabelWidgets(_,[]) :- !.


%************************************************************************
%*
%* predicate: destroyLabelWidgets/1
%*
%* syntax: destroyLabelWidgets(+Parent)
%*
%* description: destroys each sublabel of Parent.
%*
%************************************************************************

destroyLabelWidgets(Parent) :- 
	var(Parent),
	recorded(labels,parent_child(Parent,L,_),Ref),!,
	erase(Ref),
	xtDestroyWidget(L),
	destroyLabelWidgets(_).
destroyLabelWidgets(Parent) :- 
	recorded(labels,parent_child(Parent,L,_),Ref),!,
	erase(Ref),
	xtDestroyWidget(L),
	destroyLabelWidgets(Parent).
destroyLabelWidgets(_) :- !.


%************************************************************************
%*
%* predicate: listLabel/1
%*
%* syntax:
%*
%* args: LabelList	atomList
%*
%* description: lists each label of rules of knowledgebase.	
%*
%************************************************************************

listLabels(LabelList) :- 
	recorded(rules,view(labels,ViewedLabels),_),
	listLabels(ViewedLabels,LL1),
	append(LabelList,ViewedLabels,LL1),!.
listLabels(LLin,LLout) :-
	get_clause(_,_,_,_,Label),
	nonmember(Label,LLin),!,
	LLin2=[Label|LLin],
	listLabels(LLin2,LLout).
listLabels(A,A) :- !.


%************************************************************************
%*
%* predicate: listClauseHeads/1
%*
%* syntax:
%*
%* args: LabelList	atomList
%*
%* description: lists each clausehead of rules of knowledgebase.
%*
%************************************************************************

listClauseHeads(LabelList) :- 
	recorded(rules,view(clause_heads,ViewedCHs),_),
	listClauseHeads(ViewedCHs,LL1),
	append(LabelList,ViewedCHs,LL1),!.
listClauseHeads(LLin,LLout) :-
	get_clause(_,C,_,_,_),
	functor(C,CH,_),
	nonmember(CH,LLin),!,
	LLin2=[CH|LLin],
	listClauseHeads(LLin2,LLout).
listClauseHeads(A,A) :- !.


%************************************************************************
%*
%* predicate: listExampleCHs/1
%*
%* syntax:
%*
%* args: LabelList	atomList
%*
%* description: lists each clausehead of examples of knowledgebase.
%*
%************************************************************************

listExampleCHs(LabelList) :- 
	recorded(examples,view(clause_heads,ViewedCHs),_),
	listExampleCHs(ViewedCHs,LL1),
	append(LabelList,ViewedCHs,LL1),!.
listExampleCHs(LLin,LLout) :-
	get_example(_,C,_),
	functor(C,CH,_),
	nonmember(CH,LLin),!,
	LLin2=[CH|LLin],
	listExampleCHs(LLin2,LLout).
listExampleCHs(A,A) :- !.


%************************************************************************
%*
%* predicate: selectLabel/3		callback procedure	
%*
%* syntax:
%*
%* args:       +Widget			calling widget
%*	       _ClientData		client data	
%*	       _CallData		event
%*
%* description: Toggles the parent of the calling widget. (exist./view.)
%*
%************************************************************************

selectLabel(Widget,_ClientData,_CallData) :-
	recorded(labels,parent_child(P,Widget,N),Ref), !,
	erase(Ref),
	xtDestroyWidget(Widget),
	(recorded(rules,view(exLabRC,P),_) ->
	    recorded(rules,view(viewLabRC,P2),_)|
	 recorded(rules,view(viewLabRC,P),_) ->
	    recorded(rules,view(exLabRC,P2),_)|
	 recorded(rules,view(exCHRC,P),_) ->
	    recorded(rules,view(viewCHRC,P2),_)|
	 recorded(rules,view(viewCHRC,P),_) ->
	    recorded(rules,view(exCHRC,P2),_)|
	 recorded(examples,view(exCHRC,P),_) ->
	    recorded(examples,view(viewCHRC,P2),_)|
	 recorded(examples,view(viewCHRC,P),_) ->
	    recorded(examples,view(exCHRC,P2),_)|
	 recorded(examples,view(exCHRC,P),_) ->
	    recorded(examples,view(viewCHRC,P2),_)|
	 recorded(examples,view(viewCHRC,P),_) ->
	    recorded(examples,view(exCHRC,P2),_)),
	xmCreateLabel(P2,nameLabel,
	  [xmNalignment(xmALIGNMENT_BEGINNING),
	   xmNlabelType(xmSTRING),xmNlabelString(N)],
	  NameLabel),
	xtManageChild(NameLabel),
	recordz(labels,parent_child(P2,NameLabel,N),_),
	xtAddEventHandler(NameLabel,[buttonReleaseMask],
	                  false,selectLabel,_),!.


viewRules(_Widget,_ClientData,_CallData) :-
	(recorded(rules,view(all),R1) -> erase(R1)|
	 otherwise),
	recorded(rules,view(labels,_),R2),
	erase(R2),
	recorded(rules,view(clause_heads,_),R3),
	erase(R3),
	recorded(rules,view(viewLabRC,ViewLabRC),_),
	recorded(rules,view(viewCHRC,ViewCHRC),_),
	viewRules(ViewLabRC,LabelList),
	recordz(rules,view(labels,LabelList),_),
	viewRules(ViewCHRC,CHList),
	recordz(rules,view(clause_heads,CHList),_),
	destroyLabelWidgets(_),
	refresh([rules]),
	recorded(rules,view(dialog,X),_),
	xtUnmanageChild(X).
viewRules(P,List) :-
	recorded(labels,parent_child(P,C,N),Ref),
	xtDestroyWidget(C),
	erase(Ref),
	proxtGetDefaultCharset(DCharset),
	xmStringGetLtoR(N,DCharset,NCP,_),
	proxtCharPtrToString(NCP,Name),
	viewRules(P,L2),
	List=[Name|L2].
viewRules(_,[]) :- !.


viewAllRules(_Widget,_ClientData,_CallData) :-
	(recorded(rules,view(all),_)|
	 recordz(rules,view(all),_)),
	 refresh([rules]),
	destroyLabelWidgets(_),
	recorded(rules,view(dialog,X),_),
	xtUnmanageChild(X).


cancelViewRules(_Widget,_ClientData,_CallData) :-
	destroyLabelWidgets(_),
	recorded(rules,view(dialog,X),_),
	xtUnmanageChild(X).


cancelExamineRules(_Widget,_ClientData,_CallData) :-
	destroyLabelWidgets(_),
	recorded(rules,examine(dialog,X),_),
	xtUnmanageChild(X).


viewExamples(_Widget,_ClientData,_CallData) :-
	(recorded(examples,view(all),R1) -> erase(R1)|
	 otherwise),
	recorded(examples,view(clause_heads,_),R3),
	erase(R3),
	recorded(examples,view(viewCHRC,ViewCHRC),_),
	viewExamples(ViewCHRC,CHList),
	recordz(examples,view(clause_heads,CHList),_),
	destroyLabelWidgets(_),
	refresh([examples]),
	recorded(examples,view(dialog,X),_),
	xtUnmanageChild(X).
viewExamples(P,List) :-
	recorded(labels,parent_child(P,C,N),Ref),
	xtDestroyWidget(C),
	erase(Ref),
	proxtGetDefaultCharset(DCharset),
	xmStringGetLtoR(N,DCharset,NCP,_),
	proxtCharPtrToString(NCP,Name),
	viewExamples(P,L2),
	List=[Name|L2].
viewExamples(_,[]) :- !.


viewAllExamples(_Widget,_ClientData,_CallData) :-
	(recorded(examples,view(all),_)|
	 recordz(examples,view(all),_)),
	 refresh([examples]),
	destroyLabelWidgets(_),
	recorded(examples,view(dialog,X),_),
	xtUnmanageChild(X).


cancelViewExamples(_Widget,_ClientData,_CallData) :-
	destroyLabelWidgets(_),
	recorded(examples,view(dialog,X),_),
	xtUnmanageChild(X).


%************************************************************************
%*
%* predicate: popupFunctions/3		callback procedure	
%*
%* syntax:
%*
%* args:       +Widget			calling widget
%*	       +PopupShell		widget, to manage
%*	       _CallData		event	
%*
%* description: Manages the specified child of the calling widget.	
%*
%************************************************************************

popupFunctions(Widget,PopupShell,_CallData) :-
	xtGetValues(Widget,[xmNheight(Ys)]),
	xtTranslateCoords(Widget,0,Ys,X,Y),
	xtSetValues(PopupShell,[xmNx(X),xmNy(Y)]),
	xtManageChild(PopupShell),
	recordz(learnfuncs,popedup(PopupShell),_).


%************************************************************************
%*
%* predicate: yesNoPopdown/3		callback procedure
%*
%* syntax: 
%*
%* args:       _Widget			calling widget	
%*	       +JaNeinPopup		widget, to unmanage
%*	       _CallData		event	
%*
%* description: Unmanages a popup widget.
%*
%************************************************************************

yesNoPopdown(_Widget,JaNeinPopup,_CallData) :-
	xtUnmanageChild(JaNeinPopup).


%************************************************************************
%*
%* predicate: functionsPopdown/3		callback procedure
%*
%* syntax:
%*
%* args:       _Widget			calling widget	
%*	       _ClientData		client data	
%*	       _CallData		event	
%*
%* description: Unmanages a popup widget.
%*
%************************************************************************

functionsPopdown(_Widget,_ClientData,_CallData) :-
	recorded(learnfuncs,popedup(PopupShell),Ref),
	erase(Ref),
	xtUnmanageChild(PopupShell).


%************************************************************************
%*
%* predicate: doKommando/3		callback procedure
%*
%* syntax:
%*
%* args:       _Widget			calling procedure
%*	       +[KommandoText,KStatus]	widgets	
%*	       _CallData		event
%*
%* description: Executes the command given by the user.
%*
%************************************************************************

doKommando(_Widget,[KommandoText,KStatus],_CallData) :-
	proxtGetDefaultCharset(DCharset),
	proxtStringToCharPtr('-EXIT-    Input',ACP),
	xmStringCreate(ACP,DCharset,AXmS),
	xtSetValues(KStatus,[xmNlabelString(AXmS)]),
	xmTextGetString(KommandoText,KcharPtr),
	proxtCharPtrToString(KcharPtr,KS),
	xxmStringToTerm(KcharPtr,_Charset,Term),

	writeMessage(':- '),
	writelnMessage(KS),
	(call(Term) ->
	    (proxtStringToCharPtr('- yes -   Input',YesCP),
	     xmStringCreate(YesCP,DCharset,YesXmS),
	     xtSetValues(KStatus,[xmNlabelString(YesXmS)]),
	     writelnMessage('% yes'))|
	  otherwise ->
	    (proxtStringToCharPtr('- no -    Input',NoCP),
	     xmStringCreate(NoCP,DCharset,NoXmS),
	     xtSetValues(KStatus,[xmNlabelString(NoXmS)]),
	     writelnMessage('% no'))),

	doEmptyKommando(_,KommandoText,_).


%************************************************************************
%*
%* predicate: doEmptyKommando/3		callback procedure
%*
%* syntax:
%*
%* args:       _Widget			calling widget
%*	       +KommandoText		widget		
%*	       _CallData		event	
%*
%* description: Sets the command text to an empty string.	
%*
%************************************************************************

doEmptyKommando(_Widget,KommandoText,_CallData) :-
	proxtStringToCharPtr('',KommandoChrPtr),
	xmTextSetString(KommandoText,KommandoChrPtr),
        refreshKnowledgeList(_Widget,rules,_Calldata),
	refreshKnowledgeList(_Widget,examples,_Calldata),
        updateEvaluationLabel.


%************************************************************************
%*
%* predicate: beenden/3		callback procedure	
%*
%* syntax:
%*
%* args:       _Widget		calling widget	
%*	       _ClientData	client data	
%*	       _CallData	event
%*
%* description: Quits XMILES,	returns to Prolog toplevel.
%*
%************************************************************************

beenden(_Widget,_ClientData,_CallData) :-
	recorded(messages,file(F),Ref),
        erase(Ref),
	close(F),
	write('X-MILES korrekt beendet!'),nl,
        retract(my_exit_loop(no)),
        assert(my_exit_loop(yes)).


%************************************************************************
%*
%* predicate: createMessageArea/1
%*
%* syntax: createMessageArea(+Parent)
%*
%* args:
%*
%* description: Creates the area for the messages from XMILES to the user.
%*
%************************************************************************

createMessageArea(Parent) :-
	xmCreateFrame(Parent,'Messages',
	  [],MessageFrame),
	xtManageChild(MessageFrame),
	xmCreateRowColumn(MessageFrame,messages,
	  [xmNadjustLast(true),
	   xmNorientation(xmVERTICAL),
	   xmNpacking(xmPACK_TIGHT),
	   xmNrowColumnType(xmWORK_AREA)],
	  MessageColumn),
	xtManageChild(MessageColumn),

	proxtStringToCharPtr('Messages',MessageTitleChrPtr),
	proxtGetDefaultCharset(DCharset),
	xmStringCreate(MessageTitleChrPtr,DCharset,MessageTitleStr),
	xmCreateLabelGadget(MessageColumn,messageLabel,
	  [xmNlabelType(xmSTRING),xmNlabelString(MessageTitleStr)],
	  MessageLabel),
	xtManageChild(MessageLabel),

	xmCreateScrolledText(MessageColumn,messageText,
	  [xmNeditable(false),xmNeditMode(xmMULTI_LINE_EDIT),
	   xmNrows(8),
	   xmNlistSizePolicy(xmCONSTANT),
	   xmNscrollBarDisplayPolicy(xmSTATIC),
	   xmNscrollingPolicy(xmAUTOMATIC),
	   xmNautoShowCursorPosition(true)],MessageText),
	xtManageChild(MessageText),
	recordz(messages,textWidget(MessageText),_),

	xmCreateRowColumn(MessageColumn,'ButtonRow',
	  [xmNisAligned(true),xmNentryAlignment(xmALIGNMENT_CENTER),
	   xmNnumColumns(1),
	   xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN)],
	  ButtonRow),
	xtManageChild(ButtonRow),

	xmCreatePushButton(ButtonRow,'Save',
	  [],Save),
	xtManageChild(Save),
	xtAddCallback(Save,xmNactivateCallback,saveMessages,_),

	xmCreatePushButton(ButtonRow,'Clear',
	  [],Clear),
	xtManageChild(Clear),
	xtAddCallback(Clear,xmNactivateCallback,clearMessages,_),
	
	open('xmProtocol.tmp',write,F),
	recordz(messages,file(F),_).


%************************************************************************
%*
%* predicate: saveMessages/3    	callback procedure
%*
%* syntax:
%*
%* args:       _Widget		calling widget
%*	       _ClientData	client data	
%*	       _CallData	event	
%*
%* description: Saves the scrolled text widet holding the XMILES messages 
%*
%************************************************************************

saveMessages(_Widget,_ClientData,_CallData) :-
	recorded(messages,textWidget(MessageText),_),
	proxtStringToCharPtr('',EmptyCharPtr),
	xmTextSetString(MessageText,EmptyCharPtr),
	recorded(messages,file(F),Ref),
	erase(Ref),
	close(F),
	unix(shell('mv xmProtocol.tmp xmProtocol.sav')),
	open('xmProtocol.tmp',write,Fnew),
	recordz(messages,file(Fnew),_),
	writelnMessage('% wrote "xmProtocol.sav"').


%************************************************************************
%*
%* predicate: clearMessages/3	callback procedure	
%*
%* syntax:
%*
%* args:       _Widget		calling widget		
%*	       _ClientData	client data		
%*	       _CallData	event
%*
%* description: Clears the scrolled text widet holding the XMILES messages
%*
%************************************************************************

clearMessages(_Widget,_ClientData,_CallData) :-
	recorded(messages,textWidget(MessageText),_),
	proxtStringToCharPtr('',EmptyCharPtr),
	xmTextSetString(MessageText,EmptyCharPtr),
	recorded(messages,file(F),Ref),
	erase(Ref),
	close(F),
	open('xmProtocol.tmp',write,Fnew),
	recordz(messages,file(Fnew),_).


%************************************************************************
%*
%* predicate: writeError/1
%*
%* syntax: writeError(+Message)
%*
%* args:
%*
%* description: Appends the Message to the ErrorText buffer.
%*
%************************************************************************

writeError(Message) :-
	(number(Message) -> number_chars(Message,MsgChars),
	        atom_chars(MsgStr,MsgChars)|
	 otherwise -> MsgStr = Message),
	(recorded(error,errorText(EText),Ref) -> erase(Ref),
	        concat(EText,MsgStr,NewEText)|
	 otherwise -> NewEText = MsgStr),
	recordz(error,errorText(NewEText),_).


%************************************************************************
%*
%* predicate: writelnError/1
%*
%* syntax: writelnError(+Message)
%*
%* args:
%*
%* description: Appends the Message to the ErrorText buffer.
%*
%************************************************************************

writelnError(Message) :-
	(number(Message) -> number_chars(Message,MsgChars),
	        append(MsgChars,[10],MC),
	        atom_chars(MsgStr,MC)|
	 otherwise -> atom_chars(Message,MsgChars),
	        append(MsgChars,[10],MC),
	        atom_chars(MsgStr,MC)),
	(recorded(error,errorText(EText),Ref) -> erase(Ref),
	        concat(EText,MsgStr,NewEText)|
	 otherwise -> NewEText = MsgStr),
	recordz(error,errorText(NewEText),_).


%************************************************************************
%*
%* predicate: flushErrorBuffer/1
%*
%* syntax: flushErrorBuffer(+Message)
%*
%* args:
%*
%* description: Appends the ErrorText buffer to the MessageText.
%*
%************************************************************************

flushErrorBuffer :-
	(recorded(error,errorText(EText),Ref) -> erase(Ref),
	        writeMessage(EText)|
	 otherwise).


%************************************************************************
%*
%* predicate: writeMessage/1
%*
%* syntax: writeMessage(+Message)
%*
%* args:
%*
%* description: Appends the Message to the MessageText.
%*
%************************************************************************

writeMessage(Message) :-
	recorded(messages,textWidget(MessageText),_),
	(number(Message) -> number_chars(Message,MsgChars),
	        atom_chars(MsgStr,MsgChars),
	        proxtStringToCharPtr(MsgStr,MessageCharPtr)|
	 atom(Message) -> proxtStringToCharPtr(Message,MessageCharPtr)|
	 otherwise -> xxmWriteToCharPtr(write(Message),MessageCharPtr)),
	xmTextGetLastPosition(MessageText,LastPos),
	xmTextReplace(MessageText,LastPos,LastPos,MessageCharPtr),
	recorded(messages,file(F),_),
	write(F,Message).


%************************************************************************
%*
%* predicate: writelnMessage/1
%*
%* syntax: writelnMessage(+Message)
%*
%* args:
%*
%* description: Appends the Message to the MessageText.
%*
%************************************************************************

writelnMessage(Message) :-
	recorded(messages,textWidget(MessageText),_),
	(number(Message) -> number_chars(Message,MsgChars),
	        append(MsgChars,[10],MC2),
	        atom_chars(MsgStr,MC2),
	        proxtStringToCharPtr(MsgStr,MessageCharPtr)|
	 atom(Message) -> atom_chars(Message,MsgChars),
	        append(MsgChars,[10],MC2),
	        atom_chars(MsgStr,MC2),
	        proxtStringToCharPtr(MsgStr,MessageCharPtr)|
	 otherwise -> xxmWriteToCharPtr((write(Message),nl),MessageCharPtr)),
	xmTextGetLastPosition(MessageText,LastPos),
	xmTextReplace(MessageText,LastPos,LastPos,MessageCharPtr),
	xmTextScroll(MessageText,1),
	recorded(messages,file(F),_),
	write(F,Message),
	nl(F).


%************************************************************************
%*
%* predicate: createEditorArea/1
%*
%* syntax: createEditorArea(+Parent)
%*
%* args:
%*
%* description:  Creates the area for the editior for editing rules and
%*		 examples.
%*
%************************************************************************

createEditorArea(Parent) :-
	xmCreateFrame(Parent,'Editor',
	  [],EditorFrame),
	xtManageChild(EditorFrame),

	xmCreateRowColumn(EditorFrame,editor,
	  [xmNadjustLast(true),
	   xmNorientation(xmVERTICAL),
	   xmNpacking(xmPACK_TIGHT),
	   xmNrowColumnType(xmWORK_AREA)],
	  EditorRowColumn),
	xtManageChild(EditorRowColumn),

	proxtStringToCharPtr('Editor',EditorTitleChrPtr),
	proxtGetDefaultCharset(DCharset),
	xmStringCreate(EditorTitleChrPtr,DCharset,EditorTitleStr),
	xmCreateLabelGadget(EditorRowColumn,editorLabel,
	  [xmNlabelType(xmSTRING),xmNlabelString(EditorTitleStr)],
	  EditorLabel),
	xtManageChild(EditorLabel),
	recordz(editor,label(EditorLabel),_),

	xmCreateScrolledText(EditorRowColumn,kommandotext,
	  [xmNeditable(true),xmNeditMode(xmMULTI_LINE_EDIT),
	   xmNrows(5),
	   xmNlistSizePolicy(xmCONSTANT),
	   xmNscrollBarDisplayPolicy(xmSTATIC),
	   xmNscrollingPolicy(xmAUTOMATIC),
	   xmNautoShowCursorPosition(true)],EditorText),
	xtManageChild(EditorText),
	recordz(editor,textWidget(EditorText),_Ref),

	xmCreateRowColumn(EditorRowColumn,'ButtonRowColumn',
	  [xmNisAligned(true),xmNentryAlignment(xmALIGNMENT_CENTER),
	   xmNnumColumns(1),
	   xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN)],
	  ButtonRowColumn),
	xtManageChild(ButtonRowColumn),

	xmCreatePushButton(ButtonRowColumn,'Add rule',
	  [],AddRule),
	xtManageChild(AddRule),

	xmCreatePushButton(ButtonRowColumn,'Change rule',
	  [],ChangeRule),
	createYesNoPopup(ChangeRule,_YesNoPopup,changeRule,_),
	xtManageChild(ChangeRule),

	xmCreatePushButton(ButtonRowColumn,'Add example',
	  [],AddExample),
	xtManageChild(AddExample),

	xmCreatePushButton(ButtonRowColumn,'Change example',
	  [],ChangeExample),
	createYesNoPopup(ChangeExample,_YesNoPopup2,changeExample,_),
	xtManageChild(ChangeExample),

	xmCreatePushButton(ButtonRowColumn,'Clear',
	  [],Clear),
	xtManageChild(Clear),

	xtAddCallback(Clear,xmNactivateCallback,clearEditor,_),
	xtAddCallback(AddRule,xmNactivateCallback,addRule,_),
	xtAddCallback(AddExample,xmNactivateCallback,addExample,_).


%************************************************************************
%*
%* predicate: clearEditor/3	callback procedure	
%*
%* syntax:
%*
%* args:       _Widget		calling widget	
%*	       _ClientData	client data	
%*	       _CallData	event
%*
%* description: Clears the scrolled text widget holding the edited object
%*
%************************************************************************

clearEditor(_Widget,_ClientData,_CallData) :-
	recorded(editor,textWidget(EditorText),_),
	recorded(editor,label(Label),_),
	proxtGetDefaultCharset(DCharset),
	(recorded(editor,editing(_,_),Ref) -> erase(Ref),
	    proxtStringToCharPtr('Editor',LblCP),
	    xmStringCreate(LblCP,DCharset,LblS),
	    xtSetValues(Label,[xmNlabelString(LblS)]) |
         otherwise),
	proxtStringToCharPtr('',EmptyChrPtr),
	xmTextSetString(EditorText,EmptyChrPtr).	


%************************************************************************
%*
%* predicate: addRule/3				Callback procedure
%*
%* syntax:
%*
%* args:       _Widget				calling widget
%*	       _ClientData					
%*	       _CallData			event	
%*
%* description: Adds the rule in the editor to the rule list.	
%*
%************************************************************************

addRule(_Widget,_ClientData,_CallData) :-
	recorded(editor,textWidget(EditorText),_Ref),	
	xmTextGetString(EditorText,RuleCP),
	xxmStringToTerm(RuleCP,_,RuleTerm),
	proxtCharPtrToString(RuleCP,RulePString),
	span_left(RulePString,".",N),
	substring(RulePString,RuleString,0,N,_),
	store_clause(RuleTerm,_,user,ID),
	writeMessage(':- store_clause('),
	writeMessage(RuleString),
	writeMessage(',_,user,'),
	writeMessage(ID),
	writelnMessage(').'),
	writelnMessage('% rule added.'),
        updateEvaluationLabel,
	get_clause(ID,H,B,S,L),
	addRuleItem(ID,H,B,S,L).


%************************************************************************
%*
%* predicate:  resultAddRule/1			result procedure
%*
%* syntax: resultAddRule(Id)
%*
%* args:
%*
%* description: Adds the rule specified by Id to the rule list.
%*
%************************************************************************

resultAddRule(Id) :-
	get_clause(Id,H,B,S,L),
	addRuleItem(Id,H,B,S,L),
	writeMessage('% rule '),
	number_chars(Id,IdChars),
	atom_chars(IdString,IdChars),
	writeMessage(IdString),
	writelnMessage(' created.').


%************************************************************************
%*
%* predicate:  resultAddNewpreds/1			result procedure
%*
%* syntax: resultAddNewpreds(Reflist)
%*
%* args:
%*
%* description: Displays a window with alternative specialisations
%*       through new predicates, and allows to choose one to add to the kb
%*
%************************************************************************

resultAddNewpreds((ID,Reflist)):-
   toplevel(Shell),
   xmCreateBulletinBoardDialog(Shell,'Newpreds',
	 [],NewpredDialog),
   recordz(newpreddialog,ID:NewpredDialog,_),
   xtManageChild(NewpredDialog),

   xmCreateFrame(NewpredDialog,'Newpred Frame',
	         [],NewpredFrame),
   xtManageChild(NewpredFrame),

   xmCreateRowColumn(NewpredFrame,newpredColumn,
	  [xmNorientation(xmVERTICAL),xmNpacking(xmPACK_TIGHT)],
	  NewpredColumn),
   xtManageChild(NewpredColumn),

   xmCreateRowColumn(NewpredColumn,titlerow,
	  [xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN),
	   xmNisAligned(true),xmNentryAlignment(xmALIGNMENT_CENTER)],
	  TitleRow),
   xtManageChild(TitleRow),

   proxtGetDefaultCharset(DCharset),
   proxtStringToCharPtr('Choose Specialisation',TitleCP),
   xmStringCreate(TitleCP,DCharset,TitleStr),
   xmCreateLabelGadget(TitleRow,title,
	  [xmNlabelType(xmSTRING),xmNlabelString(TitleStr)],
	  TitleLabel),
   xtManageChild(TitleLabel),

   xmCreateScrolledWindow(NewpredColumn,newpredSW,
	  [xmNheight(300),xmNwidth(300),
	   xmNlistSizePolicy(xmCONSTANT),
	   xmNscrollBarDisplayPolicy(xmSTATIC),
	   xmNscrollingPolicy(xmAUTOMATIC)],
	  NewpredSW),
   xtManageChild(NewpredSW),

   xmCreateRowColumn(NewpredSW,newpredRC,
	  [xmNadjustLast(false),
	   xmNorientation(xmVERTICAL),
	   xmNpacking(xmPACK_TIGHT),
	   xmNrowColumnType(xmWORK_AREA)],
	  NewpredRC),
   xtGetValues(NewpredRC,[xmNbackground(B)]),
   xtGetValues(NewpredSW,[xmNclipWindow(CW)]),
   xtSetValues(CW,[xmNbackground(B)]),
   xtManageChild(NewpredRC),
   addnewpredclauses(Reflist,NewpredRC),

   xmCreateRowColumn(NewpredColumn,buttonrow,
	  [xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN),
	   xmNisAligned(true),xmNentryAlignment(xmALIGNMENT_CENTER)],
	  ButtonRow),
   xtManageChild(ButtonRow),

   xmCreatePushButton(ButtonRow,'Add Rule',
	  [],AddRule),
   xtManageChild(AddRule),
   xtAddCallback(AddRule,xmNactivateCallback,
	          addselectedRule,_),

   xmCreatePushButton(ButtonRow,'None',
	  [],None),
   xtManageChild(None),
   xtAddCallback(None,xmNactivateCallback,
	              addnorule,_).	


addnewpredclauses([],_).
addnewpredclauses([(NC,Pos,Neg,TR)|R],Widget):-
   proxtGetDefaultCharset(DCharset),
   xxmWriteToString(portray_clause(NC),DCharset,XmS),
   xmCreateLabel(Widget,newpredClause,
	    [xmNalignment(xmALIGNMENT_BEGINNING),
	     xmNlabelString(XmS),xmNlabelType(xmSTRING)],
	     Label),
    xtManageChild(Label),
    recordz(newpred,np(Label,NC,Pos,Neg,TR,notselected),_Ref),
    xtAddEventHandler(Label,[buttonReleaseMask],
	              false,selectnpclause,_),
    addnewpredclauses(R,Widget).


selectnpclause(Widget,_,_CallData) :-
   recorded(newpred,np(Widget,NC,Pos,Neg,TR,notselected),Ref),
   turnoff_other_selected,
   xtGetValues(Widget,[xmNbackground(B),xmNforeground(F)]),
   xtSetValues(Widget,[xmNbackground(F),xmNforeground(B)]),
   erase(Ref),
   recordz(newpred,np(Widget,NC,Pos,Neg,TR,selected),_).

selectnpclause(Widget,rules,_CallData) :-
   recorded(newpred,np(Widget,NC,Pos,Neg,TR,selected),Ref),
   xtGetValues(Widget,[xmNbackground(B),xmNforeground(F)]),
   xtSetValues(Widget,[xmNbackground(F),xmNforeground(B)]),
   erase(Ref),
   recordz(newpred,np(Widget,NC,Pos,Neg,TR,notselected),_).

turnoff_other_selected:-
   mysetof(Ref,Widget^NC^Pos^Neg^TR^B^F^(
                recorded(newpred,np(Widget,NC,Pos,Neg,TR,selected),Ref),
                erase(Ref),
                recordz(newpred,np(Widget,NC,Pos,Neg,TR,notselected),_),
                xtGetValues(Widget,[xmNbackground(B),xmNforeground(F)]),
                xtSetValues(Widget,[xmNbackground(F),xmNforeground(B)])),_).

    
addnorule(_,_,_):-
   recorded(newpreddialog,_:Widget,Ref),
   erase(Ref),
   mysetof(Ref1,X^(recorded(newpred,X,Ref1),erase(Ref1)),_),
   xtDestroyWidget(Widget).

addselectedRule(_,_,_):-
   (   recorded(newpred,np(_,NC,Pos,Neg,TR,selected),_) ->
       recorded(newpreddialog,ID:Widget,Ref0),
       delete_clause(ID),
       store_clause(NC,_,newpred,ID),
       mysetof(P,PID^(member(P,Pos),store_ex(P,+,PID)),_),            
       mysetof(N,NID^(member(N,Neg),store_ex(N,-,NID)),_),
       assertz(kb:TR),
       erase(Ref0),
       mysetof(Ref1,X^(recorded(newpred,X,Ref1),erase(Ref1)),_),
       xtDestroyWidget(Widget),
       refresh([rules,examples])
   ;   true
   ).            



%************************************************************************
%*
%* predicate:  resultAddSpec/1			result procedure
%*
%* syntax: resultAddNewpreds(Reflist)
%*
%* args:
%*
%* description: Displays a window with alternative specialisations
%*       through new predicates, and allows to choose one to add to the kb
%*
%************************************************************************

resultAddSpec((ID,Reflist)):-
   toplevel(Shell),
   xmCreateBulletinBoardDialog(Shell,'Specs',
	 [],SpecsDialog),
   recordz(specsdialog,ID:SpecsDialog,_),
   xtManageChild(SpecsDialog),

   xmCreateFrame(SpecsDialog,'Specs Frame',
	         [],SpecsFrame),
   xtManageChild(SpecsFrame),

   xmCreateRowColumn(SpecsFrame,specsColumn,
	  [xmNorientation(xmVERTICAL),xmNpacking(xmPACK_TIGHT)],
	  SpecsColumn),
   xtManageChild(SpecsColumn),

   xmCreateRowColumn(SpecsColumn,titlerow,
	  [xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN),
	   xmNisAligned(true),xmNentryAlignment(xmALIGNMENT_CENTER)],
	  TitleRow),
   xtManageChild(TitleRow),

   proxtGetDefaultCharset(DCharset),
   proxtStringToCharPtr('Choose Specialisation',TitleCP),
   xmStringCreate(TitleCP,DCharset,TitleStr),
   xmCreateLabelGadget(TitleRow,title,
	  [xmNlabelType(xmSTRING),xmNlabelString(TitleStr)],
	  TitleLabel),
   xtManageChild(TitleLabel),

   xmCreateScrolledWindow(SpecsColumn,specsSW,
	  [xmNheight(300),xmNwidth(300),
	   xmNlistSizePolicy(xmCONSTANT),
	   xmNscrollBarDisplayPolicy(xmSTATIC),
	   xmNscrollingPolicy(xmAUTOMATIC)],
	  SpecsSW),
   xtManageChild(SpecsSW),

   xmCreateRowColumn(SpecsSW,newpredRC,
	  [xmNadjustLast(false),
	   xmNorientation(xmVERTICAL),
	   xmNpacking(xmPACK_TIGHT),
	   xmNrowColumnType(xmWORK_AREA)],
	  SpecsRC),
   xtGetValues(SpecsRC,[xmNbackground(B)]),
   xtGetValues(SpecsSW,[xmNclipWindow(CW)]),
   xtSetValues(CW,[xmNbackground(B)]),
   xtManageChild(SpecsRC),
   addspecclauses(Reflist,SpecsRC),

   xmCreateRowColumn(SpecsColumn,buttonrow,
	  [xmNorientation(xmHORIZONTAL),xmNpacking(xmPACK_COLUMN),
	   xmNisAligned(true),xmNentryAlignment(xmALIGNMENT_CENTER)],
	  ButtonRow),
   xtManageChild(ButtonRow),

   xmCreatePushButton(ButtonRow,'Add Rule',
	  [],AddRule),
   xtManageChild(AddRule),
   xtAddCallback(AddRule,xmNactivateCallback,
	          addselectedSpec,_),

   xmCreatePushButton(ButtonRow,'None',
	  [],None),
   xtManageChild(None),
   xtAddCallback(None,xmNactivateCallback,
	              addnospec,_).	


addspecclauses([],_).
addspecclauses([NC|R],Widget):-
   proxtGetDefaultCharset(DCharset),
   xxmWriteToString(portray_clause(NC),DCharset,XmS),
   xmCreateLabel(Widget,specClause,
	    [xmNalignment(xmALIGNMENT_BEGINNING),
	     xmNlabelString(XmS),xmNlabelType(xmSTRING)],
	     Label),
    xtManageChild(Label),
    recordz(spec,np(Label,NC,notselected),_Ref),
    xtAddEventHandler(Label,[buttonReleaseMask],
	              false,selectspecclause,_),
    addspecclauses(R,Widget).


selectspecclause(Widget,_,_CallData) :-
   recorded(spec,np(Widget,NC,notselected),Ref),
   turnoff_other_selected_spec,
   xtGetValues(Widget,[xmNbackground(B),xmNforeground(F)]),
   xtSetValues(Widget,[xmNbackground(F),xmNforeground(B)]),
   erase(Ref),
   recordz(spec,np(Widget,NC,selected),_).

selectspecclause(Widget,rules,_CallData) :-
   recorded(spec,np(Widget,NC,selected),Ref),
   xtGetValues(Widget,[xmNbackground(B),xmNforeground(F)]),
   xtSetValues(Widget,[xmNbackground(F),xmNforeground(B)]),
   erase(Ref),
   recordz(spec,np(Widget,NC,notselected),_).

turnoff_other_selected_spec:-
   mysetof(Ref,Widget^NC^B^F^(
                recorded(spec,np(Widget,NC,selected),Ref),
                erase(Ref),
                recordz(spec,np(Widget,NC,notselected),_),
                xtGetValues(Widget,[xmNbackground(B),xmNforeground(F)]),
                xtSetValues(Widget,[xmNbackground(F),xmNforeground(B)])),_).

    
addnospec(_,_,_):-
   recorded(specsdialog,_:Widget,Ref),
   erase(Ref),
   mysetof(Ref1,X^(recorded(spec,X,Ref1),erase(Ref1)),_),
   xtDestroyWidget(Widget).

addselectedSpec(_,_,_):-
   (   recorded(spec,np(_,NC,selected),_) ->
       recorded(specsdialog,ID:Widget,Ref0),
       delete_clause(ID),
       store_clause(NC,_,spec,ID),
       erase(Ref0),
       mysetof(Ref1,X^(recorded(spec,X,Ref1),erase(Ref1)),_),
       xtDestroyWidget(Widget),
       refresh([rules])
   ;   true
   ).            

%************************************************************************
%*
%* predicate: resultAddRuleList/1		result procedure
%*
%* syntax:
%*
%* args: +IdList				rule ids to add
%*
%* description: Adds the rules specified by IdList to the rule list
%*
%************************************************************************

resultAddRuleList([]):-!.
resultAddRuleList([Id|IdList]) :-
	resultAddRule(Id),
	resultAddRuleList(IdList).


%************************************************************************
%*
%* predicate: resultSelectRules/1		result procedure
%*
%* syntax:
%*
%* args: +IdList				rule ids to select	
%*
%* description: Selects the rules specified by IdList in the rule list. 
%*
%************************************************************************

resultSelectRules([]):- unselectAll(_,rules,_),!,
	writelnMessage('% resulting rules selected').
resultSelectRules([Id:_|IdList]) :-
	resultSelectRules(IdList),
	recorded(current,clause(Id,Widget,notselected),_),
	selectClause(Widget,rules,_).


%************************************************************************
%*
%* predicate: resultSelectExamples/1		result procedure
%*
%* syntax:
%*
%* args: +IdList				rule ids to select	
%*
%* description: Selects the examples specified by IdList in the rule list
%*
%************************************************************************

resultSelectExamples([]):- unselectAll(_,examples,_),!,
	writelnMessage('% resulting examples selected').
resultSelectExamples([Id|IdList]) :-
	resultSelectExamples(IdList),
	recorded(current,example(Id,Widget,notselected),_),
	selectClause(Widget,examples,_).


%************************************************************************
%*
%* predicate: changeRule/3			Callback procedure
%*
%* syntax:
%*
%* args:       _Widget				calling widget
%*	       _ClientData					
%*	       _CallData			event	
%*
%* description: Changes the rule in the editor to the rule list.
%*
%************************************************************************

changeRule(_Widget,_ClientData,_CallData) :-
	recorded(editor,textWidget(EditorText),_Ref),
	recorded(editor,editing(rules,ID),_),
	delete_clause(ID),
	xmTextGetString(EditorText,RuleCP),
	xxmStringToTerm(RuleCP,_,RuleTerm),
	store_clause(RuleTerm,_,user,ID),
	proxtCharPtrToString(RuleCP,RulePString),
	span_left(RulePString,".",N),
	substring(RulePString,RuleString,0,N,_),
	writeMessage(':- delete_clause('),
	writeMessage(ID),
	writelnMessage(').'),
	writeMessage(':- store_clause('),
	writeMessage(RuleString),
	writeMessage(',_,user,'),
	writeMessage(ID),
	writelnMessage(').'),
	writelnMessage('% rule changed.'),
	get_clause(ID,H,B,S,L),
	addRuleItem(ID,H,B,S,L),
        updateEvaluationLabel.


%************************************************************************
%*
%* predicate: addExample/3			Callback procedure	
%*
%* syntax:
%*
%* args:       _Widget				calling widget	
%*	       _ClientData					
%*	       _CallData			event
%*
%*
%* description: Adds the example in the editor to the example list.	
%*
%************************************************************************

addExample(_Widget,_ClientData,_CallData) :-
	recorded(editor,textWidget(EditorText),_Ref),	
	xmTextGetString(EditorText,ExampleCP),
	xxmStringToTerm(ExampleCP,_,ExampleTerm),
	store_ex(ExampleTerm,?,ID),
	proxtCharPtrToString(ExampleCP,ExamplePString),
	span_left(ExamplePString,".",N),
	substring(ExamplePString,ExampleString,0,N,_),
	writeMessage(':- store_ex('),
	writeMessage(ExampleString),
	writeMessage(',?,'),
	writeMessage(ID),
	writelnMessage(').'),
	writelnMessage('% example added.'),
	get_example(ID,F,C),
	addExampleItem(ID,F,C),
        updateEvaluationLabel.


%************************************************************************
%*
%* predicate: changeExample/3			Callback procedure
%*
%* syntax:
%*
%* args:       _Widget				calling widget	
%*	       _ClientData					
%*	       _CallData			event
%*
%* description: Changes the example in the editor to the example list.
%*
%************************************************************************

changeExample(_Widget,_ClientData,_CallData) :-
	recorded(editor,textWidget(EditorText),_Ref),
	recorded(editor,editing(examples,ID),_),
	get_example(ID,_,C),
	delete_example(ID),
	xmTextGetString(EditorText,ExampleCP),
	xxmStringToTerm(ExampleCP,_,ExampleTerm),
	proxtCharPtrToString(ExampleCP,ExamplePString),
	span_left(ExamplePString,".",N),
	substring(ExamplePString,ExampleString,0,N,_),
	store_ex(ExampleTerm,C,ID),
	writeMessage(':- delete_example('),
	writeMessage(ID),
	writelnMessage(').'),
	writeMessage(':- store_ex('),
	writeMessage(ExampleString),
	writeMessage(',?,'),
	writeMessage(ID),
	writelnMessage(').'),
	writelnMessage('% example changed.'),
	get_example(ID,F,C),
	addExampleItem(ID,F,C),
        updateEvaluationLabel.


%************************************************************************
%*
%* predicate:  xxmStringToTerm/3
%*
%* syntax:
%*
%* args:       +S		xmCharPtr	
%*	       Charset		xmCharset	
%*	       +T		Term	
%*
%* description: Conversion of xmCharPtrs and Prolog Terms.	
%*
%************************************************************************

xxmStringToTerm(S,_Charset,T) :-
	var(T),nonvar(S),
	proxtCharPtrToString(S,X1),
	tell(xmTemporary),
	write(X1),
	nl,
	told,
	see(xmTemporary),
	read(T),
	seen,!.

xxmStringToTerm(XmS,Charset,T) :-
	var(XmS),nonvar(T),
	tell(xmTemporary),
	write(T),
	told,
	see(xmTemporary),
	xxmStringRead(XmS,Charset),
	seen,!.


%************************************************************************
%*
%* predicate: xxmStringRead/2
%*
%* syntax:
%*
%* args:       +S		xmString	
%*	       +Charset		xmCharset	
%*
%*
%* description: Reads a string from current input.
%*
%************************************************************************

xxmStringRead(S,Charset) :-
	stringRead(100,S1),
	atom_chars(AS,S1),
	proxtStringToCharPtr(AS,CP),
	xmStringCreateLtoR(CP,Charset,XmS),
	(length(S1,100) -> xxmStringAppendRead(XmS,S,Charset)
	| otherwise     -> S = XmS),!.


%************************************************************************
%*
%* predicate: xxmStringAppendRead/3
%*
%* syntax:
%*
%* args:       +S1		xmString, prefix	
%*	       +S		xmString	
%*	       Charset		xmCharset	
%*
%* description: Reads an xmString from the current input and appends it
%*		to S1.
%*
%************************************************************************

xxmStringAppendRead(S1,S,Charset) :-
	stringRead(100,S2),
	atom_chars(AS,S2),
	proxtStringToCharPtr(AS,CP),
	xmStringCreateLtoR(CP,Charset,XmS),
	xmStringConcat(S1,XmS,XmS1),
	(length(S2,100) -> xxmStringAppendRead(XmS1,S,Charset)
	| otherwise     -> S = XmS1),!.


%************************************************************************
%*
%* predicate: stringRead/2
%*
%* syntax: stringRead(+N,+S)
%*
%* description: Reads N characters from the current input.
%*
%************************************************************************

stringRead(0,[]).
stringRead(N,S) :-
	get0(Char),
	(Char =:= -1  -> S = []
	| otherwise -> M is N - 1,
	               stringRead(M,S1),
		       S = [Char|S1]).


%************************************************************************
%*
%* predicate: xxmWriteToString/3
%*
%* syntax:
%*
%* args:       +W		writing procedure	
%*	       +Charset		xmCharset		
%*	       -XmS		xmString	
%*
%* description: Uses the writing procedure W to generate XmS.	
%*
%************************************************************************

xxmWriteToString(W,Charset,XmS) :-
	tell(xmTemporary),
	call(W),
	told,
	see(xmTemporary),
	xxmStringRead(XmS,Charset),
	seen,!.


%************************************************************************
%*
%* predicate: xxmWriteToCharPtr/3
%*
%* syntax:
%*
%* args:       +W		writing procedure	
%*	       +Charset		xmCharset		
%*	       -CP		xmCharPtr
%*
%* description: Uses the writing procedure W to generate CP.
%*
%************************************************************************

xxmWriteToCharPtr(W,CP) :-
	tell(xmTemporary),
	call(W),
	told,
	see(xmTemporary),
	stringRead(1023,S),
	seen,
	atom_chars(AS,S),
	proxtStringToCharPtr(AS,CP),!.


%************************************************************************
%*
%* predicate: isDepth/1
%*
%* syntax: isDepth(+D)
%*
%* args:
%*
%* description: Checks if D is within the range of 1-100.	
%*
%************************************************************************

isDepth(D) :-
 	(number(D), D>0, D<101|
 	 (number(D) -> number_chars(D,M1)|
          otherwise  -> atom_chars(D,M1)),
	 writeError('% '),
   	 append("'",M1,M2),
	 append(M2,"' is not between the range of 1 - 100!",M3),
	 atom_chars(Message,M3),
	 writelnError(Message),!,fail).


%************************************************************************
%*
%* predicate: isExample/1
%*
%* syntax: isExample(+Id)
%*
%* args:
%*
%* description: checks whether Id specifies an example
%*
%************************************************************************

isExample(Id) :-
	(get_example(Id,_,_) |
 	 (number(Id) -> number_chars(Id,M1)|
          otherwise  -> atom_chars(Id,M1)),
	 writeError('% '),
	 append("'",M1,M2),
	 append(M2,"' is not an example id!",M3),
	 atom_chars(Message,M3),
	 writelnError(Message),!,fail).


%************************************************************************
%*
%* predicate: isRule
%*
%* syntax: isRule(+Id)
%*
%* args:
%*
%* description: checks whether Id specifies a rule
%*
%************************************************************************

isRule(Id) :-
	(get_clause(Id,_,_,_,_) |
 	 (number(Id) -> number_chars(Id,M1)|
          otherwise  -> atom_chars(Id,M1)),
	 writeError('% '),
         append("'",M1,M2),
	 append(M2,"' is not a rule id!",M3),
	 atom_chars(Message,M3),
	 writelnError(Message),!,fail).


%************************************************************************
%*
%* predicate: isExOrRule/1
%*
%* syntax: isExOrRule(+Id)
%*
%* args:
%*
%* description: checks whether Id specifies a rule or an example
%*
%************************************************************************

isExOrRule(Id) :-
	(get_clause(Id,_,_,_,_) | get_example(Id,_,_) |
 	 (number(Id) -> number_chars(Id,M1)|
          otherwise  -> atom_chars(Id,M1)),
	 writeError('% '),
         append("'",M1,M2),
	 append(M2,"' is not a rule id nor an example id!",M3),
	 atom_chars(Message,M3),
	 writelnError(Message),fail).


%************************************************************************
%*
%* predicate: sucheInListe/1
%*
%* syntax:
%*
%* args:        +Element	term		
%*		+List		list of terms	
%*
%* description: searches Element in List
%*
%************************************************************************

sucheInListe(A,[A|_]).
sucheInListe(_,[]).
sucheInListe(A,[_|Rest]) :- sucheInListe(A,Rest).

writeFullstop(X) :- write(X),write('.').
true(_).



%************************************************************************
%*
%* predicate: createEvaluationString/1
%*
%* syntax:
%*
%* args:       -String		list of terms	
%*
%* description: 
%*
%************************************************************************

createEvaluationString(S):-
   (   evaluated(no) ->
       S = '(not evaluated)'
   ;   S = '    (evaluated)'
   ).


%************************************************************************
%*
%* predicate: updateEvaluationLabel/0
%*
%* syntax:
%*
%* args:       
%*
%* description: updates the label that indicates whether the kb is
%*              evaluated
%*
%************************************************************************

updateEvaluationLabel:-
   recorded(irene,Widget,_),
   createEvaluationString(S),
   proxtGetDefaultCharset(DCharset),
   proxtStringToCharPtr(S,ACP),
   xmStringCreate(ACP,DCharset,AXmS),
   xtSetValues(Widget,[xmNlabelString(AXmS)]).
  