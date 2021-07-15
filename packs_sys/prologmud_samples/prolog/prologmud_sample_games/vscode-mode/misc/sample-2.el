((("ESC ESC"
   (command vscode-mode-command/workbench\.action\.exitZenMode)
   (when-orig . "inZenMode")
   (when vscode-mode-cond/inZenMode)))
 (("S-ESC"
   (command vscode-mode-command/closeReferenceSearch)
   (when-orig . "inReferenceSearchEditor && !config.editor.stablePeek")
   (when and
	 (vscode-mode-cond/inReferenceSearchEditor)
	 (not
	  (vscode-mode-cond/config\.editor\.stablePeek)))))
 (("ESC"
   (command vscode-mode-command/closeReferenceSearch)
   (when-orig . "inReferenceSearchEditor && !config.editor.stablePeek")
   (when and
	 (vscode-mode-cond/inReferenceSearchEditor)
	 (not
	  (vscode-mode-cond/config\.editor\.stablePeek)))))
 (("S-ESC"
   (command vscode-mode-command/cancelSelection)
   (when-orig . "editorHasSelection && textInputFocus")
   (when and
	 (vscode-mode-cond/editorHasSelection)
	 (vscode-mode-cond/textInputFocus))))
 (("ESC"
   (command vscode-mode-command/cancelSelection)
   (when-orig . "editorHasSelection && textInputFocus")
   (when and
	 (vscode-mode-cond/editorHasSelection)
	 (vscode-mode-cond/textInputFocus))))
 (("C-end"
   (command vscode-mode-command/cursorBottom)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("C-S-end"
   (command vscode-mode-command/cursorBottomSelect)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("C-S-M-<down>"
   (command vscode-mode-command/cursorColumnSelectDown)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("C-S-M-<left>"
   (command vscode-mode-command/cursorColumnSelectLeft)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("C-S-M-"
   (command vscode-mode-command/cursorColumnSelectPageDown)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("C-S-M-"
   (command vscode-mode-command/cursorColumnSelectPageUp)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("C-S-M-<right>"
   (command vscode-mode-command/cursorColumnSelectRight)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("C-S-M-<up>"
   (command vscode-mode-command/cursorColumnSelectUp)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("<down>"
   (command vscode-mode-command/cursorDown)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("C-S-<down>"
   (command vscode-mode-command/cursorDownSelect)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("S-<down>"
   (command vscode-mode-command/cursorDownSelect)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("end"
   (command vscode-mode-command/cursorEnd)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("S-end"
   (command vscode-mode-command/cursorEndSelect)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("home"
   (command vscode-mode-command/cursorHome)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("S-home"
   (command vscode-mode-command/cursorHomeSelect)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("<left>"
   (command vscode-mode-command/cursorLeft)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("S-<left>"
   (command vscode-mode-command/cursorLeftSelect)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 ((""
   (command vscode-mode-command/cursorPageDown)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("S-"
   (command vscode-mode-command/cursorPageDownSelect)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 ((""
   (command vscode-mode-command/cursorPageUp)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("S-"
   (command vscode-mode-command/cursorPageUpSelect)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("<right>"
   (command vscode-mode-command/cursorRight)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("S-<right>"
   (command vscode-mode-command/cursorRightSelect)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("C-home"
   (command vscode-mode-command/cursorTop)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("C-S-home"
   (command vscode-mode-command/cursorTopSelect)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("<up>"
   (command vscode-mode-command/cursorUp)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("C-S-<up>"
   (command vscode-mode-command/cursorUpSelect)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("S-<up>"
   (command vscode-mode-command/cursorUpSelect)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("S-DEL"
   (command vscode-mode-command/deleteLeft)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("DEL"
   (command vscode-mode-command/deleteLeft)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("delete"
   (command vscode-mode-command/deleteRight)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("C-a"
   (command vscode-mode-command/editor\.action\.selectAll)
   (when-orig)
   (when)))
 (("C-l"
   (command vscode-mode-command/expandLineSelection)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("S-tab"
   (command vscode-mode-command/outdent)
   (when-orig . "editorTextFocus && !editorReadonly && !editorTabMovesFocus")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly))
	 (not
	  (vscode-mode-cond/editorTabMovesFocus)))))
 (("C-S-z"
   (command vscode-mode-command/redo)
   (when-orig)
   (when)))
 (("C-y"
   (command vscode-mode-command/redo)
   (when-orig)
   (when)))
 (("C-<down>"
   (command vscode-mode-command/scrollLineDown)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("C-<up>"
   (command vscode-mode-command/scrollLineUp)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("M-"
   (command vscode-mode-command/scrollPageDown)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("M-"
   (command vscode-mode-command/scrollPageUp)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("tab"
   (command vscode-mode-command/tab)
   (when-orig . "editorTextFocus && !editorReadonly && !editorTabMovesFocus")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly))
	 (not
	  (vscode-mode-cond/editorTabMovesFocus)))))
 (("C-z"
   (command vscode-mode-command/undo)
   (when-orig)
   (when)))
 (("S-<down>"
   (command vscode-mode-command/cursorColumnSelectDown)
   (when-orig . "editorColumnSelection && textInputFocus")
   (when and
	 (vscode-mode-cond/editorColumnSelection)
	 (vscode-mode-cond/textInputFocus))))
 (("S-<left>"
   (command vscode-mode-command/cursorColumnSelectLeft)
   (when-orig . "editorColumnSelection && textInputFocus")
   (when and
	 (vscode-mode-cond/editorColumnSelection)
	 (vscode-mode-cond/textInputFocus))))
 (("S-"
   (command vscode-mode-command/cursorColumnSelectPageDown)
   (when-orig . "editorColumnSelection && textInputFocus")
   (when and
	 (vscode-mode-cond/editorColumnSelection)
	 (vscode-mode-cond/textInputFocus))))
 (("S-"
   (command vscode-mode-command/cursorColumnSelectPageUp)
   (when-orig . "editorColumnSelection && textInputFocus")
   (when and
	 (vscode-mode-cond/editorColumnSelection)
	 (vscode-mode-cond/textInputFocus))))
 (("S-<right>"
   (command vscode-mode-command/cursorColumnSelectRight)
   (when-orig . "editorColumnSelection && textInputFocus")
   (when and
	 (vscode-mode-cond/editorColumnSelection)
	 (vscode-mode-cond/textInputFocus))))
 (("S-<up>"
   (command vscode-mode-command/cursorColumnSelectUp)
   (when-orig . "editorColumnSelection && textInputFocus")
   (when and
	 (vscode-mode-cond/editorColumnSelection)
	 (vscode-mode-cond/textInputFocus))))
 (("S-ESC"
   (command vscode-mode-command/removeSecondaryCursors)
   (when-orig . "editorHasMultipleSelections && textInputFocus")
   (when and
	 (vscode-mode-cond/editorHasMultipleSelections)
	 (vscode-mode-cond/textInputFocus))))
 (("ESC"
   (command vscode-mode-command/removeSecondaryCursors)
   (when-orig . "editorHasMultipleSelections && textInputFocus")
   (when and
	 (vscode-mode-cond/editorHasMultipleSelections)
	 (vscode-mode-cond/textInputFocus))))
 (("f12"
   (command vscode-mode-command/goToNextReference)
   (when-orig . "inReferenceSearchEditor || referenceSearchVisible")
   (when or
	 (vscode-mode-cond/inReferenceSearchEditor)
	 (vscode-mode-cond/referenceSearchVisible))))
 (("f4"
   (command vscode-mode-command/goToNextReference)
   (when-orig . "inReferenceSearchEditor || referenceSearchVisible")
   (when or
	 (vscode-mode-cond/inReferenceSearchEditor)
	 (vscode-mode-cond/referenceSearchVisible))))
 (("S-f12"
   (command vscode-mode-command/goToPreviousReference)
   (when-orig . "inReferenceSearchEditor || referenceSearchVisible")
   (when or
	 (vscode-mode-cond/inReferenceSearchEditor)
	 (vscode-mode-cond/referenceSearchVisible))))
 (("S-f4"
   (command vscode-mode-command/goToPreviousReference)
   (when-orig . "inReferenceSearchEditor || referenceSearchVisible")
   (when or
	 (vscode-mode-cond/inReferenceSearchEditor)
	 (vscode-mode-cond/referenceSearchVisible))))
 (("S-RET"
   (command vscode-mode-command/refactorPreview\.apply)
   (when-orig . "refactorPreview.enabled && refactorPreview.enabled && refactorPreview.hasCheckedChanges && focusedView == 'refactorPreview'")
   (when and
	 (vscode-mode-cond/refactorPreview\.enabled)
	 (vscode-mode-cond/refactorPreview\.enabled)
	 (vscode-mode-cond/refactorPreview\.hasCheckedChanges)
	 (equal
	  (vscode-mode-cond/focusedView)
	  "refactorPreview"))))
 (("ESC"
   (command vscode-mode-command/notebook\.cell\.quitEdit)
   (when-orig . "inputFocus && notebookEditorFocused && !editorHasMultipleSelections && !editorHasSelection && !editorHoverVisible")
   (when and
	 (vscode-mode-cond/inputFocus)
	 (vscode-mode-cond/notebookEditorFocused)
	 (not
	  (vscode-mode-cond/editorHasMultipleSelections))
	 (not
	  (vscode-mode-cond/editorHasSelection))
	 (not
	  (vscode-mode-cond/editorHoverVisible)))))
 (("C-f"
   (command vscode-mode-command/actions\.find)
   (when-orig . "editorFocus || editorIsOpen")
   (when or
	 (vscode-mode-cond/editorFocus)
	 (vscode-mode-cond/editorIsOpen))))
 (("RET"
   (command vscode-mode-command/breakpointWidget\.action\.acceptInput)
   (when-orig . "breakpointWidgetVisible && inBreakpointWidget")
   (when and
	 (vscode-mode-cond/breakpointWidgetVisible)
	 (vscode-mode-cond/inBreakpointWidget))))
 (("S-ESC"
   (command vscode-mode-command/closeBreakpointWidget)
   (when-orig . "breakpointWidgetVisible && textInputFocus")
   (when and
	 (vscode-mode-cond/breakpointWidgetVisible)
	 (vscode-mode-cond/textInputFocus))))
 (("ESC"
   (command vscode-mode-command/closeBreakpointWidget)
   (when-orig . "breakpointWidgetVisible && textInputFocus")
   (when and
	 (vscode-mode-cond/breakpointWidgetVisible)
	 (vscode-mode-cond/textInputFocus))))
 (("C-u"
   (command vscode-mode-command/cursorUndo)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("C-<right>"
   (command vscode-mode-command/cursorWordEndRight)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("C-S-<right>"
   (command vscode-mode-command/cursorWordEndRightSelect)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("C-<left>"
   (command vscode-mode-command/cursorWordLeft)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("C-S-<left>"
   (command vscode-mode-command/cursorWordLeftSelect)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("C-DEL"
   (command vscode-mode-command/deleteWordLeft)
   (when-orig . "textInputFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/textInputFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("C-delete"
   (command vscode-mode-command/deleteWordRight)
   (when-orig . "textInputFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/textInputFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("C-k C-c"
   (command vscode-mode-command/editor\.action\.addCommentLine)
   (when-orig . "editorTextFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("C-d"
   (command vscode-mode-command/editor\.action\.addSelectionToNextFindMatch)
   (when-orig . "editorFocus")
   (when vscode-mode-cond/editorFocus)))
 (("S-M-."
   (command vscode-mode-command/editor\.action\.autoFix)
   (when-orig . "editorTextFocus && !editorReadonly && supportedCodeAction =~ /(\\s|^)quickfix\\b/")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly))
	 (string-match
	  (vscode-mode-cond/supportedCodeAction)
	  "/(\\s|^)quickfix\\b/" nil nil))))
 (("S-M-a"
   (command vscode-mode-command/editor\.action\.blockComment)
   (when-orig . "editorTextFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("ESC"
   (command vscode-mode-command/editor\.action\.cancelSelectionAnchor)
   (when-orig . "editorTextFocus && selectionAnchorSet")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/selectionAnchorSet))))
 (("C-f2"
   (command vscode-mode-command/editor\.action\.changeAll)
   (when-orig . "editorTextFocus && editorTextFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("C-insert"
   (command vscode-mode-command/editor\.action\.clipboardCopyAction)
   (when-orig)
   (when)))
 (("C-c"
   (command vscode-mode-command/editor\.action\.clipboardCopyAction)
   (when-orig)
   (when)))
 (("S-delete"
   (command vscode-mode-command/editor\.action\.clipboardCutAction)
   (when-orig)
   (when)))
 (("C-x"
   (command vscode-mode-command/editor\.action\.clipboardCutAction)
   (when-orig)
   (when)))
 (("S-insert"
   (command vscode-mode-command/editor\.action\.clipboardPasteAction)
   (when-orig)
   (when)))
 (("C-v"
   (command vscode-mode-command/editor\.action\.clipboardPasteAction)
   (when-orig)
   (when)))
 (("C-/"
   (command vscode-mode-command/editor\.action\.commentLine)
   (when-orig . "editorTextFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("S-M-<down>"
   (command vscode-mode-command/editor\.action\.copyLinesDownAction)
   (when-orig . "editorTextFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("S-M-<up>"
   (command vscode-mode-command/editor\.action\.copyLinesUpAction)
   (when-orig . "editorTextFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("C-k C-k"
   (command vscode-mode-command/editor\.action\.defineKeybinding)
   (when-orig . "editorTextFocus && !editorReadonly && editorLangId == 'jsonc'")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly))
	 (equal
	  (vscode-mode-cond/editorLangId)
	  "jsonc"))))
 (("C-S-k"
   (command vscode-mode-command/editor\.action\.deleteLines)
   (when-orig . "textInputFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/textInputFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("f7"
   (command vscode-mode-command/editor\.action\.diffReview\.next)
   (when-orig . "isInDiffEditor")
   (when vscode-mode-cond/isInDiffEditor)))
 (("S-f7"
   (command vscode-mode-command/editor\.action\.diffReview\.prev)
   (when-orig . "isInDiffEditor")
   (when vscode-mode-cond/isInDiffEditor)))
 (("M-f3"
   (command vscode-mode-command/editor\.action\.dirtydiff\.next)
   (when-orig . "editorTextFocus")
   (when vscode-mode-cond/editorTextFocus)))
 (("S-M-f3"
   (command vscode-mode-command/editor\.action\.dirtydiff\.previous)
   (when-orig . "editorTextFocus")
   (when vscode-mode-cond/editorTextFocus)))
 (("RET"
   (command vscode-mode-command/editor\.action\.extensioneditor\.findNext)
   (when-orig . "webviewFindWidgetFocused && !editorFocus && activeEditor == 'workbench.editor.extension'")
   (when and
	 (vscode-mode-cond/webviewFindWidgetFocused)
	 (not
	  (vscode-mode-cond/editorFocus))
	 (equal
	  (vscode-mode-cond/activeEditor)
	  "workbench.editor.extension"))))
 (("S-RET"
   (command vscode-mode-command/editor\.action\.extensioneditor\.findPrevious)
   (when-orig . "webviewFindWidgetFocused && !editorFocus && activeEditor == 'workbench.editor.extension'")
   (when and
	 (vscode-mode-cond/webviewFindWidgetFocused)
	 (not
	  (vscode-mode-cond/editorFocus))
	 (equal
	  (vscode-mode-cond/activeEditor)
	  "workbench.editor.extension"))))
 (("C-f"
   (command vscode-mode-command/editor\.action\.extensioneditor\.showfind)
   (when-orig . "!editorFocus && activeEditor == 'workbench.editor.extension'")
   (when and
	 (not
	  (vscode-mode-cond/editorFocus))
	 (equal
	  (vscode-mode-cond/activeEditor)
	  "workbench.editor.extension"))))
 (("S-M-f"
   (command vscode-mode-command/editor\.action\.formatDocument)
   (when-orig . "editorHasDocumentFormattingProvider && editorTextFocus && !editorReadonly && !inCompositeEditor")
   (when and
	 (vscode-mode-cond/editorHasDocumentFormattingProvider)
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly))
	 (not
	  (vscode-mode-cond/inCompositeEditor)))))
 (("S-M-f"
   (command vscode-mode-command/editor\.action\.formatDocument\.none)
   (when-orig . "editorTextFocus && !editorHasDocumentFormattingProvider && !editorReadonly")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorHasDocumentFormattingProvider))
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("C-k C-f"
   (command vscode-mode-command/editor\.action\.formatSelection)
   (when-orig . "editorHasDocumentSelectionFormattingProvider && editorTextFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/editorHasDocumentSelectionFormattingProvider)
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("C-f12"
   (command vscode-mode-command/editor\.action\.goToImplementation)
   (when-orig . "editorHasImplementationProvider && editorTextFocus && !isInEmbeddedEditor")
   (when and
	 (vscode-mode-cond/editorHasImplementationProvider)
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/isInEmbeddedEditor)))))
 (("S-f12"
   (command vscode-mode-command/editor\.action\.goToReferences)
   (when-orig . "editorHasReferenceProvider && editorTextFocus && !inReferenceSearchEditor && !isInEmbeddedEditor")
   (when and
	 (vscode-mode-cond/editorHasReferenceProvider)
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/inReferenceSearchEditor))
	 (not
	  (vscode-mode-cond/isInEmbeddedEditor)))))
 (("C-S-."
   (command vscode-mode-command/editor\.action\.inPlaceReplace\.down)
   (when-orig . "editorTextFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("C-S-,"
   (command vscode-mode-command/editor\.action\.inPlaceReplace\.up)
   (when-orig . "editorTextFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("C-]"
   (command vscode-mode-command/editor\.action\.indentLines)
   (when-orig . "editorTextFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("C-M-<up>"
   (command vscode-mode-command/editor\.action\.insertCursorAbove)
   (when-orig . "editorTextFocus")
   (when vscode-mode-cond/editorTextFocus)))
 (("S-M-i"
   (command vscode-mode-command/editor\.action\.insertCursorAtEndOfEachLineSelected)
   (when-orig . "editorTextFocus")
   (when vscode-mode-cond/editorTextFocus)))
 (("C-M-<down>"
   (command vscode-mode-command/editor\.action\.insertCursorBelow)
   (when-orig . "editorTextFocus")
   (when vscode-mode-cond/editorTextFocus)))
 (("C-RET"
   (command vscode-mode-command/editor\.action\.insertLineAfter)
   (when-orig . "editorTextFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("C-S-RET"
   (command vscode-mode-command/editor\.action\.insertLineBefore)
   (when-orig . "editorTextFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("C-S-\\"
   (command vscode-mode-command/editor\.action\.jumpToBracket)
   (when-orig . "editorTextFocus")
   (when vscode-mode-cond/editorTextFocus)))
 (("C-S-f2"
   (command vscode-mode-command/editor\.action\.linkedEditing)
   (when-orig . "editorHasRenameProvider && editorTextFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/editorHasRenameProvider)
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("M-f8"
   (command vscode-mode-command/editor\.action\.marker\.next)
   (when-orig . "editorFocus")
   (when vscode-mode-cond/editorFocus)))
 (("f8"
   (command vscode-mode-command/editor\.action\.marker\.nextInFiles)
   (when-orig . "editorFocus")
   (when vscode-mode-cond/editorFocus)))
 (("S-M-f8"
   (command vscode-mode-command/editor\.action\.marker\.prev)
   (when-orig . "editorFocus")
   (when vscode-mode-cond/editorFocus)))
 (("S-f8"
   (command vscode-mode-command/editor\.action\.marker\.prevInFiles)
   (when-orig . "editorFocus")
   (when vscode-mode-cond/editorFocus)))
 (("M-<down>"
   (command vscode-mode-command/editor\.action\.moveLinesDownAction)
   (when-orig . "editorTextFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("M-<up>"
   (command vscode-mode-command/editor\.action\.moveLinesUpAction)
   (when-orig . "editorTextFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("C-k C-d"
   (command vscode-mode-command/editor\.action\.moveSelectionToNextFindMatch)
   (when-orig . "editorFocus")
   (when vscode-mode-cond/editorFocus)))
 (("f3"
   (command vscode-mode-command/editor\.action\.nextMatchFindAction)
   (when-orig . "editorFocus")
   (when vscode-mode-cond/editorFocus)))
 (("RET"
   (command vscode-mode-command/editor\.action\.nextMatchFindAction)
   (when-orig . "editorFocus && findInputFocussed")
   (when and
	 (vscode-mode-cond/editorFocus)
	 (vscode-mode-cond/findInputFocussed))))
 (("C-f3"
   (command vscode-mode-command/editor\.action\.nextSelectionMatchFindAction)
   (when-orig . "editorFocus")
   (when vscode-mode-cond/editorFocus)))
 (("S-M-o"
   (command vscode-mode-command/editor\.action\.organizeImports)
   (when-orig . "editorTextFocus && !editorReadonly && supportedCodeAction =~ /(\\s|^)source\\.organizeImports\\b/")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly))
	 (string-match
	  (vscode-mode-cond/supportedCodeAction)
	  "/(\\s|^)source\\.organizeImports\\b/" nil nil))))
 (("C-["
   (command vscode-mode-command/editor\.action\.outdentLines)
   (when-orig . "editorTextFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("M-f12"
   (command vscode-mode-command/editor\.action\.peekDefinition)
   (when-orig . "editorHasDefinitionProvider && editorTextFocus && !inReferenceSearchEditor && !isInEmbeddedEditor")
   (when and
	 (vscode-mode-cond/editorHasDefinitionProvider)
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/inReferenceSearchEditor))
	 (not
	  (vscode-mode-cond/isInEmbeddedEditor)))))
 (("C-S-f12"
   (command vscode-mode-command/editor\.action\.peekImplementation)
   (when-orig . "editorHasImplementationProvider && editorTextFocus && !inReferenceSearchEditor && !isInEmbeddedEditor")
   (when and
	 (vscode-mode-cond/editorHasImplementationProvider)
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/inReferenceSearchEditor))
	 (not
	  (vscode-mode-cond/isInEmbeddedEditor)))))
 (("S-f3"
   (command vscode-mode-command/editor\.action\.previousMatchFindAction)
   (when-orig . "editorFocus")
   (when vscode-mode-cond/editorFocus)))
 (("S-RET"
   (command vscode-mode-command/editor\.action\.previousMatchFindAction)
   (when-orig . "editorFocus && findInputFocussed")
   (when and
	 (vscode-mode-cond/editorFocus)
	 (vscode-mode-cond/findInputFocussed))))
 (("C-S-f3"
   (command vscode-mode-command/editor\.action\.previousSelectionMatchFindAction)
   (when-orig . "editorFocus")
   (when vscode-mode-cond/editorFocus)))
 (("C-."
   (command vscode-mode-command/editor\.action\.quickFix)
   (when-orig . "editorHasCodeActionsProvider && editorTextFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/editorHasCodeActionsProvider)
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("C-S-r"
   (command vscode-mode-command/editor\.action\.refactor)
   (when-orig . "editorHasCodeActionsProvider && editorTextFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/editorHasCodeActionsProvider)
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("C-k C-u"
   (command vscode-mode-command/editor\.action\.removeCommentLine)
   (when-orig . "editorTextFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("f2"
   (command vscode-mode-command/editor\.action\.rename)
   (when-orig . "editorHasRenameProvider && editorTextFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/editorHasRenameProvider)
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("f12"
   (command vscode-mode-command/editor\.action\.revealDefinition)
   (when-orig . "editorHasDefinitionProvider && editorTextFocus && !isInEmbeddedEditor")
   (when and
	 (vscode-mode-cond/editorHasDefinitionProvider)
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/isInEmbeddedEditor)))))
 (("C-k f12"
   (command vscode-mode-command/editor\.action\.revealDefinitionAside)
   (when-orig . "editorHasDefinitionProvider && editorTextFocus && !isInEmbeddedEditor")
   (when and
	 (vscode-mode-cond/editorHasDefinitionProvider)
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/isInEmbeddedEditor)))))
 (("C-k C-k"
   (command vscode-mode-command/editor\.action\.selectFromAnchorToCursor)
   (when-orig . "editorTextFocus && selectionAnchorSet")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/selectionAnchorSet))))
 (("C-S-l"
   (command vscode-mode-command/editor\.action\.selectHighlights)
   (when-orig . "editorFocus")
   (when vscode-mode-cond/editorFocus)))
 (("C-k C-b"
   (command vscode-mode-command/editor\.action\.setSelectionAnchor)
   (when-orig . "editorTextFocus")
   (when vscode-mode-cond/editorTextFocus)))
 (("M-f1"
   (command vscode-mode-command/editor\.action\.showAccessibilityHelp)
   (when-orig)
   (when)))
 (("S-f10"
   (command vscode-mode-command/editor\.action\.showContextMenu)
   (when-orig . "textInputFocus")
   (when vscode-mode-cond/textInputFocus)))
 (("C-k C-i"
   (command vscode-mode-command/editor\.action\.showHover)
   (when-orig . "editorTextFocus")
   (when vscode-mode-cond/editorTextFocus)))
 (("S-M-<right>"
   (command vscode-mode-command/editor\.action\.smartSelect\.expand)
   (when-orig . "editorTextFocus")
   (when vscode-mode-cond/editorTextFocus)))
 (("S-M-<left>"
   (command vscode-mode-command/editor\.action\.smartSelect\.shrink)
   (when-orig . "editorTextFocus")
   (when vscode-mode-cond/editorTextFocus)))
 (("C-h"
   (command vscode-mode-command/editor\.action\.startFindReplaceAction)
   (when-orig . "editorFocus || editorIsOpen")
   (when or
	 (vscode-mode-cond/editorFocus)
	 (vscode-mode-cond/editorIsOpen))))
 (("C-m"
   (command vscode-mode-command/editor\.action\.toggleTabFocusMode)
   (when-orig)
   (when)))
 (("M-z"
   (command vscode-mode-command/editor\.action\.toggleWordWrap)
   (when-orig)
   (when)))
 (("C-S-SPC"
   (command vscode-mode-command/editor\.action\.triggerParameterHints)
   (when-orig . "editorHasSignatureHelpProvider && editorTextFocus")
   (when and
	 (vscode-mode-cond/editorHasSignatureHelpProvider)
	 (vscode-mode-cond/editorTextFocus))))
 (("C-i"
   (command vscode-mode-command/editor\.action\.triggerSuggest)
   (when-orig . "editorHasCompletionItemProvider && textInputFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/editorHasCompletionItemProvider)
	 (vscode-mode-cond/textInputFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("C-SPC"
   (command vscode-mode-command/editor\.action\.triggerSuggest)
   (when-orig . "editorHasCompletionItemProvider && textInputFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/editorHasCompletionItemProvider)
	 (vscode-mode-cond/textInputFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("C-k C-x"
   (command vscode-mode-command/editor\.action\.trimTrailingWhitespace)
   (when-orig . "editorTextFocus && !editorReadonly")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly)))))
 (("RET"
   (command vscode-mode-command/editor\.action\.webvieweditor\.findNext)
   (when-orig . "webviewFindWidgetFocused && !editorFocus && activeEditor == 'WebviewEditor'")
   (when and
	 (vscode-mode-cond/webviewFindWidgetFocused)
	 (not
	  (vscode-mode-cond/editorFocus))
	 (equal
	  (vscode-mode-cond/activeEditor)
	  "WebviewEditor"))))
 (("S-RET"
   (command vscode-mode-command/editor\.action\.webvieweditor\.findPrevious)
   (when-orig . "webviewFindWidgetFocused && !editorFocus && activeEditor == 'WebviewEditor'")
   (when and
	 (vscode-mode-cond/webviewFindWidgetFocused)
	 (not
	  (vscode-mode-cond/editorFocus))
	 (equal
	  (vscode-mode-cond/activeEditor)
	  "WebviewEditor"))))
 (("ESC"
   (command vscode-mode-command/editor\.action\.webvieweditor\.hideFind)
   (when-orig . "webviewFindWidgetVisible && !editorFocus && activeEditor == 'WebviewEditor'")
   (when and
	 (vscode-mode-cond/webviewFindWidgetVisible)
	 (not
	  (vscode-mode-cond/editorFocus))
	 (equal
	  (vscode-mode-cond/activeEditor)
	  "WebviewEditor"))))
 (("C-f"
   (command vscode-mode-command/editor\.action\.webvieweditor\.showFind)
   (when-orig . "!editorFocus && activeEditor == 'WebviewEditor'")
   (when and
	 (not
	  (vscode-mode-cond/editorFocus))
	 (equal
	  (vscode-mode-cond/activeEditor)
	  "WebviewEditor"))))
 (("f7"
   (command vscode-mode-command/editor\.action\.wordHighlight\.next)
   (when-orig . "editorTextFocus && hasWordHighlights")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/hasWordHighlights))))
 (("S-f7"
   (command vscode-mode-command/editor\.action\.wordHighlight\.prev)
   (when-orig . "editorTextFocus && hasWordHighlights")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/hasWordHighlights))))
 (("ESC"
   (command vscode-mode-command/editor\.cancelOperation)
   (when-orig . "cancellableOperation")
   (when vscode-mode-cond/cancellableOperation)))
 (("ESC"
   (command vscode-mode-command/editor\.debug\.action\.closeExceptionWidget)
   (when-orig . "exceptionWidgetVisible")
   (when vscode-mode-cond/exceptionWidgetVisible)))
 (("C-k C-i"
   (command vscode-mode-command/editor\.debug\.action\.showDebugHover)
   (when-orig . "editorTextFocus && inDebugMode")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/inDebugMode))))
 (("f9"
   (command vscode-mode-command/editor\.debug\.action\.toggleBreakpoint)
   (when-orig . "debuggersAvailable && editorTextFocus")
   (when and
	 (vscode-mode-cond/debuggersAvailable)
	 (vscode-mode-cond/editorTextFocus))))
 (("tab"
   (command vscode-mode-command/editor\.emmet\.action\.expandAbbreviation)
   (when-orig . "config.emmet.triggerExpansionOnTab && editorTextFocus && !editorReadonly && !editorTabMovesFocus")
   (when and
	 (vscode-mode-cond/config\.emmet\.triggerExpansionOnTab)
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/editorReadonly))
	 (not
	  (vscode-mode-cond/editorTabMovesFocus)))))
 (("C-S-["
   (command vscode-mode-command/editor\.fold)
   (when-orig . "editorTextFocus && foldingEnabled")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/foldingEnabled))))
 (("C-k C-0"
   (command vscode-mode-command/editor\.foldAll)
   (when-orig . "editorTextFocus && foldingEnabled")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/foldingEnabled))))
 (("C-k C-/"
   (command vscode-mode-command/editor\.foldAllBlockComments)
   (when-orig . "editorTextFocus && foldingEnabled")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/foldingEnabled))))
 (("C-k C-8"
   (command vscode-mode-command/editor\.foldAllMarkerRegions)
   (when-orig . "editorTextFocus && foldingEnabled")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/foldingEnabled))))
 (("C-k C-1"
   (command vscode-mode-command/editor\.foldLevel1)
   (when-orig . "editorTextFocus && foldingEnabled")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/foldingEnabled))))
 (("C-k C-2"
   (command vscode-mode-command/editor\.foldLevel2)
   (when-orig . "editorTextFocus && foldingEnabled")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/foldingEnabled))))
 (("C-k C-3"
   (command vscode-mode-command/editor\.foldLevel3)
   (when-orig . "editorTextFocus && foldingEnabled")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/foldingEnabled))))
 (("C-k C-4"
   (command vscode-mode-command/editor\.foldLevel4)
   (when-orig . "editorTextFocus && foldingEnabled")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/foldingEnabled))))
 (("C-k C-5"
   (command vscode-mode-command/editor\.foldLevel5)
   (when-orig . "editorTextFocus && foldingEnabled")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/foldingEnabled))))
 (("C-k C-6"
   (command vscode-mode-command/editor\.foldLevel6)
   (when-orig . "editorTextFocus && foldingEnabled")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/foldingEnabled))))
 (("C-k C-7"
   (command vscode-mode-command/editor\.foldLevel7)
   (when-orig . "editorTextFocus && foldingEnabled")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/foldingEnabled))))
 (("C-k C-["
   (command vscode-mode-command/editor\.foldRecursively)
   (when-orig . "editorTextFocus && foldingEnabled")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/foldingEnabled))))
 (("f12"
   (command vscode-mode-command/editor\.gotoNextSymbolFromResult)
   (when-orig . "hasSymbols")
   (when vscode-mode-cond/hasSymbols)))
 (("ESC"
   (command vscode-mode-command/editor\.gotoNextSymbolFromResult\.cancel)
   (when-orig . "hasSymbols")
   (when vscode-mode-cond/hasSymbols)))
 (("C-k C-l"
   (command vscode-mode-command/editor\.toggleFold)
   (when-orig . "editorTextFocus && foldingEnabled")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/foldingEnabled))))
 (("C-S-]"
   (command vscode-mode-command/editor\.unfold)
   (when-orig . "editorTextFocus && foldingEnabled")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/foldingEnabled))))
 (("C-k C-j"
   (command vscode-mode-command/editor\.unfoldAll)
   (when-orig . "editorTextFocus && foldingEnabled")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/foldingEnabled))))
 (("C-k C-9"
   (command vscode-mode-command/editor\.unfoldAllMarkerRegions)
   (when-orig . "editorTextFocus && foldingEnabled")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/foldingEnabled))))
 (("C-k C-]"
   (command vscode-mode-command/editor\.unfoldRecursively)
   (when-orig . "editorTextFocus && foldingEnabled")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/foldingEnabled))))
 (("tab"
   (command vscode-mode-command/insertSnippet)
   (when-orig . "editorTextFocus && hasSnippetCompletions && !editorTabMovesFocus && !inSnippetMode")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/hasSnippetCompletions)
	 (not
	  (vscode-mode-cond/editorTabMovesFocus))
	 (not
	  (vscode-mode-cond/inSnippetMode)))))
 (("C-M-RET"
   (command vscode-mode-command/notebook\.cell\.execute)
   (when-orig . "notebookCellListFocused")
   (when vscode-mode-cond/notebookCellListFocused)))
 (("M-RET"
   (command vscode-mode-command/notebook\.cell\.executeAndInsertBelow)
   (when-orig . "notebookCellListFocused")
   (when vscode-mode-cond/notebookCellListFocused)))
 (("S-RET"
   (command vscode-mode-command/notebook\.cell\.executeAndSelectBelow)
   (when-orig . "notebookCellListFocused")
   (when vscode-mode-cond/notebookCellListFocused)))
 (("C-S-v"
   (command vscode-mode-command/notebook\.cell\.pasteAbove)
   (when-orig . "notebookEditorFocused && !inputFocus")
   (when and
	 (vscode-mode-cond/notebookEditorFocused)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("<down>"
   (command vscode-mode-command/notebook\.focusNextEditor)
   (when-orig . "editorTextFocus && inputFocus && notebookEditorFocused && notebookEditorCursorAtBoundary != 'none' && notebookEditorCursorAtBoundary != 'top'")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/inputFocus)
	 (vscode-mode-cond/notebookEditorFocused)
	 (not
	  (equal
	   (vscode-mode-cond/notebookEditorCursorAtBoundary)
	   "none"))
	 (not
	  (equal
	   (vscode-mode-cond/notebookEditorCursorAtBoundary)
	   "top")))))
 (("<up>"
   (command vscode-mode-command/notebook\.focusPreviousEditor)
   (when-orig . "editorTextFocus && inputFocus && notebookEditorFocused && notebookEditorCursorAtBoundary != 'bottom' && notebookEditorCursorAtBoundary != 'none'")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/inputFocus)
	 (vscode-mode-cond/notebookEditorFocused)
	 (not
	  (equal
	   (vscode-mode-cond/notebookEditorCursorAtBoundary)
	   "bottom"))
	 (not
	  (equal
	   (vscode-mode-cond/notebookEditorCursorAtBoundary)
	   "none")))))
 (("S-M-f"
   (command vscode-mode-command/notebook\.formatCell)
   (when-orig . "editorHasDocumentFormattingProvider && editorTextFocus && inCompositeEditor && notebookEditable && !editorReadonly && activeEditor == 'workbench.editor.notebook'")
   (when and
	 (vscode-mode-cond/editorHasDocumentFormattingProvider)
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/inCompositeEditor)
	 (vscode-mode-cond/notebookEditable)
	 (not
	  (vscode-mode-cond/editorReadonly))
	 (equal
	  (vscode-mode-cond/activeEditor)
	  "workbench.editor.notebook"))))
 (("C-RET"
   (command vscode-mode-command/openReferenceToSide)
   (when-orig . "listFocus && referenceSearchVisible && !inputFocus")
   (when and
	 (vscode-mode-cond/listFocus)
	 (vscode-mode-cond/referenceSearchVisible)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("RET"
   (command vscode-mode-command/repl\.action\.acceptInput)
   (when-orig . "inDebugRepl && textInputFocus")
   (when and
	 (vscode-mode-cond/inDebugRepl)
	 (vscode-mode-cond/textInputFocus))))
 (("C-f"
   (command vscode-mode-command/repl\.action\.filter)
   (when-orig . "inDebugRepl && textInputFocus")
   (when and
	 (vscode-mode-cond/inDebugRepl)
	 (vscode-mode-cond/textInputFocus))))
 (("C-S-r"
   (command vscode-mode-command/rerunSearchEditorSearch)
   (when-orig . "inSearchEditor")
   (when vscode-mode-cond/inSearchEditor)))
 (("ESC"
   (command vscode-mode-command/search\.action\.focusQueryEditorWidget)
   (when-orig . "inSearchEditor")
   (when vscode-mode-cond/inSearchEditor)))
 (("C-S-DEL"
   (command vscode-mode-command/search\.searchEditor\.action\.deleteFileResults)
   (when-orig . "inSearchEditor")
   (when vscode-mode-cond/inSearchEditor)))
 (("ESC"
   (command vscode-mode-command/settings\.action\.clearSearchResults)
   (when-orig . "inSettingsEditor && inSettingsSearch")
   (when and
	 (vscode-mode-cond/inSettingsEditor)
	 (vscode-mode-cond/inSettingsSearch))))
 (("C-."
   (command vscode-mode-command/settings\.action\.editFocusedSetting)
   (when-orig . "inSettingsSearch")
   (when vscode-mode-cond/inSettingsSearch)))
 (("RET"
   (command vscode-mode-command/settings\.action\.focusNextSetting)
   (when-orig . "inSettingsSearch")
   (when vscode-mode-cond/inSettingsSearch)))
 (("S-RET"
   (command vscode-mode-command/settings\.action\.focusPreviousSetting)
   (when-orig . "inSettingsSearch")
   (when vscode-mode-cond/inSettingsSearch)))
 (("<down>"
   (command vscode-mode-command/settings\.action\.focusSettingsFile)
   (when-orig . "inSettingsSearch && !suggestWidgetVisible")
   (when and
	 (vscode-mode-cond/inSettingsSearch)
	 (not
	  (vscode-mode-cond/suggestWidgetVisible)))))
 (("C-f"
   (command vscode-mode-command/settings\.action\.search)
   (when-orig . "inSettingsEditor")
   (when vscode-mode-cond/inSettingsEditor)))
 (("C-/"
   (command vscode-mode-command/toggleExplainMode)
   (when-orig . "suggestWidgetVisible")
   (when vscode-mode-cond/suggestWidgetVisible)))
 (("C-k f2"
   (command vscode-mode-command/togglePeekWidgetFocus)
   (when-orig . "inReferenceSearchEditor || referenceSearchVisible")
   (when or
	 (vscode-mode-cond/inReferenceSearchEditor)
	 (vscode-mode-cond/referenceSearchVisible))))
 (("M-f5"
   (command vscode-mode-command/workbench\.action\.editor\.nextChange)
   (when-orig . "editorTextFocus")
   (when vscode-mode-cond/editorTextFocus)))
 (("S-M-f5"
   (command vscode-mode-command/workbench\.action\.editor\.previousChange)
   (when-orig . "editorTextFocus")
   (when vscode-mode-cond/editorTextFocus)))
 (("S-ESC"
   (command vscode-mode-command/workbench\.action\.hideComment)
   (when-orig . "commentEditorFocused")
   (when vscode-mode-cond/commentEditorFocused)))
 (("ESC"
   (command vscode-mode-command/workbench\.action\.hideComment)
   (when-orig . "commentEditorFocused")
   (when vscode-mode-cond/commentEditorFocused)))
 (("C-RET"
   (command vscode-mode-command/workbench\.action\.submitComment)
   (when-orig . "commentEditorFocused")
   (when vscode-mode-cond/commentEditorFocused)))
 (("C-<left>"
   (command vscode-mode-command/cursorWordAccessibilityLeft)
   (when-orig . "accessibilityModeEnabled && textInputFocus")
   (when and
	 (vscode-mode-cond/accessibilityModeEnabled)
	 (vscode-mode-cond/textInputFocus))))
 (("C-S-<left>"
   (command vscode-mode-command/cursorWordAccessibilityLeftSelect)
   (when-orig . "accessibilityModeEnabled && textInputFocus")
   (when and
	 (vscode-mode-cond/accessibilityModeEnabled)
	 (vscode-mode-cond/textInputFocus))))
 (("C-<right>"
   (command vscode-mode-command/cursorWordAccessibilityRight)
   (when-orig . "accessibilityModeEnabled && textInputFocus")
   (when and
	 (vscode-mode-cond/accessibilityModeEnabled)
	 (vscode-mode-cond/textInputFocus))))
 (("C-S-<right>"
   (command vscode-mode-command/cursorWordAccessibilityRightSelect)
   (when-orig . "accessibilityModeEnabled && textInputFocus")
   (when and
	 (vscode-mode-cond/accessibilityModeEnabled)
	 (vscode-mode-cond/textInputFocus))))
 (("S-ESC"
   (command vscode-mode-command/closeFindWidget)
   (when-orig . "editorFocus && findWidgetVisible && !isComposing")
   (when and
	 (vscode-mode-cond/editorFocus)
	 (vscode-mode-cond/findWidgetVisible)
	 (not
	  (vscode-mode-cond/isComposing)))))
 (("ESC"
   (command vscode-mode-command/closeFindWidget)
   (when-orig . "editorFocus && findWidgetVisible && !isComposing")
   (when and
	 (vscode-mode-cond/editorFocus)
	 (vscode-mode-cond/findWidgetVisible)
	 (not
	  (vscode-mode-cond/isComposing)))))
 (("C-M-RET"
   (command vscode-mode-command/editor\.action\.replaceAll)
   (when-orig . "editorFocus && findWidgetVisible")
   (when and
	 (vscode-mode-cond/editorFocus)
	 (vscode-mode-cond/findWidgetVisible))))
 (("C-S-1"
   (command vscode-mode-command/editor\.action\.replaceOne)
   (when-orig . "editorFocus && findWidgetVisible")
   (when and
	 (vscode-mode-cond/editorFocus)
	 (vscode-mode-cond/findWidgetVisible))))
 (("RET"
   (command vscode-mode-command/editor\.action\.replaceOne)
   (when-orig . "editorFocus && findWidgetVisible && replaceInputFocussed")
   (when and
	 (vscode-mode-cond/editorFocus)
	 (vscode-mode-cond/findWidgetVisible)
	 (vscode-mode-cond/replaceInputFocussed))))
 (("M-RET"
   (command vscode-mode-command/editor\.action\.selectAllMatches)
   (when-orig . "editorFocus && findWidgetVisible")
   (when and
	 (vscode-mode-cond/editorFocus)
	 (vscode-mode-cond/findWidgetVisible))))
 (("M-c"
   (command vscode-mode-command/toggleFindCaseSensitive)
   (when-orig . "editorFocus")
   (when vscode-mode-cond/editorFocus)))
 (("M-l"
   (command vscode-mode-command/toggleFindInSelection)
   (when-orig . "editorFocus")
   (when vscode-mode-cond/editorFocus)))
 (("M-r"
   (command vscode-mode-command/toggleFindRegex)
   (when-orig . "editorFocus")
   (when vscode-mode-cond/editorFocus)))
 (("M-w"
   (command vscode-mode-command/toggleFindWholeWord)
   (when-orig . "editorFocus")
   (when vscode-mode-cond/editorFocus)))
 (("M-p"
   (command vscode-mode-command/togglePreserveCase)
   (when-orig . "editorFocus")
   (when vscode-mode-cond/editorFocus)))
 (("tab"
   (command vscode-mode-command/jumpToNextSnippetPlaceholder)
   (when-orig . "editorTextFocus && hasNextTabstop && inSnippetMode")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/hasNextTabstop)
	 (vscode-mode-cond/inSnippetMode))))
 (("S-tab"
   (command vscode-mode-command/jumpToPrevSnippetPlaceholder)
   (when-orig . "editorTextFocus && hasPrevTabstop && inSnippetMode")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/hasPrevTabstop)
	 (vscode-mode-cond/inSnippetMode))))
 (("ESC"
   (command vscode-mode-command/leaveEditorMessage)
   (when-orig . "messageVisible")
   (when vscode-mode-cond/messageVisible)))
 (("S-ESC"
   (command vscode-mode-command/leaveSnippet)
   (when-orig . "editorTextFocus && inSnippetMode")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/inSnippetMode))))
 (("ESC"
   (command vscode-mode-command/leaveSnippet)
   (when-orig . "editorTextFocus && inSnippetMode")
   (when and
	 (vscode-mode-cond/editorTextFocus)
	 (vscode-mode-cond/inSnippetMode))))
 (("S-ESC"
   (command vscode-mode-command/closeDirtyDiff)
   (when-orig . "dirtyDiffVisible")
   (when vscode-mode-cond/dirtyDiffVisible)))
 (("ESC"
   (command vscode-mode-command/closeDirtyDiff)
   (when-orig . "dirtyDiffVisible")
   (when vscode-mode-cond/dirtyDiffVisible)))
 (("S-ESC"
   (command vscode-mode-command/closeMarkersNavigation)
   (when-orig . "editorFocus && markersNavigationVisible")
   (when and
	 (vscode-mode-cond/editorFocus)
	 (vscode-mode-cond/markersNavigationVisible))))
 (("ESC"
   (command vscode-mode-command/closeMarkersNavigation)
   (when-orig . "editorFocus && markersNavigationVisible")
   (when and
	 (vscode-mode-cond/editorFocus)
	 (vscode-mode-cond/markersNavigationVisible))))
 (("ESC"
   (command vscode-mode-command/notifications\.hideToasts)
   (when-orig . "notificationToastsVisible")
   (when vscode-mode-cond/notificationToastsVisible)))
 (("S-ESC"
   (command vscode-mode-command/closeParameterHints)
   (when-orig . "editorFocus && parameterHintsVisible")
   (when and
	 (vscode-mode-cond/editorFocus)
	 (vscode-mode-cond/parameterHintsVisible))))
 (("ESC"
   (command vscode-mode-command/closeParameterHints)
   (when-orig . "editorFocus && parameterHintsVisible")
   (when and
	 (vscode-mode-cond/editorFocus)
	 (vscode-mode-cond/parameterHintsVisible))))
 (("M-<down>"
   (command vscode-mode-command/showNextParameterHint)
   (when-orig . "editorFocus && parameterHintsMultipleSignatures && parameterHintsVisible")
   (when and
	 (vscode-mode-cond/editorFocus)
	 (vscode-mode-cond/parameterHintsMultipleSignatures)
	 (vscode-mode-cond/parameterHintsVisible))))
 (("<down>"
   (command vscode-mode-command/showNextParameterHint)
   (when-orig . "editorFocus && parameterHintsMultipleSignatures && parameterHintsVisible")
   (when and
	 (vscode-mode-cond/editorFocus)
	 (vscode-mode-cond/parameterHintsMultipleSignatures)
	 (vscode-mode-cond/parameterHintsVisible))))
 (("M-<up>"
   (command vscode-mode-command/showPrevParameterHint)
   (when-orig . "editorFocus && parameterHintsMultipleSignatures && parameterHintsVisible")
   (when and
	 (vscode-mode-cond/editorFocus)
	 (vscode-mode-cond/parameterHintsMultipleSignatures)
	 (vscode-mode-cond/parameterHintsVisible))))
 (("<up>"
   (command vscode-mode-command/showPrevParameterHint)
   (when-orig . "editorFocus && parameterHintsMultipleSignatures && parameterHintsVisible")
   (when and
	 (vscode-mode-cond/editorFocus)
	 (vscode-mode-cond/parameterHintsMultipleSignatures)
	 (vscode-mode-cond/parameterHintsVisible))))
 (("S-tab"
   (command vscode-mode-command/acceptAlternativeSelectedSuggestion)
   (when-orig . "suggestWidgetVisible && textInputFocus && textInputFocus")
   (when and
	 (vscode-mode-cond/suggestWidgetVisible)
	 (vscode-mode-cond/textInputFocus)
	 (vscode-mode-cond/textInputFocus))))
 (("S-RET"
   (command vscode-mode-command/acceptAlternativeSelectedSuggestion)
   (when-orig . "suggestWidgetVisible && textInputFocus && textInputFocus")
   (when and
	 (vscode-mode-cond/suggestWidgetVisible)
	 (vscode-mode-cond/textInputFocus)
	 (vscode-mode-cond/textInputFocus))))
 (("tab"
   (command vscode-mode-command/acceptSelectedSuggestion)
   (when-orig . "suggestWidgetVisible && textInputFocus")
   (when and
	 (vscode-mode-cond/suggestWidgetVisible)
	 (vscode-mode-cond/textInputFocus))))
 (("RET"
   (command vscode-mode-command/acceptSelectedSuggestion)
   (when-orig . "acceptSuggestionOnEnter && suggestWidgetVisible && suggestionMakesTextEdit && textInputFocus")
   (when and
	 (vscode-mode-cond/acceptSuggestionOnEnter)
	 (vscode-mode-cond/suggestWidgetVisible)
	 (vscode-mode-cond/suggestionMakesTextEdit)
	 (vscode-mode-cond/textInputFocus))))
 (("S-ESC"
   (command vscode-mode-command/hideSuggestWidget)
   (when-orig . "suggestWidgetVisible && textInputFocus")
   (when and
	 (vscode-mode-cond/suggestWidgetVisible)
	 (vscode-mode-cond/textInputFocus))))
 (("ESC"
   (command vscode-mode-command/hideSuggestWidget)
   (when-orig . "suggestWidgetVisible && textInputFocus")
   (when and
	 (vscode-mode-cond/suggestWidgetVisible)
	 (vscode-mode-cond/textInputFocus))))
 (("tab"
   (command vscode-mode-command/insertBestCompletion)
   (when-orig . "atEndOfWord && textInputFocus && !hasOtherSuggestions && !inSnippetMode && !suggestWidgetVisible && config.editor.tabCompletion == 'on'")
   (when and
	 (vscode-mode-cond/atEndOfWord)
	 (vscode-mode-cond/textInputFocus)
	 (not
	  (vscode-mode-cond/hasOtherSuggestions))
	 (not
	  (vscode-mode-cond/inSnippetMode))
	 (not
	  (vscode-mode-cond/suggestWidgetVisible))
	 (equal
	  (vscode-mode-cond/config\.editor\.tabCompletion)
	  "on"))))
 (("tab"
   (command vscode-mode-command/insertNextSuggestion)
   (when-orig . "hasOtherSuggestions && textInputFocus && textInputFocus && !inSnippetMode && !suggestWidgetVisible && config.editor.tabCompletion == 'on'")
   (when and
	 (vscode-mode-cond/hasOtherSuggestions)
	 (vscode-mode-cond/textInputFocus)
	 (vscode-mode-cond/textInputFocus)
	 (not
	  (vscode-mode-cond/inSnippetMode))
	 (not
	  (vscode-mode-cond/suggestWidgetVisible))
	 (equal
	  (vscode-mode-cond/config\.editor\.tabCompletion)
	  "on"))))
 (("S-tab"
   (command vscode-mode-command/insertPrevSuggestion)
   (when-orig . "hasOtherSuggestions && textInputFocus && textInputFocus && !inSnippetMode && !suggestWidgetVisible && config.editor.tabCompletion == 'on'")
   (when and
	 (vscode-mode-cond/hasOtherSuggestions)
	 (vscode-mode-cond/textInputFocus)
	 (vscode-mode-cond/textInputFocus)
	 (not
	  (vscode-mode-cond/inSnippetMode))
	 (not
	  (vscode-mode-cond/suggestWidgetVisible))
	 (equal
	  (vscode-mode-cond/config\.editor\.tabCompletion)
	  "on"))))
 (("C-"
   (command vscode-mode-command/selectNextPageSuggestion)
   (when-orig . "suggestWidgetMultipleSuggestions && suggestWidgetVisible && textInputFocus")
   (when and
	 (vscode-mode-cond/suggestWidgetMultipleSuggestions)
	 (vscode-mode-cond/suggestWidgetVisible)
	 (vscode-mode-cond/textInputFocus))))
 ((""
   (command vscode-mode-command/selectNextPageSuggestion)
   (when-orig . "suggestWidgetMultipleSuggestions && suggestWidgetVisible && textInputFocus")
   (when and
	 (vscode-mode-cond/suggestWidgetMultipleSuggestions)
	 (vscode-mode-cond/suggestWidgetVisible)
	 (vscode-mode-cond/textInputFocus))))
 (("C-<down>"
   (command vscode-mode-command/selectNextSuggestion)
   (when-orig . "suggestWidgetMultipleSuggestions && suggestWidgetVisible && textInputFocus")
   (when and
	 (vscode-mode-cond/suggestWidgetMultipleSuggestions)
	 (vscode-mode-cond/suggestWidgetVisible)
	 (vscode-mode-cond/textInputFocus))))
 (("<down>"
   (command vscode-mode-command/selectNextSuggestion)
   (when-orig . "suggestWidgetMultipleSuggestions && suggestWidgetVisible && textInputFocus")
   (when and
	 (vscode-mode-cond/suggestWidgetMultipleSuggestions)
	 (vscode-mode-cond/suggestWidgetVisible)
	 (vscode-mode-cond/textInputFocus))))
 (("C-"
   (command vscode-mode-command/selectPrevPageSuggestion)
   (when-orig . "suggestWidgetMultipleSuggestions && suggestWidgetVisible && textInputFocus")
   (when and
	 (vscode-mode-cond/suggestWidgetMultipleSuggestions)
	 (vscode-mode-cond/suggestWidgetVisible)
	 (vscode-mode-cond/textInputFocus))))
 ((""
   (command vscode-mode-command/selectPrevPageSuggestion)
   (when-orig . "suggestWidgetMultipleSuggestions && suggestWidgetVisible && textInputFocus")
   (when and
	 (vscode-mode-cond/suggestWidgetMultipleSuggestions)
	 (vscode-mode-cond/suggestWidgetVisible)
	 (vscode-mode-cond/textInputFocus))))
 (("C-<up>"
   (command vscode-mode-command/selectPrevSuggestion)
   (when-orig . "suggestWidgetMultipleSuggestions && suggestWidgetVisible && textInputFocus")
   (when and
	 (vscode-mode-cond/suggestWidgetMultipleSuggestions)
	 (vscode-mode-cond/suggestWidgetVisible)
	 (vscode-mode-cond/textInputFocus))))
 (("<up>"
   (command vscode-mode-command/selectPrevSuggestion)
   (when-orig . "suggestWidgetMultipleSuggestions && suggestWidgetVisible && textInputFocus")
   (when and
	 (vscode-mode-cond/suggestWidgetMultipleSuggestions)
	 (vscode-mode-cond/suggestWidgetVisible)
	 (vscode-mode-cond/textInputFocus))))
 (("C-SPC"
   (command vscode-mode-command/toggleSuggestionDetails)
   (when-orig . "suggestWidgetVisible && textInputFocus")
   (when and
	 (vscode-mode-cond/suggestWidgetVisible)
	 (vscode-mode-cond/textInputFocus))))
 (("C-M-SPC"
   (command vscode-mode-command/toggleSuggestionFocus)
   (when-orig . "suggestWidgetVisible && textInputFocus")
   (when and
	 (vscode-mode-cond/suggestWidgetVisible)
	 (vscode-mode-cond/textInputFocus))))
 (("RET"
   (command vscode-mode-command/acceptRenameInput)
   (when-orig . "editorFocus && renameInputVisible")
   (when and
	 (vscode-mode-cond/editorFocus)
	 (vscode-mode-cond/renameInputVisible))))
 (("S-RET"
   (command vscode-mode-command/acceptRenameInputWithPreview)
   (when-orig . "config.editor.rename.enablePreview && editorFocus && renameInputVisible")
   (when and
	 (vscode-mode-cond/config\.editor\.rename\.enablePreview)
	 (vscode-mode-cond/editorFocus)
	 (vscode-mode-cond/renameInputVisible))))
 (("S-ESC"
   (command vscode-mode-command/cancelLinkedEditingInput)
   (when-orig . "LinkedEditingInputVisible && editorTextFocus")
   (when and
	 (vscode-mode-cond/LinkedEditingInputVisible)
	 (vscode-mode-cond/editorTextFocus))))
 (("ESC"
   (command vscode-mode-command/cancelLinkedEditingInput)
   (when-orig . "LinkedEditingInputVisible && editorTextFocus")
   (when and
	 (vscode-mode-cond/LinkedEditingInputVisible)
	 (vscode-mode-cond/editorTextFocus))))
 (("S-ESC"
   (command vscode-mode-command/cancelRenameInput)
   (when-orig . "editorFocus && renameInputVisible")
   (when and
	 (vscode-mode-cond/editorFocus)
	 (vscode-mode-cond/renameInputVisible))))
 (("ESC"
   (command vscode-mode-command/cancelRenameInput)
   (when-orig . "editorFocus && renameInputVisible")
   (when and
	 (vscode-mode-cond/editorFocus)
	 (vscode-mode-cond/renameInputVisible))))
 (("C-S-l"
   (command vscode-mode-command/addCursorsAtSearchResults)
   (when-orig . "fileMatchOrMatchFocus && searchViewletVisible")
   (when and
	 (vscode-mode-cond/fileMatchOrMatchFocus)
	 (vscode-mode-cond/searchViewletVisible))))
 (("C-S-;"
   (command vscode-mode-command/breadcrumbs\.focus)
   (when-orig . "breadcrumbsPossible")
   (when vscode-mode-cond/breadcrumbsPossible)))
 (("C-S-."
   (command vscode-mode-command/breadcrumbs\.focusAndSelect)
   (when-orig . "breadcrumbsPossible")
   (when vscode-mode-cond/breadcrumbsPossible)))
 (("C-<right>"
   (command vscode-mode-command/breadcrumbs\.focusNext)
   (when-orig . "breadcrumbsActive && breadcrumbsVisible")
   (when and
	 (vscode-mode-cond/breadcrumbsActive)
	 (vscode-mode-cond/breadcrumbsVisible))))
 (("<right>"
   (command vscode-mode-command/breadcrumbs\.focusNext)
   (when-orig . "breadcrumbsActive && breadcrumbsVisible")
   (when and
	 (vscode-mode-cond/breadcrumbsActive)
	 (vscode-mode-cond/breadcrumbsVisible))))
 (("C-<left>"
   (command vscode-mode-command/breadcrumbs\.focusPrevious)
   (when-orig . "breadcrumbsActive && breadcrumbsVisible")
   (when and
	 (vscode-mode-cond/breadcrumbsActive)
	 (vscode-mode-cond/breadcrumbsVisible))))
 (("<left>"
   (command vscode-mode-command/breadcrumbs\.focusPrevious)
   (when-orig . "breadcrumbsActive && breadcrumbsVisible")
   (when and
	 (vscode-mode-cond/breadcrumbsActive)
	 (vscode-mode-cond/breadcrumbsVisible))))
 (("C-RET"
   (command vscode-mode-command/breadcrumbs\.revealFocused)
   (when-orig . "breadcrumbsActive && breadcrumbsVisible")
   (when and
	 (vscode-mode-cond/breadcrumbsActive)
	 (vscode-mode-cond/breadcrumbsVisible))))
 (("SPC"
   (command vscode-mode-command/breadcrumbs\.revealFocused)
   (when-orig . "breadcrumbsActive && breadcrumbsVisible")
   (when and
	 (vscode-mode-cond/breadcrumbsActive)
	 (vscode-mode-cond/breadcrumbsVisible))))
 (("C-RET"
   (command vscode-mode-command/breadcrumbs\.revealFocusedFromTreeAside)
   (when-orig . "breadcrumbsActive && breadcrumbsVisible && listFocus && !inputFocus")
   (when and
	 (vscode-mode-cond/breadcrumbsActive)
	 (vscode-mode-cond/breadcrumbsVisible)
	 (vscode-mode-cond/listFocus)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("<down>"
   (command vscode-mode-command/breadcrumbs\.selectFocused)
   (when-orig . "breadcrumbsActive && breadcrumbsVisible")
   (when and
	 (vscode-mode-cond/breadcrumbsActive)
	 (vscode-mode-cond/breadcrumbsVisible))))
 (("RET"
   (command vscode-mode-command/breadcrumbs\.selectFocused)
   (when-orig . "breadcrumbsActive && breadcrumbsVisible")
   (when and
	 (vscode-mode-cond/breadcrumbsActive)
	 (vscode-mode-cond/breadcrumbsVisible))))
 (("C-S-."
   (command vscode-mode-command/breadcrumbs\.toggleToOn)
   (when-orig . "!config.breadcrumbs.enabled")
   (when not
	 (vscode-mode-cond/config\.breadcrumbs\.enabled))))
 (("S-ESC"
   (command vscode-mode-command/closeAccessibilityHelp)
   (when-orig . "accessibilityHelpWidgetVisible && editorFocus")
   (when and
	 (vscode-mode-cond/accessibilityHelpWidgetVisible)
	 (vscode-mode-cond/editorFocus))))
 (("ESC"
   (command vscode-mode-command/closeAccessibilityHelp)
   (when-orig . "accessibilityHelpWidgetVisible && editorFocus")
   (when and
	 (vscode-mode-cond/accessibilityHelpWidgetVisible)
	 (vscode-mode-cond/editorFocus))))
 (("ESC"
   (command vscode-mode-command/closeReplaceInFilesWidget)
   (when-orig . "replaceInputBoxFocus && searchViewletVisible")
   (when and
	 (vscode-mode-cond/replaceInputBoxFocus)
	 (vscode-mode-cond/searchViewletVisible))))
 (("S-M-c"
   (command vscode-mode-command/copyFilePath)
   (when-orig . "!editorFocus")
   (when not
	 (vscode-mode-cond/editorFocus))))
 (("C-k C-S-c"
   (command vscode-mode-command/copyRelativeFilePath)
   (when-orig . "!editorFocus")
   (when not
	 (vscode-mode-cond/editorFocus))))
 (("M-RET"
   (command vscode-mode-command/debug\.openBreakpointToSide)
   (when-orig . "breakpointsFocused")
   (when vscode-mode-cond/breakpointsFocused)))
 (("C-RET"
   (command vscode-mode-command/debug\.openBreakpointToSide)
   (when-orig . "breakpointsFocused")
   (when vscode-mode-cond/breakpointsFocused)))
 (("delete"
   (command vscode-mode-command/debug\.removeBreakpoint)
   (when-orig . "breakpointsFocused && !breakpointSelected")
   (when and
	 (vscode-mode-cond/breakpointsFocused)
	 (not
	  (vscode-mode-cond/breakpointSelected)))))
 (("delete"
   (command vscode-mode-command/debug\.removeWatchExpression)
   (when-orig . "watchExpressionsFocused && !expressionSelected")
   (when and
	 (vscode-mode-cond/watchExpressionsFocused)
	 (not
	  (vscode-mode-cond/expressionSelected)))))
 (("M--"
   (command vscode-mode-command/decreaseSearchEditorContextLines)
   (when-orig . "inSearchEditor")
   (when vscode-mode-cond/inSearchEditor)))
 (("S-f9"
   (command vscode-mode-command/editor\.debug\.action\.toggleInlineBreakpoint)
   (when-orig . "editorTextFocus")
   (when vscode-mode-cond/editorTextFocus)))
 (("S-RET"
   (command vscode-mode-command/editor\.refocusCallHierarchy)
   (when-orig . "callHierarchyVisible")
   (when vscode-mode-cond/callHierarchyVisible)))
 (("S-M-h"
   (command vscode-mode-command/editor\.showCallHierarchy)
   (when-orig . "editorHasCallHierarchyProvider && editorTextFocus && !inReferenceSearchEditor")
   (when and
	 (vscode-mode-cond/editorHasCallHierarchyProvider)
	 (vscode-mode-cond/editorTextFocus)
	 (not
	  (vscode-mode-cond/inReferenceSearchEditor)))))
 (("S-M-h"
   (command vscode-mode-command/editor\.showIncomingCalls)
   (when-orig . "callHierarchyVisible && callHierarchyDirection == 'outgoingCalls'")
   (when and
	 (vscode-mode-cond/callHierarchyVisible)
	 (equal
	  (vscode-mode-cond/callHierarchyDirection)
	  "outgoingCalls"))))
 (("S-M-h"
   (command vscode-mode-command/editor\.showOutgoingCalls)
   (when-orig . "callHierarchyVisible && callHierarchyDirection == 'incomingCalls'")
   (when and
	 (vscode-mode-cond/callHierarchyVisible)
	 (equal
	  (vscode-mode-cond/callHierarchyDirection)
	  "incomingCalls"))))
 (("C-RET"
   (command vscode-mode-command/explorer\.openToSide)
   (when-orig . "explorerViewletFocus && explorerViewletVisible && !inputFocus")
   (when and
	 (vscode-mode-cond/explorerViewletFocus)
	 (vscode-mode-cond/explorerViewletVisible)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("S-M-f"
   (command vscode-mode-command/filesExplorer\.findInFolder)
   (when-orig . "explorerResourceIsFolder && explorerViewletVisible && filesExplorerFocus && !inputFocus")
   (when and
	 (vscode-mode-cond/explorerResourceIsFolder)
	 (vscode-mode-cond/explorerViewletVisible)
	 (vscode-mode-cond/filesExplorerFocus)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("M-<down>"
   (command vscode-mode-command/history\.showNext)
   (when-orig . "historyNavigationEnabled && historyNavigationWidget")
   (when and
	 (vscode-mode-cond/historyNavigationEnabled)
	 (vscode-mode-cond/historyNavigationWidget))))
 (("<down>"
   (command vscode-mode-command/history\.showNext)
   (when-orig . "historyNavigationEnabled && historyNavigationWidget")
   (when and
	 (vscode-mode-cond/historyNavigationEnabled)
	 (vscode-mode-cond/historyNavigationWidget))))
 (("M-<up>"
   (command vscode-mode-command/history\.showPrevious)
   (when-orig . "historyNavigationEnabled && historyNavigationWidget")
   (when and
	 (vscode-mode-cond/historyNavigationEnabled)
	 (vscode-mode-cond/historyNavigationWidget))))
 (("<up>"
   (command vscode-mode-command/history\.showPrevious)
   (when-orig . "historyNavigationEnabled && historyNavigationWidget")
   (when and
	 (vscode-mode-cond/historyNavigationEnabled)
	 (vscode-mode-cond/historyNavigationWidget))))
 (("M-="
   (command vscode-mode-command/increaseSearchEditorContextLines)
   (when-orig . "inSearchEditor")
   (when vscode-mode-cond/inSearchEditor)))
 (("C-k C-a"
   (command vscode-mode-command/keybindings\.editor\.addKeybinding)
   (when-orig . "inKeybindings && keybindingFocus")
   (when and
	 (vscode-mode-cond/inKeybindings)
	 (vscode-mode-cond/keybindingFocus))))
 (("ESC"
   (command vscode-mode-command/keybindings\.editor\.clearSearchResults)
   (when-orig . "inKeybindings && inKeybindingsSearch")
   (when and
	 (vscode-mode-cond/inKeybindings)
	 (vscode-mode-cond/inKeybindingsSearch))))
 (("C-c"
   (command vscode-mode-command/keybindings\.editor\.copyKeybindingEntry)
   (when-orig . "inKeybindings && keybindingFocus")
   (when and
	 (vscode-mode-cond/inKeybindings)
	 (vscode-mode-cond/keybindingFocus))))
 (("C-k C-k"
   (command vscode-mode-command/keybindings\.editor\.defineKeybinding)
   (when-orig . "inKeybindings && keybindingFocus")
   (when and
	 (vscode-mode-cond/inKeybindings)
	 (vscode-mode-cond/keybindingFocus))))
 (("C-k C-e"
   (command vscode-mode-command/keybindings\.editor\.defineWhenExpression)
   (when-orig . "inKeybindings && keybindingFocus")
   (when and
	 (vscode-mode-cond/inKeybindings)
	 (vscode-mode-cond/keybindingFocus))))
 (("C-<down>"
   (command vscode-mode-command/keybindings\.editor\.focusKeybindings)
   (when-orig . "inKeybindings && inKeybindingsSearch")
   (when and
	 (vscode-mode-cond/inKeybindings)
	 (vscode-mode-cond/inKeybindingsSearch))))
 (("M-k"
   (command vscode-mode-command/keybindings\.editor\.recordSearchKeys)
   (when-orig . "inKeybindings && inKeybindingsSearch")
   (when and
	 (vscode-mode-cond/inKeybindings)
	 (vscode-mode-cond/inKeybindingsSearch))))
 (("delete"
   (command vscode-mode-command/keybindings\.editor\.removeKeybinding)
   (when-orig . "inKeybindings && keybindingFocus")
   (when and
	 (vscode-mode-cond/inKeybindings)
	 (vscode-mode-cond/keybindingFocus))))
 (("C-f"
   (command vscode-mode-command/keybindings\.editor\.searchKeybindings)
   (when-orig . "inKeybindings")
   (when vscode-mode-cond/inKeybindings)))
 (("M-p"
   (command vscode-mode-command/keybindings\.editor\.toggleSortByPrecedence)
   (when-orig . "inKeybindings")
   (when vscode-mode-cond/inKeybindings)))
 (("ESC"
   (command vscode-mode-command/list\.clear)
   (when-orig . "listFocus && listHasSelectionOrFocus && !inputFocus")
   (when and
	 (vscode-mode-cond/listFocus)
	 (vscode-mode-cond/listHasSelectionOrFocus)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("<left>"
   (command vscode-mode-command/list\.collapse)
   (when-orig . "listFocus && !inputFocus")
   (when and
	 (vscode-mode-cond/listFocus)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("C-<left>"
   (command vscode-mode-command/list\.collapseAll)
   (when-orig . "listFocus && !inputFocus")
   (when and
	 (vscode-mode-cond/listFocus)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("<right>"
   (command vscode-mode-command/list\.expand)
   (when-orig . "listFocus && !inputFocus")
   (when and
	 (vscode-mode-cond/listFocus)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("S-<down>"
   (command vscode-mode-command/list\.expandSelectionDown)
   (when-orig . "listFocus && listSupportsMultiselect && !inputFocus")
   (when and
	 (vscode-mode-cond/listFocus)
	 (vscode-mode-cond/listSupportsMultiselect)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("S-<up>"
   (command vscode-mode-command/list\.expandSelectionUp)
   (when-orig . "listFocus && listSupportsMultiselect && !inputFocus")
   (when and
	 (vscode-mode-cond/listFocus)
	 (vscode-mode-cond/listSupportsMultiselect)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("<down>"
   (command vscode-mode-command/list\.focusDown)
   (when-orig . "listFocus && !inputFocus")
   (when and
	 (vscode-mode-cond/listFocus)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("home"
   (command vscode-mode-command/list\.focusFirst)
   (when-orig . "listFocus && !inputFocus")
   (when and
	 (vscode-mode-cond/listFocus)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("end"
   (command vscode-mode-command/list\.focusLast)
   (when-orig . "listFocus && !inputFocus")
   (when and
	 (vscode-mode-cond/listFocus)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 ((""
   (command vscode-mode-command/list\.focusPageDown)
   (when-orig . "listFocus && !inputFocus")
   (when and
	 (vscode-mode-cond/listFocus)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 ((""
   (command vscode-mode-command/list\.focusPageUp)
   (when-orig . "listFocus && !inputFocus")
   (when and
	 (vscode-mode-cond/listFocus)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("<up>"
   (command vscode-mode-command/list\.focusUp)
   (when-orig . "listFocus && !inputFocus")
   (when and
	 (vscode-mode-cond/listFocus)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("C-<down>"
   (command vscode-mode-command/list\.scrollDown)
   (when-orig . "listFocus && !inputFocus")
   (when and
	 (vscode-mode-cond/listFocus)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("C-<up>"
   (command vscode-mode-command/list\.scrollUp)
   (when-orig . "listFocus && !inputFocus")
   (when and
	 (vscode-mode-cond/listFocus)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("RET"
   (command vscode-mode-command/list\.select)
   (when-orig . "listFocus && !inputFocus")
   (when and
	 (vscode-mode-cond/listFocus)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("C-a"
   (command vscode-mode-command/list\.selectAll)
   (when-orig . "listFocus && listSupportsMultiselect && !inputFocus")
   (when and
	 (vscode-mode-cond/listFocus)
	 (vscode-mode-cond/listSupportsMultiselect)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("SPC"
   (command vscode-mode-command/list\.toggleExpand)
   (when-orig . "listFocus && !inputFocus")
   (when and
	 (vscode-mode-cond/listFocus)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("C-S-RET"
   (command vscode-mode-command/list\.toggleSelection)
   (when-orig . "listFocus && !inputFocus")
   (when and
	 (vscode-mode-cond/listFocus)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("y"
   (command vscode-mode-command/notebook\.cell\.changeToCode)
   (when-orig . "notebookEditorFocused && !inputFocus && activeEditor == 'workbench.editor.notebook' && notebookCellType == 'markdown'")
   (when and
	 (vscode-mode-cond/notebookEditorFocused)
	 (not
	  (vscode-mode-cond/inputFocus))
	 (equal
	  (vscode-mode-cond/activeEditor)
	  "workbench.editor.notebook")
	 (equal
	  (vscode-mode-cond/notebookCellType)
	  "markdown"))))
 (("m"
   (command vscode-mode-command/notebook\.cell\.changeToMarkdown)
   (when-orig . "notebookEditorFocused && !inputFocus && activeEditor == 'workbench.editor.notebook' && notebookCellType == 'code'")
   (when and
	 (vscode-mode-cond/notebookEditorFocused)
	 (not
	  (vscode-mode-cond/inputFocus))
	 (equal
	  (vscode-mode-cond/activeEditor)
	  "workbench.editor.notebook")
	 (equal
	  (vscode-mode-cond/notebookCellType)
	  "code"))))
 (("M-delete"
   (command vscode-mode-command/notebook\.cell\.clearOutputs)
   (when-orig . "notebookCellHasOutputs && notebookEditorFocused && !inputFocus")
   (when and
	 (vscode-mode-cond/notebookCellHasOutputs)
	 (vscode-mode-cond/notebookEditorFocused)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("C-k C-c"
   (command vscode-mode-command/notebook\.cell\.collapseCellContent)
   (when-orig . "notebookCellListFocused && !inputFocus && !notebookCellInputIsCollapsed")
   (when and
	 (vscode-mode-cond/notebookCellListFocused)
	 (not
	  (vscode-mode-cond/inputFocus))
	 (not
	  (vscode-mode-cond/notebookCellInputIsCollapsed)))))
 (("C-k t"
   (command vscode-mode-command/notebook\.cell\.collapseCellOutput)
   (when-orig . "notebookCellHasOutputs && notebookCellListFocused && !inputFocus && !notebookCellOutputIsCollapsed")
   (when and
	 (vscode-mode-cond/notebookCellHasOutputs)
	 (vscode-mode-cond/notebookCellListFocused)
	 (not
	  (vscode-mode-cond/inputFocus))
	 (not
	  (vscode-mode-cond/notebookCellOutputIsCollapsed)))))
 (("S-M-<down>"
   (command vscode-mode-command/notebook\.cell\.copyDown)
   (when-orig . "notebookEditorFocused && !inputFocus")
   (when and
	 (vscode-mode-cond/notebookEditorFocused)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("S-M-<up>"
   (command vscode-mode-command/notebook\.cell\.copyUp)
   (when-orig . "notebookEditorFocused && !inputFocus")
   (when and
	 (vscode-mode-cond/notebookEditorFocused)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("delete"
   (command vscode-mode-command/notebook\.cell\.delete)
   (when-orig . "notebookEditorFocused && !inputFocus")
   (when and
	 (vscode-mode-cond/notebookEditorFocused)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("RET"
   (command vscode-mode-command/notebook\.cell\.edit)
   (when-orig . "notebookCellListFocused && !inputFocus")
   (when and
	 (vscode-mode-cond/notebookCellListFocused)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("C-k C-c"
   (command vscode-mode-command/notebook\.cell\.expandCellContent)
   (when-orig . "notebookCellInputIsCollapsed && notebookCellListFocused")
   (when and
	 (vscode-mode-cond/notebookCellInputIsCollapsed)
	 (vscode-mode-cond/notebookCellListFocused))))
 (("C-k t"
   (command vscode-mode-command/notebook\.cell\.expandCellOutput)
   (when-orig . "notebookCellListFocused && notebookCellOutputIsCollapsed")
   (when and
	 (vscode-mode-cond/notebookCellListFocused)
	 (vscode-mode-cond/notebookCellOutputIsCollapsed))))
 (("C-<down>"
   (command vscode-mode-command/notebook\.cell\.focusInOutput)
   (when-orig . "notebookCellHasOutputs && notebookEditorFocused")
   (when and
	 (vscode-mode-cond/notebookCellHasOutputs)
	 (vscode-mode-cond/notebookEditorFocused))))
 (("C-<up>"
   (command vscode-mode-command/notebook\.cell\.focusOutOutput)
   (when-orig . "notebookEditorFocused")
   (when vscode-mode-cond/notebookEditorFocused)))
 (("C-S-RET"
   (command vscode-mode-command/notebook\.cell\.insertCodeCellAbove)
   (when-orig . "notebookCellListFocused && !inputFocus")
   (when and
	 (vscode-mode-cond/notebookCellListFocused)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("C-RET"
   (command vscode-mode-command/notebook\.cell\.insertCodeCellBelow)
   (when-orig . "notebookCellListFocused && !inputFocus")
   (when and
	 (vscode-mode-cond/notebookCellListFocused)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("S-M-win-j"
   (command vscode-mode-command/notebook\.cell\.joinAbove)
   (when-orig . "notebookEditorFocused")
   (when vscode-mode-cond/notebookEditorFocused)))
 (("M-win-j"
   (command vscode-mode-command/notebook\.cell\.joinBelow)
   (when-orig . "notebookEditorFocused")
   (when vscode-mode-cond/notebookEditorFocused)))
 (("M-<down>"
   (command vscode-mode-command/notebook\.cell\.moveDown)
   (when-orig . "notebookEditorFocused && !inputFocus")
   (when and
	 (vscode-mode-cond/notebookEditorFocused)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("M-<up>"
   (command vscode-mode-command/notebook\.cell\.moveUp)
   (when-orig . "notebookEditorFocused && !inputFocus")
   (when and
	 (vscode-mode-cond/notebookEditorFocused)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("C-k C-S-\\"
   (command vscode-mode-command/notebook\.cell\.split)
   (when-orig . "notebookCellEditable && notebookCellEditorFocused && notebookEditable && notebookEditorFocused && !notebookEditorCursorAtEditorBeginEnd")
   (when and
	 (vscode-mode-cond/notebookCellEditable)
	 (vscode-mode-cond/notebookCellEditorFocused)
	 (vscode-mode-cond/notebookEditable)
	 (vscode-mode-cond/notebookEditorFocused)
	 (not
	  (vscode-mode-cond/notebookEditorCursorAtEditorBeginEnd)))))
 (("C-l"
   (command vscode-mode-command/notebook\.centerActiveCell)
   (when-orig . "notebookEditorFocused")
   (when vscode-mode-cond/notebookEditorFocused)))
 (("C-f"
   (command vscode-mode-command/notebook\.find)
   (when-orig . "notebookEditorFocused")
   (when vscode-mode-cond/notebookEditorFocused)))
 (("C-end"
   (command vscode-mode-command/notebook\.focusBottom)
   (when-orig . "notebookEditorFocused && !inputFocus")
   (when and
	 (vscode-mode-cond/notebookEditorFocused)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("C-<down>"
   (command vscode-mode-command/notebook\.focusNextEditor)
   (when-orig . "notebookEditorFocused && notebookOutputFocused")
   (when and
	 (vscode-mode-cond/notebookEditorFocused)
	 (vscode-mode-cond/notebookOutputFocused))))
 (("C-home"
   (command vscode-mode-command/notebook\.focusTop)
   (when-orig . "notebookEditorFocused && !inputFocus")
   (when and
	 (vscode-mode-cond/notebookEditorFocused)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("<left>"
   (command vscode-mode-command/notebook\.fold)
   (when-orig . "notebookEditorFocused && !inputFocus && activeEditor == 'workbench.editor.notebook'")
   (when and
	 (vscode-mode-cond/notebookEditorFocused)
	 (not
	  (vscode-mode-cond/inputFocus))
	 (equal
	  (vscode-mode-cond/activeEditor)
	  "workbench.editor.notebook"))))
 (("C-S-["
   (command vscode-mode-command/notebook\.fold)
   (when-orig . "notebookEditorFocused && !inputFocus && activeEditor == 'workbench.editor.notebook'")
   (when and
	 (vscode-mode-cond/notebookEditorFocused)
	 (not
	  (vscode-mode-cond/inputFocus))
	 (equal
	  (vscode-mode-cond/activeEditor)
	  "workbench.editor.notebook"))))
 (("S-M-f"
   (command vscode-mode-command/notebook\.format)
   (when-orig . "notebookEditable && !editorTextFocus && activeEditor == 'workbench.editor.notebook'")
   (when and
	 (vscode-mode-cond/notebookEditable)
	 (not
	  (vscode-mode-cond/editorTextFocus))
	 (equal
	  (vscode-mode-cond/activeEditor)
	  "workbench.editor.notebook"))))
 (("ESC"
   (command vscode-mode-command/notebook\.hideFind)
   (when-orig . "notebookEditorFocused && notebookFindWidgetFocused")
   (when and
	 (vscode-mode-cond/notebookEditorFocused)
	 (vscode-mode-cond/notebookFindWidgetFocused))))
 (("<right>"
   (command vscode-mode-command/notebook\.unfold)
   (when-orig . "notebookEditorFocused && !inputFocus && activeEditor == 'workbench.editor.notebook'")
   (when and
	 (vscode-mode-cond/notebookEditorFocused)
	 (not
	  (vscode-mode-cond/inputFocus))
	 (equal
	  (vscode-mode-cond/activeEditor)
	  "workbench.editor.notebook"))))
 (("C-S-]"
   (command vscode-mode-command/notebook\.unfold)
   (when-orig . "notebookEditorFocused && !inputFocus && activeEditor == 'workbench.editor.notebook'")
   (when and
	 (vscode-mode-cond/notebookEditorFocused)
	 (not
	  (vscode-mode-cond/inputFocus))
	 (equal
	  (vscode-mode-cond/activeEditor)
	  "workbench.editor.notebook"))))
 (("delete"
   (command vscode-mode-command/notification\.clear)
   (when-orig . "notificationFocus")
   (when vscode-mode-cond/notificationFocus)))
 (("<left>"
   (command vscode-mode-command/notification\.collapse)
   (when-orig . "notificationFocus")
   (when vscode-mode-cond/notificationFocus)))
 (("<right>"
   (command vscode-mode-command/notification\.expand)
   (when-orig . "notificationFocus")
   (when vscode-mode-cond/notificationFocus)))
 (("RET"
   (command vscode-mode-command/notification\.toggle)
   (when-orig . "notificationFocus")
   (when vscode-mode-cond/notificationFocus)))
 (("SPC"
   (command vscode-mode-command/notification\.toggle)
   (when-orig . "notificationFocus")
   (when vscode-mode-cond/notificationFocus)))
 (("home"
   (command vscode-mode-command/notifications\.focusFirstToast)
   (when-orig . "notificationFocus && notificationToastsVisible")
   (when and
	 (vscode-mode-cond/notificationFocus)
	 (vscode-mode-cond/notificationToastsVisible))))
 ((""
   (command vscode-mode-command/notifications\.focusFirstToast)
   (when-orig . "notificationFocus && notificationToastsVisible")
   (when and
	 (vscode-mode-cond/notificationFocus)
	 (vscode-mode-cond/notificationToastsVisible))))
 (("end"
   (command vscode-mode-command/notifications\.focusLastToast)
   (when-orig . "notificationFocus && notificationToastsVisible")
   (when and
	 (vscode-mode-cond/notificationFocus)
	 (vscode-mode-cond/notificationToastsVisible))))
 ((""
   (command vscode-mode-command/notifications\.focusLastToast)
   (when-orig . "notificationFocus && notificationToastsVisible")
   (when and
	 (vscode-mode-cond/notificationFocus)
	 (vscode-mode-cond/notificationToastsVisible))))
 (("<down>"
   (command vscode-mode-command/notifications\.focusNextToast)
   (when-orig . "notificationFocus && notificationToastsVisible")
   (when and
	 (vscode-mode-cond/notificationFocus)
	 (vscode-mode-cond/notificationToastsVisible))))
 (("<up>"
   (command vscode-mode-command/notifications\.focusPreviousToast)
   (when-orig . "notificationFocus && notificationToastsVisible")
   (when and
	 (vscode-mode-cond/notificationFocus)
	 (vscode-mode-cond/notificationToastsVisible))))
 (("C-c"
   (command vscode-mode-command/problems\.action\.copy)
   (when-orig . "problemFocus")
   (when vscode-mode-cond/problemFocus)))
 (("C-f"
   (command vscode-mode-command/problems\.action\.focusFilter)
   (when-orig . "focusedView == 'workbench.panel.markers.view'")
   (when equal
	 (vscode-mode-cond/focusedView)
	 "workbench.panel.markers.view")))
 (("C-<down>"
   (command vscode-mode-command/problems\.action\.focusProblemsFromFilter)
   (when-orig . "problemsFilterFocus")
   (when vscode-mode-cond/problemsFilterFocus)))
 (("RET"
   (command vscode-mode-command/problems\.action\.open)
   (when-orig . "problemFocus")
   (when vscode-mode-cond/problemFocus)))
 (("C-RET"
   (command vscode-mode-command/problems\.action\.openToSide)
   (when-orig . "problemFocus")
   (when vscode-mode-cond/problemFocus)))
 (("C-."
   (command vscode-mode-command/problems\.action\.showQuickFixes)
   (when-orig . "problemFocus")
   (when vscode-mode-cond/problemFocus)))
 (("SPC"
   (command vscode-mode-command/refactorPreview\.toggleCheckedState)
   (when-orig . "listFocus && refactorPreview.enabled && !inputFocus")
   (when and
	 (vscode-mode-cond/listFocus)
	 (vscode-mode-cond/refactorPreview\.enabled)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("S-M-r"
   (command vscode-mode-command/revealFileInOS)
   (when-orig . "!editorFocus")
   (when not
	 (vscode-mode-cond/editorFocus))))
 (("RET"
   (command vscode-mode-command/revealReference)
   (when-orig . "listFocus && referenceSearchVisible && !inputFocus")
   (when and
	 (vscode-mode-cond/listFocus)
	 (vscode-mode-cond/referenceSearchVisible)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("C-RET"
   (command vscode-mode-command/scm\.acceptInput)
   (when-orig . "scmRepository")
   (when vscode-mode-cond/scmRepository)))
 (("M-<down>"
   (command vscode-mode-command/scm\.forceViewNextCommit)
   (when-orig . "scmRepository")
   (when vscode-mode-cond/scmRepository)))
 (("M-<up>"
   (command vscode-mode-command/scm\.forceViewPreviousCommit)
   (when-orig . "scmRepository")
   (when vscode-mode-cond/scmRepository)))
 (("<down>"
   (command vscode-mode-command/scm\.viewNextCommit)
   (when-orig . "scmInputIsInLastPosition && scmRepository && !suggestWidgetVisible")
   (when and
	 (vscode-mode-cond/scmInputIsInLastPosition)
	 (vscode-mode-cond/scmRepository)
	 (not
	  (vscode-mode-cond/suggestWidgetVisible)))))
 (("<up>"
   (command vscode-mode-command/scm\.viewPreviousCommit)
   (when-orig . "scmInputIsInFirstPosition && scmRepository && !suggestWidgetVisible")
   (when and
	 (vscode-mode-cond/scmInputIsInFirstPosition)
	 (vscode-mode-cond/scmRepository)
	 (not
	  (vscode-mode-cond/suggestWidgetVisible)))))
 (("ESC"
   (command vscode-mode-command/search\.action\.cancel)
   (when-orig . "listFocus && searchViewletVisible && !inputFocus")
   (when and
	 (vscode-mode-cond/listFocus)
	 (vscode-mode-cond/searchViewletVisible)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("C-c"
   (command vscode-mode-command/search\.action\.copyMatch)
   (when-orig . "fileMatchOrMatchFocus")
   (when vscode-mode-cond/fileMatchOrMatchFocus)))
 (("S-M-c"
   (command vscode-mode-command/search\.action\.copyPath)
   (when-orig . "fileMatchOrFolderMatchWithResourceFocus")
   (when vscode-mode-cond/fileMatchOrFolderMatchWithResourceFocus)))
 (("f4"
   (command vscode-mode-command/search\.action\.focusNextSearchResult)
   (when-orig . "hasSearchResult || inSearchEditor")
   (when or
	 (vscode-mode-cond/hasSearchResult)
	 (vscode-mode-cond/inSearchEditor))))
 (("S-f4"
   (command vscode-mode-command/search\.action\.focusPreviousSearchResult)
   (when-orig . "hasSearchResult || inSearchEditor")
   (when or
	 (vscode-mode-cond/hasSearchResult)
	 (vscode-mode-cond/inSearchEditor))))
 (("C-<up>"
   (command vscode-mode-command/search\.action\.focusSearchFromResults)
   (when-orig . "firstMatchFocus && searchViewletVisible")
   (when and
	 (vscode-mode-cond/firstMatchFocus)
	 (vscode-mode-cond/searchViewletVisible))))
 (("M-RET"
   (command vscode-mode-command/search\.action\.openInEditor)
   (when-orig . "hasSearchResult && searchViewletFocus")
   (when and
	 (vscode-mode-cond/hasSearchResult)
	 (vscode-mode-cond/searchViewletFocus))))
 (("RET"
   (command vscode-mode-command/search\.action\.openResult)
   (when-orig . "fileMatchOrMatchFocus && searchViewletVisible")
   (when and
	 (vscode-mode-cond/fileMatchOrMatchFocus)
	 (vscode-mode-cond/searchViewletVisible))))
 (("C-RET"
   (command vscode-mode-command/search\.action\.openResultToSide)
   (when-orig . "fileMatchOrMatchFocus && searchViewletVisible")
   (when and
	 (vscode-mode-cond/fileMatchOrMatchFocus)
	 (vscode-mode-cond/searchViewletVisible))))
 (("delete"
   (command vscode-mode-command/search\.action\.remove)
   (when-orig . "fileMatchOrMatchFocus && searchViewletVisible")
   (when and
	 (vscode-mode-cond/fileMatchOrMatchFocus)
	 (vscode-mode-cond/searchViewletVisible))))
 (("C-S-1"
   (command vscode-mode-command/search\.action\.replace)
   (when-orig . "matchFocus && replaceActive && searchViewletVisible")
   (when and
	 (vscode-mode-cond/matchFocus)
	 (vscode-mode-cond/replaceActive)
	 (vscode-mode-cond/searchViewletVisible))))
 (("C-M-RET"
   (command vscode-mode-command/search\.action\.replaceAll)
   (when-orig . "replaceActive && searchViewletVisible && !findWidgetVisible")
   (when and
	 (vscode-mode-cond/replaceActive)
	 (vscode-mode-cond/searchViewletVisible)
	 (not
	  (vscode-mode-cond/findWidgetVisible)))))
 (("C-S-RET"
   (command vscode-mode-command/search\.action\.replaceAllInFile)
   (when-orig . "fileMatchFocus && replaceActive && searchViewletVisible")
   (when and
	 (vscode-mode-cond/fileMatchFocus)
	 (vscode-mode-cond/replaceActive)
	 (vscode-mode-cond/searchViewletVisible))))
 (("C-S-1"
   (command vscode-mode-command/search\.action\.replaceAllInFile)
   (when-orig . "fileMatchFocus && replaceActive && searchViewletVisible")
   (when and
	 (vscode-mode-cond/fileMatchFocus)
	 (vscode-mode-cond/replaceActive)
	 (vscode-mode-cond/searchViewletVisible))))
 (("C-S-RET"
   (command vscode-mode-command/search\.action\.replaceAllInFolder)
   (when-orig . "folderMatchFocus && replaceActive && searchViewletVisible")
   (when and
	 (vscode-mode-cond/folderMatchFocus)
	 (vscode-mode-cond/replaceActive)
	 (vscode-mode-cond/searchViewletVisible))))
 (("C-S-1"
   (command vscode-mode-command/search\.action\.replaceAllInFolder)
   (when-orig . "folderMatchFocus && replaceActive && searchViewletVisible")
   (when and
	 (vscode-mode-cond/folderMatchFocus)
	 (vscode-mode-cond/replaceActive)
	 (vscode-mode-cond/searchViewletVisible))))
 (("C-<down>"
   (command vscode-mode-command/search\.focus\.nextInputBox)
   (when-orig . "inSearchEditor && inputBoxFocus || inputBoxFocus && searchViewletVisible")
   (when or
	 (and
	   (vscode-mode-cond/inSearchEditor)
	   (vscode-mode-cond/inputBoxFocus))
	 (and
	   (vscode-mode-cond/inputBoxFocus)
	   (vscode-mode-cond/searchViewletVisible)))))
 (("C-<up>"
   (command vscode-mode-command/search\.focus\.previousInputBox)
   (when-orig . "inSearchEditor && inputBoxFocus || inputBoxFocus && searchViewletVisible && !searchInputBoxFocus")
   (when or
	 (and
	   (vscode-mode-cond/inSearchEditor)
	   (vscode-mode-cond/inputBoxFocus))
	 (and
	   (vscode-mode-cond/inputBoxFocus)
	   (vscode-mode-cond/searchViewletVisible)
	   (not
	    (vscode-mode-cond/searchInputBoxFocus))))))
 (("C-S-l"
   (command vscode-mode-command/selectAllSearchEditorMatches)
   (when-orig . "inSearchEditor")
   (when vscode-mode-cond/inSearchEditor)))
 (("ESC"
   (command vscode-mode-command/settings\.action\.focusLevelUp)
   (when-orig . "inSettingsEditor && !inSettingsSearch")
   (when and
	 (vscode-mode-cond/inSettingsEditor)
	 (not
	  (vscode-mode-cond/inSettingsSearch)))))
 (("RET"
   (command vscode-mode-command/settings\.action\.focusSettingControl)
   (when-orig . "settingRowFocus")
   (when vscode-mode-cond/settingRowFocus)))
 (("<down>"
   (command vscode-mode-command/settings\.action\.focusSettingsFromSearch)
   (when-orig . "inSettingsSearch && !suggestWidgetVisible")
   (when and
	 (vscode-mode-cond/inSettingsSearch)
	 (not
	  (vscode-mode-cond/suggestWidgetVisible)))))
 (("RET"
   (command vscode-mode-command/settings\.action\.focusSettingsList)
   (when-orig . "inSettingsEditor && settingsTocRowFocus")
   (when and
	 (vscode-mode-cond/inSettingsEditor)
	 (vscode-mode-cond/settingsTocRowFocus))))
 (("<left>"
   (command vscode-mode-command/settings\.action\.focusTOC)
   (when-orig . "inSettingsEditor && settingRowFocus")
   (when and
	 (vscode-mode-cond/inSettingsEditor)
	 (vscode-mode-cond/settingRowFocus))))
 (("S-f9"
   (command vscode-mode-command/settings\.action\.showContextMenu)
   (when-orig . "inSettingsEditor")
   (when vscode-mode-cond/inSettingsEditor)))
 (("M-c"
   (command vscode-mode-command/toggleSearchCaseSensitive)
   (when-orig . "searchViewletFocus")
   (when vscode-mode-cond/searchViewletFocus)))
 (("M-c"
   (command vscode-mode-command/toggleSearchEditorCaseSensitive)
   (when-orig . "inSearchEditor && searchInputBoxFocus")
   (when and
	 (vscode-mode-cond/inSearchEditor)
	 (vscode-mode-cond/searchInputBoxFocus))))
 (("M-l"
   (command vscode-mode-command/toggleSearchEditorContextLines)
   (when-orig . "inSearchEditor")
   (when vscode-mode-cond/inSearchEditor)))
 (("M-r"
   (command vscode-mode-command/toggleSearchEditorRegex)
   (when-orig . "inSearchEditor && searchInputBoxFocus")
   (when and
	 (vscode-mode-cond/inSearchEditor)
	 (vscode-mode-cond/searchInputBoxFocus))))
 (("M-w"
   (command vscode-mode-command/toggleSearchEditorWholeWord)
   (when-orig . "inSearchEditor && searchInputBoxFocus")
   (when and
	 (vscode-mode-cond/inSearchEditor)
	 (vscode-mode-cond/searchInputBoxFocus))))
 (("M-p"
   (command vscode-mode-command/toggleSearchPreserveCase)
   (when-orig . "searchViewletFocus")
   (when vscode-mode-cond/searchViewletFocus)))
 (("M-r"
   (command vscode-mode-command/toggleSearchRegex)
   (when-orig . "searchViewletFocus")
   (when vscode-mode-cond/searchViewletFocus)))
 (("M-w"
   (command vscode-mode-command/toggleSearchWholeWord)
   (when-orig . "searchViewletFocus")
   (when vscode-mode-cond/searchViewletFocus)))
 (("C-w"
   (command vscode-mode-command/workbench\.action\.closeActiveEditor)
   (when-orig)
   (when)))
 (("C-f4"
   (command vscode-mode-command/workbench\.action\.closeActiveEditor)
   (when-orig)
   (when)))
 (("C-k C-w"
   (command vscode-mode-command/workbench\.action\.closeAllEditors)
   (when-orig)
   (when)))
 (("C-k C-S-w"
   (command vscode-mode-command/workbench\.action\.closeAllGroups)
   (when-orig)
   (when)))
 (("C-k w"
   (command vscode-mode-command/workbench\.action\.closeEditorsInGroup)
   (when-orig)
   (when)))
 (("C-k f"
   (command vscode-mode-command/workbench\.action\.closeFolder)
   (when-orig . "emptyWorkspaceSupport")
   (when vscode-mode-cond/emptyWorkspaceSupport)))
 (("C-w"
   (command vscode-mode-command/workbench\.action\.closeGroup)
   (when-orig . "activeEditorGroupEmpty && multipleEditorGroups")
   (when and
	 (vscode-mode-cond/activeEditorGroupEmpty)
	 (vscode-mode-cond/multipleEditorGroups))))
 (("C-f4"
   (command vscode-mode-command/workbench\.action\.closeGroup)
   (when-orig . "activeEditorGroupEmpty && multipleEditorGroups")
   (when and
	 (vscode-mode-cond/activeEditorGroupEmpty)
	 (vscode-mode-cond/multipleEditorGroups))))
 (("S-ESC"
   (command vscode-mode-command/workbench\.action\.closeQuickOpen)
   (when-orig . "inQuickOpen")
   (when vscode-mode-cond/inQuickOpen)))
 (("ESC"
   (command vscode-mode-command/workbench\.action\.closeQuickOpen)
   (when-orig . "inQuickOpen")
   (when vscode-mode-cond/inQuickOpen)))
 (("C-k u"
   (command vscode-mode-command/workbench\.action\.closeUnmodifiedEditors)
   (when-orig)
   (when)))
 (("C-S-w"
   (command vscode-mode-command/workbench\.action\.closeWindow)
   (when-orig)
   (when)))
 (("C-w"
   (command vscode-mode-command/workbench\.action\.closeWindow)
   (when-orig . "!editorIsOpen && !multipleEditorGroups")
   (when and
	 (not
	  (vscode-mode-cond/editorIsOpen))
	 (not
	  (vscode-mode-cond/multipleEditorGroups)))))
 (("M-f5"
   (command vscode-mode-command/workbench\.action\.compareEditor\.nextChange)
   (when-orig . "textCompareEditorVisible")
   (when vscode-mode-cond/textCompareEditorVisible)))
 (("S-M-f5"
   (command vscode-mode-command/workbench\.action\.compareEditor\.previousChange)
   (when-orig . "textCompareEditorVisible")
   (when vscode-mode-cond/textCompareEditorVisible)))
 (("f5"
   (command vscode-mode-command/workbench\.action\.debug\.continue)
   (when-orig . "inDebugMode")
   (when vscode-mode-cond/inDebugMode)))
 (("C-S-f5"
   (command vscode-mode-command/workbench\.action\.debug\.restart)
   (when-orig . "inDebugMode")
   (when vscode-mode-cond/inDebugMode)))
 (("C-f5"
   (command vscode-mode-command/workbench\.action\.debug\.run)
   (when-orig . "debuggersAvailable")
   (when vscode-mode-cond/debuggersAvailable)))
 (("f5"
   (command vscode-mode-command/workbench\.action\.debug\.start)
   (when-orig . "debuggersAvailable && !inDebugMode")
   (when and
	 (vscode-mode-cond/debuggersAvailable)
	 (not
	  (vscode-mode-cond/inDebugMode)))))
 (("S-f11"
   (command vscode-mode-command/workbench\.action\.debug\.stepOut)
   (when-orig . "debugState == 'stopped'")
   (when equal
	 (vscode-mode-cond/debugState)
	 "stopped")))
 (("f10"
   (command vscode-mode-command/workbench\.action\.debug\.stepOver)
   (when-orig . "debugState == 'stopped'")
   (when equal
	 (vscode-mode-cond/debugState)
	 "stopped")))
 (("S-f5"
   (command vscode-mode-command/workbench\.action\.debug\.stop)
   (when-orig . "inDebugMode")
   (when vscode-mode-cond/inDebugMode)))
 (("C-k m"
   (command vscode-mode-command/workbench\.action\.editor\.changeLanguageMode)
   (when-orig)
   (when)))
 (("C-k p"
   (command vscode-mode-command/workbench\.action\.files\.copyPathOfActiveFile)
   (when-orig)
   (when)))
 (("C-n"
   (command vscode-mode-command/workbench\.action\.files\.newUntitledFile)
   (when-orig)
   (when)))
 (("C-o"
   (command vscode-mode-command/workbench\.action\.files\.openFile)
   (when-orig)
   (when)))
 (("C-k C-o"
   (command vscode-mode-command/workbench\.action\.files\.openFolder)
   (when-orig)
   (when)))
 (("C-o"
   (command vscode-mode-command/workbench\.action\.files\.openLocalFile)
   (when-orig . "remoteFileDialogVisible")
   (when vscode-mode-cond/remoteFileDialogVisible)))
 (("C-k C-o"
   (command vscode-mode-command/workbench\.action\.files\.openLocalFolder)
   (when-orig . "remoteFileDialogVisible")
   (when vscode-mode-cond/remoteFileDialogVisible)))
 (("C-k r"
   (command vscode-mode-command/workbench\.action\.files\.revealActiveFileInWindows)
   (when-orig)
   (when)))
 (("C-s"
   (command vscode-mode-command/workbench\.action\.files\.save)
   (when-orig)
   (when)))
 (("C-k s"
   (command vscode-mode-command/workbench\.action\.files\.saveAll)
   (when-orig)
   (when)))
 (("C-S-s"
   (command vscode-mode-command/workbench\.action\.files\.saveAs)
   (when-orig)
   (when)))
 (("C-S-s"
   (command vscode-mode-command/workbench\.action\.files\.saveLocalFile)
   (when-orig . "remoteFileDialogVisible")
   (when vscode-mode-cond/remoteFileDialogVisible)))
 (("C-k C-S-s"
   (command vscode-mode-command/workbench\.action\.files\.saveWithoutFormatting)
   (when-orig)
   (when)))
 (("C-k o"
   (command vscode-mode-command/workbench\.action\.files\.showOpenedFileInNewWindow)
   (when-orig . "emptyWorkspaceSupport")
   (when vscode-mode-cond/emptyWorkspaceSupport)))
 (("C-S-f"
   (command vscode-mode-command/workbench\.action\.findInFiles)
   (when-orig)
   (when)))
 (("C-k C-<up>"
   (command vscode-mode-command/workbench\.action\.focusAboveGroup)
   (when-orig)
   (when)))
 (("C-k C-<down>"
   (command vscode-mode-command/workbench\.action\.focusBelowGroup)
   (when-orig)
   (when)))
 (("C-8"
   (command vscode-mode-command/workbench\.action\.focusEighthEditorGroup)
   (when-orig)
   (when)))
 (("C-5"
   (command vscode-mode-command/workbench\.action\.focusFifthEditorGroup)
   (when-orig)
   (when)))
 (("C-1"
   (command vscode-mode-command/workbench\.action\.focusFirstEditorGroup)
   (when-orig)
   (when)))
 (("C-4"
   (command vscode-mode-command/workbench\.action\.focusFourthEditorGroup)
   (when-orig)
   (when)))
 (("C-k C-<left>"
   (command vscode-mode-command/workbench\.action\.focusLeftGroup)
   (when-orig)
   (when)))
 (("f6"
   (command vscode-mode-command/workbench\.action\.focusNextPart)
   (when-orig)
   (when)))
 (("S-f6"
   (command vscode-mode-command/workbench\.action\.focusPreviousPart)
   (when-orig)
   (when)))
 (("C-k C-<right>"
   (command vscode-mode-command/workbench\.action\.focusRightGroup)
   (when-orig)
   (when)))
 (("C-2"
   (command vscode-mode-command/workbench\.action\.focusSecondEditorGroup)
   (when-orig)
   (when)))
 (("C-7"
   (command vscode-mode-command/workbench\.action\.focusSeventhEditorGroup)
   (when-orig)
   (when)))
 (("C-0"
   (command vscode-mode-command/workbench\.action\.focusSideBar)
   (when-orig)
   (when)))
 (("C-6"
   (command vscode-mode-command/workbench\.action\.focusSixthEditorGroup)
   (when-orig)
   (when)))
 (("C-3"
   (command vscode-mode-command/workbench\.action\.focusThirdEditorGroup)
   (when-orig)
   (when)))
 (("C-g"
   (command vscode-mode-command/workbench\.action\.gotoLine)
   (when-orig)
   (when)))
 (("C-S-o"
   (command vscode-mode-command/workbench\.action\.gotoSymbol)
   (when-orig)
   (when)))
 (("ESC"
   (command vscode-mode-command/workbench\.action\.hideInterfaceOverview)
   (when-orig . "interfaceOverviewVisible")
   (when vscode-mode-cond/interfaceOverviewVisible)))
 (("<down>"
   (command vscode-mode-command/workbench\.action\.interactivePlayground\.arrowDown)
   (when-orig . "interactivePlaygroundFocus && !editorTextFocus")
   (when and
	 (vscode-mode-cond/interactivePlaygroundFocus)
	 (not
	  (vscode-mode-cond/editorTextFocus)))))
 (("<up>"
   (command vscode-mode-command/workbench\.action\.interactivePlayground\.arrowUp)
   (when-orig . "interactivePlaygroundFocus && !editorTextFocus")
   (when and
	 (vscode-mode-cond/interactivePlaygroundFocus)
	 (not
	  (vscode-mode-cond/editorTextFocus)))))
 ((""
   (command vscode-mode-command/workbench\.action\.interactivePlayground\.pageDown)
   (when-orig . "interactivePlaygroundFocus && !editorTextFocus")
   (when and
	 (vscode-mode-cond/interactivePlaygroundFocus)
	 (not
	  (vscode-mode-cond/editorTextFocus)))))
 ((""
   (command vscode-mode-command/workbench\.action\.interactivePlayground\.pageUp)
   (when-orig . "interactivePlaygroundFocus && !editorTextFocus")
   (when and
	 (vscode-mode-cond/interactivePlaygroundFocus)
	 (not
	  (vscode-mode-cond/editorTextFocus)))))
 (("C-k RET"
   (command vscode-mode-command/workbench\.action\.keepEditor)
   (when-orig)
   (when)))
 (("C-k C-r"
   (command vscode-mode-command/workbench\.action\.keybindingsReference)
   (when-orig)
   (when)))
 (("C-9"
   (command vscode-mode-command/workbench\.action\.lastEditorInGroup)
   (when-orig)
   (when)))
 (("M-0"
   (command vscode-mode-command/workbench\.action\.lastEditorInGroup)
   (when-orig)
   (when)))
 (("C-k <down>"
   (command vscode-mode-command/workbench\.action\.moveActiveEditorGroupDown)
   (when-orig)
   (when)))
 (("C-k <left>"
   (command vscode-mode-command/workbench\.action\.moveActiveEditorGroupLeft)
   (when-orig)
   (when)))
 (("C-k <right>"
   (command vscode-mode-command/workbench\.action\.moveActiveEditorGroupRight)
   (when-orig)
   (when)))
 (("C-k <up>"
   (command vscode-mode-command/workbench\.action\.moveActiveEditorGroupUp)
   (when-orig)
   (when)))
 (("C-S-"
   (command vscode-mode-command/workbench\.action\.moveEditorLeftInGroup)
   (when-orig)
   (when)))
 (("C-S-"
   (command vscode-mode-command/workbench\.action\.moveEditorRightInGroup)
   (when-orig)
   (when)))
 (("S-M-1"
   (command vscode-mode-command/workbench\.action\.moveEditorToFirstGroup)
   (when-orig)
   (when)))
 (("S-M-9"
   (command vscode-mode-command/workbench\.action\.moveEditorToLastGroup)
   (when-orig)
   (when)))
 (("C-M-<right>"
   (command vscode-mode-command/workbench\.action\.moveEditorToNextGroup)
   (when-orig)
   (when)))
 (("C-M-<left>"
   (command vscode-mode-command/workbench\.action\.moveEditorToPreviousGroup)
   (when-orig)
   (when)))
 (("M-<left>"
   (command vscode-mode-command/workbench\.action\.navigateBack)
   (when-orig)
   (when)))
 (("M-<right>"
   (command vscode-mode-command/workbench\.action\.navigateForward)
   (when-orig)
   (when)))
 (("C-k C-q"
   (command vscode-mode-command/workbench\.action\.navigateToLastEditLocation)
   (when-orig)
   (when)))
 (("C-S-n"
   (command vscode-mode-command/workbench\.action\.newWindow)
   (when-orig)
   (when)))
 (("C-"
   (command vscode-mode-command/workbench\.action\.nextEditor)
   (when-orig)
   (when)))
 (("C-k C-"
   (command vscode-mode-command/workbench\.action\.nextEditorInGroup)
   (when-orig)
   (when)))
 (("M-1"
   (command vscode-mode-command/workbench\.action\.openEditorAtIndex1)
   (when-orig)
   (when)))
 (("M-2"
   (command vscode-mode-command/workbench\.action\.openEditorAtIndex2)
   (when-orig)
   (when)))
 (("M-3"
   (command vscode-mode-command/workbench\.action\.openEditorAtIndex3)
   (when-orig)
   (when)))
 (("M-4"
   (command vscode-mode-command/workbench\.action\.openEditorAtIndex4)
   (when-orig)
   (when)))
 (("M-5"
   (command vscode-mode-command/workbench\.action\.openEditorAtIndex5)
   (when-orig)
   (when)))
 (("M-6"
   (command vscode-mode-command/workbench\.action\.openEditorAtIndex6)
   (when-orig)
   (when)))
 (("M-7"
   (command vscode-mode-command/workbench\.action\.openEditorAtIndex7)
   (when-orig)
   (when)))
 (("M-8"
   (command vscode-mode-command/workbench\.action\.openEditorAtIndex8)
   (when-orig)
   (when)))
 (("M-9"
   (command vscode-mode-command/workbench\.action\.openEditorAtIndex9)
   (when-orig)
   (when)))
 (("C-k C-s"
   (command vscode-mode-command/workbench\.action\.openGlobalKeybindings)
   (when-orig)
   (when)))
 (("C-r"
   (command vscode-mode-command/workbench\.action\.openRecent)
   (when-orig)
   (when)))
 (("C-,"
   (command vscode-mode-command/workbench\.action\.openSettings)
   (when-orig)
   (when)))
 (("C-S-u"
   (command vscode-mode-command/workbench\.action\.output\.toggleOutput)
   (when-orig)
   (when)))
 (("C-k S-RET"
   (command vscode-mode-command/workbench\.action\.pinEditor)
   (when-orig . "!activeEditorIsPinned")
   (when not
	 (vscode-mode-cond/activeEditorIsPinned))))
 (("C-"
   (command vscode-mode-command/workbench\.action\.previousEditor)
   (when-orig)
   (when)))
 (("C-k C-"
   (command vscode-mode-command/workbench\.action\.previousEditorInGroup)
   (when-orig)
   (when)))
 (("C-e"
   (command vscode-mode-command/workbench\.action\.quickOpen)
   (when-orig)
   (when)))
 (("C-p"
   (command vscode-mode-command/workbench\.action\.quickOpen)
   (when-orig)
   (when)))
 (("C-S-tab"
   (command vscode-mode-command/workbench\.action\.quickOpenLeastRecentlyUsedEditorInGroup)
   (when-orig)
   (when)))
 (("C-tab"
   (command vscode-mode-command/workbench\.action\.quickOpenPreviousRecentlyUsedEditorInGroup)
   (when-orig)
   (when)))
 (("C-q"
   (command vscode-mode-command/workbench\.action\.quickOpenView)
   (when-orig)
   (when)))
 (("C-S-t"
   (command vscode-mode-command/workbench\.action\.reopenClosedEditor)
   (when-orig)
   (when)))
 (("C-S-h"
   (command vscode-mode-command/workbench\.action\.replaceInFiles)
   (when-orig)
   (when)))
 (("C-S-j"
   (command vscode-mode-command/workbench\.action\.search\.toggleQueryDetails)
   (when-orig . "inSearchEditor || searchViewletFocus")
   (when or
	 (vscode-mode-cond/inSearchEditor)
	 (vscode-mode-cond/searchViewletFocus))))
 (("C-k C-t"
   (command vscode-mode-command/workbench\.action\.selectTheme)
   (when-orig)
   (when)))
 (("C-k C-p"
   (command vscode-mode-command/workbench\.action\.showAllEditors)
   (when-orig)
   (when)))
 (("C-t"
   (command vscode-mode-command/workbench\.action\.showAllSymbols)
   (when-orig)
   (when)))
 (("f1"
   (command vscode-mode-command/workbench\.action\.showCommands)
   (when-orig)
   (when)))
 (("C-S-p"
   (command vscode-mode-command/workbench\.action\.showCommands)
   (when-orig)
   (when)))
 (("C-\\"
   (command vscode-mode-command/workbench\.action\.splitEditor)
   (when-orig)
   (when)))
 (("C-k C-\\"
   (command vscode-mode-command/workbench\.action\.splitEditorOrthogonal)
   (when-orig)
   (when)))
 (("C-S-b"
   (command vscode-mode-command/workbench\.action\.tasks\.build)
   (when-orig)
   (when)))
 (("ESC"
   (command vscode-mode-command/workbench\.action\.terminal\.clearSelection)
   (when-orig . "terminalFocus && terminalProcessSupported && terminalTextSelected && !terminalFindVisible")
   (when and
	 (vscode-mode-cond/terminalFocus)
	 (vscode-mode-cond/terminalProcessSupported)
	 (vscode-mode-cond/terminalTextSelected)
	 (not
	  (vscode-mode-cond/terminalFindVisible)))))
 (("C-S-c"
   (command vscode-mode-command/workbench\.action\.terminal\.copySelection)
   (when-orig . "terminalFocus && terminalProcessSupported && terminalTextSelected")
   (when and
	 (vscode-mode-cond/terminalFocus)
	 (vscode-mode-cond/terminalProcessSupported)
	 (vscode-mode-cond/terminalTextSelected))))
 (("C-c"
   (command vscode-mode-command/workbench\.action\.terminal\.copySelection)
   (when-orig . "terminalFocus && terminalProcessSupported && terminalTextSelected")
   (when and
	 (vscode-mode-cond/terminalFocus)
	 (vscode-mode-cond/terminalProcessSupported)
	 (vscode-mode-cond/terminalTextSelected))))
 (("f3"
   (command vscode-mode-command/workbench\.action\.terminal\.findNext)
   (when-orig . "terminalFindFocused && terminalProcessSupported || terminalFocus && terminalProcessSupported")
   (when or
	 (and
	   (vscode-mode-cond/terminalFindFocused)
	   (vscode-mode-cond/terminalProcessSupported))
	 (and
	   (vscode-mode-cond/terminalFocus)
	   (vscode-mode-cond/terminalProcessSupported)))))
 (("S-RET"
   (command vscode-mode-command/workbench\.action\.terminal\.findNext)
   (when-orig . "terminalFindFocused && terminalProcessSupported")
   (when and
	 (vscode-mode-cond/terminalFindFocused)
	 (vscode-mode-cond/terminalProcessSupported))))
 (("S-f3"
   (command vscode-mode-command/workbench\.action\.terminal\.findPrevious)
   (when-orig . "terminalFindFocused && terminalProcessSupported || terminalFocus && terminalProcessSupported")
   (when or
	 (and
	   (vscode-mode-cond/terminalFindFocused)
	   (vscode-mode-cond/terminalProcessSupported))
	 (and
	   (vscode-mode-cond/terminalFocus)
	   (vscode-mode-cond/terminalProcessSupported)))))
 (("RET"
   (command vscode-mode-command/workbench\.action\.terminal\.findPrevious)
   (when-orig . "terminalFindFocused && terminalProcessSupported")
   (when and
	 (vscode-mode-cond/terminalFindFocused)
	 (vscode-mode-cond/terminalProcessSupported))))
 (("C-f"
   (command vscode-mode-command/workbench\.action\.terminal\.focusFind)
   (when-orig . "terminalFindFocused && terminalProcessSupported || terminalFocus && terminalProcessSupported")
   (when or
	 (and
	   (vscode-mode-cond/terminalFindFocused)
	   (vscode-mode-cond/terminalProcessSupported))
	 (and
	   (vscode-mode-cond/terminalFocus)
	   (vscode-mode-cond/terminalProcessSupported)))))
 (("M-<down>"
   (command vscode-mode-command/workbench\.action\.terminal\.focusNextPane)
   (when-orig . "terminalFocus && terminalProcessSupported")
   (when and
	 (vscode-mode-cond/terminalFocus)
	 (vscode-mode-cond/terminalProcessSupported))))
 (("M-<right>"
   (command vscode-mode-command/workbench\.action\.terminal\.focusNextPane)
   (when-orig . "terminalFocus && terminalProcessSupported")
   (when and
	 (vscode-mode-cond/terminalFocus)
	 (vscode-mode-cond/terminalProcessSupported))))
 (("M-<up>"
   (command vscode-mode-command/workbench\.action\.terminal\.focusPreviousPane)
   (when-orig . "terminalFocus && terminalProcessSupported")
   (when and
	 (vscode-mode-cond/terminalFocus)
	 (vscode-mode-cond/terminalProcessSupported))))
 (("M-<left>"
   (command vscode-mode-command/workbench\.action\.terminal\.focusPreviousPane)
   (when-orig . "terminalFocus && terminalProcessSupported")
   (when and
	 (vscode-mode-cond/terminalFocus)
	 (vscode-mode-cond/terminalProcessSupported))))
 (("S-ESC"
   (command vscode-mode-command/workbench\.action\.terminal\.hideFind)
   (when-orig . "terminalFindVisible && terminalFocus && terminalProcessSupported")
   (when and
	 (vscode-mode-cond/terminalFindVisible)
	 (vscode-mode-cond/terminalFocus)
	 (vscode-mode-cond/terminalProcessSupported))))
 (("ESC"
   (command vscode-mode-command/workbench\.action\.terminal\.hideFind)
   (when-orig . "terminalFindVisible && terminalFocus && terminalProcessSupported")
   (when and
	 (vscode-mode-cond/terminalFindVisible)
	 (vscode-mode-cond/terminalFocus)
	 (vscode-mode-cond/terminalProcessSupported))))
 (("ESC"
   (command vscode-mode-command/workbench\.action\.terminal\.navigationModeExit)
   (when-orig . "accessibilityModeEnabled && terminalA11yTreeFocus && terminalProcessSupported")
   (when and
	 (vscode-mode-cond/accessibilityModeEnabled)
	 (vscode-mode-cond/terminalA11yTreeFocus)
	 (vscode-mode-cond/terminalProcessSupported))))
 (("C-<down>"
   (command vscode-mode-command/workbench\.action\.terminal\.navigationModeFocusNext)
   (when-orig . "accessibilityModeEnabled && terminalA11yTreeFocus && terminalProcessSupported || accessibilityModeEnabled && terminalFocus && terminalProcessSupported")
   (when or
	 (and
	   (vscode-mode-cond/accessibilityModeEnabled)
	   (vscode-mode-cond/terminalA11yTreeFocus)
	   (vscode-mode-cond/terminalProcessSupported))
	 (and
	   (vscode-mode-cond/accessibilityModeEnabled)
	   (vscode-mode-cond/terminalFocus)
	   (vscode-mode-cond/terminalProcessSupported)))))
 (("C-<up>"
   (command vscode-mode-command/workbench\.action\.terminal\.navigationModeFocusPrevious)
   (when-orig . "accessibilityModeEnabled && terminalA11yTreeFocus && terminalProcessSupported || accessibilityModeEnabled && terminalFocus && terminalProcessSupported")
   (when or
	 (and
	   (vscode-mode-cond/accessibilityModeEnabled)
	   (vscode-mode-cond/terminalA11yTreeFocus)
	   (vscode-mode-cond/terminalProcessSupported))
	 (and
	   (vscode-mode-cond/accessibilityModeEnabled)
	   (vscode-mode-cond/terminalFocus)
	   (vscode-mode-cond/terminalProcessSupported)))))
 (("C-S-`"
   (command vscode-mode-command/workbench\.action\.terminal\.new)
   (when-orig)
   (when)))
 (("C-S-c"
   (command vscode-mode-command/workbench\.action\.terminal\.openNativeConsole)
   (when-orig . "!terminalFocus")
   (when not
	 (vscode-mode-cond/terminalFocus))))
 (("C-S-v"
   (command vscode-mode-command/workbench\.action\.terminal\.paste)
   (when-orig . "terminalFocus && terminalProcessSupported")
   (when and
	 (vscode-mode-cond/terminalFocus)
	 (vscode-mode-cond/terminalProcessSupported))))
 (("C-v"
   (command vscode-mode-command/workbench\.action\.terminal\.paste)
   (when-orig . "terminalFocus && terminalProcessSupported")
   (when and
	 (vscode-mode-cond/terminalFocus)
	 (vscode-mode-cond/terminalProcessSupported))))
 (("C-M-"
   (command vscode-mode-command/workbench\.action\.terminal\.scrollDown)
   (when-orig . "terminalFocus && terminalProcessSupported")
   (when and
	 (vscode-mode-cond/terminalFocus)
	 (vscode-mode-cond/terminalProcessSupported))))
 (("S-"
   (command vscode-mode-command/workbench\.action\.terminal\.scrollDownPage)
   (when-orig . "terminalFocus && terminalProcessSupported")
   (when and
	 (vscode-mode-cond/terminalFocus)
	 (vscode-mode-cond/terminalProcessSupported))))
 (("C-end"
   (command vscode-mode-command/workbench\.action\.terminal\.scrollToBottom)
   (when-orig . "terminalFocus && terminalProcessSupported")
   (when and
	 (vscode-mode-cond/terminalFocus)
	 (vscode-mode-cond/terminalProcessSupported))))
 (("C-home"
   (command vscode-mode-command/workbench\.action\.terminal\.scrollToTop)
   (when-orig . "terminalFocus && terminalProcessSupported")
   (when and
	 (vscode-mode-cond/terminalFocus)
	 (vscode-mode-cond/terminalProcessSupported))))
 (("C-M-"
   (command vscode-mode-command/workbench\.action\.terminal\.scrollUp)
   (when-orig . "terminalFocus && terminalProcessSupported")
   (when and
	 (vscode-mode-cond/terminalFocus)
	 (vscode-mode-cond/terminalProcessSupported))))
 (("S-"
   (command vscode-mode-command/workbench\.action\.terminal\.scrollUpPage)
   (when-orig . "terminalFocus && terminalProcessSupported")
   (when and
	 (vscode-mode-cond/terminalFocus)
	 (vscode-mode-cond/terminalProcessSupported))))
 (("C-S-f"
   (command vscode-mode-command/workbench\.action\.terminal\.searchWorkspace)
   (when-orig . "terminalFocus && terminalProcessSupported && terminalProcessSupported && terminalTextSelected")
   (when and
	 (vscode-mode-cond/terminalFocus)
	 (vscode-mode-cond/terminalProcessSupported)
	 (vscode-mode-cond/terminalProcessSupported)
	 (vscode-mode-cond/terminalTextSelected))))
 (("C-v"
   (command vscode-mode-command/workbench\.action\.terminal\.sendSequence)
   (when-orig . "terminalFocus && !accessibilityModeEnabled && terminalShellType == 'pwsh'")
   (when and
	 (vscode-mode-cond/terminalFocus)
	 (not
	  (vscode-mode-cond/accessibilityModeEnabled))
	 (equal
	  (vscode-mode-cond/terminalShellType)
	  "pwsh"))))
 (("C-DEL"
   (command vscode-mode-command/workbench\.action\.terminal\.sendSequence)
   (when-orig . "terminalFocus")
   (when vscode-mode-cond/terminalFocus)))
 (("C-DEL"
   (command vscode-mode-command/workbench\.action\.terminal\.sendSequence)
   (when-orig . "terminalFocus && terminalShellType == 'cmd'")
   (when and
	 (vscode-mode-cond/terminalFocus)
	 (equal
	  (vscode-mode-cond/terminalShellType)
	  "cmd"))))
 (("C-delete"
   (command vscode-mode-command/workbench\.action\.terminal\.sendSequence)
   (when-orig . "terminalFocus")
   (when vscode-mode-cond/terminalFocus)))
 (("C-S-5"
   (command vscode-mode-command/workbench\.action\.terminal\.split)
   (when-orig . "terminalFocus && terminalProcessSupported")
   (when and
	 (vscode-mode-cond/terminalFocus)
	 (vscode-mode-cond/terminalProcessSupported))))
 (("M-c"
   (command vscode-mode-command/workbench\.action\.terminal\.toggleFindCaseSensitive)
   (when-orig . "terminalFindFocused && terminalProcessSupported || terminalFocus && terminalProcessSupported")
   (when or
	 (and
	   (vscode-mode-cond/terminalFindFocused)
	   (vscode-mode-cond/terminalProcessSupported))
	 (and
	   (vscode-mode-cond/terminalFocus)
	   (vscode-mode-cond/terminalProcessSupported)))))
 (("M-r"
   (command vscode-mode-command/workbench\.action\.terminal\.toggleFindRegex)
   (when-orig . "terminalFindFocused && terminalProcessSupported || terminalFocus && terminalProcessSupported")
   (when or
	 (and
	   (vscode-mode-cond/terminalFindFocused)
	   (vscode-mode-cond/terminalProcessSupported))
	 (and
	   (vscode-mode-cond/terminalFocus)
	   (vscode-mode-cond/terminalProcessSupported)))))
 (("M-w"
   (command vscode-mode-command/workbench\.action\.terminal\.toggleFindWholeWord)
   (when-orig . "terminalFindFocused && terminalProcessSupported || terminalFocus && terminalProcessSupported")
   (when or
	 (and
	   (vscode-mode-cond/terminalFindFocused)
	   (vscode-mode-cond/terminalProcessSupported))
	 (and
	   (vscode-mode-cond/terminalFocus)
	   (vscode-mode-cond/terminalProcessSupported)))))
 (("C-`"
   (command vscode-mode-command/workbench\.action\.terminal\.toggleTerminal)
   (when-orig)
   (when)))
 (("S-M-0"
   (command vscode-mode-command/workbench\.action\.toggleEditorGroupLayout)
   (when-orig)
   (when)))
 (("f11"
   (command vscode-mode-command/workbench\.action\.toggleFullScreen)
   (when-orig)
   (when)))
 (("C-j"
   (command vscode-mode-command/workbench\.action\.togglePanel)
   (when-orig)
   (when)))
 (("C-b"
   (command vscode-mode-command/workbench\.action\.toggleSidebarVisibility)
   (when-orig)
   (when)))
 (("C-k z"
   (command vscode-mode-command/workbench\.action\.toggleZenMode)
   (when-orig)
   (when)))
 (("C-k S-RET"
   (command vscode-mode-command/workbench\.action\.unpinEditor)
   (when-orig . "activeEditorIsPinned")
   (when vscode-mode-cond/activeEditorIsPinned)))
 (("C-numpad_add"
   (command vscode-mode-command/workbench\.action\.zoomIn)
   (when-orig)
   (when)))
 (("C-S-="
   (command vscode-mode-command/workbench\.action\.zoomIn)
   (when-orig)
   (when)))
 (("C-="
   (command vscode-mode-command/workbench\.action\.zoomIn)
   (when-orig)
   (when)))
 (("C-numpad_subtract"
   (command vscode-mode-command/workbench\.action\.zoomOut)
   (when-orig)
   (when)))
 (("C-S--"
   (command vscode-mode-command/workbench\.action\.zoomOut)
   (when-orig)
   (when)))
 (("C--"
   (command vscode-mode-command/workbench\.action\.zoomOut)
   (when-orig)
   (when)))
 (("C-numpad0"
   (command vscode-mode-command/workbench\.action\.zoomReset)
   (when-orig)
   (when)))
 (("C-S-m"
   (command vscode-mode-command/workbench\.actions\.view\.problems)
   (when-orig)
   (when)))
 (("C-S-y"
   (command vscode-mode-command/workbench\.debug\.action\.toggleRepl)
   (when-orig . "debuggersAvailable")
   (when vscode-mode-cond/debuggersAvailable)))
 (("C-k C-m"
   (command vscode-mode-command/workbench\.extensions\.action\.showRecommendedKeymapExtensions)
   (when-orig)
   (when)))
 (("C-k c"
   (command vscode-mode-command/workbench\.files\.action\.compareWithClipboard)
   (when-orig)
   (when)))
 (("C-k d"
   (command vscode-mode-command/workbench\.files\.action\.compareWithSaved)
   (when-orig)
   (when)))
 (("C-k e"
   (command vscode-mode-command/workbench\.files\.action\.focusOpenEditorsView)
   (when-orig . "workbench.explorer.openEditorsView.active")
   (when vscode-mode-cond/workbench\.explorer\.openEditorsView\.active)))
 (("ESC"
   (command vscode-mode-command/workbench\.statusBar\.clearFocus)
   (when-orig . "statusBarFocused")
   (when vscode-mode-cond/statusBarFocused)))
 (("home"
   (command vscode-mode-command/workbench\.statusBar\.focusFirst)
   (when-orig . "statusBarFocused")
   (when vscode-mode-cond/statusBarFocused)))
 (("end"
   (command vscode-mode-command/workbench\.statusBar\.focusLast)
   (when-orig . "statusBarFocused")
   (when vscode-mode-cond/statusBarFocused)))
 (("<down>"
   (command vscode-mode-command/workbench\.statusBar\.focusNext)
   (when-orig . "statusBarFocused")
   (when vscode-mode-cond/statusBarFocused)))
 (("<right>"
   (command vscode-mode-command/workbench\.statusBar\.focusNext)
   (when-orig . "statusBarFocused")
   (when vscode-mode-cond/statusBarFocused)))
 (("<up>"
   (command vscode-mode-command/workbench\.statusBar\.focusPrevious)
   (when-orig . "statusBarFocused")
   (when vscode-mode-cond/statusBarFocused)))
 (("<left>"
   (command vscode-mode-command/workbench\.statusBar\.focusPrevious)
   (when-orig . "statusBarFocused")
   (when vscode-mode-cond/statusBarFocused)))
 (("C-S-d"
   (command vscode-mode-command/workbench\.view\.debug)
   (when-orig)
   (when)))
 (("C-S-e"
   (command vscode-mode-command/workbench\.view\.explorer)
   (when-orig)
   (when)))
 (("C-S-x"
   (command vscode-mode-command/workbench\.view\.extensions)
   (when-orig)
   (when)))
 (("C-S-g"
   (command vscode-mode-command/workbench\.view\.scm)
   (when-orig)
   (when)))
 (("C-S-f"
   (command vscode-mode-command/workbench\.view\.search)
   (when-orig . "!searchViewletVisible")
   (when not
	 (vscode-mode-cond/searchViewletVisible))))
 (("C-<right>"
   (command vscode-mode-command/breadcrumbs\.focusNextWithPicker)
   (when-orig . "breadcrumbsActive && breadcrumbsVisible && listFocus && !inputFocus")
   (when and
	 (vscode-mode-cond/breadcrumbsActive)
	 (vscode-mode-cond/breadcrumbsVisible)
	 (vscode-mode-cond/listFocus)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("C-<left>"
   (command vscode-mode-command/breadcrumbs\.focusPreviousWithPicker)
   (when-orig . "breadcrumbsActive && breadcrumbsVisible && listFocus && !inputFocus")
   (when and
	 (vscode-mode-cond/breadcrumbsActive)
	 (vscode-mode-cond/breadcrumbsVisible)
	 (vscode-mode-cond/listFocus)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("ESC"
   (command vscode-mode-command/breadcrumbs\.selectEditor)
   (when-orig . "breadcrumbsActive && breadcrumbsVisible")
   (when and
	 (vscode-mode-cond/breadcrumbsActive)
	 (vscode-mode-cond/breadcrumbsVisible))))
 (("C-k <down>"
   (command vscode-mode-command/views\.moveViewDown)
   (when-orig . "focusedView != ''")
   (when not
	 (equal
	  (vscode-mode-cond/focusedView)
	  ""))))
 (("C-k <left>"
   (command vscode-mode-command/views\.moveViewLeft)
   (when-orig . "focusedView != ''")
   (when not
	 (equal
	  (vscode-mode-cond/focusedView)
	  ""))))
 (("C-k <right>"
   (command vscode-mode-command/views\.moveViewRight)
   (when-orig . "focusedView != ''")
   (when not
	 (equal
	  (vscode-mode-cond/focusedView)
	  ""))))
 (("C-k <up>"
   (command vscode-mode-command/views\.moveViewUp)
   (when-orig . "focusedView != ''")
   (when not
	 (equal
	  (vscode-mode-cond/focusedView)
	  ""))))
 (("f6"
   (command vscode-mode-command/workbench\.action\.debug\.pause)
   (when-orig . "debugState == 'running'")
   (when equal
	 (vscode-mode-cond/debugState)
	 "running")))
 (("f2"
   (command vscode-mode-command/debug\.renameWatchExpression)
   (when-orig . "watchExpressionsFocused")
   (when vscode-mode-cond/watchExpressionsFocused)))
 (("f2"
   (command vscode-mode-command/debug\.setVariable)
   (when-orig . "variablesFocused")
   (when vscode-mode-cond/variablesFocused)))
 (("SPC"
   (command vscode-mode-command/debug\.toggleBreakpoint)
   (when-orig . "breakpointsFocused && !inputFocus")
   (when and
	 (vscode-mode-cond/breakpointsFocused)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("S-delete"
   (command vscode-mode-command/deleteFile)
   (when-orig . "explorerViewletVisible && filesExplorerFocus && !explorerResourceReadonly && !inputFocus")
   (when and
	 (vscode-mode-cond/explorerViewletVisible)
	 (vscode-mode-cond/filesExplorerFocus)
	 (not
	  (vscode-mode-cond/explorerResourceReadonly))
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("delete"
   (command vscode-mode-command/deleteFile)
   (when-orig . "explorerViewletVisible && filesExplorerFocus && !explorerResourceMoveableToTrash && !explorerResourceReadonly && !inputFocus")
   (when and
	 (vscode-mode-cond/explorerViewletVisible)
	 (vscode-mode-cond/filesExplorerFocus)
	 (not
	  (vscode-mode-cond/explorerResourceMoveableToTrash))
	 (not
	  (vscode-mode-cond/explorerResourceReadonly))
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("ESC"
   (command vscode-mode-command/editor\.closeCallHierarchy)
   (when-orig . "callHierarchyVisible && !config.editor.stablePeek")
   (when and
	 (vscode-mode-cond/callHierarchyVisible)
	 (not
	  (vscode-mode-cond/config\.editor\.stablePeek)))))
 (("RET"
   (command vscode-mode-command/explorer\.openAndPassFocus)
   (when-orig . "explorerViewletVisible && filesExplorerFocus && !explorerResourceIsFolder && !inputFocus")
   (when and
	 (vscode-mode-cond/explorerViewletVisible)
	 (vscode-mode-cond/filesExplorerFocus)
	 (not
	  (vscode-mode-cond/explorerResourceIsFolder))
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("ESC"
   (command vscode-mode-command/filesExplorer\.cancelCut)
   (when-orig . "explorerResourceCut && explorerViewletVisible && filesExplorerFocus && !inputFocus")
   (when and
	 (vscode-mode-cond/explorerResourceCut)
	 (vscode-mode-cond/explorerViewletVisible)
	 (vscode-mode-cond/filesExplorerFocus)
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("C-c"
   (command vscode-mode-command/filesExplorer\.copy)
   (when-orig . "explorerViewletVisible && filesExplorerFocus && !explorerResourceIsRoot && !inputFocus")
   (when and
	 (vscode-mode-cond/explorerViewletVisible)
	 (vscode-mode-cond/filesExplorerFocus)
	 (not
	  (vscode-mode-cond/explorerResourceIsRoot))
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("C-x"
   (command vscode-mode-command/filesExplorer\.cut)
   (when-orig . "explorerViewletVisible && filesExplorerFocus && !explorerResourceIsRoot && !inputFocus")
   (when and
	 (vscode-mode-cond/explorerViewletVisible)
	 (vscode-mode-cond/filesExplorerFocus)
	 (not
	  (vscode-mode-cond/explorerResourceIsRoot))
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("SPC"
   (command vscode-mode-command/filesExplorer\.openFilePreserveFocus)
   (when-orig . "explorerViewletVisible && filesExplorerFocus && !explorerResourceIsFolder && !inputFocus")
   (when and
	 (vscode-mode-cond/explorerViewletVisible)
	 (vscode-mode-cond/filesExplorerFocus)
	 (not
	  (vscode-mode-cond/explorerResourceIsFolder))
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("C-v"
   (command vscode-mode-command/filesExplorer\.paste)
   (when-orig . "explorerViewletVisible && filesExplorerFocus && !explorerResourceReadonly && !inputFocus")
   (when and
	 (vscode-mode-cond/explorerViewletVisible)
	 (vscode-mode-cond/filesExplorerFocus)
	 (not
	  (vscode-mode-cond/explorerResourceReadonly))
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("home"
   (command vscode-mode-command/firstCompressedFolder)
   (when-orig . "explorerViewletCompressedFocus && explorerViewletVisible && filesExplorerFocus && !explorerViewletCompressedFirstFocus && !inputFocus")
   (when and
	 (vscode-mode-cond/explorerViewletCompressedFocus)
	 (vscode-mode-cond/explorerViewletVisible)
	 (vscode-mode-cond/filesExplorerFocus)
	 (not
	  (vscode-mode-cond/explorerViewletCompressedFirstFocus))
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("end"
   (command vscode-mode-command/lastCompressedFolder)
   (when-orig . "explorerViewletCompressedFocus && explorerViewletVisible && filesExplorerFocus && !explorerViewletCompressedLastFocus && !inputFocus")
   (when and
	 (vscode-mode-cond/explorerViewletCompressedFocus)
	 (vscode-mode-cond/explorerViewletVisible)
	 (vscode-mode-cond/filesExplorerFocus)
	 (not
	  (vscode-mode-cond/explorerViewletCompressedLastFocus))
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("delete"
   (command vscode-mode-command/moveFileToTrash)
   (when-orig . "explorerResourceMoveableToTrash && explorerViewletVisible && filesExplorerFocus && !explorerResourceReadonly && !inputFocus")
   (when and
	 (vscode-mode-cond/explorerResourceMoveableToTrash)
	 (vscode-mode-cond/explorerViewletVisible)
	 (vscode-mode-cond/filesExplorerFocus)
	 (not
	  (vscode-mode-cond/explorerResourceReadonly))
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("<right>"
   (command vscode-mode-command/nextCompressedFolder)
   (when-orig . "explorerViewletCompressedFocus && explorerViewletVisible && filesExplorerFocus && !explorerViewletCompressedLastFocus && !inputFocus")
   (when and
	 (vscode-mode-cond/explorerViewletCompressedFocus)
	 (vscode-mode-cond/explorerViewletVisible)
	 (vscode-mode-cond/filesExplorerFocus)
	 (not
	  (vscode-mode-cond/explorerViewletCompressedLastFocus))
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("<left>"
   (command vscode-mode-command/previousCompressedFolder)
   (when-orig . "explorerViewletCompressedFocus && explorerViewletVisible && filesExplorerFocus && !explorerViewletCompressedFirstFocus && !inputFocus")
   (when and
	 (vscode-mode-cond/explorerViewletCompressedFocus)
	 (vscode-mode-cond/explorerViewletVisible)
	 (vscode-mode-cond/filesExplorerFocus)
	 (not
	  (vscode-mode-cond/explorerViewletCompressedFirstFocus))
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("delete"
   (command vscode-mode-command/remote\.tunnel\.closeInline)
   (when-orig . "tunnelCloseable && tunnelViewFocus")
   (when and
	 (vscode-mode-cond/tunnelCloseable)
	 (vscode-mode-cond/tunnelViewFocus))))
 (("C-c"
   (command vscode-mode-command/remote\.tunnel\.copyAddressInline)
   (when-orig . "tunnelViewFocus && tunnelType == 'Detected' || tunnelViewFocus && tunnelType == 'Forwarded'")
   (when or
	 (and
	   (vscode-mode-cond/tunnelViewFocus)
	   (equal
	    (vscode-mode-cond/tunnelType)
	    "Detected"))
	 (and
	   (vscode-mode-cond/tunnelViewFocus)
	   (equal
	    (vscode-mode-cond/tunnelType)
	    "Forwarded")))))
 (("f2"
   (command vscode-mode-command/remote\.tunnel\.label)
   (when-orig . "tunnelViewFocus && tunnelType == 'Forwarded'")
   (when and
	 (vscode-mode-cond/tunnelViewFocus)
	 (equal
	  (vscode-mode-cond/tunnelType)
	  "Forwarded"))))
 (("f2"
   (command vscode-mode-command/renameFile)
   (when-orig . "explorerViewletVisible && filesExplorerFocus && !explorerResourceIsRoot && !explorerResourceReadonly && !inputFocus")
   (when and
	 (vscode-mode-cond/explorerViewletVisible)
	 (vscode-mode-cond/filesExplorerFocus)
	 (not
	  (vscode-mode-cond/explorerResourceIsRoot))
	 (not
	  (vscode-mode-cond/explorerResourceReadonly))
	 (not
	  (vscode-mode-cond/inputFocus)))))
 (("f11"
   (command vscode-mode-command/workbench\.action\.debug\.stepInto)
   (when-orig . "debugState != 'inactive'")
   (when not
	 (equal
	  (vscode-mode-cond/debugState)
	  "inactive"))))
 (("S-ESC"
   (command vscode-mode-command/closeReferenceSearch)
   (when-orig . "referenceSearchVisible && !config.editor.stablePeek")
   (when and
	 (vscode-mode-cond/referenceSearchVisible)
	 (not
	  (vscode-mode-cond/config\.editor\.stablePeek)))))
 (("ESC"
   (command vscode-mode-command/closeReferenceSearch)
   (when-orig . "referenceSearchVisible && !config.editor.stablePeek")
   (when and
	 (vscode-mode-cond/referenceSearchVisible)
	 (not
	  (vscode-mode-cond/config\.editor\.stablePeek)))))
 (("ESC"
   (command vscode-mode-command/notifications\.hideList)
   (when-orig . "notificationCenterVisible")
   (when vscode-mode-cond/notificationCenterVisible)))
 (("M-<left>"
   (command vscode-mode-command/workbench\.action\.quickInputBack)
   (when-orig . "inQuickOpen")
   (when vscode-mode-cond/inQuickOpen)))
 (("C-tab"
   (command vscode-mode-command/workbench\.action\.quickOpenNavigateNextInEditorPicker)
   (when-orig . "inEditorsPicker && inQuickOpen")
   (when and
	 (vscode-mode-cond/inEditorsPicker)
	 (vscode-mode-cond/inQuickOpen))))
 (("C-e"
   (command vscode-mode-command/workbench\.action\.quickOpenNavigateNextInFilePicker)
   (when-orig . "inFilesPicker && inQuickOpen")
   (when and
	 (vscode-mode-cond/inFilesPicker)
	 (vscode-mode-cond/inQuickOpen))))
 (("C-p"
   (command vscode-mode-command/workbench\.action\.quickOpenNavigateNextInFilePicker)
   (when-orig . "inFilesPicker && inQuickOpen")
   (when and
	 (vscode-mode-cond/inFilesPicker)
	 (vscode-mode-cond/inQuickOpen))))
 (("C-r"
   (command vscode-mode-command/workbench\.action\.quickOpenNavigateNextInRecentFilesPicker)
   (when-orig . "inQuickOpen && inRecentFilesPicker")
   (when and
	 (vscode-mode-cond/inQuickOpen)
	 (vscode-mode-cond/inRecentFilesPicker))))
 (("C-q"
   (command vscode-mode-command/workbench\.action\.quickOpenNavigateNextInViewPicker)
   (when-orig . "inQuickOpen && inViewsPicker")
   (when and
	 (vscode-mode-cond/inQuickOpen)
	 (vscode-mode-cond/inViewsPicker))))
 (("C-S-tab"
   (command vscode-mode-command/workbench\.action\.quickOpenNavigatePreviousInEditorPicker)
   (when-orig . "inEditorsPicker && inQuickOpen")
   (when and
	 (vscode-mode-cond/inEditorsPicker)
	 (vscode-mode-cond/inQuickOpen))))
 (("C-S-e"
   (command vscode-mode-command/workbench\.action\.quickOpenNavigatePreviousInFilePicker)
   (when-orig . "inFilesPicker && inQuickOpen")
   (when and
	 (vscode-mode-cond/inFilesPicker)
	 (vscode-mode-cond/inQuickOpen))))
 (("C-S-p"
   (command vscode-mode-command/workbench\.action\.quickOpenNavigatePreviousInFilePicker)
   (when-orig . "inFilesPicker && inQuickOpen")
   (when and
	 (vscode-mode-cond/inFilesPicker)
	 (vscode-mode-cond/inQuickOpen))))
 (("C-S-r"
   (command vscode-mode-command/workbench\.action\.quickOpenNavigatePreviousInRecentFilesPicker)
   (when-orig . "inQuickOpen && inRecentFilesPicker")
   (when and
	 (vscode-mode-cond/inQuickOpen)
	 (vscode-mode-cond/inRecentFilesPicker))))
 (("C-S-q"
   (command vscode-mode-command/workbench\.action\.quickOpenNavigatePreviousInViewPicker)
   (when-orig . "inQuickOpen && inViewsPicker")
   (when and
	 (vscode-mode-cond/inQuickOpen)
	 (vscode-mode-cond/inViewsPicker))))
 (("C-r"
   (command vscode-mode-command/workbench\.action\.reloadWindow)
   (when-orig . "isDevelopment")
   (when vscode-mode-cond/isDevelopment)))
 (("C-S-i"
   (command vscode-mode-command/workbench\.action\.toggleDevTools)
   (when-orig . "isDevelopment")
   (when vscode-mode-cond/isDevelopment)))
 (("ESC"
   (command vscode-mode-command/notifications\.hideToasts)
   (when-orig . "notificationFocus && notificationToastsVisible")
   (when and
	 (vscode-mode-cond/notificationFocus)
	 (vscode-mode-cond/notificationToastsVisible))))
 (("f10"
   (command vscode-mode-command/extension\.node-debug\.startWithStopOnEntry)
   (when-orig . "!inDebugMode && debugConfigurationType == 'node'")
   (when and
	 (not
	  (vscode-mode-cond/inDebugMode))
	 (equal
	  (vscode-mode-cond/debugConfigurationType)
	  "node"))))
 (("C-k C-M-s"
   (command vscode-mode-command/git\.stageSelectedRanges)
   (when-orig . "isInDiffEditor")
   (when vscode-mode-cond/isInDiffEditor)))
 (("C-S-v"
   (command vscode-mode-command/markdown\.showPreview)
   (when-orig . "!notebookEditorFocused && editorLangId == 'markdown'")
   (when and
	 (not
	  (vscode-mode-cond/notebookEditorFocused))
	 (equal
	  (vscode-mode-cond/editorLangId)
	  "markdown"))))
 (("S-M-f12"
   (command vscode-mode-command/references-view\.findReferences)
   (when-orig . "editorHasReferenceProvider")
   (when vscode-mode-cond/editorHasReferenceProvider)))
 (("C-k C-n"
   (command vscode-mode-command/git\.unstageSelectedRanges)
   (when-orig . "isInDiffEditor")
   (when vscode-mode-cond/isInDiffEditor)))
 (("C-k v"
   (command vscode-mode-command/markdown\.showPreviewToSide)
   (when-orig . "!notebookEditorFocused && editorLangId == 'markdown'")
   (when and
	 (not
	  (vscode-mode-cond/notebookEditorFocused))
	 (equal
	  (vscode-mode-cond/editorLangId)
	  "markdown"))))
 (("f4"
   (command vscode-mode-command/references-view\.next)
   (when-orig . "reference-list.hasResult && references-view.canNavigate")
   (when and
	 (vscode-mode-cond/reference-list\.hasResult)
	 (vscode-mode-cond/references-view\.canNavigate))))
 (("C-k C-r"
   (command vscode-mode-command/git\.revertSelectedRanges)
   (when-orig . "isInDiffEditor")
   (when vscode-mode-cond/isInDiffEditor)))
 (("S-f4"
   (command vscode-mode-command/references-view\.prev)
   (when-orig . "reference-list.hasResult && references-view.canNavigate")
   (when and
	 (vscode-mode-cond/reference-list\.hasResult)
	 (vscode-mode-cond/references-view\.canNavigate))))
 (("S-M-h"
   (command vscode-mode-command/references-view\.showCallHierarchy)
   (when-orig . "editorHasCallHierarchyProvider")
   (when vscode-mode-cond/editorHasCallHierarchyProvider))))
