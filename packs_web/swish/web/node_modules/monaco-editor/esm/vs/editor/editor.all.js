/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
import './browser/controller/coreCommands';
import './browser/widget/codeEditorWidget';
import './browser/widget/diffEditorWidget';
import './browser/widget/diffNavigator';
import './contrib/bracketMatching/bracketMatching';
import './contrib/caretOperations/caretOperations';
import './contrib/caretOperations/transpose';
import './contrib/clipboard/clipboard';
import './contrib/codeAction/codeActionContributions';
import './contrib/codelens/codelensController';
import './contrib/colorPicker/colorDetector';
import './contrib/comment/comment';
import './contrib/contextmenu/contextmenu';
import './contrib/cursorUndo/cursorUndo';
import './contrib/dnd/dnd';
import './contrib/find/findController';
import './contrib/folding/folding';
import './contrib/fontZoom/fontZoom';
import './contrib/format/formatActions';
import './contrib/gotoSymbol/goToCommands';
import './contrib/gotoSymbol/link/goToDefinitionAtPosition';
import './contrib/gotoError/gotoError';
import './contrib/hover/hover';
import './contrib/inPlaceReplace/inPlaceReplace';
import './contrib/linesOperations/linesOperations';
import './contrib/links/links';
import './contrib/multicursor/multicursor';
import './contrib/parameterHints/parameterHints';
import './contrib/rename/rename';
import './contrib/smartSelect/smartSelect';
import './contrib/snippet/snippetController2';
import './contrib/suggest/suggestController';
import './contrib/tokenization/tokenization';
import './contrib/toggleTabFocusMode/toggleTabFocusMode';
import './contrib/wordHighlighter/wordHighlighter';
import './contrib/wordOperations/wordOperations';
import './contrib/wordPartOperations/wordPartOperations';
// Load up these strings even in VSCode, even if they are not used
// in order to get them translated
import './common/standaloneStrings';
