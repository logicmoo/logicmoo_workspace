// This file is part of AceRules.
// Copyright 2008-2012, Tobias Kuhn, http://www.tkuhn.ch
//
// AceRules is free software: you can redistribute it and/or modify it under the terms of the GNU
// Lesser General Public License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// AceRules is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
// even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with AceRules. If
// not, see http://www.gnu.org/licenses/.

package ch.uzh.ifi.attempto.acerules.help.page;

import ch.uzh.ifi.attempto.acerules.help.HelpWindow;


public class MenuBarPage extends Page {
	
	public MenuBarPage(HelpWindow helpWindow) {
		super(helpWindow, "Menu bar", "Screen components");
		addParagraph("The menu bar contains the menus 'AceRules' and 'Program'. The question mark item '?' leads you to this help page.");
		addHeading("Menu 'AceRules'");
		addParagraph("This menu contains the commands to display information about AceRules.");
		addBoldText("Introduction:");
		addText("Shows an introduction to AceRules.");
		addHelpLink("Introduction");
		addGap();
		addBoldText("Quick start:");
		addText("Shows a quick start guide intended for new users.");
		addHelpLink("Quick start");
		addGap();
		addBoldText("Help index:");
		addText("Shows the index page of the AceRules help. From this page you can access all the other help pages.");
		addHelpLink("Help index");
		addGap();
		addBoldText("About:");
		addText("Information about the AceRules system.");
		addHelpLink("About AceRules");
		addGap();
		addHeading("Menu 'Program'");
		addParagraph("This menu contains the commands for running and managing your programs.");
		addBoldText("Run:");
		addParagraph("Runs the program that is currently in the program area.");
		addBoldText("Clear:");
		addParagraph("Clears the program area. The previous program can be restored using the 'Back in history' command.");
		addBoldText("Guess unknown words:");
		addParagraph("You can switch this option on, or turn it off. If it is on then AceRules tries to guess unknown words. " +
				"Otherwise you get an error message if a word is not known.");
		addBoldText("Save:");
		addParagraph("This commands saves your current program. After that, you can load it using the command 'Saved programs'.");
		addBoldText("Saved programs:");
		addParagraph("Shows a list of your saved programs. You can then load one of them. Deleting existing programs is also possible");
		addBoldText("Load example:");
		addParagraph("With this command you can load one of several predefined example programs.");
		addBoldText("Back in history:");
		addParagraph("Makes a step back in the history of the program area. This can be used to restore former programs.");
		addBoldText("Forward in history:");
		addParagraph("With this command you can go forward in the history of the program area. This works only if you did the " +
				"command 'Back in history' before.");
		addHeading("Menu 'Mode'");
		addText("AceRules allows to choose between different modes which correspond to different rule theories. Thus each mode " +
				"stands for a different semantics.");
		addHelpLink("Modes");
		addGap();
		addBoldText("Courteous:");
		addText("Switches to courteous mode.");
		addHelpLink("Courteous mode");
		addGap();
		addBoldText("Stable:");
		addText("Switches to stable mode.");
		addHelpLink("Stable mode");
		addGap();
		addBoldText("Stable with strong negation:");
		addText("Switches to 'stable with strong negation' mode.");
		addHelpLink("Stable with strong negation mode");
		addGap();
		addBoldText("Maximal number of answers:");
		addParagraph("Determines how many answers should be calculated as a maximum.");
		addBoldText("Mode help:");
		addText("Shows the mode help page:");
		addHelpLink("Modes");
		addGap();
		addHeading("Menu 'View'");
		addParagraph("This menu contains the commands to show or hide different output areas.");
		addBoldText("Show answer:");
		addText("Shows or hides the answer area.");
		addHelpLink("Answer area");
		addGap();
		addBoldText("Show trace:");
		addText("Shows or hides the trace area.");
		addHelpLink("Trace area");
		addGap();
	}

}
