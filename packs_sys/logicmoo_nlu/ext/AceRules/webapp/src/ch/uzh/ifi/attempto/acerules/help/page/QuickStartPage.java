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


public class QuickStartPage extends Page {
	
	public QuickStartPage(HelpWindow helpWindow) {
		super(helpWindow, "Quick start", "General");
		addText("This is a guide how to learn quickly the usage of this AceRules interface. The following video gives a " +
				"short demonstration:");
		addWebLink("AceRules Demo Video", "http://attempto.ifi.uzh.ch/site/docs/screencast_acerules.mov");
		addGap();
		addParagraph("If you want to get to learn AceRules, we propose the following three steps. " +
				"As a beginning, it is very useful to try out some example programs. This gives a feeling what AceRules is actually " +
				"doing. Next, you can read our examples of single facts and rules. Using this, you can start writing your own programs. " +
				"Finally, we propose to read some of the help pages that give a more detailed view on the AceRules system.");
		addHeading("Example programs");
		addParagraph("To load an example program, click on the menu 'Program' on the top of the page. Then click 'Load example'. " +
				"Now you can choose an example program to load. Click 'OK' and the example appears in the program area. " +
				"Press now 'Run' to execute the program. After some seconds, the answer is displayed in the answer area.");
		addHeading("Fact and rule examples");
		addText("The following two links lead you to two example collections. These examples will give you a first impression " +
				"how facts and rules have to look like.");
		addHelpLink("Facts by examples");
		addHelpLink("Rules by examples");
		addGap();
		addHeading("Learn more");
		addText("To learn more about the AceRules system and its syntax and semantics, we recommend the following help pages.");
		addHelpLink("Syntax basics");
		addHelpLink("Modes");
		addHelpLink("Two kinds of negation");
		addGap();
	}

}
