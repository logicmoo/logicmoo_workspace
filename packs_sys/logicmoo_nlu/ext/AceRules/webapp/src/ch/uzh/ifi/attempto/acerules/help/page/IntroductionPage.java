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


public class IntroductionPage extends Page {
	
	public IntroductionPage(HelpWindow helpWindow) {
		super(helpWindow, "Introduction", "General");
		addText("Check out the following demo video:");
		addWebLink("AceRules Demo Video", "http://attempto.ifi.uzh.ch/site/docs/screencast_acerules.mov");
		addGap();
		addText("AceRules is a rule system that uses Attempto Controlled English (ACE) to formalize the rules and facts. " +
				"ACE is a controlled natural language, which means that it looks like natural English but is in fact completely formal.");
		addHelpLink("Attempto Controlled English");
		addGap();
		addParagraph("The main goal of AceRules is to make rule management easy for everybody. People with no computer science " +
				"background should be able to deal with AceRules.");
		addParagraph("This web interface should serve a proof of concept that a controlled natural language (like ACE) can be used as a " +
				"convenient and natural interface for formal rule systems.");
		addText("AceRules does not rely on a specific semantics. At the moment, AceRules allows to choose between three different " +
				"modes, each of which has its own semantics. Further semantics could be incorporated with only little integration " +
				"effort.");
		addHelpLink("Modes");
		addGap();
		addParagraph("The input of AceRules is a program that consists of rules and facts. Since these rules and facts are written in " +
				"ACE, everybody should be able to read and understand them. As output, AceRules returns the set of facts that can be " +
				"concluded by the program (according to the given semantics). This output we call 'answer' and it is again displayed " +
				"in ACE. Thus the interaction with the user is completely done via controlled natural language.");
		addText("To learn how to do your first steps with AceRules, see the quick start section:");
		addHelpLink("Quick start");
		addGap();
	}

}
