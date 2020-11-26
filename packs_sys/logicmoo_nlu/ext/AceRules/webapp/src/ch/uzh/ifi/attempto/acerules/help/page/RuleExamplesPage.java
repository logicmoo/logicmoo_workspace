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


public class RuleExamplesPage extends Page {
	
	public RuleExamplesPage(HelpWindow helpWindow) {
		super(helpWindow, "Rules by examples", "Language");
		addText("Here we present a collection of examples of rules. This is probably the easiest and fastest way to learn the " +
				"AceRules syntax. See also the 'Facts by examples' section.");
		addHelpLink("Facts by examples");
		addGap();
		addParagraph("The simplest form of a rule starts with 'every'.");
		addQuote("Every customer waits.");
		addQuote("Every man knows Mary.");
		addQuote("Every woman is a daughter of someone.");
		addQuote("Every customer is more important than Bill.");
		addGap();
		addParagraph("In the same way, one can use 'everybody' or 'everything'.");
		addQuote("Everybody drinks.");
		addQuote("Everything is important.");
		addGap();
		addParagraph("All these rules can also be expressed as an if-then sentence.");
		addQuote("If there is a customer then he/she waits.");
		addQuote("If there is a man then he knows Mary.");
		addQuote("If there is somebody then he/she drinks.");
		addQuote("If there is something then it is important.");
		addGap();
		addText("In order to negate the consequence of a rule, one can use 'no' instead of 'every'. " +
				"Note that this is only possible for modes that allow strong negation.");
		addHelpLink("Modes");
		addGap();
		addQuote("No customer waits.");
		addQuote("No woman loves a car.");
		addGap();
		addParagraph("We can also use 'nobody' or 'nothing'.");
		addQuote("Nobody drinks.");
		addQuote("Nothing is important.");
		addGap();
		addParagraph("These rules can still be rephrased as an if-then sentence.");
		addQuote("If there is a customer then he/she does not wait.");
		addQuote("If there is a woman then she does not love a car.");
		addQuote("If there is somebody then he/she does not drink.");
		addQuote("If there is something then it is not important.");
		addGap();
		addParagraph("You can use variables for referencing objects.");
		addQuote("If a man X waits then X is a customer.");
		addQuote("If a customer Y knows John then Y is important.");
		addGap();
		addParagraph("You can put several conditions in the if-part of a rule. These conditions have to be separated by 'and'.");
		addQuote("If a customer X is important and X has a car then John likes X.");
		addQuote("If someone X knows someone Y and Y is a manager then X is not an employee.");
		addGap();
		addParagraph("The conditions in the if-part of a rule can be negated, using strong negation or negation as failure. (If the " +
				"current mode supports only one kind of negation then both forms are interpreted identically.)");
		addQuote("If someone X is not a customer then X is an employee.");
		addQuote("If someone X is not provably a criminal then X is trustworthy.");
		addQuote("If someone X is not provably a criminal and X is not a clerk then X is important.");
		addGap();
	}

}
