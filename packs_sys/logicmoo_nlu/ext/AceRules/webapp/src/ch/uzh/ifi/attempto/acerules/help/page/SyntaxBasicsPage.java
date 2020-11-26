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


public class SyntaxBasicsPage extends Page {
	
	public SyntaxBasicsPage(HelpWindow helpWindow) {
		super(helpWindow, "Syntax basics", "Language");
		addParagraph("This section shows the basic properties of the syntax of AceRules.");
		addHeading("Declarative sentences");
		addParagraph("Only declarative sentences are allowed. Questions or commands are not supported.");
		addText("Instead of");
		addQuote("Get a cookie!");
		addText("write for example");
		addQuote("A person gets a cookie.");
		addGap();
		addHeading("3rd person");
		addParagraph("Only 3rd person is allowed. You can use singular and plural though.");
		addText("Instead of");
		addQuote("I have a car.");
		addText("write for example");
		addQuote("John has a car.");
		addGap();
		addHeading("Simple present");
		addParagraph("The only allowed tense is the simple present.");
		addText("Instead of");
		addQuote("John wrote a book.");
		addText("write");
		addQuote("John writes a book.");
		addGap();
		addHeading("Passive with explicit subject");
		addParagraph("When using passive voice, the preposition 'by' has to be used to specify the subject.");
		addText("Instead of");
		addQuote("A book is given to Mary.");
		addText("write for example");
		addQuote("A book is given to Mary by John.");
		addGap();
		addHeading("Nouns with determiner");
		addParagraph("Nouns always need a preceding determiner. The determiner can be one of 'a', 'the', 'some', 'every', 'all', and 'no'.");
		addText("Instead of");
		addQuote("Customers have a card.");
		addText("write");
		addQuote("Some customers have a card.");
		addText("or");
		addQuote("Every customer has a card.");
		addGap();
		addHeading("If - then");
		addParagraph("Every 'if' in the sentence has to be followed by a 'then'.");
		addText("Instead of");
		addQuote("John waits if he does not have a card.");
		addText("write");
		addQuote("If John does not have a card then he waits.");
		addGap();
	}

}
