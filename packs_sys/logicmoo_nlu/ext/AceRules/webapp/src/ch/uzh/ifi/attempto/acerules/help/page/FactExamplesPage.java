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


public class FactExamplesPage extends Page {
	
	public FactExamplesPage(HelpWindow helpWindow) {
		super(helpWindow, "Facts by examples", "Language");
		addText("Here we present a collection of examples of facts. This is probably the easiest and fastest way to learn the " +
				"AceRules syntax. See also the 'Rules by examples' section.");
		addHelpLink("Rules by examples");
		addGap();
		addParagraph("A simple fact can consist of a proper name and a intransitive verb.");
		addQuote("John waits.");
		addQuote("Mary sleeps.");
		addGap();
		addParagraph("Proper names have to begin with a capital letter. Each sentence has to end with a full stop.");
		addParagraph("You can also use common nouns. They have to be preceded by 'a' in the case of singluar or 'some' in the " +
				"case of plural.");
		addQuote("A customer eats.");
		addQuote("Some managers drink.");
		addGap();
		addParagraph("The use of transitive and ditransitive verbs is straightforward.");
		addQuote("John sees a customer.");
		addQuote("Bill gives Mary a book.");
		addQuote("A manager eats a steak.");
		addGap();
		addParagraph("In all these cases, you can add one or more prepositional phrases to the sentences.");
		addQuote("John waits in a park.");
		addQuote("John waits in a park on a bench.");
		addQuote("Bill uses a hammer in the office.");
		addGap();
		addParagraph("The verb 'be' is interpreted as identity. In this way, you can express that two objects or persons are " +
				"the same.");
		addQuote("John is a customer.");
		addQuote("Mary is a clerk.");
		addGap();
		addParagraph("The verb 'be' can also be used for 'is a ... of' constructs.");
		addQuote("John is a customer of Bill.");
		addQuote("Ted is the father of Mary.");
		addQuote("A screw is a part of a car.");
		addGap();
		addParagraph("You can also use comparative forms of adjectives to express relations.");
		addQuote("Mary is taller than John.");
		addQuote("A customer is more important than Bill.");
		addGap();
		addText("All the sentences above can also be negated. Note that negated facts are only " +
				"allowed if the mode accepts strong negation.");
		addHelpLink("Modes");
		addGap();
		addQuote("John does not wait.");
		addQuote("Mary does not eat a steak.");
		addQuote("Bill does not sleep in a park.");
		addQuote("John is not more important than a manager.");
		addQuote("Tom is not a customer.");
		addQuote("Bill is not a son of an artist.");
		addGap();
		addParagraph("Alternatively, you can use the phrase 'it is false that' to express negation.");
		addQuote("It is false that John waits.");
		addGap();
	}

}
