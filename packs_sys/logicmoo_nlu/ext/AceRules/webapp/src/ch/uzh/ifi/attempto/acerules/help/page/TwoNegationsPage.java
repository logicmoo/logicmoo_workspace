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


public class TwoNegationsPage extends Page {
	
	public TwoNegationsPage(HelpWindow helpWindow) {
		super(helpWindow, "Two kinds of negation", "Language");
		addText("Essentially, there can be two different kinds of negation in rule systems: strong negation (also called 'classical " +
				"negation' or 'true negation') and negation as failure (also called 'weak negation' or 'default negation'). Some theories " +
				"allow only one of them, others allow the use of both. In AceRules, this depends on the mode.");
		addHelpLink("Modes");
		addGap();
		addParagraph("ACE allows to distinguish these two kinds of negation. If the mode of AceRules, however, allows only one kind of " +
				"negation then both syntactic forms are interpreted in the same way. Thus, if the mode of AceRules allows only " +
				"negation as failure - for example - then the strong negation syntax of ACE is interpreted as negation as failure as well.");
		addParagraph("With strong negation, we can state that something is provably false. This is the kind of negation that classical " +
				"logic theory builds on. In ACE, we express this with the common negation constructs of natural English:");
		addQuote("... does not ..., ... is not ...");
		addQuote("no ..., nothing, nobody");
		addQuote("it is false that ...");
		addGap();
		addParagraph("Negation as failure stands for a weaker form of negation. It means that there is no evidence that " +
				"something is true. It follows the principle: if we cannot show that it is true then we consider it false. ACE provides " +
				"two language constructs for representing negation as failure:");
		addQuote("... does not provably ..., ... is not provably ...");
		addQuote("it is not provable that ...");
		addGap();
		addParagraph("A program can make use of both kinds of negation. If the mode allows only one of them then both forms are " +
				"interpreted identically.");
	}

}
