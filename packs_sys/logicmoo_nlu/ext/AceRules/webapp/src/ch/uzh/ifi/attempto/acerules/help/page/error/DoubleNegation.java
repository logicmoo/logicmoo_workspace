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

package ch.uzh.ifi.attempto.acerules.help.page.error;

import ch.uzh.ifi.attempto.acerules.help.HelpWindow;
import ch.uzh.ifi.attempto.acerules.help.page.Page;


public class DoubleNegation extends Page {
	
	public DoubleNegation(HelpWindow helpWindow) {
		super(helpWindow, "Double negation is not supported", "Errors");
		addText("In modes that allow only one kind of negation, both syntactic forms of negation are interpreted identically. Thus, " +
				"it does not make sense to negate a statement with both syntactic negations like '... is not provably not ...'.");
		addHelpLink("Two kinds of negation");
		addGap();
		addHeading("Suggestions");
		addParagraph("Switch to a mode that has support for both kinds of negation.");
		addParagraph("Or remove all double negations from your program.");
	}

}
