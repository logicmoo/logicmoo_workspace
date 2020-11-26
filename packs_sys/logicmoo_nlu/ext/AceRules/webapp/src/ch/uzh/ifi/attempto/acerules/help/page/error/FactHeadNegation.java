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


public class FactHeadNegation extends Page {
	
	public FactHeadNegation(HelpWindow helpWindow) {
		super(helpWindow, "Negation in fact or head of a rule", "Errors");
		addText("In modes that have only negation as failure, the facts and the then-parts of rules are not allowed to contain " +
				"negation. Negation as failure must only occur in the if-parts of rules.");
		addHelpLink("Modes");
		addGap();
		addHeading("Suggestions");
		addParagraph("Switch to a mode that has support for strong negation.");
		addParagraph("Or rephrase your rule and remove the negation in the then-part.");
	}

}
