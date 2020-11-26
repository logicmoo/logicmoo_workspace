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


public class InvalidIdentity extends Page {
	
	public InvalidIdentity(HelpWindow helpWindow) {
		super(helpWindow, "Invalid identity", "Errors");
		addGap();
		addText("The use of identity is restricted. It must not occur of its own in the then-parts of rules and in negations. " +
				"The following example shows an invalid rule:");
		addQuote("If John knows a manager X and Mary knows a manager Y then X is Y.");
		addText("Furthermore, proper names stand for different objects or persons. You cannot unify them using identity:");
		addQuote("Billy is Bill.");
		addGap();
		addHeading("Suggestions");
		addParagraph("Rephrase the sentences containing invalid identities, or remove them.");
	}

}
