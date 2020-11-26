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


public class NafInTopLevel extends Page {
	
	public NafInTopLevel(HelpWindow helpWindow) {
		super(helpWindow, "NAF in top level", "Errors");
		addText("Negation as failure (NAF) is only allowed to occur in the if-part of rules. The following sentence is not valid:");
		addQuote("It is not provable that John is a criminal.");
		addGap();
		addHeading("Suggestions");
		addText("Make a rule out of the respective sentence. For the example above, write instead something like");
		addQuote("If it is not provable that John is a criminal then John is trustworthy.");
		addGap();
		addText("Or simply replace negation as failure by classical negation. For the example above, write instead");
		addQuote("It is false that John is a criminal.");
		addGap();
	}

}
