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


public class RedefinedVariable extends Page {
	
	public RedefinedVariable(HelpWindow helpWindow) {
		super(helpWindow, "Redefined variable", "Errors");
		addText("A variable has been defined more than once. For example, the following sentence is not valid:");
		addQuote("A man X waits and a man X sleeps.");
		addGap();
		addHeading("Suggestions");
		addText("If both occurrences should refer to the same object then you have to use a definite article after the first occurrence. For the example above, write instead:");
		addQuote("A man X waits and the man X sleeps.");
		addGap();
		addText("If they should refer to different objects then you have to use different variable names. For the example above, write instead:");
		addQuote("A man X waits and a man Y sleeps.");
		addGap();
	}

}
