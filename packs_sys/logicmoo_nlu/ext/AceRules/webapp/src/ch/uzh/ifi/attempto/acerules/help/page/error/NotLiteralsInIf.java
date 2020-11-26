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


public class NotLiteralsInIf extends Page {
	
	public NotLiteralsInIf(HelpWindow helpWindow) {
		super(helpWindow, "Not literals in if-part", "Errors");
		addText("The if-part of a rule is not allowed to contain complex structures such as other if-then-structures (or their equivalents with 'every' or 'no'). The following rule, for example, is invalid:");
		addQuote("If every man knows John then John is important.");
		addGap();
		addHeading("Suggestions");
		addParagraph("Rephrase the respective sentence, so that it contains no such structures in its if-part.");
	}

}
