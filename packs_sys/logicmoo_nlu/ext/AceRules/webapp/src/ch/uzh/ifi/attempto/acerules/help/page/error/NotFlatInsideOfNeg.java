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


public class NotFlatInsideOfNeg extends Page {
	
	public NotFlatInsideOfNeg(HelpWindow helpWindow) {
		super(helpWindow, "Not flat inside of negation", "Errors");
		addText("A negation is not allowed to contain complex structures such as other negations or if-then-statements. Thus the content inside of a negation has to be 'flat'. The two examples below show invalid sentences:");
		addQuote("It is false that John does not have a car and owns a house.");
		addQuote("It is false that if a man has a dog then the man has a house.");
		addGap();
		addHeading("Suggestions");
		addParagraph("Rephrase the respective sentences and make sure that the content inside of a negation is 'flat'.");
	}

}
