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


public class NotFlatInsideOfThen extends Page {
	
	public NotFlatInsideOfThen(HelpWindow helpWindow) {
		super(helpWindow, "Not flat inside of then-part", "Errors");
		addText("The then-part of a rule is not allowed to contain complex such as other if-then-statements (or their equivalents with 'every' or 'no'). A negation is allowed in the then-part only if it is a single negation and if there are no other statements. The two examples below show invalid sentences:");
		addQuote("If John has a car then every woman loves John.");
		addQuote("If John has a house then John has a car and he does not have a bike.");
		addGap();
		addHeading("Suggestions");
		addParagraph("Rephrase the respective sentences and make sure that the content inside of the then-part is 'flat'.");
	}

}
