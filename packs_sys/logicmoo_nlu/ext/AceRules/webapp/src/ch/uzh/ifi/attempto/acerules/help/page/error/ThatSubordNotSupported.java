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


public class ThatSubordNotSupported extends Page {
	
	public ThatSubordNotSupported(HelpWindow helpWindow) {
		super(helpWindow, "That-subordination is not supported", "Errors");
		addParagraph("That-subordinations as in 'John knows that Mary sleeps' are not supported. Supported are only the phrases 'it is false that', 'it is true that', and 'it is not provable that'.");
		addHeading("Suggestion");
		addParagraph("Rephrase the respective sentence without using that-subordination.");
	}

}
