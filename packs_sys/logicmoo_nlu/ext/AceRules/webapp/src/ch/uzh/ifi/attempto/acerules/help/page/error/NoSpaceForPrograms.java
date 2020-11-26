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


public class NoSpaceForPrograms extends Page {
	
	public NoSpaceForPrograms(HelpWindow helpWindow) {
		super(helpWindow, "No space left for programs", "Errors");
		addParagraph("All your saved programs may have altogether at most 7000 characters. This limitation is due to the fact that the saved programs are stored as cookies, and most browsers restrict the size and number of cookies.");
		addHeading("Suggestions");
		addParagraph("Delete some of your saved programs.");
		addParagraph("Alternatively, you can save your program as a local text file using copy/paste.");
	}

}
