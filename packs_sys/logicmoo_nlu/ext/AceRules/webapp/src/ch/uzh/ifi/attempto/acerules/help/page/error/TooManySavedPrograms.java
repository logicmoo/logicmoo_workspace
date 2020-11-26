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


public class TooManySavedPrograms extends Page {
	
	public TooManySavedPrograms(HelpWindow helpWindow) {
		super(helpWindow, "Too many saved programs", "Errors");
		addParagraph("You cannot save more than 10 programs. This limitation is due to the fact that the saved programs are stored as cookies, and most browsers allow only a restricted number of them.");
		addHeading("Suggestions");
		addParagraph("Delete some of your saved programs.");
		addParagraph("Alternatively, you can save your program as a local text file using copy/paste.");
	}

}
