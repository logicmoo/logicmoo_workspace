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


public class NoAnswer extends Page {
	
	public NoAnswer(HelpWindow helpWindow) {
		super(helpWindow, "Program has no answer", "Errors");
		addParagraph("There is no answer for your program. That means that your program contains a contradiction (i.e. it is " +
				"inconsistent).");
		addHeading("Suggestions");
		addParagraph("Modify your program in order to get rid of the inconsistency.");
		addText("Alternatively, you can switch to another mode that gives always at least one answer.");
		addHelpLink("Modes");
		addGap();
	}

}
