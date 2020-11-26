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


public class PrioritiesNotSupported extends Page {
	
	public PrioritiesNotSupported(HelpWindow helpWindow) {
		super(helpWindow, "Priorities are not supported", "Errors");
		addText("Priority definitions (i.e. 'overrides' statements) are only allowed in courteous mode.");
		addHelpLink("Modes");
		addGap();
		addHeading("Suggestions");
		addParagraph("Switch to another mode that allows priorities.");
		addParagraph("Or remove your priority definition from your program.");
	}

}
