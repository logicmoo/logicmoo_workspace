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


public class NoFullStopPage extends Page {
	
	public NoFullStopPage(HelpWindow helpWindow) {
		super(helpWindow, "No full stop at end", "Errors");
		addParagraph("A program consists of sentences each of which has to end with a full stop ('.'). Thus there must always be a full stop at the end of a program.");
		addHeading("Suggestions");
		addParagraph("Put a full stop at the end of your program, if you forgot to do so.");
		addParagraph("If there is an unfinished sentence at the end of the program then you have to remove this unfinished sentence. Or you simply make a complete sentence out of it.");
	}

}
