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

package ch.uzh.ifi.attempto.acerules.help.page.ref;

import ch.uzh.ifi.attempto.acerules.help.HelpWindow;
import ch.uzh.ifi.attempto.acerules.help.page.Page;


public class Grosof1997 extends Page {
	
	public Grosof1997(HelpWindow helpWindow) {
		super(helpWindow, "Grosof 1997", "References");
		addText("Benjamin N. Grosof.");
		addItalicText("Courteous Logic Programs: Prioritized Conflict Handling for Rules.");
		addText("IBM Research Report RC 20836. Technical report, IBM T.J. Watson Research Center, 1997.");
		addGap();
	}

}
