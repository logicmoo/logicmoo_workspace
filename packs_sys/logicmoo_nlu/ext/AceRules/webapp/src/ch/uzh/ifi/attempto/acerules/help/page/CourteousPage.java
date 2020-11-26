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

package ch.uzh.ifi.attempto.acerules.help.page;

import ch.uzh.ifi.attempto.acerules.help.HelpWindow;


public class CourteousPage extends Page {
	
	public CourteousPage(HelpWindow helpWindow) {
		super(helpWindow, "Courteous mode", "Modes");
		addText("The courteous mode builds upon the theory of Courteous Logic Programs.");
		addHelpLink("Grosof 1997");
		addGap();
		addText("Courteous Logic Programs allow two kinds of negation and allow priorities for conflict handling.");
		addHelpLink("Two kinds of negation");
		addHelpLink("Labels and priorities");
		addGap();
		addText("The downside of this mode is the restriction to acyclic programs. This guarantees that every program has exaclty one " +
				"answer.");
		addHelpLink("Cyclic programs");
		addGap();
	}

}
