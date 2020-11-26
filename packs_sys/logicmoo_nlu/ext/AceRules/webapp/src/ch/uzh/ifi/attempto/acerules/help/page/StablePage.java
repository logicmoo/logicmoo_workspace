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


public class StablePage extends Page {
	
	public StablePage(HelpWindow helpWindow) {
		super(helpWindow, "Stable mode", "Modes");
		addText("The stable mode builds upon the stable model semantics.");
		addHelpLink("Gelfond 1988");
		addGap();
		addText("In this mode, only negation as failure is supported, but not strong negation. All occurences of negation are " +
				"interpreted as negation as failure.");
		addHelpLink("Two kinds of negation");
		addGap();
		addParagraph("In stable mode, a program may have more than one answer.");
	}

}
