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


public class StableNegPage extends Page {
	
	public StableNegPage(HelpWindow helpWindow) {
		super(helpWindow, "Stable with strong negation mode", "Modes");
		addText("The 'stable with strong negation' mode builds upon the stable model semantics, enriched with strong negation.");
		addHelpLink("Gelfond 1988");
		addHelpLink("Gelfond 1991");
		addGap();
		addText("In this mode, both kinds of negation are allowed: negation as failure and strong negation.");
		addHelpLink("Two kinds of negation");
		addGap();
		addParagraph("In 'stable with strong negation' mode, a program may have more than one answer, or even no answer at all. If a " +
				"program does not have an answer then we can say that this program contains a contradiction (i.e. it is inconsistent).");
	}

}
