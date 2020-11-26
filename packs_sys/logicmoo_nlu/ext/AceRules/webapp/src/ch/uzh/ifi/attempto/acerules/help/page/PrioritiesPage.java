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


public class PrioritiesPage extends Page {
	
	public PrioritiesPage(HelpWindow helpWindow) {
		super(helpWindow, "Labels and priorities", "Language");
		addText("Labels and priorities can be used to define a priority structure that is used for resolving potential " +
				"conflicts. So far, the courteous mode is the only mode that allows priorities.");
		addHelpLink("Courteous mode");
		addGap();
		addParagraph("In order to define a priority structure, we need to assign labels to sentences. A label preceeds the " +
				"respective sentence and is delimited by a colon.");
		addQuote("R1: Every customer is trustworthy. R2: No criminal is trustworthy.");
		addGap();
		addParagraph("Once we have attached labels to some of the sentences, we can define the priorities with the verb 'overrides'.");
		addQuote("R2 overrides R1.");
		addGap();
		addParagraph("A label can be attached to more than one sentence at the same time. The priority structure has to be acyclic. " +
				"A label cannot override itself (directly or indirectly).");
	}

}
