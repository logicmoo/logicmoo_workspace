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


public class CyclicPage extends Page {
	
	public CyclicPage(HelpWindow helpWindow) {
		super(helpWindow, "Cyclic programs", "Language");
		addText("Programs that are executed in courteous mode need to be acyclic. For the other modes, this is not necessary.");
		addHelpLink("Courteous mode");
		addGap();
		addParagraph("A cycle in a program arises if there is a cyclic dependency among some facts. By dependency we mean: a fact " +
				"depends on another fact if there is a rule that can conclude the first (or its negation) from the latter (or its " +
				"negation). For example, the following program is cyclic:");
		addQuote("Every employee is a customer. No customer is an employee. John is an employee.");
		addGap();
		addParagraph("The fact 'John is a customer' depends on 'John is an employee' and vice versa. Thus, the program is cyclic.");
	}

}
