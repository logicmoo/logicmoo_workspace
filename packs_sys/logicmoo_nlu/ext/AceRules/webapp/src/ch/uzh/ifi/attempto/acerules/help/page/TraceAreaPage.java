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


public class TraceAreaPage extends Page {
	
	public TraceAreaPage(HelpWindow helpWindow) {
		super(helpWindow, "Trace area", "Screen components");
		addText("The trace feature is only supported in courteous mode.");
		addHelpLink("Modes");
		addGap();
		addParagraph("The trace area shows how your program is executed step by step until the final answer is reached. Step 0 shows " +
				"the facts of your program (contradictions are resolved if there are any). No rules are applied for step 0. Then these " +
				"facts are used to apply the rules, and we get step 1. Possible conflicts are resolved. Then the rules are applied a " +
				"second time, the conflicts are resolved, and we get step 2. This goes on until the final answer is reached.");
		addParagraph("To hide the trace area, you can click the 'X' sign on the upper right corner of the trace area. To show or hide " +
				"different output areas, use the menu 'View'.");
	}

}
