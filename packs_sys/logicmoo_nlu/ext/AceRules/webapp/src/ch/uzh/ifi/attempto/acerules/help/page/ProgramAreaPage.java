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


public class ProgramAreaPage extends Page {
	
	public ProgramAreaPage(HelpWindow helpWindow) {
		super(helpWindow, "Program area", "Screen components");
		addParagraph("The program area contains your current program. You can type directly into the large blue field, or you can load a program using the menu 'Program'.");
		addText("Press the 'Run'-button to execute your program. The answer of your program will be shown in the answer area.");
		addHelpLink("Answer area");
		addGap();
		addParagraph("By clicking '<' and '>' you can navigate through the history of the program area.");
	}

}
