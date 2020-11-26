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


public class AnswerAreaPage extends Page {
	
	public AnswerAreaPage(HelpWindow helpWindow) {
		super(helpWindow, "Answer area", "Screen components");
		addParagraph("The answer area shows the answer of your program. At the beginning it is empty. After a program has been run, it shows the result.");
		addParagraph("By saying 'answer', we mean the set of all sentences that can be deduced by the program.");
		addParagraph("You cannot directly edit the answer area, since it is only an output component.");
		addParagraph("To hide the answer area, you can click the 'X' sign on the upper right corner of the answer area. To show or hide different output areas, use the menu 'View'.");
	}

}
