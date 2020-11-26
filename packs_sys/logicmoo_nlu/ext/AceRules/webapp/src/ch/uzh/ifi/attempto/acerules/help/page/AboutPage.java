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


public class AboutPage extends Page {
	
	public AboutPage(HelpWindow helpWindow) {
		super(helpWindow, "About AceRules", "General");
		addBoldText("Version:");
		addParagraph("2008-12-23");
		addBoldText("Developer:");
		addText("Tobias Kuhn");
		addWebLink("Contact", "mailto:tkuhn@ifi.uzh.ch");
		addGap();
		addBoldText("Affiliation:");
		addText("Attempto group, Department of Informatics & Institute of Computational Linguistics, University of Zurich.");
		addWebLink("Homepage", "http://attempto.ifi.uzh.ch");
		addGap();
	}

}
