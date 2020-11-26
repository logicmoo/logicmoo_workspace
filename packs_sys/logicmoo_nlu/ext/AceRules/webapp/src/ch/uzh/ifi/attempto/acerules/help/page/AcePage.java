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


public class AcePage extends Page {
	
	public AcePage(HelpWindow helpWindow) {
		super(helpWindow, "Attempto Controlled English", "Language");
		addText("Attempto Controlled English (ACE) is a controlled natural language. ACE looks like natural English but it is in " +
				"fact completely formal. Every ACE sentence can be translated unambiguously into first-order logic. ACE is intended to " +
				"be a knowledge representation and specification language. It has been developed at the Department of Informatics at " +
				"the University of Zurich.");
		addHelpLink("Fuchs 2006");
		addGap();
		addText("The following webpage contains an introduction into the syntax of ACE.");
		addWebLink("ACE in a nutshell", "http://attempto.ifi.uzh.ch/docs/ace_nutshell.html");
		addGap();
		addText("For more information about ACE, please visit the project homepage:");
		addWebLink("Attempto homepage", "http://attempto.ifi.uzh.ch");
		addGap();
	}

}
