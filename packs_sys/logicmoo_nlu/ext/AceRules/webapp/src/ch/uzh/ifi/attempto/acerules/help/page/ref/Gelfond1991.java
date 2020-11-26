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

package ch.uzh.ifi.attempto.acerules.help.page.ref;

import ch.uzh.ifi.attempto.acerules.help.HelpWindow;
import ch.uzh.ifi.attempto.acerules.help.page.Page;


public class Gelfond1991 extends Page {
	
	public Gelfond1991(HelpWindow helpWindow) {
		super(helpWindow, "Gelfond 1991", "References");
		addText("Michael Gelfond, Vladimir Lifschitz.");
		addItalicText("Classical Negation in Logic Programs and Disjunctive Databases.");
		addText("In New Generation Computing, volume 9, number 3/4, 365-386, 1991.");
		addGap();
	}

}