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


public class Gelfond1988 extends Page {
	
	public Gelfond1988(HelpWindow helpWindow) {
		super(helpWindow, "Gelfond 1988", "References");
		addText("Michael Gelfond, Vladimir Lifschitz.");
		addItalicText("The Stable Model Semantics for Logic Programming.");
		addText("In proceedings of the Fifth International Conference on Logic Programming, 1070-1080, The MIT Press, 1988.");
		addGap();
	}

}