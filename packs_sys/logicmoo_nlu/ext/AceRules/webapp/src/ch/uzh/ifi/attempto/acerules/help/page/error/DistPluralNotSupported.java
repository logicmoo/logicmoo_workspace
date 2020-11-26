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

package ch.uzh.ifi.attempto.acerules.help.page.error;

import ch.uzh.ifi.attempto.acerules.help.HelpWindow;
import ch.uzh.ifi.attempto.acerules.help.page.Page;


public class DistPluralNotSupported extends Page {
	
	public DistPluralNotSupported(HelpWindow helpWindow) {
		super(helpWindow, "Distributive plural is not supported", "Errors");
		addText("Distributive plural like 'each of 3 men' is not supported. Thus you cannot write sentences like:");
		addQuote("Each of 3 men has a car.");
		addHeading("Suggestions");
		addText("Mention each of the objects or persons explicitly. Instead of the example above, write:");
		addQuote("A man X has a car. A man Y has a car. A man Z has a car.");
		addGap();
		addText("Or use the collective plural instead. Note that this gives a different meaning. A 'normal' plural is always interpreted in a collective way. Instead of the example above, you can write:");
		addQuote("3 men have a car.");
		addGap();
	}

}
