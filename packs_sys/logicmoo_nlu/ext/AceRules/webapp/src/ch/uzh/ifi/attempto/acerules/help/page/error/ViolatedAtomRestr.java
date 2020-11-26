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


public class ViolatedAtomRestr extends Page {
	
	public ViolatedAtomRestr(HelpWindow helpWindow) {
		super(helpWindow, "Violated atom-restriction", "Errors");
		addGap();
		addText("The program violates the atom-restriction. All the complex statements within negation and in the then-part of rules " +
				"are considered atomic. This means that the objects therein do not have an own existence, i.e. they are not accessible " +
				"for references. For example, the following program violates the atom-restriction:");
		addQuote("Every man has a car. Bill has a car. John knows the car.");
		addParagraph("'has a car' is considered atomic because it occurs in the then-part of the first sentence. This means that " +
				"'car' in the second sentence is not allowed to be referenced in the third sentence.");
		addParagraph("Programs that violate the atom-restriction can not be translated into a valid rule structure.");
		addHeading("Suggestions");
		addParagraph("Make sure that the atom-restriction is fulfilled. Remove the rule that introduces the restriction, or remove the " +
				"references to the objects of atomic statements.");
	}

}
