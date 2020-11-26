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


public class ContainsQuery extends Page {
	
	public ContainsQuery(HelpWindow helpWindow) {
		super(helpWindow, "Program must not contain queries", "Errors");
		addText("A program is not allowed to contain queries (i.e. questions), since it does not make sense to put queries into the program. Thus, sentences like");
		addQuote("Who uses a hammer?");
		addText("are not allowed.");
		addGap();
		addHeading("Suggestions");
		addParagraph("Remove all the queries from your program or rephrase them as declarative sentences.");
	}

}
