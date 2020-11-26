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


public class ModalityNotSupported extends Page {
	
	public ModalityNotSupported(HelpWindow helpWindow) {
		super(helpWindow, "Modality is not supported", "Errors");
		addParagraph("Modal statements like 'John can run' or 'John must wait' are currently not supported. In a restricted form, they will be supported in a future version of AceRules.");
		addHeading("Suggestion");
		addText("Rephrase the respective sentence without using modality. For example, instead of");
		addQuote("John can run.");
		addText("write");
		addQuote("John runs.");
		addGap();
	}

}
