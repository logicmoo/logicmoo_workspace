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


public class ShortcutsPage extends Page {
	
	public ShortcutsPage(HelpWindow helpWindow) {
		super(helpWindow, "Keyboard shortcuts", "General");
		addParagraph("Some commands can be accessed by keyboard shortcuts. Some of the commands have several possible shortcuts, but not all of them might work. This depends on your browser, platform, and configuration.");
		addGap();
		addText("Help index:");
		addQuote("Ctrl-Shift-H");
		addGap();
		addText("Run:");
		addQuote("Ctrl-Shift-R / Ctrl-Shift-U");
		addGap();
		addText("Save:");
		addQuote("Ctrl-Shift-S");
		addGap();
		addText("Saved programs:");
		addQuote("Ctrl-Shift-P");
		addGap();
		addText("Back in history:");
		addQuote("Ctrl-Shift-LeftArrow / Ctrl-Shift-B");
		addGap();
		addText("Forward in history:");
		addQuote("Ctrl-Shift-RightArrow / Ctrl-Shift-F");
		addGap();
	}

}
