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

package ch.uzh.ifi.attempto.acerules;

import ch.uzh.ifi.attempto.acerules.style.Styles;
import nextapp.echo2.app.ApplicationInstance;
import nextapp.echo2.app.Window;


public class App extends ApplicationInstance {
	
	public Window init() {
		setStyleSheet(Styles.DEFAULT_STYLE_SHEET);
		Window window = new Window();
		window.setContent(new UserInterface().getContentPane());
		window.setTitle("AceRules");
		return window;
	}

}