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

package ch.uzh.ifi.attempto.acerules.help;

import nextapp.echo2.app.ApplicationInstance;
import nextapp.echo2.app.Button;
import nextapp.echo2.app.Color;
import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;
import nextapp.echo2.webcontainer.command.BrowserRedirectCommand;


public class WebLink extends Button implements ActionListener {

	private String uri;
	
	public WebLink(String text, String uri) {
		super("[" + text + "]");
		this.uri = uri;
		
		setBackground(Color.WHITE);
		setForeground(new Color(150, 150, 150));
		setRolloverEnabled(true);
		setRolloverForeground(new Color(255, 100, 0));
		setBorder(null);
		addActionListener(this);
	}

	public void actionPerformed(ActionEvent e) {
		ApplicationInstance.getActive().enqueueCommand(new BrowserRedirectCommand(uri));
	}

}
