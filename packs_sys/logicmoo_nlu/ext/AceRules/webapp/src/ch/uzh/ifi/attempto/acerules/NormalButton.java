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

import nextapp.echo2.app.Alignment;
import nextapp.echo2.app.Border;
import nextapp.echo2.app.Button;
import nextapp.echo2.app.Color;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.event.ActionListener;


public class NormalButton extends Button {
		
	public NormalButton(String text, ActionListener actionListener) {
		super(text);
		
		setActionCommand(text);
		setBackground(new Color(200, 200, 255));
		setBorder(new Border(1, Color.BLACK, Border.STYLE_OUTSET));
		setRolloverEnabled(true);
		setRolloverForeground(Color.WHITE);
		setRolloverBackground(new Color(100, 100, 200));
		setRolloverBorder(new Border(1, Color.BLACK, Border.STYLE_SOLID));
		setWidth(new Extent(70));
		setInsets(new Insets(2, 2));
		setAlignment(new Alignment(Alignment.CENTER, Alignment.CENTER));
		addActionListener(actionListener);
	}

}
