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
import nextapp.echo2.app.event.ActionListener;


public class SquareButton extends Button {
		
	public SquareButton(String text, String actionCommand, ActionListener actionListener) {
		super(text);
		
		setActionCommand(actionCommand);
		setForeground(Color.BLACK);
		setBackground(Color.WHITE);
		setBorder(new Border(1, Color.WHITE, Border.STYLE_SOLID));
		setRolloverEnabled(true);
		setRolloverForeground(Color.WHITE);
		setRolloverBackground(new Color(100, 100, 200));
		setRolloverBorder(new Border(1, Color.BLACK, Border.STYLE_SOLID));
		setWidth(new Extent(18));
		setHeight(new Extent(18));
		setAlignment(new Alignment(Alignment.CENTER, Alignment.CENTER));
		addActionListener(actionListener);
	}

}
