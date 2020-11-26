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

import ch.uzh.ifi.attempto.acerules.help.HelpButton;
import nextapp.echo2.app.Color;
import nextapp.echo2.app.Column;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.Label;
import nextapp.echo2.app.Row;
import nextapp.echo2.app.SplitPane;
import nextapp.echo2.app.WindowPane;
import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;


public class ErrorMessage extends WindowPane implements ActionListener {
	
	private UserInterface ui;
	private String help;

	public ErrorMessage(String message, WindowPane parent, UserInterface ui, String help) {
		this.ui = ui;
		this.help = help;
		setTitle("Error");
		setModal(true);
		setClosable(false);
		setWidth(new Extent(400));
		setHeight(new Extent(180));
		setResizable(false);
		setMovable(true);
		setTitleBackground(new Color(200, 100, 100));
		setStyleName("Default");
		
		SplitPane splitPane = new SplitPane(SplitPane.ORIENTATION_VERTICAL_BOTTOM_TOP);
		splitPane.setSeparatorPosition(new Extent(45));

		Row buttonBar = new Row();
		buttonBar.setCellSpacing(new Extent(10));
		buttonBar.add(new NormalButton("OK", this));
		buttonBar.add(new HelpButton(this));
		Column column1 = new Column();
		column1.setInsets(new Insets(133, 10));
		column1.add(buttonBar);
		splitPane.add(column1);
		
		Column column2 = new Column();
		column2.setInsets(new Insets(10, 10));
		column2.setCellSpacing(new Extent(5));
		for (String s : message.split("\n")) {
			column2.add(new Label(s));
		}
		splitPane.add(column2);
		
		add(splitPane);
		
		if (parent == null) {
			setPositionX(new Extent(150));
			setPositionY(new Extent(50));
		} else {
			setPositionX(new Extent(parent.getPositionX().getValue() + (parent.getWidth().getValue() - getWidth().getValue())/2));
			setPositionY(new Extent(parent.getPositionY().getValue() + (parent.getHeight().getValue() - getHeight().getValue())/2));
		}
	}
	
	public void actionPerformed(ActionEvent e) {
		if (e.getActionCommand().equals("?")) {
			ui.showHelp(help);
		}
		setVisible(false);
	}

}
