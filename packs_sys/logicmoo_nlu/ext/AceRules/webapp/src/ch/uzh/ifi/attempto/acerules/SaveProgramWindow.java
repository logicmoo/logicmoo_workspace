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
import nextapp.echo2.app.Color;
import nextapp.echo2.app.Column;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.Label;
import nextapp.echo2.app.Row;
import nextapp.echo2.app.SplitPane;
import nextapp.echo2.app.TextField;
import nextapp.echo2.app.WindowPane;
import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;
import nextapp.echo2.app.layout.ColumnLayoutData;


public class SaveProgramWindow extends WindowPane implements ActionListener {

	private SavedPrograms savedPrograms;
	private UserInterface ui;
	private TextField textField;

	public SaveProgramWindow(UserInterface ui, SavedPrograms savedPrograms) {
		this.ui = ui;
		this.savedPrograms = savedPrograms;
		setTitle("Save program");
		setModal(true);
		setClosable(false);
		setWidth(new Extent(440));
		setHeight(new Extent(165));
		setResizable(false);
		setMovable(true);
		setTitleBackground(new Color(150, 150, 200));
		setStyleName("Default");
		
		SplitPane splitPane = new SplitPane(SplitPane.ORIENTATION_VERTICAL_BOTTOM_TOP);
		splitPane.setSeparatorPosition(new Extent(35));

		Row buttonBar = new Row();
		buttonBar.setCellSpacing(new Extent(10));
		ColumnLayoutData centerLayout = new ColumnLayoutData();
		centerLayout.setAlignment(new Alignment(Alignment.CENTER, Alignment.CENTER));
		buttonBar.setLayoutData(centerLayout);
		buttonBar.add(new NormalButton("OK", this));
		buttonBar.add(new NormalButton("Cancel", this));
		//buttonBar.add(new HelpButton(this));
		
		Column column1 = new Column();
		column1.add(buttonBar);
		splitPane.add(column1);
		
		Label label = new Label("Enter a name for this program:");
		Column column2 = new Column();
		column2.setInsets(new Insets(10, 10));
		column2.setCellSpacing(new Extent(10));
		column2.add(label);
		
		textField = new TextField();
		textField.setBackground(new Color(230, 230, 255));
		textField.setBorder(new Border(1, Color.BLACK, Border.STYLE_INSET));
		textField.setWidth(new Extent(380));
		column2.add(textField);
		
		splitPane.add(column2);
		
		add(splitPane);
		
		setPositionX(new Extent(150));
		setPositionY(new Extent(50));
	}
	
	public void actionPerformed(ActionEvent e) {
		if (e.getActionCommand().equals("Cancel")) {
			setVisible(false);
		} else if (e.getActionCommand().equals("OK")) {
			if (textField.getText().length() == 0) {
				ui.showWindow(new ErrorMessage("Enter a name.", this, ui, ""));
				return;
			}
			if (textField.getText().length() > 20) {
				ui.showWindow(new ErrorMessage("This name is too long. It may contain at most 20 characters.", this, ui, ""));
				return;
			}
			if (savedPrograms.existsProgram(textField.getText())) {
				ui.showWindow(new ErrorMessage("This name does already exist.", this, ui, ""));
				return;
			}
			savedPrograms.addProgram(textField.getText(), ui.getProgram());
			setVisible(false);
		} else if (e.getActionCommand().equals("?")) {
			ui.showHelp("Not available");
		}
	}

}
