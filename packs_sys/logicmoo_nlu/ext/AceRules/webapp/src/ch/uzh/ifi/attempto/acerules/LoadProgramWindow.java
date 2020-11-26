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
import nextapp.echo2.app.Alignment;
import nextapp.echo2.app.Border;
import nextapp.echo2.app.Color;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Grid;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.Label;
import nextapp.echo2.app.ListBox;
import nextapp.echo2.app.Row;
import nextapp.echo2.app.WindowPane;
import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;
import nextapp.echo2.app.layout.GridLayoutData;


public class LoadProgramWindow extends WindowPane implements ActionListener {

	private UserInterface userInterface;
	private SavedPrograms savedPrograms;
	private NormalButton okButton, cancelButton, deleteButton, helpButton;
	private ListBox listBox;

	public LoadProgramWindow(UserInterface userInterface, SavedPrograms savedPrograms) {
		this.userInterface = userInterface;
		this.savedPrograms = savedPrograms;
		
		refresh();
	}
	
	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == okButton) {
			userInterface.setProgram(savedPrograms.getProgram(listBox.getSelectedValue().toString()));
			setVisible(false);
		} else if (e.getSource() == cancelButton) {
			setVisible(false);
		} else if (e.getSource() == deleteButton) {
			savedPrograms.deleteProgram(listBox.getSelectedValue().toString());
			if (savedPrograms.isEmpty()) {
				userInterface.showWindow(new ErrorMessage("There are no saved programs anymore.", this, userInterface, "No saved programs"));
				setVisible(false);
			}
			refresh();
		} else if (e.getSource() == helpButton) {
			userInterface.showHelp("Not available");
		}
	}
	
	private void refresh() {
		removeAll();
		
		setTitle("Saved programs");
		setModal(true);
		setClosable(false);
		setWidth(new Extent(400));
		setHeight(new Extent(235));
		setResizable(false);
		setMovable(true);
		setTitleBackground(new Color(150, 150, 200));
		setStyleName("Default");
		
		Grid grid = new Grid(1);
		grid.setInsets(new Insets(10, 10));
		grid.setColumnWidth(0, new Extent(380));
		grid.setRowHeight(0, new Extent(40));
		
		Label label = new Label("Choose a program to load or delete.");
		GridLayoutData layout = new GridLayoutData();
		layout.setAlignment(new Alignment(Alignment.LEFT, Alignment.TOP));
		label.setLayoutData(layout);
		grid.add(label);
		
		listBox = new ListBox(savedPrograms.getProgramNames());
		listBox.setSelectedIndex(0);
		listBox.setLayoutData(layout);
		listBox.setBackground(new Color(230, 230, 255));
		listBox.setBorder(new Border(1, Color.BLACK, Border.STYLE_INSET));
		listBox.setHeight(new Extent(80));
		grid.add(listBox);

		Row buttonBar = new Row();
		buttonBar.setCellSpacing(new Extent(10));
		okButton = new NormalButton("Load", this);
		deleteButton = new NormalButton("Delete", this);
		cancelButton = new NormalButton("Close", this);
		helpButton = new HelpButton(this);
		helpButton.setWidth(new Extent(20));
		buttonBar.add(okButton);
		buttonBar.add(deleteButton);
		buttonBar.add(cancelButton);
		//buttonBar.add(helpButton);
		GridLayoutData layoutb = new GridLayoutData();
		layoutb.setAlignment(new Alignment(Alignment.CENTER, Alignment.BOTTOM));
		buttonBar.setLayoutData(layoutb);
		grid.add(buttonBar);
		
		add(grid);
		
		setPositionX(new Extent(150));
		setPositionY(new Extent(50));
	}

}
