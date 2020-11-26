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
import nextapp.echo2.app.Column;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.Label;
import nextapp.echo2.app.Row;
import nextapp.echo2.app.TextArea;
import nextapp.echo2.app.event.ActionListener;
import nextapp.echo2.app.layout.RowLayoutData;


public class ProgramArea extends Row {
	
	private TextArea textArea;
	private Button modeButton;
	
	public ProgramArea(ActionListener actionListener) {
		setInsets(new Insets(10, 10, 0, 0));
		Column group = new Column();
		group.setInsets(new Insets(10, 10));
		group.setBorder(new Border(2, new Color(150, 150, 150), Border.STYLE_DOTTED));
		
		Row head = new Row();
		head.setInsets(new Insets(0, 0, 0, 5));
		Row labelRow = new Row();
		labelRow.setCellSpacing(new Extent(5));
		RowLayoutData labelRowLayout = new RowLayoutData();
		labelRowLayout.setWidth(new Extent(592));
		labelRow.setLayoutData(labelRowLayout);
		Label label = new Label("Program");
		label.setFont(new Font(Font.ARIAL, Font.BOLD, new Extent(15)));
		labelRow.add(label);
		//Label label2 = new Label("(Rules and facts)");
		//label2.setFont(new Font(Font.ARIAL, Font.PLAIN, new Extent(12)));
		//RowLayoutData label2Layout = new RowLayoutData();
		//label2Layout.setAlignment(new Alignment(Alignment.LEFT, Alignment.BOTTOM));
		//label2.setLayoutData(label2Layout);
		//labelRow.add(label2);
		head.add(labelRow);
		SquareButton helpButton = new SquareButton("?", "Program help", actionListener);
		RowLayoutData headLayoutRight = new RowLayoutData();
		headLayoutRight.setAlignment(new Alignment(Alignment.RIGHT, Alignment.DEFAULT));
		helpButton.setLayoutData(headLayoutRight);
		head.add(helpButton);
		group.add(head);
		
		textArea = new TextArea();
		textArea.setWidth(new Extent(600));
		textArea.setHeight(new Extent(200));
		textArea.setFont(new Font(Font.VERDANA, Font.PLAIN, new Extent(12)));
		textArea.setBackground(new Color(230, 230, 255));
		textArea.setBorder(new Border(1, Color.BLACK, Border.STYLE_INSET));
		textArea.setInsets(new Insets(5, 5));
		group.add(textArea);
		
		Row buttonRow = new Row();
		buttonRow.setInsets(new Insets(0, 5, 0, 0));
		buttonRow.setCellSpacing(new Extent(5));
		Row modeRow = new Row();
		RowLayoutData modeRowLayout = new RowLayoutData();
		modeRowLayout.setWidth(new Extent(475));
		modeRow.setLayoutData(modeRowLayout);
		modeButton = new Button();
		modeButton.setActionCommand("Mode help");
		modeButton.setForeground(new Color(150, 150, 150));
		modeButton.addActionListener(actionListener);
		modeRow.add(modeButton);
		buttonRow.add(modeRow);
		SquareButton backButton = new SquareButton("<", "Back in history", actionListener);
		SquareButton forwardButton = new SquareButton(">", "Forward in history", actionListener);
		NormalButton runButton = new NormalButton("Run", actionListener);
		buttonRow.add(backButton);
		buttonRow.add(forwardButton);
		buttonRow.add(runButton);
		group.add(buttonRow);
		add(group);
	}
	
	public String getText() {
		return textArea.getText();
	}
	
	public void setText(String text) {
		textArea.setText(text);
	}
	
	public void setMode(String mode) {
		modeButton.setText(mode);
	}

}
