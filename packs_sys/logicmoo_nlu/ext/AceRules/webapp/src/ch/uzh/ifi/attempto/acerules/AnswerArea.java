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

import java.util.ArrayList;
import java.util.List;

import echopointng.TabbedPane;
import echopointng.tabbedpane.DefaultTabModel;
import nextapp.echo2.app.Alignment;
import nextapp.echo2.app.Border;
import nextapp.echo2.app.Color;
import nextapp.echo2.app.Column;
import nextapp.echo2.app.Component;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.Label;
import nextapp.echo2.app.Row;
import nextapp.echo2.app.TextArea;
import nextapp.echo2.app.event.ActionListener;
import nextapp.echo2.app.layout.RowLayoutData;


public class AnswerArea extends Row {
	
	private TabbedPane tabbedPane = new TabbedPane();
	private Column group = new Column();
	
	public AnswerArea(ActionListener actionListener) {
		setInsets(new Insets(10, 10, 0, 0));
		group.setInsets(new Insets(10, 10));
		group.setBorder(new Border(2, new Color(150, 150, 150), Border.STYLE_DOTTED));
		
		Row head = new Row();
		head.setInsets(new Insets(0, 0, 0, 5));
		head.setCellSpacing(new Extent(5));
		Row labelRow = new Row();
		labelRow.setCellSpacing(new Extent(5));
		RowLayoutData headLayoutLeft = new RowLayoutData();
		headLayoutLeft.setAlignment(new Alignment(Alignment.LEFT, Alignment.TOP));
		headLayoutLeft.setWidth(new Extent(558));
		labelRow.setLayoutData(headLayoutLeft);
		Label label = new Label("Answer");
		label.setFont(new Font(Font.ARIAL, Font.BOLD, new Extent(15)));
		labelRow.add(label);
		//Label label2 = new Label("(Derived facts)");
		//label2.setFont(new Font(Font.ARIAL, Font.PLAIN, new Extent(12)));
		//RowLayoutData label2Layout = new RowLayoutData();
		//label2Layout.setAlignment(new Alignment(Alignment.LEFT, Alignment.BOTTOM));
		//label2.setLayoutData(label2Layout);
		//labelRow.add(label2);
		head.add(labelRow);
		SquareButton helpButton = new SquareButton("?", "Answer help", actionListener);
		SquareButton closeButton = new SquareButton("X", "Answer close", actionListener);
		head.add(helpButton);
		head.add(closeButton);
		group.add(head);
		
		group.add(createTextArea());
		
		tabbedPane.setTabPlacement(Alignment.BOTTOM);
		tabbedPane.setBorder(new Border(1, Color.BLACK, Border.STYLE_INSET));
		
		add(group);
	}
	
	public void setWaitStatus() {
		TextArea textArea = createTextArea();
		textArea.setForeground(new Color(150, 150, 150));
		textArea.setAlignment(new Alignment(Alignment.CENTER, Alignment.TOP));
		textArea.setText("please wait...");
		setContent(textArea);
	}
	
	public void addAnswers(List<String> answers) {
		ArrayList<TextArea> textAreas = new ArrayList<TextArea>();
		
		for (String answer : answers) {
			TextArea textArea = createTextArea();
			if (answer.length() == 0) {
				textArea.setForeground(new Color(150, 150, 150));
				textArea.setText("(empty answer)");
			} else {
				textArea.setText(answer);
			}
			textAreas.add(textArea);
		}
		
		if (textAreas.size() == 1) {
			setContent(textAreas.get(0));
		} else {
			DefaultTabModel tabModel = (DefaultTabModel) tabbedPane.getModel();
			while (tabModel.size() > 0) tabModel.removeTabAt(0);
			for (int i = 0; i < textAreas.size(); i++) {
				TextArea textArea = textAreas.get(i);
				textArea.setBorder(new Border(0, Color.BLACK, Border.STYLE_SOLID));
				tabModel.addTab(" " + (i+1) + " ", textArea);
			}
			setContent(tabbedPane);
		}
	}
	
	public void clear() {
		setContent(createTextArea());
	}
	
	private void setContent(Component c) {
		group.remove(group.getComponentCount()-1);
		group.add(c);
	}
	
	public static TextArea createTextArea() {
		TextArea textArea = new TextArea();
		textArea.setWidth(new Extent(600));
		textArea.setHeight(new Extent(200));
		textArea.setInsets(new Insets(5, 5));
		textArea.setEnabled(false);
		textArea.setBorder(new Border(1, Color.BLACK, Border.STYLE_INSET));
		textArea.setFont(new Font(Font.VERDANA, Font.PLAIN, new Extent(12)));
		textArea.setBackground(new Color(230, 230, 230));
		return textArea;
	}

}
