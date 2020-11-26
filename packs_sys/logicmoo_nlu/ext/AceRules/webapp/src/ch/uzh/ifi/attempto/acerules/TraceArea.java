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

import echopointng.ContainerEx;
import nextapp.echo2.app.Alignment;
import nextapp.echo2.app.Border;
import nextapp.echo2.app.Color;
import nextapp.echo2.app.Column;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.Label;
import nextapp.echo2.app.Row;
import nextapp.echo2.app.event.ActionListener;
import nextapp.echo2.app.layout.ColumnLayoutData;
import nextapp.echo2.app.layout.RowLayoutData;


public class TraceArea extends Row {
	
	private Column content;
	private int step = 0;
	private boolean active = true;
	
	public TraceArea(ActionListener actionListener) {
		setInsets(new Insets(10, 10, 0, 0));
		Column group = new Column();
		group.setInsets(new Insets(10, 10));
		group.setBorder(new Border(2, new Color(150, 150, 150), Border.STYLE_DOTTED));
		
		Row head = new Row();
		head.setInsets(new Insets(0, 0, 0, 5));
		head.setCellSpacing(new Extent(5));
		Row labelR = new Row();
		RowLayoutData headLayoutLeft = new RowLayoutData();
		headLayoutLeft.setAlignment(new Alignment(Alignment.LEFT, Alignment.TOP));
		headLayoutLeft.setWidth(new Extent(558));
		labelR.setLayoutData(headLayoutLeft);
		Label label = new Label("Trace");
		label.setFont(new Font(Font.ARIAL, Font.BOLD, new Extent(15)));
		labelR.add(label);
		head.add(labelR);
		SquareButton helpButton = new SquareButton("?", "Trace help", actionListener);
		SquareButton closeButton = new SquareButton("X", "Trace close", actionListener);
		head.add(helpButton);
		head.add(closeButton);
		group.add(head);
		
		content = new Column();
		content.setCellSpacing(new Extent(5));
		
		ContainerEx scrollArea = new ContainerEx();
		scrollArea.setWidth(new Extent(590));
		scrollArea.setHeight(new Extent(200));
		scrollArea.setInsets(new Insets(10, 5, 10, 10));
		scrollArea.setBorder(new Border(1, new Color(150, 150, 150), Border.STYLE_SOLID));
		scrollArea.add(content);
		group.add(scrollArea);
		
		add(group);
	}
	
	public void setWaitStatus() {
		if (!active) return;
		clear();
		Label label = new Label("please wait...");
		label.setForeground(new Color(150, 150, 150));
		label.setFont(new Font(Font.VERDANA, Font.PLAIN, new Extent(12)));
		ColumnLayoutData layout = new ColumnLayoutData();
		layout.setAlignment(new Alignment(Alignment.CENTER, Alignment.TOP));
		label.setLayoutData(layout);
		content.add(label);
	}
	
	public void clear() {
		if (!active) return;
		content.removeAll();
		step = 0;
	}
	
	public void addStep(String text) {
		if (!active) return;
		content.add(new Label(""));
		Label stepLabel = new Label("Step " + step + ":");
		stepLabel.setForeground(new Color(150, 150, 150));
		stepLabel.setFont(new Font(Font.VERDANA, Font.ITALIC, new Extent(12)));
		content.add(stepLabel);
		Column column = new Column();
		column.setBackground(new Color(230, 230, 230));
		column.setInsets(new Insets(5,5));
		String[] lines = text.split("\n");
		for (String l : lines) {
			Label label = new Label(l);
			label.setBackground(new Color(230, 230, 230));
			label.setLineWrap(true);
			label.setFont(new Font(Font.VERDANA, Font.PLAIN, new Extent(12)));
			column.add(label);
		}
		content.add(column);
		step++;
	}
	
	public boolean isActive() {
		return active;
	}
	
	public void setActive(boolean active) {
		this.active = active;
		content.removeAll();
		step = 0;
		if (!active) {
			Label label = new Label("not available for this mode");
			label.setForeground(new Color(150, 150, 150));
			label.setFont(new Font(Font.VERDANA, Font.PLAIN, new Extent(12)));
			ColumnLayoutData layout = new ColumnLayoutData();
			layout.setAlignment(new Alignment(Alignment.CENTER, Alignment.TOP));
			label.setLayoutData(layout);
			content.add(label);
		}
	}

}
