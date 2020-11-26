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

package ch.uzh.ifi.attempto.acerules.help.page;

import ch.uzh.ifi.attempto.acerules.help.HelpLink;
import ch.uzh.ifi.attempto.acerules.help.HelpWindow;
import ch.uzh.ifi.attempto.acerules.help.WebLink;
import nextapp.echo2.app.Button;
import nextapp.echo2.app.Color;
import nextapp.echo2.app.Column;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Font;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.Label;


public class Page extends Column {
	
	private String title, category;
	private HelpWindow helpWindow;
	
	
	public Page(HelpWindow helpWindow, String title, String category) {
		this.helpWindow = helpWindow;
		this.title = title;
		this.category = category;
		setCellSpacing(new Extent(0));
		setInsets(new Insets(10, 10));
		addGap();
		if (category.equals("Errors")) {
			addTitle("Error: " + title);
		} else if (category.equals("References")) {
			addTitle("Reference: " + title);
		} else {
			addTitle(title);
		}
	}
	
	public String getTitle() {
		return title;
	}
	
	public String getCategory() {
		return category;
	}
	
	public void addTitle(String text) {
		Label l = new Label(text);
		l.setFont(new Font(Font.ARIAL, Font.BOLD, new Extent(18)));
		add(l);
		addGap();
	}
	
	public void addHeading(String text) {
		Label l = new Label(text);
		l.setFont(new Font(Font.ARIAL, Font.BOLD, new Extent(14)));
		addGap();
		add(l);
		addGap();
	}
	
	public void addBoldText(String text) {
		Label l = new Label(text);
		l.setFont(new Font(Font.ARIAL, Font.BOLD, new Extent(13)));
		add(l);
	}
	
	public void addItalicText(String text) {
		Label l = new Label(text);
		l.setFont(new Font(Font.ARIAL, Font.ITALIC, new Extent(13)));
		add(l);
	}
	
	public void addText(String text) {
		add(new Label(text));
	}
	
	public void addParagraph(String text) {
		addText(text);
		addGap();
	}
	
	public void addHorizontalLine() {
		Button b = new Button();
		b.setBackground(Color.LIGHTGRAY);
		b.setHeight(new Extent(1));
		add(b);
	}
	
	public void addHelpLink(String helpLink) {
		Column c = new Column();
		c.setInsets(new Insets(0, 2, 0, 2));
		c.add(new HelpLink(helpWindow, helpLink));
		add(c);
	}
	
	public void addWebLink(String text, String uri) {
		Column c = new Column();
		c.setInsets(new Insets(0, 3, 0, 3));
		c.add(new WebLink(text, uri));
		add(c);
	}
	
	public void addQuote(String text) {
		Column c = new Column();
		c.setInsets(new Insets(20, 5, 0, 5));
		Label l = new Label(text);
		l.setFont(new Font(Font.ARIAL, Font.ITALIC, new Extent(13)));
		c.add(l);
		add(c);
	}
	
	public void addGap() {
		Column c = new Column();
		c.setInsets(new Insets(0, 10, 0, 0));
		add(c);
	}
	
	public void addBigGap() {
		Column c = new Column();
		c.setInsets(new Insets(0, 20, 0, 0));
		add(c);
	}

}
