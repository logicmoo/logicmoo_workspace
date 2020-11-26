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

import java.util.ArrayList;

import ch.uzh.ifi.attempto.acerules.UserInterface;
import ch.uzh.ifi.attempto.acerules.help.page.Page;

import nextapp.echo2.app.Button;
import nextapp.echo2.app.Color;
import nextapp.echo2.app.ContentPane;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.Row;
import nextapp.echo2.app.SplitPane;
import nextapp.echo2.app.WindowPane;
import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;


public class HelpWindow extends WindowPane implements ActionListener {

	private PageManager pageManager;
	private UserInterface ui;
	private ArrayList<Page> history = new ArrayList<Page>();
	private int historyPos;
	private ContentPane contentPane = new ContentPane();

	public HelpWindow(UserInterface ui) {
		this.ui = ui;
		pageManager = new PageManager(this);
		setTitle("Help");
		setWidth(new Extent(400));
		setHeight(new Extent(550));
		setPositionX(new Extent(680));
		setPositionY(new Extent(15));
		setMinimumWidth(new Extent(300));
		setMinimumHeight(new Extent(200));
		setTitleBackground(new Color(150, 150, 200));
		setStyleName("Default");
		
		SplitPane splitPane = new SplitPane(SplitPane.ORIENTATION_VERTICAL);
		splitPane.setSeparatorPosition(new Extent(28));
		splitPane.setSeparatorColor(Color.BLACK);
		splitPane.setSeparatorHeight(new Extent(1));
		
		Row buttonRow = new Row();
		buttonRow.setInsets(new Insets(10, 5));
		buttonRow.setCellSpacing(new Extent(15));

		Button backButton = new Button("<Back");
		backButton.setActionCommand("Back");
		backButton.setRolloverEnabled(true);
		backButton.setRolloverForeground(Color.WHITE);
		backButton.addActionListener(this);
		buttonRow.add(backButton);
		Button forwardButton = new Button("Forward>");
		forwardButton.setActionCommand("Forward");
		forwardButton.setRolloverEnabled(true);
		forwardButton.setRolloverForeground(Color.WHITE);
		forwardButton.addActionListener(this);
		buttonRow.add(forwardButton);
		Button indexButton = new Button("Index");
		indexButton.setActionCommand("Index");
		backButton.setActionCommand("Back");
		indexButton.setRolloverEnabled(true);
		indexButton.setRolloverForeground(Color.WHITE);
		indexButton.addActionListener(this);
		buttonRow.add(indexButton);
		
		ContentPane buttonPane = new ContentPane();
		buttonPane.setBackground(new Color(230, 230, 230));
		buttonPane.add(buttonRow);
		splitPane.add(buttonPane);
		
		splitPane.add(contentPane);
		
		add(splitPane);
		
		history.add(pageManager.getIndexPage());
		historyPos = 0;
	}
	
	public void showPage(String pageName) {
		showPage(pageManager.getPage(pageName));
	}
	
	public void showPage(Page newPage) {
		if (history.get(historyPos) != newPage) {
			contentPane.removeAll();
			contentPane.add(newPage);
			history = new ArrayList<Page>(history.subList(0, historyPos+1));
			history.add(newPage);
			historyPos = history.size()-1;
		}
		ui.showWindow(this);
	}
	
	public void back() {
		if (historyPos > 0) {
			historyPos--;
		}
		contentPane.removeAll();
		contentPane.add(history.get(historyPos));
	}
	
	public void forward() {
		if (historyPos < history.size()-1) {
			historyPos++;
		}
		contentPane.removeAll();
		contentPane.add(history.get(historyPos));
	}
	
	public void actionPerformed(ActionEvent e) {
		if (e.getActionCommand().equals("Index")) {
			showPage(pageManager.getIndexPage());
		} else if (e.getActionCommand().equals("Back")) {
			back();
		} else if (e.getActionCommand().equals("Forward")) {
			forward();
		}
	}

}
