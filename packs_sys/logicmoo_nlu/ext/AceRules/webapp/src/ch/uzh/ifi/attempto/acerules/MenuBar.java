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

import nextapp.echo2.app.Border;
import nextapp.echo2.app.Color;
import nextapp.echo2.app.event.ActionListener;
import nextapp.echo2.extras.app.BorderPane;
import nextapp.echo2.extras.app.MenuBarPane;
import nextapp.echo2.extras.app.menu.DefaultMenuModel;
import nextapp.echo2.extras.app.menu.DefaultOptionModel;
import nextapp.echo2.extras.app.menu.DefaultRadioOptionModel;
import nextapp.echo2.extras.app.menu.DefaultToggleOptionModel;
import nextapp.echo2.extras.app.menu.SeparatorModel;


public class MenuBar extends BorderPane {

	private MenuBarPane menuBarPane;
	private DefaultToggleOptionModel showAnswerItem, showTraceItem;
	private DefaultToggleOptionModel guessItem;
	private DefaultRadioOptionModel courtModeItem, stableModeItem, stableStrongModeItem;
	
	public MenuBar(ActionListener actionListener) {
		menuBarPane = new MenuBarPane();
		menuBarPane.addActionListener(actionListener);
		menuBarPane.setBackground(new Color(230, 230, 230));
		menuBarPane.setForeground(Color.BLACK);
		menuBarPane.setSelectionBackground(new Color(100, 100, 200));
		menuBarPane.setBorder(new Border(1, Color.BLACK, Border.STYLE_SOLID));
		
		DefaultMenuModel menuBar = new DefaultMenuModel();
		DefaultMenuModel mainMenu = new DefaultMenuModel("AceRules", "AceRules");
		mainMenu.addItem(new DefaultOptionModel("Introduction", "Introduction", null));
		mainMenu.addItem(new DefaultOptionModel("Quick start", "Quick start", null));
		mainMenu.addItem(new DefaultOptionModel("Help index", "Help index", null));
		mainMenu.addItem(new SeparatorModel());
		mainMenu.addItem(new DefaultOptionModel("About", "About", null));
		menuBar.addItem(mainMenu);
		DefaultMenuModel programMenu = new DefaultMenuModel("Program", "Program");
		programMenu.addItem(new DefaultOptionModel("Run", "Run", null));
		programMenu.addItem(new DefaultOptionModel("Clear", "Clear", null));
		programMenu.addItem(new SeparatorModel());
		programMenu.addItem(guessItem = new DefaultToggleOptionModel("Guess unknown words", "Guess unknown words"));
		programMenu.addItem(new SeparatorModel());
		programMenu.addItem(new DefaultOptionModel("Save", "Save", null));
		programMenu.addItem(new DefaultOptionModel("Saved programs", "Saved programs", null));
		programMenu.addItem(new DefaultOptionModel("Load example", "Load example", null));
		programMenu.addItem(new SeparatorModel());
		programMenu.addItem(new DefaultOptionModel("Back in history", "Back in history", null));
		programMenu.addItem(new DefaultOptionModel("Forward in history", "Forward in history", null));
		menuBar.addItem(programMenu);
		DefaultMenuModel modeMenu = new DefaultMenuModel("Mode", "Mode");
		modeMenu.addItem(courtModeItem = new DefaultRadioOptionModel("Courteous", "mode", "Courteous"));
		modeMenu.addItem(stableModeItem = new DefaultRadioOptionModel("Stable", "mode", "Stable"));
		modeMenu.addItem(stableStrongModeItem = new DefaultRadioOptionModel("Stable with strong negation", "mode", "Stable with strong negation"));
		modeMenu.addItem(new SeparatorModel());
		DefaultMenuModel maxAnswersMenu = new DefaultMenuModel("Maximal number of answers", "Maximal number of answers");
		maxAnswersMenu.addItem(new DefaultRadioOptionModel("1 answer", "maxanswers", "1 answer"));
		maxAnswersMenu.addItem(new DefaultRadioOptionModel("5 answers", "maxanswers", "5 answers"));
		maxAnswersMenu.addItem(new DefaultRadioOptionModel("10 answers", "maxanswers", "10 answers"));
		maxAnswersMenu.addItem(new DefaultRadioOptionModel("15 answers", "maxanswers", "15 answers"));
		modeMenu.addItem(maxAnswersMenu);
		modeMenu.addItem(new SeparatorModel());
		modeMenu.addItem(new DefaultOptionModel("Mode help", "Mode help", null));
		menuBar.addItem(modeMenu);
		DefaultMenuModel viewMenu = new DefaultMenuModel("View", "View");
		viewMenu.addItem(showAnswerItem = new DefaultToggleOptionModel("Show answer", "Show answer"));
		viewMenu.addItem(showTraceItem = new DefaultToggleOptionModel("Show trace", "Show trace"));
		menuBar.addItem(viewMenu);
		menuBar.addItem(new DefaultOptionModel("Menu bar help", "?", null));
		menuBarPane.setModel(menuBar);
		menuBarPane.getStateModel().setSelected("Guess unknown words", false);

		setStyleName("MenuBar");
		menuBarPane.getStateModel().setSelected("10 answers", true);
		add(menuBarPane);
	}
	
	public boolean isSelected(String id) {
		return menuBarPane.getStateModel().isSelected(id);
	}
	
	public void setSelected(String id, boolean selected) {
		// ugly solution, but otherwise the menu-bar does not refresh
		DefaultOptionModel item = null;
		if (id.equals("Show answer")) {
			item = showAnswerItem;
		} else if (id.equals("Show trace")) {
			item = showTraceItem;
		} else if (id.equals("Guess unknown words")) {
			item = guessItem;
		} else if (id.equals("Courteous")) {
			item = courtModeItem;
		} else if (id.equals("Stable")) {
			item = stableModeItem;
		} else if (id.equals("Stable with strong negation")) {
			item = stableStrongModeItem;
		}
		if (menuBarPane.getStateModel().isSelected(id) != selected) {
			menuBarPane.doAction(item);
		}
	}

}
