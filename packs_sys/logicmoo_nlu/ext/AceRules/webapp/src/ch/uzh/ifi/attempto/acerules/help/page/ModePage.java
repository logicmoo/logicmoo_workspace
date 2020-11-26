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

import ch.uzh.ifi.attempto.acerules.help.HelpWindow;
import nextapp.echo2.app.Border;
import nextapp.echo2.app.Color;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Grid;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.Label;


public class ModePage extends Page {
	
	public ModePage(HelpWindow helpWindow) {
		super(helpWindow, "Modes", "Modes");
		addParagraph("AceRules contains at the moment three different modes: Courteous (C), Stable (S), and Stable with strong negation (SN).");
		addHelpLink("Courteous mode");
		addHelpLink("Stable mode");
		addHelpLink("Stable with strong negation mode");
		addGap();
		addParagraph("Each of these modes implies a specific semantics. This means that a program is interpreted differently, depending " +
				"on the current mode. The following table gives an overview of the properties of the different modes.");
		Grid table = new Grid(4);
		table.setBorder(new Border(1, Color.BLACK, Border.STYLE_SOLID));
		table.setInsets(new Insets(5));
		table.setColumnWidth(1, new Extent(30));
		table.setColumnWidth(2, new Extent(30));
		table.setColumnWidth(3, new Extent(30));
		addRow(table, "",                    "C", "S",   "SN");
		addRow(table, "Strong negation",     "X", "",    "X");
		addRow(table, "Negation as failure", "X", "X",   "X");
		addRow(table, "Priorities",          "X", "",    "");
		addRow(table, "Cycles allowed",      "",  "X",   "X");
		addRow(table, "Number of answers",   "1", "1-*", "0-*");
		addRow(table, "Trace support",       "X", "",    "");
		add(table);
		addGap();
		addText("The modes C and SN allow two different kinds of negation (called 'strong negation' and 'negation as failure'), whereas " +
				"S has only negation as failure. To learn more about these two kinds of negation, visit the following page.");
		addHelpLink("Two kinds of negation");
		addGap();
		addText("Only mode C allows to use labels and priorities. These priorities are used to resolve potential conflicts.");
		addHelpLink("Labels and priorities");
		addGap();
		addParagraph("Programs in mode C create always exactly one answer. In the other two modes, a program can generate more than one " +
				"answer or - in the case of mode SN - generate no answer at all.");
		addParagraph("The choice of the right mode is an important and difficult decision. In a real world system, this decision " +
				"should not been taken by the end-user but by the system designer.");
		
	}
	
	private static void addRow(Grid table, String s1, String s2, String s3, String s4) {
		table.add(new Label(s1));
		table.add(new Label(s2));
		table.add(new Label(s3));
		table.add(new Label(s4));
	}

}
