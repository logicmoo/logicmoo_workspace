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
import java.util.Arrays;

import ch.uzh.ifi.attempto.acerules.help.HelpButton;

import nextapp.echo2.app.Alignment;
import nextapp.echo2.app.Border;
import nextapp.echo2.app.Color;
import nextapp.echo2.app.Column;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.Label;
import nextapp.echo2.app.ListBox;
import nextapp.echo2.app.Row;
import nextapp.echo2.app.SelectField;
import nextapp.echo2.app.WindowPane;
import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;
import nextapp.echo2.app.layout.ColumnLayoutData;
import nextapp.echo2.app.list.DefaultListModel;


public class LoadExampleWindow extends WindowPane implements ActionListener {
	
	private static ArrayList<Example> examples = new ArrayList<Example>();

	private UserInterface userInterface;
	private NormalButton okButton, cancelButton, helpButton;
	private ListBox listBox;
	private SelectField selectField;

	public LoadExampleWindow(UserInterface userInterface) {
		this.userInterface = userInterface;
		setTitle("Load Example");
		setModal(true);
		setClosable(false);
		setWidth(new Extent(400));
		setHeight(new Extent(280));
		setResizable(false);
		setMovable(true);
		setTitleBackground(new Color(150, 150, 200));
		setStyleName("Default");
		
		Column column = new Column();
		column.setInsets(new Insets(10, 10));
		column.setCellSpacing(new Extent(10));
		
		column.add(new Label("Show examples for mode:"));
		
		selectField = new SelectField(new String[] {"Courteous", "Stable", "Stable with strong negation", "All"});
		selectField.setSelectedIndex(Arrays.asList(UserInterface.modes).indexOf(userInterface.getMode()));
		selectField.setBackground(new Color(230, 230, 255));
		selectField.setBorder(new Border(1, Color.BLACK, Border.STYLE_INSET));
		selectField.addActionListener(this);
		column.add(selectField);
		
		column.add(new Label("Examples:"));
		
		listBox = new ListBox();
		listBox.setBackground(new Color(230, 230, 255));
		listBox.setBorder(new Border(1, Color.BLACK, Border.STYLE_INSET));
		listBox.setHeight(new Extent(80));
		column.add(listBox);
		
		column.add(new Label(""));

		Row buttonBar = new Row();
		buttonBar.setCellSpacing(new Extent(10));
		ColumnLayoutData centerLayout = new ColumnLayoutData();
		centerLayout.setAlignment(new Alignment(Alignment.CENTER, Alignment.CENTER));
		buttonBar.setLayoutData(centerLayout);
		okButton = new NormalButton("OK", this);
		cancelButton = new NormalButton("Cancel", this);
		helpButton = new HelpButton(this);
		buttonBar.add(okButton);
		buttonBar.add(cancelButton);
		//buttonBar.add(helpButton);
		column.add(buttonBar);
		
		add(column);
		
		setPositionX(new Extent(150));
		setPositionY(new Extent(50));
		
		updateExamples();
	}
	
	private void updateExamples() {
		DefaultListModel listModel;
		String mode = selectField.getSelectedItem().toString();
		if (mode.equals("All")) {
			listModel = new DefaultListModel(examples.toArray());
		} else {
			listModel = new DefaultListModel();
			for (Example e : examples) {
				if (e.hasMode(mode)) listModel.add(e);
			}
		}
		listBox.setModel(listModel);
		listBox.setSelectedIndex(0);
	}
	
	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == okButton) {
			userInterface.setProgram(((Example) listBox.getSelectedValue()).content);
			setVisible(false);
		} else if (e.getSource() == cancelButton) {
			setVisible(false);
		} else if (e.getSource() == helpButton) {
			userInterface.showHelp("Not available");
		} else if (e.getSource() == selectField) {
			updateExamples();
		}
	}
	
	static {
		examples.add(new Example(
				"Cascade1",
					"John is a manager.\n" +
					"John is a customer.\n" +
					"Every customer is a buyer.\n" +
					"Every buyer is external.\n" +
					"Every manager is an employee.\n" +
					"Every employee is a worker.\n" +
					"Every worker is a wage-earner.",
				"Courteous", "Stable", "Stable with strong negation"));
		examples.add(new Example(
				"Cascade2",
					"John is a manager.\n" +
					"John is a customer.\n" +
					"Every customer is a buyer.\n" +
					"Every buyer is external.\n" +
					"Every manager is an employee.\n" +
					"Every employee is a worker.\n" +
					"Every worker is a wage-earner.\n" +
					"No wage-earner is a customer.",
				"Courteous"));
		examples.add(new Example(
				"Criminals1",
					"John is a customer.\n" +
					"Mary is a criminal.\n" +
					"Bill is a thief.\n" +
					"If someone X is not a criminal then X is trustworthy.\n" +
					"Every thief is a criminal.",
				"Courteous", "Stable", "Stable with strong negation"));
		examples.add(new Example(
				"Criminals2",
					"If someone X is not provably a criminal then X is not a criminal.\n" +
					"Every thief is a criminal.\n" +
					"Every shoplifter is a thief.\n" +
					"Mary is a thief.\n" +
					"Bill is a shoplifter.\n" +
					"John is a customer.",
				"Stable with strong negation"));
		examples.add(new Example(
				"Criminals3",
					"If someone X is not provably a criminal then X is not a criminal.\n" +
					"Every thief is a criminal.\n" +
					"Every shoplifter is a thief.\n" +
					"No criminal is trustworthy.\n" +
					"Every client is trustworthy.\n" +
					"Mary is a thief.\n" +
					"Bill is a shoplifter.\n" +
					"John is a client.\n" +
					"Sue is a customer.",
		"Stable with strong negation"));
		examples.add(new Example(
				"Family1",
					"If someone X is a relative of someone Y then Y is a relative of X.\n" +
					"If someone X is a child of someone Y then Y is a parent of X.\n" +
					"If someone X is a parent of someone Y then Y is a child of X.\n" +
					"If someone X is a parent of someone Y then X is a relative of Y.\n" +
					"If someone X is a male child of someone Y then X is a son of Y.\n" +
					"If someone X is a female child of someone Y then X is a daughter of Y.\n" +
					"If someone X is a male parent of someone Y then X is the father of Y.\n" +
					"If someone X is a female parent of someone Y then X is the mother of Y.\n" +
					"\n" +
					"John is male.\n" +
					"Mary is female.\n" +
					"Bill is male.\n" +
					"Tom is male.\n" +
					"Bob is male.\n" +
					"Sue is female.\n" +
					"\n" +
					"John is a child of Mary.\n" +
					"Bill is a parent of John.\n" +
					"Sue is a child of Tom.\n" +
					"Bob is a parent of Mary.",
				"Stable", "Stable with strong negation"));
		examples.add(new Example(
				"Family2",
					"If someone X is a relative of someone Y then Y is a relative of X.\n" +
					"If someone X is the husband of someone Y then Y is the wife of X.\n" +
					"If someone X is the wife of someone Y then Y is the husband of X.\n" +
					"If someone X is the husband of someone then X is male.\n" +
					"If someone X is the wife of someone then X is female.\n" +
					"If someone X is the husband of someone Y then X is a relative of X.\n" +
					"If someone X is a parent of someone Y then Y is a child of X.\n" +
					"If someone X is a child of someone Y then Y is a parent of X.\n" +
					"If someone X is a son of someone Y then X is a child of Y.\n" +
					"If someone X is a daughter of someone Y then X is a child of Y.\n" +
					"If someone X is a son of someone then X is male.\n" +
					"If someone X is a daughter of someone then X is female.\n" +
					"If someone X is male and is a child of someone Y then X is a son of Y.\n" +
					"If someone X is female and is a child of someone Y then X is a daughter of Y.\n" +
					"If someone X is a father of someone Y then X is a parent of Y.\n" +
					"If someone X is a mother of someone Y then X is a parent of Y.\n" +
					"If someone X is a father of someone then X is male.\n" +
					"If someone X is a mother of someone then X is female.\n" +
					"If someone X is male and is a parent of someone Y then X is the father of Y.\n" +
					"If someone X is female and is a parent of someone Y then X is the mother of Y.\n" +
					"If someone X is a child of someone Y then X is a relative of Y.\n" +
					"\n" +
					"John is the husband of Mary.\n" +
					"Mary is a parent of Bob.\n" +
					"Mary is female.\n" +
					"Bob is male.\n" +
					"Bob is a child of John.\n" +
					"Sue is a daughter of Bob.\n" +
					"Bob is a father of Bill.",
				"Stable", "Stable with strong negation"));
		examples.add(new Example(
				"Mail1",
					"Junk-Rule: If a message is from a retailer then the message is not important.\n" +
					"Delivery-Rule: If a message is from someone from who Karen awaits a delivery then the message is important.\n" +
					"Delivery-Rule overrides Junk-Rule.\n" +
					"Karen awaits a delivery from ParisCo.\n" +
					"FaveCo is a retailer.\n" +
					"BabyCo is a retailer.\n" +
					"ParisCo is a retailer.\n" +
					"FaveCo-Rule: If a message is from FaveCo then the message is important.\n" +
					"FaveCo-Rule overrides Junk-Rule.\n" +
					"Message110 is a message and is from ParisCo.\n" +
					"Message116 is a message and is from FaveCo.\n" +
					"Message211 is a message and is from BabyCo.",
				"Courteous"));
		examples.add(new Example(
				"Mail2",
					"Family-Rule: If a message is from someone who belongs-to the family of Fred then the message is important.\n" +
					"Daisy-Rule: If a message is from Daisy then the message is not important.\n" +
					"Emergency-Rule: If a message is a notification of an emergency then the message is important.\n" +
					"Daisy-Rule overrides Family-Rule.\n" +
					"Emergency-Rule overrides Daisy-Rule.\n" +
					"Emergency-Rule overrides Family-Rule.\n" +
					"If there is an illness of someone who belongs-to the family of Fred then the illness is an emergency.\n" +
					"Betty belongs-to the family of Fred.\n" +
					"Daisy belongs-to the family of Fred.\n" +
					"Item19 is a message that is from Betty.\n" +
					"Item20 is a message that is from Daisy.\n" +
					"Item115 is a message that is from Daisy.\n" +
					"Item115 is a message that is a notification of an illness of Daisy.",
				"Courteous"));
		examples.add(new Example(
				"Mollusks",
					"Every cephalopod is a mollusk.\n" +
					"Every nautilus is a cephalopod.\n" +
					"Mollusk-Rule: Every mollusk is a shellbearer.\n" +
					"Cephalopond-Rule: No cephalopod is a shellbearer.\n" +
					"Nautilus-Rule: Every nautilus is a shellbearer.\n" +
					"Molly is a mollusk.\n" +
					"Sophie is a cephalopod.\n" +
					"Natalie is a nautilus.\n" +
					"Nautilus-Rule overrides Cephalopond-Rule.\n" +
					"Cephalopond-Rule overrides Mollusk-Rule.\n" +
					"Nautilus-Rule overrides Mollusk-Rule.",
				"Courteous"));
		examples.add(new Example(
				"Multiple",
					"If someone X is not provably a customer then X is an employee.\n" +
					"If someone X is not provably an employee then X is a customer.\n" +
					"Mary waits.\n" +
					"John sleeps.\n" +
					"Bill is a customer.",
				"Stable", "Stable with strong negation"));
		examples.add(new Example(
				"Nixon",
					"Quaker-Rule: Every quaker is a pacifist.\n" +
					"Republican-Rule: No republican is a pacifist.\n" +
					"Nixon is a quaker.\n" +
					"Nixon is a republican.\n" +
					"Republican-Rule overrides Quaker-Rule.",
				"Courteous"));
		examples.add(new Example(
				"Platypus",
					"R1: Every monotreme is a mammal.\n" +
					"R2: Everything that has some fur is a mammal.\n" +
					"R3: Nothing that lays some eggs is a mammal.\n" +
					"R4: Nothing that has a bill is a mammal.\n" +
					"R1 overrides R3.\n" +
					"R2 overrides R4.\n" +
					"Every platypus is a monotreme.\n" +
					"Every platypus has some fur.\n" +
					"Every platypus lays some eggs.\n" +
					"Every platypus has a bill.\n" +
					"John is a platypus.",
				"Courteous"));
		examples.add(new Example(
				"Simple",
					"Every person has a bike.\n" +
					"If a person has a house then he/she has a car.\n" +
					"If a person has a bike then he/she does not have a dog.\n" +
					"If a person does not have a dog and has a car then he/she does not have a cat.\n" +
					"John is a person and has a house.\n" +
					"Mary is a person and has a cat.",
				"Courteous", "Stable with strong negation"));
		examples.add(new Example(
				"Transitive",
					"If someone X is more important than someone that is more important than someone Y then X is more important than Y.\n" +
					"John is more important than Mary.\n" +
					"Mary is more important than Bill.\n" +
					"Bill is more important than Sue.",
				"Courteous", "Stable", "Stable with strong negation"));
	}

	private static class Example {
		String name;
		String[] modes;
		String content;
		
		Example(String name, String content, String... modes) {
			this.name = name;
			this.content = content;
			this.modes = modes;
		}
		
		boolean hasMode(String mode) {
			return Arrays.asList(modes).contains(mode);
		}
		
		public String toString() {
			return name;
		}
	}

}
