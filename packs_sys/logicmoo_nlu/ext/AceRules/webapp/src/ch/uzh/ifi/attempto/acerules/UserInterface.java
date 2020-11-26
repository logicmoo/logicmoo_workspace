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

import static echopointng.KeyStrokeListener.*;
import ch.uzh.ifi.attempto.acerules.help.HelpWindow;

import echopointng.KeyStrokeListener;
import nextapp.echo2.app.ApplicationInstance;
import nextapp.echo2.app.Color;
import nextapp.echo2.app.Column;
import nextapp.echo2.app.ContentPane;
import nextapp.echo2.app.Extent;
import nextapp.echo2.app.Insets;
import nextapp.echo2.app.SplitPane;
import nextapp.echo2.app.TaskQueueHandle;
import nextapp.echo2.app.WindowPane;
import nextapp.echo2.app.event.ActionEvent;
import nextapp.echo2.app.event.ActionListener;


public class UserInterface implements ActionListener {
	
	public static final String[] modes = new String[] {"Courteous", "Stable", "Stable with strong negation"};

	private ApplicationInstance application;
	private TaskQueueHandle taskQueue;

	private ContentPane contentPane = new ContentPane();
	private ContentPane mainPane = new ContentPane();
	private ProgramArea programArea;
	private AnswerArea answerArea;
	private TraceArea traceArea;
	private HelpWindow helpWindow;
	private SavedPrograms savedPrograms = new SavedPrograms();
	private MenuBar menuBar;
	private History history = new History();

	public UserInterface() {
		application = ApplicationInstance.getActive();
		taskQueue = application.createTaskQueue();

		KeyStrokeListener keyStrokeListener = new KeyStrokeListener();
		keyStrokeListener.addActionListener(this);
		keyStrokeListener.addKeyCombination(CONTROL_MASK | SHIFT_MASK | VK_R, "Run");
		keyStrokeListener.addKeyCombination(CONTROL_MASK | SHIFT_MASK | VK_U, "Run");
		keyStrokeListener.addKeyCombination(CONTROL_MASK | SHIFT_MASK | VK_LEFT, "Back in history");
		keyStrokeListener.addKeyCombination(CONTROL_MASK | SHIFT_MASK | VK_B, "Back in history");
		keyStrokeListener.addKeyCombination(CONTROL_MASK | SHIFT_MASK | VK_RIGHT, "Forward in history");
		keyStrokeListener.addKeyCombination(CONTROL_MASK | SHIFT_MASK | VK_F, "Forward in history");
		keyStrokeListener.addKeyCombination(CONTROL_MASK | SHIFT_MASK | VK_S, "Save");
		keyStrokeListener.addKeyCombination(CONTROL_MASK | SHIFT_MASK | VK_P, "Saved programs");
		keyStrokeListener.addKeyCombination(CONTROL_MASK | SHIFT_MASK | VK_H, "Help index");
		
		helpWindow = new HelpWindow(this);
		helpWindow.showPage("Introduction");
		
		contentPane.setBackground(Color.WHITE);
		
		SplitPane splitPane = new SplitPane(SplitPane.ORIENTATION_VERTICAL);
		splitPane.setSeparatorPosition(new Extent(30));
		
		menuBar = new MenuBar(this);
		splitPane.add(menuBar);
		
		Column mainColumn = new Column();
		mainColumn.setInsets(new Insets(0, 0, 0, 10));
		programArea = new ProgramArea(this);
		mainColumn.add(programArea);
		answerArea = new AnswerArea(this);
		mainColumn.add(answerArea);
		traceArea = new TraceArea(this);
		traceArea.setVisible(false);
		mainColumn.add(traceArea);
		mainColumn.add(keyStrokeListener);
		mainPane.add(mainColumn);
		
		splitPane.add(mainPane);
		contentPane.add(splitPane);
		
		menuBar.setSelected("Show answer", true);
		menuBar.setSelected("Courteous", true);
		programArea.setMode("Courteous");
	}
	
	public void actionPerformed(ActionEvent e) {
		if (e.getActionCommand().equals("Introduction")) {
			helpWindow.showPage("Introduction");
		} else if (e.getActionCommand().equals("Quick start")) {
			helpWindow.showPage("Quick start");
		} else if (e.getActionCommand().equals("Help index")) {
			helpWindow.showPage("Help index");
		} else if (e.getActionCommand().equals("About")) {
			helpWindow.showPage("About AceRules");
		} else if (e.getActionCommand().equals("Run")) {
			history.add(programArea.getText());
			answerArea.setWaitStatus();
			if (traceArea.isVisible()) {
				traceArea.setWaitStatus();
			} else {
				traceArea.clear();
			}
			final String mode;
			if (menuBar.isSelected("Stable")) {
				mode = "stable";
			} else if (menuBar.isSelected("Stable with strong negation")) {
				mode = "stable_strong";
			} else {
				mode = "court";
			}
			Thread thread = new Thread() {
				public synchronized void run() {
					enqueueTask(new Request(UserInterface.this, programArea.getText(), mode, traceArea.isVisible()));
				}
			};
			thread.start();
		} else if (e.getActionCommand().equals("Clear")) {
			history.add(programArea.getText());
			history.add("");
			programArea.setText("");
		} else if (e.getActionCommand().equals("Load example")) {
			showWindow(new LoadExampleWindow(this));
		} else if (e.getActionCommand().equals("Saved programs")) {
			if (savedPrograms.isEmpty()) {
				showWindow(new ErrorMessage("There are no saved programs.", null, this, "No saved programs"));
			} else {
				showWindow(new LoadProgramWindow(this, savedPrograms));
			}
		} else if (e.getActionCommand().equals("Save")) {
			if (savedPrograms.isFull()) {
				showWindow(new ErrorMessage("Cannot store more than 10 programs.", null, this, "Too many saved programs"));
				return;
			}
			if (programArea.getText().length() > 3950) {
				showWindow(new ErrorMessage("Program is too large (> 4000 characters) to be stored.", null, this, "Program too large to save"));
				return;
			}
			if (programArea.getText().length() == 0) {
				showWindow(new ErrorMessage("Program is empty.", null, this, "Empty program"));
				return;
			}
			if (programArea.getText().length() > savedPrograms.getFreeSpace()) {
				showWindow(new ErrorMessage("No space left to save this program. Altogether 7000 characters can be stored.", null, this, "No space left for programs"));
				return;
			}
			showWindow(new SaveProgramWindow(this, savedPrograms));
		} else if (e.getActionCommand().equals("Courteous")) {
			programArea.setMode("Courteous");
			answerArea.clear();
			traceArea.setActive(true);
		} else if (e.getActionCommand().equals("Stable")) {
			programArea.setMode("Stable");
			answerArea.clear();
			traceArea.setActive(false);
		} else if (e.getActionCommand().equals("Stable with strong negation")) {
			programArea.setMode("Stable with strong negation");
			answerArea.clear();
			traceArea.setActive(false);
		} else if (e.getActionCommand().equals("Program help")) {
			helpWindow.showPage("Program area");
		} else if (e.getActionCommand().equals("Answer help")) {
			helpWindow.showPage("Answer area");
		} else if (e.getActionCommand().equals("Answer close")) {
			answerArea.setVisible(false);
			menuBar.setSelected("Show answer", false);
		} else if (e.getActionCommand().equals("Show answer")) {
			answerArea.setVisible(menuBar.isSelected("Show answer"));
		} else if (e.getActionCommand().equals("Trace help")) {
			helpWindow.showPage("Trace area");
		} else if (e.getActionCommand().equals("Trace close")) {
			traceArea.setVisible(false);
			menuBar.setSelected("Show trace", false);
		} else if (e.getActionCommand().equals("Show trace")) {
			traceArea.setVisible(menuBar.isSelected("Show trace"));
		} else if (e.getActionCommand().equals("Back in history")) {
			programArea.setText(history.back());
		} else if (e.getActionCommand().equals("Forward in history")) {
			programArea.setText(history.forward());
		} else if (e.getActionCommand().equals("Menu bar help")) {
			helpWindow.showPage("Menu bar");
		} else if (e.getActionCommand().equals("Mode help")) {
			helpWindow.showPage("Modes");
		}
	}
	
	public void setProgram(String program) {
		history.add(program);
		programArea.setText(program);
	}
	
	public String getProgram() {
		return programArea.getText();
	}
	
	public AnswerArea getAnswerArea() {
		return answerArea;
	}
	
	public void showOutputArea() {
		if (answerArea.isVisible() || traceArea.isVisible()) return;
		answerArea.setVisible(true);
		menuBar.setSelected("Show answer", true);
	}
	
	public TraceArea getTraceArea() {
		return traceArea;
	}
	
	public ContentPane getContentPane() {
		return contentPane;
	}
	
	public MenuBar getMenuBar() {
		return menuBar;
	}
	
	public String getMode() {
		for (String m : modes) {
			if (menuBar.isSelected(m)) return m;
		}
		return null;
	}
	
	public void showWindow(WindowPane window) {
		mainPane.add(window);
	}
	
	public void showHelp(String helpPageTitle) {
		helpWindow.showPage(helpPageTitle);
	}
	
	public void enqueueTask(Runnable task) {
		application.enqueueTask(taskQueue, task);
	}

}
