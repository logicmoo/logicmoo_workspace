/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.console.internal.views;

import java.util.Collections;
import java.util.Vector;

import org.cs3.pdt.console.ConsoleModel;
import org.cs3.pdt.console.ConsoleModelEvent;
import org.cs3.pdt.console.ConsoleModelListener;
import org.cs3.pdt.console.PDTConsole;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.eclipse.jface.dialogs.IDialogSettings;


/*
 * 
 * the only allowed modifications of the history are 
 * 	history.add(Obj),
 *  history.clear()
 * 
 * every operation that modifies the history MUST have the following
 * post conditions: 
 *   pointer=history.size(); //after mod.
 *   lastline == null;
 *   
 * 
 * invarianz: 
 * lastline==null 
 *  <-> currently visible linebuffer is not in history
 *  <-> pointer==history.size()
 *
 *  lastline==null or 0<=pointer<history.size()
 *  
 *  0<=pointer<=history.size()
 */
public class ConsoleHistory implements ConsoleModelListener {
	
	private static final String HISTORY_SECTION = "history";
	
	private Vector<String> history = new Vector<String>();
	private int pointer = 0;
	private String lastLine;
	
	private ConsoleModel model;
	private final String key;

	private IDialogSettings section;
	
	public ConsoleHistory(String key) {
		this.key = key;
		loadHistory();
	}

	public void setConsoleModel(ConsoleModel consoleModel) {
		if (model == consoleModel) {
			return;
		}
		if (model != null) {
			model.removeConsoleListener(this);
		}
		this.model = consoleModel;
		if (model != null) {
			model.addConsoleListener(this);
		}
	}

	public ConsoleModel getConsoleModel() {		
		return model;
	}

	public void previous() {		
		if(model==null||history.isEmpty()||pointer<=0){
			return;
		}
		String current = model.getLineBuffer();
		if(lastLine==null){	//eq to pointer==history.size()		
			lastLine=current;			
		}	
		pointer--;
		model.setLineBuffer(history.get(pointer));
	}

	public void next() {
		if(model==null){
			return;
		}
		if(lastLine==null){//eq to pointer==history.size()
			return;
		}
		pointer++;
		if(pointer==history.size()){
			model.setLineBuffer(lastLine);
			lastLine=null;			
		} else {			
			model.setLineBuffer(history.get(pointer));
		}
	}

	public void previousCompletion(String prefix) {
		if(model==null||history.isEmpty()||pointer<=0){
			return;
		}
		String current = model.getLineBuffer();
		if(lastLine==null){	//eq to pointer==history.size()		
			lastLine=current;			
		}
		int newPos = pointer - 1;
		while (newPos >= 0 && !history.get(newPos).startsWith(prefix)) {
			newPos--;
		}
		if (newPos >= 0) {
			pointer = newPos;
			model.setLineBuffer(history.get(pointer));
		}
	}
	
	public void nextCompletion(String prefix) {
		if(model==null||history.isEmpty()||pointer<=0){
			return;
		}
		String current = model.getLineBuffer();
		if(lastLine==null){	//eq to pointer==history.size()		
			lastLine=current;			
		}
		int newPos = pointer + 1;
		while (newPos < history.size() && !history.get(newPos).startsWith(prefix)) {
			newPos++;
		}
		if (newPos < history.size()) {
			pointer = newPos;
			model.setLineBuffer(history.get(pointer));
		}
	}
	
	@Override
	public void onCommit(ConsoleModelEvent e) {		
		lastLine=null;
		String value = e.getCommitText();
	    //ignore commits like ";", " ", "" + pdt_reload(

		if(! ( value.equals(" ")
				|| value.equals("")
				|| value.equals(";")
				|| value.startsWith("pdt_reload:pdt_reload("))
				) {
			history.add(e.getCommitText());
			limitHistory();
			saveHistory();
		}
		pointer = history.size();
	}

	@Override
	public void onModeChange(ConsoleModelEvent e) {	}
	
	@Override
	public void onEditBufferChanged(ConsoleModelEvent e) {	}
	
	@Override
	public void onOutput(ConsoleModelEvent e) {	}

	@Override
	public void afterConnect(ConsoleModelEvent e) {}

	@Override
	public void beforeDisconnect(ConsoleModelEvent e) {}
	
	private void loadHistory() {
		String[] array = getSettings().getArray(key);
		if (array != null) {
			Collections.addAll(history, array);
			pointer = history.size();
		}
	}

	private void saveHistory() {
		getSettings().put(key, history.toArray(new String[history.size()]));
	}
	
	private void limitHistory() {
		while (history.size() > getHistorySize()) {
			history.remove(0);
		}
	}
	
	private int getHistorySize() {
		int size = PrologConsolePlugin.getDefault().getPreferenceStore().getInt(PDTConsole.PREF_CONSOLE_HISTORY_SIZE);
		return (size < 1 ? 1 : size);
	}

	private IDialogSettings getSettings() {
		if (section == null) {
			IDialogSettings dialogSettings = PrologConsolePlugin.getDefault().getDialogSettings();
			section = dialogSettings.getSection(HISTORY_SECTION);
			if (section == null) {
				section = dialogSettings.addNewSection(HISTORY_SECTION);
			}
		}
		return section;
	}

}


