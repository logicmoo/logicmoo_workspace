/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.editor.internal.views.lightweightOutline;

import java.util.Map;

import org.cs3.pdt.editor.internal.queries.PDTOutlineQuery;
import org.cs3.pdt.editor.internal.structureElements.OutlineModuleElement;


public class PrologSourceFileModel {

	private Map<String, OutlineModuleElement> modules;
	private static final String FILE_NOT_LOADED_MESSAGE = "Consult the current file to see its outline";
	private String message = null;
	
	public PrologSourceFileModel(Map<String, OutlineModuleElement> modules) {
		this(modules, null);
	}
	
	public PrologSourceFileModel(Map<String, OutlineModuleElement> modules, String fileName) {
		update(modules, fileName);
	}
	
	public void update(Map<String,OutlineModuleElement> modules, String fileName){
		message = null;
		this.modules = modules;
		if (modules.isEmpty()) {
			if (fileName != null && !PDTOutlineQuery.isFileLoaded(fileName)) {
				message = FILE_NOT_LOADED_MESSAGE;
			}
		} else {
			for (OutlineModuleElement module : modules.values()) {
				module.setParent(this);
			}
		}
	}
	
	public boolean hasChildren() {
		if(((modules == null) || (modules.isEmpty())) && message == null)
			return false;
		return true;
	}

	public Object[] getElements() {
		if (modules.isEmpty() && message != null) {
			return new Object[]{message};
		} else {
			return modules.values().toArray();
		}
	}
	
	public void dispose() {
		modules.clear();
	}
}


