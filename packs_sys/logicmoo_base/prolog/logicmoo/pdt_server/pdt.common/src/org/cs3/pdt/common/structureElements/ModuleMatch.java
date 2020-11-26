/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.common.structureElements;

import org.eclipse.core.resources.IFile;
import org.eclipse.search.ui.text.Match;

public class ModuleMatch extends Match {

	private String module;
	private IFile file;

	public ModuleMatch(SearchModuleElement element, String module, IFile file, int line) {
		super(element, UNIT_LINE, line, 1);
		this.module = module;
		this.file = file;
	}

	public String getModule() {
		return module;
	}

	public IFile getFile() {
		return file;
	}

}
