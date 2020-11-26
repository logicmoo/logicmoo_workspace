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

package org.cs3.pdt.common.structureElements;

import java.util.List;

import org.eclipse.core.resources.IFile;

public class PrologMatch extends AbstractPrologMatch {

	private String module;

	private String visibility;
	private String declOrDef;
	private String name;
	private int arity;
	
	public PrologMatch(SearchMatchElement searchMatchElement, String visibility, String module, String name, int arity, List<String> properties, IFile file, int line, String declOrDef, String signature) {
		super(searchMatchElement, properties, file, line, signature);
		this.declOrDef = declOrDef;
		this.visibility = visibility;
		this.module = module;
		this.name = name;
		this.arity = arity;
	}
	
	public PrologMatch(SearchMatchElement searchMatchElement, String visibility, String module, String name, int arity, List<String> properties, IFile file, int offset, int length, String declOrDef, String signature) {
		super(searchMatchElement, properties, file, offset, length, signature);
		this.declOrDef = declOrDef;
		this.visibility = visibility;
		this.module = module;
		this.name = name;
		this.arity = arity;
	}
	
	public String getModule() {
		return module;
	}
	
	public String getName() {
		return name;
	}
	
	public int getArity() {
		return arity;
	}
	
	public String getDeclOrDef() {
		return declOrDef;
	}
	
	public String getVisibility() {
		return visibility;
	}
	
}

