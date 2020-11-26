/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2014, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/
package org.cs3.pdt.analysis.model;

import java.util.ArrayList;
import java.util.List;

public class ResultGroup implements IResultElementGroup {
	
	public static final int RESULT_GROUP_PRIORITY = 16;
	
	private String analysisName;
	private String identifier;
	private String description;
	private IResultElement parent;
	private ArrayList<IResultElement> children = new ArrayList<>();

	public ResultGroup(String analysisName, String identifier, String description, IResultElement parent) {
		this.analysisName = analysisName;
		this.identifier = identifier;
		this.description = description;
		this.parent = parent;
	}

	@Override
	public String getAnalysisName() {
		return analysisName;
	}
	
	@Override
	public String getIdentifier() {
		return identifier;
	}

	@Override
	public String getDescription() {
		return description;
	}

	@Override
	public IResultElement getParent() {
		return parent;
	}

	@Override
	public List<IResultElement> getChildren() {
		return new ArrayList<>(children);
	}

	@Override
	public boolean hasChildren() {
		return !children.isEmpty();
	}
	
	public void addChild(IResultElement element) {
		if(!children.contains(element))
			children.add(element);
	}

	@Override
	public int compareTo(IResultElement o) {
		int c = analysisName.compareTo(o.getAnalysisName());
		if (c != 0) {
			return c;
		}
		if (o instanceof IResultElementGroup) {
			return identifier.compareTo(((IResultElementGroup) o).getIdentifier());
		}
		return getPriority() - o.getPriority();
	}

	@Override
	public int getPriority() {
		return RESULT_GROUP_PRIORITY;
	}

}
