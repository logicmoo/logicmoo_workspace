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

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.ui.texteditor.MarkerUtilities;

public class Result implements IResult {

	public static final int RESULT_PRIORITY = 0;
	
	private String analysisName;
	private String description;
	private String severity;
	private IFile file;
	private IResultElement parent;
	
	private IMarker marker;

	private ArrayList<IResultElement> children = new ArrayList<>();

	public Result(String analysisName, String description, String severity, IFile file, IResultElement parent) {
		this.analysisName = analysisName;
		this.description = description;
		this.severity = severity;
		this.file = file;
		this.parent = parent;
	}

	@Override
	public String getAnalysisName() {
		return analysisName;
	}

	@Override
	public String getDescription() {
		return description;
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
		children.add(element);
	}

	@Override
	public IFile getResource() {
		return file;
	}

	@Override
	public int getLine() {
		return MarkerUtilities.getLineNumber(marker);
	}

	@Override
	public String getSeverity() {
		return severity;
	}

	@Override
	public IMarker getMarker() {
		return marker;
	}
	
	public void setMarker(IMarker marker) {
		this.marker = marker;
	}

	@Override
	public int compareTo(IResultElement o) {
		int c = analysisName.compareTo(o.getAnalysisName());
		if (c != 0) {
			return c;
		}
		if (o instanceof IResult) {
			c = file.getName().compareTo(((IResult) o).getResource().getName());
			if (c != 0) {
				return c;
			}
			return getLine() - ((IResult) o).getLine();
		}
		return getPriority() - o.getPriority();
	}

	@Override
	public int getPriority() {
		return RESULT_PRIORITY;
	}

	@Override
	public IResultElement getParent() {
		return parent;
	}

}
