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

public class Analysis implements IAnalysis {

	private String name;
	private IAnalysisCategory category;
	private String description;
	private String severity;
	private IFactbase factbase;
	
	public Analysis(String name, IAnalysisCategory category, String description, String severity, IFactbase factbase) {
		this.name = name;
		this.category = category;
		this.description = description;
		this.severity = severity;
		this.factbase = factbase;
	}

	@Override
	public String getName() {
		return name;
	}

	@Override
	public IAnalysisCategory getCategory() {
		return category;
	}

	@Override
	public String getDescription() {
		return description;
	}

	@Override
	public String getSeverity() {
		return severity;
	}

	@Override
	public IFactbase getFactbase() {
		return factbase;
	}
	
	@Override
	public int hashCode() {
		return name.hashCode();
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Analysis) {
			return name.equals(((Analysis) obj).name);
		}
		return false;
	}

	@Override
	public int compareTo(IAnalysis o) {
		return getName().compareTo(o.getName());
	}

}
