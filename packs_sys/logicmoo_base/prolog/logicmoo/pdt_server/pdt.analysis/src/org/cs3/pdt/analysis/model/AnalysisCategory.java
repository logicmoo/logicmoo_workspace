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

public class AnalysisCategory implements IAnalysisCategory {

	private String name;
	private String description;
	private IFactbase factbase;
	private ArrayList<IAnalysis> analyses = new ArrayList<>();

	public AnalysisCategory(String name, String description, IFactbase factbase) {
		this.name = name;
		this.description = description;
		this.factbase = factbase;
	}

	@Override
	public String getName() {
		return name;
	}

	@Override
	public String getDescription() {
		return description;
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
		if (obj instanceof AnalysisCategory) {
			return name.equals(((AnalysisCategory) obj).name);
		}
		return false;
	}

	@Override
	public int compareTo(IAnalysisCategory o) {
		return getName().compareTo(o.getName());
	}
	
	public void addAnalysis(IAnalysis analysis) {
		if (!analyses.contains(analysis))  // gk: added check to
			analyses.add(analysis);        // avoid duplicate entries
	}

	@Override
	public List<IAnalysis> getAnalyses() {
		return new ArrayList<>(analyses);
	}

}
