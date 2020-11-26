/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2015, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/
package org.cs3.pdt.analysis.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class ResultList {
	
	private String analysisName;
	
	private ArrayList<IResultElement> results = new ArrayList<>();
	
	private HashMap<String, IResultElementGroup> groups = new HashMap<>();

	public ResultList(String analysisName) {
		this.analysisName = analysisName;
	}
	
	public String getAnalysisName() {
		return analysisName;
	}

	public void addResult(IResultElement resultElement) {
		if(!results.contains(resultElement))
			results.add(resultElement);
	}
	
	public List<IResultElement> getResults() {
		return new ArrayList<>(results);
	}
	
	public int getNumberOfResults() {
		return results.size();
	}
	
	public void addResultElementGroup(String identifier, IResultElementGroup group) {
		groups.put(identifier, group);
	}
	
	public IResultElementGroup getResultElementGroup(String identifier) {
		return groups.get(identifier);
	}

	public void clearResults() {
		results.clear();
		groups.clear();
	}
	
}
