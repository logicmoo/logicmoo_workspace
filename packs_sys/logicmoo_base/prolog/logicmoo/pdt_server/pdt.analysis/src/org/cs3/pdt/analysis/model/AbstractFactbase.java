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

public abstract class AbstractFactbase implements IFactbase {
	
	private String name;

	protected ArrayList<IAnalysisCategory> categories = new ArrayList<>();
	protected ArrayList<IAnalysis> analyses = new ArrayList<>();

	public AbstractFactbase(String name) {
		this.name = name;
	}

	@Override
	public String getName() {
		return name;
	}
	
	@Override
	public List<IAnalysis> getAnalyses() {
		return new ArrayList<>(analyses);
	}

	@Override
	public List<IAnalysisCategory> getCategories() {
		return new ArrayList<>(categories);
	}

	@Override
	public List<IResultElement> getResults() {
		return null;
	}

	@Override
	public int getNumberOfResults() {
		int count = 0;
		for (IAnalysis analysis : getAnalyses()) {
			count += getNumberOfResults(analysis);
		}
		return count;
	}

	@Override
	public int getNumberOfResults(IAnalysisCategory category) {
		int count = 0;
		for (IAnalysis analysis : category.getAnalyses()) {
			count += getNumberOfResults(analysis);
		}
		return count;
	}

}
