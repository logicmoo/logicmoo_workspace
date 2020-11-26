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

import java.util.List;

public interface IFactbase {
	
	String getName();
	
	List<IAnalysis> getAnalyses();
	
	List<IAnalysisCategory> getCategories();
	
	List<IResultElement> getResults();
	
	List<IResultElement> getResults(IAnalysis analysis);
	
	int getNumberOfResults();

	int getNumberOfResults(IAnalysis analysis);
	
	int getNumberOfResults(IAnalysisCategory category);
	
	void clearResults();
	
	void clearResults(IAnalysis analysis);
	
	void clearResults(List<IAnalysis> analyses);
	
}
