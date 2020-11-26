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
package org.cs3.pdt.analysis.model.prolog;

import static org.cs3.prolog.connector.common.QueryUtils.bT;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.analysis.AnalysisPredicates;
import org.cs3.pdt.analysis.AnalysisProperties;
import org.cs3.pdt.analysis.model.AbstractFactbase;
import org.cs3.pdt.analysis.model.Analysis;
import org.cs3.pdt.analysis.model.AnalysisCategory;
import org.cs3.pdt.analysis.model.IAnalysis;
import org.cs3.pdt.analysis.model.IResultElement;
import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.process.PrologProcess;

public class PrologFactbase extends AbstractFactbase {

	private PrologResultModel prologResultModel;

	public PrologFactbase(String name, PrologResultModel prologResultModel) {
		super(name);
		this.prologResultModel = prologResultModel;
		collectAnalyses();
	}

	@Override
	public List<IResultElement> getResults(IAnalysis analysis) {
		return prologResultModel.getResults(analysis);
	}

	@Override
	public int getNumberOfResults(IAnalysis analysis) {
		return prologResultModel.getNumberOfResults(analysis);
	}

	@Override
	public void clearResults() {
		prologResultModel.clearResults(this);
	}

	@Override
	public void clearResults(IAnalysis analysis) {
		prologResultModel.clearResults(this, analysis);
	}

	@Override
	public void clearResults(List<IAnalysis> analyses) {
		prologResultModel.clearResults(this, analyses);
	}

	void collectAnalyses() {
		collectAnalyses(PDTConnectorPlugin.getDefault().getPrologProcessRegistry().getPrologProcess(getName()));
	}
	
	void collectAnalyses(PrologProcess process) {
		analyses.clear();
		categories.clear();
		
		HashMap<String, String> knownCategories = new HashMap<>();
		HashMap<String, AnalysisCategory> usedCategories = new HashMap<>();
		try {
			List<Map<String, Object>> results;
			results = process.queryAll(bT(AnalysisPredicates.ANALYSIS_CATEGORY, "Name", "Description"));
			for (Map<String, Object> result : results) {
				knownCategories.put((String) result.get("Name"), (String) result.get("Description"));
			}
			results = process.queryAll(bT(AnalysisPredicates.ANALYSIS_DEFINITION, "Name", "Severity", "Category", "Description"));
			for (Map<String, Object> result : results) {
				String categoryName = (String) result.get("Category");
				AnalysisCategory category = usedCategories.get(categoryName);
				if (category == null) {
					String categoryDescription = knownCategories.get(categoryName);
					if (categoryDescription == null) {
						categoryDescription = "";
					}
					category = new AnalysisCategory(categoryName, categoryDescription, this);
					usedCategories.put(categoryName, category);
					categories.add(category);
				}
				
				Analysis analysis = new Analysis(
						(String) result.get("Name"),
						category,
						(String) result.get("Description"),
						(String) result.get("Severity"),
						this);
				analyses.add(analysis);
				category.addAnalysis(analysis);
			}
		} catch (Exception e) {
			Debug.report(e);
		}
	}
	
	List<IAnalysis> getEnabledAnalyses() {
		List<IAnalysis> allAnalyses = getAnalyses();
		ArrayList<IAnalysis> enabledAnalyses = new ArrayList<>();
		Set<String> enabledAnalysisNames = AnalysisProperties.getEnabledAnalyses(getName());
		for (IAnalysis analysis : allAnalyses) {
			if (enabledAnalysisNames.contains(analysis.getName())) {
				enabledAnalyses.add(analysis);
			}
		}
		return enabledAnalyses;
	}

}
