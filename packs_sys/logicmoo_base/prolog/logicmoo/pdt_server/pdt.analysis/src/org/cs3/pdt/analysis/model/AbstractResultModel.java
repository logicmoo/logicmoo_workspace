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

public abstract class AbstractResultModel implements IResultModel {
	
	private ArrayList<IResultModelListener> listeners = new ArrayList<>();

	@Override
	public void addListener(IResultModelListener listener) {
		synchronized (listeners) {
			listeners.add(listener);
		}
	}

	@Override
	public void removeListener(IResultModelListener listener) {
		synchronized (listeners) {
			listeners.remove(listener);
		}
	}
	
	@SuppressWarnings("unchecked")
	protected void fireAnalysesUpdated(IFactbase factbase) {
		ArrayList<IResultModelListener> cloned;
		synchronized (listeners) {
			cloned = (ArrayList<IResultModelListener>) listeners.clone();
		}
		for (IResultModelListener listener : cloned) {
			listener.analysesUpdated(factbase);
		}
	}

	@SuppressWarnings("unchecked")
	protected void fireResultsUpdated(IFactbase factbase, List<IAnalysis> analyses) {
		ArrayList<IResultModelListener> cloned;
		synchronized (listeners) {
			cloned = (ArrayList<IResultModelListener>) listeners.clone();
		}
		for (IResultModelListener listener : cloned) {
			listener.resultsUpdated(factbase, analyses);
		}
	}
	
}
