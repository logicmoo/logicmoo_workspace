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
package org.cs3.pdt.common.callhierachy;

import static org.cs3.prolog.connector.common.QueryUtils.bT;
import static org.cs3.prolog.connector.common.QueryUtils.quoteAtom;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.common.PDTCommonPredicates;
import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.process.PrologProcess;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.progress.IDeferredWorkbenchAdapter;
import org.eclipse.ui.progress.IElementCollector;

public class DeferredWorkbenchAdapter implements IDeferredWorkbenchAdapter {
	
	private static final ISchedulingRule RULE = new ISchedulingRule() {
		@Override
		public boolean isConflicting(ISchedulingRule rule) {
			return this == rule;
		}
		
		@Override
		public boolean contains(ISchedulingRule rule) {
			return this == rule;
		}
	};
	
	private int mode;

	private IProject scope;

	@Override
	public Object[] getChildren(Object o) {
		return new Object[0];
	}

	@Override
	public ImageDescriptor getImageDescriptor(Object object) {
		return null;
	}

	@Override
	public String getLabel(Object o) {
		return o.toString();
	}

	@Override
	public Object getParent(Object o) {
		if (o instanceof PredicateEdge) {
			return ((PredicateEdge) o).getParent();
		}
		return null;
	}

	@Override
	public void fetchDeferredChildren(Object object, IElementCollector collector, IProgressMonitor monitor) {
		try {
			Predicate source = null;
			if (object instanceof Predicate) {
				source = (Predicate) object;
			} else if (object instanceof PredicateEdge) {
				PredicateEdge edge = (PredicateEdge) object;
				if (edge.getSource().equals(edge.getTarget())) {
					return;
				}
				source = edge.getTarget();
			}
			if (source != null) {
				PrologProcess process = PDTCommonUtil.getActivePrologProcess();
				try {
					String predicateName;
					if (mode == CallHierarchyView.CALLER_MODE) {
						predicateName = PDTCommonPredicates.FIND_CALLER;
					} else {
						predicateName = PDTCommonPredicates.FIND_CALLEE;
					}
					List<Map<String, Object>> results = process.queryAll(bT(predicateName, 
							quoteAtom(source.getModule()),
							quoteAtom(source.getName()),
							source.getArity(),
							scope != null ? QueryUtils.prologFileNameQuoted(scope.getLocation().toFile()) : "_",
							"TargetModule",
							"TargetName",
							"TargetArity",
							"Count",
							"Visibility"));
					ArrayList<PredicateEdge> targets = new ArrayList<>();
					for (Map<String, Object> result : results) {
						try {
							Predicate target = new Predicate(
									(String) result.get("TargetModule"),
									(String) result.get("TargetName"),
									(String) result.get("TargetArity"),
									(String) result.get("Visibility"));
							target.setDeferredWorkbenchAdapter(this);
							int count = Integer.parseInt((String) result.get("Count"));
							PredicateEdge edge = new PredicateEdge(object, source, target, count, this);
							targets.add(edge);
						} catch (Exception e) {
							Debug.report(e);
						}
					}
					Collections.sort(targets);
					collector.add(targets.toArray(), monitor);
				} catch (Exception e) {
					Debug.report(e);
				}
			}
		} finally {
			collector.done();
			monitor.done();

		}
	}

	@Override
	public boolean isContainer() {
		return true;
	}

	@Override
	public ISchedulingRule getRule(Object object) {
		return RULE;
	}

	public void setMode(int mode) {
		this.mode = mode;
	}
	
	public void setScope(IProject scope) {
		this.scope = scope;
	}

}
