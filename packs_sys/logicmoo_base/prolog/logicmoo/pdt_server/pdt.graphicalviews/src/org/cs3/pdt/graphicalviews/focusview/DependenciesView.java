/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker, Ilshat Aliev
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2013, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.graphicalviews.focusview;

import org.cs3.pdt.graphicalviews.main.PDTGraphView;


public class DependenciesView extends ViewBase {
	
	@Override
	public GraphProcessLoaderBase createGraphProcessLoader(PDTGraphView pdtGraphView) {
		return new DependenciesGraphProcessLoader(pdtGraphView);
	}

	@Override
	protected ViewCoordinatorBase createViewCoordinator() {
		return new ProjectViewCoordinator(this);
	}
}
