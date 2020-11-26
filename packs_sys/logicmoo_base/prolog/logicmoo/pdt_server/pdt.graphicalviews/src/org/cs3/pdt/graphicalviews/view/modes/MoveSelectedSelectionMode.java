/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.graphicalviews.view.modes;

import y.base.YCursor;
import y.layout.Layouter;
import y.layout.router.OrthogonalEdgeRouter;
import y.view.Graph2D;
import y.view.MoveSelectionMode;

/**
* A special mode for moving a selection of the graph.
*/
public class MoveSelectedSelectionMode extends MoveSelectionMode {
	// The rerouting of the edges should only be calculated after the move for performance issues
	private static final boolean ROUTE_EDGES_ON_MOVE = false;
	private Layouter router = new OrthogonalEdgeRouter();
	
	public MoveSelectedSelectionMode() {
		super();
	}
	
	public MoveSelectedSelectionMode(Layouter router) {
		this();
		this.router = router;
	}


	@Override
	protected void selectionOnMove(double dx, double dy, double x, double y) {
		if (ROUTE_EDGES_ON_MOVE) {
			routeEdgesToSelection();
			super.selectionOnMove(dx, dy, x, y);
		}
	}

	@Override
	protected void selectionMovedAction(double dx, double dy, double x, double y) {
		routeEdgesToSelection();
		super.selectionMovedAction(dx, dy, x, y);
	}

	private void routeEdgesToSelection() {
		final Graph2D graph = view.getGraph2D();
		YCursor cursor= graph.selectedNodes();
		
		if (cursor.ok()) {
			
			router.doLayout(graph);
			graph.updateViews();
		}
	}
}


