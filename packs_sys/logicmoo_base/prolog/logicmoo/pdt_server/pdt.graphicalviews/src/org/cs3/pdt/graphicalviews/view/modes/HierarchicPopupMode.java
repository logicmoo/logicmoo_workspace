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

import javax.swing.AbstractAction;
import javax.swing.JPopupMenu;

import y.base.Node;
import y.view.Graph2D;
import y.view.Graph2DViewActions;
import y.view.PopupMode;

/**
 * provides the context sensitive popup menus
 */
public class HierarchicPopupMode extends PopupMode{

	@Override
	public JPopupMenu getPaperPopup(double x, double y) {
		return addFolderPopupItems(new JPopupMenu(), x, y, null, false);
	}

	@Override
	public JPopupMenu getNodePopup(Node v) {
		Graph2D graph = getGraph2D();
		return addFolderPopupItems(new JPopupMenu(),
				graph.getCenterX(v),
				graph.getCenterY(v),
				v, true);
	}

	@Override
	public JPopupMenu getSelectionPopup(double x, double y) {
		return addFolderPopupItems(new JPopupMenu(), x, y, null, getGraph2D().selectedNodes().ok());
	}

	private JPopupMenu addFolderPopupItems(JPopupMenu pm, double x, double y, Node node, boolean selected)
	{
		AbstractAction action;
		action = new Graph2DViewActions.OpenFoldersAction(view);
		//	       action.setEnabled(node != null && hierarchy.isGroupNode(node));
		pm.add(action);


		return pm;
	}

}


