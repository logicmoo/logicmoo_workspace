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

package org.cs3.pdt.graphicalviews.model.realizer.nodes;

import org.cs3.pdt.graphicalviews.utils.Size;

import y.view.NodeLabel;
import y.view.NodeRealizer;
import y.view.ShapeNodeRealizer;

public class NodeRealizerBase extends ShapeNodeRealizer {

	public NodeRealizerBase() {
		super();
	}
	
	public NodeRealizerBase(NodeRealizer r) {
		super(r);
	}

	public NodeRealizerBase(byte roundRect) {
		super(roundRect);
	}

	public String getInfoText() {
		return "";
	}

	public Size calcLabelSize(String text) {
		NodeLabel l = new NodeLabel();
		l.setText(text);
		l.setModel(NodeLabel.FREE);
		l.bindRealizer(this);
		return new Size((int)l.getWidth(), (int)l.getHeight());
	}
}
