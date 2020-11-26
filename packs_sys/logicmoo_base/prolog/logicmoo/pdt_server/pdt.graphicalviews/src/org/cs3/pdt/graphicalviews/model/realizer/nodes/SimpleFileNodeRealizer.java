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

package org.cs3.pdt.graphicalviews.model.realizer.nodes;

import java.awt.Color;
import java.awt.Graphics2D;

import org.cs3.pdt.graphicalviews.model.GraphModel;
import org.cs3.pdt.graphicalviews.preferences.PredicateLayoutPreferences;

import y.geom.YDimension;
import y.view.NodeLabel;
import y.view.NodeRealizer;
import y.view.ShapeNodeRealizer;


public class SimpleFileNodeRealizer extends NodeRealizerBase {

	public static final int INITIAL_STATE = 0;
	public static final int TRANSITION_STATE = 1;
	public static final int FINAL_STATE = 2;
	public static final byte CUSTOM_SHAPE = 4;
	protected final double MAX_NODE_HEIGHT = Double.MAX_VALUE;
	protected final double MAX_NODE_WIDTH = Double.MAX_VALUE;
	private int state;
	private GraphModel	 model;

	public SimpleFileNodeRealizer(GraphModel  model){
		super(ShapeNodeRealizer.ROUND_RECT);
		state = TRANSITION_STATE;
		this.model= model;

		setFillColor(Color.WHITE);
	}

	public SimpleFileNodeRealizer(NodeRealizer r)
	{
		super(r);
		if(r instanceof SimpleFileNodeRealizer)
		{
			SimpleFileNodeRealizer sr = (SimpleFileNodeRealizer)r;
			state = sr.state;
			model	= sr.model;
		}
		else
		{
			state = FINAL_STATE;
		}
		
		init();
	}

	protected void init() {
		
		NodeLabel label = getLabel();
		
		label.setConfiguration(PredicateLayoutPreferences.getNameCroppingConfiguration());
		label.setAutoSizePolicy(NodeLabel.AUTOSIZE_NODE_SIZE);
		
		label.setUserData(new YDimension(getWidth(), getHeight()));
	}
	
	@Override
	protected void paintNode(Graphics2D gfx) {

		if (model.getDataHolder().isTopFile(getNode())) {
			setFillColor(Color.GREEN);
		} else if(model.getDataHolder().isBottomFile(getNode())) {
			setFillColor(Color.ORANGE);
		} else {
			setFillColor(Color.YELLOW);
		}
		
		super.paintNode(gfx);
	}

	public int getState() {
		return state;
	}

	public void setState(int initialState) {
		state  = initialState;
	}

	@Override
	protected void labelBoundsChanged(NodeLabel arg0) {
		getLabel().setUserData(new YDimension(getWidth(), getHeight()));
		
		super.labelBoundsChanged(arg0);
	}

	@Override
	public NodeRealizer createCopy(NodeRealizer r)
	{
		return new SimpleFileNodeRealizer(r);
	}

}


