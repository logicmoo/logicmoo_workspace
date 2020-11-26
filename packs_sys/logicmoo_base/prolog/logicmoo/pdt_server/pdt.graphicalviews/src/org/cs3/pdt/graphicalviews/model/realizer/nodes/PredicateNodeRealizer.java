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

import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.NODE_SIZE_FIXED;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.NODE_SIZE_FIXED_WIDTH;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.NODE_SIZE_INDIVIDUAL;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.NODE_SIZE_MAXIMUM;
import static org.cs3.pdt.graphicalviews.preferences.PreferenceConstants.NODE_SIZE_MEDIAN;

import java.awt.Color;
import java.awt.Graphics2D;

import org.cs3.pdt.graphicalviews.model.GraphDataHolder;
import org.cs3.pdt.graphicalviews.model.GraphModel;
import org.cs3.pdt.graphicalviews.preferences.PredicateAppearancePreferences;
import org.cs3.pdt.graphicalviews.preferences.PredicateLayoutPreferences;
import org.cs3.pdt.graphicalviews.utils.Size;
import org.eclipse.jface.preference.IPreferenceStore;

import y.base.Node;
import y.geom.YDimension;
import y.view.LineType;
import y.view.NodeLabel;
import y.view.NodeRealizer;
import y.view.ShapeNodeRealizer;


public class PredicateNodeRealizer extends NodeRealizerBase {

	public static final int INITIAL_STATE = 0;
	public static final int TRANSITION_STATE = 1;
	public static final int FINAL_STATE = 2;
	public static final byte CUSTOM_SHAPE = 4;
	protected final double MAX_NODE_HEIGHT = Double.MAX_VALUE;
	protected final double MAX_NODE_WIDTH = Double.MAX_VALUE;
	private int state;
	private GraphModel	 model;

	public PredicateNodeRealizer(GraphModel  model){
		super(ShapeNodeRealizer.ROUND_RECT);
		state = TRANSITION_STATE;
		this.model= model;

		setFillColor(Color.WHITE);
	}

	public PredicateNodeRealizer(NodeRealizer r)
	{
		super(r);
		if(r instanceof PredicateNodeRealizer)
		{
			PredicateNodeRealizer sr = (PredicateNodeRealizer)r;
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
		byte myStyle;
		GraphDataHolder dataHolder = model.getDataHolder();
		
		if (dataHolder.isDynamicNode(getNode())) {
			myStyle = PredicateAppearancePreferences.getDynamicPredicateBorderStyle().getLineStyle();
		} else if (dataHolder.isMetaPred(getNode())) {
//		} else if ("inferred".equals(dataHolder.getMetaPredType(getNode()))) {
			myStyle = LineType.DASHED_2.getLineStyle();
		} else {
			myStyle = PredicateAppearancePreferences.getBorderStyle().getLineStyle();
		}
		
//		if ("inferred".equals(model.getDataHolder().getMetaPredType(getNode()))) {
//			setLabelText(model.getLabelTextForNode(getNode()) + " [inferred]");
//		}
		
		LineType myLineType = LineType.getLineType(1, myStyle);
		setLineType(myLineType);

		if (dataHolder.isMetaPred(getNode())) {
			setShapeType(HEXAGON);
		} else if (dataHolder.isTransparentNode(getNode())) {
			setShapeType(ELLIPSE);
		} else {
			setShapeType(ROUND_RECT);
		}

		if (dataHolder.isExported(getNode())) {
			setFillColor(PredicateAppearancePreferences.getExportedPredicateColor());
		} else {
			setFillColor(PredicateAppearancePreferences.getPredicateColor());
		}

		if (dataHolder.isUnusedLocal(getNode())) {
			setLineColor(PredicateAppearancePreferences.getUnusedPredicateBorderColor());
		} else {
			setLineColor(PredicateAppearancePreferences.getBorderColor());
		}
		
		super.paintNode(gfx);
	}
	
	public void fitContent() {
		
		Size s = calcLabelSize(model.getLabelTextForNode(getNode()));
		
		int width = 0;
		int height = PredicateLayoutPreferences.getNumberOfLines() * s.getHeight() + 20;
		
		IPreferenceStore prefs = PredicateLayoutPreferences.getCurrentPreferences();
		
		if (PredicateLayoutPreferences.getNodeSizePreference().equals(NODE_SIZE_FIXED)) {
			width = prefs.getInt(NODE_SIZE_FIXED_WIDTH);
		}
		else if (PredicateLayoutPreferences.getNodeSizePreference().equals(NODE_SIZE_MAXIMUM)) {
			width = model.getNodesMaxWidth();
		}
		else if (PredicateLayoutPreferences.getNodeSizePreference().equals(NODE_SIZE_MEDIAN)) {
			width = model.getNodesMedianWidth();
		}
		else if (PredicateLayoutPreferences.getNodeSizePreference().equals(NODE_SIZE_INDIVIDUAL)) {
			width = (int)s.getWidth() + 14;
		}
		
		setSize(width, height);
	}
	
	@Override
	public String getInfoText() {
		StringBuilder sb = new StringBuilder(); 
		Node node = getNode();

		GraphDataHolder data = model.getDataHolder();

		if (data.isModule(node)) {
			sb.append("Module: ");
		} else if (data.isFile(node)) {
			sb.append("File: ");
		} else if (data.isPredicate(node)) {
			sb.append("Predicate: ");
		}

		sb.append(getLabelText());
		
		if (data.isExported(node)) {
			sb.append(" [Exported]");
		}

		if (data.isDynamicNode(node)) {
			sb.append(" [Dynamic]");
		}

		if (data.isUnusedLocal(node)) {
			sb.append(" [Unused]");
		}
		
		if ("inferred".equals(data.getMetaPredType(getNode()))) {
			setLabelText(model.getLabelTextForNode(getNode()) + " [Inferred]");
		}
			
		return sb.toString();
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
		return new PredicateNodeRealizer(r);
	}

}


