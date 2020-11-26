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

package org.cs3.pdt.graphicalviews.main;

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.net.URL;

import javax.swing.JPanel;

import org.cs3.pdt.graphicalviews.focusview.CallGraphViewBase;
import org.cs3.pdt.graphicalviews.focusview.ViewBase;
import org.cs3.pdt.graphicalviews.graphml.GraphMLReader;
import org.cs3.pdt.graphicalviews.model.GraphDataHolder;
import org.cs3.pdt.graphicalviews.model.GraphLayout;
import org.cs3.pdt.graphicalviews.model.GraphModel;
import org.cs3.pdt.graphicalviews.view.modes.MoveSelectedSelectionMode;
import org.cs3.pdt.graphicalviews.view.modes.ToggleOpenClosedStateViewMode;
import org.cs3.pdt.graphicalviews.view.modes.WheelScroller;
import org.cs3.prolog.connector.common.Util;

import y.base.Node;
import y.layout.router.OrthogonalEdgeRouter;
import y.view.EditMode;
import y.view.Graph2D;
import y.view.Graph2DView;
import y.view.Graph2DViewMouseWheelZoomListener;
import y.view.NavigationMode;
import y.view.ViewMode;

public class PDTGraphView extends  JPanel {
	final ViewBase focusView;
	final Graph2DView view;
	GraphModel graphModel;
	Graph2D graph;
	GraphMLReader reader;

	GraphLayout layoutModel;
	
	EditMode editMode;
	NavigationMode navigationMode;
	Graph2DViewMouseWheelZoomListener wheelZoomListener;
	WheelScroller wheelScroller;

	boolean navigation = false;

	private static final long serialVersionUID = -611433500513523511L;

	public PDTGraphView(ViewBase focusView)
	{
		setLayout(new BorderLayout());
		
		this.focusView = focusView;
		
		layoutModel = new  GraphLayout();

		reader = new GraphMLReader();
		view = new Graph2DView();

		initEditMode();
		
		initNavigationMode();
		
		initMouseZoomSupport();

		initKeyListener();
		
		recalculateMode();
		
		add(view);
	}

	private void  initEditMode() {
		editMode = new EditMode();
		editMode.allowNodeCreation(false);
		editMode.allowEdgeCreation(false);
		//editMode.setPopupMode(new HierarchicPopupMode());
		editMode.setMoveSelectionMode(new MoveSelectedSelectionMode(new OrthogonalEdgeRouter()));
		
		view.addViewMode(editMode);
		view.addViewMode(new ToggleOpenClosedStateViewMode());
		
	}

	protected void initNavigationMode() {
		navigationMode = new NavigationMode();
		navigationMode.setDefaultCursor(new Cursor(Cursor.MOVE_CURSOR));
		navigationMode.setNavigationCursor(new Cursor(Cursor.MOVE_CURSOR));
	}

	private void initMouseZoomSupport() {
		wheelZoomListener = new Graph2DViewMouseWheelZoomListener();
		wheelScroller = new WheelScroller(view);
		
		view.getCanvasComponent().addMouseWheelListener(wheelScroller);
	}
	
	private void initKeyListener() {
		view.getCanvasComponent().addKeyListener(new KeyListener() {
			
			
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.getKeyCode() == KeyEvent.VK_CONTROL) {
					navigation = calculateMode(navigation, true, focusView.isNavigationModeEnabled());
				}
			}

			@Override
			public void keyReleased(KeyEvent e) {
				if (e.getKeyCode() == KeyEvent.VK_CONTROL) {
					navigation = calculateMode(navigation, false, focusView.isNavigationModeEnabled());
				}
			}

			@Override
			public void keyTyped(KeyEvent arg0) { }
		});
	}
	
	public GraphModel getGraphModel() {
		return graphModel;
	}
	
	public void recalculateMode() {
		navigation = calculateMode(navigation, false, focusView.isNavigationModeEnabled());
	}
	
	private boolean calculateMode(boolean isEditorInNavigation, boolean isCtrlPressed, boolean isNavigationModeEnabled) {
		
		boolean setNavitaionMode = isCtrlPressed ^ isNavigationModeEnabled;
		
		// If mode was not changed
		if (setNavitaionMode == isEditorInNavigation) {
			return setNavitaionMode;
		}
		
		if (setNavitaionMode) { 
			// Navigation mode
			view.removeViewMode(editMode);
			view.addViewMode(navigationMode);
			
			view.getCanvasComponent().removeMouseWheelListener(wheelScroller);
			view.getCanvasComponent().addMouseWheelListener(wheelZoomListener);
		}
		else { 
			// Edit mode
			view.removeViewMode(navigationMode);
			view.addViewMode(editMode);
			
			view.getCanvasComponent().removeMouseWheelListener(wheelZoomListener);
			view.getCanvasComponent().addMouseWheelListener(wheelScroller);
		}
		
		return setNavitaionMode;
	}

	public GraphDataHolder getDataHolder() {
		return graphModel.getDataHolder();
	}
	
	public Graph2D getGraph2D() {
		return graph;
	}

	public void addViewMode(ViewMode viewMode){
		view.addViewMode(viewMode);
	}

	public void setModel(GraphModel model){
		this.graphModel = model;
	}

	public void loadGraph(URL resource) {
		loadGraph(reader.readFile(resource));
	}
	
	public void loadGraph(GraphModel model) {
		graphModel = model;
		if (focusView instanceof CallGraphViewBase)
		{
			CallGraphViewBase callGraphView = (CallGraphViewBase)focusView;
			graphModel.setMetapredicateCallsVisisble(callGraphView.isMetapredicateCallsVisible());
			graphModel.setInferredCallsVisible(callGraphView.isInferredCallsVisible());
		}
		graphModel.categorizeData();
		graphModel.assignPortsToEdges();
		graph = graphModel.getGraph();
		view.setGraph2D(graph);

		updateView();
	}
	
	protected void updateView() {
		for (Node node: graph.getNodeArray()) {
			String labelText = graphModel.getLabelTextForNode(node);
			graph.setLabelText(node,labelText);
		}
		
		calcLayout();
	}

	public boolean isEmpty() {
		return graph == null 
			|| graph.getNodeArray().length == 0;
	}

	public void calcLayout() {
		view.applyLayout(layoutModel.getLayouter());
		
		//		layoutModel.getLayouter().doLayout(graph);
		//		graph.updateViews();
		
		view.fitContent();
		view.updateView();
	}
	
	public void updateLayout() {
		if (Util.isMacOS()) {
			view.applyLayout(layoutModel.getLayouter());
		} else {
			view.applyLayoutAnimated(layoutModel.getLayouter());
		}
		view.fitContent();
		view.updateView();
	}
}


