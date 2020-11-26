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

package org.cs3.pdt.graphicalviews.model;

import org.cs3.pdt.graphicalviews.main.PluginActivator;
import org.cs3.pdt.graphicalviews.main.PreferencesUpdateListener;
import org.cs3.pdt.graphicalviews.preferences.MainPreferencePage;
import org.cs3.pdt.graphicalviews.preferences.PredicateLayoutPreferences;
import org.cs3.pdt.graphicalviews.preferences.PreferenceConstants;

import y.layout.CompositeLayoutStage;
import y.layout.LayoutOrientation;
import y.layout.LayoutStage;
import y.layout.Layouter;
import y.layout.OrientationLayouter;
import y.layout.hierarchic.IncrementalHierarchicLayouter;
import y.layout.hierarchic.incremental.DrawingDistanceCalculator;
import y.layout.hierarchic.incremental.HierarchicLayouter;
import y.layout.hierarchic.incremental.SimplexNodePlacer;
import y.layout.organic.OutputRestriction;
import y.layout.organic.SmartOrganicLayouter;
import y.layout.router.OrthogonalEdgeRouter;

public class GraphLayout {

	private Layouter hierarhicLayouter;
	private Layouter organicLayouter;
	private LayoutStage edgeLayouter; 

	
	public GraphLayout() {
		
		initialize();
		
		PluginActivator.getDefault().addPreferencesUpdateListener(new PreferencesUpdateListener() {
			public void preferencesUpdated() {
				initialize();
			}
		});
	}


	protected void initialize() {
		edgeLayouter = createEdgeLayout();
		hierarhicLayouter = createHierarchicalLayouter();
		organicLayouter = createOrganicLayouter();
	}


	protected LayoutStage createEdgeLayout() {
		OrthogonalEdgeRouter router = new OrthogonalEdgeRouter();
	    router.setLocalCrossingMinimizationEnabled(true);
	    router.setReroutingEnabled(true);
		return router;
	}


	protected Layouter createHierarchicalLayouter() {
		IncrementalHierarchicLayouter layouter = new IncrementalHierarchicLayouter();
		
		//set some options
		layouter.getNodeLayoutDescriptor().setMinimumLayerHeight(2);
		layouter.getNodeLayoutDescriptor().setMinimumDistance(10);

		//use top-to-bottom layout orientation
		// the  layouter.setOrientation(..) is not working therefore set orientation manually
		OrientationLayouter ol = new OrientationLayouter();
		ol.setOrientation(LayoutOrientation.TOP_TO_BOTTOM);
		layouter.setOrientationLayouter(ol);
		
		layouter.setFromScratchLayeringStrategy(IncrementalHierarchicLayouter.LAYERING_STRATEGY_HIERARCHICAL_TIGHT_TREE);
		layouter.setGroupAlignmentPolicy(IncrementalHierarchicLayouter.POLICY_ALIGN_GROUPS_CENTER);

	    final AdaptNodeToLabelWidths stage = new AdaptNodeToLabelWidths();
	    stage.setAdaptGroupNodesOnly(true);
	    layouter.prependStage(stage);
	    
	    final HierarchicLayouter hl = layouter.getHierarchicLayouter();
	    final DrawingDistanceCalculator ddc = hl.getDrawingDistanceCalculator();
	    // drawing distance calculator that calculates a minimum width for all
	    // non-empty group nodes in such a way that node.x is less than or equal to
	    // the minimum x-coordinate of all node labels and node.x + node.width is
	    // greater than or equal to the maximum x-coordinate of all node labels
	    //
	    // this is done due to the fact that non-empty group nodes are an exception
	    // to the general "no node resizing" policy of yFiles layout algorithms
	    final LabelAwareDrawingDistanceCalculator laddc =
	            new LabelAwareDrawingDistanceCalculator(ddc);
	    hl.setDrawingDistanceCalculator(laddc);

	    // horizontal group compaction tries to prevent
	    // IncrementalHierarchicLayouter from being to generous when calculating
	    // group node sizes for non-empty group nodes
	    SimplexNodePlacer snp = new SimplexNodePlacer();
	    snp.setGroupCompactionStrategy(SimplexNodePlacer.GROUP_COMPACTION_MAX);
		snp.setEdgeStraighteningOptimizationEnabled(true);
		layouter.setGroupCompactionEnabled(true);
		layouter.setRecursiveGroupLayeringEnabled(true);
	    layouter.setNodePlacer(snp);

		return layouter;
	}
	
	private Layouter createOrganicLayouter() {
		SmartOrganicLayouter layouter = new SmartOrganicLayouter();
		layouter.setMinimalNodeDistance(20);
		layouter.setPreferredEdgeLength(20);
		layouter.setQualityTimeRatio(1);
		layouter.setCompactness(1);
		int compactness = MainPreferencePage.getLayoutCompactness();
		if (compactness < 100) {
			layouter.setOutputRestriction(OutputRestriction.NONE);
		}
		else {
			layouter.setOutputRestriction(OutputRestriction.createRectangularCageRestriction(compactness, compactness, compactness, compactness));
		}
		layouter.setNodeEdgeOverlapAvoided(true);
		return layouter;
	}
	
	public Layouter getLayouter(){
		CompositeLayoutStage stage  = new CompositeLayoutStage();
		
		if (PredicateLayoutPreferences.getLayoutPreference().equals(PreferenceConstants.LAYOUT_HIERARCHY)) {
			
			stage.setCoreLayouter(hierarhicLayouter);
		}
		else {
			stage.setCoreLayouter(organicLayouter);
		}
		
		stage.appendStage(edgeLayouter);
		
		return stage;
	}
	
	public Layouter getEdgeLayouter(){
		return this.edgeLayouter;
	}
	
	
}


