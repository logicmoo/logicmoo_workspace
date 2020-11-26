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


import y.base.DataProvider;
import y.base.Node;
import y.base.NodeCursor;
import y.geom.YRectangle;
import y.layout.AbstractLayoutStage;
import y.layout.LayoutGraph;
import y.layout.Layouter;
import y.layout.NodeLabelLayout;
import y.layout.NodeLayout;
import y.layout.grouping.GroupingKeys;
import y.util.DataProviderAdapter;


/**
 * <code>LayoutStage</code> that updates node bounds such that for all labels
 * associated to a node, the minimum and maximum x-coordinates of a label's
 * bounding box are contained in the node bounds.
 *
 * @author Thomas Behr
 */
public class AdaptNodeToLabelWidths extends AbstractLayoutStage {
  private static final double GROUP_LABEL_WIDTH_ADJUSTMENT = 20;

  private boolean adaptGroupNodesOnly;
  private double groupLabelWidthAdjustment;

  public AdaptNodeToLabelWidths() {
    this.adaptGroupNodesOnly = false;
    this.groupLabelWidthAdjustment = GROUP_LABEL_WIDTH_ADJUSTMENT;
  }


  /**
   * Returns the group label width adjustment.
   * @return the group label width adjustment.
   */
  public double getGroupLabelWidthAdjustment() {
    return groupLabelWidthAdjustment;
  }

  /**
   * Specifies the group label width adjustment.
   * The group label width adjustment is added to label widths for group nodes
   * to be able to take the special group state switch label of yFiles default
   * <code>GroupNodeRealizer</code> into account.
   * @param adjustment   the group label width adjustment.
   */
  public void setGroupLabelWidthAdjustment( final double adjustment ) {
    this.groupLabelWidthAdjustment = adjustment;
  }


  /**
   * Returns <code>true</code> if this layout stage should work on group nodes
   * only and <code>false</code> otherwise.
   * @return <code>true</code> if this layout stage should work on group nodes
   * only and <code>false</code> otherwise.
   */
  public boolean getAdaptGroupNodesOnly() {
    return adaptGroupNodesOnly;
  }

  /**
   * Specifies whether this layout stage should work on group nodes only.
   * Defaults to <code>false</code>
   * @param adaptGroupNodesOnly   if <code>true</code>, update node bounds
   *                              for group nodes only.
   */
  public void setAdaptGroupNodesOnly( final boolean adaptGroupNodesOnly ) {
    this.adaptGroupNodesOnly = adaptGroupNodesOnly;
  }


  @Override
public boolean canLayout( final LayoutGraph graph ) {
    final Layouter layouter = getCoreLayouter();
    return layouter == null || layouter.canLayout(graph);
  }

  @Override
public void doLayout( final LayoutGraph graph ) {
    adaptNodeWidthToLabels(graph);
    final Layouter layouter = getCoreLayouter();
    if (layouter != null) {
      layouter.doLayout(graph);
    }
  }

  private void adaptNodeWidthToLabels( final LayoutGraph graph ) {
    DataProvider isGroup;
    isGroup = graph.getDataProvider(GroupingKeys.GROUP_DPKEY);
    if (isGroup == null) {
      isGroup = new DataProviderAdapter() {
        @Override
		public boolean getBool( final Object node ) {
          return false;
        }
      };
    }

    for (NodeCursor nc = graph.nodes(); nc.ok(); nc.next()) {
      final Node node = nc.node();
      final boolean _isGroup = isGroup.getBool(node);

      if (!_isGroup && adaptGroupNodesOnly) {
        continue;
      }

      final NodeLayout nl = graph.getNodeLayout(node);
      double minX = nl.getX();
      double maxX = nl.getX() + nl.getWidth();

      final double adjustment =
              _isGroup ? groupLabelWidthAdjustment : 0;

      final NodeLabelLayout[] labelLayouts = graph.getNodeLabelLayout(node);
      for (int i = 0; i < labelLayouts.length; ++i) {
        final YRectangle lbx = labelLayouts[i].getBox();
        if (minX > lbx.x) {
          minX = lbx.x;
        }
        if (maxX < lbx.x + lbx.width + adjustment) {
          maxX = lbx.x + lbx.width + adjustment;
        }
      }

      nl.setSize(maxX - minX, nl.getHeight());
      nl.setLocation(minX, nl.getY());
    }
  }
}


