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


import y.base.Node;
import y.geom.YRectangle;
import y.layout.LayoutGraph;
import y.layout.NodeLabelLayout;
import y.layout.NodeLayout;
import y.layout.hierarchic.incremental.DrawingDistanceCalculator;
import y.layout.hierarchic.incremental.Layer;
import y.layout.hierarchic.incremental.Layers;
import y.layout.hierarchic.incremental.LayoutDataProvider;
import y.layout.hierarchic.incremental.NodeData;


/**
 * <code>DrawingDistanceCalculator</code> that ensures a minimum group node
 * width of <code>max label x - min label x</code>.
 *
 * @author Thomas Behr
 */
public class LabelAwareDrawingDistanceCalculator
        implements DrawingDistanceCalculator {
  private static final double GROUP_LABEL_WIDTH_ADJUSTMENT = 20;


  private final DrawingDistanceCalculator wrappedCalculator;
  private double groupLabelWidthAdjustment;

  public LabelAwareDrawingDistanceCalculator(
          final DrawingDistanceCalculator wrappedCalculator
  ) {
    this.wrappedCalculator = wrappedCalculator;
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
   * The group label width adjustment is added to label widths to be able to
   * take the special group state switch label of yFiles default
   * <code>GroupNodeRealizer</code> into account.
   * @param adjustment   the group label width adjustment.
   */
  public void setGroupLabelWidthAdjustment( final double adjustment ) {
    this.groupLabelWidthAdjustment = adjustment;
  }


  @Override
public void initialize(
          LayoutGraph graph, Layers layers, LayoutDataProvider ldp
  ) {
    if (wrappedCalculator != null) {
      wrappedCalculator.initialize(graph, layers, ldp);
    }
  }

  @Override
public double getMinDistance(
          LayoutGraph graph,
          Layer layer,
          LayoutDataProvider ldp,
          Node left,
          Node right
  ) {
    if (left != null && right != null &&
        ldp.getNodeData(left).getType() == NodeData.TYPE_GROUP_BEGIN &&
        ldp.getNodeData(right).getType() == NodeData.TYPE_GROUP_END) {
      final Node groupNode = ldp.getNodeData(left).getAssociatedNode();

      final NodeLayout nl = graph.getNodeLayout(groupNode);
      double minX = nl.getX();
      double maxX = nl.getX() + nl.getWidth();

      final NodeLabelLayout[] labelLayouts = graph.getNodeLabelLayout(groupNode);
      for (int i = 0; i < labelLayouts.length; ++i) {
        final YRectangle lbx = labelLayouts[i].getBox();
        if (minX > lbx.x) {
          minX = lbx.x;
        }
        if (maxX < lbx.x + lbx.width + groupLabelWidthAdjustment) {
          maxX = lbx.x + lbx.width + groupLabelWidthAdjustment;
        }
      }

      return maxX - minX;
    } else if (wrappedCalculator != null) {
      return wrappedCalculator.getMinDistance(graph, layer, ldp, left, right);
    } else {
      return 0;
    }
  }

  @Override
public void dispose(
          LayoutGraph graph, Layers layers, LayoutDataProvider ldp
  ) {
    if (wrappedCalculator != null) {
      wrappedCalculator.dispose(graph, layers, ldp);
    }
  }
}


