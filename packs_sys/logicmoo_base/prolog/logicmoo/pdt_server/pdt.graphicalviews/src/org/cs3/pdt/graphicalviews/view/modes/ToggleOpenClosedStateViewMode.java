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

import java.awt.event.MouseEvent;

import y.base.Node;
import y.view.Graph2D;
import y.view.Graph2DViewActions;
import y.view.NodeLabel;
import y.view.NodeRealizer;
import y.view.ProxyShapeNodeRealizer;
import y.view.ViewMode;
import y.view.hierarchy.GroupNodeRealizer;
import y.view.hierarchy.HierarchyManager;

public class ToggleOpenClosedStateViewMode extends ViewMode {  
  HierarchyManager hierarchyManager;  

  @Override
public void mouseClicked(MouseEvent event) {  
    // Convert the mouse event's coordinates from view to world coordinates.   
    double x = translateX(event.getX());  
    double y = translateY(event.getY());  
      
    // Retrieve the node that has been hit at the location.   
    Node node = getHitInfo(event).getHitNode();  
    
    if(node == null)
    	return;
    
    GroupNodeRealizer groupNodeRealizer = getGroupNodeRealizer(node);
  
    if(groupNodeRealizer == null)
    	return;
    
    NodeLabel stateLabel = groupNodeRealizer.getStateLabel();  
    // Test, if the mouse event occurred on the state icon.   
    if (stateLabel.getBox().contains(x, y)) {  
    	// Retrieve the HierarchyManager of the hierarchically organized graph.   
    	hierarchyManager = HierarchyManager.getInstance(view.getGraph2D());  

    	if (hierarchyManager.isFolderNode(node)) {
    		// If the node is a folder node a specific pre defined action openFolders needs to be used
    		// A group node is: a folder node if the state is closed.
    		//                  a group node  if the state is open
    		new Graph2DViewActions.OpenFoldersAction().openFolders(view);  
    	}  
    	else {  
    		new Graph2DViewActions.CloseGroupsAction().closeGroups(view);  
    	}  
    }  
      
  }
 /**
  * 
  * Tests if the node is rendered by GroupNodeRealizer.   
  * 
  */
  private GroupNodeRealizer getGroupNodeRealizer(Node node) {
	  GroupNodeRealizer groupNodeRealizer = null;
	  Graph2D graph =  view.getGraph2D();

	  NodeRealizer nodeRealizer = graph.getRealizer(node);
	  if (nodeRealizer instanceof GroupNodeRealizer) {
		  groupNodeRealizer = (GroupNodeRealizer) nodeRealizer;
	  }else if (nodeRealizer instanceof ProxyShapeNodeRealizer &&
			  ((ProxyShapeNodeRealizer) nodeRealizer).getRealizerDelegate() instanceof GroupNodeRealizer) {
		  groupNodeRealizer = (GroupNodeRealizer) ((ProxyShapeNodeRealizer) nodeRealizer).getRealizerDelegate();
	  }
	  return groupNodeRealizer;
  }  
}  


