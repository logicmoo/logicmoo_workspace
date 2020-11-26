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

import org.w3c.dom.Element;
import org.w3c.dom.Node;

import y.io.graphml.graph2d.AbstractNodeRealizerSerializer;
import y.io.graphml.input.GraphMLParseContext;
import y.io.graphml.input.GraphMLParseException;
import y.io.graphml.output.GraphMLWriteContext;
import y.io.graphml.output.XmlWriter;
import y.view.NodeRealizer;
/**
 * RealizerSerializer that can be used to serialize instances of StateNodeRealizer to/from GraphML
 */
public class PredicateNodeRealizerSerializer extends AbstractNodeRealizerSerializer {
	public PredicateNodeRealizerSerializer(){
		super();
		
	}
	
	@Override
	public String getName() {
		return "predicate";
	}


	@Override
	public Class<PredicateNodeRealizer> getRealizerClass() {
		return PredicateNodeRealizer.class;
	}

	@Override
	public void parse(NodeRealizer realizer, Node domNode, GraphMLParseContext context) throws GraphMLParseException {
		super.parse(realizer, domNode, context);
		
		PredicateNodeRealizer snr = (PredicateNodeRealizer) realizer;
		
		String attribute = ((Element) domNode).getAttribute("key");
		if(attribute.equals("module")){
			Node moduleNode = ((Element) domNode).getChildNodes().item(0);
			String state=moduleNode.toString();
			if("initial".equals(state)) {
				snr.setState(PredicateNodeRealizer.INITIAL_STATE);
			}
			else if("transition".equals(state)) {
				snr.setState(PredicateNodeRealizer.TRANSITION_STATE);
			}
			else if("final".equals(state)) {
				snr.setState(PredicateNodeRealizer.FINAL_STATE);
			}

		}

	}


	@Override
	public void writeAttributes(NodeRealizer nr, XmlWriter writer, GraphMLWriteContext context) {
		super.writeAttributes(nr, writer, context);
		PredicateNodeRealizer snr = (PredicateNodeRealizer) nr;
		switch(snr.getState()) {
		case PredicateNodeRealizer.INITIAL_STATE:
			writer.writeAttribute("state", "initial");
			break;
		case PredicateNodeRealizer.TRANSITION_STATE:
			writer.writeAttribute("state", "transition");
			break;
		case PredicateNodeRealizer.FINAL_STATE:
			writer.writeAttribute("state", "final");
			break;
		}
	}


	@Override
	public String getNamespaceURI() {
		return null;
	}
}


