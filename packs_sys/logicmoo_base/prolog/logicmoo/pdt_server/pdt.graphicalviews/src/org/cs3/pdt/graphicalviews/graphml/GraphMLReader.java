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

package org.cs3.pdt.graphicalviews.graphml;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import org.cs3.pdt.graphicalviews.model.GraphDataHolder;
import org.cs3.pdt.graphicalviews.model.GraphModel;
import org.cs3.pdt.graphicalviews.model.realizer.nodes.PredicateNodeRealizerSerializer;

import y.io.GraphMLIOHandler;
import y.io.graphml.KeyScope;
import y.io.graphml.KeyType;
import y.io.graphml.graph2d.Graph2DGraphMLHandler;
import y.util.GraphCopier;
import y.view.Graph2D;
import y.view.Graph2DCopyFactory;


public class GraphMLReader {

	private static final Graph2D DEFAULT_EMPTY_GRAPH=new Graph2D();

	private GraphMLIOHandler ioHandler = new GraphMLIOHandler();
	private Graph2DGraphMLHandler core = ioHandler.getGraphMLHandler();
	private GraphCopier graphCopier = new GraphCopier(new Graph2DCopyFactory());

	private GraphModel model = null;

	public GraphMLReader(){
		model = new GraphModel();
		model.useHierarchy();

		addInputDataAccessorsToCore();

		ioHandler.addNodeRealizerSerializer(new PredicateNodeRealizerSerializer());
	}

	private void addInputDataAccessorsToCore() {
		GraphDataHolder dataHolder = model.getDataHolder();
		core.addInputDataAcceptor("id", dataHolder.getNodeMap(), KeyScope.NODE, KeyType.STRING);
		core.addInputDataAcceptor("module", dataHolder.getModuleMap(), KeyScope.NODE,KeyType.STRING);
		core.addInputDataAcceptor("moduleOfPredicate", dataHolder.getModuleOfPredicateMap(), KeyScope.NODE,KeyType.STRING);
		core.addInputDataAcceptor("fileName", dataHolder.getFileNameMap(), KeyScope.ALL, KeyType.STRING);
		core.addInputDataAcceptor("label", dataHolder.getLabelMap(), KeyScope.ALL, KeyType.STRING);
		core.addInputDataAcceptor("lineNumber", dataHolder.getLineNumberMap(), KeyScope.ALL, KeyType.INT);
		core.addInputDataAcceptor("offset", dataHolder.getOffsetMap(), KeyScope.ALL, KeyType.STRING);
		core.addInputDataAcceptor("kind", dataHolder.getKindMap(), KeyScope.ALL, KeyType.STRING);
		core.addInputDataAcceptor("metadata", dataHolder.getMetadataMap(), KeyScope.ALL, KeyType.STRING);
		core.addInputDataAcceptor("functor", dataHolder.getFunctorMap(), KeyScope.NODE, KeyType.STRING);
		core.addInputDataAcceptor("arity", dataHolder.getArityMap(), KeyScope.NODE, KeyType.INT);
		core.addInputDataAcceptor("frequency", dataHolder.getCallFrequencyMap(), KeyScope.EDGE, KeyType.INT);
		core.addInputDataAcceptor("isDynamic", dataHolder.getDynamicMap(), KeyScope.NODE, KeyType.BOOLEAN);
		core.addInputDataAcceptor("isTransparent", dataHolder.getTransparentMap(), KeyScope.NODE, KeyType.BOOLEAN);
		core.addInputDataAcceptor("isMultifile", dataHolder.getMultifileMap(), KeyScope.NODE, KeyType.BOOLEAN);
		core.addInputDataAcceptor("isMetaPredicate", dataHolder.getMetaPredMap(), KeyScope.NODE, KeyType.BOOLEAN);
		core.addInputDataAcceptor("metaPredicateType", dataHolder.getMetaPredTypeMap(), KeyScope.NODE, KeyType.STRING);
		core.addInputDataAcceptor("isExported", dataHolder.getExportedMap(), KeyScope.NODE, KeyType.BOOLEAN);
		core.addInputDataAcceptor("isUnusedLocal", dataHolder.getUnusedLocalMap(), KeyScope.NODE, KeyType.BOOLEAN);
		core.addInputDataAcceptor("exported_static_predicates", dataHolder.getModulePublicStaticPredicatesMap(), KeyScope.NODE, KeyType.STRING);
		core.addInputDataAcceptor("exported_dynamic_predicates", dataHolder.getModulePublicDynamicPredicatesMap(), KeyScope.NODE, KeyType.STRING);
		core.addInputDataAcceptor("node_stereotype", dataHolder.getNodeStereoTypeMap(), KeyScope.NODE, KeyType.STRING);
		core.addInputDataAcceptor("imported_predicates", dataHolder.getModuleImportedPredicatesMap(), KeyScope.EDGE, KeyType.STRING);
		core.addInputDataAcceptor("file_node_name", dataHolder.getFileNodeNameMap(), KeyScope.NODE, KeyType.STRING);
		core.addInputDataAcceptor("file_node_path", dataHolder.getFileNodePathMap(), KeyScope.NODE, KeyType.STRING);
		core.addInputDataAcceptor("file_node_type", dataHolder.getFileTypeMap(), KeyScope.NODE, KeyType.STRING);
		core.addInputDataAcceptor("node_label", dataHolder.getNodeLabelMap(), KeyScope.NODE, KeyType.STRING);
		core.addInputDataAcceptor("styles", dataHolder.getStylesMap(), KeyScope.ALL, KeyType.STRING);
		core.addInputDataAcceptor("node_content", dataHolder.getNodeContentMap(), KeyScope.NODE, KeyType.STRING);
	}

	private boolean loadFile(URL resource){
		model.clear();
		try {
			ioHandler.read(model.getGraph(), resource);
			return true;
		} catch (IOException e) {
			return false;
		}
	}



	public GraphModel readFile(URL resource){
		loadFile(resource);
		return model;
	}

	// For testing:
	Graph2D readFile(InputStream inputFileStream){
		model.clear();
		try {
			ioHandler.read(model.getGraph(), inputFileStream);
		} catch (IOException e) {
			return (Graph2D) graphCopier.copy(DEFAULT_EMPTY_GRAPH);
		}
		return model.getGraph();
	}



}


