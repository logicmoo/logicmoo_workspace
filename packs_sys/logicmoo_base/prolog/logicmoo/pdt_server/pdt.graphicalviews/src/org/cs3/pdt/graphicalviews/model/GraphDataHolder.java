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

import y.base.DataAcceptor;
import y.base.DataMap;
import y.base.Edge;
import y.base.Node;
import y.util.Maps;

public class GraphDataHolder {

	private static final String MODULE = "module";
	private static final String FILE = "file";
	private static final String FILE_NODE = "file_node";
	private static final String PREDICATE = "predicate";
	private static final String CALL = "call";
	private static final String METADATA_DATABASE = "database";
	private static final String METADATA_METACALL = "metacall";
	private static final String LOADING = "loading";
	private static final String TOP_FILE = "top";
	private static final String LOGTALK_GROUP = "logtalk_group";
	private static final String LOGTALK_NODE = "logtalk_node";
	private static final String LOGTALK_EDGE = "logtalk_edge";
	@SuppressWarnings("unused")
	private static final String INTERMEDIATE_FILE = "intermediate";
	private static final String BOTTOM_FILE = "bottom";

	// Addition data:
	private DataMap nodeMap = Maps.createHashedDataMap();
	private DataMap moduleMap = Maps.createHashedDataMap();
	private DataMap moduleOfPredicateMap = Maps.createHashedDataMap();
	private DataMap fileNameMap = Maps.createHashedDataMap();
	private DataMap lineNumberMap = Maps.createHashedDataMap();
	private DataMap offsetMap = Maps.createHashedDataMap();
	private DataMap fileNodeNameMap = Maps.createHashedDataMap();
	private DataMap fileNodePathMap = Maps.createHashedDataMap();
	private DataMap fileTypeMap = Maps.createHashedDataMap();
	private DataMap kindMap = Maps.createHashedDataMap();
	private DataMap metadataMap = Maps.createHashedDataMap();
	private DataMap functorMap = Maps.createHashedDataMap();
	private DataMap arityMap = Maps.createHashedDataMap();
	private DataMap callFrequencyMap = Maps.createHashedDataMap();
	private DataMap dynamicMap = Maps.createHashedDataMap();
	private DataMap transparentMap = Maps.createHashedDataMap();
	private DataMap metaPredMap = Maps.createHashedDataMap();
	private DataMap metaPredTypeMap = Maps.createHashedDataMap();
	private DataMap multifileMap = Maps.createHashedDataMap();
	private DataMap exportedMap = Maps.createHashedDataMap();
	private DataMap unusedLocal = Maps.createHashedDataMap();
	private DataMap modulePublicStaticPredicatesMap = Maps.createHashedDataMap();
	private DataMap modulePublicDynamicPredicatesMap = Maps.createHashedDataMap();
	private DataMap nodeStereoTypeMapMap = Maps.createHashedDataMap();
	private DataMap moduleImportedPredicatesMap = Maps.createHashedDataMap();
	private DataMap labelMap = Maps.createHashedDataMap();
	private DataMap nodeLabelMap = Maps.createHashedDataMap();
	private DataMap stylesMap = Maps.createHashedDataMap();
	private DataMap nodeContentMap = Maps.createHashedDataMap();


	// Getter and Setter
	public DataMap getNodeMap() {
		return nodeMap;
	}

	public DataMap getModuleMap() {
		return moduleMap;
	}
	
	public DataMap getModuleOfPredicateMap() {
		return moduleOfPredicateMap;
	}

	public DataMap getFileNameMap() {
		return fileNameMap;
	}
	
	public DataAcceptor getLineNumberMap() {
		return lineNumberMap;
	}
	public DataMap getFileNodeNameMap() {
		return fileNodeNameMap;
	}
	
	public DataMap getOffsetMap() {
		return offsetMap;
	}
	
	public DataMap getFileNodePathMap() {
		return fileNodePathMap;
	}

	public DataMap getKindMap() {
		return kindMap;
	}
	
	public DataMap getMetadataMap() {
		return metadataMap;
	}

	public DataMap getFunctorMap() {
		return functorMap;
	}

	public DataMap getArityMap() {
		return arityMap;
	}

	public DataMap getCallFrequencyMap() {
		return callFrequencyMap;
	}

	public DataMap getDynamicMap() {
		return dynamicMap;
	}

	public DataMap getTransparentMap() {
		return transparentMap;
	}

	public DataMap getMetaPredMap() {
		return metaPredMap;
	}
	
	public DataMap getMetaPredTypeMap() {
		return metaPredTypeMap;
	}

	public DataMap getMultifileMap() {
		return multifileMap;
	}


	public DataMap getExportedMap() {
		return exportedMap;
	}

	public DataMap getUnusedLocalMap() {
		return unusedLocal;
	}
	
	public DataMap getModulePublicStaticPredicatesMap() {
		return modulePublicStaticPredicatesMap;
	}
	
	public DataMap getModulePublicDynamicPredicatesMap() {
		return modulePublicDynamicPredicatesMap;
	}
	
	public DataMap getNodeStereoTypeMap() {
		return nodeStereoTypeMapMap;
	}
	
	public DataMap getModuleImportedPredicatesMap() {
		return moduleImportedPredicatesMap;
	}
	
	public DataMap getFileTypeMap() {
		return fileTypeMap;
	}
	
	public DataMap getLabelMap() {
		return labelMap;
	}
	
	public DataMap getNodeLabelMap() {
		return nodeLabelMap;
	}

	public DataMap getStylesMap() {
		return stylesMap;
	}
	
	public DataMap getNodeContentMap() {
		return nodeContentMap;
	}
	
	public String getFileName(Node node) {
		return (String)getFileNameMap().get(node);
	}
	
	public String getFileName(Edge edge) {
		return (String)getFileNameMap().get(edge);
	}
	
	public String getOffset(Node node) {
		return (String)getOffsetMap().get(node);
	}
	
	public String getOffset(Edge edge) {
		return (String)getOffsetMap().get(edge);
	}

	public boolean isPredicate(Node node) {
		DataMap kindMap = getKindMap();
		String kind = (String)kindMap.get(node);
		return kind.equals(PREDICATE);
	}

	public boolean isModule(Node node) {
		DataMap kindMap = getKindMap();
		String kind = (String)kindMap.get(node);
		return kind.equals(MODULE);
	}

	public boolean isFile(Node node) {
		DataMap kindMap = getKindMap();
		String kind = (String)kindMap.get(node);
		return kind.equals(FILE);
	}
	
	public boolean isFileNode(Node node) {
		DataMap kindMap = getKindMap();
		String kind = (String)kindMap.get(node);
		return kind.equals(FILE_NODE);
	}

	public boolean isCallEdge(Edge edge) {
		DataMap kindMap = getKindMap();
		String kind = (String)kindMap.get(edge);
		return kind.equals(CALL);
	}
	
	public boolean isMetaCall(Edge edge) {
		DataMap metadataMap = getMetadataMap();
		String metadata = String.valueOf(metadataMap.get(edge));
		return METADATA_METACALL.equals(metadata); 
	}
	
	public boolean isDatabaseCall(Edge edge) {
		DataMap metadataMap = getMetadataMap();
		String metadata = String.valueOf(metadataMap.get(edge));
		return METADATA_DATABASE.equals(metadata); 
	}
	
	public String getModulePublicStaticPredicates(Node node) {
		return (String)modulePublicStaticPredicatesMap.get(node);
	}
	
	public String getModulePublicDynamicPredicates(Node node) {
		return (String)modulePublicDynamicPredicatesMap.get(node);
	}
	
	public String getModuleImportedPredicates(Edge edge) {
		return (String)moduleImportedPredicatesMap.get(edge);
	}

	public boolean isLoadingEdge(Edge edge) {
		DataMap kindMap = getKindMap();
		String kind = kindMap.get(edge).toString();
		return kind.equals(LOADING);
	}

	public boolean isDynamicNode(Node node) {
		Object returnNode = dynamicMap.get(node);
		if(returnNode == null)
			return false;
		return (Boolean)returnNode;
	}

	public boolean isTransparentNode(Node node) {
		Object returnNode = transparentMap.get(node);
		if(returnNode == null)
			return false;
		return (Boolean)returnNode;
	}

	public boolean isMetaPred(Node node) {
		Object returnNode = metaPredMap.get(node);
		if(returnNode == null)
			return false;
		return (Boolean)returnNode;
	}
	
	public String getMetaPredType(Node node) {
		return (String)metaPredTypeMap.get(node);
	}

	public boolean isMultifile(Node node) {
		Object returnNode = multifileMap.get(node);
		if(returnNode == null)
			return false;
		return (Boolean)returnNode;
	}

	public boolean isExported(Node node) {
		Object returnNode = exportedMap.get(node);
		if(returnNode == null)
			return false;
		return (Boolean)returnNode;
	}

	public boolean isUnusedLocal(Node node) {
		Object returnNode = unusedLocal.get(node);
		if(returnNode == null)
			return false;
		return (Boolean)returnNode;
	}

	public boolean isTopFile(Node node) {
		String type = fileTypeMap.get(node).toString();
		return type.equals(TOP_FILE);
	}
	
	public boolean isBottomFile(Node node) {
		String type = fileTypeMap.get(node).toString();
		return type.equals(BOTTOM_FILE);
	}

	public String getLabelTextForNode(Node node) {
		String labelText;
		if (isModule(node)) {
			labelText = getModuleName(node);
		} else if (isFile(node)) {
			labelText = getLabel(node);
		} else if (isFileNode(node)) {
			labelText = getFileNodeText(node);
		} else if (isPredicate(node))  {
			labelText = getPredicateText(node);
		} else if (isLogtalkGraphNode(node) || isLogtalkGraphGroup(node))  {
			labelText = getNodeLabel(node);
		} else {
			labelText = getNodeText(node);
		}
		return labelText;
	}

	//	public int getIdForNode(Node node) {
	//		return (Integer) nodeMap.get(node);
	//	}

	private String getLabel(Object element) {
		return labelMap.get(element).toString();
	}

	private String getModuleName(Node node) {
		return moduleMap.get(node).toString();
	}
	
	private String getFileNodeText(Node node) {
		return fileNodeNameMap.get(node).toString();
	}

	private String getPredicateText(Node node) {
		return functorMap.get(node) + " / " + arityMap.get(node);
	}

	public String getNodeText(Node node) {
		return nodeMap.get(node).toString();
	}

	public int getFrequency(Edge edge) {
		return callFrequencyMap.getInt(edge);
	}
	
	public int getLineNumber(Node node) {
		return lineNumberMap.getInt(node);
	}
	
	public String getEdgeLabel(Edge edge) {
		return (String) labelMap.get(edge);
	}
	
	public String getNodeStereoType(Node node) {
		Object object = nodeStereoTypeMapMap.get(node);
		if (object != null) {
			return object.toString();
		} else {
			return null;
		}
	}
	
	public String getNodeLabel(Node node) {
		return (String) nodeLabelMap.get(node);
	}
	
	public boolean isLogtalkGraphGroup(Node node) {
		return LOGTALK_GROUP.equals(getKindMap().get(node));
	}
	
	public boolean isLogtalkGraphNode(Node node) {
		return LOGTALK_NODE.equals(getKindMap().get(node));
	}
	
	public boolean isLogtalkGraphEdge(Edge edge) {
		return LOGTALK_EDGE.equals(getKindMap().get(edge));
	}
	
	public String getNodeStyle(Node node) {
		return getStyle(node);
	}
	
	public String getEdgeStyle(Edge edge) {
		return getStyle(edge);
	}
	
	private String getStyle(Object element) {
		return (String) getStylesMap().get(element);
	}
	
	public String getNodeContent(Node node) {
		return (String) getNodeContentMap().get(node);
	}
	
	public boolean isFileNodeAModule(Node node) {
		return MODULE.equals(fileTypeMap.get(node));
	}

}


