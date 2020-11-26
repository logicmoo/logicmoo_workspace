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

package org.cs3.pdt.editor.internal.queries;

import static org.cs3.prolog.connector.common.QueryUtils.bT;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.common.PDTCommonPredicates;
import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.connector.PrologConnectorPredicates;
import org.cs3.pdt.editor.PDT;
import org.cs3.pdt.editor.PDTPlugin;
import org.cs3.pdt.editor.internal.structureElements.OutlineModuleElement;
import org.cs3.pdt.editor.internal.structureElements.PrologClause;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.process.PrologException;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.cs3.prolog.connector.session.PrologSession;


public class PDTOutlineQuery {

	public static Map<String, OutlineModuleElement> getProgramElementsForFile(String fileName/*, Shell shell*/) {	
		PrologSession session=null;
		try {
			session = PDTCommonUtil.getActivePrologProcess().getSession();
			
			session.queryOnce(PrologConnectorPredicates.WAIT_FOR_RELOAD_FINISHED);

			String query = bT(PDTCommonPredicates.FIND_DEFINITION_CONTAINED_IN,
					QueryUtils.quoteAtom(fileName),
					getOptions(),
					"Entity",
					"EntityLine",
					"KindOfEntity",
					"Functor",
					"Arity",
					"TypeOfDef",
					"Line",
					"PropertyList");
			List<Map<String, Object>> result = session.queryAll(query);

			if(! result.isEmpty()) {
				return extractResults(result, fileName, session);
			}
		}catch(Exception e){
			Debug.report(e);
		} finally {
			if(session!=null)session.dispose();
		}
		return new HashMap<String, OutlineModuleElement>();
	}
	
	private static String getOptions() {
		StringBuffer buf = new StringBuffer("[multifile(");
		buf.append(Boolean.toString(PDTPlugin.getDefault().getPreferenceStore().getBoolean(PDT.PREF_OUTLINESHOW_MULTIFILE)));
		buf.append("), all_clauses(");
		buf.append(PDTPlugin.getDefault().getPreferenceStore().getString(PDT.PREF_OUTLINE_SHOW_ALL_CLAUSES));
		buf.append("), first_arg_size_limit(");
		buf.append(Integer.toString(PDTPlugin.getDefault().getPreferenceStore().getInt(PDT.PREF_OUTLINE_FIRST_ARGUMENT_VARIABLE_FILE_SIZE) * 1024));
		buf.append(")]");
		return buf.toString();
	}

	@SuppressWarnings("unchecked")
	private static Map<String, OutlineModuleElement> extractResults(List<Map<String, Object>> result, String fileName, PrologSession session) throws PrologException, PrologProcessException {
		Map<String, OutlineModuleElement> modules= new HashMap<String, OutlineModuleElement>();	
		for (Map<String, Object> predicate : result) {
			String module = predicate.get("Entity").toString();
			int entityLine = Integer.parseInt( predicate.get("EntityLine").toString() );
			String name = predicate.get("Functor").toString();
			String kindOfEntity = predicate.get("KindOfEntity").toString();
			int arity=Integer.parseInt(predicate.get("Arity").toString());
			int line = Integer.parseInt(predicate.get("Line").toString());
			String type = predicate.get("TypeOfDef").toString();
			Object prop = predicate.get("PropertyList");
			List<String> properties = null;
			if (prop instanceof Vector<?>) {
				properties = (Vector<String>)prop;
			} else {
				properties = new Vector<String>();
			}
			String forEntity = PDTCommonUtil.getProperty("for", properties);
			if (forEntity != null) {
				module = forEntity;
			}
			PrologClause clause = new PrologClause(fileName, module, entityLine, kindOfEntity, name, arity, line, type, properties);
			OutlineModuleElement moduleElement = modules.get(module);
			if (moduleElement == null) {
//				boolean fileAndModuleFileEqual = true;
//				if ("module".equals(kindOfEntity)) {
//					if (session.queryOnce(bT("module_of_file", Util.quoteAtom(fileName), Util.quoteAtom(module))) == null) {
//						fileAndModuleFileEqual = false;
//					}
//				}
//				moduleElement = new OutlineModuleElement(clause.getOccuranceFile(), module, entityLine, kindOfEntity, fileAndModuleFileEqual);
				moduleElement = new OutlineModuleElement(clause.getOccuranceFile(), module, entityLine, kindOfEntity, forEntity == null);
				modules.put(module, moduleElement);
			}
			moduleElement.addClause(clause);
			
			
//			if (!modules.containsKey(module)) {
//				modules.put(module, new OutlineModuleElement(clause.getOccuranceFile(), module, entityLine, kindOfEntity));
//			}
//			OutlineModuleElement currentModuleElem = modules.get(module);
//			String label = module+":"+name+"/"+arity;
//
//			OutlinePredicateElement prologPredicate;
//			if (currentModuleElem.hasPredicate(label)) {
//				prologPredicate = currentModuleElem.getPredicate(label);
//			} else {
//				prologPredicate = new OutlinePredicateElement(null, module, name, arity, properties, fileName);
//				currentModuleElem.addChild(label, prologPredicate);
//			}
//			
//			String occuranceLabel = calculateOccuranceLabel(line, type, properties);
//			String occuranceFile = getOccuranceFileName(properties, fileName);
//			
//			OutlineClauseElement occurrence = new OutlineClauseElement(occuranceLabel.toString(), occuranceFile, line, type, prologPredicate);
//			if (occuranceFile.equals(fileName)) {
//				prologPredicate.addOccurence(occurrence);
//			} else {
//				prologPredicate.addClauseToFile(occuranceFile, occurrence);
//			}
		}

		return modules;
	}

//	public static String calculateOccuranceLabel(int line, String type,
//			List<String> properties) {
//		StringBuffer occuranceLabel = new StringBuffer("Line: ");
//		occuranceLabel.append(Integer.toString(line));
//		occuranceLabel.append(" (");
//		occuranceLabel.append(type);
////		if (type.equals("multifile")) {
////			for (String property : properties) {
////				if (property.startsWith("from(")) {
////					occuranceLabel.append(" @ ");
////					occuranceLabel.append((String) property.subSequence(5, property.length()-1));
////				} else if (property.startsWith("for(")) {
////					occuranceLabel.append(" @ ");
////					occuranceLabel.append((String) property.subSequence(4, property.length()-1));
////				}
////			}
////		}
//		occuranceLabel.append(")");
//		return occuranceLabel.toString();
//	}
//	
//	private static String getOccuranceFileName(List<String> properties, String file) {
//		String selectedFile = null;
//		if (properties.contains("multifile")) {
//			for (String property : properties) {
//				if (property.startsWith("defining_file(")) {
//					selectedFile = property.substring(15, property.length()-2);
//				}
//			}
//		}
//		if (selectedFile == null) {
//			selectedFile = file;
//		}
//		return selectedFile;
//	}
	
	public static boolean isFileLoaded(String fileName) {
		PrologProcess process = PDTCommonUtil.getActivePrologProcess();
		Map<String, Object> result = null;
		try {
			result = process.queryOnce(bT(PDTCommonPredicates.LOADED_FILE, QueryUtils.quoteAtom(fileName)));
		} catch (PrologProcessException e) {
			Debug.report(e);
		}
		return result != null;
	}
}


