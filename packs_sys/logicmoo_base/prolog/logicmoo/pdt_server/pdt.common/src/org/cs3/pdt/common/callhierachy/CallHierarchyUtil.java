/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2014, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/
package org.cs3.pdt.common.callhierachy;

import static org.cs3.prolog.connector.common.QueryUtils.bT;
import static org.cs3.prolog.connector.common.QueryUtils.quoteAtom;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.common.PDTCommonPlugin;
import org.cs3.pdt.common.PDTCommonPredicates;
import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.common.internal.ImageRepository;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;

public class CallHierarchyUtil {

	private static IDialogSettings section;
	private static final String CALL_HIERARCHY_SECTION = "call.hierarchy.section";
	private static final String CALL_HIERARCHY_HISTORY_SECTION_PREFIX = "call.hierarchy.history.";
	private static final String CALL_HIERARCHY_HISTORY_SIZE = "call.hierarchy.history.size";
	static final int MAX_HISTORY_SIZE = 10;
	private static final String MODULE = "module";
	private static final String NAME = "name";
	private static final String ARITY = "arity";
	private static final String VISIBILITY = "visibility";
	
	static IDialogSettings getSettings() {
		if (CallHierarchyUtil.section == null) {
			IDialogSettings dialogSettings = PDTCommonPlugin.getDefault().getDialogSettings();
			CallHierarchyUtil.section = dialogSettings.getSection(CallHierarchyUtil.CALL_HIERARCHY_SECTION);
			if (CallHierarchyUtil.section == null) {
				CallHierarchyUtil.section = dialogSettings.addNewSection(CallHierarchyUtil.CALL_HIERARCHY_SECTION);
			}
		}
		return CallHierarchyUtil.section;
	}

	static void selectLocationInEditor(Location location) {
		try {
			PDTCommonUtil.selectInEditor(location.getStart(), location.getEnd() - location.getStart(), location.getFile(), true, false);
		} catch (PartInitException e) {
			Debug.report(e);
		}
	}
	
	static List<Predicate> loadHistory() {
		ArrayList<Predicate> predicates = new ArrayList<>();
		IDialogSettings settings = getSettings();
		int historySize = getHistorySize();
		for (int i = 0; i < historySize; i++) {
			String sectionName = CALL_HIERARCHY_HISTORY_SECTION_PREFIX + Integer.toString(i);
			IDialogSettings predicateSection = settings.getSection(sectionName);
			if (predicateSection != null) {
				predicates.add(new Predicate(predicateSection.get(MODULE), predicateSection.get(NAME), predicateSection.get(ARITY), predicateSection.get(VISIBILITY)));
			}
		}
		return predicates;
	}
	
	static void saveHistory(List<Predicate> predicates) {
		int i = 0;
		IDialogSettings settings = getSettings();
		for (Predicate predicate : predicates) {
			String sectionName = CALL_HIERARCHY_HISTORY_SECTION_PREFIX + Integer.toString(i);
			IDialogSettings predicateSection = settings.getSection(sectionName);
			if (predicateSection == null) {
				predicateSection = settings.addNewSection(sectionName);
			}
			predicateSection.put(MODULE, predicate.getModule());
			predicateSection.put(NAME, predicate.getName());
			predicateSection.put(ARITY, predicate.getArity());
			predicateSection.put(VISIBILITY, predicate.getVisibility());
			i++;
		}
		getSettings().put(CALL_HIERARCHY_HISTORY_SIZE, predicates.size());
	}
	
	static void clearHistory() {
		getSettings().put(CALL_HIERARCHY_HISTORY_SIZE, 0);
	}
	
	private static int getHistorySize() {
		int size = 0;
		try {
			size = getSettings().getInt(CALL_HIERARCHY_HISTORY_SIZE);
		} catch (NumberFormatException e) {
			getSettings().put(CALL_HIERARCHY_HISTORY_SIZE, 0);
		}
		return size;
	}

	static Image getImageForVisibility(String visibility) {
		switch (visibility) {
		case "exported":
			return ImageRepository.getImage(ImageRepository.PE_PUBLIC);
		case "non_exported":
			return ImageRepository.getImage(ImageRepository.PE_PROTECTED);
		case "undefined":
			return ImageRepository.getImage(ImageRepository.PE_UNDEFINED);
		}
		return null;
	}

	static Predicate createPredicate(String specifiedModule, String name, String arity) throws PrologProcessException {
		return internalCreatePredicate(bT("module", quoteAtom(specifiedModule)), name, arity);
	}
	
	private static Predicate internalCreatePredicate(String moduleOrFile, String name, String arity) throws PrologProcessException {
		PrologProcess process = PDTCommonUtil.getActivePrologProcess();
		Map<String, Object> queryResult = process.queryOnce(bT(PDTCommonPredicates.FIND_PREDICATE_DECLARATION_AND_VISIBILITY, moduleOrFile, quoteAtom(name), arity, "DefiningModule", "Visibilty"));
		if (queryResult == null) {
			return null;
		}
		String module = (String) queryResult.get("DefiningModule");
		String visibility = (String) queryResult.get("Visibilty");
		Predicate newRoot = new Predicate(module, name, arity, visibility);
		return newRoot;
	}
	
	public static void showCallHierarchyFor(IWorkbenchWindow window, String module, String name, int arity) throws PartInitException, PrologProcessException {
		CallHierarchyView view = (CallHierarchyView) window.getActivePage().showView(CallHierarchyView.ID);
		Predicate root = createPredicate(module, name, Integer.toString(arity));
		view.setRoot(root);
	}
	
	public static void showCallHierarchyFor(IWorkbenchWindow window, String name, int arity, String fileName) throws PartInitException, PrologProcessException {
		CallHierarchyView view = (CallHierarchyView) window.getActivePage().showView(CallHierarchyView.ID);
		Predicate root = internalCreatePredicate(bT("file", quoteAtom(fileName)), name, Integer.toString(arity));
		view.setRoot(root);
	}
	
}
