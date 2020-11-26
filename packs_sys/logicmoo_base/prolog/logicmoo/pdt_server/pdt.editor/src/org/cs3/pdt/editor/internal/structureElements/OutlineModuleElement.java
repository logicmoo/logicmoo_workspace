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

package org.cs3.pdt.editor.internal.structureElements;

import java.util.HashMap;
import java.util.Map;

import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.common.metadata.PrologSourceLocation;
import org.cs3.pdt.common.structureElements.PrologTreeElement;

public class OutlineModuleElement extends PrologSourceLocation implements PrologTreeElement{
	private String name;  
	private String kind;   // Legal values are "module" (Prolog) or "entity" (Logtalk)
	private Map<String, OutlinePredicateElement> predicates= new HashMap<String,OutlinePredicateElement>();
	private Object parent;
	private boolean fileEqualToEditorFile;
	
	public OutlineModuleElement(String filePath, String name, int line, String kindOfEntity, boolean fileEqualToEditorFile) {
		super(filePath,line);
		this.name = name;
		kind = kindOfEntity;
		this.fileEqualToEditorFile = fileEqualToEditorFile;
	}
	
	public boolean hasPredicate(String key) {
		return predicates.containsKey(key);
	}
	
	public OutlinePredicateElement getPredicate(String key) {
		return predicates.get(key);
	}
	
	public void addChild(String key, OutlinePredicateElement predicate) {
		predicates.put(key, predicate);
	}
	
	@Override
	public boolean hasChildren() {
		return !(predicates.isEmpty());
	}
	
	public String getKind() {
		return kind;
	}
	
	public boolean fileEqualToEditorFile() {
		return fileEqualToEditorFile;
	}
	
	public String getName() {
		return name;
	}
	
	@Override
	public Object[] getChildren() {
		return predicates.values().toArray();
	}

	@Override
	public String getLabel() {
		return getName();
	}

	@Override
	public int hashCode() {
		return name.hashCode();
	}
	
	@Override
	public boolean equals(Object object) {
		if (object == null || !(object instanceof OutlineModuleElement)) {
			return false;
		} else {
			OutlineModuleElement other = (OutlineModuleElement) object;
			return (name.equals(other.name) && kind.equals(other.kind));
		}
	}
	
	public void setParent(Object parent) {
		this.parent = parent;
	}

	@Override
	public Object getParent() {
		return parent;
	}

	public void addClause(PrologClause clause) {
		String signature = getSignature(clause);
		OutlinePredicateElement predicateElement = predicates.get(signature);
		if (predicateElement == null) {
			predicateElement = new OutlinePredicateElement(this, clause.getEntity(), clause.getFunctor(), clause.getArity(), clause.getProperties(), clause.getFile());
			predicates.put(signature, predicateElement);
		}
		predicateElement.addClause(clause);
		if (PDTCommonUtil.getProperty("for", clause.getProperties()) != null) {
			fileEqualToEditorFile = false; 
		}
	}
	
	private String getSignature(PrologClause clause) {
		return clause.getFunctor() + "/" + clause.getArity();
	}

}


