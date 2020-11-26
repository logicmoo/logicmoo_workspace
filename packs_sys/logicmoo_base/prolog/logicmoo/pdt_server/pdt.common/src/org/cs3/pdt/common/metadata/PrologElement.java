/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.common.metadata;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;


/**
 * a tuple describing a logical prolog element like a predicate or a clause.

 */
public class PrologElement 
       extends PrologSourceLocation
       implements Serializable, Comparable<PrologElement>{

	/**
	 * Comment for <code>serialVersionUID</code>
	 */
	private static final long serialVersionUID = 1L;

	protected String contextModule;
	protected String functorName;
	protected int arity;

	protected boolean isKnown;
	private List<String> properties = null;

	/**
	 * Creates a PrologElementData Entity. This class is a container for
	 * Prolog elements like facts, clauses or modules. 
	 * 
	 * @param functorName
	 * @param arity if arity is -1 the element is a module.
	 */
	protected PrologElement(String filePath, int line,
			  String contextModule,String functorName, int arity, List<String>  properties) {
		super(filePath,line);
		this.functorName = functorName;
		this.arity = arity;
		this.contextModule=contextModule;		
		this.properties = properties;
		this.isKnown=true;
	}
	

	/**
	 * Creates a PrologElementData Entity. This class is a container for
	 * Prolog elements like facts, clauses or modules. 
	 * 
	 * @param functorName
	 * @param arity if arity is -1 the element is a module.
	 */
	protected PrologElement(String filePath, int line, String contextModule,String functorName, int arity) {
		this(filePath, line, contextModule, functorName,  arity, new ArrayList<String>());
		this.isKnown=false;
	}

	/**
	 * Creates a PrologElementData Entity. This class is a container for
	 * Prolog elements like facts, clauses or modules. 
	 * 
	 * @param functorName
	 * @param arity if arity is -1 the element is a module.
	 */
	protected PrologElement(String contextModule,String functorName, int arity, List<String>  properties) {
		this(null,0,contextModule, functorName,  arity, properties);
	}

	/**
	 * Returns the signature of the predicate:
	 * module:name/arity.
	 * 
	 * @return
	 */
	public String getSignature() {
//		if(arity == -1)
//			return functorName + " (module)";
		String modulePart;
		if(contextModule == null)
			modulePart = "";
		else 
			modulePart = contextModule + ":";
		return modulePart + functorName + "/" + (arity == -1 ? "(" + arity + ")" : arity);
	}

	@Override
	public String toString() {
		return getSignature();
	}

	public String getModule() {
		return contextModule;
	}

	public String getFunctor() {
		return functorName;
	}
	
	public int getArity() {
		return arity;
	}

	/**
	 * @return Returns the pub.
	 */
	public boolean isPublic() {
		if(!isKnown){
			throw new UnsupportedOperationException("Not enough information.");
		}
		return properties.contains("exported") || properties.contains("public");
	}
	
	/**
	 * @return Returns the pub.
	 */
	public boolean isProtected() {
		if(!isKnown){
			throw new UnsupportedOperationException("Not enough information.");
		}
		return properties.contains("protected");
	}
	
	/**
	 * @return Returns the pub.
	 */
	public boolean isPrivate() {
		if(!isKnown){
			throw new UnsupportedOperationException("Not enough information.");
		}
		return properties.contains("private");
	}
	
	/**
	 * @return Returns the pub.
	 */
	public boolean isLocal() {
		if(!isKnown){
			throw new UnsupportedOperationException("Not enough information.");
		}
		return properties.contains("local");
	}
	
	public boolean isDynamic() {
		if(!isKnown){
			throw new UnsupportedOperationException("Not enough information.");
		}
		return properties.contains("dynamic");
	}

	public boolean isMultifile() {
		if(!isKnown){
			throw new UnsupportedOperationException("Not enough information.");
		}
		return properties.contains("multifile");
	}

	public boolean isKnown() {
		return isKnown;
	}

	@Override
	public int hashCode() {
		return getSignature().hashCode();
	}

	public List<String> getProperties() {
		return properties;
	}

	@Override
	public boolean equals(Object obj) {
		if(obj==null){
			return false;
		}
		if(obj instanceof PrologElement){
			return ((PrologElement)obj).getSignature().equals(getSignature());
		}
		return super.equals(obj);
	}

	static public Comparator<PrologElement> getComparator() {
		return new Comparator<PrologElement>() {
			@Override
			public int compare(PrologElement arg0, PrologElement arg1) {
				return arg0.getSignature().compareTo(
						arg1.getSignature());
			}
		};
	}

	@Override
	public int compareTo(PrologElement arg0) {
		return getSignature().compareTo(((Predicate)arg0).getSignature());
	}

}


