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

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.ui.progress.IDeferredWorkbenchAdapter;

public class Predicate implements Comparable<Predicate>, IAdaptable {
	
	private String module;
	private String name;
	private String arityString;
	private int arity;
	private String visibility;
	
	private DeferredWorkbenchAdapter deferredWorkbenchAdapter;
	
	private String label;
	private StyledString styledString;
	
	private int hash = 0;

	public Predicate(String module, String name, String arity, String visibility) {
		this.module = module;
		this.name = name;
		this.arityString = arity;
		this.arity = Integer.parseInt(arity);
		this.visibility = visibility;
	}
	
	public String getModule() {
		return module;
	}

	public String getName() {
		return name;
	}

	public String getArity() {
		return arityString;
	}

	@Override
	public int hashCode() {
		if (hash == 0) {
			hash = (module + "#" + name + "#" + arityString).hashCode();
		}
		return hash;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Predicate) {
			return module.equals(((Predicate) obj).module)
					&& name.equals(((Predicate) obj).name)
					&& arity == ((Predicate) obj).arity;
		}
		return false;
	}
	
	public String getLabel() {
		if (label == null) {
			label = module + ":" + name + "/" + arityString;
		}
		return label;
	}
	
	public StyledString getStyledString() {
		if (styledString == null) {
			styledString = new StyledString();
			styledString.append(module, StyledString.QUALIFIER_STYLER);
			styledString.append(':', StyledString.QUALIFIER_STYLER);
			styledString.append(name);
			styledString.append('/');
			styledString.append(arityString);
		}
		return styledString;
	}
	
	public String getVisibility() {
		return visibility;
	}
	
	@Override
	public String toString() {
		return getLabel();
	}

	@Override
	public int compareTo(Predicate o) {
		int c = module.compareTo(o.module);
		if (c != 0) {
			return c;
		}
		c = name.compareTo(o.name);
		if (c != 0) {
			return c;
		}
		return arity - o.arity;
	}

	@Override
	public Object getAdapter(@SuppressWarnings("rawtypes") Class adapter) {
		if (IDeferredWorkbenchAdapter.class.equals(adapter)) {
			return deferredWorkbenchAdapter;
		}
		return null;
	}

	public void setDeferredWorkbenchAdapter(DeferredWorkbenchAdapter deferredWorkbenchAdapter) {
		this.deferredWorkbenchAdapter = deferredWorkbenchAdapter;
	}

}
