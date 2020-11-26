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

package org.cs3.prolog.connector.cterm;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Vector;

import org.cs3.prolog.connector.Connector;
import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.common.Util;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.cs3.prolog.connector.session.PrologSession;

/**
 * Contains utility methods for handling CTerms.
 * 
 */
public class CTermUtil {
	
	public static boolean isList(CTerm term) {
		if(term instanceof CEmptyList){
			return true;
		}
		return term.getFunctorValue().equals(".") && term.getArity() == 2;
	}

	public static void checkFlags(int flags) {
		if(Util.flagsSet(flags,PrologProcess.CTERMS | PrologProcess.UNQUOTE_ATOMS) ){
			throw new IllegalArgumentException("cannot combine CTERMS and UNTQUOTE_ATOMS");
		}
		if(Util.flagsSet(flags, PrologProcess.CTERMS | PrologProcess.PROCESS_LISTS)){
			throw new IllegalArgumentException("cannot combine CTERMS and PROCESS_LISTS");
		}
	}
	
	public static CTerm[] listAsArray(CTerm term) {
		Vector<CTerm> v = listAsVector(term);
		return v.toArray(new CTerm[v.size()]);
	}

	public static Vector<CTerm> listAsVector(CTerm term) {
		Vector<CTerm> v = new Vector<CTerm>();
		while (term instanceof CCompound && ".".equals(term.getFunctorValue())
				&& 2 == term.getArity()) {
			CCompound compound = (CCompound) term;
			v.add(compound.getArgument(0));
			term = compound.getArgument(1);
		}
		return v;
	}
	
	/**
	 * Converts a property list term to a Map.
	 * 
	 * The argument should be list containing elements of the form
	 * property(value) or =(Property,Value). Atomic elements return themselves as second 
	 * argument since they don't have an explicit value. The returned map will contain a
	 * single value for each property found. If the same property name is
	 * encountered more than once, the returned map will reflect only the last
	 * occurrence.
	 * 
	 * @param term a term with functor "./2"
	 * 
	 * @return a map with key-type string, value type CTerm
	 */
	public static Map<String,CTerm> listAsMap(CTerm term) {
		Map<String,CTerm> m = new HashMap<String,CTerm>();
		while (term instanceof CCompound && ".".equals(term.getFunctorValue())
				&& 2 == term.getArity()) {
			CCompound compound = (CCompound) term;
			CTerm propertyTerm = compound.getArgument(0);
			if (propertyTerm instanceof CCompound) {
				if (propertyTerm.getFunctorValue().equals("=")
						&& propertyTerm.getArity() == 2) {
					m.put(
							(renderTerm(((CCompound) propertyTerm)
									.getArgument(0))),
							((CCompound) propertyTerm).getArgument(1));
				} else if (propertyTerm.getArity() == 1) {
					m.put(propertyTerm.getFunctorValue(),
							((CCompound) propertyTerm).getArgument(0));
				}

			} else if (propertyTerm.getArity() == 0) {
				m.put(propertyTerm.getFunctorValue(), propertyTerm);
			}

			term = compound.getArgument(1);
		}
		return m;
	}


	public static CTerm createCTerm(Object input) {
		return CTermFactory.createCTerm(input);
	}

	public static String renderTerm(CTerm term) {
		StringBuffer sb = new StringBuffer();
		renderTerm(term, sb);
		return sb.toString();
	}

	private static void renderTerm(CTerm term, StringBuffer sb) {

		if (term instanceof CVariable) {
			sb.append(((CVariable) term).getVariableName());
		} else {
			sb.append(term.getFunctorImage());
		}
		if (term.getArity() > 0) {
			sb.append('(');
			CCompound compound = (CCompound) term;
			for (int i = 0; i < compound.getArity(); i++) {
				if (i > 0) {
					sb.append(",");
				}
				renderTerm(compound.getArgument(i), sb);
			}
			sb.append(')');
		}
	}

	public static String renderSignature(CTerm sig, String defaultModule) {
		CCompound term = (CCompound) sig;
		String module = defaultModule;
		if (":".equals(term.getFunctorValue())) {
			module = term.getArgument(0).getFunctorValue();
			term = (CCompound) term.getArgument(1);
		}
		String name = term.getArgument(0).getFunctorValue();
		int arity = ((CInteger) term.getArgument(1)).getIntValue();
		return module + ":" + name + "/" + arity;
	}
	

	
	public static CTerm parseNonCanonicalTerm(String query) throws IOException, PrologProcessException {
		PrologProcess process = Connector.newPrologProcess();
		CTerm returnTerm = parseNonCanonicalTerm(query, process);
		process.stop();
		return returnTerm;
	}

	public static CTerm parseNonCanonicalTerm(String term, PrologProcess process) throws PrologProcessException {

		PrologSession session = process.getSession(PrologProcess.CTERMS|PrologProcess.UNBOUND_VARIABLES);
		
		String query = QueryUtils.bT("atom_to_term", QueryUtils.quoteAtom(term), "Term", "D");

		Map<String, Object> result = session.queryOnce(query);
		session.dispose();

		CTerm cTerm = (CTerm) result.get("Term");
		cTerm.toString(); // FIXME: XXX: if this is not here, everything crashes ... nobody knows why
		Map<String, CTerm> map = listAsMap((CTerm) result.get("D"));
		Map<String, String> dictionary = transformDictionary(map);
		cTerm.rename(dictionary);
		return cTerm;

	}
	
	private static Map<String, String> transformDictionary(Map<String, CTerm> map) {
		Map<String, String> newMap = new HashMap<String, String>();
		
		for(String key : map.keySet()) {
			CTerm cTerm = map.get(key);
			newMap.put(cTerm.getFunctorValue(), Util.unquoteAtom(key));
		}
		
		return newMap;
	}
}


