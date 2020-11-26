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

package org.cs3.plunit.matcher;

import static org.hamcrest.core.IsEqual.equalTo;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.hamcrest.Description;
import org.hamcrest.Factory;
import org.hamcrest.Matcher;
import org.hamcrest.TypeSafeMatcher;
import org.hamcrest.collection.IsCollectionWithSize;
import org.hamcrest.collection.IsIterableContainingInOrder;

public class PrologResultsInAnyOrderMatcher extends TypeSafeMatcher<List<List<String>>> {
	
	
	private List<List<Matcher>> matchers;
	private boolean[] lineNumbers;
	private ArrayList<Matcher> lines;

	public PrologResultsInAnyOrderMatcher() {
		
	}
	
    public PrologResultsInAnyOrderMatcher(List<List<Matcher>> matchers) {
		this.matchers = matchers;
		this.lines = new ArrayList<Matcher>();
		for (List<Matcher> list : matchers) {
			lines.add(new IsIterableContainingInOrder(list));
		}
		lineNumbers=new boolean[matchers.size()];
		for (int i=0;i<matchers.size();i++){ 
			lineNumbers[i]=false;
		}
		
	}

	

	@Override
	protected boolean matchesSafely(List<List<String>> item) {
		if(item.size() < matchers.size())
			return false;
		
		for (List<String> list : item) {
			matchLine(list);
		}
		
		for (boolean bool : lineNumbers) {
			if(bool == false){
				return false;
			}
			
		}
		return true;
	}

	private void matchLine(List<String> singleList) {
		for(int j=0;j<matchers.size();j++){
				if(lines.get(j).matches(singleList)){
						lineNumbers[j]=true;
				}
		}
	}

	@Override
	public void describeTo(Description description) {
		description.appendValueList("[", ",", "]", matchers);
		
	}


	
	
	/**
     *  Matches if all arguments are contained in any order in the iterable
     */
	@Factory
	public static  Matcher<List<List<String>>> has(String[]... input) {
		List<List<Matcher>> allMatchers = new ArrayList<List<Matcher>>();
		for (String[] line : input) {
			List<Matcher> lineMatcher = new ArrayList<Matcher>();
			for (String item : line) {
				lineMatcher.add(equalTo(item));
			}
			allMatchers.add(lineMatcher);
		}
		return new PrologResultsInAnyOrderMatcher(allMatchers);
	}

	/**
     *  Matches if all arguments are contained in any order in the iterable
     */
	@Factory
	public static  Matcher<List<List<String>>> has(Matcher[]... matchers) {
		List<List<Matcher>> allMatchers = new ArrayList<List<Matcher>>();
		for (Matcher[] line : matchers) {
			List<Matcher> lineMatcher = Arrays.asList(line);
			allMatchers.add(lineMatcher);
		}
		return new PrologResultsInAnyOrderMatcher(allMatchers);
	} 
	/**
	 * matches if the Iterable<String> object has size n;
	 * @param size
	 */
	@Factory
	public	static <E> Matcher<? super List<E>> hasAnswers(int n) {
		return IsCollectionWithSize.<E>hasSize(n);
	}
	
}



