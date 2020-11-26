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

import static org.hamcrest.CoreMatchers.allOf;
import static org.hamcrest.CoreMatchers.anyOf;

import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;
import org.hamcrest.Factory;
import org.hamcrest.Matcher;
public class CombinableMatcher<T> extends BaseMatcher<T> {

	private final Matcher<? super T> fMatcher;

	public CombinableMatcher(Matcher<? super T> matcher) {
		fMatcher= matcher;
	}

	@Override
	public boolean matches(Object item) {
		return fMatcher.matches(item);
	}

	@Override
	public void describeTo(Description description) {
		description.appendDescriptionOf(fMatcher);
	}

	@Factory
	public CombinableMatcher<T> and(Matcher<? super T> matcher) {
		return new CombinableMatcher<T>(allOf(matcher, fMatcher));
	}

	@Factory
	public CombinableMatcher<T> or(Matcher<? super T> matcher) {
		return new CombinableMatcher<T>(anyOf(matcher, fMatcher));
	}
}


