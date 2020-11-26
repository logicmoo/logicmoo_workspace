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

import java.math.BigInteger;

import org.cs3.prolog.connector.internal.cterm.parser.ASTNode;

/**
 * Represents an integer.
 */
public class CInteger extends CTerm{
	
	private BigInteger bigInt;
	
	public CInteger(ASTNode node) {
		super(node);
		bigInt = new BigInteger(getFunctorValue());
	}
	
	public int getIntValue() {
		int intValue = bigInt.intValue();
		
		if (bigInt.compareTo(BigInteger.valueOf(intValue)) == 0) {
			return intValue;
		} else {
			throw new NumberFormatException("Value is to large to fit in an int (" + bigInt + ")");
		}
	}
	
	public long getLongValue() {
		long longValue = bigInt.longValue();
		
		if (bigInt.compareTo(BigInteger.valueOf(longValue)) == 0) {
			return longValue;
		} else {
			throw new NumberFormatException("Value is to large to fit in a long (" + bigInt + ")");
		}
	}
	
	public BigInteger getBigIntegerValue() {
		return new BigInteger(getFunctorValue());
	}

}


