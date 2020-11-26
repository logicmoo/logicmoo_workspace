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

import java.math.BigDecimal;

import org.cs3.prolog.connector.internal.cterm.parser.ASTNode;

/**
 * Represents a floating point number.
 */
public class CFloat extends CTerm {
	
	private BigDecimal bigDec;
	
	public CFloat(ASTNode node) {
		super(node);
		bigDec = new BigDecimal(getFunctorValue());
	}
	
	public float getFloatValue() {
		return bigDec.floatValue();
	}
	
	public double getDoubleValue() {
		return bigDec.doubleValue();
	}
	
	public BigDecimal getBigDecimalValue() {
		return new BigDecimal(getFunctorValue());
	}
}


