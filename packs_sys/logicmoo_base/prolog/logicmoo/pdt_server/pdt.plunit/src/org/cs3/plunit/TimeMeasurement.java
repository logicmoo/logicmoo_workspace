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

package org.cs3.plunit;

/**
 * @author Tobias Rho
 * 
 */
public class TimeMeasurement
{

	private long ms = 0;

	String name;

	private boolean printTimeMeasuring;

	public TimeMeasurement(String name, boolean printTimeMeasuring)
	{
		this.name = name;
		ms = System.currentTimeMillis();
		this.printTimeMeasuring = printTimeMeasuring;
		if (printTimeMeasuring)
			println("start " + name + " ...");
	}

	public void println(String s)
	{
		System.out.println(s);
	}

	public void print(String s)
	{
		System.out.print(s);
	}

	public long getTimeDiff()
	{
		long diff = System.currentTimeMillis() - ms;
		if (printTimeMeasuring)
		{
			print("end   " + name + "\t\t");
			println(diff / 1000 + "." + diff % 1000 + " sec");
		}
		return diff;
	}

}


