package org.cs3.plbenchmarks;

public class Logger {
	
	public static boolean loggingEnabled = true;
	
	public static void log(Object o) {
		if (loggingEnabled) {
			System.out.println(o);
		}
	}
}
