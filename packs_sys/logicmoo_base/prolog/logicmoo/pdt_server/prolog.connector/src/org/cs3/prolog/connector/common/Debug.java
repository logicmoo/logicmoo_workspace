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

package org.cs3.prolog.connector.common;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * Provides a application-wide mechanism to send debug, or other informational
 * Messages. Unless specified otherwise, all Messages will be send to the
 * standart error stream. This class examines the System Property
 * <u>org.cs3.pl.jtransformer.debug_level </u> which should take one of the
 * following values:
 * <ul>
 * <li>NONE - No console output at all</li>
 * <li>ERROR - Only serious errors should be reported</li>
 * <li>WARNING - Less serious error conditions should be reported</li>
 * <li>INFO - Informational Messages about program operation should be printed</li>
 * <li>DEBUG - Insanely verbose debug output.</li>
 * </ul>
 */
public class Debug {
	final public static int LEVEL_NONE = 0;
	final public static int LEVEL_ERROR = 1;
	final public static int LEVEL_WARNING = 2;
	final public static int LEVEL_INFO = 3;
	final public static int LEVEL_DEBUG = 4;
	static private int debugLevel = LEVEL_ERROR;	
	private static String PREFIX_OUTPUT = "pdt: ";
	static private String outputDir;
	static private PrintStream outputLogFilePrintStream; 
	static private PrintStream out = System.err;
	static private SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM HH:mm:ss");

	/**
	 * Sets the debug level.
	 * 
	 * @param s
	 *            one of
	 *            <ul>
	 *            <li>NONE - No console output at all</li>
	 *            <li>ERROR - Only serious errors should be reported</li>
	 *            <li>WARNING - Less serious error conditions should be reported
	 *            </li>
	 *            <li>INFO - Informational Messages about program operation
	 *            should be printed</li>
	 *            <li>DEBUG - Insanely verbose debug output.</li>
	 *            </ul>
	 * 
	 */
	static public void setDebugLevel(String s) {
		System.out.println(PREFIX_OUTPUT + "set debug level: " + s);
		if (s.equalsIgnoreCase("NONE"))
			debugLevel = LEVEL_NONE;
		else if (s.equalsIgnoreCase("ERROR"))
			debugLevel = LEVEL_ERROR;
		else if (s.equalsIgnoreCase("WARNING"))
			debugLevel = LEVEL_WARNING;
		else if (s.equalsIgnoreCase("INFO"))
			debugLevel = LEVEL_INFO;
		else if (s.equalsIgnoreCase("DEBUG"))
			debugLevel = LEVEL_DEBUG;
		else {
			debugLevel = LEVEL_ERROR;
			error("Invalid debug level specified, set debug level to ERROR");
		}
	}

	/**
	 * Changes the default output.
	 * 
	 * @param output
	 *            one of
	 *            <ul>
	 *            <li>LOGFILE</li>
	 *            <li>CONSOLE</li>
	 *            </ul>
	 */
	static public void setOutputTo(String output) {		
		if (output.equalsIgnoreCase("LOGFILE") ) {
			if (outputLogFilePrintStream!=null) {
				setOutputStream(outputLogFilePrintStream);
				out.println(PREFIX_OUTPUT + "set debug output to " + output);
			} else {
				setOutputStream(System.err);
				out.println(PREFIX_OUTPUT + "set debug output NOT to "+ output+" because LogFileStream/File invalid, please check preferences");
				out.println(PREFIX_OUTPUT + "set debug output to CONSOLE");
			}
		} else if (output.equalsIgnoreCase("CONSOLE")) {
			setOutputStream(System.err);
			out.println(PREFIX_OUTPUT + "set debug output to " + output);
		}
	}	
	
	/**
	 * Sets the directory for the logfile.
	 * 
	 * @param logFileDir
	 *            directory for the logile.
	 * @throws FileNotFoundException
	 */
	static public void setLogDir(String logFileDir) throws FileNotFoundException {
		outputDir = logFileDir;
		if (outputDir != null && !outputDir.equals("")) {
			File logFile = new File(outputDir, "pdt.log");
			System.out.println(PREFIX_OUTPUT + "debug output is written to: " + logFile.getAbsolutePath());
			BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(new FileOutputStream(logFile, true));
			outputLogFilePrintStream = new PrintStream(bufferedOutputStream);
			setOutputTo("LOGFILE");
		} else {
			setOutputTo("CONSOLE");
		}
	}


	/**
	 * sets the level of reporting to the specified constant. Only the LEVEL_*
	 * constants declared in this class are valid argument.
	 * 
	 * @param level
	 *            a LEVEL_* constant
	 */
	static public void setDebugLevel(int level) {
		if (level < LEVEL_NONE || level > LEVEL_DEBUG)
			throw new IllegalArgumentException("Invalid debug level: " + level);
		debugLevel = level;
	}

	/**
	 * Sends an informational message to be considered for output. It is printed
	 * if the current debug level is equal or greater to LEVEL_INFO
	 * 
	 * @param msg
	 *            the message
	 */
	public static void info(String msg) {
		write(LEVEL_INFO, msg);
	}

	/**
	 * Sends an warning message to be considered for output. It is printed if
	 * the current debug level is equal or greater to LEVEL_WARNING
	 * 
	 * @param msg
	 *            the message
	 */
	public static void warning(String msg) {
		write(LEVEL_WARNING,msg);
	}

	/**
	 * sends a debug-level Message to be considered for output. It is printed if
	 * the current debug level is equal to LEVEL_DEBUG
	 * 
	 * @param msg
	 *            the message
	 */
	public static void debug(String msg) {
		write(LEVEL_DEBUG, msg);
	}

	/**
	 * Sends an error message to be considered for output. It is printed if the
	 * current debug level is not set to LEVEL_NONE
	 * 
	 * @param msg
	 *            the message
	 */
	public static void error(String msg) {
		write(LEVEL_ERROR,msg);
	}

	/**
	 * reports an Error or an Exception. Exceptions are treated as
	 * Warning-level, while Errors are considered just that, Errors.
	 * 
	 * @param t
	 *            a throwable object
	 */
	public static void report(Throwable t) {
		if (debugLevel == LEVEL_NONE)
			return;
		if (t instanceof Error && debugLevel != LEVEL_NONE) {
			write(LEVEL_ERROR, PREFIX_OUTPUT + "The following Error was caught:");
			t.printStackTrace(out);
		} else if (debugLevel >= LEVEL_ERROR) {
			write(LEVEL_WARNING, PREFIX_OUTPUT + "The following Exception was caught:");
			t.printStackTrace(out);
		}
		out.flush();
	}

	private static void setOutputStream(PrintStream out) {
		if (out == null)
			throw new IllegalArgumentException("Output stream is null");
		Debug.out = out;
		out.println(PREFIX_OUTPUT + "----------------------------------");
		out.println(PREFIX_OUTPUT + new Date());
		out.println(PREFIX_OUTPUT + "----------------------------------");
	}

	private static void write(int level, String msg) {
		if (level > debugLevel)
			return;
		String prefix;
		switch (level) {
		case LEVEL_DEBUG:
			prefix = "DEBUG";
			break;
		case LEVEL_WARNING:
			prefix = "WARNING";
			break;
		case LEVEL_ERROR:
			prefix = "ERROR";
			break;
		case LEVEL_INFO:
			prefix = "INFO";
			break;
		default:
			throw new IllegalStateException(PREFIX_OUTPUT + "Invalid debug level: " + level);
		}
		Thread currentThread = Thread.currentThread();
		String nameOfCurrentThread = currentThread.getName();
		
		out.println(PREFIX_OUTPUT + prefix + " - " + dateFormat.format(new Date()) + " - " + nameOfCurrentThread + ": "+ msg);
		out.flush();
	}

	public static void rethrow(String string, Throwable e) {
		warning("wrapping an exception:" + string);
		report(e);
		throw new RuntimeException(string, e);

	}

	public static void rethrow(Throwable e) {
		warning("wrapping an exception");
		report(e);
		throw new RuntimeException(e);
	}
	
	/**
	 * Returns a limited number of latest log file entries.
	 * 
	 * @param entryCount
	 *            number of entries for the output
	 * @return the specified number of entries
	 */
	public static String getTailOfLogFile(int entryCount) {
		File logFile = new File(outputDir, "pdt.log");
		if (!logFile.exists()) {
			return "";
		}
		String logFileString = Util.readFromFile(logFile);
		String[] logFileContent = logFileString.split("\npdt:");
		if (logFileContent.length > entryCount) {
			StringBuffer buf = new StringBuffer();
			for (int i=logFileContent.length - entryCount; i<logFileContent.length; i++) {
				buf.append(logFileContent[i] + "\n");
			}
			return buf.toString();
		} else {
			return logFileString;
		}
	}
	
}


