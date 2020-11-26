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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

/**
 * Contains static methods that do not quite fit anywhere else.
 */
public class Util {
	
	/**
	 * Converts a logical character offset to a physical character offset. E.g.
	 * prolog uses logical offsets in the sense that it counts any line
	 * delimiter as a single character, even if it is CRLF, etc.
	 * <p>
	 * Eclipse documents and views however seem to count physical characters,
	 * i.e. the CRLF line delimiter would count as two characters.
	 * 
	 * @param data the text
	 * @param logical the logical offset
	 * @return the physical offset
	 * @see #physicalToLogicalOffset(String, int)
	 */
	public static int logicalToPhysicalOffset(String data, int logical) {
		int physical = 0;
		int nextPos = data.indexOf("\r\n");
		while (nextPos >= 0 && nextPos < logical) {
			physical += (nextPos + 2);
			logical -= (nextPos + 1);
			data = data.substring(nextPos + 2);
			nextPos = data.indexOf("\r\n");
		}
		return physical + logical;
	}
	
	
	/**
	 * @param data the text
	 * @param physical the physical offset
	 * @return the logical offset
	 * @see #logicalToPhysicalOffset(String, int)
	 */
	public static int physicalToLogicalOffset(String data, int physical) {
		int logical = 0;
		int nextPos = data.indexOf("\r\n");
		while (nextPos >= 0 && nextPos < physical) {
			physical -= (nextPos + 2);
			logical += (nextPos + 1);
			data = data.substring(nextPos + 2);
			nextPos = data.indexOf("\r\n");
		}
		return physical + logical;
	}

	/**
	 * Getting the lock file for starting a socket server.
	 * 
	 * @return the lock file
	 */
	public static File getLockFile() {
		String tmpdir = System.getProperty("java.io.tmpdir");
		return new File(tmpdir, generateFingerPrint());
	}

	private static String generateFingerPrint() {
		long l = System.currentTimeMillis();
		double m = Math.random();
		return "fp_" + l + "_" + m;
	}

	/**
	 * Pretty print of a Map.
	 * 
	 * @param input the map that should be printed
	 * @return the String representation
	 */
	public static String prettyPrint(Map<String, ?> input) {
		if (input != null) {
			boolean first = true;
			StringBuffer result = new StringBuffer();
			Set<String> keys = input.keySet();
			Iterator<String> it = keys.iterator();
			while (it.hasNext()) {
				if (!first) {
					result.append(", ");
				}
				String key = it.next();
				Object value = input.get(key);
				String valueAsString = value.toString();
				result.append(key + "-->" + valueAsString);
				first = false;

			}
			return result.toString();
		}
		return "";
	}
	
	/**
	 * Pretty print of an array.
	 *  
	 * @param a the array that should be printed
	 * @return the String representation
	 */
	public static String prettyPrint(Object[] a) {
		if (a == null) {
			return "";
		}
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < a.length; i++) {
			if (i > 0) {
				sb.append(", ");
			}
			sb.append(a[i].toString());
		}
		return sb.toString();
	}
	
	/**
	 * Pretty print of a collection.
	 *  
	 * @param input the collection that should be printed
	 * @return the String representation
	 */
	public static String prettyPrint(Collection<?> input) {
		if (input != null && !input.isEmpty()) {
			Iterator<?> it = input.iterator();
			return concatinateElements(it);
		}
		return "";
	}

	
	private static String concatinateElements(Iterator<?> it) {
		StringBuffer sb = new StringBuffer();
		boolean first = true;
		while ( it.hasNext()) {
			if (!first) {
				sb.append(", ");
			}
			Object next = it.next();
			String elm = next == null ? "<null>" : next.toString();
			sb.append(elm);
			first = false;
		}
		return sb.toString();
	}

	/**
	 * Get the logfile (create if necessary).
	 * 
	 * @param dir path to the directory
	 * @param name filename
	 * @return the logFile
	 * @throws IOException
	 */
	public static File getLogFile(String dir, String name) throws IOException {
		File logFile = new File(dir,name);
				
		if (!logFile.exists()) {
			logFile.getParentFile().mkdirs();
			logFile.createNewFile();
		}
		return logFile.getCanonicalFile();
	}

	/**
	 * Copy an InputStream to an OutputStream.
	 * 
	 * @param in
	 * @param out
	 * @throws IOException
	 */
	public static void copy(InputStream in, OutputStream out)
			throws IOException {
		BufferedInputStream bIn = null;
		BufferedOutputStream bOut = null;
		try {
			bIn = new BufferedInputStream(in);
			bOut = new BufferedOutputStream(out);
			byte[] buf = new byte[255];
			int read = -1;
			while ((read = bIn.read(buf)) > -1) {
				out.write(buf, 0, read);
			}
		} finally {
			bOut.flush();
		}
	}

	/**
	 * Checks if current OS is Windows.
	 * 
	 * @return true if current OS is Windows
	 */
	public static boolean isWindows() {
		boolean windowsPlattform = System.getProperty("os.name").indexOf("Windows") > -1;
		return windowsPlattform;
	}

	/**
	 * Checks if current OS is MacOS.
	 * 
	 * @return true if current OS is MacOS
	 */
	public static boolean isMacOS() {
		boolean mac = System.getProperty("os.name").indexOf("Mac") > -1;
		return mac;
	}

	/**
	 * Read InputStream to String.
	 * 
	 * @param in the InputStream
	 * @return String representation
	 * @throws IOException
	 */
	public static String readInputStreamToString(InputStream in) throws IOException {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		byte[] buf = new byte[1024];
		int read = in.read(buf);
		while (read > 0) {
			out.write(buf, 0, read);
			read = in.read(buf);
		}
		return out.toString();
	}

	/**
	 * Escapes special characters in a String.
	 * @param s the input string
	 * @return the output String
	 */
	public static String escape(String s) {
		StringBuffer result = new StringBuffer(s.length() + 10);
		for (int i = 0; i < s.length(); ++i) {
			char c = s.charAt(i);
			switch (c) {
			case '<':
				result.append("&lt;"); //$NON-NLS-1$
				break;
			case '>':
				result.append("&gt;"); //$NON-NLS-1$
				break;
			case '"':
				result.append("&quot;"); //$NON-NLS-1$
				break;
			case '\'':
				result.append("&apos;"); //$NON-NLS-1$
				break;
			case '&':
				result.append("&amp;"); //$NON-NLS-1$
				break;
			case '{':
				result.append("&cbo;"); //$NON-NLS-1$
				break;
			case '}':
				result.append("&cbc;"); //$NON-NLS-1$
				break;
			default:
				result.append(c);
				break;
			}
		}
		return result.toString();
	}

	public static String unescapeBuffer(StringBuffer line) {
		return unescape(line.toString(),0,line.length());
	}
	
	private static String unescape(String line, int start, int end) {
		StringBuffer sb = new StringBuffer();
		boolean escape = false;
		StringBuffer escBuf = new StringBuffer();
		for (int i = start; i < end; i++) {
			char c = line.charAt(i);
			switch (c) {
			case '&':
				escape = true;
				break;
			case ';':
				if (escape) {
					escape = false;
					String escSeq = escBuf.toString();
					escBuf.setLength(0);
					if ("lt".equals(escSeq)) {
						sb.append('<');
					} else if ("gt".equals(escSeq)) {
						sb.append('>');
					} else if ("cbo".equals(escSeq)) {
						sb.append('{');
					} else if ("cbc".equals(escSeq)) {
						sb.append('}');
					} else if ("amp".equals(escSeq)) {
						sb.append('&');
					} else if ("apos".equals(escSeq)) {
						sb.append('\'');
					} else if ("quot".equals(escSeq)) {
						sb.append('\"');
					}
				} else {
					sb.append(c);
				}
				break;
			default:
				if (escape) {
					escBuf.append(c);
				} else {
					sb.append(c);
				}
				break;
			}
		}
		return sb.toString();
	}

	/**
	 * Find a free port for communication with Prolog.
	 * 
	 * @return free port number
	 * @throws IOException
	 */
	public static int findFreePort() throws IOException {
		ServerSocket ss = new ServerSocket(0);
		int port = ss.getLocalPort();
		ss.close();
		return port;
	}

	public static String splice(Collection<String> c, String delim) {
		if (c != null && !c.isEmpty()) {
			StringBuffer sb = new StringBuffer();
			for (Iterator<String> it = c.iterator(); it.hasNext();) {
				Object next = it.next();
				sb.append(next);
				if (it.hasNext()) {
					sb.append(delim);
				}
			}
			return sb.toString();
		}
		
		return "";
	}
	
	/**
	 * Unquote atom (replace ' at the start and end).
	 * 
	 * @param atom quoted atom
	 * @return unquoted atom
	 */
	public static String unquoteAtom(String atom) {
		atom = atom.trim();
		if (atom.length() == 0 || atom.charAt(0) != '\'') {
			return atom;
		}
		atom = atom.substring(1, atom.length() - 1);
		StringBuffer sb = new StringBuffer();

		for (int i = 0; i < atom.length(); i++) {
			char c = atom.charAt(i);
			if (c == '\\') {
				int len = appendUnescapedChar(atom, i, sb);
				i += len - 1;
			} else {
				sb.append(c);
			}
		}
		return sb.toString();
	}
	
	/**
	 * Unquote String or atom (replace " or ' at the start and end).
	 * 
	 * @param atom quoted atom
	 * @return unquoted atom
	 */
	public static String unquoteStringOrAtom(String atom) {
		atom = atom.trim();
		if (atom.length() == 0){
			return atom;
		}
		if( atom.charAt(0) == '\"' || atom.charAt(0) == '\'') {
			atom = atom.substring(1, atom.length() - 1);
		}
		StringBuffer sb = new StringBuffer();

		for (int i = 0; i < atom.length(); i++) {
			char c = atom.charAt(i);
			if (c == '\\') {
				int len = appendUnescapedChar(atom, i, sb);
				i += len - 1;
			} else {
				sb.append(c);
			}
		}
		return sb.toString();
	}

	private static int appendUnescapedChar(String image, int i, StringBuffer sb) {
		if (image.length() <= i + 1) {
			sb.append('\\');
			return 1;
		}
		char c = image.charAt(i + 1);
		if (Character.isDigit(c)) {
			return appendUnescapedOctalCharSpec(image, i, sb);
		}
		switch (c) {
		case 'a':
			// sb.append('\a'); there is no bell char in java
			return 2;
		case 'b':
			sb.append('\b');
			return 2;
		case 'c':
			// sb.append('\c'); not supported
			return 2;
		case '\n':
			// ignoring
			return 2;
		case 'f':
			sb.append('\f');
			return 2;
		case 'n':
			sb.append('\n');
			return 2;
		case 'r':
			sb.append('\r');
			return 2;
		case 't':
			sb.append('\t');
			return 2;
		case 'v':
			// sb.append('\v'); vertical tabs are not supported in java
			return 2;
		case 'x':
			return appendUnescapedHexCharSpec(image, i, sb);
		case '\\':
			sb.append('\\');
			return 2;
		case '\'':
			sb.append('\'');
			return 2;
		default:
			sb.append('\\');
			return 1;
//			sb.append(c);
//			return 2;
		}
	}

	private static int appendUnescapedOctalCharSpec(String image, int i,
			StringBuffer sb) {
		String val = "";
		int j = i + 1;
		while (j < image.length() && j < i + 4 && isOctDigit(image.charAt(j))) {
			val += image.charAt(j);
			j++;
		}
		sb.append((char) Integer.parseInt(val, 8));
		if (j < image.length() && image.charAt(j) == '\\') {
			return 1 + j - i;
		}
		return j - i;
	}

	private static int appendUnescapedHexCharSpec(String image, int i,
			StringBuffer sb) {

		String val = "";
		int j = i + 2;
		while (j < image.length() && j < i + 4 && isHexDigit(image.charAt(j))) {
			val += image.charAt(j);
			j++;
		}
		sb.append((char) Byte.parseByte(val));
		if (j < image.length() && image.charAt(j) == '\\') {
			return 1 + j - i;
		}
		return j - i;
	}

	private static boolean isHexDigit(char c) {

		return Character.isDigit(c) || 'a' <= Character.toLowerCase(c)
				&& Character.toLowerCase(c) <= 'f';
	}

	private static boolean isOctDigit(char c) {

		return Character.isDigit(c) || '0' <= c && c <= '7';
	}

	/**
	 * Splits a String by the given delimiter and adds the parts to the
	 * specified Collection.
	 * 
	 * This method can NOT be replaced by String.split because the results
	 * differ (empty strings, trim).
	 * 
	 * @param string
	 *            the String to split
	 * @param delimiter
	 *            the delimiter
	 * @param results
	 *            Collection where the parts are added to
	 */
	public static void split(String string, String delimiter, Collection<String> results) {
		if (string == null) {
			return;
		}
		int i = -1;
		while ((i = string.indexOf(delimiter, 0)) >= 0) {
			results.add(string.substring(0, i).trim());
			string = string.substring(i + delimiter.length());
		}
		String rest = string.trim();
		if (rest.length() > 0) {
			results.add(rest);
		}

	}

	/**
	 * Splits a String by the given delimiter.
	 * 
	 * This method can NOT be replaced by String.split because the results
	 * differ (empty strings, trim).
	 * 
	 *  @param string
	 *            the String to split
	 * @param delimiter
	 *            the delimiter
	 * @return an array with the parts of the string.
	 */
	public static String[] split(String string, String delimiter) {
		Vector<String> v = new Vector<String>();
		split(string, delimiter, v);
		return v.toArray(new String[v.size()]);
	}
	
	public static String splice(Object[] c, String delim) {
		if (c != null && c.length > 0) {
			StringBuffer sb = new StringBuffer();
			for (int i = 0; i < c.length; i++) {
				if (i > 0) {
					sb.append(delim);
				}
				Object next = c[i];
				sb.append(next);

			}
			return sb.toString();
		}
		return "";
	}

	public static boolean flagsSet(int flags, int set) {
		return (flags & set) == set;
	}
	
	/**
	 * Reads text from file to String.
	 * 
	 * @param f
	 *            the input file
	 * @return content of the file
	 */
	public static String readFromFile(File f) {
		StringBuffer buf = new StringBuffer();
		BufferedReader bufferedReader = null;
		try {
			bufferedReader = new BufferedReader(new FileReader(f));
			String line;
			while ((line = bufferedReader.readLine()) != null) {
				buf.append(line + "\n");
			}
		} catch (Exception e) {
		} finally {
			if (bufferedReader != null) {
				try {
					bufferedReader.close();
				} catch (IOException ioe) {
				}
			}
		}
		return buf.toString();
	}
	
	private static Set<File> tempFiles = new HashSet<File>();
	
	public static void addTempFile(File tempFile) {
		if (tempFile != null) {
			tempFiles.add(tempFile);
		}
	}
	
	public static Set<File> getTempFiles() {
		return new HashSet<File>(tempFiles);
	}
}

