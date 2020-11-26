package org.cs3.prolog.connector.common;

import java.util.Map;

import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;

/**
 * Contains helper methods to determine Prolog flags.
 */
public class PrologFlags {
	
	/**
	 * Returns the version of Prolog which is currently used by the given process. 
	 * 
	 * @param process the current Prolog process
	 * @return String Version of Prolog
	 * @throws PrologProcessException in case of connection problems
	 */
	public static String getPrologVersion(PrologProcess process) throws PrologProcessException {
		return getCurrentPrologFlag(process, "version");
	}

	/**
	 * Returns the Prolog dialect which is currently used by the given process. 
	 * 
	 * @param process the current Prolog process
	 * @return String Prolog dialect
	 * @throws PrologProcessException in case of connection problems
	 */
	public static String getPrologDialect(PrologProcess process) throws PrologProcessException {
		return getCurrentPrologFlag(process, "dialect");
	}

	/**
	 * Returns the address size of the hosting machine. Typically 32 or 64.
	 * 
	 * @param process the current Prolog process
	 * @return String Prolog dialect
	 * @throws PrologProcessException in case of connection problems
	 */
	public static String getPrologAddressBits(PrologProcess process) throws PrologProcessException {
		return getCurrentPrologFlag(process, "address_bits");
	}
	
	/**
	 * Returns the value for the given flag on the specified Prolog process.
	 * 
	 * @param process the current Prolog process
	 * @param key prolog flag
	 * @return value of the flag, null if the flag is unknown
	 * @throws PrologProcessException in case of connection problems
	 */
	public static String getCurrentPrologFlag(PrologProcess process, String key) throws PrologProcessException {
		Map<String, Object> result = process.queryOnce(QueryUtils.bT("current_prolog_flag", key, "Value"));
		Object value = result.get("Value");
		if (value == null) {
			// no result = flag is unknown
			return null;
		}
		return value.toString();
	}
}
