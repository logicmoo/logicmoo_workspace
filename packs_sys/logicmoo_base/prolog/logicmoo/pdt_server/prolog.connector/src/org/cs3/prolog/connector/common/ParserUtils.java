package org.cs3.prolog.connector.common;

/**
 * Contains utility methods for parsing Prolog code.
 *
 */
public class ParserUtils {

	/**
	 * Checks if c may be part of a Prolog variable.
	 * 
	 * @param c the character
	 * @return true if c may be part of a Prolog variable;<br>false otherwise
	 */
	public static boolean isVarChar(char c) {
		if (c == '_')
			return true;
		if (c >= 'A' && c <= 'Z')
			return true;
		if (c >= 'a' && c <= 'z')
			return true;
		if (c >= '0' && c <= '9')
			return true;
		return false;
	}

	/**
	 * Checks if a Prolog variable may start with c.
	 * 
	 * This is equivalent to checking if c is an upper case character or
	 * underscore.
	 * 
	 * @param c
	 *            the character
	 * @return true if a Prolog variable may start with c;<br>false otherwise.
	 */
	public static boolean isVarPrefix(char c) {
		return (Character.isUpperCase(c) || c == '_');
	}

	/**
	 * Checks if a Prolog functor may start with the specified prefix.
	 * 
	 * This is equivalent to checking if prefix starts with a lower case
	 * character.
	 * 
	 * @param prefix
	 * @return true if a Prolog functor may start with prefix;<br>false otherwise.
	 */
	public static boolean isFunctorPrefix(String prefix) {
		if (prefix == null | prefix.length() == 0)
			return false;
		if (prefix.charAt(0) >= 'a' && prefix.charAt(0) <= 'z')
			return true;
	
		return false;
	}

	/**
	 * Checks if a Prolog variable may start with the specified prefix.
	 * 
	 * This is equivalent to checking if prefix starts with an upper case
	 * character or underscore.
	 * 
	 * @param prefix
	 * @return true if a Prolog variable may start with prefix;<br>false otherwise.
	 */
	public static boolean isVarPrefix(String prefix) {
		if (prefix.length() == 0)
			return false;
		return isVarPrefix(prefix.charAt(0));
	}

	/**
	 * Checks if c is a valid character as part of a Prolog predicate name that
	 * is NOT enclosed in simple quotes.
	 * 
	 * @param c
	 *            the character
	 * @return true if c is a valid character as part of a Prolog predicate
	 *         name;<br>
	 *         false otherwise.
	 */
	static public boolean isNormalPredicateNameChar(char c) {
		if (c >= 'a' && c <= 'z') return true;
		if (c >= '0' && c <= '9') return true;
		if (c >= 'A' && c <= 'Z') return true;
		if (c == '_' || c == ':')  return true;
		return false;
	}
	
	public static boolean isNonQuotedPredicateNameChar(char c) {
		if (c >= 'a' && c <= 'z') return true;
		if (c >= '0' && c <= '9') return true;
		if (c >= 'A' && c <= 'Z') return true;
		if (c == '_')  return true;
		return false;
	}

	/**
	 * Checks if c is a character that may be contained in a Prolog predicate
	 * name that IS enclosed in simple quotes.
	 * 
	 * @param c
	 *            the character
	 * @return true if c is a character that may be contained in a Prolog
	 *         predicate name that IS enclosed in simple quotes;<br>
	 *         false otherwise.
	 */
	static public boolean isSpecialPredicateNameChar(char c) {
		return (c == '\''  
			 || c == '\\'
			 || c == '.' 
			 || c == '+' 
			 || c == '-' 
			 || c == '*' 
	         || c == '$'
	    // TODO: add all the other special characters!
		);
	}

	/**
	 * Checks if c is a valid character as part of a Prolog predicate name
	 * (including module definition).
	 * 
	 * @param c
	 *            the character
	 * @return true if c is a valid character as part of a Prolog predicate
	 *         name;<br>
	 *         false otherwise.
	 */
	public static boolean isPredicateNameChar(char c) {
		return (isNormalPredicateNameChar(c) || isSpecialPredicateNameChar(c));
	}

	public static boolean isNonQualifiedPredicateNameChar(char c) {
		return isPredicateNameChar(c) && c != ':';
	}
	
	public static boolean isSingleSecondChar(char c) {
		if (c >= '0' && c <= '9')
			return true;
		if (c >= 'A' && c <= 'Z')
			return true;
		return false;
	}

}
