package org.cs3.prolog.connector.process;

import java.util.List;

/**
 * Provides the possibility to automatically load files and create file search
 * paths when starting a {@link PrologProcess}.
 */
public interface StartupStrategy {

	/**
	 * Returns the file search path init statements.
	 * 
	 * The elements in this list have the following structure:
	 * <pre>
	 * assertz(file_search_path(&lt;alias&gt;, &lt;path&gt;))
	 * </pre>
	 * 
	 * @return a list of file search path init statements.
	 */
	public List<String> getFileSearchPathInitStatements();

	/**
	 * Returns the load file init statements.
	 * 
	 * The elements in this list have the following structure:
	 * <pre>
	 * [&lt;path&gt;]
	 * </pre>
	 * 
	 * @return a list of load file init statements.
	 */
	public List<String> getLoadFileInitStatements();

}
