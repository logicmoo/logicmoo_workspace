package org.cs3.prolog.connector.process;

import static org.cs3.prolog.connector.common.QueryUtils.bT;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.cs3.prolog.connector.common.QueryUtils;

/**
 * Provides a default implementation for the {@link StartupStrategy}.
 */
public class DefaultStartupStrategy implements StartupStrategy {
	
	private final List<String> loadFileInitStatments = new ArrayList<>();
	private final List<String> fileSearchPathInitStatements = new ArrayList<>(); 

	/**
	 * Adds a file.
	 * 
	 * @param loadFile
	 */
	public void addLoadFile(File loadFile) {
		String cmd = createConsultCommand(loadFile);
		loadFileInitStatments.add(cmd);
	}
	
	/**
	 * Removes a file.
	 * @param loadFile
	 * @return true if this strategy contained the specified file
	 */
	public boolean removeLoadFile(File loadFile) {
		String cmd = createConsultCommand(loadFile);
		return loadFileInitStatments.remove(cmd);
	}
	
	private String createConsultCommand(File loadFile) {
		return "[" + QueryUtils.prologFileNameQuoted(loadFile) + "]";
	}

	/**
	 * Adds a file search path.
	 * @param alias
	 * @param path
	 */
	public void addFileSearchPath(String alias, File path) {
		String cmd = createFileSearchPathCommand(alias, path);
		fileSearchPathInitStatements.add(cmd);
	}
	
	/**
	 * Removes a file search path.
	 * @param alias
	 * @param path
	 * @return true if this strategy contained the specified file search path
	 */
	public boolean removeFileSearchPath(String alias, File path) {
		String cmd = createFileSearchPathCommand(alias, path);
		return fileSearchPathInitStatements.remove(cmd);
	}
	
	private String createFileSearchPathCommand(String alias, File fsp) {
		String term = bT("user:file_search_path", alias, QueryUtils.prologFileNameQuoted(fsp));
		return bT("assertz", term);
	}

	@Override
	public List<String> getFileSearchPathInitStatements() {
		return fileSearchPathInitStatements;
	}

	@Override
	public List<String> getLoadFileInitStatements() {
		return loadFileInitStatments;
	}

}
