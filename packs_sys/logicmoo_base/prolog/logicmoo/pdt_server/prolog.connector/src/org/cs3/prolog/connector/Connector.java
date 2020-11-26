package org.cs3.prolog.connector;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.cs3.prolog.connector.common.ProcessUtils;
import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.common.Util;
import org.cs3.prolog.connector.internal.process.socket.SocketPrologProcess;
import org.cs3.prolog.connector.process.DefaultStartupStrategy;
import org.cs3.prolog.connector.process.PrologProcess;

/**
 * This class is the main entry point for creating a Prolog process.
 * <p>
 * A process with default settings is created by calling:
 * {@link #newPrologProcess()}
 */
public class Connector {

	// Preferences
    public static final String EP_TRACKERS = "prologContextTracker";
	public static final String PREF_HIDE_PLWIN = "process.hide_plwin";
	public static final String PREF_SERVER_LOGDIR = "process.server_log_dir";
	public static final String PREF_INVOCATION = "process.invocation";
	public static final String PREF_EXECUTABLE = "prolog.connector.executable";
	public static final String PREF_COMMAND_LINE_ARGUMENTS = "process.command.line.arguments";
	public static final String PREF_ENVIRONMENT = "process.environment";
	public static final String PREF_ADDITIONAL_STARTUP = "process.additional.startup";
	public static final String PREF_TIMEOUT = "process.timeout";
	public static final String PREF_HOST = "process.host";
	public static final String PREF_PORT = "process.port";
	
	
    private static final String CONSULT_SERVER_PL = "consult_server.pl";

    /**
	 * Creates a new uninitialized Prolog process.
	 * <p>
	 * For this process, all the settings has to be set manually.
	 * See {@link #newPrologProcess()} for an implementation with
	 * default settings. 
	 * 
	 * @return a new uninitialized Prolog process.
	 */
	public static PrologProcess newUninitializedPrologProcess() {
		return newUninitializedPrologProcess(null);
	}

	/**
	 * Creates a new uninitialized Prolog process.
	 * <p>
	 * For this process, all the settings has to be set manually. See
	 * {@link #newPrologProcess()} for an implementation with default settings.
	 * 
	 * @param name
	 *            the name of the process.
	 * @return a new uninitialized Prolog process with the specified name.
	 */
	public static PrologProcess newUninitializedPrologProcess(String name) {
		SocketPrologProcess socketPrologProcess = new SocketPrologProcess(name);
		try {
			socketPrologProcess.setConsultServerLocation(QueryUtils.prologFileName(Connector.getConsultServerFile()));
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		return socketPrologProcess;
	}

	/**
	 * Creates a new Prolog process with default settings.
	 * <p>
	 * The executable which is used to start the process is provided by the
	 * {@link ProcessUtils#getExecutablePreference()} method.
	 * 
	 * @return a new Prolog process.
	 * @throws IOException
	 */
	public static PrologProcess newPrologProcess() throws IOException {
		return Connector.newPrologProcess(null);
	}

	/**
	 * Creates a new Prolog process with default settings and the provided
	 * executable.
	 * 
	 * @param executable
	 *            path to the Prolog executable.
	 * @return a new Prolog process.
	 * @throws IOException
	 */
	public static PrologProcess newPrologProcess(String executable) throws IOException {
		SocketPrologProcess process = new SocketPrologProcess(null);
		process.setStartupStrategy(new DefaultStartupStrategy());
		process.setOSInvocation(ProcessUtils.getInvocationCommand());
		if (executable == null) {
			process.setExecutablePath(ProcessUtils.getExecutablePreference());
		} else {
			process.setExecutablePath(executable);
		}
		process.setConsultServerLocation(QueryUtils.prologFileName(Connector.getConsultServerFile()));
		process.setHost("localhost");
		process.setTimeout("15000");
		process.setHidePlwin(true);
		process.setUseSessionPooling(true);
		return process;
	}

	private static File consultServerFile = null;
	
	private static File getConsultServerFile() throws IOException {
		if (consultServerFile == null) {
			String tempDir = System.getProperty("java.io.tmpdir");
			InputStream resourceAsStream;
			resourceAsStream = SocketPrologProcess.class.getResourceAsStream(CONSULT_SERVER_PL);
			if (resourceAsStream == null) {
				throw new RuntimeException("Cannot find " + CONSULT_SERVER_PL);
			}
			consultServerFile = new File(tempDir, CONSULT_SERVER_PL);
			if (consultServerFile.exists()) {
				consultServerFile.delete();
			}
			Util.copy(resourceAsStream, new FileOutputStream(consultServerFile));
		}
		return consultServerFile;
	}
}
