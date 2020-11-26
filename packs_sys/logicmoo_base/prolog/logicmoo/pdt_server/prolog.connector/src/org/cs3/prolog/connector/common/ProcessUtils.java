package org.cs3.prolog.connector.common;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;

/**
 * Contains utility methods to determine default settings.
 */
public class ProcessUtils {
	
	private static final String EMPTY_STRING = "";

	public static String guessEnvironmentVariables() {
		if (Util.isMacOS()) {
			String home = System.getProperty("user.home");
			return "DISPLAY=:0.0, HOME=" + home;
		}
		return "";
	}

	public static String getInvocationCommand() {
		if (Util.isWindows()) {
			return "cmd.exe /c start \"cmdwindow\" /min ";
		} else {
			return "";
		}
	}

	public static String getExecutablePreference() {
		return ProcessUtils.getExecutablePreference(PDTConstants.DIALECT_SWI);
	}
	
	public static String getExecutablePreference(String dialect) {
		return getExecutablePreference(dialect, null);
	}
	
	public static String getExecutablePreference(String dialect, File localPrologDirectory) {
		if (localPrologDirectory == null) {
			if (PDTConstants.DIALECT_SWI.equals(dialect)) {
				if (Util.isWindows()) {
					return getWindowsExecutable(PDTConstants.WINDOWS_EXECUTABLES_SWI);
				} else {
					return getUnixExecutable(PDTConstants.UNIX_COMMAND_LINE_EXECUTABLES_SWI);
				}
			} else if (PDTConstants.DIALECT_YAP.equals(dialect)) {
				if (Util.isWindows()) {
					return getWindowsExecutable(PDTConstants.WINDOWS_EXECUTABLES_YAP);
				} else {
					return getUnixExecutable(PDTConstants.UNIX_COMMAND_LINE_EXECUTABLES_YAP);
				}
			} else {
				return "";
			}
		} else { // localPrologDirectory was found, use subfolder, depending on dialect
			if (PDTConstants.DIALECT_SWI.equals(dialect)) {
				File swiDirectory = new File(localPrologDirectory, "swi");
				if (Util.isWindows()) {
					return getWindowsExecutable(PDTConstants.WINDOWS_EXECUTABLES_SWI, swiDirectory);
				} else {
					return getUnixExecutable(PDTConstants.UNIX_COMMAND_LINE_EXECUTABLES_SWI, swiDirectory);
				}
			} else if (PDTConstants.DIALECT_YAP.equals(dialect)) {
				File yapDirectory = new File(localPrologDirectory, "yap");
				if (Util.isWindows()) {
					return getWindowsExecutable(PDTConstants.WINDOWS_EXECUTABLES_YAP, yapDirectory);
				} else {
					return getUnixExecutable(PDTConstants.UNIX_COMMAND_LINE_EXECUTABLES_YAP, yapDirectory);
				}
			} else {
				return "";
			}
		}
	}

	/**
	 * Finds the current SWI-Prolog executable for UNIX/BSD-BASED OS
	 * @param unixCommandLineExecutables 
	 * @return the complete path of the executable otherwise it will return xpce
	 */
	public static String getUnixExecutable(String unixCommandLineExecutables) {
		return getUnixExecutable(unixCommandLineExecutables, null);
	}
	
	public static String getUnixExecutable(String unixCommandLineExecutables, File localPdtDirectory) {
		// currently, the localPdtDirectory argument is ignored, because it's only used in the installer
		//   and this is only available for windows
		
		// TODO shall we look for the env. variables as we do for Windows ?
		String[] appendPath = null;
	
		// Hack to resolve the issue of locating xpce in MacOS
		if (Util.isMacOS()) {
			appendPath = new String[1];
			appendPath[0] = "PATH=$PATH:" + System.getProperty("user.home") + "/bin:/opt/local/bin";
		}
	
		try {
				Process process = Runtime.getRuntime().exec(
						"which " + unixCommandLineExecutables, appendPath);
	
				if (process == null)
					return EMPTY_STRING;
	
				BufferedReader br = new BufferedReader(new InputStreamReader(
						process.getInputStream()));
				String path = br.readLine();
	
				if (path != null && !path.startsWith("no " + unixCommandLineExecutables)) {
					return path;
				}
			return EMPTY_STRING;
	
		} catch (IOException e) {
	
			return EMPTY_STRING;
		}
	}

	/**
	 * Finds the current SWI-Prolog executable for Windows OS
	 * @param executables 
	 * @return the complete path of the executable otherwise it will return plwin
	 */
	public static String getWindowsExecutable(String executables) {
		return getWindowsExecutable(executables, null);
	}
	
	public static String getWindowsExecutable(String executables, File localPrologDirectory) {

		String exec = executables;
		if (exec.indexOf(".exe") == -1)
			exec += ".exe";
		
		if (localPrologDirectory != null) {
			File binDir = new File(localPrologDirectory, "bin");
			File exeFile = new File(binDir, exec);
			if (exeFile.exists()) {
				return exeFile.getAbsolutePath();
			}
		}
		
		String plwin = null;
	
		String path;
		try {
	
			Process process = Runtime.getRuntime().exec(
					"cmd.exe /c echo %PATH%");
	
			if (process == null)
				return EMPTY_STRING;
	
			BufferedReader br = new BufferedReader(new InputStreamReader(
					process.getInputStream()));
			path = br.readLine();
	
			if (path == null)
				return EMPTY_STRING;
	
			// TODO just search in case of executable was not found.
			String[] paths = Util.split(path, ";");
			File exeFile = null;

			for (int i = 0; i < paths.length; i++) {
				String currPath = paths[i] + "\\" + exec;
				exeFile = new File(currPath);

				if (exeFile.exists()) {
					plwin = currPath;
					break;
				}
			}

			if(plwin== null){
				return EMPTY_STRING;
			}
			return plwin;

		} catch (IOException e) {

			return EMPTY_STRING;
		}
	}

	public static String createExecutable(String invocation, String execution, String commandLineArguments, String startupFiles) {
		StringBuilder executable = new StringBuilder();
		if (invocation != null) {
			executable.append(invocation);
			executable.append(" ");
		}
		if (Util.isWindows()) {
			executable.append("\"");
		}
		executable.append(execution);
		if (Util.isWindows()) {
			executable.append("\"");
		}
		
		if (commandLineArguments != null && !commandLineArguments.isEmpty() && !commandLineArguments.trim().isEmpty()) {
			executable.append(" ");
			executable.append(commandLineArguments);
		}
		if (startupFiles != null && !startupFiles.isEmpty() && !startupFiles.trim().isEmpty()) {
			executable.append(" -s ");
			executable.append(startupFiles);
		}
		return executable.toString();
	}

	public static String getLogtalkStartupFile() {
		if (Util.isWindows()) {
			return "\"%LOGTALKHOME%\\integration\\logtalk_swi.pl\"";
		} else {
			String logtalkHome = System.getenv("LOGTALKHOME");
			if (logtalkHome != null) {
				return new File(logtalkHome, "integration/logtalk_swi.pl").getAbsolutePath();
			} else {
				return "$LOGTALKHOME/integration/logtalk_swi.pl";
			}
		}
	}

	public static String getLogtalkEnvironmentVariables() {
		if (Util.isWindows()) {
			return "";
		} else {
			StringBuffer buf = new StringBuffer();
			String guessedEnvironmentVariables = guessEnvironmentVariables();
			if (!guessedEnvironmentVariables.isEmpty()) {
				buf.append(guessedEnvironmentVariables);
				buf.append(", ");
			}
			buf.append("LOGTALKHOME=");
			buf.append(System.getenv("LOGTALKHOME"));
			buf.append(", ");
			buf.append("LOGTALKUSER=");
			buf.append(System.getenv("LOGTALKUSER"));
			return buf.toString();
		}
	}

}
