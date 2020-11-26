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

/*
 */
package org.cs3.prolog.connector.internal.process.socket;

import java.io.BufferedOutputStream;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.InputStreamPump;
import org.cs3.prolog.connector.common.ProcessUtils;
import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.common.Util;
import org.cs3.prolog.connector.internal.process.ServerStartAndStopStrategy;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessStartException;
import org.cs3.prolog.connector.process.StartupStrategy;

public class SocketServerStartAndStopStrategy implements ServerStartAndStopStrategy {

	private static JackTheProcessRipper processRipper;
	
	private static final String STARTUP_ERROR_LOG_PROLOG_CODE = 
			":- multifile message_hook/3.\n" +
			":- dynamic message_hook/3.\n" +
			":- dynamic pdt_startup_error_message/1.\n" +
			":- dynamic collect_pdt_startup_error_messages/0.\n" +
			"collect_pdt_startup_error_messages.\n" +
			"message_hook(_,Level,Lines):-\n" +
			"    collect_pdt_startup_error_messages,\n" +
			"    (Level == error; Level == warning),\n" + 
			"    prolog_load_context(term_position, T),\n" +
			"    (   T = '$stream_position'(_,Line,_,_,_)\n" +
			"    ->  true\n" +
			"    ;   T = '$stream_position'(_,Line,_,_)\n" +
			"    ),\n" +
			"    prolog_load_context(source, File),\n" +
			"    with_output_to(atom(Msg0), (current_output(O), print_message_lines(O, '', Lines))),\n" +
			"    format(atom(Msg), 'Location: ~w:~w~nMessage: ~w', [File, Line, Msg0]),\n" +
			"    assertz(pdt_startup_error_message(Msg)),\n" +
			"    fail.\n" +
			"write_pdt_startup_error_messages_to_file(_File) :-\n" +
			"    retractall(collect_pdt_startup_error_messages),\n" + 
			"    \\+ pdt_startup_error_message(_),\n" +
			"    !.\n" +
			"write_pdt_startup_error_messages_to_file(File) :-\n" +
			"    open(File, write, Stream),\n" +
			"    forall(pdt_startup_error_message(Msg),format(Stream, '~w~n', [Msg])),\n" +
			"    close(Stream).\n";
	private static final String STARTUP_ERROR_LOG_LOGTALK_CODE =
			":- logtalk::assertz((message_hook(_, Kind, core, Tokens) :-\n" +
			"    user::collect_pdt_startup_error_messages,\n" +
			"    functor(Kind, Level, _),\n" +
			"    (Level == error; Level == warning),\n" + 
			"    with_output_to(atom(Msg), (current_output(S), logtalk::print_message_tokens(S, '', Tokens))),\n" +
			"    user::assertz(pdt_startup_error_message(Msg)),\n" +
			"    fail)).\n";

	private static final String INVALID_PATH_TO_THE_PROLOG_EXECUTABLE = "Failed to start the Prolog process.\nThe path to the Prolog executable is invalid.";
	private static final String TIMEOUT_EXCEEDED = "Failed to connect to the Prolog process.\nTimeout exceeded.";

	public SocketServerStartAndStopStrategy() {
		processRipper=JackTheProcessRipper.getInstance();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.cs3.pl.prolog.ServerStartAndStopStrategy#startServer(org.cs3.pl.prolog
	 * .IPrologProcess)
	 */
	@Override
	public  Process startServer(PrologProcess process) {
		if (!(process instanceof SocketPrologProcess)) {
			throw new ClassCastException("SocketPrologProcess needed but got another PrologProcess");
		}
		SocketPrologProcess socketProcess = (SocketPrologProcess) process;
		return startSocketServer(socketProcess);
	}

	private Process startSocketServer(SocketPrologProcess socketProcess) {
		File lockFile = Util.getLockFile();
		socketProcess.setLockFile(lockFile);
		File errorLogFile = Util.getLockFile();
		socketProcess.setErrorLogFile(errorLogFile);
		Util.addTempFile(errorLogFile);
		int port = getFreePort(socketProcess);
		Process process = getNewProcess(socketProcess, port);
		try {			
			initializeBuffers(socketProcess, process);
			waitForProcessToGetRunning(socketProcess, process);
			String errorLogFileContent = getErrorLogFileContent(socketProcess);
			if (errorLogFileContent != null && !errorLogFileContent.isEmpty()) {
				Debug.warning("Prolog warnings and errors during initialization:\n" + errorLogFileContent);
			}
			return process;
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}
	}

	private static Process getNewProcess(SocketPrologProcess socketProcess, int port) {
		String[] commandArray = getCommandArray(socketProcess, port);
		Map<String, String> env = getEnvironmentAsArray(socketProcess);
		Process process = null;
		try {
			Debug.info("Starting server with " + Util.prettyPrint(commandArray));
			// Temporary safety code to ensure that the command array contains no empty strings:
			List<String> commands = new ArrayList<String>();
			// keep this until its clear why there is an empty string elements in the array
			for (String string : commandArray) {
				if(string != null && string.length()>0){
					commands.add(string);
				}
			}
			commandArray=commands.toArray(new String[0]);
			
			if (env.size() == 0) {
				Debug.info("inheriting system environment");
				ProcessBuilder processBuilder = new ProcessBuilder(commands);
				processBuilder.environment();
				process = processBuilder.start();
			} else {
				Debug.info("using environment: " + Util.prettyPrint(env));
				ProcessBuilder processBuilder = new ProcessBuilder(commands);
				Map<String, String> processEnvironment = processBuilder.environment();
				processEnvironment.putAll(env);
				process = processBuilder.start();
			}
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}
		return process;
	}

	private static void initializeBuffers(SocketPrologProcess socketProcess,
			Process process) throws IOException {
		BufferedWriter writer = initializeCorrespondingLogFile(socketProcess);
		new InputStreamPump(process.getErrorStream(), writer).start();
		new InputStreamPump(process.getInputStream(), writer).start();
	}

	private static BufferedWriter initializeCorrespondingLogFile(
			SocketPrologProcess socketProcess) throws IOException {
		File logFile = Util.getLogFile(socketProcess.getServerLogDir(),"pdt.server.log");
		BufferedWriter writer = new BufferedWriter(new FileWriter(logFile.getAbsolutePath(), true));
		writer.write("---------------------------------\n");
		writer.write(new Date().toString() + "\n");
		writer.write("---------------------------------\n");
		return writer;
	}
	
	private  void waitForProcessToGetRunning(SocketPrologProcess socketProcess,
			Process process) {
		long timeout = socketProcess.getTimeout();
		long startTime = System.currentTimeMillis();
		while (!isRunning(socketProcess)) {
			try {
				long now = System.currentTimeMillis();
				if (now - startTime > timeout) {
					String errorLogFileContent = getErrorLogFileContent(socketProcess);
					if (errorLogFileContent != null) {
						Debug.error("Prolog errors during initialization:\n" + errorLogFileContent);
					}
					throw new PrologProcessStartException(TIMEOUT_EXCEEDED);
				}
				Thread.sleep(50);
			} catch (InterruptedException e1) {
				Debug.report(e1);
			}
			try {
				if (process.exitValue() != 0) {
					throw new RuntimeException("Failed to start the Prolog process. Process exited with error code " + process.exitValue());
				}
			} catch (IllegalThreadStateException e) {
				; // nothing. the process is still coming up.
			}
		}
	}

	private String getErrorLogFileContent(SocketPrologProcess socketProcess) {
		String errorLogFileContent = null;
		try {
			errorLogFileContent = Util.readInputStreamToString(new FileInputStream(socketProcess.getErrorLogFile()));
		} catch (FileNotFoundException e) {
		} catch (IOException e) {
		}
		return errorLogFileContent;
	}



	private static Map<String, String> getEnvironmentAsArray(SocketPrologProcess socketProcess) {
		String environment = socketProcess.getEnvironment();
		Map<String, String> env = new HashMap<String, String>();
		String[] envarray = Util.split(environment, ",");
		for (int i = 0; i < envarray.length; i++) {
			String[] mapping = Util.split(envarray[i], "=");
			env.put(mapping[0], mapping[1]);
		}
		return env;
	}

	private static String[] getCommandArray(SocketPrologProcess socketProcess, int port) {
		String[] command = getCommands(socketProcess);
		String[] args = getArguments(socketProcess, port);
		String[] commandArray = new String[command.length + args.length];
		System.arraycopy(command, 0, commandArray, 0, command.length);
		System.arraycopy(args, 0, commandArray, command.length, args.length);
		return commandArray;
	}

	private static String[] getArguments(SocketPrologProcess socketProcess, int port) {
		File tmpFile = null;
		try {
			tmpFile = File.createTempFile("socketProcess", null);
			Util.addTempFile(tmpFile);
			writeInitialisationToTempFile(socketProcess, port, tmpFile);
		} catch (IOException e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
		String[] args = buildArguments(socketProcess, tmpFile);
		return args;
	}

	private static String[] getCommands(SocketPrologProcess socketProcess) {
		String executablePath = socketProcess.getExecutablePath();
		if (executablePath == null || executablePath.isEmpty() || !new File(executablePath).exists()) {
			Debug.error(INVALID_PATH_TO_THE_PROLOG_EXECUTABLE);
			throw new PrologProcessStartException(INVALID_PATH_TO_THE_PROLOG_EXECUTABLE);
		}
		String executable = ProcessUtils.createExecutable(
				socketProcess.getOSInvocation(),
				executablePath,
				socketProcess.getCommandLineArguments(),
				socketProcess.getAdditionalStartupFile());
		String[] command = Util.split(executable, " ");
		return command;
	}

	private static String[] buildArguments(SocketPrologProcess socketProcess,
			File tmpFile) {
		String[] args = new String[] { "-g", "[" + QueryUtils.prologFileNameQuoted(tmpFile) + "]" };
		return args;
	}

	private static void writeInitialisationToTempFile(SocketPrologProcess socketProcess,
			int port, File tmpFile) throws FileNotFoundException {
		PrintWriter tmpWriter = new PrintWriter(new BufferedOutputStream(new FileOutputStream(tmpFile)));
//      Don't set the encoding globally because it breaks something
//		tmpWriter.println(":- set_prolog_flag(encoding, utf8).");
		tmpWriter.println(STARTUP_ERROR_LOG_PROLOG_CODE);
		if (socketProcess.getAdditionalStartupFile() != null && socketProcess.getAdditionalStartupFile().contains("logtalk")) {
			tmpWriter.print(STARTUP_ERROR_LOG_LOGTALK_CODE);
		}
		tmpWriter.println(":- (current_prolog_flag(xpce_threaded, _) -> set_prolog_flag(xpce_threaded, true) ; true).");
		tmpWriter.println(":- (current_prolog_flag(dialect, swi) -> guitracer ; true).");
		if (socketProcess.isHidePlwin()) {
			tmpWriter.println(":- (  (current_prolog_flag(dialect, swi), current_prolog_flag(windows, true))  -> win_window_pos([show(false)]) ; true).");
		}
		tmpWriter.println(":- (current_prolog_flag(windows,_T) -> set_prolog_flag(tty_control,false) ; true).");

		tmpWriter.println(":- ['" + socketProcess.getConsultServerLocation() + "'].");
		StartupStrategy startupStrategy = socketProcess.getStartupStrategy();
		for (String fspInit : startupStrategy.getFileSearchPathInitStatements()) {
			tmpWriter.println(":- " + fspInit + ".");
		}
		for (String lfInit : startupStrategy.getLoadFileInitStatements()) {
			tmpWriter.println(":- " + lfInit + ".");
		}
		tmpWriter.println(":- consult_server(" + port + "," + QueryUtils.prologFileNameQuoted(socketProcess.getLockFile()) + ").");
		tmpWriter.println(":- write_pdt_startup_error_messages_to_file(" + QueryUtils.prologFileNameQuoted(socketProcess.getErrorLogFile()) + ").");
		tmpWriter.close();
	}

	private static int getFreePort(SocketPrologProcess socketProcess) {
		int port;
		try {
			port = Util.findFreePort();
			socketProcess.setPort(port);
		} catch (IOException e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
		return port;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.cs3.pl.prolog.ServerStartAndStopStrategy#stopServer(org.cs3.pl.prolog
	 * .IPrologProcess, boolean)
	 */
	@Override
	public void stopServer(PrologProcess process) {
		if (!(process instanceof SocketPrologProcess)) {
			throw new ClassCastException("SocketPrologProcess needed but got another PrologProcess");
		}
		try {
			if (!isRunning(process)) {
				Debug.info("There is no server running. I do not stop anything.");
				return;
			}
			SocketPrologProcess socketProcess = (SocketPrologProcess) process;
			stopSocketServer(socketProcess);
		} catch (Throwable e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
	}
	
	private static void stopSocketServer(SocketPrologProcess socketProcess){
		try {
			SocketClient client = getSocketClient(socketProcess);
			sendClientShutdownCommand(client);
			long pid = client.getServerPid();
			client.close();
			socketProcess.getLockFile().delete();
			File errorLogFile = socketProcess.getErrorLogFile();
			if (errorLogFile.exists()) {
				errorLogFile.delete();
			}
			Debug.info("server process will be killed in about a second.");
			processRipper.markForDeletion(pid);
		} catch (Exception e) {
			Debug.warning("There was a problem during server shutdown.");
			Debug.report(e);
		}
	}

	private static SocketClient getSocketClient(SocketPrologProcess socketProcess)
			throws UnknownHostException, IOException {
		int port = socketProcess.getPort();
		SocketClient client = new SocketClient(socketProcess.getHost(), port);
		return client;
	}

	private static void sendClientShutdownCommand(SocketClient client) 
			throws UnknownHostException, IOException {
		client.readUntil(SocketCommunicationConstants.GIVE_COMMAND);
		client.writeln(SocketCommunicationConstants.SHUTDOWN);
		client.readUntil(SocketCommunicationConstants.BYE);
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.cs3.pl.prolog.ServerStartAndStopStrategy#isRunning(org.cs3.pl.prolog
	 * .IPrologProcess)
	 */
	@Override
	public  boolean isRunning(PrologProcess process) {
		File lockFile = ((SocketPrologProcess) process).getLockFile();
		return lockFile != null && lockFile.exists();
	}
}


