package org.cs3.plbenchmarks;

import java.awt.Component;
import java.io.File;
import java.io.IOException;
import java.util.List;

import javax.swing.JOptionPane;

import org.apache.commons.io.FileUtils;
import org.cs3.plbenchmarks.data.BenchmarkCommand;
import org.cs3.plbenchmarks.data.Configuration;
import org.cs3.plbenchmarks.data.FactbaseVersionPair;
import org.cs3.plbenchmarks.data.PrologVersion;

public class PbtRunner {

	private static final String RESULT_PREDICATE_NAME = "result";
	private static final String SESSION_PREDICATE_NAME = "session";
	private static final String BENCHMARK_PREDICATE_NAME = "benchmark";
	private PbtController controller;
	private Configuration conf;
	private File dummyLoadFile;
	private Component gui;
	private File lockFile;
	private String id;
	
	public PbtRunner(PbtController controller, Configuration conf, Component gui) {
		this.controller = controller;
		this.conf = conf;
		this.gui = gui;
		this.id = Long.toString(System.currentTimeMillis());
	}

	public void run() {
		Logger.log("start with " + conf.getName());
		
		if (conf.isSaveFiles()) {
			// move files to subdir
			File outputFolder = new File(controller.getOutputFile()).getParentFile();
			File outputSubdir = new File(outputFolder, conf.getName() + "(" + id + ")");
			
			if (!outputSubdir.mkdirs()) {
				outputSubdir = new File(outputFolder, id);
				outputSubdir.mkdirs();
			}
			
			File loadFile = new File(conf.getLoadFile());
			
			if (loadFile.isFile()) {
				try {
					FileUtils.copyFileToDirectory(loadFile, outputSubdir);
				} catch (IOException e1) {
					e1.printStackTrace();
				}
			}
			
			try {
				FileUtils.writeStringToFile(new File(outputSubdir, "benchmark_code.txt"), conf.getBenchmarkCommandsAsString(), false);
			} catch (IOException e1) {
				e1.printStackTrace();
			}
		}
		
		lockFile = new File("resources/PBT_LockFile_" + System.currentTimeMillis());
		int position = 0;
		for (FactbaseVersionPair pair : conf.getPairs()) {
			
			while(lockFile.isFile()) {
				try {
					Logger.log("lock file exists, wait 5 seconds");
					Thread.sleep(5000);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
			
			if(!lockFile.isFile()) {
				try {
					if (dummyLoadFile != null && dummyLoadFile.isFile()) {
						dummyLoadFile.delete();
					}
					if (lockFile.createNewFile()) {
						Logger.log("lock file doesn't exist, start with " + pair + "(#" + position + ")");
						run(pair, position);
					} else {
						if (gui != null) {
							JOptionPane.showMessageDialog(gui, "Can't create lockfile", "Error", JOptionPane.ERROR_MESSAGE);
						}
						System.err.println("Can't create lockfile (" + lockFile.getAbsolutePath() + ")");
					}
				} catch (IOException e) {
					if (gui != null) {
						JOptionPane.showMessageDialog(gui, "Can't create lockfile", "Error", JOptionPane.ERROR_MESSAGE);
					}
					System.err.println("Can't create lockfile (" + lockFile.getAbsolutePath() + ")");
				}
			}
			position++;
		}
		while(lockFile.isFile()) {
			try {
				Logger.log("lock file exists, wait 5 seconds");
				Thread.sleep(5000);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
	}

	private void run(FactbaseVersionPair pair, int position) {
		try {
			// create dummy prolog file
			createDummyLoadFile(pair, position);

			// run selected prolog version with the dummy file
			String versionName = pair.getVersion();
			String commandArray = createCommandArray(controller.getVersion(versionName));

			letItRide(commandArray);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private void letItRide(String commandArray) {
		try {
			Logger.log("starting: " + commandArray);
			Runtime.getRuntime().exec(commandArray);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private String createCommandArray(PrologVersion version) {
//		String commandString = " -f " + "\"" + dummyLoadFile.getPath() + "\" -g run_benchmark,halt";
		
		String commandString;
		
		if (isWindows()) {
			commandString = "cmd.exe /c start \"cmdwindow\" /min \"" + version.getExecutable() + "\"" + " -f " + "\"" + dummyLoadFile.getPath() + "\" -g run_benchmark,halt";
//			commandString += "\"" + pbtLoadFile.getAbsolutePath() + "\"";
		} else {
			commandString = version.getExecutable() + " -f " + dummyLoadFile.getPath() + " -g run_benchmark,halt";
//			commandString = version.getExecutable() + " -f " + dummyLoadFile.getPath() + " -g \"['" + pbtLoadFile.getAbsolutePath() + "'],run_benchmark,halt\"";
		}

//		String commandString = "cmd.exe /c start \"cmdwindow\" /min " + "\"" + version.getExecutable() + "\" -f " + "\"" + dummyLoadFile.getPath() + "\" -g \"run_benchmark, halt\"";
//		commandString += "\"" + pbtLoadFile.getAbsolutePath() + "\"";
		
		return commandString;
	}
	
	private boolean isWindows() {
		boolean windowsPlattform = System.getProperty("os.name").indexOf("Windows") > -1;
		return windowsPlattform;
	}

	private void createDummyLoadFile(FactbaseVersionPair pair, int position) throws IOException {
		StringBuffer buf = new StringBuffer();
		File pbtLoadFile = new File("resources/benchmark.pl");
		buf.append(":- [" + getPathForProlog(pbtLoadFile.getAbsolutePath()) + "].\n");
		if (conf.isLoadJT()) {
			buf.append(":- assert(file_search_path(jtransformer, " + getPathForProlog(controller.getJTPrologEngineDir()) + ")).\n");
			buf.append(":- assert(file_search_path(stjava, " + getPathForProlog(controller.getSTJavaDir()) + ")).\n");
			buf.append(":- assert(file_search_path(pdt_prolog_library, " + getPathForProlog(controller.getPdtLoadDir()) + ")).\n");
			buf.append(consult(controller.getSTJavaFile()));
			buf.append(consult(controller.getJTPrologEngineFile()));
		} else if (conf.isLoadPDT()) {
			buf.append(consult(controller.getPdtLoadFile()));
		}
		if (!conf.getLoadFile().isEmpty()) {
			buf.append(consult(conf.getLoadFile()));
		}
		buf.append(consultFactbase(pair));
		
		buf.append("run_benchmark :-\n");
		buf.append("  append(");
		buf.append(getPathForProlog(conf.getOutputFile()));
		buf.append("),\n");
//		buf.append("current_prolog_flag(version,V), printf('version: ~w',V),\n");
		
		if (position == 0) {
			buf.append(writeBenchmarkFact());
		}
		
//		long timestamp = System.currentTimeMillis();
		buf.append(writeSessionFact(pair, position));
		
		List<BenchmarkCommand> benchmarkCommands = conf.getBenchmarkCommands();
		
		int i=0;
		for(BenchmarkCommand command : benchmarkCommands) {
			buf.append(benchmark(command, i, 0, position, true));
			for (int j=0; j<command.getAdditionalRuns(); j++) {
				buf.append(benchmark(command, i, j+1, position, false));	
			}
			i++;
		}
		
		buf.append("  told,\n");
		
		buf.append("  delete_file(" + getPathForProlog(lockFile.getAbsolutePath()) + ").");
		
		dummyLoadFile = File.createTempFile("prolog_benchmark", "");

		FileUtils.writeStringToFile(dummyLoadFile, buf.toString(), true);
	}
	
	private String writeBenchmarkFact() {
		StringBuffer buf = new StringBuffer();
		buf.append("  writeln(");
		StringBuffer sessionString = new StringBuffer();
		sessionString.append(BENCHMARK_PREDICATE_NAME);
		sessionString.append("(");
		sessionString.append(id);
		sessionString.append(", ");
		sessionString.append(quote(conf.getName()));
		sessionString.append(", ");
		sessionString.append(quote(getPDTVersion()));
		sessionString.append(").");
		buf.append(quote(sessionString.toString()));
		buf.append("),\n");
		return buf.toString();
	}

	private String consultFactbase(FactbaseVersionPair pair) {
		File factbaseDir = controller.getFactbaseFolder();
		File plFile = new File(factbaseDir, pair.getFactbase());
		File qlfFile = new File(factbaseDir, "qlf/" + pair.getQlfName());
		
		boolean supportsQLF = controller.getVersion(pair.getVersion()).isSupportsQLF();
		String consultString = "";
		
		if(supportsQLF) {
			if (qlfFile.isFile()) {
				Logger.log("load qlf file for " + pair);
				consultString = consult(qlfFile.getPath()); 
			} else {
				Logger.log("load pl file for " + pair);
				consultString =  qcompile(plFile.getPath(), qlfFile.getPath());
			}
		} else {
			consultString = consult(plFile.getPath());
		}
		
		return consultString;
	}

	private String benchmark(BenchmarkCommand command, int dummy1, int dummy2, int subId, boolean first) {
		String goal = command.getPredicateCall();
		StringBuffer buf = new StringBuffer();
		String dummy = dummy1 + "_" + dummy2;
		
//		buf.append("  performance(");
		buf.append("  pbt_performance(");
		buf.append(goal);
		buf.append(", Time_" + dummy + ", CountAll_" + dummy);
		buf.append(", Infer_" + dummy);
		buf.append(", " + (command.isUniqueResults() ? "unique" : "null"));
		buf.append("),\n");
		buf.append("  printf('");
		buf.append(RESULT_PREDICATE_NAME);
		buf.append("(");
		buf.append(id);
		buf.append(", ");
		buf.append(subId);
		buf.append(", ");
		buf.append(goal);
		buf.append(", ~w, ~w, ~w, ");
		if (first) {
			buf.append("first");
		} else {
			buf.append("subsequent");
		}
		buf.append(").\\n', [");
		buf.append("Time_" + dummy + ", CountAll_" + dummy);
		buf.append(", Infer_" + dummy);
		buf.append("]),\n");
		return buf.toString();
	}

	private String writeSessionFact(FactbaseVersionPair pair, int position) {
		StringBuffer buf = new StringBuffer();
		buf.append("  current_prolog_flag(version_data,VersionData),\n");
		buf.append("  printf('");
		buf.append(SESSION_PREDICATE_NAME);
		buf.append("(");
		buf.append(id);
		buf.append(", ");
		buf.append(position);
		buf.append(", ~w , \\'");
		buf.append(pair.getFactbase());
		buf.append("\\').~n', [VersionData]),\n");
		return buf.toString();
	}

	private String getPDTVersion() {
		String pdtFile = controller.getPdtLoadFile();
		String pdtVersion = null;
		int startIndex = pdtFile.indexOf("library_");
		if (startIndex > -1) {
			int endIndex = pdtFile.indexOf("\\", startIndex);
			if (endIndex > -1) {
				pdtVersion = pdtFile.substring(startIndex + 8, endIndex);
			} else {
				pdtVersion = pdtFile.substring(startIndex + 8);
			}
		} else {
			pdtVersion = "git";
		}
		return pdtVersion;
	}

	private String consult(String path) {
		String resultString = ":- consult(";
		resultString += getPathForProlog(path);
		resultString += ").\n";
		return resultString;
	}
	
	private String qcompile(String path, String qlfTarget) {
		String resultString = ":- qcompile(";
		resultString += getPathForProlog(path);
		resultString += "), rename_file(";
		resultString += getPathForProlog(path + ".qlf");
		resultString += ", ";
		resultString += getPathForProlog(qlfTarget);
		resultString += ").\n";
		return resultString;
	}

	private String getPathForProlog(String path) {
		String result = path.replace("\\", "/");
		return quote(result);
	}

	private String quote(String result) {
		return "'" + result.replace("'", "\\'") + "'";
	}
}
