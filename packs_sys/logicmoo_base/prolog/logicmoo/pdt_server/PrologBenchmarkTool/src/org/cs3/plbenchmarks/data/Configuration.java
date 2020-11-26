package org.cs3.plbenchmarks.data;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class Configuration implements Serializable {

	private static final long serialVersionUID = -6078547359154305844L;
	private String name;
	private List<FactbaseVersionPair> pairs;
	private String loadFile;
	private String outputFile;
	private List<BenchmarkCommand> benchmarkCommands;
	private boolean loadPDT;
	private boolean loadJT;
	private boolean saveFiles = true;
	
	public Configuration(String name, List<FactbaseVersionPair> pairs, String loadFile, String outputFile, String benchmarkCode, boolean loadPDT, boolean loadJT) {
		this.name = name;
		this.pairs = pairs;
		this.loadFile = loadFile;
		this.outputFile = outputFile;
		this.loadPDT = loadPDT;
		this.loadJT = loadJT;
		parseBenchmarkCommands(benchmarkCode);
	}


	private void parseBenchmarkCommands(String benchmarkCode) {
		benchmarkCommands = new ArrayList<>();
		String[] benchmarkArray = benchmarkCode.split("\n");
		for (String s : benchmarkArray) {
			benchmarkCommands.add(new BenchmarkCommand(s));
		}
	}
	
	public List<BenchmarkCommand> getBenchmarkCommands() {
		return benchmarkCommands;
	}

	public String getBenchmarkCommandsAsString() {
		StringBuffer buf = new StringBuffer();
		for (BenchmarkCommand command : benchmarkCommands) {
			buf.append(command.toString());
			buf.append("\n");
		}
		return buf.toString();
	}

	public String getName() {
		return name;
	}
	
	public boolean isLoadPDT() {
		return loadPDT;
	}
	
	public boolean isLoadJT() {
		return loadJT;
	}

	public List<FactbaseVersionPair> getPairs() {
		return pairs;
	}

	public String getLoadFile() {
		return loadFile;
	}

	public String getOutputFile() {
		return outputFile;
	}

	@Override
	public String toString() {
		return name + " (" + pairs.size() + " runs, " + benchmarkCommands.size() + " queries)";
	}


	public boolean isSaveFiles() {
		return saveFiles;
	}


	public void setSaveFiles(boolean saveFiles) {
		this.saveFiles = saveFiles;
	}


}
