package org.cs3.plbenchmarks.data;

import java.io.Serializable;

public class BenchmarkCommand implements Serializable {

	private static final long serialVersionUID = -3543533224672008631L;
	
	private String predicateCall;
	private int additionalRuns;
	private boolean uniqueResults;
	
	public BenchmarkCommand(String predicateCall, int additionalRuns, boolean uniqueResults) {
		this.predicateCall = predicateCall;
		this.additionalRuns = additionalRuns;
		this.uniqueResults = uniqueResults;
	}
	
	public BenchmarkCommand(String unparsedString) {
		String parsedCommand = unparsedString.trim();
		if (parsedCommand.endsWith("{unique}")) {
			uniqueResults = true;
			parsedCommand = parsedCommand.substring(0, parsedCommand.length() - 8).trim();
		}
		int additional = 0;
		if (parsedCommand.matches(".*\\{([0-9]+)\\}")) {
			int begin = parsedCommand.lastIndexOf("{");
			int end = parsedCommand.lastIndexOf("}");
			String additionalString = parsedCommand.substring(begin+1, end);
			additional = Integer.parseInt(additionalString) - 1;
			parsedCommand = parsedCommand.substring(0, begin);
		}
		
		this.predicateCall = parsedCommand;
		this.additionalRuns = additional;
	}
	
	public String getPredicateCall() {
		return "(" + predicateCall + ")";
	}
	
	public int getAdditionalRuns() {
		return additionalRuns;
	}
	
	public boolean isUniqueResults() {
		return uniqueResults;
	}
	
	@Override
	public String toString() {
		String result = predicateCall;
		if (additionalRuns > 0) {
			result += "{" + (additionalRuns+1) + "}";
		}
		if (uniqueResults) {
			result += "{unique}";
		}
		return result;
	}
	
}
