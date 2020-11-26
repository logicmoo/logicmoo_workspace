package org.cs3.pdt.connector.load;

import java.util.ArrayList;
import java.util.List;

import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.process.StartupStrategy;

public class BootstrapStartupStrategy implements StartupStrategy {

	private final List<String> loadFileInitStatments = new ArrayList<>();
	private final List<String> fileSearchPathInitStatements = new ArrayList<>(); 
	
	@Override
	public List<String> getFileSearchPathInitStatements() {
		return fileSearchPathInitStatements;
	}

	@Override
	public List<String> getLoadFileInitStatements() {
		return loadFileInitStatments;
	}

	public boolean contains(BootstrapPrologContribution library) {
		if (library instanceof BootstrapPrologContributionAlias) {
			return fileSearchPathInitStatements.contains(library.getPrologInitStatement());
		} else if (library instanceof BootstrapPrologContributionFile) {
			return loadFileInitStatments.contains(library.getPrologInitStatement());
		} else {
			Debug.error(library.getClass() + " is no valid BootstrapPrologContribution");
		}
		return false;
	}

	public void add(BootstrapPrologContribution library) {
		if (library instanceof BootstrapPrologContributionAlias) {
			fileSearchPathInitStatements.add(library.getPrologInitStatement());
		} else if (library instanceof BootstrapPrologContributionFile) {
			loadFileInitStatments.add(library.getPrologInitStatement());
		} else {
			Debug.error(library.getClass() + " is no valid BootstrapPrologContribution");
		}
	}

}
