package org.cs3.plbenchmarks.data;

public class PrologVersion {

	private String name;
	private String executable;
	private boolean supportsQLF;
	
	public PrologVersion(String unparsedString) {
		String[] split = unparsedString.trim().split("\\|");
		if (split.length == 2) {
			name = split[0].trim();
			executable = split[1].trim();
		} else {
			name = "FAIL";
			executable = null;
		}
		
		// TODO: unsauber
		supportsQLF = name.toUpperCase().startsWith("SWI");
	}

	public PrologVersion(String name, String executable) {
		this.name = name;
		this.executable = executable;
	}
	
	public String getName() {
		return name;
	}

	public String getExecutable() {
		return executable;
	}
	
	public boolean isSupportsQLF() {
		return supportsQLF;
	}
	
	@Override
	public String toString() {
		return name;
	}
	
	@Override
	public boolean equals(Object o) {
		if (o instanceof PrologVersion) {
			PrologVersion version = (PrologVersion) o;
			return (executable.equals(version.getExecutable()) && name.equals(version.getName()));
		} else {
			return false;
		}
	}
	
}
