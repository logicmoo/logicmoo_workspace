package org.cs3.plbenchmarks.data;

import java.io.Serializable;


public class FactbaseVersionPair implements Serializable {

	private static final long serialVersionUID = 3310245937765097180L;
	private String version;
	private String factbase;
	
	public FactbaseVersionPair(String version, String factbase) {
		this.version = version;
		this.factbase = factbase;
	}
	
	public String getFactbase() {
		return factbase;
	}
	
	public String getVersion() {
		return version;
	}
	
	@Override
	public String toString() {
		return factbase + "(version: " + version + ")";
	}
	
	@Override
	public boolean equals(Object o) {
		if (o instanceof FactbaseVersionPair) {
			FactbaseVersionPair pair = (FactbaseVersionPair) o;
			return (factbase.equals(pair.getFactbase()) && version.equals(pair.getVersion()));
		} else {
			return false;
		}
	}
	
	public String getQlfName() {
		String versionName = version.replace(" ", "_");
		versionName = versionName.toLowerCase();
		return factbase + "_" + versionName + ".qlf";
	}
}
