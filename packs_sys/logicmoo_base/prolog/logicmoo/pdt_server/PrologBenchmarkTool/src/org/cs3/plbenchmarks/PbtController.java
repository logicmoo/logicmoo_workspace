package org.cs3.plbenchmarks;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.cs3.plbenchmarks.data.PrologVersion;

public class PbtController {

	private static final String PBT_PROPERTIES = "resources/pbt.properties";
	private static final String OUTPUT_FILE = "output_file";
	private static final String PDT_LOAD_FILE = "pdt_load_file";
	private static final String ST_JAVA_LOAD_FILE = "st_java_load_file";
	private static final String JT_PROLOG_LOAD_FILE = "jt_prolog_load_file";
	private static final String FACTBASE_FOLDER = "factbase_folder";
	private static final String BASE_DIR = "base_dir";
	
	private Properties myProps;
	private List<PrologVersion> versionList;
	private Map<String, PrologVersion> versionMap;
	private List<String> factbaseList;

	public PbtController() {
		// load properties file
		myProps = new Properties();
		try {
			myProps.load(new FileInputStream(PBT_PROPERTIES));  
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		// load lists
		readVersions();
		readFactbases();
	}

	public String getOutputFile() {
		return myProps.getProperty(OUTPUT_FILE);
	}

	public int getFactbaseCount() {
		return factbaseList.size();
	}
	
	public int getVersionCount() {
		return versionList.size();
	}

	public String getFactbase(int i) {
		return factbaseList.get(i);
	}
	
	public PrologVersion getVersion(int i) {
		return versionList.get(i);
	}
	
	public PrologVersion getVersion(String name) {
		return versionMap.get(name);
	}
	
	private void readFactbases() {
		factbaseList = new ArrayList<>();

		File folder = new File(myProps.getProperty(FACTBASE_FOLDER));

		String[] factbases = folder.list(new FilenameFilter() {
			@Override public boolean accept(File f, String name) {
				return name.toLowerCase().endsWith(".pl");
			}
		});

		for (String s : factbases) {
			factbaseList.add(s.substring(0, s.lastIndexOf(".")));
		}
		
	}
	
	private void readVersions() {
		File f = new File("resources/versions.txt");
		versionList = new ArrayList<>();
		versionMap = new HashMap<>();
		
		try{
			// Open the file that is the first 
			// command line parameter
			FileInputStream fstream = new FileInputStream(f);
			// Get the object of DataInputStream
			DataInputStream in = new DataInputStream(fstream);
			BufferedReader br = new BufferedReader(new InputStreamReader(in));
			String strLine;
			//Read File Line By Line
			while ((strLine = br.readLine()) != null)   {
				// Print the content on the console
				if (!strLine.trim().isEmpty()) {
					PrologVersion version = new PrologVersion(strLine);
					versionList.add(version);
					versionMap.put(version.getName(), version);
				}
			}
			//Close the input stream
			in.close();
		} catch (Exception e) {
			System.err.println("Error: " + e.getMessage());
		}
	}

	public String getPdtLoadFile() {
		return myProps.getProperty(PDT_LOAD_FILE);
	}
	
	public String getPdtLoadDir() {
		File f = new File(myProps.getProperty(PDT_LOAD_FILE));
		return f.getParent();
	}
	
	public File getFactbaseFolder() {
		return new File(myProps.getProperty(FACTBASE_FOLDER));
	}

	public File getBaseDir() {
		return new File(myProps.getProperty(BASE_DIR));
	}

	public String getSTJavaFile() {
		return myProps.getProperty(ST_JAVA_LOAD_FILE);
	}
	
	public String getSTJavaDir() {
		File f = new File(myProps.getProperty(ST_JAVA_LOAD_FILE));
		return f.getParent();
	}

	public String getJTPrologEngineFile() {
		return myProps.getProperty(JT_PROLOG_LOAD_FILE);
	}
	
	public String getJTPrologEngineDir() {
		File f = new File(myProps.getProperty(JT_PROLOG_LOAD_FILE));
		return f.getParentFile().getParent();
	}
}
