package org.cs3.pdt.common.internal;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.InvalidPropertiesFormatException;
import java.util.Properties;
import java.util.Set;

import org.cs3.prolog.connector.common.Debug;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;

public class PDTProperties {
	
	private static HashMap<IProject, PDTProperties> projectMap = new HashMap<>();
	
	public static PDTProperties getPDTProperties(IProject project) {
		PDTProperties result = projectMap.get(project);
		if (result == null) {
			result = new PDTProperties(project);
			projectMap.put(project, result);
		}
		return result;
	}
	
	public static Set<IFile> getAllEntryPoints() {
		HashSet<IFile> result = new HashSet<IFile>();
		for (PDTProperties properties : projectMap.values()) {
			result.addAll(properties.entryPoints);
		}
		return result;
	}
		
	private static final String ENTRY_POINT_SEPARATOR = "|";
	private static final String ENTRY_POINTS_PROPERTY = "entry.points";

	private IProject project;
	private Properties properties = new Properties();
	private File propertyFile;
	
	private HashSet<IFile> entryPoints = new HashSet<>();
	
	private PDTProperties(IProject project) {
		this.project = project;
		propertyFile = getPropertyFile();
		loadPropertyFile();
	}
	
	private File getPropertyFile() {
		return project.getFile(".pdtproperties").getRawLocation().makeAbsolute().toFile();
	}
	
	public void loadPropertyFile() {
		entryPoints.clear();
		if (propertyFile.exists()) {
			try {
				properties.loadFromXML(new FileInputStream(propertyFile));
				loadEntryPoints();
			} catch (IOException e) {
				Debug.report(e);
			}
		}
	}
	
	public void setProperty(String property, String value) throws InvalidPropertiesFormatException, FileNotFoundException, IOException {
		properties.setProperty(property, value);
		properties.storeToXML(new FileOutputStream(propertyFile), "PDT Properties");
	}
	
	public String getProperty(String property) throws InvalidPropertiesFormatException, FileNotFoundException, IOException {
		return properties.getProperty(property);
	}
	
	public void addEntryPointFile(IFile file) throws InvalidPropertiesFormatException, FileNotFoundException, IOException {
		entryPoints.add(file);
		String entryPointsProperty = getProperty(ENTRY_POINTS_PROPERTY);
		String path = file.getProjectRelativePath().toString();
		if (entryPointsProperty == null || entryPointsProperty.isEmpty()) {
			entryPointsProperty = path + ENTRY_POINT_SEPARATOR;
		} else {
			if (entryPointsProperty.contains(path)) {
				return;
			}
			entryPointsProperty += path + ENTRY_POINT_SEPARATOR;
		}
		setProperty(ENTRY_POINTS_PROPERTY, entryPointsProperty);
	}
	
	public void removeEntryPointFile(IFile file) throws InvalidPropertiesFormatException, FileNotFoundException, IOException {
		entryPoints.remove(file);
		String entryPointsProperty = getProperty(ENTRY_POINTS_PROPERTY);
		String path = file.getProjectRelativePath().toString();
		if (entryPointsProperty == null || entryPointsProperty.isEmpty()) {
			return;
		} 
		if (entryPointsProperty.contains(path + ENTRY_POINT_SEPARATOR)) {
			entryPointsProperty = entryPointsProperty.replace(path + ENTRY_POINT_SEPARATOR, "");
			setProperty(ENTRY_POINTS_PROPERTY, entryPointsProperty);
		}
	}
	
	public boolean isEntryPoint(IFile file) {
		return entryPoints.contains(file);
	}
	
	public Set<IFile> getEntryPoints() {
		return entryPoints;
	}
	
	private void loadEntryPoints() throws InvalidPropertiesFormatException, FileNotFoundException, IOException {
		String entryPointProperty = getProperty(ENTRY_POINTS_PROPERTY);
		if (entryPointProperty == null || entryPointProperty.isEmpty()) {
			return;
		}
		for (String entryPointPath : entryPointProperty.split("\\" + ENTRY_POINT_SEPARATOR)) {
			if (!entryPointPath.isEmpty()) {
				IFile file = project.getFile(entryPointPath);
				if (file.exists()) {
					entryPoints.add(file);
				}
			}
		}
	}
	

}
