package org.cs3.plbenchmarks.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.util.ArrayList;
import java.util.List;

import org.cs3.plbenchmarks.data.Configuration;

public class IOUtils {
	
	public static List<Configuration> loadConfigsFromFile(File f) {
		List<Configuration> configs = new ArrayList<>();
		try (FileInputStream fis = new FileInputStream(f); ObjectInputStream ois = new ObjectInputStream(fis)) {
			int size = ois.readInt();
			for (int i=0; i<size; i++) {
				Object o = ois.readObject();
				if (o instanceof Configuration) {
					configs.add((Configuration) o);
				} else {
					System.err.println("non-valid object in config file");
				}
			}
		} catch (IOException | ClassNotFoundException e) {
			e.printStackTrace();
		}
		return configs;
	}
}
