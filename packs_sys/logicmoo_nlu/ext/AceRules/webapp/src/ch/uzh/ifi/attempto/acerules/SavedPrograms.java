// This file is part of AceRules.
// Copyright 2008-2012, Tobias Kuhn, http://www.tkuhn.ch
//
// AceRules is free software: you can redistribute it and/or modify it under the terms of the GNU
// Lesser General Public License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// AceRules is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
// even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with AceRules. If
// not, see http://www.gnu.org/licenses/.

package ch.uzh.ifi.attempto.acerules;

import java.util.Arrays;

import javax.servlet.http.Cookie;

import nextapp.echo2.app.ApplicationInstance;
import nextapp.echo2.webcontainer.ContainerContext;
import nextapp.echo2.webcontainer.command.BrowserSetCookieCommand;


public class SavedPrograms {
	
	private String[] names = new String[10];
	private String[] programs = new String[10];
	private int programCount = 0;
	
	public SavedPrograms() {
		ContainerContext cc = (ContainerContext) ApplicationInstance.getActive().getContextProperty(ContainerContext.CONTEXT_PROPERTY_NAME);
		Cookie[] cookies = cc.getCookies();
		for (Cookie c : cookies) {
			if (c.getName().startsWith("SavedProgram")) {
				String value = c.getValue().replace('&', '\n');
				if (value.length() == 0) continue;
				int nameIndex = value.indexOf("\n");
				if (nameIndex == -1) continue;
				if (programCount > 9) break;
				String name = value.substring(0, nameIndex);
				if (existsProgram(name)) continue;
				names[programCount] = name;
				programs[programCount] = value.substring(nameIndex + 1);
				programCount++;
			}
		}
		storeAllCookies();
	}
	
	public void addProgram(String name, String program) {
		if (isFull()) return;
		if (program.length() > 3950) return;
		if (getFreeSpace() < program.length()) return;
		String nameC = name.replace('&', ' ').replace('\n', ' ');
		if (existsProgram(nameC)) return;
		programs[programCount] = program;
		names[programCount] = nameC;
		storeCookie(programCount);
		programCount++;
	}
	
	public boolean isEmpty() {
		return programCount < 1;
	}
	
	public boolean isFull() {
		return programCount > 9;
	}
	
	public int getFreeSpace() {
		int fs = 7000;
		for (String p : programs) {
			if (p == null) continue;
			fs -= p.length();
		}
		return fs;
	}
	
	public boolean existsProgram(String name) {
		if (name == null) return false;
		String nameC = name.replace('&', ' ').replace('\n', ' ');
		for (String n : names) {
			if (nameC.equals(n)) return true;
		}
		return false;
	}
	
	public String getProgram(String name) {
		if (name == null) return "";
		for (int i=0 ; i<10 ; i++) {
			if (name.equals(names[i])) return programs[i];
		}
		return "";
	}
	
	public String[] getProgramNames() {
		String[] r = new String[programCount];
		for (int i=0 ; i<programCount ; i++) {
			r[i] = names[i];
		}
		Arrays.sort(r);
		return r;
	}
	
	public void deleteProgram(String name) {
		if (name == null) return;
		for (int i=0 ; i<programCount ; i++) {
			if (name.equals(names[i])) {
				for (int j=i+1 ; j<programCount ; j++) {
					names[j-1] = names[j];
					programs[j-1] = programs[j];
				}
				programCount--;
				names[programCount] = null;
				programs[programCount] = null;
				storeAllCookies();
				return;
			}
		}
	}
	
	private void storeCookie(int i) {
		String program = programs[i];
		String value = "";
		if (program != null && program.length() > 0) {
			value = names[i] + "&" + program.replace('\n', '&');
		}
		Cookie cookie = new Cookie("SavedProgram" + i, value);
		cookie.setMaxAge(1000000000);
		ApplicationInstance.getActive().enqueueCommand(new BrowserSetCookieCommand(cookie));
	}
	
	private void storeAllCookies() {
		for (int i=0 ; i<10 ; i++) {
			storeCookie(i);
		}
	}

}
