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

import java.util.ArrayList;


public class History {
	
	public ArrayList<String> texts;
	public int position;
	
	public History() {
		texts = new ArrayList<String>();
		texts.add("");
		position = 0;
	}
	
	public void add(String text) {
		if (!texts.get(texts.size()-1).equals(text)) {
			texts.add(text);
		}
		position = texts.size()-1;
	}
	
	public String back() {
		if (position > 0) {
			position--;
		}
		return texts.get(position);
	}
	
	public String forward() {
		if (position < texts.size()-1) {
			position++;
		}
		return texts.get(position);
	}

}
