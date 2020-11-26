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

package ch.uzh.ifi.attempto.acerules.help;

import java.util.ArrayList;
import java.util.HashMap;

import ch.uzh.ifi.attempto.acerules.help.page.AboutPage;
import ch.uzh.ifi.attempto.acerules.help.page.AcePage;
import ch.uzh.ifi.attempto.acerules.help.page.AnswerAreaPage;
import ch.uzh.ifi.attempto.acerules.help.page.CourteousPage;
import ch.uzh.ifi.attempto.acerules.help.page.CyclicPage;
import ch.uzh.ifi.attempto.acerules.help.page.FactExamplesPage;
import ch.uzh.ifi.attempto.acerules.help.page.IntroductionPage;
import ch.uzh.ifi.attempto.acerules.help.page.MenuBarPage;
import ch.uzh.ifi.attempto.acerules.help.page.ModePage;
import ch.uzh.ifi.attempto.acerules.help.page.NotAvailablePage;
import ch.uzh.ifi.attempto.acerules.help.page.Page;
import ch.uzh.ifi.attempto.acerules.help.page.PrioritiesPage;
import ch.uzh.ifi.attempto.acerules.help.page.ProgramAreaPage;
import ch.uzh.ifi.attempto.acerules.help.page.QuickStartPage;
import ch.uzh.ifi.attempto.acerules.help.page.RuleExamplesPage;
import ch.uzh.ifi.attempto.acerules.help.page.ShortcutsPage;
import ch.uzh.ifi.attempto.acerules.help.page.StableNegPage;
import ch.uzh.ifi.attempto.acerules.help.page.StablePage;
import ch.uzh.ifi.attempto.acerules.help.page.SyntaxBasicsPage;
import ch.uzh.ifi.attempto.acerules.help.page.TraceAreaPage;
import ch.uzh.ifi.attempto.acerules.help.page.TwoNegationsPage;
import ch.uzh.ifi.attempto.acerules.help.page.error.ContainsQuery;
import ch.uzh.ifi.attempto.acerules.help.page.error.CyclicProgram;
import ch.uzh.ifi.attempto.acerules.help.page.error.DistPluralNotSupported;
import ch.uzh.ifi.attempto.acerules.help.page.error.DoubleNegation;
import ch.uzh.ifi.attempto.acerules.help.page.error.EmptyProgPage;
import ch.uzh.ifi.attempto.acerules.help.page.error.FactHeadNegation;
import ch.uzh.ifi.attempto.acerules.help.page.error.FunctionwordApposition;
import ch.uzh.ifi.attempto.acerules.help.page.error.InexistentLabel;
import ch.uzh.ifi.attempto.acerules.help.page.error.InvalidIdentity;
import ch.uzh.ifi.attempto.acerules.help.page.error.InvalidPriorities;
import ch.uzh.ifi.attempto.acerules.help.page.error.InvalidSentence;
import ch.uzh.ifi.attempto.acerules.help.page.error.ModalityNotSupported;
import ch.uzh.ifi.attempto.acerules.help.page.error.NafInTopLevel;
import ch.uzh.ifi.attempto.acerules.help.page.error.NoAnswer;
import ch.uzh.ifi.attempto.acerules.help.page.error.NoFullStopPage;
import ch.uzh.ifi.attempto.acerules.help.page.error.NoSavedPrograms;
import ch.uzh.ifi.attempto.acerules.help.page.error.NoSpaceForPrograms;
import ch.uzh.ifi.attempto.acerules.help.page.error.NotFlatInsideOfNeg;
import ch.uzh.ifi.attempto.acerules.help.page.error.NotFlatInsideOfThen;
import ch.uzh.ifi.attempto.acerules.help.page.error.NotLiteralsInIf;
import ch.uzh.ifi.attempto.acerules.help.page.error.PrioritiesNotSupported;
import ch.uzh.ifi.attempto.acerules.help.page.error.ProgramTooLarge;
import ch.uzh.ifi.attempto.acerules.help.page.error.PropernameApposition;
import ch.uzh.ifi.attempto.acerules.help.page.error.RedefinedVariable;
import ch.uzh.ifi.attempto.acerules.help.page.error.ThatSubordNotSupported;
import ch.uzh.ifi.attempto.acerules.help.page.error.TooManySavedPrograms;
import ch.uzh.ifi.attempto.acerules.help.page.error.UnknownWord;
import ch.uzh.ifi.attempto.acerules.help.page.error.UnresolvedAnaphor;
import ch.uzh.ifi.attempto.acerules.help.page.error.ViolatedAtomRestr;
import ch.uzh.ifi.attempto.acerules.help.page.ref.Fuchs2006;
import ch.uzh.ifi.attempto.acerules.help.page.ref.Gelfond1988;
import ch.uzh.ifi.attempto.acerules.help.page.ref.Gelfond1991;
import ch.uzh.ifi.attempto.acerules.help.page.ref.Grosof1997;



public class PageManager {
	
	private static final HashMap<String,String> errorCodeMap = new HashMap<String,String>();
	private static final String[] categoryNames = new String[] {"General", "Language", "Modes", "Screen components", "References", "Errors"};
	
	static {
		errorCodeMap.put("ar:parser.meta-preprocess.EmptyProgram", "Empty program");
		errorCodeMap.put("ar:parser.meta-preprocess.NoFullStopAtEnd", "No full stop at end");
		errorCodeMap.put("ar:parser.meta-preprocess.ContainsQuery", "Program must not contain queries");
		errorCodeMap.put("ar:parser.meta-preprocess.InexistentLabel", "Label does not exist");
		errorCodeMap.put("ar:parser.priority-handler.InvalidPriorities", "Invalid priorities");
		errorCodeMap.put("ar:parser.generate-drs.InvalidSentence", "Invalid sentence");
		errorCodeMap.put("ar:parser.generate-drs.UnknownWord", "Unknown word");
		errorCodeMap.put("ar:parser.generate-drs.PropernameInApposition", "Proper name in apposition");
		errorCodeMap.put("ar:parser.generate-drs.FunctionwordInApposition", "Functionword in apposition");
		errorCodeMap.put("ar:parser.generate-drs.UnresolvedAnaphor", "Unresolved anaphor");
		errorCodeMap.put("ar:parser.generate-drs.RedefinedVariable", "Redefined variable");
		errorCodeMap.put("ar:parser.drs-check-1.NotFlatInsideOfNeg", "Not flat inside of negation");
		errorCodeMap.put("ar:parser.drs-check-1.NafInToplevel", "NAF in top level");
		errorCodeMap.put("ar:parser.drs-check-1.NotLiteralsInIf", "Not literals in if-part");
		errorCodeMap.put("ar:parser.drs-check-1.NotFlatInThen", "Not flat inside of then-part");
		errorCodeMap.put("ar:parser.drs-check-1.ModalityNotSupported", "Modality is not supported");
		errorCodeMap.put("ar:parser.drs-check-1.ThatSubordNotSupported", "That-subordination is not supported");
		errorCodeMap.put("ar:parser.drs-check-1.DistPluralNotSupported", "Distributive plural is not supported");
		errorCodeMap.put("ar:parser.condense-drs.InvalidIdentity", "Invalid identity");
		errorCodeMap.put("ar:parser.check-grouping.ViolatedAtomRestr", "Violated atom-restriction");
		errorCodeMap.put("ar:court-interpreter.answerset-generator.CyclicADG", "Cyclic program");
		errorCodeMap.put("ar:court-interpreter.cycle-checker.CyclicADG", "Cyclic program");
		errorCodeMap.put("ar:court-interpreter.eliminate-small-cycles.CyclicADG", "Cyclic program");
		errorCodeMap.put("ar:stable-interpreter.stable-checker.OverridesNotSupported", "Priorities are not supported");
		errorCodeMap.put("ar:stable-interpreter.stable-checker.NegationInFactOrHead", "Negation in fact or head of a rule");
		errorCodeMap.put("ar:stable-interpreter.stable-checker.DoubleNegation", "Double negation is not supported");
		errorCodeMap.put("ar:stable-interpreter.get-stable-model.NoModel", "Program has no answer");
	}

	private HashMap<String,Page> pages = new HashMap<String,Page>();
	private HashMap<String,ArrayList<Page>> categories = new HashMap<String,ArrayList<Page>>();
	private Page indexPage;
	
	public PageManager(HelpWindow helpWindow) {
		
		// General
		addPage(new AboutPage(helpWindow));
		addPage(new IntroductionPage(helpWindow));
		addPage(new QuickStartPage(helpWindow));
		addPage(new ShortcutsPage(helpWindow));
		
		// Language
		addPage(new AcePage(helpWindow));
		addPage(new SyntaxBasicsPage(helpWindow));
		addPage(new FactExamplesPage(helpWindow));
		addPage(new RuleExamplesPage(helpWindow));
		addPage(new TwoNegationsPage(helpWindow));
		addPage(new PrioritiesPage(helpWindow));
		addPage(new CyclicPage(helpWindow));
		
		// Modes
		addPage(new ModePage(helpWindow));
		addPage(new CourteousPage(helpWindow));
		addPage(new StablePage(helpWindow));
		addPage(new StableNegPage(helpWindow));
		
		// Screen components
		addPage(new ProgramAreaPage(helpWindow));
		addPage(new AnswerAreaPage(helpWindow));
		addPage(new TraceAreaPage(helpWindow));
		addPage(new MenuBarPage(helpWindow));
		
		// References
		addPage(new Fuchs2006(helpWindow));
		addPage(new Gelfond1988(helpWindow));
		addPage(new Gelfond1991(helpWindow));
		addPage(new Grosof1997(helpWindow));
		
		// Errors
		addPage(new EmptyProgPage(helpWindow));
		addPage(new NoFullStopPage(helpWindow));
		addPage(new ContainsQuery(helpWindow));
		addPage(new InexistentLabel(helpWindow));
		addPage(new InvalidPriorities(helpWindow));
		addPage(new InvalidSentence(helpWindow));
		addPage(new UnknownWord(helpWindow));
		addPage(new PropernameApposition(helpWindow));
		addPage(new FunctionwordApposition(helpWindow));
		addPage(new UnresolvedAnaphor(helpWindow));
		addPage(new RedefinedVariable(helpWindow));
		addPage(new NotFlatInsideOfNeg(helpWindow));
		addPage(new NafInTopLevel(helpWindow));
		addPage(new NotLiteralsInIf(helpWindow));
		addPage(new NotFlatInsideOfThen(helpWindow));
		addPage(new ModalityNotSupported(helpWindow));
		addPage(new ThatSubordNotSupported(helpWindow));
		addPage(new DistPluralNotSupported(helpWindow));
		addPage(new InvalidIdentity(helpWindow));
		addPage(new ViolatedAtomRestr(helpWindow));
		addPage(new CyclicProgram(helpWindow));
		addPage(new PrioritiesNotSupported(helpWindow));
		addPage(new FactHeadNegation(helpWindow));
		addPage(new DoubleNegation(helpWindow));
		addPage(new NoAnswer(helpWindow));
		addPage(new NoSavedPrograms(helpWindow));
		addPage(new TooManySavedPrograms(helpWindow));
		addPage(new ProgramTooLarge(helpWindow));
		addPage(new NoSpaceForPrograms(helpWindow));
		
		// Hidden
		addPage(new NotAvailablePage(helpWindow));
		
		indexPage = new Page(helpWindow, "Help index", "");
		addPage(indexPage);
		refreshIndex();
	}
	
	public Page getPage(String name) {
		if (pages.get(name) == null) {
			if (errorCodeMap.get(name) == null) {
				return pages.get("Not available");
			}
			return pages.get(errorCodeMap.get(name));
		}
		return pages.get(name);
	}
	
	public Page getIndexPage() {
		return indexPage;
	}
	
	public void refreshIndex() {
		indexPage.removeAll();
		indexPage.addGap();
		indexPage.addTitle("Help index");
		
		for (String cn : categoryNames) {
			ArrayList<Page> cl = categories.get(cn);
			if (cl == null) continue;
			indexPage.addHeading(cn);
			for (Page p : cl) {
				indexPage.addHelpLink(p.getTitle());
			}
			indexPage.addGap();
		}
	}
	
	private void addPage(Page page) {
		pages.put(page.getTitle(), page);
		ArrayList<Page> c = categories.get(page.getCategory());
		if (c == null) {
			c = new ArrayList<Page>();
			c.add(page);
			categories.put(page.getCategory(), c);
		} else {
			c.add(page);
		}
	}

}
