package RegulusGUI;

import se.sics.prologbeans.PBString;
import se.sics.prologbeans.Term;

public class RegulusSummaryItem {

	/*
	 
	Class used to represent Regulus summary items.  

	Example:

	Form:  .MAIN-->switch,on,the,light
	Tag:   parse
	Sem:   concat([[utterance_type,command]],concat([[action,switch]],concat([[onoff,on]],[[device,light]])))
	Feats: []
	Tree:

	.MAIN (node 1) [TOY1_RULES:1-5]
	   utterance (node 2) [TOY1_RULES:6-10]
	      command (node 3) [TOY1_RULES:11-15]
	      /  verb lex(switch) (node 4) [TOY1_LEXICON:7-9]
	      |  onoff lex(on) (node 5) [TOY1_LEXICON:23-24]
	      |  np (node 6) [TOY1_RULES:26-30]
	      |  /  lex(the)
	      \  \  noun lex(light) (node 7) [TOY1_LEXICON:15-16]
	      
	------------------------------- FILES -------------------------------

	TOY1_LEXICON: c:/cygwin/home/speech/regulus/examples/toy1/regulus/toy1_lexicon.regulus
	TOY1_RULES:   c:/cygwin/home/speech/regulus/examples/toy1/regulus/toy1_rules.regulus      
	      
	cat = ".MAIN"
	tag = "parse"
	sem = "concat([[utterance_type,command]],concat([[action,switch]],concat([[onoff,on]],[[device,light]])))"
	features = "[]"
	form = ".MAIN-->switch,on,the,light"
	tree = RegulusTree representing the tree

	*/

	private String cat;
	private String tag;
	private String sem;
	private String features;
	private String form;
	private RegulusTree tree;
		
	public String getCat() {
		return cat;
	}
	
	public String getTag() {
		return tag;
	}
				
	public String getSem() {
		return sem;
	}
		
	public String getFeatures() {
		return features;
	}
		
	public String getForm() {
		return form;
	}
				
	public RegulusTree getTree() {
		return tree;
	}
				
	public boolean isLex() {
		return tree.isLex();
	}
		
	public void setCat(String c) {
		cat = c;
	}
	
	public void setTag(String t) {
		cat = t;
	}
				
	public void setSem( String s) {
		sem = s;
	}
		
	public void setFeatures( String s) {
		features = s;
	}
		
	public void setForm( String s ) {
		form = s;
	}
				
	public void setTree( RegulusTree a ) {
		tree = a;
	}
	
	/*
	item(TagString,    1
	     CatString     2
		 SemString,    3
		 FeatsString,  4
		 FormString,   5
		 GUITree)      6
	*/
	
	public void initFromProlog(Term prologItem) {
		if ( prologItem!= null && prologItem.isCompound() && prologItem.getArity() == 6 ) {
			
			setTag( unpackPBString(prologItem.getArgument(1)) );
			setCat( unpackPBString(prologItem.getArgument(2)) );
			setSem( unpackPBString(prologItem.getArgument(3)) );
			setFeatures( unpackPBString(prologItem.getArgument(4)) );
			setForm( unpackPBString(prologItem.getArgument(5)) );
			
			RegulusTree newTree = new RegulusTree();
			setTree( newTree );
			newTree.initFromProlog( prologItem.getArgument(6) );
			}
		}
	
	private static String[][] conversionTable = 
	{
		{"NL", "\n"},
		{"a1", "á"},
	    {"a2", "â"},
		{"a3", "à"},
		{"a4", "ä"},
		{"a5", "å"},

		{"c1", "ç"},

		{"e1", "é"},
		{"e2", "ê"},
		{"e3", "è"},
		{"e4", "ë"},
		{"e6", "æ"},

		{"i1", "í"},
		{"i2", "î"},
		{"i3", "ì"},
		{"i4", "ï"},

		{"n1", "ñ"},

		{"o1", "ó"},
		{"o2", "ô"},
		{"o3", "ò"},
		{"o4", "ö"},

		{"u1", "ú"},
		{"u2", "û"},
		{"u3", "ù"},
		{"u4", "ü"},
	};

	private static String unpackPBString(Term PBStringTerm) {
		if ( PBStringTerm == null ) {
			return null;
		}
		else if ( !PBStringTerm.isString() ) {
			return PBStringTerm.toString();
		}
		else {
			String s = ((PBString) PBStringTerm).getString();
			if ( s.equals("*empty_string*") ) {
				return "";
			}
			else {
				String s1;
				for ( int i = 0; i < conversionTable.length; i++ ) {
					String fromString = "!" + conversionTable[i][0] + "!";
					String toString = conversionTable[i][1];
					s1 = s.replaceAll(fromString, toString);
					s = s1;
				}
				return s;
			}
		}
	}
	
	public String toString() {
		String s = "";
		s += "\nForm = " + getForm();
		s += "\nTag =  " + getTag();
		s += "\nCat = " + getCat();
		s += "\nSem = " + getSem();
		s += "\nFeatures = " + getFeatures();
		s += "\nTree = \n";
		s += getTree().toString();
		return s;
	}
	
	public String toStringNoTree() {
		String s = "";
		s += "\nForm = " + getForm();
		s += "\nTag =  " + getTag();
		s += "\nCat = " + getCat();
		s += "\nSem = " + getSem();
		s += "\nFeatures = " + getFeatures();
		return s;
	}
	
	public boolean containsCut() {
		return getTree().containsCut();
	}
		
}


