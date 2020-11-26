package RegulusGUI;

import se.sics.prologbeans.*;

/*
 
Class used to represent Regulus tree structures.  

In general, a RegulusTree is recursive. The daughter field is an array containing the
RegulusTrees under the current one. 

If the tree is lexical, the lex field should contain a string, and the daughters field should be null.
If the tree is not lexical, the lex field should be null, and the daughters field should contain an array.

Example: tree for "switch on the light" in the Toy1 grammar

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
nodeNumber = 1
file = "c:/cygwin/home/speech/regulus/examples/toy1/regulus/toy1_rules.regulus"
startLine = 1
endLine = 5
daughters = RegulusTree array, one element for RegulusTree at node 2
nDaughters = 1
lex = null

*/

public class RegulusTree {

	private String cat;
	private int nodeNumber;
	private String file;
	private int startLine;
	private int endLine;
	private RegulusTree[] daughters;
	private int nDaughters;
	private String lex;
	private boolean cut;
	
	
	public String getCat() {
		return cat;
	}
	
	public int getNodeNumber() {
		return nodeNumber;
	}
		
	public String getFile() {
		return file;
	}
	
	public int getStartLine() {
		return startLine;
	}
	
	public int getEndLine() {
		return endLine;
	}
	
	public RegulusTree[] getDaughters() {
		return daughters;
	}
	
	public int getNDaughters() {
		return nDaughters;
	}
	
	public String getLex() {
		return lex;
	}
	public boolean getCut() {
		return cut;
	}

	
	public boolean isLex() {
		return ( lex != null );
	}
	
	public void setCat(String c) {
		cat = c;
		
	}
	
	public void setNodeNumber( int n) {
		nodeNumber = n;
		
	}
		
	public void setFile( String s) {
		file = s;
	}
	
	public void setStartLine( int i ) {
		startLine = i;
	}
	
	public void setEndLine( int i ) {
		endLine = i;
	}
	
	public void setDaughters( RegulusTree[] a ) {
		daughters = a;
		
	}
	
	public void setNDaughters( int i ) {
		nDaughters = i;
	}
	
	public void setLex( String s ) {
		lex = s;
	}
	
	public void setCut( boolean c){
		cut = c;
	}
	
	
	/*
	tree(CatString,    1
         NodeNumber,   2
         Cut,          3
	     FileString,   4
	     StartLine,    5
	     EndLine,      6
	     Lex,          7
	     NDaughters,   8
	     GUIDaughters) 9
	 */
	
	public void initFromProlog(Term prologTree) {
		if ( prologTree!= null && prologTree.isCompound() && prologTree.getArity() == 9 ) {
				Term cutTerm = prologTree.getArgument(3);
				String cut0 = unpackPBString(cutTerm);
				if ( cut0.equals("cut") ) {
					setCut(true);
				} else {
					setCut(false);
				}
				Term lexTerm = prologTree.getArgument(7);
				//String lex0 = ((PBString) lexTerm).toString();
				String lex0 = unpackPBString(lexTerm);
				if ( !lex0.equals("n/a") ) {
					setLex(lex0);
				}
				else {
					setCat( ((PBString) prologTree.getArgument(1)).toString() );
					setNodeNumber( ( prologTree.getArgument(2)).intValue() );
					setFile( ((PBString) prologTree.getArgument(4)).toString() );
					setStartLine( ( prologTree.getArgument(5)).intValue() );
					setEndLine( ( prologTree.getArgument(6)).intValue() );
					setLex( null );
					setNDaughters( ( prologTree.getArgument(8)).intValue() );
				
					Term daughtersTerm = prologTree.getArgument(9);
					setDaughters( new RegulusTree[ daughtersTerm.getArity() ] );
					for ( int i = 0 ; i < daughtersTerm.getArity() ; i++ ) {
						daughters[i] = new RegulusTree();
						daughters[i].initFromProlog( daughtersTerm.getArgument( i + 1 ) );
					}
				}
			}
	}
	
	public String toString() {
		return toString(0);
	}
	
	public String toString(int indent) {
		String s = "";
		for (int i = 0; i < indent; i++ ) {
			s += " ";
		}
		if ( getLex() == null ) {
			if ( getCut() ) {
				s += ( getCat() + " (node " + getNodeNumber() + " CUT ) " );
			} else {
				s += ( getCat() + " (node " + getNodeNumber() + " ) " );
			}
			s += ( getFile() + " lines " + getStartLine() + "-" + getEndLine() + "\n" );
			for ( int i = 0; i < getNDaughters() ; i++ ) {
				s += getDaughters()[i].toString(indent + 3);
			}
		}
		else {
			s += "lex(" + getLex() + ")\n";
		}
		return s;
	}
	
	public boolean containsCut() {
		// If it's a lex node
		if ( isLex() ) {
			// then return 'true' if the lex value is 'CUT'
			if ( getLex().equals("CUT") ) {
				return true;
			}
			// otherwise return 'false'
			else {
				return false;
			}
		}
		// If it's not a lex node, it must have daughters
		// Use i to loop through the daughters
		else {
			for ( int i = 0 ; i < getNDaughters() ; i++ ) {
				// daughter is the i-th daughter
				RegulusTree daughter = getDaughters()[i];
				// if that daughter contains a cut, then return 'true'
				if ( daughter.containsCut() ) {
					return true;
				}
			}
			// If we got here, none of the daughters contained a cut
			// so return 'false'
			return false;
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

}


