package RegulusGUI;

import se.sics.prologbeans.PBString;
import se.sics.prologbeans.Term;

/*
 * 
 * Used to return information from a dialogue processing operation. In the command-line 
 * version of the top-level, this information would be presented as in the
 * following example:
 * 
 * 
 * 

>> when was the last meeting

      Old state: []
             LF: [[whq,form(past,[[be,term(the_last,meeting,[]),[[time,when]]]])]]
    Resolved LF: [[whq,form(past,[[be,term(the_last,meeting,[]),[[time,when]]]])]]
     Resolution: [trivial]
  Dialogue move: [query_object=when, tense_information=referent(past), utterance_type=whq, 
                  aggregate(last_n_meetings(1),[])]
  Resolved move: [query_object=when, 
                  (tense_information = 
                   interval(datime(1980,0,0,0,0,0),datime(2007,11,29,19,57,15))),
                  utterance_type=whq, aggregate(last_n_meetings(1),[])]
Abstract action: say(referent_list([attribute(meeting,meeting_10,when)]))
Concrete action: tts(from 10 00 to 18 00 on november 25)
      New state: [lf=[[whq,form(past,[[be,term(the_last,meeting,[]),[[time,when]]]])]], 
                  referents=[record(meeting,meeting_10),attribute(meeting,meeting_10,when)]]

 */

public class DialogueProcessingResult {
	
	private String old_state;
	private String source;
	private String lf;
	private String resolved_lf;
	private String resolution;
	private String dialogue_move;
	private String resolved_dialogue_move;
	private String paraphrase;
	private String abstract_action;
	private String concrete_action;
	private String new_state;
	
	public String getSource() {
		return source;
	}
	
	public String getOldState() {
		return old_state;
	}
	
	public String getLF() {
		return lf;
	}
	
	public String getResolvedLF() {
		return resolved_lf;
	}
	
	public String getResolution() {
		return resolution;
	}
	
	public String getDialogueMove() {
		return dialogue_move;
	}
	
	public String getResolvedDialogueMove() {
		return resolved_dialogue_move;
	}
	
	public String getParaphrase() {
		return paraphrase;
	}
	
	public String getAbstractAction() {
		return abstract_action;
	}
	public String getConcreteAction() {
		return concrete_action;
	}
	
	public String getNewState() {
		return new_state;
	}
	
	
	public void setSource(String s) {
		source = s;
	}
	
	public void setOldState(String s) {
		old_state = s;
	}
	
	public void setLF(String s) {
		lf = s;
	}
	
	public void setResolvedLF(String s) {
		resolved_lf = s;
	}
	
	public void setResolution(String s) {
		resolution = s;
	}
	
	public void setDialogueMove(String s) {
		dialogue_move = s;
	}
	
	public void setResolvedDialogueMove(String s) {
		resolved_dialogue_move = s;
	}
	
	public void setParaphrase(String s) {
		paraphrase = s;
	}
	
	public void setAbstractAction(String s) {
		abstract_action = s;
	}
	
	public void setConcreteAction(String s) {
		concrete_action = s;
	}
	
	public void setNewState(String s) {
		new_state = s;
	}
	
	public void setKeyValue(String key, String value) {
		if ( key.equals("sent") ) {
			setSource(value);
			}
		else if ( key.equals("in_state") ) {
			setOldState(value);
			}
		else if ( key.equals("parse") ) {
			setLF(value);
			}
		else if ( key.equals("resolved_lf") ) {	
			setResolvedLF(value);
			}
		else if ( key.equals("resolution") ) {
			setResolution(value);
			}
		else if ( key.equals("dialogue_move") ) {
			setDialogueMove(value);
			}
		else if ( key.equals("resolved_dialogue_move") ) {
			setResolvedDialogueMove(value);
			}
		else if ( key.equals("paraphrase") ) {
			setParaphrase(value);
			}
		else if ( key.equals("abstract_action") ) {
			setAbstractAction(value);
			}
		else if ( key.equals("action") ) {
			setConcreteAction(value); 
			}
		else if ( key.equals("out_state") ) {
			setNewState(value);
			}
		}
	
	public String toString() {
		String s = "";
		
		s += "\nsource:\n";
		s += getSource();
			
		s += "\n\nold state:\n";
		s += getOldState();
		
		s += "\n\nlf:\n";
		s += getLF();
			
		s += "\n\nresolved_lf:\n";	
		s += getResolvedLF();

		s += "\n\nresolution:\n";
		s += getResolution();

		s += "\n\ndialogue_move:\n";
		s += getDialogueMove();
		
		s += "\n\nresolved_dialogue_move:\n";
		s += getResolvedDialogueMove();
		
		s += "\n\nparaphrase:\n";
		s += getParaphrase();
		
		s += "\n\nabstract_action:\n";
		s += getAbstractAction();

		s += "\n\nconcrete_action:\n";
		s += getConcreteAction();

		s += "\n\nnew_state:\n";
		s += getNewState();

		return s;
	}
	
	public boolean initFromProlog(Term prologItem) {
		if ( prologItem!= null && prologItem.isCompound() ) {
			int arity = prologItem.getArity();
			for ( int i = 1; i <= arity; i++ ) {
				Term arg = prologItem.getArgument(i);
				if ( arg != null && arg.isCompound() && arg.getArity() == 2 ) {
					String key = unpackPBString(arg.getArgument(1));
					String value = unpackPBString(arg.getArgument(2));
					setKeyValue(key, value);
				}
			}
			return true;
		}
		else {
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
