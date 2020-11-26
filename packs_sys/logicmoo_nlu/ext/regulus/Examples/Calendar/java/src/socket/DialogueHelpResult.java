package socket;

import java.util.Vector;

/**
 * 
 * This object encapsulates information regarding the HelpSystem, i.e. 
 * the help sentences received from dialogue server .
 *
 */
public class DialogueHelpResult {

	Vector<String> helpSentList = new Vector<String>();
	
	/*
	 * Parses the string and each help sentence is added to the ArrayList.
	 */
	public DialogueHelpResult(String dialogueServerReply){
			
		if(dialogueServerReply.startsWith("help(")) {
				// test if there is only one result 
				int idxStartHelpSent = dialogueServerReply.indexOf("(") +1;
				int idxEndHelpSent = dialogueServerReply.indexOf("\\n");
				
				while (idxEndHelpSent > -1){
					String tmp = dialogueServerReply.substring(idxStartHelpSent+1, idxEndHelpSent );					
					helpSentList.add(tmp); 
										
					idxStartHelpSent = idxEndHelpSent + 1;
					idxEndHelpSent = dialogueServerReply.indexOf("\\n", idxEndHelpSent + 1);
				}
				
				idxEndHelpSent = dialogueServerReply.indexOf(")");
				String tmp = dialogueServerReply.substring(idxStartHelpSent+1, idxEndHelpSent -1 );
				helpSentList.add(tmp);							
		}
		else{
			helpSentList = null;
		}
		
	}
	
	public Vector<String> getHelpSentences(){
		return helpSentList; 
	}
			
}

