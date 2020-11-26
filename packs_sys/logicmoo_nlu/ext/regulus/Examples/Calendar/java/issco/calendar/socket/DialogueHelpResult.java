package issco.calendar.socket;

/**
 * 
 * This object encapsulates information regarding the HelpSystem, i.e. 
 * the help sentences received from dialogue server .
 *
 */
public class DialogueHelpResult {

	java.util.ArrayList<String> helpSentList = new java.util.ArrayList<String>();
	
	/*
	 * Creates a RegResultNBest object . Override the RegResult constructor, which 
	 * extracts recognition and interpretation strings out of the regulus server reply.
	 * The interpretationObject is created on demand.
	 */
	public DialogueHelpResult(String dialogueServerReply){
		// System.out.println("Debug only : " + dialogueServerReply);
			
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
	
	public java.util.ArrayList<String> getHelpSentences(){
		return helpSentList; 
	}
	
	public static void main(String[] args){
		DialogueHelpResult test = new DialogueHelpResult("help('who was at the last meeting\nwhere was the last meeting\nwhen was the last meeting in geneva\nwhen was the last meeting in england\nwhen was the last meeting').");
		java.util.Iterator it = test.helpSentList.iterator();
		while(it.hasNext()){
			System.out.println(it.next());
		}
	}
		
}

