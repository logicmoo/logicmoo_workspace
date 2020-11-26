import java.io.*;
import java.net.*;
import java.util.*;

/*******************************************
 *                                         *  
 *  OutputManager for the toy1app program  *
 *   - Takes in abstractAction             *
 *   - Returns concreteAction              *
 *                                         *  
 *******************************************/


public class OutputManager {

  /***************** 
   *  Constructor  *
   *****************/

    public OutputManager (){
    }


  /******************************************************************
   *  Main method:                                                   *
   *  This method takes in an abstract action, and converts it to a  *
   *  concrete action, which in this case is a string to be sent to  * 
   *  the TTS.                                                       *
   *******************************************************************/

    public String abstractActionToAction (ArrayList<Object> absAction) 
	throws Exception {
	ArrayList<Object> wordlist = new ArrayList<Object>();  
	String ttsString = new String();

	wordlist = generationGrammar(absAction);
	System.err.println("OM: wordlist " + wordlist);
	ttsString = joinWithSpacesTTS(wordlist);
	System.err.println("OM: ttsString " + ttsString);

	return ttsString;
    }


  /************************
   *  Supporting Methods  *
   ************************/


    /***
     *  This method takes in an ArrayList of strings and concatenates them 
     *  with spaces between each string, as well as prepending the TTS 
     *  switch prefix.
     ***/

    public String joinWithSpacesTTS(ArrayList<Object> wordlist) {
	String ttsString = ("-tts_text:");
	
	if (wordlist.size() > 1) {
	    for(Iterator iterWords = wordlist.iterator(); iterWords.hasNext();) {
		String word = (String) iterWords.next();
		//System.err.println("OM: word " + word);
		ttsString = ttsString.concat(" ");
		ttsString = ttsString.concat((String) word);
	    }
	} else {
	    ttsString = ttsString.concat(" ");
	    ttsString = ttsString.concat((String) wordlist.get(0));
	}

	ttsString = ttsString.concat(".");
	return ttsString;
    }

    /***
     *  This method takes in the absAction and if absAction contains only 
     *  one concept, it matches that concept to an output phrase (string).
     *  Otherwise, if the absAction contains a dev, this method returns a 
     *  template answer:  "<devType> in <location> is <Intensity>".  
     ***/

    public ArrayList<Object> generationGrammar (ArrayList<Object> absAction) 
	throws Exception {

 	ArrayList<Object> wordlist = new ArrayList<Object>();  

 	System.err.println("OM: absAction " + absAction);

	if (absAction.get(1).equals("no")) {
	    wordlist.add("no");
	} else if (absAction.get(1).equals("unable_to_interpret")) {
	    wordlist.add("sorry that doesn't make sense");
	} else if (absAction.get(1).equals("ambiguous")) {
	    wordlist.add("sorry that's ambiguous");
	} else if (((ArrayList<Object>) absAction.get(1)).size() == 4) {
	    
	    ArrayList<Object> dev = (ArrayList<Object>) absAction.get(1);  
	    wordlist.add(generationGrammer1(dev.get(1)));
	    wordlist.add("in");
	    wordlist.add(generationGrammer1(dev.get(0)));
	    wordlist.add("is");
	    wordlist.add(generationGrammer1(dev.get(3)));
	} else {
	    throw new IllegalArgumentException("This is not a valid absAction!  absAction = " + absAction);
	}
	return wordlist;
    }
    

    /***
     *  This method provides the appropriate lexical item (string) 
     *  for the variable value provided by dev in the method,
     *  generationGrammer(ArrayList<Object> absAction)
     ***/

    public String generationGrammer1 (Object absActionAtom) {
	String words = new String();

	if (absActionAtom.equals("light")) {
	    words = "the light";	
	} else if (absActionAtom.equals("fan")) {
	    words = "the fan";		
	} else if (absActionAtom.equals("kitchen")) {
	    words = "the kitchen";		
	} else if (absActionAtom.equals("living_room")) {
	    words = "the living room";		
	} else if (absActionAtom.equals((int) 0)) {
	    words = "off";		
	} else if (absActionAtom.equals((int) 100)) {
	    words = "on";
	} else if (absActionAtom.equals((int) 50)) {
	    words = "dimmed";
	}
	return words;
    }
}//e end of class 
