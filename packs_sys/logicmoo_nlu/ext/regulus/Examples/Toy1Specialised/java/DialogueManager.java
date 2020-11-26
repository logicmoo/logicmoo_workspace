import java.io.*;
import java.net.*;
import java.util.*;


/*********************************************
 *                                           *
 *  DialogueManager for the toy1app program  *
 *   - Takes in dialogueMove                 *
 *   - Returns abstractAction and outState;  *
 *                                           *
 *********************************************/
 
public class DialogueManager {

  /**********************
   *  Global variables  *
   **********************/

    private ArrayList<Object> stateArray = null;
    
  /*****************
   *  constructor  * 
   *****************/

    public DialogueManager (){
    }

    /****
     *  Initialize state:  
     *  This only needs to be intialized once per Toy1app, and only if  
     *  there is no previous state.  It doesn't need to be constructed  
     *  for each DialogueManager object, and is thus not in the constructor
     ****/

    public ArrayList<Object> initialDialogueState() {
	ArrayList<Object> kitchenLight = new ArrayList<Object>();
	kitchenLight.add("kitchen");
	kitchenLight.add("light");
	kitchenLight.add("off");
	kitchenLight.add(new Integer(0));

	ArrayList<Object> kitchenFan = new ArrayList<Object>();
	kitchenFan.add("kitchen");
	kitchenFan.add("fan");
	kitchenFan.add("off");
	kitchenFan.add(new Integer(0));

	ArrayList<Object> living_roomLight = new ArrayList<Object>();
	living_roomLight.add("living_room");
	living_roomLight.add("light");
	living_roomLight.add("off");
	living_roomLight.add(new Integer(0));
	   
	stateArray = new ArrayList<Object>();
	stateArray.add(kitchenLight);
	stateArray.add(kitchenFan);
	stateArray.add(living_roomLight);

	System.err.println("DM: stateArray: " + stateArray);
	return stateArray;
    }
    
  /***************************************************************** 
   *  Main Method called by Toy1App:                              *
   *                                                               *
   *  Takes in a move and inState, and returns a list composed of  *
   *  an abstract action (absAction) and outstate.                 *
   *****************************************************************/

    public ArrayList<Object> updateDialogueState 
	(ArrayList<Object> move, ArrayList<Object> inS) throws Exception {
	

	System.err.println("DM: inState: " + inS);
			
	//** find all the possible pairs of move and outstate
	ArrayList<Object> pairs = new ArrayList<Object>();
	pairs = (ArrayList<Object>) possibleUpdate(move, inS);
	
	//** debugging statement; only print to stderr if there are no pairs
	if (!(pairs.isEmpty())) {
	    System.err.println("DM: Pairs Separated:     ");
	    for(Iterator iterPair = pairs.iterator(); iterPair.hasNext();) {
		System.err.println("     " + iterPair.next());
	    }
	}

	//** produce a list containing the chosen abstract action and outstate 
	ArrayList<Object> actionStateList = new ArrayList<Object>();
 	actionStateList = updateState1(move, pairs, inS);

	System.err.println("DM: absAction: " + actionStateList.get(0));
	System.err.println("DM: outState: " + actionStateList.get(1));
	return actionStateList;
    }

  /************************** 
   *  Supporting Functions  * 
   **************************/


    /***
     *  Returns the abstract action (absAct) and outState depending on the 
     *  uttType, current state, and number of possible action-outState pairs
     ***/

    public ArrayList<Object> updateState1(ArrayList<Object> move,
					  ArrayList<Object> pairs,
					  ArrayList<Object> inS) {

	ArrayList<Object> absAct = new ArrayList<Object>();
	ArrayList<Object> outS = new ArrayList<Object>();
	ArrayList<Object> actionStateList = new ArrayList<Object>();

	String uttType = (String) move.get(0);

	if (uttType.equals("query") && pairs.isEmpty()) {
	    absAct.add("say");
	    absAct.add("no");
	    outS = (ArrayList<Object>) inS.clone();
	} else if (uttType.equals("command") && pairs.isEmpty()) {
	    absAct.add("say");
	    absAct.add("unable_to_interpret");
	    outS = (ArrayList<Object>) inS.clone();
	} else if (pairs.size() == 1) {
	    ArrayList<Object> act_state = (ArrayList<Object>) pairs.get(0);
	    absAct = (ArrayList<Object>) act_state.get(0);
	    outS = (ArrayList<Object>) act_state.get(1);
	} else if (pairs.size() > 1) {
	    
	    absAct.add("say");
	    absAct.add("ambiguous");
	    outS = (ArrayList<Object>) inS.clone();

	}
	    
	actionStateList.add(absAct);
	actionStateList.add(outS);
	return actionStateList;
    }

    /***
     *  returns an ArrayList of all the possible pairs of actions
     *  and their corresponding outStates for a given utterance.
     ***/

    public ArrayList<Object> possibleUpdate 
	(ArrayList<Object> move, ArrayList<Object> inS) throws Exception {

	ArrayList<Object> outS = new ArrayList<Object>();
	ArrayList<Object> pairs = new ArrayList<Object>();
	ArrayList<Object> members = new ArrayList<Object>();


	System.err.println("DM: 1-inS:" + inS);

	String uttType = (String) move.get(0);
	ArrayList<Object> pat = (ArrayList<Object>) move.get(1);
	if (pat.size() > 4) {
	    throw new IllegalArgumentException("ArrayList<Object> is larger than 4!");
	}
	//** If the move is a query, find all the dev(ices) which match pat(tern)
	if (uttType.equals("query")){
	    members = (ArrayList<Object>) findMembers(pat,inS);
	    System.err.println("DM: members " + members);
	    outS = (ArrayList<Object>) inS.clone();
	
	    //** for each member (dev that matches pat), 
	    //** add it and the old inState to the pairs list
	    for(Iterator iterMembers = members.iterator(); iterMembers.hasNext();){
		ArrayList<Object> actionStateList = new ArrayList<Object>();
		ArrayList<Object> absAct = new ArrayList<Object>();
		
		ArrayList<Object> member = (ArrayList<Object>) iterMembers.next();
		System.err.println("DM: member.next " + member);
		absAct.add("say");
		absAct.add(member);

		actionStateList.add(absAct);
		actionStateList.add(outS);
		System.err.println("DM: query ASL " + actionStateList);
		pairs.add(actionStateList);
	    
	    //System.err.println("DM: M-absAct " + absAct);
	    //System.err.println("DM: M-outS " + outS);
	}
	    
	//** If the move is a command...
	} else if (uttType.equals("command")){
	    for(Iterator iterInState = inS.iterator(); 
		iterInState.hasNext();){
		ArrayList<Object> dev = (ArrayList<Object>) iterInState.next();
		ArrayList<Object> actionStateList = new ArrayList<Object>();

		System.err.println("DM: pat "+ pat + " dev " + dev);
		if (dev.size() > 4) {
		    throw new IllegalArgumentException("ArrayList<Object> is larger than 4!");
		}


		//** ...if the item at the 2nd index of pat and dev are the same
		if (pat.get(1).equals(dev.get(1))) {
		    ArrayList<Object> absAct = new ArrayList<Object>();
		    outS = (ArrayList<Object>) inS.clone();

		    ArrayList<Object> tempDev = (ArrayList<Object>) dev.clone();

		    //** then if the first items of both are the same
		    //** and the last two items of each are not the same
		    if (((pat.get(0).equals(dev.get(0))) && 
			 (!((pat.subList(2,4)).equals(dev.subList(2,4))))) || 
			//** OR if the first pat item is "blank"
			//** and at least one of the items in pat differs from dev
			((((String) pat.get(0)).equals("blank")) && 
			 (!((pat.subList(2,4)).equals(dev.subList(2,4))))))
			{
			    //** THEN unify pat and dev 
			    absAct.add("say");
			    tempDev.set(2,pat.get(2));
			    tempDev.set(3,pat.get(3));
			    absAct.add(tempDev); 
			    outS.set((inS.indexOf(dev)),tempDev);

			    //** Add absAct-outS pair to pairs list 
			    actionStateList.add(absAct);
			    actionStateList.add(outS);
			    System.err.println("DM: actionStateList: " + actionStateList);
			    pairs.add(actionStateList);
			
		    } 

		} 
		System.err.println("DM: pat "+pat + " outS " + outS);
		//System.err.println("DM: 2-inS:" + inS);
	    } 
	}

	return pairs;
    }
	
    /***
     *	find all the devices (state in inState) which contain the pattern (pat)
     *  and return them in a list.  This is equivalent to the functions 
     *  provided by both built-in prolog functions findall/3 and members/2. 
     *  Called by possibleUpdate(ArrayList<Object> move, ArrayList<Object> inS).
     ***/

    public ArrayList<Object> findMembers (ArrayList<Object> pat, 
					 ArrayList<Object> inState){
	ArrayList<Object> members = new ArrayList<Object>();

	for(Iterator iterInState = inState.iterator(); iterInState.hasNext();){
	    ArrayList<Object> dev = (ArrayList<Object>) iterInState.next(); 
	    Boolean flag = true;
	    
	    if (dev.size() > 4) {
		throw new IllegalArgumentException("ArrayList<Object> is larger than 4!");
	    }

	    //** for each item in the pattern, if the flag is already true;
	    //** and the same item is found at the same index in dev, 
	    //** or the item is underspecified,
	    //** keep the flag true.  Otherwise, change the flag to be false.

	    for(Iterator iterPatObjs = pat.iterator(); iterPatObjs.hasNext();){
		Object patObj = (Object) iterPatObjs.next();
		if ((flag == true) && 
		    (patObj.equals(dev.get(pat.indexOf(patObj))) || 
		     patObj.equals("blank") ||
		     patObj.equals(500))){
		    flag = true;
		} else {
		    flag = false;
		}
	    }
	    
	    //** only add that dev to members if the flag is still true.
	    if (flag == true) {
		members.add(dev);
	    }
	}
	System.err.println("DM: members " + members);
	return members;
    }

}
