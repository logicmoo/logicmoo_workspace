import java.io.*;
import java.net.*;
import java.util.*;

/******************************************
 *                                        *
 *  InputManager for the toy1app program  *
 *   - Takes in lf and inState            *
 *   - Returns dialogueMove               *
 *                                        *
 ******************************************/

public class InputManager {

 /************************
  *  Instance variables  *
  ************************/

    private ArrayList<Object> lf;
    private ArrayList<Object> inState;
    private String uttType = "blank";
    private String device = "blank";
    private String location = "blank";
    private String onoff = "blank";
    private Integer intensity = 500;
    private String action = "blank";


  /*********************************************************
   *  constructor - empty, used only to initialize object  *
   *********************************************************/

    public InputManager() {
	//formally required, not used in this version of Toy1
	//inState = new ArrayList<Object>(); 
    }

  /*********************************************************************
   *  Main Method called by Toy1App:                                   *
   *  This method takes in the LF from the regClient Interpretation    *
   *  object and returns a dialogue move, in the form of an ArrayList  * 
   *  consisting of an utterance type (uttType) and device ArrayList   *
   *  [location,devType,OnOff,Intensity].                              *
   *********************************************************************/
   
    public ArrayList<Object> lfToDialogueMove(ArrayList<Object> lf) throws Exception { 
	//ArrayList<Object> inState
	//formally required object in prolog, unused in this Toy1app
    
	this.lf = lf;
	System.err.println("IM: LF: " + lf);

	
	ArrayList<Object> dev = new ArrayList<Object>();
	dev.add(getLocation());
	dev.add(getDeviceType());
	dev.add(getOnOff());
	dev.add(getIntensity());

	ArrayList<Object> move = new ArrayList<Object>();
	move.add(getDoOrQuery());
	move.add(dev);

	System.err.println("IM: move: " + move);
	return (ArrayList<Object>) move;
    }

  /*************************************************************** 
   *  Supporting Functions                                       *
   *  NB. "Blank" and "500" represent underspecified variables.  * 
   ***************************************************************/
    
    /***
     *  returns the location (i.e. kitchen/living room/blank) from the recognition LF  
     ***/

    public String getLocation(){
	String location = "blank";
	
	for(Iterator iterVariables = lf.iterator(); iterVariables.hasNext();){
	    ArrayList<Object> variable = (ArrayList<Object>) iterVariables.next();
	    if (((String) variable.get(0)).equals("location")){
		location = (String) variable.get(1);
		break;
		    }
	}
	    
	System.err.println("IM: Location from in IM: " + location);
	this.location = location;
	return location;
    }
    
    /***
     *  returns the device type (i.e. light/fan/blank) from the recognition LF  
     ***/

    public String getDeviceType(){
	String device = "blank";
	
	for(Iterator iterVariables = lf.iterator(); iterVariables.hasNext();){
	    ArrayList<Object> variable = (ArrayList<Object>) iterVariables.next();
	    if (((String) variable.get(0)).equals("device")){
		device = (String) variable.get(1);
		break;
		    }
	}
	    
	System.err.println("IM: Device from in IM: " + device);
	this.device = device;
	return device;
    }

    /***
     *  returns the on/off status (i.e. on/off/blank) from the recognition LF  
     ***/

    public String getOnOff(){
	String onoff = "blank";
	String action = "blank";
	
	//** if onoff status was specified by user, set global onoff variable to its value
	for(Iterator iterVariables = lf.iterator(); iterVariables.hasNext();){
	    ArrayList<Object> variable = (ArrayList<Object>) iterVariables.next();
	    if (((String) variable.get(0)).equals("onoff")){
		onoff = (String) variable.get(1);
		break;
		    }
	}

	//** if the preceding loop fails and action == dim, set global onoff to "on"
	if (onoff == "blank") {
	    for(Iterator iterVariables = lf.iterator(); iterVariables.hasNext();){
		ArrayList<Object> variable = (ArrayList<Object>) iterVariables.next();
		if (((String) variable.get(0)).equals("action")){
		    if (((String) variable.get(1)).equals("dim")){
			onoff = "on";
		    }
		    break;
		}
	    }
	    
	}
	    
	System.err.println("IM: Onoff from in IM: " + onoff);
	this.onoff = onoff;
	return onoff;
    }


    /***
     *  This method returns the intensity status (i.e. 0/50/100/500) from the LF  
     ***/
    
    public Integer getIntensity(){
	Integer intensity = 500;

 	String uttType = "blank";
	String action = "blank";
	
 	for(Iterator iterVariables = lf.iterator(); iterVariables.hasNext();){
	    ArrayList<Object> variable = (ArrayList<Object>) iterVariables.next();

	    //**set local values for uttType and action
	    if (((String) variable.get(0)).equals("utterance_type")) {
		uttType = (String) variable.get(1);
	    } else if (((String) variable.get(0)).equals("action")){
		action = (String) variable.get(1);
	    }
		
 	}		
	
	System.err.println("IM: UttType: " + uttType);
	System.err.println("IM: onoff: " + onoff);
	System.err.println("IM: ACTION: " + action);

	
	if (onoff.equals("on")) { //** for "on" cases 
	    if (uttType.equals("command")){  //** command && on
		if (action.equals("dim")) {
		    intensity = 50;
		} else {
		    intensity = 100;
		}
	    } else if (uttType.equals("query")) { //** query && on
		intensity = 500; 
	    }
	} else if (onoff.equals("off")) { //** for all "off" cases
	    intensity = 0;
	} 

	System.err.println("IM: Intensity from in IM: " + intensity);
	this.intensity = intensity;
	return intensity;
    }


    /***
     *  This method returns the utterance type (i.e command/query).
     ***/

    public String getDoOrQuery(){
	String uttType = "blank";
	
	for(Iterator iterVariables = lf.iterator(); iterVariables.hasNext();){
	    ArrayList<Object> variable = (ArrayList<Object>) iterVariables.next();
	    if (((String) variable.get(0)).equals("utterance_type")){
		uttType = (String) variable.get(1);
		break;
		    }
	}
	    
	System.err.println("IM: Utterance type from in IM: " + uttType);
	this.uttType = uttType;
	return uttType;
    }

} 
