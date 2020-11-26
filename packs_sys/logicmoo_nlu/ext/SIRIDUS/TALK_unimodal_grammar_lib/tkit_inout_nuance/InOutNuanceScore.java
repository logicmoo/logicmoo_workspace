/*
  Trindikit ASR+TTS agent for Nuance ASR and Nuance Vocalizer
  Author: David Hjelm

*/


import com.sri.oaa2.com.*;
import com.sri.oaa2.icl.*;
import com.sri.oaa2.lib.*;
import vcommerce.core.sc.*;
import vcommerce.core.util.*;
import nuance.core.sc.*;
import nuance.core.util.*;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.util.*;

public class InOutNuanceScore 
    extends JFrame 
{
    
    String agentName =  "InOutNuanceScore";

    LibOaa liboaa;
    String output_delay_id="0";
    HashMap saved_calls=new HashMap();
    int delay_id_counter=0;
    private int laps;

   
    JLabel status_lbl,input_lbl,output_lbl;
   
    NuanceSpeechChannel nsc;

       
    public static void main(String[] args){
	new InOutNuanceScore(args);
    }

    public InOutNuanceScore(String[] args){
	super("Nuance Speech Rec");
     
	//create graphics
	setDefaultCloseOperation(EXIT_ON_CLOSE);
	setLayout(new BorderLayout());
	status_lbl = new JLabel("Closed");
 	add(BorderLayout.NORTH,status_lbl);
	input_lbl = new JLabel("INPUT : ");
 	add(BorderLayout.CENTER,input_lbl);
	output_lbl = new JLabel("OUTPUT: ");
	add(BorderLayout.SOUTH,output_lbl);
	pack();
	setVisible(true);
	
	//create speech channel
	

	try{
	    NuanceConfig nc=new NuanceConfig(args);
	    nsc=new  NuanceSpeechChannel(nc);
	    System.out.println("Grammars: "+nsc.getAllGrammars().elementAt(0));
	    //nsc.setFloatParameter("client.NoSpeechTimeoutSecs",0.5f);
	   
	    if(nsc.getTelephonyControl()!=null){
		status_lbl.setText("Waiting for a call");
		nsc.getTelephonyControl().waitForCall();
		nsc.getTelephonyControl().answerCall();
	    }
	    status_lbl.setText("Initialized");
	}
	catch(Exception e){
	    e.printStackTrace();
	    System.exit(0);
	}

	//connect to oaa
		liboaa=new LibOaa(new LibCom(new LibComTcpProtocol(), 
				     nsc.getRemainingArgumentList()));
	if (!liboaa.oaaSetupCommunication(agentName)) {
            System.err.println("Couldnt connect to facilitator");
            System.exit(0);
        }
	liboaa.oaaRegisterCallback("oaa_AppDoEvent",
	      new OAAEventListener() {
		  public boolean doOAAEvent(IclTerm goal, 
					    IclList params, 
					    IclList answers) {
		      return handle(goal, params, answers);
		  }
            });
	String slvstr= "["+ 
	    "tkit_call_module(input,init),"+
	    "tkit_call_module(input,input),"+
	    "tkit_call_module(input,quit),"+
	     "tkit_call_module(output,init),"+
	    "tkit_call_module(output,output),"+
	    "tkit_call_module(output,quit)"+
	    "]";
	IclList solvables=(IclList)IclTerm.fromString(true,slvstr);  
	if (!liboaa.oaaRegister("parent", agentName, 
				solvables, new IclList())) {
            System.err.println("Could not register");
            System.exit(0);
        }
	liboaa.oaaReady(true);
    }
 

    public boolean handle(IclTerm goal, IclList params, IclList answers) {
	System.out.println("Received "+goal);
	try{
	    if(!((IclStruct)goal).getFunctor().equals("tkit_call_module"))
		return false;
	    
	    //sloppy programming, we know that only correct solvables ever
	    //get this far. however they can be variables, in that case
	    //ClassCastException is thrown
	    String arg1= ((IclStr)goal.getTerm(0)).toUnquotedString();
	    String arg2= ((IclStr)goal.getTerm(1)).toUnquotedString();
	    //we only need to handle output:output and input:input
	    if(arg1.equals("output") && arg2.equals("output"))
		output();
	    if(arg1.equals("input") && arg2.equals("input"))
		input();
	    //we assume it always succeeds
	    answers.add(goal);
	    return true;
	}
	catch(ClassCastException e){
	    return false;
	}
	


    }


    void input(){
	String text="";
	double conf=0.0;
	String g = (String)nsc.getAllGrammars().elementAt(0);
	status_lbl.setText("recognizing with "+g);
	try{
	    RecResult rr=nsc.playAndRecognize(g);
	    if(rr.isNormalRecognition()){
		text = rr.getSingleResult(0).getRecognizedString();
		conf =  ((double)rr.getSingleResult(0).getConfidence())/100.0;
		status_lbl.setText("recognition succeded");
	    }
	    else if(rr.isRejection()){
		text = "'FAIL'";
		conf = 0.0;   
		status_lbl.setText("recognition rejected");
	    }
	    else if(rr.isNoSpeechTimeout()){

		text="";
		conf=1.0;
		status_lbl.setText("no speech timeout");
	    }
	    else if(rr.isTooMuchSpeechTimeout()){

		text="";
		conf=1.0;
		status_lbl.setText("too much speech timeout");
	    }	

	    else{
		text="";
		conf=1.0;
		status_lbl.setText("recognition error");
	    }
	    
	    input_lbl.setText("INPUT: "+text+" / "+conf);
	    setScoreAndInput(conf,text);
	}
	catch(Exception e){
	    status_lbl.setText(e.getMessage());
	    setScoreAndInput(1.0,"");
	}
    }
    

    void output(){
	try{
	String text = readOutput();
	output_lbl.setText("OUTPUT: "+text);
	status_lbl.setText("synthesizing");
	nsc.getPromptPlayer().appendPrompt(text);
	nsc.getPromptPlayer().play(false);
	status_lbl.setText("synthesis done");
	
	}
	catch(Exception e){
	    status_lbl.setText(e.getMessage());
	}
	moveNextMovesToLatestMoves();
	setLatestSpeakerSys();

    }


 
    void setScoreAndInput(double score,String input){
//  	IclList updates;
//  	if(input.equals("'FAIL'")){
//  	    IclList inputstring = stringToPrologString("");
//  	    updates = new IclList(new IclStruct("set",
//  							new IclStr("score"),
//  							new IclFloat(score)),
//  					  new IclStruct("set",
//  							new IclStr("input"),
//  							inputstring));
//  	}
//  	else{	    
	IclList inputstring = stringToPrologString(input);
	IclList updates = new IclList(new IclStruct("set",
						    new IclStr("score"),
						    //new IclFloat(1.0f)),   
						    new IclFloat(score)),
			      new IclStruct("set",
					    new IclStr("input"),
					    inputstring));
	
	IclTerm goal = new IclStruct("tkit_apply_rule",
				     new IclStr("setScoreAndInput"),
				     new IclList(),
				     updates);
	liboaa.oaaSolve(goal,new IclList(),new IclList());
	
    }


 IclList stringToPrologString(String str){
	IclList l=new IclList();
	for(int i=0;i<str.length();i++){
	    l.add(new IclInt((int)str.charAt(i)));
	}
	return l;
    }

    void setLatestSpeakerSys() throws RuntimeException{
	IclTerm g = new IclStruct("tkit_apply",
				  new IclStruct("set",
						new IclStr("latest_speaker"),
						new IclStr("sys")));
	if(!liboaa.oaaSolve(g,new IclList(),new IclList()))
	    throw new RuntimeException(""+g+" failed");
    }

    void moveNextMovesToLatestMoves() throws RuntimeException{
	//last_moves:=$next_moves
	IclVar A = new IclVar("A");
	//unify(A,$next_moves)
	IclTerm pre = new IclStruct("unify",
				    A,
				    new IclStruct("$",
						  new IclStr("next_moves")));
	IclTerm eff1 = new IclStruct("set",
				     new IclStr("latest_moves"),
				     A);
	
	//IclTerm eff2 = new IclStruct("set",
	//			     new IclStr("next_moves"),
	//			     new IclStruct("oqueue",
	//					   new IclList()));
	IclTerm eff2 = new IclStruct("clear",new IclStr("next_moves"));
	IclList effs = new IclList();
	effs.add(eff1);
	effs.add(eff2);
	IclTerm goal = new IclStruct("tkit_apply_rule",
				     new IclStr("nextMoves2LastMoves"),
				     new IclList(pre),
				     effs);

	//OAAs java library cannot parse the result here, since it contains
	//^-operators, so we must do a hack. post with reply(none) and
	//then sleep for 30 msecs (to simulate posting and reading answer)
	IclList params = (IclList)IclTerm.fromString(true,"[reply(none)]");
	liboaa.oaaSolve(goal,params,new IclList());
	try{ Thread.sleep(30); } catch(Exception e){ }
	
	
    }
    


    String readOutput() throws RuntimeException{
	//String goalStr="tkit_check(equals('$'(input),Input))";
	//IclTerm goal=IclTerm.fromString(true,goalStr);
        IclTerm goal=new IclStruct("tkit_check",
				   new IclStruct("unify",
				   new IclStruct("$",new IclStr("output")),
				   new IclVar()));
	IclList params=(IclList)IclTerm.fromString(true,"[block(true),priority(10)]");
	IclList answers=new IclList();
	System.err.println("Sending "+goal);
	liboaa.oaaSolve(goal,params,answers);
	System.err.println("got answers"+answers);
	if(answers.getNumChildren()<1)
	    throw new RuntimeException(""+goal+" failed");
	IclTerm output=(answers.getTerm(0).getTerm(0).getTerm(1));
	String outputStr=prologStringToString(output);
	if(outputStr==null)
	     throw new RuntimeException("value of input not a string");
	return outputStr;  
    }

    String prologStringToString(IclTerm iclstr){
	String str="";
	if(!iclstr.isList()){
	    System.err.println("not list");
	    return null;
	}
	try{
	    for(Iterator i=iclstr.iterator();i.hasNext();){
		char c=(char)((IclInt)i.next()).toInt();
		str+=c;
	    }
	    return str;
	}
	catch(ClassCastException e){
	    System.err.println("classcast");
	    return null;
	}
    }

}

