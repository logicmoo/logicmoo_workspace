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

public class InOutNuanceScore2 
    extends JFrame 
    implements ActionListener,Runnable
{
    
    String agentName =  "InOutNuance";

    LibOaa liboaa;
    String output_delay_id="0";
    HashMap saved_calls=new HashMap();
    int delay_id_counter=0;
    private int laps;

    boolean open;
    boolean usr_speaking;
    boolean output_pending;
    JPanel ctrl_panel;
    JButton toggle_bt;
    JLabel input_lbl,output_lbl,status_lbl;
    Thread thread;
    
    long time_of_latest_sos=0;
    
    NuanceSpeechChannel nsc;

       
    public static void main(String[] args){
	new InOutNuanceScore2(args);
    }

    public InOutNuanceScore2(String[] args){
	super("Nuance Speech Rec");
     
	//create graphics
	setDefaultCloseOperation(EXIT_ON_CLOSE);
	setLayout(new BorderLayout());
	
	ctrl_panel = new JPanel();
	ctrl_panel.setLayout(new FlowLayout());
	toggle_bt = new JButton("Open");
	toggle_bt.addActionListener(this);

	status_lbl = new JLabel("INITIALIZING");
	ctrl_panel.add(toggle_bt);
	ctrl_panel.add(status_lbl);
	add(BorderLayout.NORTH,ctrl_panel);
	
	input_lbl = new JLabel("INPUT : ");
 	add(BorderLayout.CENTER,input_lbl);
	
	output_lbl = new JLabel("OUTPUT: ");
	add(BorderLayout.SOUTH,output_lbl);
	pack();
	setVisible(true);
	
//create speech channel
	
	open = false;
	usr_speaking = false;

	try{
	    NuanceConfig nc=new NuanceConfig(args);
	    nsc=new  NuanceSpeechChannel(nc);
	    nsc.setStringParameter("wavout.FileFormat","riff");


	    System.out.println("Grammars: "+nsc.getAllGrammars().elementAt(0));
	    //nsc.setFloatParameter("client.NoSpeechTimeoutSecs",0.5f);
	    nsc.getListeners().add(new SpeechChannelListener(){
		    public void handleEvent(vcommerce.core.sc.Event e){
			if(e instanceof PlaybackStoppedEvent && output_pending){
			    output_pending=false;
			    return_delayed_output();
			}
			else if(e instanceof StartOfSpeechEvent){
			    //shut up tts
			    time_of_latest_sos = Calendar.getInstance().getTimeInMillis();
			    System.err.println("shutup");
			    liboaa.oaaSolve( new IclStr("tts_kill"),
					      new IclList(new IclStruct("reply",new IclStr("none"))),
					      new IclList());
			    System.err.println("shutupped");
			    usr_speaking=true;
			}
			else if(e instanceof EndOfSpeechEvent){
			    usr_speaking=false;
			}
		    }
		});
	    
	    if(nsc.getStringParameter("audio.Provider").equals("sip")){
		setTitle("sip");
	    }
	
	    if(nsc.getTelephonyControl()!=null){
		status_lbl.setText("Waiting for a call");
		nsc.getTelephonyControl().waitForCall();
		nsc.getTelephonyControl().answerCall();
	    }
	    status_lbl.setText("Idle");
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
	    //"tkit_call_module(input,input),"+
	    "tkit_call_module(input,quit),"+
	    // "tkit_call_module(output,init),"+
	    //"tkit_call_module(output,output),"+
	    //"tkit_call_module(output,quit),"+
	    //"tkit_call_module(output,abort)"+
	    "output_speech(String)"+
	    "]";
	IclList solvables=(IclList)IclTerm.fromString(true,slvstr);  
	if (!liboaa.oaaRegister("parent", agentName, 
				solvables, new IclList())) {
            System.err.println("Could not register");
            System.exit(0);
        }
	liboaa.oaaReady(true);
    }
 
    void return_delayed_output(){
	System.err.println("returning "+(IclTerm)saved_calls.get(output_delay_id));
	liboaa.oaaReturnDelayedSolutions(output_delay_id,(IclTerm)saved_calls.get(output_delay_id));

    }


    public boolean handle(IclTerm goal, IclList params, IclList answers) {
	System.out.println("\nkollar output\n");
	if(((IclStruct)goal).getFunctor().equals("output_speech")){
	    String str = "";
	    try{
		str = ((IclStr)goal.getTerm(0)).toUnquotedString();
	    }
	    catch(ClassCastException e){
		return false;
	    }
	    output_lbl.setText("OUTPUT: "+str);

	    try{
		nsc.getPromptPlayer().appendTTS(str);
		output_pending=true;
	    }
	    catch(Exception e){
		e.printStackTrace();
	    }
		     
	    if(!usr_speaking){
		try{
		    nsc.abort("new output");
		    nsc.abort(AbortType.RECOGNITION,"new output");
		}
		catch(Exception e){
		    e.printStackTrace();
		}
		
	    }
	    
	    output_delay_id = generateDelayID();
	    liboaa.oaaDelaySolution(output_delay_id);
	    saved_calls.put(output_delay_id,goal);
	    return true;
	}
	
	else if(!((IclStruct)goal).getFunctor().equals("tkit_call_module"))
	    return false;
	//sloppy programming, we know that only correct solvables ever
	//get this far. however they can be variables, in that case
	//ClassCastException is thrown
	String arg1="";
	String arg2="";
	
	try{
	    arg1= ((IclStr)goal.getTerm(0)).toUnquotedString();
	    arg2= ((IclStr)goal.getTerm(1)).toUnquotedString();
	}
	catch(ClassCastException e){
	    return false;
	}
	
	
	
	
	
	//we need to handle output:output and output:abort
	 if(arg1.equals("output")){
	     


	     if(arg2.equals("output")){
		 //NO OUTPUT
		 String output = readOutput();
		 try{ 
		     Thread.sleep(output.length()*50);
	    
		 }
		 catch(Exception e){
		 }
		 setLatestSpeakerSys();
		 moveNextMovesToLatestMoves();
		 answers.add(goal);
		 return true;
		 /*
	

		 if(!open){
//  		     try{nsc.getPromptPlayer().play(false);}
//  		     catch(Exception e){
//  			 e.printStackTrace();
		     //  		     }
		     setLatestSpeakerSys();
		     moveNextMovesToLatestMoves();
		     answers.add(goal);
		     return true;
		 }
		 
		 else {
		     try{
			 nsc.getPromptPlayer().appendTTS(output);
			 output_pending=true;
		     }
		     catch(Exception e){
			 e.printStackTrace();
		     }
		     
		     //if(!usr_speaking){
			// try{
			  //   nsc.abort("new output");
				 //nsc.abort(AbortType.RECOGNITION,"new output");
			 //}
			 //catch(Exception e){
			 //    e.printStackTrace();
			// }
			 
		     //}
		     
		     output_delay_id = generateDelayID();
		     liboaa.oaaDelaySolution(output_delay_id);
		      return true;
		     }

*/


		 //setLatestSpeakerSys();
		 //moveNex"tMovesToLatestMoves();
                 //return delayed solutions(output_delay_id,[tkit_call_module(output,output)])
	     }
	     else if(arg2.equals("abort")){
		 try{ 
		     nsc.abort(AbortType.PLAYBACK,"user input"); 
		 }
		 catch(Exception e){
		     e.printStackTrace();
		 }
		 answers.add(goal);
		 return true;
	     }
	     else{
		 answers.add(goal);
		 return true;
	     }
	 }
	 answers.add(goal);
	 return true;
    }
    
    


    public void actionPerformed(ActionEvent e){
	Object source = e.getSource();
	if(source == toggle_bt){
	    if(open){
		//stop listening
		open=false;
		try{
		    nsc.abort("closed");
		}
		catch(Exception ex){
		    ex.printStackTrace();
		}
		toggle_bt.setLabel("Open");
	    }
	    else{
		open=true;
		thread=new Thread(this);
		thread.start();
		toggle_bt.setLabel("Close");
	    }
	}
	    
    }
    
    public void run(){
	while(open){
	    String input;
	    double score;
	    try{
		String filename="usr"+Calendar.getInstance().getTimeInMillis()+".wav";
		nsc.setStringParameter("client.RecordFilename",filename);
		RecResult rr= nsc.playAndRecognize((String)nsc.getAllGrammars().elementAt(0));
		//laps++;
		if(rr.isNormalRecognition()){
		    input=rr.getSingleResult(0).getRecognizedString();
		    score=((double)rr.getSingleResult(0).getConfidence())/100.0;
		    log_utterance(rr,filename);
		    setScoreAndInput(score,input);
		    //laps = 0;
		}
		else if(rr.isNoSpeechTimeout()){
		    input="";
		    score=1.0;
//  		    if(laps > 5){
//  			setScoreAndInput(score,input);
//  			laps = 0;
//  		    }
		    //do not set input
		}
		else{
		    input="'FAIL'";
		    score=0.0;
		    log_utterance(rr,filename);
		    //setScoreAndInput(score,input);
		}
	    }
	    catch(Exception e){
		e.printStackTrace();
		System.err.println(e.getMessage());
	    }
	    
	}
    }
    
    
     String generateDelayID(){
	 ++delay_id_counter;
	 return agentName + delay_id_counter;
	 //return agentName;
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
						    new IclFloat(1.0f)),   
						    //new IclFloat(score)),
			      new IclStruct("push",
					    new IclStr("input_queue"),
					    inputstring));
	
	IclTerm goal = new IclStruct("tkit_apply_rule",
				     new IclStr("setScoreAndInput"),
				     new IclList(),
				     updates);
	input_lbl.setText(""+score+" "+input);
	//	output_lbl.setText(goal.toString());
	liboaa.oaaSolve(goal,new IclList(),new IclList());
	
    }


    void log_utterance(RecResult rr,String filename){
	IclTerm iclrr=kvSet2Icl(rr);
	IclTerm g=new IclStruct("tkit_log",
				new IclStruct("event",
					  new IclInt(time_of_latest_sos),
					  new IclStruct("asr",
							iclrr,
							new IclStr(filename))));
					      
	
	IclList p=new IclList(new IclStruct("reply",new IclStr("none")));
	liboaa.oaaSolve(g,p,new IclList());			

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
	//IclTerm eff2 = new IclStruct("clear",new IclStr("next_moves"));
	IclList effs = new IclList();
	effs.add(eff1);
	//effs.add(eff2);
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


        

    //this method converts the nuance KVSet structures to icl format
    //A key-value pair will be represented as Key=Value. Keys are of type String
    //and will be represented as IclStr. Although any type of Object is allowed
    //as a value , the following types are used:
    //KVSet which will be represented as IclList
    //Vector which will be represented as IclList
    //Integer which will be represented as IclInt
    //String which will be represented as IclStr
    //Floats which will be represented as IclFloat
    public static IclList kvSet2Icl(KVSet set){
	IclList icl = new IclList();
	for (Enumeration e = set.getKeys(); e.hasMoreElements() ;) {
		String key = (String)e.nextElement();
		Object value = set.getObject(key);
		IclTerm iclValue = toIcl(value);
		icl.add(new IclStruct(key,iclValue));
		
	}
	    return icl;
    }

    public static IclList kvVector2Icl(Vector v){
	IclList icl = new IclList();
	for(int i=0;i<v.size();++i)
	    icl.add(toIcl(v.elementAt(i)));
	return icl;
    }
	
    private static IclTerm toIcl(Object o){
	if(o instanceof KVSet) 
	    return kvSet2Icl((KVSet)o);
	else if(o instanceof Vector)
	    return kvVector2Icl((Vector)o);
	else if(o instanceof String)
	    return new IclStr((String)o);
	else if(o instanceof Integer)
	    return new IclInt(((Integer)o).intValue());
	else if(o instanceof Float)
	    return new IclFloat(((Float)o).floatValue());
	else
	    return new IclStr("Could not convert value to icl");
	
    }

}

