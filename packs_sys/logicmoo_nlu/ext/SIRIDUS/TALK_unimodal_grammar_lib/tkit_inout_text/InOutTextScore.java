import com.sri.oaa2.com.*;
import com.sri.oaa2.icl.*;
import com.sri.oaa2.lib.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

public class InOutTextScore extends JFrame implements ActionListener{
    
    JTextField textField;
    
    JTextArea textArea;
    JScrollPane scroll;
    LibOaa liboaa;
    IclList solvables;
    final String agentName="InOutTextScore";
    boolean inputTyped;

    String input = null;

    public static void main(String[] args){
	new InOutTextScore(args);

    }
    
    //this
    public void actionPerformed(ActionEvent e){
	Object source = e.getSource();
	if(source == textField){
	    String text=e.getActionCommand();
	    textArea.append("U> "+text+"\n");
	    textField.setText("");
	    JScrollBar sb = scroll.getVerticalScrollBar();
	    sb.setValue(sb.getMaximum());
	    input=text;

	}
    }

    public InOutTextScore(String[] args){
	setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	setLayout(new BorderLayout());
	textField=new JTextField();
	textField.addActionListener(this);
	add(BorderLayout.NORTH,textField);
	textArea=new JTextArea(6,40);
	//textArea.setPreferredSize(200,400);
	scroll = new JScrollPane(textArea,
				 JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				 JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
	textArea.setLineWrap(true);
	textArea.setWrapStyleWord(true);
	add(BorderLayout.CENTER,scroll);
	setTitle("Text I/O Trindikit modules agent");
	pack();
	setVisible(true);

	liboaa=new LibOaa(new LibCom(new LibComTcpProtocol(), args));
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
	    " ]";
	solvables=(IclList)IclTerm.fromString(true,slvstr);
	    
	    
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
	textField.setEditable(true);
	while(input==null){
	    try{
		Thread.sleep(200);
	    }
	    catch (InterruptedException e) { }
	}
	setScoreAndInput(1.0,input);
	input=null;
	textField.setEditable(false);
    }
       


    void setScoreAndInput(double score,String input){
	IclList inputstring = stringToPrologString(input);
	IclList updates = new IclList(new IclStruct("set",
						    new IclStr("score"),
						    new IclFloat(score)), 
				      new IclStruct("set",
				      		    new IclStr("input"),
				      	    inputstring));
				      //new IclStruct("push",
				      //		    new IclStr("input_queue"),
				      //	    inputstring));
	
	IclTerm goal = new IclStruct("tkit_apply_rule",
				     new IclStr("setScoreAndInput"),
				     new IclList(),
				     updates);
	liboaa.oaaSolve(goal,new IclList(),new IclList());
	
    }





//     void setInput(String text){
// 	IclTerm op=new IclStruct("push",
// 				 new IclStr("input_queue"),
// 				 stringToPrologString(text));
// 	IclTerm g=new IclStruct("tkit_apply",op);
// 	if(!liboaa.oaaSolve(g,new IclList(),new IclList()))
// 	    throw new RuntimeException(""+g+" failed");
	
//     }

//     void setScore(double d){
// 	IclTerm op=new IclStruct("set",
// 				 new IclStr("score"),
// 				 new IclFloat(d));
// 	IclTerm g=new IclStruct("tkit_apply",op);
// 	if(!liboaa.oaaSolve(g,new IclList(),new IclList()))
// 	    throw new RuntimeException(""+g+" failed");
//     }

    void output(){
	try{
	    String output=readOutput();
	    textArea.append("S> "+output+"\n");
	    JScrollBar sb = scroll.getVerticalScrollBar();
	    sb.setValue(sb.getMaximum());
	    setLatestSpeakerSys();
	    moveNextMovesToLatestMoves();
	}
	catch(Exception e){
	    textArea.append(e.getMessage()+"\n");
	}
    }
    
    void setLatestSpeakerSys() throws RuntimeException{
	IclTerm g = new IclStruct("tkit_apply",
				  new IclStruct(":=",
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
    
    IclList stringToPrologString(String str){
	IclList l=new IclList();
	for(int i=0;i<str.length();i++){
	    l.add(new IclInt((int)str.charAt(i)));
	}
	return l;
    }


}
