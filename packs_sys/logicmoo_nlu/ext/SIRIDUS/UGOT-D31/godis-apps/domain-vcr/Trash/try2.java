
/* A Java version of the home domain simulation. Brought to you 
   by Cormac O'Brien. Layout design by Alexander Berman.     */



import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.net.ServerSocket;
import java.net.Socket;
import java.io.InputStream;
import java.io.DataInputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.*;
import javax.swing.border.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;
import java.applet.Applet;
import java.lang.Object;


class GridBagWindow extends JFrame {
    final boolean shouldFill = true;
    boolean inAnApplet = true;
    Panelet2 graph;


    public GridBagWindow(){
	Container contentPane = getContentPane();
        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
	contentPane.setLayout(gridbag);
        if (shouldFill) {
            //natural height, maximum width
            c.fill = GridBagConstraints.HORIZONTAL; 
        }

	graph = new Panelet2();
	graph.setLayout(null);
  	graph.resize(300, 450);
	c.gridx = 0;
	c.gridy = 0;
	gridbag.setConstraints(graph, c);
        contentPane.add(graph);

	addWindowListener(new WindowAdapter() {
		public void windowClosing(WindowEvent e) {
		    if (inAnApplet) {
			dispose();
		    } else {
			System.exit(0);
		    }
		}
	    });

    }

    public Panelet2 getGraph(){
	return graph;
    }

}


class Panelet2 extends Panel {
    String ans = "ON";
    String autoAns = "OFF";
    String bsLang = "ENGLISH";
    String hsLang = "ENGLISH";
    String hsVol = "4";
    String earVol = "4";
    String intTone = "MIXED";
    String exTone = "MIXED";
    String mesTone = "MIXED";
    String searchTone = "MIXED";
    int prog = 0;
    String pos[] = new String[8];
    String date[] = new String[8];
    String start[] = new String[8];
    String stop[] = new String[8];
    String playStatus = "";
    String activeChannel = "1";

    public void paint(Graphics g) {
	g.setColor(Color.white);
	g.fillRect(0,0,1000,500);
	g.setColor(Color.black);
	g.drawString("VCR", 160,25);
	g.drawString("PLAY STATUS", 15,75);
	g.drawString("ACTIVE CHANNEL", 15,105);
	g.drawString("SCHEDULED PROGRAMS", 45,135);
	
	g.drawString("CHN  DATE  START  STOP", 15,165);
	int position = 195;
	int j = 0;
	for(int i = 0; i< prog;i++){
	    g.setColor(Color.red);
	    g.drawString(pos[i]+"      "+date[i]+"     "+start[i]+"     "+stop[i], 15,position);
	    position = position+30;
	    j++;
	} 
	g.setColor(Color.black);
	for(int i = j; i<= 8;i++){
	    g.drawString("--       --/--       --:--       --:--", 15,position);
	    position = position+30;
	}



	g.setColor(Color.red);
	
	g.drawString(playStatus,175,75);
	g.drawString(activeChannel,175,105);

    }


    Image offScreenBuffer;
    
    public void update(Graphics g)
    {
	Graphics gr; 
	// Will hold the graphics context from the offScreenBuffer.
	// We need to make sure we keep our offscreen buffer the same size
	// as the graphics context we're working with.
	if (offScreenBuffer==null || (! (offScreenBuffer.getWidth(this) == this.size().width && offScreenBuffer.getHeight(this) == this.size().height)))
	    {
		offScreenBuffer = this.createImage(size().width, size().height);
	    }

	// We need to use our buffer Image as a Graphics object:
	gr = offScreenBuffer.getGraphics();
	gr.setColor(Color.white);
	gr.fillRect(0,0,size().width, size().height);
	paint(gr); // Passes our off-screen buffer to our paint method, which,
	// unsuspecting, paints on it just as it would on the Graphics
	// passed by the browser or applet viewer.
	g.drawImage(offScreenBuffer, 0, 0, this);
	// And now we transfer the info in the buffer onto the
	// graphics context we got from the browser in one smooth motion.
    } 







    public boolean mouseDown(Event e, int x, int y) {
	System.out.println("Co-ordinates "+x+" "+y);
	return true;
    }

    public void setAns(String s){
	ans = s;
	repaint();
    }
    
    public void setAutoAns(String s){
	autoAns = s;
	repaint();
    }

    public void setBsLang(String s){
	bsLang = s;
	repaint();
    }

    public void setHsLang(String s){
	hsLang = s;
	repaint();
    }

    public void setHsVol(String s){
	hsVol = s;
	repaint();
    }
    
    public void setEarVol(String s){
	earVol = s;
	repaint();
    }

    public void setIntTone(String s){
	intTone = s;
	repaint();
    }

    public void setExTone(String s){
	exTone = s;
	repaint();
    }

    public void setMesTone(String s){
	mesTone = s;
	repaint();
    }

    public void setSearchTone(String s){
	searchTone = s;
	repaint();
    }


    public void setPlayStatus(String s){
	playStatus = s;
	repaint();
    }

    public void setActiveChannel(String s){
	activeChannel = s;
	repaint();
    }


    public void setRecord(String s1, String s2, String s3, String s4){	
	pos[prog] = s1;
	date[prog] = s2;
	start[prog] = s3;
	stop[prog] = s4;
	prog++;
	repaint();
    }


    public void deleteRecord(int i){
	pos[i] = "--";
	date[i] = "--/--";
	start[i] = "--:--";
	stop[i] = "--:--";
	prog--; //addition on the spot
	repaint();
    }



}


class try2{
    public static void main(String args[]) {
	GridBagWindow window = new GridBagWindow();
	GridBagWindow2 window2 = new GridBagWindow2();

	window.inAnApplet = false;
	window.setTitle("Virtual VCR");
	window.pack();
	window.resize(300,450);
	window.setVisible(true);
	//window.setLocation(400,0);


	window2.inAnApplet = false;
	window2.setTitle("House");
	window2.pack();
	window2.resize(265,265);
	window2.setLocation(0,460);
	//window2.setVisible(true);
	

	try
	    {

		String getNum = args[1];
		
		

		Integer inti = new Integer(1);
		
		
				    
		inti = Integer.valueOf(getNum);
		
		int port = inti.intValue();
									
		
		Socket outSock = new Socket(args[0],port);
		System.out.println("made connection");
		InputStream in = outSock.getInputStream();
		DataInputStream ds = new DataInputStream(in);
		boolean done = false;
		Indra obj = new Indra("stuff.txt");

		while (!done){
		    String test = ds.readLine();
		    test = test.trim();
	
		    obj.writeToFile(test);

		    if(test.startsWith("<update domain=")){
			int first1 = test.indexOf("n=\"");
			String newer = test.substring(first1+3);
			int second1 = newer.indexOf("\"");
			String new2er = newer.substring(0,second1);
	
			
			if(new2er.equals("home")){
	
			    int value1 = 0;

			    int first = test.indexOf("y=\"");
			    String new1 = test.substring(first+3);
			    int second = new1.indexOf("\"");
			    String new2 = new1.substring(0,second);
			    
			    first = new1.indexOf("e=\"");
			    String new3 = new1.substring(first+3);
			    second = new3.indexOf("\"");
			    new3 = new3.substring(0,second);

			    String new4 = new3.substring(0,1);
			    String new5 = new3.substring(2);

			    if(new4.equals("1")){
				value1 = 1;
				
			    }else {
				
				Integer inti1 = Integer.valueOf(new5);
				value1 = inti1.intValue();


			    }
			


			    if(new2.equals("REL2")){
				window2.getGraph().setKitchenLamp(value1);
			    } else if(new2.equals("REL1")){
				
				window2.getGraph().setTv_roomLamp(value1);
			    } else if(new2.equals("REL3")){
				
				window2.getGraph().setHallLamp(value1);
			    } else if(new2.equals("DIM10")){
				window2.getGraph().setLiving_roomLamp(value1);
			    } else if(new2.equals("REL4")){
				window2.getGraph().setStudyLamp(value1);
			    }
			    
			}

			if(new2er.startsWith("tp")){
			    
			    int first = test.indexOf("y=\"");
			    String new1 = test.substring(first+3);
			    int second = new1.indexOf("\"");
			    String set = new1.substring(0,second);

			    int fir = new1.indexOf("e=\"");
			    String new11 = new1.substring(fir+3);
			    int sec = new11.indexOf("\"");
			    String value = new11.substring(0,sec);
			    value = value.toUpperCase();


			    
			    if(set.equals("answering_machine_onoff")){
				window.getGraph().setAns(value);
			    }else if(set.equals("autoanswer_onoff")){
				window.getGraph().setAutoAns(value);
			    }else if(set.equals("base_station_language")){
				window.getGraph().setBsLang(value);
			    }else if(set.equals("handset_language")){
				window.getGraph().setHsLang(value);
			    }else if(set.equals("ring_volume")){
				window.getGraph().setHsVol(value);
			    }else if(set.equals("earpiece_volume")){
				window.getGraph().setEarVol(value);
			    }else if(set.equals("tone_or_melody(internal)")){
				window.getGraph().setIntTone(value);
			    }else if(set.equals("tone_or_melody(external)")){
				window.getGraph().setExTone(value);
			    }else if(set.equals("tone_or_melody(message)")){
				window.getGraph().setMesTone(value);
			    }else if(set.equals("tone_or_melody(search_signal)")){
				window.getGraph().setSearchTone(value);
			    }

			}

			if(new2er.startsWith("vcr")){
			    
			    int first = test.indexOf("y=\"");
			    String new1 = test.substring(first+3);
			    int second = new1.indexOf("\"");
			    String set = new1.substring(0,second);
 

			    int fir = new1.indexOf("e=\"");
			    String new11 = new1.substring(fir+3);
			    int sec = new11.indexOf("\"");
			    String value = new11.substring(0,sec);
			    value = value.toUpperCase();
			    
			    if(set.equals("play_status")){
				window.getGraph().setPlayStatus(value);
			    } else if(set.equals("program_position")){
				window.getGraph().setActiveChannel(value);
			    }
			    else if(set.equals("new_channel")){
				window.getGraph().setActiveChannel(value);
}


			}



		    }


		    if(test.startsWith("<addProgram")){
			int one = test.indexOf("slot=");
			String st = test.substring(one+6,one+7);

			
			int two = test.indexOf("program=");
			String st1 = test.substring(two+9,two+11);

			
			int three = test.indexOf("date=");
			String st2 = test.substring(three+6,three+10);

			
			int four = test.indexOf("start=");
			String st3 = test.substring(four+7,four+11);

			
			int five = test.indexOf("stop=");
			String st4 = test.substring(five+6,five+10);
			
			window.getGraph().setRecord(st1,st2,st3,st4);



		    }

		    if(test.startsWith("deleteProgram")){
			int one = test.indexOf("slot=");
			String st = test.substring(one+6,one+7);
			one = Integer.parseInt(st);
			
			window.getGraph().deleteRecord(one-1);
		    
		    }



		}
	    }
	catch (Throwable t)
	    {
		t.printStackTrace();
		//System.err.println("File input error");
	    }




    }
}










class GridBagWindow2 extends JFrame {
    final boolean shouldFill = true;
    boolean inAnApplet = true;
    Panelet3 graph;


    public GridBagWindow2(){
	Container contentPane = getContentPane();
        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
	contentPane.setLayout(gridbag);
        if (shouldFill) {
	    //natural height, maximum width              
	    c.fill = GridBagConstraints.HORIZONTAL;           
	}
	

	graph = new Panelet3();
  	graph.resize(250, 250);
	graph.setLayout(null);
	c.gridx = 0;
	c.gridy = 0;
	gridbag.setConstraints(graph, c);
        contentPane.add(graph);
	contentPane.setSize(250,250);
	addWindowListener(new WindowAdapter() {
		public void windowClosing(WindowEvent e) {
		    if (inAnApplet) {
			dispose();
		    } else {
			System.exit(0);
		    }
		}
	    });

    }

    public Panelet3 getGraph(){
	return graph;
    }

}





class Panelet3 extends Panel {
    int tv = 0;
    int kitch = 0;
    int hall = 0;
    int liv = 0;
    int study = 0;


    public void paint(Graphics g) {
	g.setColor(Color.blue);
	g.fillRect(0,0,500,250);
	g.setColor(Color.white);
	g.drawString("TV_ROOM",15 ,25);
	g.drawString("KITCHEN",15 ,75);
	g.drawString("HALL",15 ,125);
	g.drawString("LIVING ROOM",15 ,175);
	g.drawString("STUDY",15 ,225);


	g.setColor(Color.white);
	if(tv==1){
	    
	    g.setColor(Color.yellow);
	    g.fillRect(125,0,100,45);
	    g.setColor(Color.white);
	}else {
	    g.fillRect(125,0,100,45);
	}
	
	//light and temp sensors
	//g.fillRect(235,0,100,45);
	//g.fillRect(345,0,100,45);


	
	if(kitch==1){
	    g.setColor(Color.yellow);
	    g.fillRect(125,50,100,45);
	    g.setColor(Color.white);
	}else {
	    g.fillRect(125,50,100,45);
	}

	
	if(hall==1){
	    g.setColor(Color.yellow);
	    g.fillRect(125,100,100,45);
	    g.setColor(Color.white);
	}else {
	    g.fillRect(125,100,100,45);
	}

	if(liv>0){
	    g.setColor(Color.yellow);
	    g.fillRect(125,150,100,45);
	    g.setColor(Color.white);
	}else {
	    g.fillRect(125,150,100,45);
	}

	if(study==1){
	    g.setColor(Color.yellow);
	    g.fillRect(125,200,100,45);
	    g.setColor(Color.white);
	}else {
	    g.fillRect(125,200,100,45);
	}


	g.setColor(Color.red);
	g.drawString("LAMP",135,25);
	//g.drawString("light_sensor",245,25);
	//g.drawString("temp_sensor",355,25);
	g.drawString("LAMP",135,25);
	g.drawString("LAMP",135,75);
	g.drawString("LAMP",135,125);
	g.drawString("DIMMER",135,175);
	g.drawString("LAMP",135,225);
	
    }

    public boolean mouseDown(Event e, int x, int y) {
	System.out.println("Co-ordinates "+x+" "+y);
	return true;
    }

    public void setKitchenLamp(int i){
	kitch = i;
	repaint();
    }

    public void setTv_roomLamp(int i){
	
	tv = i;
	repaint();
    }

    public void setHallLamp(int i){
	hall = i;
	repaint();
    }

    public void setLiving_roomLamp(int i){
	liv = i;
	repaint();
    }


    public void setStudyLamp(int i){
	study = i;
	repaint();
    }



}





class Indra 
 {
 DataInputStream dis;
 DataOutputStream dos;
 String str;
 FileInputStream fis;
 FileOutputStream fos;
 File f;
 String line;
 String l;

 Indra(String s)
 {
 str= new String(); 
 line = new String();
 try
 {
 f = new File(s);
 fos = new FileOutputStream(f);
 dos = new DataOutputStream(fos);
 }
 catch(IOException e)
 {
 System.out.println("Some IOException occured Indra");
 }
 }

 void writeToFile(String s)
 {
 s = s + "\n"; // next line
 try
 { 
 dos.writeBytes(s); // writing line by liine
 }
 catch(IOException e)
 {
 System.out.println("Some IOException occured Indra");
 } 
 }

 void readFromFile()
 {
 try
 {
 fis = new FileInputStream(f);
 dis = new DataInputStream(fis);

 while (dis.available() > 0)
 {
 l = new String(dis.readLine()); //reading line by line
 System.out.println(l);
 }
 fis.close(); //closing FileInputStream
 dis.close(); //closing DataInputStream
 }
 catch(IOException e)
 {
 System.out.println("Some IOException occured Indra");
 }
 }

 void closeFile()
 {
 try
 { 
 fos.close(); // close the FileInputStream
 dos.close(); //thank u for closing me
 }
 catch(IOException e)
 {
 System.out.println("Some IOException occured Indra");
 }
 }

}
