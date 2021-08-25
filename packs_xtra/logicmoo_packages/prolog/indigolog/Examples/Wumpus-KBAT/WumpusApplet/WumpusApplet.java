import java.lang.*;
import java.io.*;
import java.net.*;
import java.applet.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public class WumpusApplet extends JApplet implements Runnable, ActionListener{
    //network stuff
	public PrintWriter out;
	public BufferedReader in;
	public ServerSocket server;
	public Socket client;
	public Thread serverThread;
	public static int listeningPort = 9002;
	
	//wumpus world
	public WumpusWorld wumpusWorld;      
	public WumpusInfo wumpusInfo;      

	public void init() {      
      getContentPane().setBackground(Color.darkGray);
      //initialize wumpus world
      wumpusWorld = new WumpusWorld(this, 8,8,new Dimension(500,500));
      wumpusInfo = new WumpusInfo(this, new Dimension(200,500));
      //put things in place
      getContentPane().add(wumpusInfo, BorderLayout.WEST);
      getContentPane().add(wumpusWorld, BorderLayout.CENTER);
    }

    public void start() {
		logMessage("Creating server thread..\n");
        if (serverThread == null) {
            serverThread = new Thread(this, "Server");
            serverThread.start();
            //serverThread.setDaemon(true);
        }
    }

	public void run(){
	  logMessage("Server thread created.\n");
	  try{		 
		 //initialize server 
         server = new ServerSocket(listeningPort);
         logMessage("Server initialized.\n");
	  }
	  catch(IOException e){
	  	 errorMessage("WumpusApplet failed to listen on port " + listeningPort + ".");
	  }
	  while(true){
	  	try{		 
			wumpusInfo.startButton.setEnabled(false);
			//wumpusInfo.startButton.setText("Start!");	  	
	        logMessage("Server listening..\n");
		 	client = server.accept();
         	logMessage("Wumpus has connected!\n");

	         //set up communication streams
		 	out = new PrintWriter(client.getOutputStream(), true);
         	in = new BufferedReader(new
            	InputStreamReader(client.getInputStream()));

			wumpusInfo.startButton.setEnabled(true);
			//wumpusInfo.startButton.setText("Start!");
			
			String msg = "";
		 	while(!msg.equals("end")){
         		while (!in.ready()) {   // wait for some message
         			serverThread.sleep(15);
         		}   
		   		msg = in.readLine();        // get msg
           		logMessage("Message: " + msg + "\n");  // log message
				handleMessage(msg);
		 	}
	        logMessage("Wumpus disconnected.\n");
	      	in.close();
		  	out.close(); 
      		client.close();
	  	}
	  	catch(Exception e){
         	errorMessage("Oh no! Error in the server thread!\n");	  
         	errorMessage(e.toString() + "\n");
	  	}
	  }
	}
	
	public void handleMessage(String msg){
			if(msg.indexOf("robot")==0){
          		try{
		           	String stringx = msg.substring(msg.indexOf("(")+1,msg.indexOf(",")).trim();
           			String stringy = msg.substring(msg.indexOf(",")+1,msg.indexOf(")")).trim();
           			int x = Integer.parseInt(stringx)-1;
					int y = Integer.parseInt(stringy)-1;
					wumpusWorld.moveRobot(x, y);

				}catch(Exception e){
					warningMessage("Parse error. TCP message '" + msg + "' ignored.");
				}
		   	}else if(msg.indexOf("wumpus")==0){
          		try{
		           	String stringx = msg.substring(msg.indexOf("(")+1,msg.indexOf(",")).trim();
           			String stringy = msg.substring(msg.indexOf(",")+1,msg.indexOf(")")).trim();
           			int x = Integer.parseInt(stringx)-1;
					int y = Integer.parseInt(stringy)-1;
					wumpusWorld.setWumpus(x, y);
				}catch(Exception e){
					warningMessage("Parse error. TCP message '" + msg + "' ignored.");
				}
		   	}else if(msg.indexOf("gold")==0){
          		try{
		           	String stringx = msg.substring(msg.indexOf("(")+1,msg.indexOf(",")).trim();
           			String stringy = msg.substring(msg.indexOf(",")+1,msg.indexOf(")")).trim();
           			int x = Integer.parseInt(stringx)-1;
					int y = Integer.parseInt(stringy)-1;
					wumpusWorld.setGold(x, y);
				}catch(Exception e){
					warningMessage("Parse error. TCP message '" + msg + "' ignored.");
				}
		   	}else if(msg.indexOf("pit")==0){
          		try{
		           	String stringx = msg.substring(msg.indexOf("(")+1,msg.indexOf(",")).trim();
           			String stringy = msg.substring(msg.indexOf(",")+1,msg.indexOf(")")).trim();
           			int x = Integer.parseInt(stringx)-1;
					int y = Integer.parseInt(stringy)-1;
					wumpusWorld.setPit(x, y);		   	
				}catch(Exception e){
					warningMessage("Parse error. TCP message '" + msg + "' ignored.");
				}
		   	}else if(msg.indexOf("set")==0){
          		try{
		           	String stringx = msg.substring(msg.indexOf("(")+1,msg.indexOf(",")).trim();
		           	msg = msg.substring(msg.indexOf(",")+1);
           			String stringy = msg.substring(0,msg.indexOf(",")).trim();
		           	msg = msg.substring(msg.indexOf(",")+1);
		           	String data = msg.substring(0, msg.lastIndexOf(")"));
           			int x = Integer.parseInt(stringx)-1;
					int y = Integer.parseInt(stringy)-1;
					wumpusWorld.setCell(x, y, data);				
				}catch(Exception e){
					warningMessage("Parse error. TCP message '" + msg + "' ignored.");
					warningMessage(e.toString());
				}
		   	}else if(msg.indexOf("write")==0){
          		try{
		           	String data = msg.substring(msg.indexOf("(")+1,msg.lastIndexOf(")")).trim();
			        wumpusInfo.appendInfo(data+"\n");
				}catch(Exception e){
					warningMessage("Parse error. TCP message '" + msg + "' ignored.");
				}
			}else if(msg.trim().equals("reset")){
		   		wumpusWorld.reset();
		   		wumpusInfo.reset();
		   	}		
	}
	
	public void stop() {
		logMessage("Applet stopped.\n");
    }
	
	public void destroy(){
	  logMessage("Applet destruction initiated.\n");
      try{
      	server.close();
      }
      catch(Exception e){
         errorMessage("Oh no! Error in the applet!\n");
      }
    }
    
    void logMessage(String s){
    	wumpusInfo.appendLog(s);
    }
    
    void warningMessage(String s){
    	//System.out.println("Warning: " + s);
		//System.out.flush();
        logMessage("Warning: " + s);	  
	}

    void errorMessage(String s){
    	//System.out.println("Error: " + s);
		//System.out.flush();
        logMessage("Fatal error: " + s);	  
		System.exit(0);
	}
	
	public void actionPerformed (ActionEvent e){
		//e.getSource().getClass().getName().equals("...JButton");
		
		//i know that there are only buttons now handled
		JButton b = (JButton)e.getSource();
		if(b.getText().equals("Start!")){
			//change the text on the button
			b.setText("Pause");
			//send message to indigolog server
			out.println("start");
		}else if(b.getText().equals("Pause")){
			//change the text on the button
			//b.setText("Resume");
			//send message to indigolog server
			out.println("pause");
		}
		else if(b.getText().equals("Resume")){
			//change the text on the button
			b.setText("Pause");
			//send message to indigolog server
			out.println("resume");
		}
	}

	/**
 	* Allow this JApplet to run as as application as well.
 	*
 	* @param args command line arguments ignored.
 	*/
	public static void main( String args[] ){
   		final WumpusApplet applet = new WumpusApplet();
   		final JFrame frame = new JFrame( "Wumpus World");
   		frame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
   		frame.setSize(750, 550 + 35 /* allow room for Frame bar */);
		frame.addWindowListener(
   		  new WindowAdapter(){
      		/**
       		* Handle request to shutdown.
	       	*
       		* @param e event giving details of closing.
       		*/
      		public void windowClosing( WindowEvent e ){
         		applet.stop();
         		applet.destroy();
         		System.exit(0);
         	} // end WindowClosing
      	  } // end anonymous class
   		); // end addWindowListener line

   		frame.getContentPane().add( applet );
   		applet.init();
   		frame.validate();
   		frame.setVisible(true);
   		applet.start();
   	} // end main
}
