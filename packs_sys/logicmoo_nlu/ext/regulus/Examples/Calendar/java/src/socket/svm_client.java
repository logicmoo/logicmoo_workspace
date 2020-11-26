package socket;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.nio.charset.Charset;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Connection to svm_light_server via stream socket, by specifying port number and IP address. <br/>
 * In order to call the classification server, establish a TCP connection to it on the given port. 
 * Then, write the test instance file (as specified at http://svmlight.joachims.org) followed by a '!'. 
 * The server will send back the predictions file. 
 * '\n' should be used for all end-line sequences.
 */

public class svm_client {

	public static Logger logger = Logger.getLogger(ClientCom.class.getName());

	static final String lineSeparator = System.getProperty("line.separator");

    	
	String serverHost = "localhost";
	int serverPort = 1234;
	
	Socket socketClient = null;
	PrintWriter outPrintWriter = null;
	BufferedReader inBufferedReader = null;


    private static Object syncObject = new Object();
    
    public svm_client(String host, int serverPort) throws ServerConnectionException {
	    this.serverHost = host;
	    this.serverPort = serverPort;
	    
		init(this.serverHost, this.serverPort);
	}

    /**
     * Method used for initialisation.
     * @see constructor of this class.
     * @param serverHost
     * @param serverPort
     * @param charSet
     * @throws ServerConnectionException
     */
	private void init(String serverHost, int serverPort ) throws ServerConnectionException {
		Exception socketException = null;
    

		try {
			try{
					logger.info("Connecting to " + serverHost + ":" + serverPort);
					socketClient = new Socket(serverHost, serverPort);
					logger.info("Connected.");
				}
				catch(Exception se){
				    socketException = se;
				    logger.log(Level.SEVERE, "Problems when connecting to the server ! ");
				    // se.printStackTrace();
				}

			if(socketClient != null){
				outPrintWriter = new PrintWriter(socketClient.getOutputStream(), true);
				inBufferedReader = new BufferedReader(new InputStreamReader(socketClient.getInputStream()));
			}
			else{
			    throw socketException;
			}
		}
		catch (Exception e) {
			socketClient = null;
			if(outPrintWriter != null){
				outPrintWriter.close();
			}
			if(inBufferedReader != null){
				try {
					inBufferedReader.close();
				}
				catch (IOException e1) {
				}
			}
			throw new ServerConnectionException("Could not connect to the server on '" + serverHost + "' port " + serverPort);
		}
	}
    
	
	public String sendRequest(String request) throws ServerConnectionException {
		String reply = null;
			
		if(outPrintWriter != null) {
			// logger.info("sending request over " + serverPort + " : '" + request + "'");
			outPrintWriter.print(request + "\n\r");
			outPrintWriter.flush();
			// logger.info("request sent.");
			
			try {
				reply = inBufferedReader.readLine();
			}
			catch (IOException e) {
				// System.out.println("Could not process request " + request );
				// throw new ServerConnectionException("Could not process request '" + request + "'", e);
				// System.out.println("IO Exception WHEN GETTING THE ANSWER FROM SERVER " );
			}
			catch (Exception ex) {
				System.out.println("EXCEPTION WHEN GETTING THE ANSWER FROM SERVER " );
			}
		}	
		return reply;
	}
	
	public void close() throws IOException{
		if(outPrintWriter != null){
			// outPrintWriter.write("client_shutdown." + lineSeparator);
			outPrintWriter.close();

		}
		
		if(inBufferedReader != null){
			inBufferedReader.close();
		}

		if(socketClient != null){
		    try{
		        socketClient.close();
		    }
		    catch(IOException e){
		    }
		}
	}

	/**
	 * Testing if the connection to the svm_light server can be done.
	 */
	private static void startClient(String host, int serverPort){
		synchronized(syncObject){
			svm_client cc;
			try {
				syncObject = new Object();
				// CLIENT OF THE 	SVM_LIGHT_SERVER:
				cc = new svm_client(host, serverPort);
				//TODO READ external file containing testing 
/*
				//  Send the test instance file (as specified at http://svmlight.joachims.org) 
				followed by a '!'. The server will send back the predictions file. 
				'\n' should be used for all end-line sequences.
				*/
				
				String clientRequest = "0 qid:1 100:0.1 1000:0 1600:0.0 1700:0.0 1800:0.0 2100:0.0 \n " ;
				
				clientRequest += "1 qid:1 100:0.2 1000:0 1600:0.0 1700:0.0 1800:0.0 2100:0.0 \n ";
				clientRequest += "0 qid:1 100:0.3 1000:0 1600:0.0 1700:0.0 1800:0.0 2100:0.0 \n ";				
				clientRequest += "1 qid:1 100:0.4 1000:0 1600:0.0 1700:0.0 1800:0.0 2100:0.0 \n ";			
				clientRequest += "0 qid:1 100:0.5 1000:0 1600:0.0 1700:0.0 1800:0.0 2100:0.0 \n ";				
				clientRequest += "1 qid:1 100:0.6 1000:0 1600:0.0 1700:0.0 1800:0.0 2100:0.0 \n ";
				
				clientRequest += "0 100:0.1 1000:0 1600:0.0 1700:0.0 1800:0.0 2100:0.0 \n ";				
				clientRequest += "1 100:0.2 1000:0 1600:0.0 1700:0.0 1800:0.0 2100:0.0 \n ";				
				clientRequest += "0 100:0.3 1000:0 1600:0.0 1700:0.0 1800:0.0 2100:0.0 \n ";				
				clientRequest += "1 100:0.4 1000:0 1600:0.0 1700:0.0 1800:0.0 2100:0.0 \n ";				
				clientRequest += "1 100:0.5 1000:0 1600:0.0 1700:0.0 1800:0.0 2100:0.0 \n ";				
				clientRequest += "1 100:0.6 1000:0 1600:0.0 1700:0.0 1800:0.0 2100:0.0 \n ! ";
				
				String reply = cc.sendRequestToServer(clientRequest);
				if (reply != null)
					System.out.println("reply = " + reply);
				else
					System.out.println("No reply ! ");

				/*
				clientRequest = "!";			
				reply = cc.sendRequestToServer(clientRequest);
				System.out.println("reply = " + reply);
				*/		
				cc.close();
			}
			catch (ServerConnectionException e) {
				logger.log(Level.WARNING, "", e);
				// e.printStackTrace();
			}
			catch (IOException e) {
				logger.log(Level.WARNING, "IO Exception when closing connection ",e);
				// e.printStackTrace();
			}

		}
	}
	
	/**
	 * Invokes the dialogue server to process the N-best list (previously received from the regServer) 
	 * @param msg4DialogueServer message to be send to the dialogue server
	 * @return reply from the dialogue server
	 */
	protected String sendRequestToServer(String msg4Server){
		String reply = "";
		String currentReply = "";
		
		while (currentReply != null){
			try {
				currentReply = this.sendRequest(msg4Server);
				if (currentReply != null)
					reply +=  " \n" + currentReply;			
			}
			catch (ServerConnectionException e) {
				logger.log(Level.WARNING, "", e);
				System.out.println("WARNING: ");
				e.printStackTrace();
				currentReply = null;
			}
		}
		return reply;
	}

	/*
	 * Testing the connection to the dialogue server
	 */
	public static void main(String[] argv){			
		startClient("localhost", 1234);
	}
}

