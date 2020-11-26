package issco.calendar.socket;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.nio.charset.Charset;
import java.util.logging.Level;
import java.util.logging.Logger;


/**
 * Connection to a server via stream socket, by specifying port number and IP address.
 */
public class ClientCom {
	public static Logger logger = Logger.getLogger(ClientCom.class.getName());

	static final String lineSeparator = System.getProperty("line.separator");
    static final Charset DEFAULT_CHARSET = Charset.forName("iso-8859-1");
    	
	String serverHost = null;
	int serverPort = 0;
	
	Socket socketClient = null;
	PrintWriter outPrintWriter = null;
	BufferedReader inBufferedReader = null;
    private Charset charSet = null;

    private static Object syncObject = new Object();
    
    public ClientCom(String host, int serverPort, Charset charSet) throws ServerConnectionException {
	    this.serverHost = host;
	    this.serverPort = serverPort;
	    this.charSet = charSet;
		init(this.serverHost, this.serverPort, this.charSet);
	}

	private void init(String serverHost, int serverPort, Charset charSet) throws ServerConnectionException {
		Exception socketException = null;
        this.charSet = charSet;

		try {
			try{
					logger.fine("Connecting to " + serverHost + ":" + serverPort);
					socketClient = new Socket(serverHost, serverPort);
					logger.fine("Connected.");
				}
				catch(Exception se){
				    socketException = se;
				    logger.log(Level.SEVERE, "Problems when connecting to the server ! ");
				    // se.printStackTrace();
				}

			if(socketClient != null){
				outPrintWriter = new PrintWriter(socketClient.getOutputStream(), true);
				inBufferedReader = new BufferedReader(new InputStreamReader(socketClient.getInputStream(), charSet));
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
    
    public void reconfigure(Charset charSet) throws IOException{
        inBufferedReader = new BufferedReader(new InputStreamReader(socketClient.getInputStream(), charSet));
    }
	
	public String sendRequest(String request) throws ServerConnectionException {
		String reply = null;
			
		if(outPrintWriter != null) {
			logger.info("sending request over " + serverPort + " = '" + request + "'");
			outPrintWriter.print(request + "\n\r");
			outPrintWriter.flush();
			logger.info("request sent.");
			
			try {
				reply = inBufferedReader.readLine();
			}
			catch (IOException e) {
				System.out.println("Could not process request " + request );
				throw new ServerConnectionException("Could not process request '" + request + "'", e);
			}
			catch (Exception ex) {
				System.out.println("EXCEPTION WHEN GETTING THE ANSWER FROM SERVER " );
			}
		}	
		return reply;
	}
	
	public void close() throws IOException{
		if(outPrintWriter != null){
			outPrintWriter.write("client_shutdown." + lineSeparator);
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

	private boolean loadPackageRequest(String request) throws ServerConnectionException {
		boolean returnValue = false;
		String reply = null;
		
		logger.fine("sending request = _" + request + "_");		
		outPrintWriter.print(request + lineSeparator);
		
		logger.fine("request sent.");
		
		try {
			int r = 0;	
			while((r = inBufferedReader.read()) != -1){
				logger.fine("r = " + r);
			}
			
			logger.log(Level.FINE, ("reply = " + reply));
		}	
		catch (IOException e) {
			System.out.println("Could not connect process request '" + request + "'");
			throw new ServerConnectionException("Could not connect process request '" + request + "'", e);
		}
		catch(Exception e2){
			System.out.println(" No answer from server. " );
		}
		
		return returnValue;
	}
	
	public String getParseTree(){
		String tree = null;
		return tree;
	}
	
	/**
	 * @deprecated
	 * Useful only for testing from the main of this class 
	 * if the dialogue server starts.
	 */
	private static void startDialogueServer(){
		synchronized(syncObject){
			String[] envVar = new String[2];
			envVar[0] = "REGULUS=D:/regulus/";
			envVar[1] = "PATH=D:/work/MEDSLTrelated/SICStus Prolog 3.12.5/bin";
			
			java.io.File workingDir = new java.io.File("D:/Regulus/Examples/Calendar/scripts/");
			OSProcess dialogueServer = new OSProcess("D:/Regulus/Examples/Calendar/scripts/run_server.bat", envVar, workingDir);
			
			dialogueServer.run();
			syncObject = new Object();
		}
	}

	/**
	 * @deprecated
	 * Useful only as an example and for testing from the main of this class 
	 * if the dialogue server starts.
	 */
	private static void startDialogueServer2(){
		synchronized(syncObject){
			// OSProcess dialogueServer = new OSProcess("sicstus -l D:/Regulus/Examples/Calendar/scripts/load_and_run_server.pl");
			OSProcess dialogueServer = new OSProcess("redirect.exe -e D:/Regulus/Examples/Calendar/scripts/logs/dialogue_server_err.txt -o D:/Regulus/Examples/Calendar/scripts/logs/dialogue_server_out.txt sicstus -l D:/Regulus/Examples/Calendar/scripts/load_and_run_server.pl");
			dialogueServer.run();			
		}
	}

	
	/**
	 * @deprecated
	 * Useful only to test from the main of this class 
	 * if the connection to the dialogue server can be done.
	 */
	private static void startDialogueClient(){
		synchronized(syncObject){
			ClientCom cc;
			try {
				syncObject = new Object();
				// CLIENT OF THE DIALOGUE SERVER:
				cc = new ClientCom("localhost", 1975, DEFAULT_CHARSET);

				String clientRequest1 = "client('localhost').";
				cc.sendNBestListToDialogueServer(clientRequest1);
				
				String clientRequest2 = "action(process_nbest_list([nbest_hyp(55,'were there meetings that monday'),nbest_hyp(55,'were there meetings my monday'),nbest_hyp(48,'were there meetings her monday'),nbest_hyp(48,'were there meetings after monday'),nbest_hyp(48,'were there meetings last monday'),nbest_hyp(48,'were there meetings for monday')])).";
				cc.sendNBestListToDialogueServer(clientRequest2);
							
				cc.sendNBestListToDialogueServer(clientRequest1);
				
				//
				//String clientRequest3 = "action(process_rec_string('when is the next meeting')).";
				String clientRequest3 = "action(process_nbest_list([nbest_hyp(55,'were there meetings that monday'),nbest_hyp(55,'were there meetings my monday'),nbest_hyp(48,'were there meetings her monday'),nbest_hyp(48,'were there meetings after monday'),nbest_hyp(48,'were there meetings last monday'),nbest_hyp(48,'were there meetings for monday')])).";
				cc.sendNBestListToDialogueServer(clientRequest3);
				
				cc.sendNBestListToDialogueServer(clientRequest3);

				String clientRequest = "disconnect.";
				cc.sendNBestListToDialogueServer(clientRequest);
								
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
	protected String sendNBestListToDialogueServer(String msg4DialogueServer){
		String reply = null;
		
//		if (dialogueClient != null){
			try {
				reply = this.sendRequest(msg4DialogueServer);			
			}
			catch (ServerConnectionException e) {
				logger.log(Level.WARNING, "", e);
				System.out.println("WARNING: ");
				e.printStackTrace();
			}
		return reply;
	}

	/*
	 * Testing the connection to the dialogue server
	 */
	public static void main(String[] argv){			
		 startDialogueServer2();
			for (int t=0; t< 20 ; t++){
				for (int k=0; k< 999999999 ; k++){}
			}
		startDialogueClient();
	}
}
