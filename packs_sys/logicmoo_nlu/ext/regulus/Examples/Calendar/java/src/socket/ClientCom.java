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
				System.out.println("Could not process the request : " + request );
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

	
	public String getParseTree(){
		String tree = null;
		return tree;
	}
	
}
