package gui.transact;

import socket.ClientCom;
import socket.ServerConnectionException;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

public class TrInitDialClient extends TrState {
	public static final Logger logger = Logger.getLogger(
			TrInitDialClient.class.getPackage().getName());

	private ClientCom dialogClient;

	@Override
	public void nextStep(Transaction tr) {
		if (this.isTasksCompleted())
			tr.setState(TrConstants.READY_TO_RECOGNISE.getState());		
		else
			tr.setState(TrConstants.EXIT_APP.getState());
	}

	@Override
	public void runTasks(Transaction tr) {
		System.out.println(this.status());
		tr.addVisitedState(this);
		
		//////////////////////////////////
		// Connect to dialogue server	//
		////////////////////////////////// 
		try{
			boolean connection_refused = true;
			int noTry = 0;
			while (connection_refused && (noTry++ < 6) ){
				this.dialogClient = connectToDialogueServer(
						tr.getConfig().getDialogueServerHost(), 
						tr.getConfig().getDialogueServerPort(), 
						tr.getConfig().getDialogueServerCharset());
				tr.setDialClient(this.dialogClient);
				if (this.dialogClient != null)
					connection_refused = false;				
			}
			if (!connection_refused)
				this.setTasksCompleted(true);
		}
		catch (Exception e) {
			String errorMessage = "The following error occured while initiating the dialogue client :\n";
			errorMessage += e.getMessage();
			errorMessage += "\nPlease check your app configuration file and try again.";
			logger.log(Level.SEVERE, "Could not start all required processes.",
					e);
			//this.setTasksCompleted(false); // is false by default
		}

	}
	
    /**
     * Stops the processes started by tasks of TrInitDialClient state.
     */
	@Override
	public void stopStateProcs(Transaction tr){
		if (tr.getDialServHandler() != null )    	
			// 	disconnect client from dialogue server and then kill dialogue server 
			disconnectFromDialogueServer();   	 
	}
	
	/**
	 * Creates a client of the dialogue server and sends a client connection request.  
	 */
	private ClientCom connectToDialogueServer(String host, int serverPort, java.nio.charset.Charset charSet){		
		try {
			// CLIENT OF THE DIALOGUE SERVER:
			//this.dialogueClient = new ClientCom("localhost", 1985, DEFAULT_CHARSET);
			this.dialogClient = new ClientCom(host, serverPort, charSet);

			String clientRequest1 = "client('localhost').";
			this.dialogClient.sendRequest(clientRequest1);
			// logger.info(this.dialogueClient.)
			return this.dialogClient;
		}
		catch (ServerConnectionException e) {
			logger.log(Level.SEVERE, "Connection to dialogue server refused because: ", e);
			e.printStackTrace();
			return null;
		}
	}
	  
	/**
	 * Sends to the dialogue server the message "client_shutdown"
	 * which closes the client connection and also kills the dialogue server process. 
	 * @param ClientCom cc 
	 */
	private void disconnectFromDialogueServer(){		
		try {
			//String clientRequest_old = "disconnect.";
			//dialogClient.sendRequest(clientRequest_old);		

			String clientRequest = "client_shutdown.";
			this.dialogClient.sendRequest(clientRequest);

			dialogClient.close();
		}
		catch (ServerConnectionException e) {
			logger.log(Level.WARNING, "", e);
			e.printStackTrace();
		}
		catch (IOException e) {
			logger.log(Level.SEVERE, "IO Exception when closing connection ");
			e.printStackTrace();
		}		
	}
	
	

}
