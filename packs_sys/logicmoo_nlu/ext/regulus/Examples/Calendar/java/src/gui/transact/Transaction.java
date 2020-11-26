package gui.transact;

import gui.ApplicationState;
import gui.CalendarConfiguration;
import gui.Dialogue;
import socket.ClientCom;
import socket.DialogueServerHandler;
import socket.ServerConnectionException;
import socket.SpeechHandler;

import java.util.Iterator;
import java.util.Observable;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

//TODO: make CalendarPanel an observer of this class, so that every state change 
//      will imply an actualisation of the interface.
/**
 * This class models a transaction (i.e. a communication with the speech/dialogue server) .
 * This class contains the "context" of the state machine. 
 * A Transaction object records which instance of TrState is the current state 
 * and a history of the states. Subclasses of TrState use a Transition object 
 * to communicate changes in the state back to the Transaction.  
 */
public class Transaction extends Observable {
	public static Logger logger = Logger.getLogger(Transaction.class.getName());

	private static final String CMD_REMOVE_CONTEXT = "action(return_to_initial_context).";  //  @jve:decl-index=0:
    private static final String OK_RESPONSE_REMOVE_CONTEXT = "ok";  //  @jve:decl-index=0:


	private TrState state = TrConstants.AVAILABLE.getState(); 
	// visitedStates contains only those states that were visited and 
	// which started certain processes that need to be stopped by TrExitApp	
	private Vector<TrState> visitedStates = new Vector<TrState>(); 
	// visitedReady is true if TrReady was visited	
	private boolean visitedReadyState = false; 

	
	boolean isVisitedReadyState() {
		return visitedReadyState;
	}

	void setVisitedReadyState(boolean visitedReadyState) {
		this.visitedReadyState = visitedReadyState;
	}

	private CalendarConfiguration config;
	private ApplicationState appState;
	private Dialogue observableDialog;
	
	private SpeechHandler calRecHandler ;
	private DialogueServerHandler dialServHandler ;

	// private String msg4DialogueServer = ""; // ex: "action(process_nbest_list([nbest_hyp(30,when is manny),.."
	ClientCom dialClient = null;
	
	protected Vector<TrState> getVisitedStates() {
		return visitedStates;
	}

	protected void addVisitedState(TrState visitedState) {
		this.visitedStates.add(visitedState);
	}

	public Transaction(CalendarConfiguration config, ApplicationState appState, Dialogue observableDialog) {
		super();
		this.config = config;
		this.appState = appState;
		this.observableDialog = observableDialog;
		
		this.calRecHandler = new SpeechHandler(this.config, this.appState);
		this.dialServHandler = new DialogueServerHandler(config, appState);
		
	}

	Dialogue getObservableDialog() {
		return observableDialog;
	}

	CalendarConfiguration getConfig() {
		return config;
	}

	
	ApplicationState getAppState() {
		return appState;
	}

	SpeechHandler getCalRecHandler() {
		return calRecHandler;
	}

	DialogueServerHandler getDialServHandler() {
		return dialServHandler;
	}
	
	
	
	/**
	 * When the tasks for this state were executed already,
	 * then the user can ask for the next state. 
	 * This "nextStep()" method elicits different behaviours, 
	 * depending on the state of the transaction.
	 */
	public void nextStep() {
		state.nextStep(this);
	}

	/**
	 * Executes the tasks for the current state.
	 */
	public void runTasks() {
		state.runTasks(this);
	}
	
//	/**
//	 * Executes the tasks for the current state.
//	 */
//	public void runRecognizeTask() throws IrrelevantStateTask{
//		state.runRecognizeTask(this);
//	}
	
	/**
	 * Changes the current state and notifies observers of Transaction.
	 */
	public void setState(TrState state) {
		this.state = state;
		setChanged();
		notifyObservers();
	}

	/**
	 * @return a textual description of the transaction's state
	 */
	public String status() {
		return state.status();
	}

	/**
	 * Removes all visited states, except TRInitGlobalProcesses state
	 */
	public void removeVisitedStates() {
		Vector<TrState> newVisitedStates = new Vector<TrState>();
		Iterator<TrState> it = visitedStates.iterator();  
		while (it.hasNext()){
			TrState state = it.next();
			if (state instanceof TrInitGlobalProcesses){
				newVisitedStates.addElement(state);
			}
		}
		visitedStates = newVisitedStates;
		
	}

	protected ClientCom getDialClient() {
		return dialClient;
	}

	protected void setDialClient(ClientCom dialClient) {
		this.dialClient = dialClient;
	}
	
	/**
	 * Sends a message to the dialogue server. 
	 * @param msg4DialogueServer message to be send to the dialogue server. e.g., msg4DialogueServer contains the N-best list previously received from the regServer.
	 * @return reply from the dialogue server
	 */
	String sendRequestToDialogueServer(String msg4DialogueServer){
		String reply = null;

		if (this.dialClient != null){
			try {
				//TODO If no answer after a certain amount of time then return null
				reply = this.dialClient.sendRequest(msg4DialogueServer);
			}
			catch (ServerConnectionException e) {
				logger.log(Level.WARNING, "", e);
				System.out.println("WARNING: ");
				e.printStackTrace();
			}
		}
		else{
			System.out.println("dialogue client not connected");
		}
		return reply;
	}
	
	/**
	 * Invokes the dialogue server  
	 * @param msg4DialogueServer message to be send to the dialogue server
	 * @return reply from the dialogue server
	 */
	public void sendRemoveContextCmdToDialogueServer(){
		String dialogServerReply = sendRequestToDialogueServer(CMD_REMOVE_CONTEXT);
		if (dialogServerReply.indexOf(OK_RESPONSE_REMOVE_CONTEXT) > -1){
			logger.info("Dialogue context removed. ");
		}
	}

}
