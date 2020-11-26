package gui.transact;

import java.util.logging.Level;
import java.util.logging.Logger;

public class TrInitDialServ extends TrState {
	public static final Logger logger = Logger.getLogger(TrInitDialServ.class.getPackage()
			.getName());

	@Override
	public void nextStep(Transaction tr) {
		if (this.isTasksCompleted())
			tr.setState(TrConstants.INIT_DIAL_CLIENT.getState());		
		else
			tr.setState(TrConstants.EXIT_APP.getState());
	}

	@Override
	public void runTasks(Transaction tr) {
		System.out.println(this.status());
		tr.addVisitedState(this);  
		
		try{
			tr.getDialServHandler().startup();
			this.setTasksCompleted(true);
		}			
		catch(Exception e){
			logger.log(Level.SEVERE, "Couldn't create dialog server process! ");
		}

	}
	
    /**
	/**
	 * Stub method since the dialogue server process was killed by  
	 * sending to the dialogue server the message "client_shutdown"
	 * which closes the client connection and also kills the dialogue server process. 
	 * 
     */
	@Override
    public void stopStateProcs(Transaction tr){};	

}
