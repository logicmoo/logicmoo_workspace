package gui.transact;

/**
 * This abstract class is the top of a hierarchy of transaction state classes.
 */
public abstract class TrState {

	private boolean tasksCompleted = false;
	
	 protected boolean isTasksCompleted() {
		return tasksCompleted;
	}

	protected void setTasksCompleted(boolean tasksCompleted) {
		this.tasksCompleted = tasksCompleted;
	}

	/**
     *  Subclasses must decide what to do 
     *  when the user asks for executing the 
     *  tasks required inside this state of the transaction.
     */
    public abstract void runTasks(Transaction tr); 	
    
    
	/**
     * Subclasses must decide which state follows when the user asks for 
     * executing the next step of the transaction 
     */
    public abstract void nextStep(Transaction tr);

    /**
     * Subclasses must override this method in order to 
     * stop the processes started by the corresponding state tasks.
     * Stub method by default so that subclasses can ignore this if irrelevant. 
     */
    public void stopStateProcs(Transaction tr){};

/*    *//**
     * Stub method by default so that subclasses can ignore this if irrelevant. 
     *//*
    public void runRecognizeTask(Transaction tr) throws IrrelevantStateTask{
    	throw new IrrelevantStateTask(" the recognize task can not be run in this state. State status is  " + tr.status());
    };*/

    
    /**
     * Return a textual description of this state.
     * @return a textual description of this state
     */
    public String status() {
        String s = getClass().getName();
        // return s.substring(s.lastIndexOf('.') + 1) + " completed : " + tasksCompleted;
        return s.substring(s.lastIndexOf('.') + 1) ;
    }

}