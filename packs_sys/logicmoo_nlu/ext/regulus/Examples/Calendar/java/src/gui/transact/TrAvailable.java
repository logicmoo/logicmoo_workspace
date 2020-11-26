package gui.transact;

public class TrAvailable extends TrState {

	@Override
	public void nextStep(Transaction tr) {
		if (this.isTasksCompleted())
			tr.setState(TrConstants.INIT_GLOBAL_PROCS.getState());
		else{
			// TODO replace the stub code here with meaningfull code
		}
	}

	@Override
	public void runTasks(Transaction tr) {
		System.out.println(this.status());
		// don't need to perform tr.addVisitedState(this); since 
		// this state doesn't start any processes (that should be stopped by TrExitApp
		this.setTasksCompleted(true);
		
	}

}
