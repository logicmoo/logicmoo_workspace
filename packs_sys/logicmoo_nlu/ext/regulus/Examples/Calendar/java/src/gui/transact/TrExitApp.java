package gui.transact;

import java.util.Vector;

public class TrExitApp extends TrState {

	@Override
	public void nextStep(Transaction tr) {
	}

	@Override
	public void runTasks(Transaction tr) {
		System.out.println(this.status());
		// this state doesn't start any processes (that should be stopped by TrExitApp
		
		// Get visited states and shutdown the processes started during these states
		Vector<TrState> visitedStates = tr.getVisitedStates();
		// Here take the states in the reverse order that have been added to the list of visited states
		for (TrState visitedState : visitedStates){
			visitedState.stopStateProcs(tr);
		}
		
	}

}
