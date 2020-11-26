import se.gu.ling.trindikit.oaa.common.*;

/**
 * An OAA agent for handling IclDb. The agent will be used
 * to control a database of objects, which will be used to 
 * build calendar files.
 *
 * @author Andreas Wallentin
 * @version 041014
 */
public class FindAgent extends OAAAgent{
    static final String name = "FindAgent";
    static final int RUNNING = 0;

    public FindAgent(String[] arsgf){
	super(RUNNING,1);
	addSolver(RUNNING, new Find());
	addSolver(RUNNING, new Adder3());
	addSolver(RUNNING, new Adder4());
	addSolver(RUNNING, new Add());
	addSolver(RUNNING, new Deleter());
	addSolver(RUNNING, new BuildCalendar());
	addSolver(RUNNING, new BuildDb());
	register(name,arsgf);
    }

    public static void main(String[] args){
	new FindAgent(args);
    }

}
