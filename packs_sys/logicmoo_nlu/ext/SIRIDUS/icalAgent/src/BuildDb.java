import se.gu.ling.trindikit.oaa.common.*;
import com.sri.oaa2.icl.*;

/**
 * This class builds an IclDb from an *.ics file (vCalendar format)
 *
 * @author Andreas Wallentin
 * @version 041020s
 */
public class BuildDb extends OAASolver{

    // CalendarFileName is the string/name of the calendar
    // file that will be used to make an IclDb.
    static String solvableString = "buildDb(CalendarFileName)";

    public BuildDb(){
	super(solvableString);
    }

    public boolean solve(IclTerm goal,IclList params, IclList answers){
	String name = ((IclStr)goal.getTerm(0)).toUnquotedString();
	CalToDb ctd = new CalToDb(name);
	if(ctd.isBuilt()){
	    answers.add(goal);
	    return true;
	}
	else{
	    answers.add(new IclStr("Db could not be built from "+name+". Please check if the name is correct?"));
	    return false;
	}
    }
}
