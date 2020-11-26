import se.gu.ling.trindikit.oaa.common.*;
import com.sri.oaa2.icl.*;

import net.fortuna.ical4j.model.Calendar;

/**
 * This class builds *.ics in VCalendar format. It builds
 * the calendar files from the objects in the database.
 *
 * @author Andreas Wallentin
 * @version 041014
 */
public class BuildCalendar extends OAASolver{

    // CalendarName is the string/name of the calendar
    // file that will be made.
    static String solvableString = "buildCal(CalendarName)";

    public BuildCalendar(){
	super(solvableString);
    }

    public boolean solve(IclTerm goal,IclList params, IclList answers){
	String name = ((IclStr)goal.getTerm(0)).toUnquotedString();
	EmptyCal cal = new EmptyCal(name);

	IclList dbanswers = new IclList();
	Db.getInstance().db_Solve(IclTerm.fromString(true,"entry(Everything)"),new IclList(),dbanswers);

	new DbToCal(name,dbanswers,cal.getCalendar());
	answers.add(goal);
	return true;
    }
}
