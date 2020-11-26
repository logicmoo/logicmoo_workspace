import se.gu.ling.trindikit.oaa.common.*;
import com.sri.oaa2.icl.*;
import java.io.*;

/**
 * This class is used to events to the database.
 *
 * @author Andreas Wallentin
 * @version 041021
 */
public class Add extends OAASolver{
    
    /** 
     * Type is what kind of event it is. That is meeting, birthday,
     * appointment ...<br>
     * Summary should be the text information in the new event<br>
     * StartDatime and EndDatime are points of time looking like the Prolog
     * predicate datime/6; datime(yyyy,mm,dd,hh,mm,ss)<br>
     * Location is a string saying where the event is taking place
     */
    static String solvableString = "addEvent(Type,Summary,StartDatime,EndDatime,Location)";

    // saving the db's
    private File saves;
    private ObjectOutputStream out;

    public Add(){
	super(solvableString);
	saves = Db.getInstance().getSave();
    }
    
    /**
     * The solve method extended from OAA.<br>
     * goal == entry(Type,Summary,StartDatime,StopDatime,Location)<br>
     * Type == the type of event; meeting, birthday, appointment... <br>
     * Summary == the message connected to the object<br>
     * StartDatime == start datime/6; datime(yyyy,mm,dd,hh,mm,ss) <br>
     * StopDatim == stop datime/6; datime(yyyy,mm,dd,hh,mm,ss) <br>
     * Locatio == location of the event <br><br>
     * Summary and Location could be empty strings. The other ones 
     * are crucial for the functionality.
     *
     * @param goal an IclTerm
     * @param params an IclList
     * @param answers an IclList
     *
     * @return true if the method succeeds, otherwise false
     */
    public boolean solve(IclTerm goal, IclList params, IclList answers){
// goal == add(meeting,'summary',datime(2004,10,22,10,12,00),datime(2004,10,22,10,12,00),'Lindholmen')
	try{
	    IclStruct temp,temp2,temp3,temp4;

	    String type = ((IclStr)goal.getTerm(0)).toUnquotedString();
	    type = type.toLowerCase();

	}
	catch(ClassCastException e){
	    return false;
	}
	answers.add(goal);
	return true;
    }

}
