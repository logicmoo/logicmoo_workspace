import net.fortuna.ical4j.model.Calendar;
import net.fortuna.ical4j.model.PropertyList;
import net.fortuna.ical4j.model.ComponentList;
import net.fortuna.ical4j.model.property.Version;
import net.fortuna.ical4j.model.property.ProdId;
import java.io.PrintWriter;

/**
 * Creates an empty iCalendar, without any events
 * or todos. This is used as a default calendar in
 * order to add new entries.
 * 
 * @author Andreas Wallentin
 * @version 041014
 */

public class EmptyCal{

    private String file;
    private Calendar cal;

    public EmptyCal(String file){
	this.file = file;

// CALENDAR
	PropertyList props = new PropertyList();
	props.add(new Version(null,Version.VERSION_2_0));
	props.add(new ProdId(null,"Test"));
	try{
	    cal = new Calendar(props,new ComponentList());
	    //System.out.println(cal.toString());
	    PrintWriter fout = new PrintWriter(file);
	    fout.println(cal.toString());
	    fout.close();
	}
	catch(Exception e){e.printStackTrace();}
    }

    public Calendar getCalendar(){
	return cal;
    }


}
