import net.fortuna.ical4j.data.CalendarBuilder;
//import net.fortuna.ical4j.data.CalendarOutputter;

import net.fortuna.ical4j.model.Calendar;
import net.fortuna.ical4j.model.ParameterList;
import net.fortuna.ical4j.model.PropertyList;
import net.fortuna.ical4j.model.ComponentList;
import net.fortuna.ical4j.model.Property;

import net.fortuna.ical4j.model.parameter.Value;
import net.fortuna.ical4j.model.parameter.TzId;

import net.fortuna.ical4j.model.property.DtStart;
import net.fortuna.ical4j.model.property.DtEnd;
import net.fortuna.ical4j.model.property.Summary;
import net.fortuna.ical4j.model.property.Due;

import net.fortuna.ical4j.model.component.VTimeZone;
import net.fortuna.ical4j.model.component.VEvent;
import net.fortuna.ical4j.model.component.VToDo;
import net.fortuna.ical4j.model.component.VAlarm;

import com.sri.oaa2.icl.*;

import java.io.*;
import java.util.Iterator;

/**
 * Adds an events/todos to a Calendar. Then writing a text
 * representation of a Calendar object to a file. 
 *
 * @author Andreas Wallentin
 * @version 041015
 */
public class DbToCal{

    private String file,
	startTime,
	stopTime,
	summary;

    private Calendar cal;
    private CalendarBuilder calb;
    private ComponentList comps;
    private PropertyList props;

    private FileInputStream fin;
//    private FileOutputStream fout;
    private java.util.Calendar jCal;

    /** 
     * The time format, where 15 == 3pm, should be: yyyymmddThh<br>
     *                 or                           yyyymmddThhmm<br>
     *                 or                           yyyymmddThhmmss<br>
     * For example: 20040825T1404  <br>
     * NOT        : 2004-08-25:14-4 <br>
     * NOT        : 20040825 <br>
     * 
     * @param file the file name to save the calendar to (*.ics)
     * @param list IclList with all the entries in the database
     * @param cal the Calendar to add information to
     */ 
    public DbToCal(String file,IclList list,Calendar cal){
	this.cal = cal;
	this.file = file;
	try{
	    setCalendar(list);
	}
	catch(Exception e){e.printStackTrace();}
	makeCalendar();
    }

    /**
     * Takes an IclList as argument, adding events and todos
     * to the Calendar.
     *
     * @param list the IclList with database entries
     */
    public void setCalendar(IclList list) throws Exception{

	// NOTICE !!
	// find(alarm(test,id(1),date('20041015T165040')))
	// do not insert the ID in the calendar file??

	Iterator it = list.iterator();
	while(it.hasNext()){
	    IclStruct tmp = (IclStruct)it.next();
	    //entry(alarm(hej,id(1),date('20041015T165040')))
	    
	    IclStruct evto = (IclStruct)tmp.getTerm(0);
	    //alarm(hej,id(1),startdate('20041015T165040'))
	    //alarm(hej,id(1),startdate('20041015T165040'),stopdate('20041015T16504034'))

	    String choice = evto.getFunctor();

	    if(choice.equals("event")){
		summary = ((IclStr)evto.getTerm(0)).toUnquotedString();
		IclStruct tmp2 = (IclStruct)evto.getTerm(2);
		startTime = ((IclStr)tmp2.getTerm(0)).toUnquotedString();
		tmp2 = (IclStruct)evto.getTerm(3);
		stopTime = ((IclStr)tmp2.getTerm(0)).toUnquotedString();
		//addEvent(startTime,stopTime,summary);
		add(choice,startTime,stopTime,summary);
	    }
	    else if(choice.equals("todo")){
		summary = ((IclStr)evto.getTerm(0)).toUnquotedString();
		IclStruct tmp2 = (IclStruct)evto.getTerm(2);
		startTime = ((IclStr)tmp2.getTerm(0)).toUnquotedString();
		//addTodo(startTime,summary);
		add(choice,startTime,null,summary);
	    }
	    else if(choice.equals("alarm")){
		summary = ((IclStr)evto.getTerm(0)).toUnquotedString();
		IclStruct tmp2 = (IclStruct)evto.getTerm(2);
		startTime = ((IclStr)tmp2.getTerm(0)).toUnquotedString();
		//addAlarm(startTime,summary);
		add(choice,startTime,null,summary);
	    }
	    else
		throw new Exception("fel i cal making");

	}
    }

    /**
     * When the calendar is complete, this method is called
     * to save it to a file.
     */
    public void makeCalendar(){
	try{
	    PrintWriter fout = new PrintWriter(new FileOutputStream(new File(file)));
	    fout.println(cal.toString());
	    fout.close();
	}
	catch(FileNotFoundException e){}
    }

    /**
     * Method that adds a new entry to the Calendar. It takes four 
     * strings as argument.
     *
     * @param choice whether it is an event, todo or alarm
     * @param startTime the time the entry should begin
     * @param stopTime the time the entry should end
     * @param summary the text associated with the entry
     */
    public void add(String choice,String startTime,String stopTime,String summary){
	int year = 0,
	    month = 0,
	    day = 0,
	    hour = 0,
	    min = 0,
	    sec = 0;
	
	try{
	    jCal = java.util.Calendar.getInstance();

	    props = cal.getProperties();
	    comps = cal.getComponents();
	    

// Property must have ParameterList
// Component must have ComponentList
// Calendars must have ParameterList and ComponentList

	    TzId tzid = new TzId(jCal.getTimeZone().getID());

	    ParameterList params = new ParameterList();
	    params.add(tzid);

// Time "strings" that are allowed:
//
// 20040523T10       
// 20040523T1023     
// 20040523T102300   
	    year = Integer.parseInt(startTime.substring(0,4));
	    month = (Integer.parseInt(startTime.substring(4,6))) - 1; //month - 1 == correct month no
	    day = Integer.parseInt(startTime.substring(6,8));
	    
	    if(startTime.length() > 8 && startTime.substring(8,9).equals("T")){
		if(startTime.length() >= 11)
		    hour = Integer.parseInt(startTime.substring(9,11));
		if(startTime.length() >= 13)
		    min = Integer.parseInt(startTime.substring(11,13));
		if(startTime.length() >= 15)
		    sec = Integer.parseInt(startTime.substring(13,15));
	    }
	    jCal.set(year,month,day,hour,min,sec);
	    Summary sum = new Summary(new ParameterList(),summary);  
	    PropertyList propL = new PropertyList();

	    if(choice.equals("todo")){
		Due start = new Due(params,jCal.getTime());	
		propL.add(start);}
	    else{
		DtStart start = new DtStart(params,jCal.getTime());
		propL.add(start);
	    }

// STOP TIME only if choice is event
	    if(choice.equals("event")){
		hour = 0; min = 0; sec = 0;
		year = Integer.parseInt(stopTime.substring(0,4));
		month = (Integer.parseInt(stopTime.substring(4,6))) - 1;
		day = Integer.parseInt(stopTime.substring(6,8));
		if(stopTime.length() > 8 && stopTime.substring(8,9).equals("T")){
		    if(stopTime.length() >= 11)
			hour = Integer.parseInt(stopTime.substring(9,11));
		    if(stopTime.length() >= 13)
			min = Integer.parseInt(stopTime.substring(11,13));
		    if(stopTime.length() >= 15)
			sec = Integer.parseInt(stopTime.substring(13,15));
		}
		jCal.set(year,month,day,hour,min,sec);
		DtEnd end = new DtEnd(params,jCal.getTime());
		propL.add(end);
		propL.add(sum);
		VEvent event = new VEvent(propL);	
		comps.add(event);
	    }
	    else if(choice.equals("todo")){
		propL.add(sum);
		VToDo todo = new VToDo(propL);
		comps.add(todo);
	    }
	    else{ // alarm
		propL.add(sum); 
		VAlarm alarm = new VAlarm(propL);
		comps.add(alarm);
	    }
	   
	    cal = new Calendar(props,comps);
	}
	catch(Exception ee){ee.printStackTrace();}
    }

}
