import com.sri.oaa2.icl.*;
import java.io.*;
import java.io.FileReader;
import java.util.*;

/**
 * This class "converts" an existing vCalendar file (*.ics) to a
 * LinkedList. The list is then being used to make an IclDb with
 * the events and todos and so forth, as IclStructs.
 *
 * @author Andreas Wallentin
 * @version 041015
 */
public class CalToDb{

    private String name; // name of an *.ics file
    private IclDb db;
    private boolean buildOK;

    /**
     * Simple constructor.
     *
     * @param fileName the name of an *.ics file
     */
    public CalToDb(String fileName){
	name = fileName;
	//buildOK = false;
	db = new IclDb();
	go();
    }

    /*
     * Private method for looping through the LinkedList and
     * making an IclDb out of it.
     */
    private void go(){
	LinkedList ll = getLines(new File(name));
	if(ll == null)
	    buildOK = false;
	else{
	    db = toStruct(ll);
	    Db.getInstance().setDb(db);
	    buildOK = true;
	}
    }


    /**
     * Method for getting the IclDb that is made from an existing
     * *.ics file.
     *
     * @return the newly made IclDb
     */
    public IclDb getDb(){
	return db;
    }

    /**
     * Method for getting a LinkedList from an *.ics file. Every new
     * tag is starting a new line. Whitespaces are removed.
     *
     * @param file the *.ics file to be handled
     * @return a LinkedList with no whitespaces
     */
    public LinkedList getLines(File file) {

	BufferedReader reader;
	LinkedList list = new LinkedList();
	String tab   = new String(new char[] {'\t'});
	String space = new String(new char[] {' '});
	try{
	    String temp;
	    boolean done = false;
	    reader = new BufferedReader(new FileReader(file));
	    System.out.println("Läser fil: "+file.getAbsolutePath());
	    while(!done){
		temp = reader.readLine();
		if(temp == null)
		    done = true;
		else if(temp.length() > 0){
		    if(temp.startsWith(" ")){
			// append to last input after removal of " " in the beginning
			String s = (String)list.removeLast();
			s = s.concat(temp.trim());
			list.addLast(s);
		    }
		    else{
			list.addLast(temp);
		    }
		}
	    }
	}
	catch ( FileNotFoundException e ) {
	    System.err.println("No file to convert!\nCheck name of calendar file name.");
	    return null;
	}
	catch ( IOException ee ) {
	    System.err.println("Reading from file error!");
	}

	return list;
    }


    /**
     * Method for making an IclDb from a LinkedList. Events and todos
     * and so forth, become IclStructs in the IclDb.
     *
     * @param list the LinkedList to convert
     * @return returns the IclDb made from the LinkedList
     */
    public IclDb toStruct(LinkedList list) {
	int id = 0;

	String startDate = "",
	    stopDate = "",
	    summary = "";

	String string = (String)list.getFirst();
	list.removeFirst();

	while ( !string.equals("END:VCALENDAR") ) {
	    //	    System.out.println(string);
	    if ( string.equals("BEGIN:VEVENT") ) {
		//System.out.println("\ninne i event\n");
		string = (String)list.getFirst();
		list.removeFirst();
		
		while ( !string.equals("END:VEVENT") ) {
	//DTSTART;TZID=Europe/Stockholm:20041015T130000 == length 15
		    
		    if ( string.startsWith("DTSTART") ) {
			String[] sa = string.split(":");
			startDate = sa[1].trim(); // == length 15
		    }
        //DTEND;TZID=Europe/Stockholm:20041015T130000 == length 15
		    else if ( string.startsWith("DTEND") ) {
			String[] sa = string.split(":");
			stopDate = sa[1].trim(); // == length 15
		    }

		    else if (string.startsWith("SUMMARY") ) {
			String[] sa = string.split(":");
			if(sa.length == 2)
			    summary = sa[1];
			else
			    summary = "";
		    }
// 		    else if (string.startsWith("DURATION") ) {
// 			plussa på startdate;
// 		    }
		    else {
			/*
			 * resten av slasket
			 *
			 */
		    }
		    string = (String)list.getFirst();
		    list.removeFirst();
		}
// adding event to IclDb
		IclStruct temp = new IclStruct("event",new IclStr(summary));
		IclStruct temp2 = new IclStruct("startdate",new IclStr(startDate));
		IclStruct temp3 = new IclStruct("stopdate",new IclStr(stopDate));
		// temp4 for id
		//int id = Db.getInstance().getId();
		IclStruct temp4 = new IclStruct("id",new IclInt(++id));

		temp.add(temp2);
		temp.add(temp3);
		temp.add(1,temp4);
		//Db.getInstance().assertz(new IclStruct("find",temp));
		db.assertz(new IclStruct("entry",temp));

		string = (String)list.getFirst();
		list.removeFirst();
		//System.out.println("\nlämnar event\n");
	    }
	    else if ( string.equals("BEGIN:VTODO") ) {
		//System.out.println("\ninne i todo\n");
		while ( ! string.equals("END:VTODO") ) {

		    if ( string.startsWith("DUE") ) {
			String[] sa = string.split(":");
			startDate = sa[1].trim();
		    }	
		    else if (string.startsWith("SUMMARY") ) {
			String[] sa = string.split(":");
			if(sa.length == 2)
			    summary = sa[1];
			else
			    summary = "";
		    }
		    else {
			/*
			 * resten av slasket
			 *
			 */
		    }
		    string = (String)list.getFirst();
		    list.removeFirst();
		}
// adding todo to IclDb
		IclStruct temp = new IclStruct("todo",new IclStr(summary));
		IclStruct temp2 = new IclStruct("startdate",new IclStr(startDate));
		// temp3 for id
		//int id = Db.getInstance().getId();
		IclStruct temp3 = new IclStruct("id",new IclInt(++id));
		
		temp.add(temp2);
		temp.add(1,temp3);
		//Db.getInstance().assertz(new IclStruct("find",temp));
		db.assertz(new IclStruct("entry",temp));
		//System.out.println("\nlämnar todo\n");
		
		string = (String)list.getFirst();
		list.removeFirst();
	    }
	    else if ( string.equals("BEGIN:VALARM") ) {
//		System.out.println("\ninne i alarm\n");
		while ( ! string.equals("END:VALARM") ) {

		    if ( string.startsWith("DTSTART") ) {
			String[] sa = string.split(":");
			startDate = sa[1].trim();
		    }	
		    else if (string.startsWith("SUMMARY") ) {
			String[] sa = string.split(":");
			if(sa.length == 2)
			    summary = sa[1];
			else
			    summary = "";
		    }
		    else {
			/*
			 * resten av slasket
			 *
			 */
		    }

		    string = (String)list.getFirst();
		    list.removeFirst();
		}
// adding todo to IclDb
		IclStruct temp = new IclStruct("alarm",new IclStr(summary));
		IclStruct temp2 = new IclStruct("startdate",new IclStr(startDate));
		// temp3 for id
		//int id = Db.getInstance().getId();
		IclStruct temp3 = new IclStruct("id",new IclInt(++id));
		
		temp.add(temp2);
		temp.add(1,temp3);
		//Db.getInstance().assertz(new IclStruct("find",temp));
		db.assertz(new IclStruct("entry",temp));
		
//		System.out.println("\nlämnar  alarm\n");
//		System.out.println(string);
		string = (String)list.getFirst();
		list.removeFirst();
	    }    
	    else if( string.equals("BEGIN:VCALENDAR") ) {
		string = (String)list.getFirst();
		list.removeFirst();
		while ( !string.startsWith("BEGIN") ) {
// 		    String[] sa = string.split(":");
// 		    String functor = sa[0];
// 		    String value = sa[1];
// 		    functor.toLowerCase();
// 		    IclStr arg = new IclStr(value);
// 		    IclStruct info = new IclStruct(functor, arg);
// 		    head.add(info);

		    string = (String)list.getFirst();
		    list.removeFirst();
		}
	    }
	    else{ // when nothing is mathced, go to next
		string = (String)list.getFirst();
		list.removeFirst();
	    }
	}// when string == END:VCALENDAR		    
	return db;
    }

    
    /**
     * This method returns whether a Db could be built from a calendar file
     * or not. Mostly used to handle cases of non-existent calendar files.
     * 
     * @return returns true if the Db is built successfully from a file, false if not.
     */
    public boolean isBuilt(){
	return buildOK;
    }

}
