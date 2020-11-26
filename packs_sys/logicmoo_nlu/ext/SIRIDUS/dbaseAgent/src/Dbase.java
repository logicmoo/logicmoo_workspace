import se.gu.ling.trindikit.oaa.common.*;
import com.sri.oaa2.icl.*;
import java.io.*;

// problem när strängarna innehåller ett ' eller liknande tecken...
// aw 050428

/**
 * Class that makes an IclDb from a text file. The entries should be one
 * per line.
 *
 * @author Andreas Wallentin
 * @version April 2005
 */
public class Dbase extends IclDb{

    private BufferedReader in;

    public Dbase(String dbaseName){
	super();
	boolean done = false;
	boolean fault = false;
	String line;
	int lines = 0;
	try{
	    in = new BufferedReader(new FileReader(dbaseName));
	    while(!done){
		line = in.readLine();
		lines++;
		// make IclList out of db_entry line
		if(line == null)
		    done = true;
		else{
		    if( line.startsWith("**") ){
			System.out.println("Line " + lines + " is commented out. It will not be part of the IclDb.");
		    }
		    else{
			try{
			    IclStruct tmp0 = (IclStruct)IclTerm.fromString(line);
			    db_Assert(tmp0,new IclList());
			}
			catch(Exception h){
			    fault = true;
			    System.out.println("Could not add line "+lines+" in the database to the IclDb. It probably contains a non-letter symbol like \"'\"");
			    h.getMessage();}
		    }
		}
	    }
	    if(fault)
		System.out.println("\nDue to the above, some queries may not be fully correct.\n");
	    in.close();	
	}
	catch(FileNotFoundException e){System.out.println("\n\nThere is no such database file.\n\n");}
	catch(IOException ee){ee.printStackTrace();}
    }

    /**
     * Searches the database for answers to a query.
     *
     * @param query the query to search the database with
     * @return an IclList with the unified answer/-s to the query
     */
    public IclList findAnswers(IclStruct query){
	IclList ans = new IclList();
	db_Solve(query,new IclList(),ans);
	return ans;
    }
    
}

