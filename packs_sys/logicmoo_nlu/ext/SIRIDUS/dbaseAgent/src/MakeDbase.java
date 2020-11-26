import se.gu.ling.trindikit.oaa.common.*;
import com.sri.oaa2.icl.*;

/**
 * Class that makes a database file. The database is a text file that is made
 * given a directory with music files. 
 *
 * @author Andreas Wallentin
 * @version April 2005
 */
public class MakeDbase extends OAASolver{

    static String solvableString = "makeDbase(StartDir,DbaseName)";
    private DbaseAgent dba;

    public MakeDbase(DbaseAgent dba){
	super(solvableString);
	this.dba = dba;
    }

    public boolean solve(IclTerm goal, IclList params, IclList answers){
	// goal == makeDbase(StartDir,DbaseName)
	// StartDir == dir where to start search for music files
	// DbaseName == name of the file to make
	try{
	    String musicDir = ((IclStr)goal.getTerm(0)).toUnquotedString();
	    String fileName = ((IclStr)goal.getTerm(1)).toUnquotedString();
	    dba.makeDbase(musicDir,fileName);
	}
	catch(Exception e){
	    e.printStackTrace();
	    return false;
	}
	answers.add(goal);
	return true;
    }
}
