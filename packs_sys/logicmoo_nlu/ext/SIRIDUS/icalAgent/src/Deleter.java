import se.gu.ling.trindikit.oaa.common.*;
import com.sri.oaa2.icl.*;
import java.io.*;

/**
 * As the name says, this will be used to delete objects
 * from the IclDb.
 *
 * @author Andreas Wallentin
 * @version 041014
 */
public class Deleter extends OAASolver{

    // IdNumber is an integer, corresponding to the objects
    // ID number.

    static String solvableString = "delete(IdNumber)";

     // saving of db's
    private File saves;
    private ObjectOutputStream out;
  
    public Deleter(){
	super(solvableString);
	saves = Db.getInstance().getSave();
    }
    
    /**
     * The solve method extended from OAA.<br>
     * goal == delete(X)<br>
     * X == a number corresponding to the ID number of
     * the object to delete.<br>
     *
     * @param goal an IclTerm
     * @param params an IclList
     * @param answers an IclList
     *
     * @return true if the method succeeds, otherwise false
     */
    public boolean solve(IclTerm goal, IclList params, IclList answers){
	try{
	    int id = ((IclInt)goal.getTerm(0)).toInt();
	    if(Db.getInstance().delete(id)){
		answers.add(new IclStr("Object with ID "+id+" is deleted"));

		IclList dbanswers = new IclList();
		Db.getInstance().db_Solve(IclTerm.fromString(true,"entry(Everything)"),new IclList(),dbanswers);

		out = new ObjectOutputStream(new FileOutputStream(saves));
		out.writeObject(dbanswers);
		out.close();
		answers.add(goal);
		return true;
	    }
	    else{
		answers.add(new IclStr("Object with ID "+id+" did not exist"));
		return false;
	    }
	}
	catch(ClassCastException e){
	    return false;
	}
	catch(IOException e2){
	    return false;
	}
    }
}
