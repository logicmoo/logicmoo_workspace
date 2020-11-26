import se.gu.ling.trindikit.oaa.common.*;
import com.sri.oaa2.icl.*;

/**
 * This class is used to search the database, using the
 * the entry(X) solvable.<br>
 * X could be 'anything', any term. That term is 
 * used to search the databse, using unification.  
 *
 * @author Andreas Wallentin
 * @version 041014
 */
public class Find extends OAASolver{
    
    // X could be 'anything', any term. That term is
    // used to search the databse, using unification.

    static String solvableString = "entry(X)";
    IclStruct event,
	todo,
	alarm,
	events,
	alarms,
	todos;
   
    public Find(){
	super(solvableString);
    }
    
    /**
     * The solve method extended from OAA.<br>
     * goal == entry(X)<br>
     * X == term to unify in the database<br>
     *
     * @param goal
     * @param params
     * @param answers
     *
     * @return true if the method succeeds, otherwise false
     */
    public boolean solve(IclTerm goal,IclList params, IclList answers){	
	try{
	    if(Db.getInstance().db_Solve(goal,params,answers)){
		//		String ss = answers.toString();
		//		System.out.println(ss);	
		return true;
	    }
	    else
		return false;
	}
	catch(ClassCastException e){
	    return false;
	}
    }
}

