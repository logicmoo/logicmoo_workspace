import se.gu.ling.trindikit.oaa.common.*;
import com.sri.oaa2.icl.*;
import java.io.*;

/**
 * This class is used to add objects to the databas.
 * The add method takes three arguments, hence the name.
 * The events MUST have four arguments, since they take
 * both start date and end date. The other ones do not
 * have to take an end date. Events can therefore NOT be 
 * added in this class.<br><br>
 * The add solvable should look like this: add(Choice,String,StartDate)
 * String should be the text information in
 * the new entry
 * Choice is what kind of information it is.
 * That is todo, alarm...
 * Date is...date in the format "20040824T113000"
 * 
 * Events are NOT handled here.
 *
 * @author Andreas Wallentin
 * @version 041014
 */
public class Adder3 extends OAASolver{

    /** String should be the text information in
     * the new event
     * Choice is what kind of information it is.
     * That is todo, alarm...
     * Date is...date "20041031:1523" eller "20040824T113000"???
     * 
     * Events are NOT handled here.
     */
    static String solvableString = "add(Choice,String,StartDate)";

    // what kind of input
    private final String TO = "todo";
    private final String AL = "alarm";

    // saving of db's
    private File saves;
    private ObjectOutputStream out;

    public Adder3(){
	super(solvableString);
	saves = Db.getInstance().getSave();
    }
    
    /**
     * The solve method extended from OAA.<br>
     * goal == find(W,X,Y)<br>
     * W == the type; todo,alarm <br>
     * X == the message connected to the object<br>
     * Y == start date/time<br>
     *
     * @param goal an IclTerm
     * @param params an IclList
     * @param answers an IclList
     *
     * @return true if the method succeeds, otherwise false
     */
    public boolean solve(IclTerm goal, IclList params, IclList answers){
	try{
	    IclStruct temp,temp2,temp3;

	    String choice = ((IclStr)goal.getTerm(0)).toUnquotedString();
	    choice = choice.toLowerCase();

	    String info = ((IclStr)goal.getTerm(1)).toUnquotedString();
	    String date = ((IclStr)goal.getTerm(2)).toUnquotedString();

	    if( choice.equals(TO) || choice.equals(AL) ){
		temp = new IclStruct(choice,new IclStr(info));
		temp2 = new IclStruct("startdate",new IclStr(date));
		
		// temp3 for id
		int id = Db.getInstance().getId();
		temp3 = new IclStruct("id",new IclInt(id));

		temp.add(temp2);
		temp.add(1,temp3);
		Db.getInstance().assertz(new IclStruct("entry",temp));

		IclList dbanswers = new IclList();
		Db.getInstance().db_Solve(IclTerm.fromString(true,"entry(Everything)"),new IclList(),dbanswers);

		out = new ObjectOutputStream(new FileOutputStream(saves));
		out.writeObject(dbanswers);
		out.close();
	    }
	    else
		return false;
	}
	catch(ClassCastException e){
	    return false;
	}
	catch(IOException e){
	    return false;
	}

	answers.add(goal);
	return true;
    }

}
