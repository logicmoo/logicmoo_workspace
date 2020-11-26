import se.gu.ling.trindikit.oaa.common.*;
import com.sri.oaa2.icl.*;
import java.io.*;

/**
 * This class is used to add objects to the databas.
 * The add method takes four arguments, hence the name.
 * The events MUST have four arguments, since they take
 * both start date and end date. The other ones do not
 * have to take an end date.<br><br>
 * The add solvable should look like this: add(Choice,String,StartDate,EndDate)
 * String should be the text information in
 * the new event
 * Choice is what kind of information it is.
 * That is todo, alarm...
 * Date is...date in the format "20040824T113000"
 *
 * @author Andreas Wallentin
 * @version 041014
 */
public class Adder4 extends OAASolver{
    
    /** String should be the text information in
     * the new event
     * Choice is what kind of information it is.
     * That is event, todo, alarm...
     * Date is...date "20040824T113000"
     *
     * Event must have an end date!!
     */
    static String solvableString = "add(Choice,String,StartDate,EndDate)";

    // what input
    private final String EV = "event";
    private final String TO = "todo";
    private final String AL = "alarm";

    // saving the db's
    private File saves;
    private ObjectOutputStream out;

    public Adder4(){
	super(solvableString);
	saves = Db.getInstance().getSave();
    }
    
    /**
     * The solve method extended from OAA.<br>
     * goal == add(W,X,Y,Z)<br>
     * W == the type; event,todo,alarm <br>
     * X == the message connected to the object<br>
     * Y == start date/time<br>
     * Z == end date/time<br>
     *
     * @param goal an IclTerm
     * @param params an IclList
     * @param answers an IclList
     *
     * @return true if the method succeeds, otherwise false
     */
    public boolean solve(IclTerm goal, IclList params, IclList answers){
	try{
	    IclStruct temp,temp2,temp3,temp4;

	    String choice = ((IclStr)goal.getTerm(0)).toUnquotedString();
	    choice = choice.toLowerCase();

	    String info = ((IclStr)goal.getTerm(1)).toUnquotedString();

	    String startDate = ((IclStr)goal.getTerm(2)).toUnquotedString();
	    String endDate = ((IclStr)goal.getTerm(3)).toUnquotedString();
	    
	    if( choice.equals(EV) || choice.equals(TO) || choice.equals(AL) ){
		temp = new IclStruct(choice,new IclStr(info));
		temp2 = new IclStruct("startdate",new IclStr(startDate));
		temp3 = new IclStruct("stopdate",new IclStr(endDate));
		// temp4 for id
		int id = Db.getInstance().getId();
		temp4 = new IclStruct("id",new IclInt(id));
//              System.out.println(id);
		temp.add(temp2);
		temp.add(temp3);
		temp.add(1,temp4);
		Db.getInstance().assertz(new IclStruct("entry",temp));

		IclList dbanswers = new IclList();
		Db.getInstance().db_Solve(IclTerm.fromString(true,"entry(Everything)"),new IclList(),dbanswers);

//		System.out.println("nu är den så här stor: "+ dbanswers.size() );
		out = new ObjectOutputStream(new FileOutputStream(saves));
		out.writeObject(dbanswers);
		out.close();
		answers.add(goal);
		return true;
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
	//	answers.add(goal);
	//return true;
    }

}
