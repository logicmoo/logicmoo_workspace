import se.gu.ling.trindikit.oaa.common.*;
import com.sri.oaa2.icl.*;

public class CurrentMusicDb extends OAASolver{
    private MusicDbAgent dba;
    static String solvableString="currentMusicDb(Db)";

    public CurrentMusicDb(MusicDbAgent _dba){
	super(solvableString);
	dba=_dba;
    }

    public boolean solve(IclTerm goal, IclList params, IclList answers){
	String db_name=dba.getDbName();
	if(db_name==null)
	    return false;
	else{
	    goal.replaceElement(0,new IclStr(db_name));
	    answers.add(goal);
	    return true;
	}
	
	
    }

    


}
