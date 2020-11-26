import se.gu.ling.trindikit.oaa.common.*;
import com.sri.oaa2.icl.*;
public class Song extends OAASolver{
    
    private MusicDbAgent dba;
    static String solvableString="song(A)";
    
    public Song(MusicDbAgent _dba){
	super(solvableString);
	dba=_dba;
    }
    
    public boolean solve(IclTerm goal, IclList params, IclList answers){
	IclList ans=new IclList();
	IclTerm q = new IclStruct("post",goal);
	IclDb db=dba.getDb();
	if(db==null)
	    return false;
	if(db.db_Solve(q, new IclList(),ans)){
	    for(int i=0;i<ans.size();i++){
		answers.add(ans.getTerm(i).getTerm(0));
	    }
	    return true;
	}
	else return false;
    }

}
