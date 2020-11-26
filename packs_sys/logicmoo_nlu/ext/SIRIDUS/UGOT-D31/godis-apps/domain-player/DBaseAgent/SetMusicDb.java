import se.gu.ling.trindikit.oaa.common.*;
import com.sri.oaa2.icl.*;
import java.io.*;

public class SetMusicDb extends OAASolver{
    private MusicDbAgent dba;
    static String solvableString="setMusicDb(Db)";
 
    public SetMusicDb(MusicDbAgent _dba){
	super(solvableString);
	dba=_dba;
    }
    
    public boolean solve(IclTerm goal, IclList params, IclList answers){
	try{
	    File f = new File(((IclStr)goal.getTerm(0)).toUnquotedString());
	    if(f.getAbsolutePath().equals(dba.getDbName())){
		answers.add(goal);
		return true;
	    }
	    
	    if(!f.exists() || !f.isFile())
		return false;
	    
	    if(readInDbFile(f)){
		answers.add(goal);
		return true;
	    }
	}
	catch(ClassCastException e){
	}
	return false;
    }

    boolean readInDbFile(File f){
	try{
	    IclDb db = new IclDb();
	    BufferedReader in = new BufferedReader(new FileReader(f));
	    String line="";
	    while(line!=null){
		line=in.readLine();
		System.err.println(line);
		if(line!=null && 
		   !line.startsWith("**")) 
		   //line.matches("\\w"))
		    try{
			db.assertz(new IclStruct("post",IclTerm.fromString(true,line)));
		    }
		    catch(RuntimeException re){
			return false;
		    }
	    }
	    in.close();
	    dba.setDb(db);
	    return true;
	}
	catch(Exception e){
	    e.printStackTrace();
	    return false;
	}
	
    }
}




