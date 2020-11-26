import se.gu.ling.trindikit.oaa.common.*;
import com.sri.oaa2.icl.*;

public class MusicDbAgent extends OAAAgent{

    
    static final String name = "MusicDb";
    static final int RUNNING = 0;


    private IclDb db;
    private String db_name;

    public MusicDbAgent(String[] args){
	super(RUNNING,1);
	addSolver(RUNNING,new Song(this));
	addSolver(RUNNING,new SetMusicDb(this));
	addSolver(RUNNING,new CurrentMusicDb(this));
	register(name,args);
    }

    public String getDbName(){
	return db_name;
    }

    public void setDb(IclDb _db){
	db=_db;
    }

    public IclDb getDb(){
	return db;
    }

    public static void main(String[] args){
	// flags
	// -dir MusicDirectory
	// -dbfile FileName, req if dir above
	// -h
	boolean help=false;
	boolean create_mode=false;
	String musicdir=null;


	String dbfile=null;
	for(int i=0;i<args.length;i++){
	    if(args[i].equals("-h")){
		help=true;
	    }
	    else if( args[i].equals("-dir")){
		create_mode=true;
		if(++i<args.length)
		    musicdir=args[i];
	    }
	    else if( args[i].equals("-dbfile")){
		create_mode=true;
		if(++i<args.length)
		    dbfile=args[i];
	    }
	}
	if(help)
	    printHelp();
	else if(create_mode){
	    if(musicdir!=null && dbfile!=null)
		MusicDbCreator.makeDatabase(musicdir,dbfile);
	    else
		printHelp();	    
	}
	else
	    new MusicDbAgent(args);
    }  

    
    private static void printHelp(){
	String s = "\nUsage:\n";
	String s1 = "Use either option 1 or options 2 and 3\n\n";
	String s2 = "java DbaseAgent <option 1>\nor\n";
	String s3 = "java DbaseAgent <option 2/3> <option 2/3>\n";
	String s4 = "OPTION 1: Arguments for handling OAA(optional)\n\n";
	String s5 = "OPTION 2: -dir MusicDirectory(required)\n";
	String s6 = "OPTION 3: -dbfile NameOfDatabaseFileToMake(required)\n\n";
	System.out.println(s+s1+s2+s3+s4+s5+s6);
    }  

}
