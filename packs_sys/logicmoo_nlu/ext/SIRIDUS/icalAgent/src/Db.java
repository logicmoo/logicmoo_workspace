import com.sri.oaa2.icl.*;
import java.util.Iterator;
import java.io.*;

/**
 * An IclDb, used to store IclTerms. These will be used
 * to build calendar files - VCalendar format. (for iCal and K-Organizer)
 * The Db is made as a Singleton object. That is, there 
 * can be only one Db at any one time.
 *
 * @author Andreas Wallentin
 * @version 041014
 */
public class Db extends IclDb{

    private static Db theInstance;

    private File saves;
    private ObjectInputStream in;
    private ObjectOutputStream out;
    
    static final String DEFAULT_NAME = "db.sav";
    static final String DEFAULT_DIR = "dbs";
    private IclList list;

    private int id;

    public static boolean clear = true;

    private Db(String x){
	super();
    }

    /**
     * The contructor of the database. If there exists an old
     * database, that one is 'loaded' and used. The new entries
     * are inserted in the old database.
     */
    private Db(){
	super();
	try{
	    File dir = new File(DEFAULT_DIR);
	    dir.mkdirs();
	    saves = new File(dir,DEFAULT_NAME);
	    System.out.println("Checking for existing DB");


	    // If clear == true, then the DB has been reset in order
	    // to make a DB from another file.
	    // If so, do not load existing DB
	    if(Db.clear){
		System.out.println(saves.getAbsolutePath());
		if(saves.exists()){	
		    System.out.println("Loading existing DB: " + saves.getAbsolutePath());
		    in = new ObjectInputStream(new FileInputStream(saves));
		    list = (IclList)in.readObject();
		    //		    System.out.println("sparad lista är så här stor: "+ list.size());
		    in.close();
		    id = getLastId(list);
		    if(id < 0)
			id = 0;
		    
		    Iterator it = list.iterator();
		    while(it.hasNext())
			this.assertz((IclStruct)it.next());
		    
		    System.out.println("Done loading existing DB.\nAdding new objects to this one.");
		    Db.clear = false;
		}
		else{
		    id = 0;
		    System.out.println("No existing DB.\nSaving a new one.");

		}
	    }
	    else{
		System.out.println("Loading a new Db.");
		id = 0;
		Db.clear = true;
	    }
	}
	catch(FileNotFoundException e){System.out.println("No such file available");}	
	catch(IOException e2){System.out.println("Reading/input problem");}	
	catch(ClassNotFoundException e2){System.out.println("Wrong type of object in DB");}

    }

    /**
    * Getting the only instance of the DB
    *
    * @return the only instance of an IclDb
    */
    public static Db getInstance(){
	if(theInstance == null)
	    theInstance = new Db();
	return theInstance;
    }


    /**
     * Getting the binary file where the
     * calendar information is stored.
     *
     * @return the binary file containg calendar information
     */
    public synchronized File getSave(){
	return saves;
    }

    /**
     * Getting the next ID number in order to use
     * it to identify the objects. It is use to 
     * simplify the delete method.
     *
     * @return the next number to be used
     */
    public synchronized int getId(){
	++id;
	//	System.out.println("id == " + id);
	return id;
    }
    public synchronized void setId(int i){
	id = i;
	//	System.out.println("set id == " + id);
    }

    /**
     * Deleting an object in the IclDb with a
     * specific ID number.
     *
     * @param idNo the ID number of the object to delete
     * @return true if deleted, otherwise false
     */
    public synchronized boolean delete(int idNo){
	IclList l = new IclList();
	
	db_Solve(IclTerm.fromString(true,"entry(Everything)"),new IclList(),l);

	for(int i = 0; i < l.size(); i++){
	    String tmp = l.getTerm(i).toString();
	    int ind = tmp.indexOf("id("); // index for 'i', the id no == ind+3
	    int lst = tmp.indexOf(")",ind);

	    String sub = tmp.substring(ind+3,lst); // idNO
	    int id2;
	    try{id2 = Integer.parseInt(sub);}
	    catch(NumberFormatException e){return false;}
	    if(idNo == id2){
		retract(l.getTerm(i));
		return true;
	    }
	}

	return false;
    }

    /**
     * Method for getting the latest/highest ID number that is
     * used in the IclDb. Even if a sequence of ID's is broken,
     * the highest number is returned.
     *
     * @param l an IclList with all IclTerms in the IclDb
     * @return the highest ID number that is used
     */
    public int getLastId(IclList l){
	int id2;
	try{
	    int lastInd = l.size() - 1;
	    
	    String tmp = l.getTerm(lastInd).toString();
	    int ind = tmp.indexOf("id("); // får index för i, no == ind+3
	    int lst = tmp.indexOf(")",ind);
	    String sub = tmp.substring(ind+3,lst); // idN
  
	    id2 = Integer.parseInt(sub);
	}
	catch(Exception e){return -1;}
	return id2;
    }


    /**
     * Setting the Db to an new IclDb.
     *
     * @param db the new IclDb
     */
   public void setDb(IclDb db){
       this.theInstance = null;
       
       Db.clear = false;
       Db.getInstance();
       IclList l = new IclList();
       db.db_Solve(IclTerm.fromString(true,"entry(Everything)"),new IclList(),l);
       for(int i = 0; i < l.size(); i++){
	   Db.getInstance().assertz(l.getTerm(i));
       }
       try{
	   out = new ObjectOutputStream(new FileOutputStream(saves));
	   out.writeObject(l);
	   out.close();
       }      
       catch(Exception d){d.printStackTrace();}
       
       Db.getInstance().setId(l.size());
   }
}
