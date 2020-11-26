import se.gu.ling.trindikit.oaa.common.*;
import java.io.*;
import java.util.Vector;

import com.sri.oaa2.icl.*;

import javazoom.jlgui.player.amp.playlist.*;
import javazoom.jlgui.player.amp.util.FileUtil;
import javazoom.jlgui.player.amp.tag.*;

/**
 * A database agent, handling queries about songs. When the user has made a database file it
 * can be modified externally. Open the file in a text editor and comment out lines by putting
 * ** first on the line. This will make the application skip those lines if, for example, you
 * know some lines are not correct.
 *
 * @author Andreas Wallentin
 * @version April 2005
 */
public class DbaseAgent extends OAAAgent{

    static final String name = "DbaseAgent";
    static final int RUNNING = 0;

    // for music files
    private Vector musicFiles;
    private BasePlaylist playlist;
    private PlaylistItem pli;
    private TagInfo taginfo;

    private Dbase dbase;

    private String artist,
	title,
	album,
	genre,
	path,
	length,
	track,
	year,
	bitrate,
	samplingrate,
	comment;
    private Vector comments;

    private PrintWriter out;
    private BufferedReader in;

    public DbaseAgent(String[] args){
	super(RUNNING,1);
	
	// not used anymore ??
	//addSolver(RUNNING,new MakeDbase(this));
	addSolver(RUNNING,new SearchDbase(this));
	register(name,args);
    }  

    public DbaseAgent(String musicDir, String fileName){
	super(RUNNING,1);
	playlist = new BasePlaylist();
	makeDbase(musicDir,fileName);
    }

    /**
     * Given a directory and a file name, this method searches the directory
     * recursively for music files. Having found the music files it reads the
     * meta information, printing it to the file to be used as a database.
     *
     * @param musicDir the directory to start search for music files
     * @param fileName the file to be used as a database
     */    
    public void makeDbase(String musicDir, String fileName){
	musicFiles = new Vector();
	String n = null;
	File[] allFiles = FileUtil.findFilesRecursively(new File(musicDir));
	try{
	    out = new PrintWriter(new FileOutputStream(fileName));
	}
	catch(FileNotFoundException fn){
	    System.out.println("There is no such file.");
	    System.exit(1);
	}
	for(int i = 0; i < allFiles.length; i++){
	    n = allFiles[i].getAbsolutePath();
	    if(FileUtil.isMusicFile(n))
		musicFiles.add(allFiles[i]);
	}
	if(musicFiles.isEmpty())
	    System.out.println("There were no music files in: "+musicDir);
	else{
	    for(int x = 0; x < musicFiles.size(); x++){
		//System.out.println(musicFiles.get(x).toString());
		pli = new PlaylistItem(musicFiles.get(x).toString(),musicFiles.get(x).toString(),-1,true);
		playlist.appendItem(pli);
	    }
	    //System.out.println("\n\ndone adding pli's\n\n");
	    for(int y = 0; y < playlist.getPlaylistSize(); y++){
		setArtist(playlist.getItemAt(y));
		setTitle(playlist.getItemAt(y));
		setAlbum(playlist.getItemAt(y));
		setLength(playlist.getItemAt(y));
		setTrack(playlist.getItemAt(y));
		setGenre(playlist.getItemAt(y));
		setYear(playlist.getItemAt(y));
		setBitRate(playlist.getItemAt(y));
		setSamplingRate(playlist.getItemAt(y));
		setComments(playlist.getItemAt(y));
		setPath(playlist.getItemAt(y));// getName/getLocation
		printDbLine();
	    }
	}
	musicFiles.clear();
	out.close();
    }

    /**
     * Searches the database and returns the answer/-s.
     *
     * @param dbaseName what database to search
     * @param query the search parameter so be unified
     * @return the answer/-s as an IclList
     */
    public IclList searchDbase(String dbaseName,IclStruct query){
	// query == song([ artist(...), ... ])
	if(dbase == null)
	    dbase = new Dbase(dbaseName);
	System.out.println("Search query: "+query.toString());
	IclList ans = dbase.findAnswers(query);
	return ans;
    }

    void setArtist(PlaylistItem item){
	try{
	    taginfo = item.getTagInfo();
	    artist = "artist('"+taginfo.getArtist().trim()+"')";
	}
	catch(Exception e){
	    artist = "artist('')";
	}
    }

    void setTitle(PlaylistItem item){
	try{
	    taginfo = item.getTagInfo();
	    title ="title('"+ taginfo.getTitle().trim()+"')";
	}
	catch(Exception e){
	    title = "title('')";
	}
    }

    void setAlbum(PlaylistItem item){
	try{
	    taginfo = item.getTagInfo();
	    album = "album('"+taginfo.getAlbum().trim()+"')";
	}
	catch(Exception e){
	    album = "album('')";
	}
    }

    void setLength(PlaylistItem item){
	try{
	    taginfo = item.getTagInfo();
	    length = "length('"+Long.toString(taginfo.getPlayTime())+"')";
	}
	catch(Exception e){
	    length = "length('')";
	}
    }

    void setTrack(PlaylistItem item){
	try{
	    taginfo = item.getTagInfo();
	    track = "track('"+Integer.toString(taginfo.getTrack())+"')";
	}
	catch(Exception e){
	    track = "track('')";
	}
    }

    void setGenre(PlaylistItem item){
	try{
	    taginfo = item.getTagInfo();
	    genre = "genre('"+taginfo.getGenre().trim()+"')";
	}
	catch(Exception e){
	    genre = "genre('')";
	}
    }

    void setYear(PlaylistItem item){
	try{
	    taginfo = item.getTagInfo();
	    year = "year('"+taginfo.getYear().trim()+"')";
	}
	catch(Exception e){
	    year = "year('')";
	}
    }

    void setBitRate(PlaylistItem item){
	try{
	    taginfo = item.getTagInfo();
	    bitrate = "bitrate('"+Integer.toString(taginfo.getBitRate())+"')";
	}
	catch(Exception e){
	    bitrate = "bitrate('')";
	}
    }

    void setSamplingRate(PlaylistItem item){
	try{
	    taginfo = item.getTagInfo();
	    samplingrate = "samplingrate('"+Integer.toString(taginfo.getSamplingRate())+"')";
	}
	catch(Exception e){
	    samplingrate = "samplingrate('')";
	}
    }

    void setComments(PlaylistItem item){
	try{
	    taginfo = item.getTagInfo();
	    comments = taginfo.getComment();
	    if(comments.isEmpty())
		comment = "comment('')";
	    else{
		String tmp = "";
		for(int i = 0; i < comments.size(); i++){
		    tmp += comments.get(i).toString();
		}
		comment = "comment('"+tmp+"')";
	    }
	}
	catch(Exception e){
	    comment = "comment('')";
	}
    }

    void setPath(PlaylistItem item){
	path = "path('"+item.getLocation()+"')";
    }

    // prints the meta information to a database file
    void printDbLine(){
	String tmp = artist+","+title+","+album+","+length+","+track+","+genre+","+year+","+
	    bitrate+","+samplingrate+","+comment+","+path;
	//out.close();
	tmp = "song(["+tmp+"])";
	out.println(tmp);
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


    public static void main(String[] args){
	// flags
	// -dir MusicDirectory
	// -dbfile FileName, req if dir above
	// -h
	
	if( args.length == 1 && (args[0].equals("-h") || args[0].equals("--help")) )
	    printHelp();
	else if(args.length == 4 && args[0].equals("-dir") && args[2].equals("-dbfile")){
	    new DbaseAgent(args[1],args[3]);
	}
	else if(args.length == 4 && args[0].equals("-dbfile") && args[2].equals("-dir")){
	    new DbaseAgent(args[3],args[1]);
	}
	else{
	    new DbaseAgent(args);
	}
    }
}
