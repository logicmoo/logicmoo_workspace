import java.util.*;
import java.io.*;
import com.sri.oaa2.icl.*;

import javazoom.jlgui.player.amp.playlist.*;
import javazoom.jlgui.player.amp.util.FileUtil;
import javazoom.jlgui.player.amp.tag.*;

public class MusicDbCreator{

    public static void makeDatabase(String dir, String dbfile){
	Vector musicFiles=new Vector();
	PrintWriter out=null;
	try{
	    out = new PrintWriter(new FileOutputStream(dbfile));
	}
	catch(FileNotFoundException fn){
	    System.out.println(fn.getMessage());
	    System.exit(1);
	}
	File[] allFiles = FileUtil.findFilesRecursively(new File(dir));
	for(int i = 0; i < allFiles.length; i++){
	    String n = allFiles[i].getAbsolutePath();
	    if(FileUtil.isMusicFile(n)){
		IclTerm l=getDBPost(new PlaylistItem(n,n,-1,true));
		out.println(l);
	    }
	}
	out.close();
    }

    static IclTerm getDBPost(PlaylistItem item){
	IclList infolist = new IclList();
	infolist.add(getArtist(item));
	infolist.add(getTitle(item));
	infolist.add(getAlbum(item));
	infolist.add(getLength(item));
	infolist.add(getTrack(item));
	infolist.add(getGenre(item));
	infolist.add(getYear(item));
	infolist.add(getBitRate(item));
	infolist.add(getSamplingRate(item));
	infolist.add(getPath(item));
	return new IclStruct("song",infolist);
    }

    static IclTerm getArtist(PlaylistItem item){
	String a="";
	try{
	    a = item.getTagInfo().getArtist().trim();
	}
	catch(Exception e){}
	return new IclStruct("artist",new IclStr(a));
    }

      static IclTerm getTitle(PlaylistItem item){
	  String t = "";
	try{
	    t = item.getTagInfo().getTitle().trim();
	}
	catch(Exception e){}
	return new IclStruct("title",new IclStr(t));
      }  

     static IclTerm getAlbum(PlaylistItem item){
	 String a = "";
	try{
	    a = item.getTagInfo().getAlbum().trim();
	}
	catch(Exception e){}
	return new IclStruct("album",new IclStr(a));
      }  

    static IclTerm getLength(PlaylistItem item){
	long l = -1;
	try{
	    l = item.getTagInfo().getPlayTime();
	}
	catch(Exception e){}
	return new IclStruct("length",new IclInt(l));
    }
    
    static IclTerm getTrack(PlaylistItem item){
	int t = -1;
	try{
	    t = item.getTagInfo().getTrack();
	}
	catch(Exception e){}
	return new IclStruct("track",new IclInt(t));
    }
    
    static IclTerm getGenre(PlaylistItem item){
	String g = "";
	try{
	    g = item.getTagInfo().getGenre().trim();
	}
	catch(Exception e){}
	return new IclStruct("genre",new IclStr(g));
      }  

    static IclTerm getYear(PlaylistItem item){
	String y="";
	try{
	    y = item.getTagInfo().getYear().trim();
	}
	catch(Exception e){}
	return new IclStruct("year",new IclStr(y));
    }

    static IclTerm getBitRate(PlaylistItem item){
	int br=0;
	try{
	    br=item.getTagInfo().getBitRate();
	}
	catch(Exception e){}
	return new IclStruct("bitrate",new IclInt(br));
    }

    static IclTerm getSamplingRate(PlaylistItem item){
	int sr = 0;
	try{
	    sr=item.getTagInfo().getSamplingRate();
	}
	catch(Exception e){}
	return new IclStruct("samplingrate",new IclInt(sr));
    }

    static IclTerm getComments(PlaylistItem item){
	IclList c = new IclList();
	try{
	    for(Iterator i=item.getTagInfo().getComment().iterator();
		i.hasNext();)
		c.add(new IclStr((String)i.next()));
	}
	catch(Exception e){}
	return new IclStruct("comment",c);
    }
    
    static IclTerm getPath(PlaylistItem item){
	String p="";
	try{
	    p=item.getLocation();
	}
	catch(Exception e){}
	return new IclStruct("path",new IclStr(p));
    }
}    



