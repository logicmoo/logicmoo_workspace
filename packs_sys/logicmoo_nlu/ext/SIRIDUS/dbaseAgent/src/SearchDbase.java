import se.gu.ling.trindikit.oaa.common.*;
import com.sri.oaa2.icl.*;

/**
 * Class that handles the search command through OAA.
 *
 * @author Andreas Wallentin
 * @version April 2005
 */
public class SearchDbase extends OAASolver{

    static String solvableString = "searchDbase(DbaseName,Query,Answer)";
    private DbaseAgent dba;

    public SearchDbase(DbaseAgent dba){
	super(solvableString);
	this.dba = dba;
    }

    public boolean solve(IclTerm goal, IclList params, IclList answers){
	// goal == searchDbase(DbaseName,Query,Answer)
	// Query is a struct to be unified
	// Query == song([artist(Artist),title(Tit)...])
	// Answers will be an IclList with returing answers
	try{ 
	    String dbaseName = ((IclStr)goal.getTerm(0)).toUnquotedString();
	    IclStruct entries = (IclStruct)goal.getTerm(1);
	    IclList ans = dba.searchDbase(dbaseName,entries);
	    //System.out.println(ans.size());
	    if(ans.isEmptyList())
		return false;
	    goal.replaceElement(2,ans);
	    printAnswers(ans);
	    answers.add(goal);
	}
	catch(Exception e){
	    e.printStackTrace();
	    System.err.println(e.getMessage());
	    return false;
	}

	return true;
    }

    // prints the answers in the list
    void printAnswers(IclList ans){
	int x = 0;
	for(int i = 0; i < ans.size(); i++)
	    System.out.println( (++x) +". " + ans.getTerm(i).toString());
    }

    // check for correct order in song(...)
    // oaa_Solve(searchDbase(DbaseFileName,song([artist(Art),title(Tit),album(Alb),length(Len),track(Tra),genre(Gen),year(Year),bitrate(Bit),samplingrate(Samp),comment(Com),path(Path)]),Answer),[])
    //
}
