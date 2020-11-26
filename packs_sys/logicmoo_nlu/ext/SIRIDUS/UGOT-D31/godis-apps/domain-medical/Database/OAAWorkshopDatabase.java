import com.sri.oaa2.com.*;
import com.sri.oaa2.lib.*;
import com.sri.oaa2.icl.*;

import java.io.*;
import java.util.*;

import  org.mitre.midiki.workshop.*;

public class OAAWorkshopDatabase{

    WorkshopDatabase wd;
    LibOaa oaalib;
    String agentName="WorkshopDatabase";
    IclTerm solvables;
    
    public OAAWorkshopDatabase(String[] args){
	//open database
	if (args.length >0 ) {
            try {
                FileInputStream fos = new FileInputStream(args[0]);
                ObjectInputStream oos = new ObjectInputStream(fos);
                wd = (WorkshopDatabase)oos.readObject();
		System.err.println(wd);
                fos.close();
                System.out.println("Read database from "+args[0]);
            } catch (Exception e) {
                System.err.println("*** Couldn't read disease database ***");
                e.printStackTrace();
                return;
            }
	}
	else {
	    System.err.println("*** Please specify the disease database ***");
	    return;
	}
	

	solvables=IclUtils.fromString(true, 
				      "[diagnose(Findings,QueryResponse),"+
				      "symptom_names(Symptoms),"+
				      "disease_names(Diseases),"+
				      "history_names(HistoryItems),"+
				      "symptom_synonyms(Synonyms),"+
				      "test_names(TestNames),"+
				      "tests(Tests),"+
				      "description(Disease,Description)]");
	if(!register(args)){
	    System.err.println("Could not connect to OAA");
	    System.exit(0);
	}
    }
	

	//EventHandler
    boolean handleEvent(IclTerm goal,IclTerm params, IclTerm answers){
	if(!goal.isStruct())
	    return false;
	String functor = ((IclStruct)goal).getFunctor();

	if(functor.equals("disease_names"))
	    return handleDiseaseNames(goal,params,answers);   

	else if(functor.equals("symptom_names"))
	    return handleSymptomNames(goal,params,answers); 

	else if(functor.equals("history_names"))
	    return handleHistoryNames(goal,params,answers); 
	
	else if(functor.equals("test_names"))
	    return handleTestNames(goal,params,answers); 

	else if(functor.equals("tests"))
	    return handleTests(goal,params,answers); 


	else if(functor.equals("symptom_synonyms"))
	    return handleSymptomSynonyms(goal,params,answers); 
	
	else if(functor.equals("description"))
	    return handleDescription(goal,params,answers); 

	else if(functor.equals("diagnose"))
	    return handleDiagnose(goal,params,answers);

	else return false;
	    
    }
    
    boolean handleDiseaseNames(IclTerm goal,IclTerm params, IclTerm answers){
	IclList diseases = new IclList();
	for(Iterator i=wd.getAllDiseases().iterator();i.hasNext();)
	    diseases.add(new IclStr((String)i.next()));
	goal.replaceElement(0,diseases);
	answers.add(goal);
	return true;
    }

    boolean handleSymptomNames(IclTerm goal,IclTerm params, IclTerm answers){
	IclList symptoms = new IclList();
	for(Iterator i=wd.getAllSymptoms().iterator();i.hasNext();)
	    symptoms.add(new IclStr((String)i.next()));
	goal.replaceElement(0,symptoms);
	answers.add(goal);
	return true;
    }

    
    boolean handleHistoryNames(IclTerm goal,IclTerm params, IclTerm answers){
	IclList hitems = new IclList();
	for(Iterator i=wd.getAllHistoryItems().iterator();i.hasNext();)
	    hitems.add(new IclStr((String)i.next()));
	goal.replaceElement(0,hitems);
	answers.add(goal);
	return true;
    }

    boolean handleTestNames(IclTerm goal,IclTerm params, IclTerm answers){
	IclList testnames = new IclList();
	for(Iterator i=wd.getAllTestNames().iterator();i.hasNext();)
	    testnames.add(new IclStr((String)i.next()));
	goal.replaceElement(0,testnames);
	answers.add(goal);
	return true;
    }    

     boolean handleTests(IclTerm goal,IclTerm params, IclTerm answers){   
	 IclList tests=new IclList();
	 for(Iterator i=wd.getAllTests().iterator();i.hasNext();)
	     tests.add(test2Icl((DiagnosticTest)i.next()));
	 goal.replaceElement(0,tests);
	 answers.add(goal);
	 return false;
     }

     boolean handleSymptomSynonyms(IclTerm goal,IclTerm params, IclTerm answers){   
	 IclList syns=new IclList();
	 for(Iterator i=wd.getAllSymptomSynonyms().iterator();i.hasNext();)
	     syns.add(synonym2Icl((Synonym)i.next()));
	 goal.replaceElement(0,syns);
	 answers.add(goal);
	 return false;
     }

    boolean handleDescription(IclTerm goal,IclTerm params, IclTerm answers){ 
	IclTerm disease=goal.getTerm(0);
	if(!disease.isStr())
	    return false;
	
	Disease descr = wd.getDiseaseDescription(((IclStr)disease).toIdentifyingString());
	if(descr==null)
	    return false;
	else{
	    goal.replaceElement(1,disease2Icl(descr));
	    answers.add(goal);
	    return true;
	}
						 
    }


    boolean handleDiagnose(IclTerm goal,IclTerm params, IclTerm answers){
	LinkedList symp = new LinkedList();
	LinkedList test = new LinkedList();
	LinkedList hist = new LinkedList();
	for(Iterator i=goal.getTerm(0).iterator();i.hasNext();){
	    
	    IclTerm sub=(IclTerm)i.next();
	

	    DiagnosticTest t=parseTest(sub);
	    if(t!=null){
		test.add(t);
	    }
	    else{
		Symptom s=parseSymptom(sub);
		if(s!=null){
		    symp.add(s);
		    
		}
		else{
		    MedicalHistory m=parseHistory(sub);
		    if(m!=null){
			hist.add(m);
		    }
	    }
	    }
	    
	}
	//everything added
	QueryResponse qr=wd.getApplicableDiseases(symp,test,hist);
	// System.out.println(qr);
	
	//add diseases
	IclList iclPossibleDiseases = new IclList();
	for(Iterator i=qr.possibleDiagnoses.iterator();i.hasNext();)
	    iclPossibleDiseases.add(new IclStr((String)i.next()));
	
	//add symptoms
	IclList iclDiscriminatingSymptoms = new IclList();
	for(Iterator i=qr.discriminatingSymptoms.iterator();i.hasNext();)
	    iclDiscriminatingSymptoms.add(symptom2Icl((Symptom)i.next()));
	
	
	//add tests
	IclList iclDiscriminatingTests = new IclList();
	for(Iterator i=qr.discriminatingTests.iterator();i.hasNext();)
	    iclDiscriminatingTests.add(test2Icl((DiagnosticTest)i.next()));
	
	
	//add history
	IclList iclDiscriminatingHistory = new IclList();
	for(Iterator i=qr.discriminatingHistory.iterator();i.hasNext();)
	    iclDiscriminatingTests.add(hist2Icl((MedicalHistory)i.next()));
	
	IclStruct iclQR = new IclStruct("queryResponse",
					iclPossibleDiseases,
					iclDiscriminatingSymptoms,
					iclDiscriminatingTests,
					iclDiscriminatingHistory);
	goal.replaceElement(1,iclQR);
	answers.add(goal);
	return true;
    }
    


    IclTerm symptom2Icl(Symptom s){
	String required = (s.required?"yes":"no");
	String present = (s.present?"yes":"no");
	return new IclStruct("symptom",
			    new IclStr(s.name),
			    new IclStruct("required",new IclStr(required)),
			    new IclStruct("present",new IclStr(present)));
    }

    
    IclTerm test2Icl(DiagnosticTest t){ 
	String required = (t.required?"yes":"no");
	String present = (t.present?"yes":"no");
	
	return new IclStruct("diagnosticTest",
			     new IclStr(t.name),
			     new IclStruct("test",new IclStr(t.test)),
			     new IclStruct("result",new IclStr(t.result)),
			     new IclStruct("required",new IclStr(required)),
			     new IclStruct("present",new IclStr(present)));
			     
    }

   IclTerm hist2Icl(MedicalHistory m){
	String required = (m.required?"yes":"no");
	String present = (m.present?"yes":"no");
	return new IclStruct("medicalHistory",
			    new IclStr(m.name),
			    new IclStruct("required",new IclStr(required)),
			    new IclStruct("present",new IclStr(present)));
    }


    //return List instead of term
    IclTerm synonym2Icl(Synonym s){
	IclList syns = new IclList();
	for(Iterator i=s.iterator();i.hasNext();){
	    syns.add(new IclStr((String)i.next()));
	}
	return new IclStruct("synonym",syns);
    }


    IclTerm disease2Icl(Disease d){
	IclList syms = new IclList();
	IclList tests = new IclList();
	IclList hist = new IclList();

	
	for(Iterator i=d.symptoms.iterator();i.hasNext();)
	    syms.add(symptom2Icl((Symptom)i.next()));
	
	for(Iterator i=d.tests.iterator();i.hasNext();)
	    tests.add(test2Icl((DiagnosticTest)i.next()));    

	for(Iterator i=d.history.iterator();i.hasNext();)
	    hist.add(hist2Icl((MedicalHistory)i.next())); 	

	return new IclStruct("disease",
			     new IclStr(d.name),
			     new IclStruct("description",new IclStr(d.description)),
			     new IclStruct("regions",new IclStr(d.regions)),
			     new IclStruct("symptoms",syms),
			     new IclStruct("tests",tests),
			     new IclStruct("history",hist),
			     new IclStruct("treatment",new IclStr(d.treatment)),
			     new IclStruct("prevention",new IclStr(d.prevention)));
    }
   
    


    Symptom parseSymptom(IclTerm t){
	IclTerm tmp;
	boolean presence = true;
	String name = null;
	if(t.isStr()){
	    name = ((IclStr)t).toIdentifyingString();
	    presence = true;
	}
	

	else if(t.isStruct()){
	    if(((IclStruct)t).getFunctor().equals("neg"))
		{
		presence = false;
		tmp = t.getTerm(0);

		}
	    else
		{
		   presence = true;
		   tmp = t; 
		}
	    if(tmp.isStr())
		name=((IclStr)tmp).toIdentifyingString();
	    else if( tmp.isStruct() && ((IclStruct)tmp).getFunctor().equals("symptom")){
		tmp=tmp.getTerm(0);
		if(tmp.isStr())
		    name=((IclStr)tmp).toIdentifyingString();
	    }
	}
	


		String name1 = WorkshopDatabaseTest.
		    containsIgnoreCase(wd.getAllSymptoms(),name);
		
		if(name1==null){

		    for(Iterator i=wd.getAllSymptomSynonyms().iterator();
			i.hasNext();){
			Synonym s=(Synonym)i.next();
			if(s.isSynonymFor(name)){
			    name1=s.getDefault();
			    break;
			}
			
		    }
		}

		
		
		if(name1==null)
		    return null;
		else{
		    
		    return new Symptom(name1,true,presence);
		}
    }

    DiagnosticTest parseTest(IclTerm t){
	IclTerm tmp;
	boolean presence = true;
	String name = null;
	if(t.isStr()){
	    name = ((IclStr)t).toIdentifyingString();
	    presence = true;
	}
	else if(t.isStruct()){
	    if(((IclStruct)t).getFunctor().equals("neg"))
		{
		    presence = false;
		    tmp = t.getTerm(0);
		}
	    else
		{
		    presence = true;
		    tmp = t; 
		}
	    
	    if(tmp.isStr())
		name=((IclStr)tmp).toIdentifyingString();
	    else if( tmp.isStruct() && ((IclStruct)tmp).getFunctor().equals("test")){
		tmp=tmp.getTerm(0);
		if(tmp.isStr())
		    name=((IclStr)tmp).toIdentifyingString();
	    }
	}
	if(name!=null)
	    name=WorkshopDatabaseTest.containsIgnoreCase(wd.getAllTestNames(),
						     name);
	
	if(name==null)
	    return null;
	else
	    return new DiagnosticTest(name,"","",true,presence);
    }
    

    
    MedicalHistory  parseHistory(IclTerm t){
	IclTerm tmp;
	boolean presence = true;
	String name = null;
	if(t.isStr()){
	    name = ((IclStr)t).toIdentifyingString();
	    presence = true;
	}
	else if(t.isStruct()){
	    if(((IclStruct)t).getFunctor().equals("neg"))
		{
		presence = false;
		tmp = t.getTerm(0);
		}
	    else
		{
		    presence = true;
		    tmp = t; 
		}
	    if(tmp.isStr())
		name=((IclStr)tmp).toIdentifyingString();
	    else if( tmp.isStruct() && 
		     ((IclStruct)tmp).getFunctor().equals("medicalHistory")){
		tmp=tmp.getTerm(0);
		if(tmp.isStr())
		    name=((IclStr)tmp).toIdentifyingString();
	    }
	}					
	
	if(name==null)
	    return null;
	else
	    return new MedicalHistory(name,true,presence);
    }
    



    public boolean register(String[] args){
	oaalib= new LibOaa(new LibCom(new LibComTcpProtocol(),args));
	if (!oaalib.oaaSetupCommunication(agentName)){
	    System.err.println("Couldnt connect to facilitator");
	    return false;
	}
	
	if (!oaalib.oaaRegister("parent", 
                               agentName, 
			       solvables,
			       new IclList()))
	    {
                System.err.println("Could not register");
                return false;
            }
	
	oaalib.oaaRegisterCallback("oaa_AppDoEvent",new OAAEventListener() {
		public boolean doOAAEvent(IclTerm goal, 
                                          IclList params, 
                                          IclList answers){
		    return handleEvent(goal,params,answers);
		}});
	
	  oaalib.oaaReady(true);
	  return true;
    }
    
    	
    public static void main(String[] args){
	new OAAWorkshopDatabase(args);
    }
	
	

}





