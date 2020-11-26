package org.mitre.midiki.workshop;

import javax.xml.parsers.*;
import javax.xml.transform.*;
import javax.xml.transform.stream.*;
import javax.xml.transform.dom.*;
import org.w3c.dom.*;
import org.xml.sax.*;

import java.io.*;
import java.io.IOException;

import java.util.*;

/**
 * Factory class for parsing XML files conforming to the diseasedb.dtd
 * into WorkshopDatabase objects. 
 *
 * @author <a href="mailto:cburke@mitre.org">Carl Burke</a>
 * @version 1.0
 * @since 1.0
 */
public class WorkshopDatabaseFactory
{
    /**
     * Generates a master list of all disease names in the database.
     * The details of each disease are filled in later.
     *
     * @param l an <code>org.w3c.dom.NodeList</code> value
     * @param wdb a <code>WorkshopDatabase</code> value
     */
    static protected void extractDiseases(org.w3c.dom.NodeList l,
                                          WorkshopDatabase wdb)
    {
        wdb.diseaseList = new ArrayList();
        if (l==null) {
            System.out.println("No disease nodes found.");
            return;
        }
        int count = l.getLength();
        for (int i=0; i<count; i++) {
            org.w3c.dom.Node n = l.item(i);
            org.w3c.dom.NamedNodeMap attrs = n.getAttributes();
            String diseaseName = attrs.getNamedItem("name").getNodeValue();
            wdb.diseaseList.add(diseaseName);
        }
        wdb.symptomsByDisease = new DiseaseToSymptomMapping[wdb.diseaseList.size()];
        for (int i=0; i<wdb.diseaseList.size(); i++) {
            wdb.symptomsByDisease[i] = new DiseaseToSymptomMapping((String)wdb.diseaseList.get(i),null);
        }
        wdb.testsByDisease = new DiseaseToSymptomMapping[wdb.diseaseList.size()];
        for (int i=0; i<wdb.diseaseList.size(); i++) {
            wdb.testsByDisease[i] = new DiseaseToSymptomMapping((String)wdb.diseaseList.get(i),null);
        }
        wdb.historyByDisease = new DiseaseToSymptomMapping[wdb.diseaseList.size()];
        for (int i=0; i<wdb.diseaseList.size(); i++) {
            wdb.historyByDisease[i] = new DiseaseToSymptomMapping((String)wdb.diseaseList.get(i),null);
        }
    }

    /**
     * Compiles a master list of unique symptoms. Symptoms are assumed to
     * be the same if they have the same name, or if thier names are synonyms.
     *
     * @param l an <code>org.w3c.dom.NodeList</code> value
     * @param wdb a <code>WorkshopDatabase</code> value
     */
    static protected void extractSymptoms(org.w3c.dom.NodeList l,
                                          WorkshopDatabase wdb)
    {
        wdb.uniqueSymptoms = new HashMap();
        if (l==null) {
            System.out.println("No symptom nodes found.");
            return;
        }
        int count = l.getLength();
        for (int i=0; i<count; i++) {
            org.w3c.dom.Node n = l.item(i);
            org.w3c.dom.NamedNodeMap attrs = n.getAttributes();
            String symptomName = 
                wdb.resolveSynonyms(attrs.getNamedItem("name").getNodeValue());
            org.w3c.dom.Node q = attrs.getNamedItem("required");
            String symptomRequiredText = "yes";
            if (q!=null) symptomRequiredText = q.getNodeValue();
            boolean symptomRequired =
                symptomRequiredText.equalsIgnoreCase("yes");
            org.w3c.dom.Node p = attrs.getNamedItem("present");
            String symptomPresentText = "yes";
            if (p!=null) symptomPresentText = p.getNodeValue();
            boolean symptomPresent =
                symptomPresentText.equalsIgnoreCase("yes");
            Object symptomRecord = wdb.uniqueSymptoms.get(symptomName);
            if (symptomRecord != null) continue;
            Symptom symp =
                new Symptom(symptomName, symptomRequired, symptomPresent);
            wdb.uniqueSymptoms.put(symptomName, symp);
        }
        wdb.symptomList = Arrays.asList(wdb.uniqueSymptoms.keySet().toArray());
        wdb.diseasesBySymptom = new DiseaseToSymptomMapping[wdb.symptomList.size()];
        for (int i=0; i<wdb.symptomList.size(); i++) {
            wdb.diseasesBySymptom[i] = new DiseaseToSymptomMapping(null,(String)wdb.symptomList.get(i));
        }
    }

    /**
     * Compiles a master list of unique tests.
     *
     * @param l an <code>org.w3c.dom.NodeList</code> value
     * @param wdb a <code>WorkshopDatabase</code> value
     */
    static protected void extractTests(org.w3c.dom.NodeList l,
                                       WorkshopDatabase wdb)
    {
        wdb.uniqueTests = new HashMap();
        if (l==null) {
            System.out.println("No test nodes found.");
            return;
        }
        int count = l.getLength();
        for (int i=0; i<count; i++) {
            org.w3c.dom.Node n = l.item(i);
            org.w3c.dom.NamedNodeMap attrs = n.getAttributes();
            String testName = 
                wdb.resolveSynonyms(attrs.getNamedItem("name").getNodeValue());
            org.w3c.dom.Node t = attrs.getNamedItem("test");
            String testTest = "PatientInterview";
            if (t!=null) testTest = t.getNodeValue();
            org.w3c.dom.Node r = attrs.getNamedItem("result");
            String testResult = "abnormal result";
            if (r!=null) testResult = r.getNodeValue();
            org.w3c.dom.Node q = attrs.getNamedItem("required");
            String testRequiredText = "yes";
            if (q!=null) testRequiredText = q.getNodeValue();
            boolean testRequired =
                testRequiredText.equalsIgnoreCase("yes");
            org.w3c.dom.Node p = attrs.getNamedItem("present");
            String testPresentText = "yes";
            if (p!=null) testPresentText = p.getNodeValue();
            boolean testPresent =
                testPresentText.equalsIgnoreCase("yes");
            Object testRecord = wdb.uniqueTests.get(testName);
            if (testRecord != null) continue;
            DiagnosticTest symp =
                new DiagnosticTest(testName, testTest, testResult, testRequired, testPresent);
            wdb.uniqueTests.put(testName, symp);
        }
        wdb.testList = Arrays.asList(wdb.uniqueTests.keySet().toArray());
        wdb.diseasesByTest = new DiseaseToSymptomMapping[wdb.testList.size()];
        for (int i=0; i<wdb.testList.size(); i++) {
            wdb.diseasesByTest[i] = new DiseaseToSymptomMapping(null,(String)wdb.testList.get(i));
        }
    }

    /**
     * Compiles a master list of unique history items.
     *
     * @param l an <code>org.w3c.dom.NodeList</code> value
     * @param wdb a <code>WorkshopDatabase</code> value
     */
    static protected void extractHistory(org.w3c.dom.NodeList l,
                                         WorkshopDatabase wdb)
    {
        wdb.uniqueHistory = new HashMap();
        if (l==null) {
            System.out.println("No history nodes found.");
            return;
        }
        int count = l.getLength();
        for (int i=0; i<count; i++) {
            org.w3c.dom.Node n = l.item(i);
            org.w3c.dom.NamedNodeMap attrs = n.getAttributes();
            String historyName = 
                wdb.resolveSynonyms(attrs.getNamedItem("name").getNodeValue());
            org.w3c.dom.Node q = attrs.getNamedItem("required");
            String historyRequiredText = "yes";
            if (q!=null) historyRequiredText = q.getNodeValue();
            boolean historyRequired =
                historyRequiredText.equalsIgnoreCase("yes");
            org.w3c.dom.Node p = attrs.getNamedItem("present");
            String historyPresentText = "yes";
            if (p!=null) historyPresentText = p.getNodeValue();
            boolean historyPresent =
                historyPresentText.equalsIgnoreCase("yes");
            Object historyRecord = wdb.uniqueHistory.get(historyName);
            if (historyRecord != null) continue;
            MedicalHistory symp =
                new MedicalHistory(historyName, historyRequired, historyPresent);
            wdb.uniqueHistory.put(historyName, symp);
        }
        wdb.historyList = Arrays.asList(wdb.uniqueHistory.keySet().toArray());
        wdb.diseasesByHistory = new DiseaseToSymptomMapping[wdb.historyList.size()];
        for (int i=0; i<wdb.historyList.size(); i++) {
            wdb.diseasesByHistory[i] = new DiseaseToSymptomMapping(null,(String)wdb.historyList.get(i));
        }
    }

    /**
     * Generates Synonym objects from <synonyms> tags in the database.
     *
     * @param l an <code>org.w3c.dom.NodeList</code> value
     * @param wdb a <code>WorkshopDatabase</code> value
     */
    static protected void extractSynonyms(org.w3c.dom.NodeList l,
                                          WorkshopDatabase wdb)
    {
        wdb.synonymList = new ArrayList();
        if (l==null) {
            System.out.println("No synonym nodes found.");
            return;
        }
        int count = l.getLength();
        for (int i=0; i<count; i++) {
            Synonym syn = new Synonym();
            org.w3c.dom.Node n = l.item(i);
            org.w3c.dom.NodeList children = n.getChildNodes();
            int ccnt = children.getLength();
            for (int j=0; j<ccnt; j++) {
                org.w3c.dom.Node s = children.item(j);
                if (s == null) {
                    continue;
                }
                org.w3c.dom.NamedNodeMap attrs = s.getAttributes();
                if (attrs==null) continue;
                org.w3c.dom.Node synonymNameNode = attrs.getNamedItem("name");
                String synonym = null;
                if (synonymNameNode != null)
                    synonym = synonymNameNode.getNodeValue();
                syn.addSynonym(synonym);
            }
            wdb.synonymList.add(syn);
        }
    }

    /**
     * Finds the first "#text" child of the specified node and returns
     * its value. Handy utility routine.
     *
     * @param node an <code>org.w3c.dom.Node</code> value
     * @return a <code>String</code> value
     */
    static protected String textChild(org.w3c.dom.Node node)
    {
        org.w3c.dom.NodeList l = node.getChildNodes();
        for (int i=0; i<l.getLength(); i++) {
            org.w3c.dom.Node c = l.item(i);
            if (c.getNodeName().equals("#text")) {
                return c.getNodeValue();
            }
        }
        return null;
    }

    /**
     * Creates a sparse matrix mapping diseases to thier symptoms.
     * Since each individual disease is examined during this process,
     * the set of disease descriptions are also loaded at this time.
     *
     * @param diseaseNodes an <code>org.w3c.dom.NodeList</code> value
     * @param wdb a <code>WorkshopDatabase</code> value
     */
    static protected void mapSymptomsToDiseases(org.w3c.dom.NodeList diseaseNodes,
                                                WorkshopDatabase wdb)
    {
        wdb.diseases = new ArrayList();
        int dcnt = diseaseNodes.getLength();
        for (int i=0; i<dcnt; i++) {
            Disease diseaseRecord = new Disease();
            wdb.diseases.add(diseaseRecord);
            org.w3c.dom.Node disease = diseaseNodes.item(i);
            if (disease == null) {
                System.out.println("Error fetching disease node "+i+"!");
                continue;
            }
            org.w3c.dom.NamedNodeMap dattrs = disease.getAttributes();
            if (dattrs == null) {
                System.out.println("Error fetching attributes, disease "+i+"!");
                continue;
            }
            String diseaseName = dattrs.getNamedItem("name").getNodeValue();
            diseaseRecord.name = diseaseName;
            org.w3c.dom.NodeList diseaseSections = disease.getChildNodes();
            if (diseaseSections == null) {
                System.out.println("Empty disease node "+i+"!");
                continue;
            }
            for (int j=0; j<diseaseSections.getLength(); j++) {
                org.w3c.dom.Node section = diseaseSections.item(j);
                if (section == null) {
                    System.out.println("Error fetching section node "+j+", disease "+i+"!");
                    continue;
                }
                if (section.getNodeName().equalsIgnoreCase("description")) {
                    diseaseRecord.description = textChild(section);
                }
                if (section.getNodeName().equalsIgnoreCase("regions")) {
                    diseaseRecord.regions = textChild(section);
                }
                if (section.getNodeName().equalsIgnoreCase("treatment")) {
                    diseaseRecord.treatment = textChild(section);
                }
                if (section.getNodeName().equalsIgnoreCase("prevention")) {
                    diseaseRecord.prevention = textChild(section);
                }
                if (section.getNodeName().equalsIgnoreCase("symptoms")) {
                    diseaseRecord.symptoms = new ArrayList();
                    org.w3c.dom.NodeList symptoms = section.getChildNodes();
                    if (symptoms == null) {
                        System.out.println("Empty symptoms section in disease "+i+"!");
                        continue;
                    }
                    for (int k=0; k<symptoms.getLength(); k++) {
                        org.w3c.dom.Node symptom = symptoms.item(k);
                        if (symptom == null) {
                            System.out.println("Error fetching symptom "+k+", disease "+i+"!");
                            continue;
                        }
                        if (!symptom.getNodeName().equalsIgnoreCase("symptom")) {
                            continue;
                        }
                        org.w3c.dom.NamedNodeMap attrs = symptom.getAttributes();
                        if (attrs == null) {
                            System.out.println("Error fetching symptom "+k+" attributes, disease "+i+"!");
                            continue;
                        }
                        String symptomName = 
                            wdb.resolveSynonyms(attrs.getNamedItem("name").getNodeValue());
                        Object symptomRecord = wdb.uniqueSymptoms.get(symptomName);
                        if (symptomRecord != null) {
                            diseaseRecord.symptoms.add(symptomRecord);
                        }
                        DiseaseToSymptomMapping dtsm =
                            new DiseaseToSymptomMapping(diseaseName,
                                                        symptomName);
                        wdb.symptomsByDisease[i].addSymptom(dtsm);
                        for (int ss=0; ss<wdb.diseasesBySymptom.length; ss++) {
                            if (wdb.diseasesBySymptom[ss].symptom.equals(symptomName)) {
                                wdb.diseasesBySymptom[ss].addDisease(dtsm);
                                break;
                            }
                        }
                    }
                }
                if (section.getNodeName().equalsIgnoreCase("tests")) {
                    diseaseRecord.tests = new ArrayList();
                    org.w3c.dom.NodeList tests = section.getChildNodes();
                    if (tests == null) {
                        System.out.println("Empty tests section in disease "+i+"!");
                        continue;
                    }
                    for (int k=0; k<tests.getLength(); k++) {
                        org.w3c.dom.Node test = tests.item(k);
                        if (test == null) {
                            System.out.println("Error fetching test "+k+", disease "+i+"!");
                            continue;
                        }
                        if (!test.getNodeName().equalsIgnoreCase("test")) {
                            continue;
                        }
                        org.w3c.dom.NamedNodeMap attrs = test.getAttributes();
                        if (attrs == null) {
                            System.out.println("Error fetching test "+k+" attributes, disease "+i+"!");
                            continue;
                        }
                        String testName = 
                            wdb.resolveSynonyms(attrs.getNamedItem("name").getNodeValue());
                        Object testRecord = wdb.uniqueTests.get(testName);
                        if (testRecord != null) {
                            diseaseRecord.tests.add(testRecord);
                        }
                        DiseaseToSymptomMapping dtsm =
                            new DiseaseToSymptomMapping(diseaseName,
                                                        testName);
                        wdb.testsByDisease[i].addSymptom(dtsm);
                        for (int ss=0; ss<wdb.diseasesByTest.length; ss++) {
                            if (wdb.diseasesByTest[ss].symptom.equals(testName)) {
                                wdb.diseasesByTest[ss].addDisease(dtsm);
                                break;
                            }
                        }
                    }
                }
                if (section.getNodeName().equalsIgnoreCase("history")) {
                    diseaseRecord.history = new ArrayList();
                    org.w3c.dom.NodeList history = section.getChildNodes();
                    if (history == null) {
                        System.out.println("Empty history section in disease "+i+"!");
                        continue;
                    }
                    for (int k=0; k<history.getLength(); k++) {
                        org.w3c.dom.Node historyItem = history.item(k);
                        if (historyItem == null) {
                            System.out.println("Error fetching history "+k+", disease "+i+"!");
                            continue;
                        }
                        if (!historyItem.getNodeName().equalsIgnoreCase("history-item")) {
                            continue;
                        }
                        org.w3c.dom.NamedNodeMap attrs = historyItem.getAttributes();
                        if (attrs == null) {
                            System.out.println("Error fetching history "+k+" attributes, disease "+i+"!");
                            continue;
                        }
                        String historyName = 
                            wdb.resolveSynonyms(attrs.getNamedItem("name").getNodeValue());
                        Object historyRecord = wdb.uniqueHistory.get(historyName);
                        if (historyRecord != null) {
                            diseaseRecord.history.add(historyRecord);
                        }
                        DiseaseToSymptomMapping dtsm =
                            new DiseaseToSymptomMapping(diseaseName,
                                                        historyName);
                        wdb.historyByDisease[i].addSymptom(dtsm);
                        for (int ss=0; ss<wdb.diseasesByHistory.length; ss++) {
                            if (wdb.diseasesByHistory[ss].symptom.equals(historyName)) {
                                wdb.diseasesByHistory[ss].addDisease(dtsm);
                                break;
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Parses the expected disease database sections.
     *
     * @param doc an <code>org.w3c.dom.Document</code> value
     * @param wdb a <code>WorkshopDatabase</code> value
     */
    static protected void processDocument(org.w3c.dom.Document doc,
                                          WorkshopDatabase wdb)
    {
        org.w3c.dom.NodeList diseaseNodes =
            doc.getElementsByTagName("disease");
        org.w3c.dom.NodeList symptomNodes =
            doc.getElementsByTagName("symptom");
        org.w3c.dom.NodeList testNodes =
            doc.getElementsByTagName("test");
        org.w3c.dom.NodeList historyNodes =
            doc.getElementsByTagName("history-item");
        org.w3c.dom.NodeList synonymNodes =
            doc.getElementsByTagName("synonyms");
        extractSynonyms(synonymNodes, wdb);
        extractDiseases(diseaseNodes, wdb);
        extractSymptoms(symptomNodes, wdb);
        extractTests(testNodes, wdb);
        extractHistory(historyNodes, wdb);
        mapSymptomsToDiseases(diseaseNodes, wdb);
    }

    /**
     * Opens the specified database file and interprets it as
     * an XML file conforming to diseasedb.dtd. If successful,
     * it returns a WorkshopDatabase object which can be queried.
     * The resulting WorkshopDatabase does not require an XML parser
     * to be present in the system.
     *
     * @param databaseFileName a <code>String</code> value
     * @return a <code>WorkshopDatabase</code> value
     */
    static public WorkshopDatabase newDatabase(String databaseFileName) {
        WorkshopDatabase wdb = new WorkshopDatabase();
        try {
            // in order to do validation, must enable validation
            // and set a SAXErrorHandler.
            DocumentBuilderFactory dbFac =
                DocumentBuilderFactory.newInstance();
            dbFac.setValidating(true);
            dbFac.setNamespaceAware(true);
            // May need next line for compatibility with future
            // versions of Xerces.
            // Currently turning off validation, since only DTD created
            dbFac.setAttribute("http://xml.org/sax/features/validation", new Boolean(false));
            dbFac.setAttribute("http://apache.org/xml/features/validation/schema", new Boolean(false));
            DocumentBuilder db = dbFac.newDocumentBuilder();
            db.setErrorHandler(new WorkshopErrorHandler());
            // make an input source from the document
            InputSource ss = new InputSource(new FileReader(databaseFileName));
            // parse the source
            org.w3c.dom.Document doc = db.parse(ss);
            processDocument(doc, wdb);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return wdb;
    }

    /**
     * Reads the file specified by the first program argument and parses it
     * into a WorkshopDatabase by a call to
     * newDatabase(). When called with one argument, this routine executes
     * a few simple tests against the database and dumps the results to
     * System.out. When called with at least two arguments, the second
     * argument is interpreted as a filename to which the generated
     * WorkshopDatabase should be serialized.
     *
     * @param args a <code>String[]</code> value
     */
    public static void main(String[] args) {
        if (args.length==0) {
            System.out.println("Usage: java WorkshopDatabaseFactory <diseasedb.xml> <diseasedb.ser>");
            return;
        }
        WorkshopDatabase wd = WorkshopDatabaseFactory.newDatabase(args[0]);
        if (wd==null) {
            System.out.println("Failed to load database from "+args[0]);
            return;
        }
        if (args.length == 1) {
        System.out.println("*** GET DISEASE NAMES ***");
        System.out.println(wd.getAllDiseases());
        System.out.println("*** GET SYMPTOM NAMES ***");
        System.out.println(wd.getAllSymptoms());
        System.out.println("*** GET SYNONYMS ***");
        System.out.println(wd.getAllSymptomSynonyms());
        System.out.println("*** GET DISEASE DESCRIPTIONS ***");
        Iterator it = wd.getAllDiseases().iterator();
        while (it.hasNext()) {
            String diseaseName = (String)it.next();
            System.out.println(wd.getDiseaseDescription(diseaseName));
        }
        System.out.println("*** GET DISEASES FOR SOME SELECTED SYMPTOMS ***");
        LinkedList symp = new LinkedList();
        LinkedList test = new LinkedList();
        LinkedList hist = new LinkedList();
        test.add(new DiagnosticTest("fever","","",true,true));
        System.out.println(test);
        System.out.println(wd.getApplicableDiseases(symp,test,hist));
        test.clear();
        symp.add(new Symptom("jaundice",true,true));
        System.out.println(symp);
        System.out.println(wd.getApplicableDiseases(symp,test,hist));
        symp.clear();
        symp.add(new Symptom("flu-like illness",true,true));
        System.out.println(symp);
        System.out.println(wd.getApplicableDiseases(symp,test,hist));
        symp.add(new Symptom("headache",true,true));
        System.out.println(symp);
        System.out.println(wd.getApplicableDiseases(symp,test,hist));
        symp.add(new Symptom("diarrhea",true,true));
        System.out.println(symp);
        System.out.println(wd.getApplicableDiseases(symp,test,hist));
        } else {
            try {
                FileOutputStream fos = new FileOutputStream(args[1]);
                ObjectOutputStream oos = new ObjectOutputStream(fos);
                oos.writeObject(wd);
                oos.flush();
                fos.close();
                System.out.println("Wrote serialized database to "+args[1]);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }
}

