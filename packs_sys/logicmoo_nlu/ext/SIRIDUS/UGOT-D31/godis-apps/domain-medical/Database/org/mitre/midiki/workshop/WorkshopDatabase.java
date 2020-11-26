package org.mitre.midiki.workshop;

import java.io.*;
import java.util.*;

/**
 * Simple medical diagnosis database to support the MITRE Dialogue Workshop.
 * Primary usage is to identify possible diseases in database given a list
 * of symptoms, either present or absent, and further symptoms which can be
 * used to narrow a diagnosis if multiple diseases can be diagnosed.
 * Not intended for use with a large number of diseases; the algorithms used
 * were chosen for ease of implementation, not scalability.
 *
 * @author <a href="mailto:cburke@mitre.org">Carl Burke</a>
 * @version 1.0
 * @since 1.0
 * @see Serializable
 */
public class WorkshopDatabase implements Serializable
{
    /**
     * Unsorted list of disease names.
     */
    protected ArrayList diseaseList;
    /**
     * List of synonym sets.
     */
    protected ArrayList synonymList;
    /**
     * Unsorted list of resolved symptom names.
     */
    protected List symptomList;
    /**
     * Unsorted list of test names.
     */
    protected List testList;
    /**
     * Unsorted list of medical history item names.
     */
    protected List historyList;
    /**
     * List of detailed records for diseases.
     */
    protected ArrayList diseases;
    /**
     * Maps resolved names of all symptoms to the appropriate record.
     * Only the first occurrance of a symptom is considered.
     */
    protected HashMap uniqueSymptoms;
    /**
     * Maps resolved names of all tests to the appropriate record.
     * Only the first occurrance of a symptom is considered.
     */
    protected HashMap uniqueTests;
    /**
     * Maps resolved names of all history to the appropriate record.
     * Only the first occurrance of a symptom is considered.
     */
    protected HashMap uniqueHistory;

    /**
     * Row headers for the sparse matrix.
     */
    protected DiseaseToSymptomMapping[] symptomsByDisease;
    /**
     * Column headers for the sparse matrix.
     */
    protected DiseaseToSymptomMapping[] diseasesBySymptom;

    /**
     * Row headers for the sparse matrix.
     */
    protected DiseaseToSymptomMapping[] testsByDisease;
    /**
     * Column headers for the sparse matrix.
     */
    protected DiseaseToSymptomMapping[] diseasesByTest;

    /**
     * Row headers for the sparse matrix.
     */
    protected DiseaseToSymptomMapping[] historyByDisease;
    /**
     * Column headers for the sparse matrix.
     */
    protected DiseaseToSymptomMapping[] diseasesByHistory;

    /**
     * Checks all known synonym sets for the specified string.
     * If the string is not found, it is returned unchanged. If it is
     * found in a synonym set, we return the default synonym that
     * labels the set. The default synonym is the first one encountered
     * when parsing the XML file for that synonym set.
     *
     * @param s a <code>String</code> value
     * @return a <code>String</code> value
     */
    protected String resolveSynonyms(String s)
    {
        Iterator it = synonymList.iterator();
        while (it.hasNext()) {
            Synonym syn = (Synonym)it.next();
            if (syn.isSynonymFor(s)) {
                return syn.getDefault();
            }
        }
        return s;
    }

    /**
     * Converts a disease name into a numeric index. used internally for
     * traversing the database.
     *
     * @param diseaseName a <code>String</code> value
     * @return an <code>int</code> value
     */
    protected int diseaseIndex(String diseaseName)
    {
        Iterator it = diseaseList.iterator();
        for (int idx=0; it.hasNext(); idx++) {
            String name = (String)it.next();
            if (name.equalsIgnoreCase(diseaseName)) {
                return idx;
            }
        }
        return -1;
    }

    /**
     * Converts a symptom name to a numeric index. Used internally for
     * traversing the database.
     *
     * @param symptomName a <code>String</code> value
     * @return an <code>int</code> value
     */
    protected int symptomIndex(String symptomName)
    {
        symptomName = resolveSynonyms(symptomName);
        Iterator it = symptomList.iterator();
        for (int idx=0; it.hasNext(); idx++) {
            String name = (String)it.next();
            if (name.equalsIgnoreCase(symptomName)) {
                return idx;
            }
        }
        return -1;
    }

    /**
     * Converts a test name to a numeric index. Used internally for
     * traversing the database.
     *
     * @param testName a <code>String</code> value
     * @return an <code>int</code> value
     */
    protected int testIndex(String testName)
    {
        testName = resolveSynonyms(testName);
        Iterator it = testList.iterator();
        for (int idx=0; it.hasNext(); idx++) {
            String name = (String)it.next();
            if (name.equalsIgnoreCase(testName)) {
                return idx;
            }
        }
        return -1;
    }

    /**
     * Converts a history name to a numeric index. Used internally for
     * traversing the database.
     *
     * @param historyName a <code>String</code> value
     * @return an <code>int</code> value
     */
    protected int historyIndex(String historyName)
    {
        historyName = resolveSynonyms(historyName);
        Iterator it = historyList.iterator();
        for (int idx=0; it.hasNext(); idx++) {
            String name = (String)it.next();
            if (name.equalsIgnoreCase(historyName)) {
                return idx;
            }
        }
        return -1;
    }

    /**
     * Returns true if the specified disease is compatible with the
     * observed symptoms.
     *
     * @param diseaseName a <code>String</code> value
     * @param symptoms a <code>List</code> value
     * @param tests a <code>List</code> value
     * @param history a <code>List</code> value
     * @return a <code>boolean</code> value
     */
    protected boolean compatibleWithSymptoms(String diseaseName,
                                             List symptoms,
                                             List tests,
                                             List history)
    {
        int disease = diseaseIndex(diseaseName);
        if (disease==-1) {
            System.out.println("Unrecognized disease: "+diseaseName);
            return false;
        }
        Iterator si = symptoms.iterator();
        while (si.hasNext()) {
            Symptom s = (Symptom)si.next();
            Symptom s2 = null;
            String s1 = resolveSynonyms(s.name);
            boolean matched = false;
            boolean found = false;
            DiseaseToSymptomMapping dtsm =
                symptomsByDisease[disease].nextSymptomForDisease;
            while (dtsm != null) {
                String sn = resolveSynonyms(dtsm.symptom);
                Object o = uniqueSymptoms.get(sn);
                if (o==null) {
                    System.out.println("Internal error: dtsm.symptom ("+dtsm.disease+"/"+dtsm.symptom+") resolves to unrecognized symptom");
                    return false;
                }
                s2 = (Symptom)o;
                if (s1.equals(sn)) {
                    found = true;
                    matched = (s.present==s2.present);
                    break;
                }
                dtsm = dtsm.nextSymptomForDisease;
            }
            if (!matched) {
                // the specified symptom has not been matched for the disease.
                // however, if the symptom specifies that it is 'not present',
                // and it was not found, we can construe this as a match.
                if (found) {
                    // we found the symptom, but the presence data disagrees.
                    // if the symptom is not present and not required,
                    // then we can ignore this; otherwise we return false.
                    if (s.present || s2.required)
                        return false;
                }
                if (s.present) return false; // required and not found
            }
        }
        //
        // check any diagnostic tests
        //
        si = tests.iterator();
        while (si.hasNext()) {
            DiagnosticTest s = (DiagnosticTest)si.next();
            DiagnosticTest s2 = null;
            String s1 = resolveSynonyms(s.name);
            boolean matched = false;
            boolean found = false;
            DiseaseToSymptomMapping dtsm =
                testsByDisease[disease].nextSymptomForDisease;
            while (dtsm != null) {
                String sn = resolveSynonyms(dtsm.symptom);
                Object o = uniqueTests.get(sn);
                if (o==null) {
                    System.out.println("Internal error: dtsm.symptom ("+dtsm.disease+"/"+dtsm.symptom+") resolves to unrecognized symptom");
                    return false;
                }
                s2 = (DiagnosticTest)o;
                if (s1.equals(sn)) {
                    found = true;
                    matched = (s.present==s2.present);
                    break;
                }
                dtsm = dtsm.nextSymptomForDisease;
            }
            if (!matched) {
                // the specified symptom has not been matched for the disease.
                // however, if the symptom specifies that it is 'not present',
                // and it was not found, we can construe this as a match.
                if (found) {
                    // we found the symptom, but the presence data disagrees.
                    // if the symptom is not present and not required,
                    // then we can ignore this; otherwise we return false.
                    if (s.present || s2.required)
                        return false;
                }
                if (s.present) return false; // required and not found
            }
        }

        //
        // check any medical history items
        //
        si = history.iterator();
        while (si.hasNext()) {
            MedicalHistory s = (MedicalHistory)si.next();
            MedicalHistory s2 = null;
            String s1 = resolveSynonyms(s.name);
            boolean matched = false;
            boolean found = false;
            DiseaseToSymptomMapping dtsm =
                historyByDisease[disease].nextSymptomForDisease;
            while (dtsm != null) {
                String sn = resolveSynonyms(dtsm.symptom);
                Object o = uniqueHistory.get(sn);
                if (o==null) {
                    System.out.println("Internal error: dtsm.symptom ("+dtsm.disease+"/"+dtsm.symptom+") resolves to unrecognized symptom");
                    return false;
                }
                s2 = (MedicalHistory)o;
                if (s1.equals(sn)) {
                    found = true;
                    matched = (s.present==s2.present);
                    break;
                }
                dtsm = dtsm.nextSymptomForDisease;
            }
            if (!matched) {
                // the specified symptom has not been matched for the disease.
                // however, if the symptom specifies that it is 'not present',
                // and it was not found, we can construe this as a match.
                if (found) {
                    // we found the symptom, but the presence data disagrees.
                    // if the symptom is not present and not required,
                    // then we can ignore this; otherwise we return false.
                    if (s.present || s2.required)
                        return false;
                }
                if (s.present) return false; // required and not found
            }
        }

        // all presented symptoms have been matched by the disease symptoms
        return true;
    }

    /**
     * Calculates the symptoms which can be used to discriminate between
     * some of the specified diseases. Symptoms which are already used
     * in the diagnosis to this point should be passed in, and will not
     * be included in the output.
     *
     * @param diseases a <code>List</code> of possible diseases
     * @param symptoms a <code>List</code> of known symptoms
     * @return a <code>LinkedList</code> of possible discriminators
     */
    protected LinkedList discriminatingSymptoms(List diseases,
                                                List symptoms)
    {
        // if 0 or 1 diseases, nothing to discriminate between
        if (diseases.size() < 2) return new LinkedList();
        // discriminating symptoms are those that apply to at least one
        // of the listed diseases, but not to all of them.
        //
        // step 1: extract candidate set
        //
        HashMap candidates = new HashMap();
        Iterator dit = diseases.iterator();
        while (dit.hasNext()) {
            String d = (String)dit.next();
            int didx = diseaseIndex(d);
            DiseaseToSymptomMapping dtsm =
                symptomsByDisease[didx].nextSymptomForDisease;
            while (dtsm != null) {
                String sn = resolveSynonyms(dtsm.symptom);
                Object o = uniqueSymptoms.get(sn);
                if (o==null) {
                    System.out.println("Internal error: dtsm.symptom ("+dtsm.disease+"/"+dtsm.symptom+") resolves to unrecognized symptom");
                    return null;
                }
                Symptom s2 = (Symptom)o;
                boolean matched = false;
                Iterator si = symptoms.iterator();
                while (si.hasNext()) {
                    Symptom s = (Symptom)si.next();
                    String s1 = resolveSynonyms(s.name);
                    if ((s1.equals(sn)) && (s.present==s2.present)) {
                        matched = true;
                        break;
                    }
                }
                if (!matched) {
                    Object o2 = candidates.get(s2);
                    if (o2==null) {
                        candidates.put(s2, new Integer(1));
                    } else {
                        Integer i2 = (Integer)o2;
                        candidates.remove(s2);
                        int newCount = i2.intValue()+1;
                        if (newCount < diseases.size()) {
                            candidates.put(s2, new Integer(newCount));
                        }
                    }
                }
                dtsm = dtsm.nextSymptomForDisease;
            }
        }
        return new LinkedList(candidates.keySet());
    }

    /**
     * Calculates the tests which can be used to discriminate between
     * some of the specified diseases. Tests which are already used
     * in the diagnosis to this point should be passed in, and will not
     * be included in the output.
     *
     * @param diseases a <code>List</code> of possible diseases
     * @param tests a <code>List</code> of known tests
     * @return a <code>LinkedList</code> of possible discriminators
     */
    protected LinkedList discriminatingTests(List diseases,
                                             List tests)
    {
        // if 0 or 1 diseases, nothing to discriminate between
        if (diseases.size() < 2) return new LinkedList();
        // discriminating tests are those that apply to at least one
        // of the listed diseases, but not to all of them.
        //
        // step 1: extract candidate set
        //
        HashMap candidateTests = new HashMap();
        Iterator dit = diseases.iterator();
        while (dit.hasNext()) {
            String d = (String)dit.next();
            int didx = diseaseIndex(d);
            DiseaseToSymptomMapping dtsm =
                testsByDisease[didx].nextSymptomForDisease;
            while (dtsm != null) {
                String sn = resolveSynonyms(dtsm.symptom);
                Object o = uniqueTests.get(sn);
                if (o==null) {
                    System.out.println("Internal error: dtsm.symptom ("+dtsm.disease+"/"+dtsm.symptom+") resolves to unrecognized symptom");
                    return null;
                }
                DiagnosticTest s2 = (DiagnosticTest)o;
                boolean matched = false;
                Iterator si = tests.iterator();
                while (si.hasNext()) {
                    DiagnosticTest s = (DiagnosticTest)si.next();
                    String s1 = resolveSynonyms(s.name);
                    if ((s1.equals(sn)) && (s.present==s2.present)) {
                        matched = true;
                        break;
                    }
                }
                if (!matched) {
                    Object o2 = candidateTests.get(s2);
                    if (o2==null) {
                        candidateTests.put(s2, new Integer(1));
                    } else {
                        Integer i2 = (Integer)o2;
                        candidateTests.remove(s2);
                        int newCount = i2.intValue()+1;
                        if (newCount < diseases.size()) {
                            candidateTests.put(s2, new Integer(newCount));
                        }
                    }
                }
                dtsm = dtsm.nextSymptomForDisease;
            }
        }
        return new LinkedList(candidateTests.keySet());
    }

    /**
     * Calculates the history which can be used to discriminate between
     * some of the specified diseases. History which is already used
     * in the diagnosis to this point should be passed in, and will not
     * be included in the output.
     *
     * @param diseases a <code>List</code> of possible diseases
     * @param history a <code>List</code> of known history
     * @return a <code>LinkedList</code> of possible discriminators
     */
    protected LinkedList discriminatingHistory(List diseases,
                                               List history)
    {
        // if 0 or 1 diseases, nothing to discriminate between
        if (diseases.size() < 2) return new LinkedList();
        // discriminating histotry is that that applies to at least one
        // of the listed diseases, but not to all of them.
        //
        // step 1: extract candidate set
        //
        HashMap candidateHistory = new HashMap();
        Iterator dit = diseases.iterator();
        while (dit.hasNext()) {
            String d = (String)dit.next();
            int didx = diseaseIndex(d);
            DiseaseToSymptomMapping dtsm =
                historyByDisease[didx].nextSymptomForDisease;
            while (dtsm != null) {
                String sn = resolveSynonyms(dtsm.symptom);
                Object o = uniqueHistory.get(sn);
                if (o==null) {
                    System.out.println("Internal error: dtsm.symptom ("+dtsm.disease+"/"+dtsm.symptom+") resolves to unrecognized symptom");
                    return null;
                }
                MedicalHistory s2 = (MedicalHistory)o;
                boolean matched = false;
                Iterator si = history.iterator();
                while (si.hasNext()) {
                    MedicalHistory s = (MedicalHistory)si.next();
                    String s1 = resolveSynonyms(s.name);
                    if ((s1.equals(sn)) && (s.present==s2.present)) {
                        matched = true;
                        break;
                    }
                }
                if (!matched) {
                    Object o2 = candidateHistory.get(s2);
                    if (o2==null) {
                        candidateHistory.put(s2, new Integer(1));
                    } else {
                        Integer i2 = (Integer)o2;
                        candidateHistory.remove(s2);
                        int newCount = i2.intValue()+1;
                        if (newCount < diseases.size()) {
                            candidateHistory.put(s2, new Integer(newCount));
                        }
                    }
                }
                dtsm = dtsm.nextSymptomForDisease;
            }
        }
        return new LinkedList(candidateHistory.keySet());
    }

    /**
     * Prints a cross-reference listing of diseases by symptom on System.out.
     * Not useful for querying, but may be informative.
     *
     */
    public void printDiseasesBySymptom()
    {
        //
        // print out a cross-reference of diseases by symptom
        //
        for (int i=0; i<diseasesBySymptom.length; i++) {
            DiseaseToSymptomMapping dtsm = diseasesBySymptom[i];
            System.out.println(dtsm.symptom+":");
            while (dtsm.nextDiseaseForSymptom != null) {
                dtsm = dtsm.nextDiseaseForSymptom;
                System.out.println("   "+dtsm.disease);
            }
            
        }
    }

    /**
     * Creates an empty <code>WorkshopDatabase</code> instance.
     * Use <code>WorkshopDatabaseFactory.newDatabase()</code> to populate it,
     * or load it from an <code>ObjectInputStream</code>.
     *
     */
    public WorkshopDatabase() {
    }

    /**
     * Returns a list containing the names of all the diseases
     * in the database. <code>getDiseaseDescription()</code> is warranted
     * to work for every name in this list.
     *
     * @return a <code>List</code> value
     */
    public List getAllDiseases()
    {
        return diseaseList;
    }

    /**
     * Returns the list of unique symptoms in the database. There is one
     * unique symptom for every synonym set, plus all symptoms whose names
     * do not appear in synonym sets. Does not consider presence value
     * when compiling the list, but that doesn't matter for the simple
     * example we are using for the workshop because all disease descriptions
     * include only those symptoms which must be present.
     *
     * @return a <code>List</code> value
     */
    public List getAllSymptoms()
    {
        return symptomList;
    }

    /**
     * Returns the names of tests in the database. 
     *
     * @return a <code>List</code> value
     */
    public List getAllTestNames()
    {
        return testList;
    }

    /**
     * Returns the list of unique tests in the database. 
     *
     * @return a <code>List</code> value
     */
    public List getAllTests()
    {
        return new LinkedList(uniqueTests.values());
    }

    /**
     * Returns the list of unique history items in the database. 
     *
     * @return a <code>List</code> value
     */
    public List getAllHistoryItems()
    {
        return historyList;
    }

    /**
     * Returns all synonym sets specified in the database.
     *
     * @return a <code>List</code> value
     */
    public List getAllSymptomSynonyms()
    {
        return synonymList;
    }

    /**
     * Searches the database for diseases matching the specified
     * symptoms. If no diseases are found, the lists of possible
     * diseases and discriminating symptoms will be empty. If only
     * one disease is found, the list of discriminating symptoms will
     * be empty. Otherwise, the list of discriminating symptoms will
     * hold all symptoms and thier associated tests which can narrow
     * down the diagnosis. The symptoms are not ordered in any particular way.
     *
     * @param symptoms a <code>List</code> value
     * @return a <code>QueryResponse</code> value
     */
    public QueryResponse getApplicableDiseases(List symptoms,
                                               List tests,
                                               List history)
    {
        LinkedList possibleDiseases = new LinkedList();
        Iterator it = diseaseList.iterator();
        while (it.hasNext()) {
            String diseaseName = (String)it.next();
            if (compatibleWithSymptoms(diseaseName, symptoms, tests, history)) {
                possibleDiseases.add(diseaseName);
            }
        }
        LinkedList possibleDiscriminators =
            discriminatingSymptoms(possibleDiseases, symptoms);
        LinkedList possibleTests =
            discriminatingTests(possibleDiseases, tests);
        LinkedList possibleHistory =
            discriminatingHistory(possibleDiseases, history);
        return new QueryResponse(possibleDiseases,
                                 possibleDiscriminators,
                                 possibleTests,
                                 possibleHistory);
    }

    /**
     * Locates the detailed record for the specified disease.
     * Case is not significant. Returns null if no matching disease found.
     *
     * @param diseaseName a <code>String</code> value
     * @return a <code>Disease</code> value
     */
    public Disease getDiseaseDescription(String diseaseName)
    {
        Iterator it = diseases.iterator();
        while (it.hasNext()) {
            Disease d = (Disease)it.next();
            if (d.name.equalsIgnoreCase(diseaseName)) {
                return d;
            }
        }
        return null;
    }

}

