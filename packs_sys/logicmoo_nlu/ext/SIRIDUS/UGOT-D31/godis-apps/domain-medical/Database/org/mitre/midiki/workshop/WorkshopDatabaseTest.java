package org.mitre.midiki.workshop;

import java.io.*;
import java.io.IOException;

import java.util.*;

/**
 * Simple test class for dialogue workshop backend. Allows a user to run
 * tests against the disease database from the command line. Run with no
 * arguments to get the syntax for specific tests. It should be easy for
 * a Java programmer to adapt this class to the needs of a dialogue system
 * with a Java interface.
 *
 * @author <a href="mailto:cburke@mitre.org">Carl Burke</a>
 * @version 1.0
 * @since 1.0
 */
public class WorkshopDatabaseTest
{
    /**
     * Checks the container for a copy of this string, ignoring case.
     * If the item is found, the canonical version is returned.
     *
     * @param c a <code>Container</code> value
     * @param s a <code>String</code> value
     * @return a <code>String</code> value
     */
    static public String containsIgnoreCase(Collection c, String s)
    {
        Iterator it = c.iterator();
        while (it.hasNext()) {
            String sit = (String)it.next();
            if (sit.equalsIgnoreCase(s)) return sit;
        }
        return null;
    }

    /**
     * Parses a symptom specification of the form "symptom"/<flag> into
     * a Symptom object. The test component of the symptom is not specified.
     * If the <flag> component is not specified, it is assumed to be "yes".<br>
     * <br>parseSymptom("headache/yes") yields Symptom(headache,"",yes)
     * <br>parseSymptom("\"headache\"/no") yields Symptom(headache,"",no)
     * <br>parseSymptom("headache") yields Symptom(headache,"",yes)
     *
     * @param s a <code>String</code> value
     * @param wd a <code>WorkshopDatabase</code> in which to search for the symptom name
     * @return a <code>Symptom</code> value
     */
    static public Symptom parseSymptom(String s,
                                       WorkshopDatabase wd)
    {
        int slash = s.lastIndexOf("/");
        String presence = null;
        if (slash==-1) {
            presence = "yes";
        } else {
            presence = s.substring(slash+1);
            s = s.substring(0,slash);
        }
        if (s.startsWith("\"")) {
            s = s.substring(1,s.length()-1);
        }
        s = containsIgnoreCase(wd.symptomList,wd.resolveSynonyms(s));
        if (s==null) return null;
        return new Symptom(s,true,presence.equals("yes"));
    }

    /**
     * Parses a symptom specification of the form "symptom"/<flag> into
     * a DiagnosticTest object. The test and result components of the test
     * are not specified.
     * If the <flag> component is not specified, it is assumed to be "yes".<br>
     *@see parseSymptom
     *
     * @param s a <code>String</code> value
     * @param wd a <code>WorkshopDatabase</code> in which to search for the test name
     * @return a <code>DiagnosticTest</code> value
     */
    static public DiagnosticTest parseTest(String s,
                                           WorkshopDatabase wd)
    {
        int slash = s.lastIndexOf("/");
        String presence = null;
        if (slash==-1) {
            presence = "yes";
        } else {
            presence = s.substring(slash+1);
            s = s.substring(0,slash);
        }
        if (s.startsWith("\"")) {
            s = s.substring(1,s.length()-1);
        }
        s = containsIgnoreCase(wd.testList,wd.resolveSynonyms(s));
        if (s==null) return null;
        return new DiagnosticTest(s,"","",true,presence.equals("yes"));
    }

    /**
     * Parses a symptom specification of the form "symptom"/<flag> into
     * a MedicalHistory object. 
     * If the <flag> component is not specified, it is assumed to be "yes".<br>
     *@see parseSymptom
     *
     * @param s a <code>String</code> value
     * @param wd a <code>WorkshopDatabase</code> in which to search for the history item name
     * @return a <code>MedicalHistory</code> value
     */
    static public MedicalHistory parseHistory(String s,
                                              WorkshopDatabase wd)
    {
        int slash = s.lastIndexOf("/");
        String presence = null;
        if (slash==-1) {
            presence = "yes";
        } else {
            presence = s.substring(slash+1);
            s = s.substring(0,slash);
        }
        if (s.startsWith("\"")) {
            s = s.substring(1,s.length()-1);
        }
        s = containsIgnoreCase(wd.historyList,wd.resolveSynonyms(s));
        if (s==null) return null;
        return new MedicalHistory(s,true,presence.equals("yes"));
    }

    /**
     * Reads arguments and tests queries against the specified database.
     *
     * @param args a <code>String[]</code> value
     */
    public static void main(String[] args) {
        WorkshopDatabase wd = null;
        if (args.length >= 2) {
            try {
                FileInputStream fos = new FileInputStream(args[0]);
                ObjectInputStream oos = new ObjectInputStream(fos);
                wd = (WorkshopDatabase)oos.readObject();
                fos.close();
                System.out.println("Read database from "+args[0]);
            } catch (Exception e) {
                System.out.println("*** Couldn't read disease database ***");
                e.printStackTrace();
                return;
            }

            switch (args[1].charAt(0)) {
                case 'd':
                    System.out.println("*** GET DISEASE NAMES ***");
                    System.out.println(wd.getAllDiseases());
                    break;
                case 's':
                    System.out.println("*** GET SYMPTOM NAMES ***");
                    System.out.println(wd.getAllSymptoms());
                    break;
                case 't':
                    System.out.println("*** GET TEST NAMES AND SYMPTOMS ***");
                    System.out.println(wd.getAllTests());
                    break;
                case 'h':
                    System.out.println("*** GET HISTORY ITEM NAMES ***");
                    System.out.println(wd.getAllHistoryItems());
                    break;
                case 'n':
                    System.out.println("*** GET SYNONYMS ***");
                    System.out.println(wd.getAllSymptomSynonyms());
                    break;
                case 'r':
                    System.out.println("*** GET DISEASE DESCRIPTION ***");
                    if (args.length < 3) {
                        System.out.println("Error: no disease name specified");
                        return;
                    }
                    Disease descr = wd.getDiseaseDescription(args[2]);
                    if (descr==null) {
                        System.out.println(args[2]+" not stored in database");
                    } else {
                        System.out.println(descr);
                    }
                    break;
                case 'g':
                    System.out.println("*** DIAGNOSE DISEASE FROM SPECIFIED SYMPTOMS ***");
                    LinkedList symp = new LinkedList();
                    LinkedList test = new LinkedList();
                    LinkedList hist = new LinkedList();
                    for (int i=2; i<args.length; i++) {
                        Symptom newSymptom = parseSymptom(args[i],wd);
                        if (newSymptom != null) {
                            System.out.println(newSymptom);
                            symp.add(newSymptom);
                        }
                    }
                    for (int i=2; i<args.length; i++) {
                        DiagnosticTest newTest = parseTest(args[i],wd);
                        if (newTest != null) {
                            System.out.println(newTest);
                            test.add(newTest);
                        }
                    }
                    for (int i=2; i<args.length; i++) {
                        MedicalHistory newHistory = parseHistory(args[i],wd);
                        if (newHistory != null) {
                            System.out.println(newHistory);
                            hist.add(newHistory);
                        }
                    }
                    System.out.println(wd.getApplicableDiseases(symp,test,hist));
                    break;
                default:
                    System.out.println("Error: unrecognized option "+args[1]);
                    break;
            }
        } else {
            System.out.println("Usage: java org.mitre.midiki.workshop.WorkshopDatabaseTest <dbname> <testid>");
            System.out.println("where");
            System.out.println("   <dbname> - typically 'diseasedb.ser'");
            System.out.println("   <testid>");
            System.out.println("      d - get all disease names");
            System.out.println("      s - get all symptom names");
            System.out.println("      t - get all testable symptoms and test names");
            System.out.println("      h - get all history item names");
            System.out.println("      n - get all symptom synonyms");
            System.out.println("      r <name> - get description of named disease");
            System.out.println("      g <findings> - diagnose given the named findings");
            System.out.println("        each finding is of the form \"name\"/<yesno>");
            System.out.println("        (<yesno> is either yes or no)");
            System.out.println("        Separate multiple findings with spaces.");
            System.out.println("        Findings obtained via diagnostic tests are allowed,");
            System.out.println("        as are items of medical history, as well as symptoms");
            System.out.println("        reported by the patient.");
        }
    }
}

