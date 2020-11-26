package org.mitre.midiki.workshop;

import java.io.Serializable;

/**
 * Indicates that the presence of the specified symptom
 * supports a diagnosis of the specified disease.
 * The database is implemented as a sparse matrix in which
 * each entry is a <code>DiseaseToSymptomMapping</code>.
 * Not a user-accessible object.
 *
 * @author <a href="mailto:cburke@mitre.org">Carl Burke</a>
 * @version 1.0
 * @since 1.0
 * @see Serializable
 */
public class DiseaseToSymptomMapping implements Serializable
{
    /**
     * Name of the diagnosable disease.
     */
    protected String disease;
    /**
     * Name of the presented symptom.
     */
    protected String symptom;
    /**
     * Reference to the next symptom for this disease.
     */
    protected DiseaseToSymptomMapping nextSymptomForDisease;
    /**
     * Reference to the next disease that presents this symptom.
     */
    protected DiseaseToSymptomMapping nextDiseaseForSymptom;

    /**
     * No-arg constructor.
     *
     */
    public DiseaseToSymptomMapping()
    {
    }

    /**
     * Creates a new <code>DiseaseToSymptomMapping</code> instance.
     *
     * @param d a <code>String</code> value
     * @param s a <code>String</code> value
     */
    public DiseaseToSymptomMapping(String d, String s)
    {
        disease=d; symptom=s;
    }

    /**
     * Adds a new matrix entry to the end of the chain for this symptom.
     *
     * @param dtsm a <code>DiseaseToSymptomMapping</code> value
     */
    public void addDisease(DiseaseToSymptomMapping dtsm)
    {
        DiseaseToSymptomMapping ndfs = nextDiseaseForSymptom;
        DiseaseToSymptomMapping last = this;
        while (ndfs!=null) {
            last = ndfs;
            ndfs = ndfs.nextDiseaseForSymptom;
        }
        last.nextDiseaseForSymptom = dtsm;
    }

    /**
     * Adds a new entry to the end of the chain for this disease.
     *
     * @param dtsm a <code>DiseaseToSymptomMapping</code> value
     */
    public void addSymptom(DiseaseToSymptomMapping dtsm)
    {
        DiseaseToSymptomMapping nsfd = nextSymptomForDisease;
        DiseaseToSymptomMapping last = this;
        while (nsfd!=null) {
            last = nsfd;
            nsfd = nsfd.nextSymptomForDisease;
        }
        last.nextSymptomForDisease = dtsm;
    }
}
