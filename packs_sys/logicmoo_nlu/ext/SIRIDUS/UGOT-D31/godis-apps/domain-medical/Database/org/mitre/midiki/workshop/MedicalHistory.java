package org.mitre.midiki.workshop;

import java.io.Serializable;

/**
 * Represents an item in the patient's medical history which is suggestive
 * of a particular disease. Typically, disease descriptions show those historys
 * which must be present, while diagnosis queries may contain historys which
 * have been identified as absent (as distinct from historys whose status
 * is unknown).
 *
 * @author <a href="mailto:cburke@mitre.org">Carl Burke</a>
 * @version 1.0
 * @since 1.0
 * @see Serializable
 */
public class MedicalHistory implements Serializable
{
    /**
     * The name of the history.
     */
    public String name;
    /**
     * <code>true</code> if the symptom is required vs suggestive.
     */
    public boolean required;
    /**
     * <code>true</code> if the history is present.
     */
    public boolean present;

    /**
     * Generates a string representation of the history.
     *
     * @return a <code>String</code> value
     */
    public String toString()
    {
        StringBuffer sb = new StringBuffer(128);
        sb.append("MedicalHistory(");
        sb.append(name);
        sb.append(",");
        if (required) {
            sb.append("required=yes,");
        } else {
            sb.append("required=no,");
        }
        if (present) {
            sb.append("present=yes");
        } else {
            sb.append("present=no");
        }
        sb.append(")");
        return sb.toString();
    }

    /**
     * Returns <code>true</code> if the history matches the query.
     * Since synonyms are not considered, this is a less powerful
     * match than the one performed by <code>WorkshopDatabase</code>
     * in calculating a diagnosis.
     *
     * @param s a <code>MedicalHistory</code> value
     * @return a <code>boolean</code> value
     */
    public boolean matches(MedicalHistory s)
    {
        if (!name.equalsIgnoreCase(s.name)) return false;
        return (present==s.present);
    }

    /**
     * Empty constructor.
     *
     */
    public MedicalHistory()
    {
    }

    /**
     * Creates a new <code>MedicalHistory</code> instance.
     *
     * @param n a <code>String</code> value
     * @param q a <code>boolean</code> value
     * @param p a <code>boolean</code> value
     */
    public MedicalHistory(String n, boolean q, boolean p)
    {
        this();
        name=n; required=q; present=p;
    }

}
