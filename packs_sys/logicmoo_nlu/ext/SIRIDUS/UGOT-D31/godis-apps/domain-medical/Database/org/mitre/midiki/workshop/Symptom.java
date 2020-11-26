package org.mitre.midiki.workshop;

import java.io.Serializable;

/**
 * Represents a physical symptom, and whether the symptom must
 * be present or absent. Typically, disease descriptions show those symptoms
 * which must be present, while diagnosis queries may contain symptoms which
 * have been identified as absent (as distinct from symptoms whose status
 * is unknown).
 *
 * @author <a href="mailto:cburke@mitre.org">Carl Burke</a>
 * @version 1.0
 * @since 1.0
 * @see Serializable
 */
public class Symptom implements Serializable
{
    /**
     * The name of the symptom.
     */
    public String name;
    /**
     * <code>true</code> if the symptom is required vs suggestive.
     */
    public boolean required;
    /**
     * <code>true</code> if the symptom is present.
     */
    public boolean present;

    /**
     * Generates a string representation of the symptom.
     *
     * @return a <code>String</code> value
     */
    public String toString()
    {
        StringBuffer sb = new StringBuffer(128);
        sb.append("Symptom(");
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
     * Returns <code>true</code> if the symptom matches the query.
     * Since synonyms are not considered, this is a less powerful
     * match than the one performed by <code>WorkshopDatabase</code>
     * in calculating a diagnosis.
     *
     * @param s a <code>Symptom</code> value
     * @return a <code>boolean</code> value
     */
    public boolean matches(Symptom s)
    {
        if (!name.equalsIgnoreCase(s.name)) return false;
        return (present==s.present);
    }

    /**
     * Empty constructor.
     *
     */
    public Symptom()
    {
    }

    /**
     * Creates a new <code>Symptom</code> instance.
     *
     * @param n a <code>String</code> value
     * @param q a <code>boolean</code> value
     * @param p a <code>boolean</code> value
     */
    public Symptom(String n, boolean q, boolean p)
    {
        this();
        name=n; required=q; present=p;
    }

}
