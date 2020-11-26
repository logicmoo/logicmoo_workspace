package org.mitre.midiki.workshop;

import java.io.Serializable;
import java.util.*;

/**
 * Database record containing detailed information about a disease.
 *
 * @author <a href="mailto:cburke@mitre.org">Carl Burke</a>
 * @version 1.0
 * @since 1.0
 * @see Serializable
 */
public class Disease implements Serializable
{
    /**
     * The common name of the disease.
     */
    public String name;
    /**
     * A few paragraphs describing the disease.
     */
    public String description;
    /**
     * Text identifying regions in which this disease is known
     * to be present.
     */
    public String regions;
    /**
     * List of symptoms presented by this disease.
     */
    public ArrayList symptoms;
    /**
     * List of tests presented by this disease.
     */
    public ArrayList tests;
    /**
     * List of medical history items presented by this disease.
     */
    public ArrayList history;
    /**
     * Text describing available treatments for this disease.
     */
    public String treatment;
    /**
     * Text describing possible measures for preventing infection.
     */
    public String prevention;

    /**
     * Generates a string representation of this object.
     *
     * @return a <code>String</code> value
     */
    public String toString()
    {
        StringBuffer sb = new StringBuffer(1024);
        sb.append("Disease(");
        sb.append(name);
        sb.append(",");
        sb.append("description='");
        sb.append(description);
        sb.append("',regions='");
        sb.append(regions);
        sb.append("',symptoms='");
        sb.append(symptoms);
        sb.append("',tests='");
        sb.append(tests);
        sb.append("',history='");
        sb.append(history);
        sb.append("',treatment='");
        sb.append(treatment);
        sb.append("',prevention='");
        sb.append(prevention);
        sb.append("')");
        return sb.toString();
    }

    /**
     * Empty constructor.
     *
     */
    public Disease()
    {
    }
}
