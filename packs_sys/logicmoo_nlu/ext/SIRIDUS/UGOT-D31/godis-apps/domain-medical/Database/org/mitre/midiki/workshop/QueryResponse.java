package org.mitre.midiki.workshop;

import java.util.*;

/**
 * Wrapper class for the results of a database query.
 *
 * @author <a href="mailto:cburke@mitre.org">Carl Burke</a>
 * @version 1.0
 * @since 1.0
 */
public class QueryResponse
{
    /**
     * The list of all diseases which could be diagnosed given the
     * symptoms that were presented.
     */
    public List possibleDiagnoses;
    /**
     * Possibly empty list of symptoms which could be added to the
     * last query to reduce the number of diagnosed diseases.
     */
    public List discriminatingSymptoms;
    /**
     * Possibly empty list of tests which could be added to the
     * last query to reduce the number of diagnosed diseases.
     */
    public List discriminatingTests;
    /**
     * Possibly empty list of history which could be added to the
     * last query to reduce the number of diagnosed diseases.
     */
    public List discriminatingHistory;

    /**
     * Generates a string representation of the query result.
     *
     * @return a <code>String</code> value
     */
    public String toString()
    {
        StringBuffer sb = new StringBuffer(1024);
        sb.append("QueryResponse(");
        sb.append(possibleDiagnoses.toString());
        sb.append(",");
        sb.append(discriminatingSymptoms.toString());
        sb.append(",");
        sb.append(discriminatingTests.toString());
        sb.append(",");
        sb.append(discriminatingHistory.toString());
        sb.append(")");
        return sb.toString();
    }

    /**
     * Creates an empty <code>QueryResponse</code> instance.
     *
     */
    public QueryResponse()
    {
    }

    /**
     * Creates a new <code>QueryResponse</code> instance.
     *
     * @param d a <code>List</code> value
     * @param s a <code>List</code> value
     */
    public QueryResponse(List d, List s, List t, List h)
    {
        this();
        possibleDiagnoses=d; discriminatingSymptoms=s;
        discriminatingTests=t; discriminatingHistory=h;
    }

}
