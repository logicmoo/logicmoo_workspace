package org.mitre.midiki.workshop;

import java.io.Serializable;
import java.util.*;

/**
 * Identifies a list of symptom names which are considered to be identical.
 * The first symptom in the list is the way it will be known.
 * Case is not significant, but no more sophisticated fault-tolerance
 * is provided. In particular, there is no notion of distance between
 * a presented string and this synonym set.
 *
 * @author <a href="mailto:cburke@mitre.org">Carl Burke</a>
 * @version 1.0
 * @since 1.0
 * @see Serializable
 */
public class Synonym implements Serializable
{
    /**
     * List of strings in this set.
     */
    private LinkedList syns;

    /**
     * Generate a string representation of this set.
     *
     * @return a <code>String</code> value
     */
    public String toString()
    {
        StringBuffer sb = new StringBuffer(128);
        sb.append("Synonym(");
        Iterator it = syns.iterator();
        for (int idx = 0; it.hasNext(); idx++) {
            String syn = (String)it.next();
            if (idx==0) {
                sb.append("default=");
                sb.append(syn);
            } else {
                sb.append(",");
                sb.append(syn);
            }
        }
        sb.append(")");
        return sb.toString();
    }

    /**
     * Creates a new <code>Synonym</code> instance.
     *
     */
    public Synonym()
    {
        syns = new LinkedList();
    }

    /**
     * Adds a synonym to the set.
     *
     * @param s a <code>String</code> value
     */
    public void addSynonym(String s)
    {
        syns.add(s);
    }

    /**
     * Returns the first string added to this set.
     *
     * @return a <code>String</code> value
     */
    public String getDefault()
    {
        return (String) syns.getFirst();
    }

    /**
     * Returns <code>true</code> if the specified string appears
     * in this synonym set.
     *
     * @param n a <code>String</code> value
     * @return a <code>boolean</code> value
     */
    public boolean isSynonymFor(String n)
    {
        Iterator it = syns.iterator();
        while (it.hasNext()) {
            String s = (String)it.next();
            if (s.equalsIgnoreCase(n)) return true;
        }
        return false;
    }

    /**
     * Provides public access to the contents of this synonym set.
     *
     * @return an <code>Iterator</code> value
     */
    public Iterator iterator()
    {
        return syns.iterator();
    }
}
