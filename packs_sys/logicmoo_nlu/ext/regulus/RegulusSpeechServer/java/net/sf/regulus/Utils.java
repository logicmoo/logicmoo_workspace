package net.sf.regulus;

import java.util.Collection;
import java.util.Iterator;

public class Utils {

    /**
     * Returns items contained in collection as a String of items separated by the String separator  
     * @param collection
     * @param separator
     * @return
     */
    public final static String collectionToString(final Collection collection, final String separator) {
        StringBuffer result = new StringBuffer();

        Iterator iter = collection.iterator();
        int numItems = collection.size();
        int j = 0;
        
        while(iter.hasNext()){
            result.append(iter.next());
            j++;
            if(j != numItems){
                result.append(separator);                            
            }
        }

        return result.toString();
    }
    
    /**
     * Returns true if the String provided contains only whitespace, is the empty String, or null 
     * @param str
     * @return
     */
    public static boolean isStringEmpty(String str) {
        if ((str == null) || (str.trim().equals(""))) {
            return true;
        }
        return false;
    }

}
