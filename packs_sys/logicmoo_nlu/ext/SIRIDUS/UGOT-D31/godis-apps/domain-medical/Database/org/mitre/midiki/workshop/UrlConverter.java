package org.mitre.midiki.workshop;

/**
 * Translation class to encode/decode URLs. Allows queries to be sent
 * via HTTP GET within the URL itself, and subsequently decoded by the
 * query processor. Shamelessly ported from JavaScript at
 * http://www.blooberry.com/indexdot/html/topics/urlencoding.htm.
 *
 * @author <a href="mailto:cburke@mitre.org">Carl Burke</a>
 * @version 1.0
 * @since 1.0
 */
public class UrlConverter
{
    /**
     * Hexadecimal digits.
     */
    static protected char[] hexVals =
    {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
     'A', 'B', 'C', 'D', 'E', 'F'};
    /**
     * Graphic characters which must always be converted inside a URL.
     */
    static protected String unsafeAnywhereString = "\"<>%\\^[]`+$,";
    /**
     * Graphic characters which must be encoded if they are to appear
     * in the URL without thier usual syntactic implications.
     */
    static protected String unsafeLocationString = ";/?:@=&#";

    /**
     * Returns true if the specified character is unsafe anywhere.
     *
     * @param compareChar a <code>char</code> value
     * @return a <code>boolean</code> value
     */
    public boolean isUnsafe(char compareChar)
    {
        if ((unsafeAnywhereString.indexOf(compareChar) == -1) &&
            (compareChar > 32) &&
            (compareChar < 123))
        { return false; } // found no unsafe chars, return false
        else
        { return true; }
    }

    /**
     * Returns true if the specified character is unsafe in the location,
     * and you don't want it to have the standard meaning.
     *
     * @param compareChar a <code>char</code> value
     * @return a <code>boolean</code> value
     */
    public boolean isUnsafeLocation(char compareChar)
    {
        if ((unsafeAnywhereString.indexOf(compareChar) == -1) &&
            (unsafeLocationString.indexOf(compareChar) == -1) &&
            (compareChar > 32) &&
            (compareChar < 123))
        { return false; } // found no unsafe chars, return false
        else
        { return true; }
    }

    /**
     * Converts an integer into a hexadecimal (or smaller radix) string.
     *
     * @param num an <code>int</code> value
     * @param radix an <code>int</code> value
     * @return a <code>String</code> value
     */
    public String decToHex(int num, int radix)
    {
        String hexString = "";
        while (num >= radix)
        {
            int temp = num % radix;
            num = (num / radix);
            hexString = hexString + hexVals[temp];
        }
        hexString = hexString + hexVals[num];
        return reversal(hexString);
    }

    /**
     * Reverses the string.
     *
     * @param s a <code>String</code> value
     * @return a <code>String</code> value
     */
    public String reversal(String s)
    {
        int len = s.length();
        String trans = "";
        for (int i=0; i<len; i++)
        { trans = trans + s.substring(len-i-1, len-i); }
        return trans;
    }

    /**
     * Converts a character into a hexadecimal URL escape sequence.
     *
     * @param val a <code>char</code> value
     * @return a <code>String</code> value
     */
    public String convert(char val)
    { return  "%" + decToHex(val, 16); }

    /**
     * Encodes a string into a safe URL location.
     *
     * @param val a <code>String</code> value
     * @return a <code>String</code> value
     */
    public String encode(String val)
    {
        int len     = val.length();
        int i       = 0;
        
        String newStr  = "";
        
        for (i=0;i<len;i++)
        {
            // hack to eliminate the rest of unicode from this
            if (val.charAt(i) < 255)
            {
                if (isUnsafeLocation(val.charAt(i)) == false)
                { newStr = newStr + val.substring(i,i+1); }
                else
                { newStr = newStr + convert(val.charAt(i)); }
            }
            else // woopsie! restore.
            {
                throw new RuntimeException("unhandled multibyte character");
            }
        }
        return newStr;
    }

    /**
     * Decodes an encoded URL string.
     *
     * @param val a <code>String</code> value
     * @return a <code>String</code> value
     */
    public String decode(String val)
    {
        int len     = val.length();
        int backlen = len;
        int i       = 0;
        
        String newStr  = "";
        String frag    = "";
        String encval  = "";
        String original = val;

        while (backlen > 0)
        {
            int lastpercent = val.lastIndexOf("%");
            if (lastpercent != -1) // we found a % char. Need to handle
            {
                // everything *after* the %
                frag = val.substring(lastpercent+1);
                // re-assign val to everything *before* the %
                val  = val.substring(0,lastpercent);
                if (frag.length() >= 2) // end contains unencoded
                {
                    //  alert ("frag is greater than or equal to 2");
                    encval = frag.substring(0,2);
                    newStr = frag.substring(2) + newStr;
                    //convert the char here. for now it just doesn't add it.
                    if ("01234567890abcdefABCDEF".indexOf(encval.charAt(0)) != -1 &&
                        "01234567890abcdefABCDEF".indexOf(encval.charAt(1)) != -1)
                    {
                        try {
                            int ccode = Integer.parseInt(encval, 16);
                            encval = new String(new char[]{(char)ccode});
                        } catch (NumberFormatException e) {
                            encval="~";
                        }
                        newStr = encval + newStr; // prepend the char in
                    }
                    // if so, convert. Else, ignore it.
                }
                // adjust length of the string to be examined
                backlen = lastpercent;
            } else {
                // if there is no %, just leave the value as-is
                newStr = val + newStr; backlen = 0;
            }
        }
        return newStr;
    }

    static public void main(String[] args)
    {
        UrlConverter uc = new UrlConverter();

        System.out.println(uc.encode("this is a \"test\" of the converter"));
        System.out.println(uc.decode("g*%22swimming%20in%20fresh%20water%22/no"));
    }
}
