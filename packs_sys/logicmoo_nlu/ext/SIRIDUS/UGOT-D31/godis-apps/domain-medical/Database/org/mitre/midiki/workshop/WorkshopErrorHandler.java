
package org.mitre.midiki.workshop;

import javax.xml.parsers.*;
import javax.xml.transform.*;
import javax.xml.transform.stream.*;
import javax.xml.transform.dom.*;
import java.io.*;
import org.w3c.dom.*;
import org.xml.sax.*;

/**
 * Error handler for use by XML document parser.
 *
 * @author <a href="mailto:cburke@mitre.org">Carl Burke</a>
 * @version 1.0
 * @since 1.0
 * @see org.xml.sax.ErrorHandler
 */
public class WorkshopErrorHandler implements org.xml.sax.ErrorHandler
{
    public void warning(SAXParseException exception)
        throws SAXException
    {
        System.out.println("*** caught WARNING at "+
                           exception.getLineNumber()+","+
                           exception.getColumnNumber()+" in "+
                           exception.getPublicId()+"; "+
                           exception.getSystemId());
        exception.printStackTrace();
        System.out.println("*** continuing...");
    }
    public void error(SAXParseException exception)
        throws SAXException
    {
        System.out.println("*** caught ERROR at "+
                           exception.getLineNumber()+","+
                           exception.getColumnNumber()+" in "+
                           exception.getPublicId()+"; "+
                           exception.getSystemId());
        exception.printStackTrace();
        System.out.println("*** continuing...");
    }
    public void fatalError(SAXParseException exception)
        throws SAXException
    {
        System.out.println("*** caught FATAL ERROR at "+
                           exception.getLineNumber()+","+
                           exception.getColumnNumber()+" in "+
                           exception.getPublicId()+"; "+
                           exception.getSystemId());
        throw exception;
    }
}
