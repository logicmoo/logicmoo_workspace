/*This code is copyrighted by Teknowledge (c) 2001.  It is released under
the GNU Public License <http://www.gnu.org/copyleft/gpl.html>.  Users of
this code also consent, by use of this code, to credit Teknowledge, in any writings, briefings,
publications, presentations, or other representations of any software which
incorporates, builds on, or uses this code.*/
package com.tks.celt;

import org.xml.sax.ContentHandler;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import java.util.HashMap;


/**
 * <p>Title:QueryHandler </p>
 * <p>Description: QueryHandler will be used by ResponseProcessor to perform SAX callbacks</p>
 * <p>Copyright: Teknowledge Corp Copyright (c) 2002</p>
 * <p>Company: Teknowledge Corporation</p>
 * @author bvasired
 * @version 1.0
 */

public class QueryHandler implements ContentHandler, ErrorHandler {


   /**
    * holds the content produced by SAXParser
    */

     private HashMap bindings;

   /**
    * keeps name of binding encountered by SAXParser
    */
     private String bindingName;

   /**
    * keeps value of binding encountered by SAXParser
    */
     private String bindingValue;

    /**
    * The empty constructor for QueryHandler class
    */
    public QueryHandler() {

        this.bindings = new HashMap();
    }

    /**
    * takes HashMap and assigns to bindings.
    * @param HashMap passedMap
    */
    public QueryHandler(HashMap passedMap) {

        this.bindings = passedMap;
    }



   /**
    * Receive notification of a warning.     * @param sAXParseException
    * @return void
    * @exception SAXException
    * SAX parsers will use this method to report conditions that are not errors or fatal errors as defined by the XML 1.0 recommendation. The default behaviour is to take no action.
    * The SAX parser must continue to provide normal parsing events after invoking this method: it should still be possible for the application to process the document completely.
    * Filters may use this method to report other, non-XML warnings as well.
    * Parameters:
    * exception - The warning information encapsulated in a SAX parse exception.
    * @throws org.xml.sax.SAXException
    */
   public void warning(org.xml.sax.SAXParseException sAXParseException) throws SAXException
   {
          //System.out.println(sAXParseException.toString());

   }

   /**
    * Receive notification of the beginning of a document.     * @return void
    * @exception SAXException
    * The SAX parser will invoke this method only once, before any other methods in this interface or in DTDHandler (except for setDocumentLocator).
    * @throws org.xml.sax.SAXException
    */
   public void startDocument() throws SAXException
   {
      //System.out.println("*****startdocument*********");
   }

   /**
    * Receive notification of character data.     * @param values
    * @param param
    * @param param2
    * @return void
    * @exception SAXException
    * The Parser will call this method to report each chunk of character data. SAX parsers may return all contiguous character data in a single chunk, or they may split it into several chunks; however, all of the characters in any single event must come from the same external entity so that the Locator provides useful information.
    * The application must not attempt to read from the array outside of the specified range.
    * Note that some parsers will report whitespace in element content using the ignorableWhitespace method rather than this one (validating parsers must do so).
    * Parameters:
    * ch - The characters from the XML document.
    * start - The start position in the array.
    * length - The number of characters to read from the array.
    */
   public void characters(char[] values, int param, int param2) throws SAXException
   {

      //System.out.println("this binding name " + this.bindingName + " : " + new String(values, param, param2) );
      String value = new String(values, param, param2);

      this.bindingName = this.bindingName.trim();



      if(this.bindingName.equalsIgnoreCase(PrologHandler.LOGIC_EXP)){
          this.bindings.put(this.bindingName, value);
      }else if(this.bindingName.equalsIgnoreCase(PrologHandler.PARSE_TR)){
          this.bindings.put(this.bindingName, value);
      }else if(this.bindingName.equalsIgnoreCase(PrologHandler.ACTION)){
          this.bindings.put(this.bindingName, value);
      }else if(this.bindingName.equalsIgnoreCase(PrologHandler.WARNING)){
          this.bindings.put(this.bindingName, value);
      }

   }

   /**
    * Receive notification of ignorable whitespace in element content.     * @param values
    * @param param
    * @param param2
    * @return void
    * @exception SAXException
    * Validating Parsers must use this method to report each chunk of whitespace in element content (see the W3C XML 1.0 recommendation, section 2.10): non-validating parsers may also use this method if they are capable of parsing and using content models.
    * SAX parsers may return all contiguous whitespace in a single chunk, or they may split it into several chunks; however, all of the characters in any single event must come from the same external entity, so that the Locator provides useful information.
    * The application must not attempt to read from the array outside of the specified range.
    * Parameters:
    * ch - The characters from the XML document.
    * start - The start position in the array.
    * length - The number of characters to read from the array.
    * @throws org.xml.sax.SAXException
    */
   public void ignorableWhitespace(char[] values, int param, int param2) throws SAXException
   {


   }

   /**
    * Receive notification of a processing instruction.     * @param str
    * @param str1
    * @return void
    * @exception SAXException
    * The Parser will invoke this method once for each processing instruction found: note that processing instructions may occur before or after the main document element.
    * A SAX parser must never report an XML declaration (XML 1.0, section 2.8) or a text declaration (XML 1.0, section 4.3.1) using this method.
    * Parameters:
    * target - The processing instruction target.
    * data - The processing instruction data, or null if none was supplied. The data does not include any whitespace separating it from the target.
    */
   public void processingInstruction(java.lang.String str, java.lang.String str1) throws SAXException
   {



   }

   /**
    * Begin the scope of a prefix-URI Namespace mapping.     * @param str
    * @param str1
    * @return void
    * @exception SAXException
    * The information from this event is not necessary for normal Namespace processing: the SAX XML reader will automatically replace prefixes for element and attribute names when the http://xml.org/sax/features/namespaces feature is true (the default).
    * There are cases, however, when applications need to use prefixes in character data or in attribute values, where they cannot safely be expanded automatically; the start/endPrefixMapping event supplies the information to the application to expand prefixes in those contexts itself, if necessary.
    * Note that start/endPrefixMapping events are not guaranteed to be properly nested relative to each-other: all startPrefixMapping events will occur before the corresponding startElement event, and all endPrefixMapping events will occur after the corresponding endElement event, but their order is not otherwise guaranteed.
    * There should never be start/endPrefixMapping events for the "xml" prefix, since it is predeclared and immutable.
    * Parameters:
    * prefix - The Namespace prefix being declared.
    * uri - The Namespace URI the prefix is mapped to.
    * @throws org.xml.sax.SAXException
    * @roseuid 3CA1518802A7
    */
   public void startPrefixMapping(java.lang.String str, java.lang.String str1) throws SAXException
   {
   }

   /**
    * Receive notification of the beginning of a document.     * @return void
    * @exception SAXException
    * The SAX parser will invoke this method only once, before any other methods in this interface or in DTDHandler (except for setDocumentLocator).
    * @throws org.xml.sax.SAXException
    * @roseuid 3CA151890082
    */
   public void endDocument() throws SAXException
   {

        //System.out.println("***end document****");
   }

   /**
    * Receive notification of a skipped entity.     * @param str
    * @return void
    * @exception SAXException
    * The Parser will invoke this method once for each entity skipped. Non-validating processors may skip entities if they have not seen the declarations (because, for example, the entity was declared in an external DTD subset). All processors may skip external entities, depending on the values of the http://xml.org/sax/features/external-general-entities and the http://xml.org/sax/features/external-parameter-entities properties.
    * Parameters:
    * name - The name of the skipped entity. If it is a parameter entity, the name will begin with '%', and if it is the external DTD subset, it will be the string "[dtd]".
    */
   public void skippedEntity(java.lang.String str) throws SAXException
   {
   }

   /**
    * Receive notification of a recoverable error.     * @param sAXParseException
    * @return void
    * @exception SAXException
    * This corresponds to the definition of "error" in section 1.2 of the W3C XML 1.0 Recommendation. For example, a validating parser would use this callback to report the violation of a validity constraint. The default behaviour is to take no action.
    * The SAX parser must continue to provide normal parsing events after invoking this method: it should still be possible for the application to process the document through to the end. If the application cannot do so, then the parser should report a fatal error even if the XML 1.0 recommendation does not require it to do so.
    * Filters may use this method to report other, non-XML errors as well.
    * Parameters:
    * exception - The error information encapsulated in a SAX parse exception.
    * @throws org.xml.sax.SAXException
    */
   public void error(org.xml.sax.SAXParseException sAXParseException) throws SAXException
   {

           // System.out.println(sAXParseException.toString());

   }

   /**
    * Receive an object for locating the origin of SAX document events.     * @param locator
    * @return void
    * @exception
    * SAX parsers are strongly encouraged (though not absolutely required) to supply a locator: if it does so, it must supply the locator to the application by invoking this method before invoking any of the other methods in the ContentHandler interface.
    * The locator allows the application to determine the end position of any document-related event, even if the parser is not reporting an error. Typically, the application will use this information for reporting its own errors (such as character content that does not match an application's business rules). The information returned by the locator is probably not sufficient for use with a search engine.
    * Note that the locator will return correct information only during the invocation of the events in this interface. The application should not attempt to use it at any other time.
    * Parameters:
    * locator - An object that can return the location of any SAX document event.
    */
   public void setDocumentLocator(org.xml.sax.Locator locator)
   {
   }

   /**
    * Receive notification of a non-recoverable error.     * @param sAXParseException
    * @return void
    * @exception SAXException
    * This corresponds to the definition of "fatal error" in section 1.2 of the W3C XML 1.0 Recommendation. For example, a parser would use this callback to report the violation of a well-formedness constraint.
    * The application must assume that the document is unusable after the parser has invoked this method, and should continue (if at all) only for the sake of collecting addition error messages: in fact, SAX parsers are free to stop reporting any other events once this method has been invoked.
    * Parameters:
    * exception - The error information encapsulated in a SAX parse exception.
    * @throws org.xml.sax.SAXException
    */
   public void fatalError(org.xml.sax.SAXParseException sAXParseException) throws SAXException
   {

          System.out.println(sAXParseException.toString());
   }

   /**
    * End the scope of a prefix-URI mapping.     * @param str
    * @return void
    * @exception SAXException
    * See startPrefixMapping for details. This event will always occur after the corresponding endElement event, but the order of endPrefixMapping events is not otherwise guaranteed.
    * Parameters:
    * prefix - The prefix that was being mapping.
    * @throws org.xml.sax.SAXException
    */
   public void endPrefixMapping(java.lang.String str) throws SAXException
   {

         // System.out.println("in end prefix " + str);

   }

   /**
    * Receive notification of the beginning of an element.     * @param str
    * @param str1
    * @param str2
    * @param attributes
    * @return void
    * @exception SAXException
    * The Parser will invoke this method at the beginning of every element in the XML document; there will be a corresponding endElement event for every startElement event (even when the element is empty). All of the element's content will be reported, in order, before the corresponding endElement event.
    * This event allows up to three name components for each element:
    * the Namespace URI;
    * the local name; and
    * the qualified (prefixed) name.
    * Any or all of these may be provided, depending on the values of the http://xml.org/sax/features/namespaces and the http://xml.org/sax/features/namespace-prefixes properties:
    * the Namespace URI and local name are required when the namespaces property is true (the default), and are optional when the namespaces property is false (if one is specified, both must be);
    * the qualified name is required when the namespace-prefixes property is true, and is optional when the namespace-prefixes property is false (the default).
    * Note that the attribute list provided will contain only attributes with explicit values (specified or defaulted): #IMPLIED attributes will be omitted. The attribute list will contain attributes used for Namespace declarations (xmln
    */
   public void startElement(java.lang.String str, java.lang.String str1, java.lang.String str2, org.xml.sax.Attributes attributes) throws SAXException
   {

        //this.bindingName = str1;
        //System.out.println("Argumrnt value : " + attributes.getValue(0));
         this.bindingName = attributes.getValue(0);

   }

   /**
    * Receive notification of the end of an element.     * @param str
    * @param str1
    * @param str2
    * @return void
    * @exception SAXException
    * The SAX parser will invoke this method at the end of every element in the XML document; there will be a corresponding startElement event for every endElement event (even when the element is empty).
    * For information on the names, see startElement.
    * Parameters:
    * uri - The Namespace URI, or the empty string if the element has no Namespace URI or if Namespace processing is not being performed.
    * localName - The local name (without prefix), or the empty string if Namespace processing is not being performed.
    * qName - The qualified XML 1.0 name (with prefix), or the empty string if qualified names are not available.
    * @throws org.xml.sax.SAXException
    */
   public void endElement(java.lang.String str, java.lang.String str1, java.lang.String str2) throws SAXException
   {

        this.bindingName = "";
        //System.out.println("end element " + str + ", " + str1 + ", " + str2 );

   }
}