/*This code is copyrighted by Teknowledge (c) 2001.  It is released under
the GNU Public License <http://www.gnu.org/copyleft/gpl.html>.  Users of
this code also consent, by use of this code, to credit Teknowledge, in any writings, briefings,
publications, presentations, or other representations of any software which
incorporates, builds on, or uses this code.*/
package com.tks.celt;

import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import java.util.HashMap;
import java.util.Iterator;

import org.apache.xerces.parsers.SAXParser;
import org.xml.sax.XMLReader;
import org.xml.sax.InputSource;
import org.jdom.Document;
import org.jdom.input.SAXBuilder;


/**
 * <p>Title: ResponseProcessor</p>
 * <p>Description: This class uses the SAXParser to parse given input. Saxparser parses given input and
 * populates the bindings Hashmap. </p>
 * <p>Copyright: Teknowledge Corp Copyright (c) 2002</p>
 * <p>Company: Teknowledge Corporation</p>
 * @author bvasired
 * @version 1.0
 */

public class ResponseProcessor{

   /**
    * holds the content produced by SAXParser
    */

     private HashMap bindings;


    /**
    * holds the Document produced by SAXParser
    */

     private Document document;

   /**
    * The empty constructor for ResponseProcessor class
    */
    public ResponseProcessor() {

        this.bindings = new HashMap();

    }


    /**
        Process the XML response returned via the specified inputStream
        @param InputStream fromServer
    */

    public void processResponse(InputStream fromServer){

        //System.out.println("in process response");
        XMLReader parser = new SAXParser();
        QueryHandler handler = new QueryHandler(this.bindings);

        try{

            parser.setFeature("http://xml.org/sax/features/validation", false);
            parser.setFeature("http://apache.org/xml/features/validation/warn-on-undeclared-elemdef",false);
            parser.setContentHandler(handler);
            parser.setErrorHandler(handler);
            //System.out.println("before parsing");
            parser.parse(new InputSource(fromServer));

            //System.out.println("after parsing");


        }catch(Exception e){
            System.out.println(e);
            this.error(e);
        }


    }

    /**
        Process the XML response returned via the specified server string
        @param fromServer
    */

     public void processResponse(Reader fromServer){

        XMLReader parser = new SAXParser();
        QueryHandler handler = new QueryHandler(this.bindings);

        try{

            parser.setFeature("http://xml.org/sax/features/validation", false);
            parser.setFeature("http://apache.org/xml/features/validation/warn-on-undeclared-elemdef",false);
            parser.setContentHandler(handler);
            parser.setErrorHandler(handler);
            parser.parse(new InputSource(fromServer));


        }catch(Exception e){
            System.out.println(e);
            this.error(e);
        }

     }

         /**
        Process the XML response returned via the specified server string
        @param fromServer
    */

     public void processResponse(String fromServer){



        try{

            SAXBuilder builder = new SAXBuilder();
            builder.setFeature("http://xml.org/sax/features/validation", false);
            //builder.setFeature("http://apache.org/xml/features/validation/warn-on-undeclared-elemdef",false);
            this.document = builder.build(new StringReader(fromServer));
            //System.out.println("in  response processor, document " + this.document.toString());

        }catch(Exception e){
            System.out.println(e);
            this.error(e);
        }

     }

   /**
    * This method generates the errors while processing the given input
    * @param exception exception
    */
    public void error(java.lang.Exception exception){

    }

   /**
    * returns Iterator contains all available bindings.
    */
    public Iterator enumerateBindings(){

      return this.bindings.keySet().iterator();
    }

   /**
    * returns value for given binding
    */

    public Object getBindingValue(String passedBinding){

      return this.bindings.get(passedBinding);
    }

   /**
    * returs Map of all bindings
    */

    public HashMap getBindings(){

      return this.bindings;
    }


   /**
    * returs Document
    */

    public Document getDocument(){

      return this.document;
    }

}