/*	wsdlcpp.java

The contents of this file are subject to the MPL 1.1 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
http://www.cs.fsu.edu/~engelen/wsdllicense.html
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
for the specific language governing rights and limitations under the License.

The Initial Developers of the Original Code are Kiran Kaja and Robert A. van Engelen.
Copyright (C) 2000-2002 Kiran Kaja and Robert A. van Engelen. All Rights Reserved.

*/

import java.io.IOException;
import java.util.*;
import java.io.*;

import org.xml.sax.SAXException;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentType;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

// Import your vendor's DOM parser
import org.apache.xerces.parsers.DOMParser;

public class wsdlcpp
{
    Hashtable tobeProcessedArrays = new Hashtable();
    
    Hashtable processedArrays = new Hashtable();

    Hashtable structureNodes = new Hashtable();

    Hashtable bindingsList = new Hashtable();

    Hashtable soapActions = new Hashtable();
    
    Hashtable nameSpaces = new Hashtable();
   /*sets the name spaces of the entire documents it looks only into the
     * definitions node*/
    String docNS=null;
    String definitionNS=null;
    String soapNS=null;
    String defaultNS=null;
    
    Hashtable allPrimitiveDataType = new Hashtable();
    Hashtable allPrimitiveDataTypeEquivalent = new Hashtable();

    boolean defWsdl=false;
    boolean defSoap=false;

    public void setNameSpaces(Node node)//get the doucument node
    {
	String wsdlNS=null;
	String xsdNS=null;
	String encodingNS=null;
	
	NodeSearch defSearch = new NodeSearch(node,"definitions",new Hashtable());

	Node defNode = defSearch.getNextPartialMatch();

	String tns=null;
	String defans=null;

	if(defNode==null)
	    {
		System.out.println("NO Definitions node found");
		return;
	    }

	String defname = defNode.getNodeName();

	docNS = defname.substring(0,defname.indexOf("definitions"));
	
	NamedNodeMap attributes = defNode.getAttributes();
	for (int i=0; i<attributes.getLength(); i++)
	    {
		Node current = attributes.item(i);

		if(current.getNodeName().compareToIgnoreCase("targetNamespace")!=0)
		    nameSpaces.put(current.getNodeValue(),current.getNodeName());
		else
		    {
			tns =  current.getNodeValue();
			continue;
		    }

		if(current.getNodeName().compareToIgnoreCase("xmlns")==0)
		    {
			defans =  current.getNodeValue();
			if(defans.endsWith("/wsdl/"))
			    {
				defWsdl = true;
			    }
			if(defans.endsWith("/soap/"))
			    {
				defSoap = true;
			    }
			continue;
		    }
		
		if(current.getNodeValue().endsWith("/wsdl/"))
		    {
			StringTokenizer st = new StringTokenizer(current.getNodeName(),":");
			if(st.countTokens()==2)
			    {
				st.nextToken();
				wsdlNS =new String(st.nextToken()+":");
			    }
		    }
		
		if(current.getNodeValue().endsWith("/soap/"))
		    {
			StringTokenizer st = new
			    StringTokenizer(current.getNodeName(),":");
			if(st.countTokens()==2)
			    {
				st.nextToken();
				soapNS =new String(st.nextToken()+":");
			    }
			else
			    {
				//System.out.println("Found soap name space but name has more than 2 elements on tokenizer :");
				
			    }
			  
		    }
		
		if(current.getNodeValue().endsWith("/soap/encoding/"))
		    {
			StringTokenizer st = new
			    StringTokenizer(current.getNodeName(),":");
			if(st.countTokens()==2)
			    {
				st.nextToken();
				encodingNS =new String(st.nextToken()+":");
			    }
			else
			    {
				//System.out.println("Found encoding name space but name has more than 2 elements on tokenizer :");
				
			    }
		    }
		
		if(current.getNodeValue().endsWith("/XMLSchema"))
		    {
			StringTokenizer st = new
			    StringTokenizer(current.getNodeName(),":");
			if(st.countTokens()==2)
			    {
				st.nextToken();
				xsdNS =new String(st.nextToken()+":");
			    }
			else
			    {
				//System.out.println("Found xsd name space but name has more than 2 elements on tokenizer :");
				
			    }
		    }

	    }

	
	if(tns!=null)
	    {
		for (int i=0; i<attributes.getLength(); i++)
		    {
			Node current = attributes.item(i);
			if(current.getNodeName().compareToIgnoreCase("targetNamespace")==0)
			    continue;
			
			if(current.getNodeValue().equals(tns))
			    {
				StringTokenizer st = new
				    StringTokenizer(current.getNodeName(),":");
				if(st.countTokens()==2)
				    {
					st.nextToken();
					definitionNS =new String(st.nextToken()+":");
				    }
				else
				    {
					//System.out.println("Found default name space but name has more than 2 elements on tokenizer :");
					
				    }
			    }
			
		    }
	    }
	else
	    {
		System.out.println("TNS not found");
	    }
	 

	if(defans!=null)
	    {


		for (int i=0; i<attributes.getLength(); i++)
		    {
			Node current = attributes.item(i);
			if(current.getNodeName().compareToIgnoreCase("targetNamespace")==0)
			    continue;
			if(current.getNodeName().compareToIgnoreCase("xmlns")==0)
			    continue;
			
			if(current.getNodeValue().equals(defans))
			    {
				StringTokenizer st = new
				    StringTokenizer(current.getNodeName(),":");
				if(st.countTokens()==2)
				    {
					st.nextToken();
					defaultNS =new String(st.nextToken()+":");
					
				    }
				else
				    {
					//System.out.println("Found default name space but name has more than 2 elements on tokenizer :");
					
				    }
			    }
		    }
	    }
    }




    

    /*call the parsing process by creating hte document object */
    public void myParser(String uri)
    {
	
	System.out.println("/*Parsing XML File: " + uri + "*/");       
        
        // Instantiate your vendor's DOM parser implementation
        DOMParser parser = new DOMParser(); 
        try
	    {
		parser.parse(uri);
		
		Document doc = parser.getDocument();           
		
		parseMyDoc(doc);
		/*write .h file*/
		String outfilename = uri.substring(0,uri.indexOf('.'))+".h";
		
		FileWriter fw = new FileWriter(outfilename);
		PrintWriter pw = new PrintWriter(fw);

		System.out.println("Processing Done.\n\t\t Header File\t\t"+outfilename);

		NsNodeSearch ndefinition = new NsNodeSearch(doc,docNS+"definitions",new Hashtable());
		NsNode ndefinitionNode = ndefinition.getNextNode();
		
		/*System.out.println(soapActions);*/

		Hashtable namespaces = ndefinitionNode.getNameSpace();

		Enumeration keyList;
		String key;
		String twsdlns = getNsEquivalent(namespaces,"wsdlNS");
		String txsdns = getNsEquivalent(namespaces,"xsdNS");
		String tsoapns = getNsEquivalent(namespaces,"soapNS");
		String defaultns = getNsEquivalent(namespaces,"definitionNS");
		
		String name;


		keyList = bindingsList.keys();
		while(keyList.hasMoreElements())
		    {
			key = (String)keyList.nextElement();

			pw.println("//gsoap "+(String)bindingsList.get(key)+" schema namespace: "+ key);
		    }

		keyList = namespaces.keys();
		while(keyList.hasMoreElements())
		    {
			key = (String)keyList.nextElement();
			name = getName(key)+":";
			
			if(key.startsWith("xmlns:"))
			    {
				if(!(name.equals(twsdlns)||name.equals(txsdns)||name.equals(tsoapns)||name.equals(defaultns)))
				    {
					pw.println("//gsoap "+ getName(key)+" schema namespace: "+(String)namespaces.get(key));
				    }
			    }
		    }

		pw.println("");

		if(defaultns!=null)
		    {
			defaultns = defaultns.substring(0,defaultns.length()-1);
			pw.println("//gsoap "+ defaultns + " service namespace: "+namespaces.get("xmlns:"+defaultns));
		    }
		pw.println("");

		pw.println("//gsoap "+defaultns+" service location: "+serviceAddrLocation);
		pw.println("//gsoap "+defaultns+" service name: "+"soap"+serviceName);

		pw.println("\n/*start primitive data types*/");

		keyList = allPrimitiveDataType.keys();
		
		while(keyList.hasMoreElements())
		    {
			key=(String)keyList.nextElement();

			if(((Boolean)allPrimitiveDataType.get(key)).booleanValue())
			    pw.print(allPrimitiveDataTypeEquivalent.get(key));

		    }
			    /*System.out.println(allPrimitiveDataType);*/

		pw.println("\n/*end primitive data types*/\n");
		
		keyList = allDataType.keys();
		
		while(keyList.hasMoreElements())
		    {
			pw.println( (String)allDataType.get(keyList.nextElement()));
		    }
		
		keyList = allOperation.keys();
		
		while(keyList.hasMoreElements())
		    {
			key = (String)keyList.nextElement();
			
			pw.println("//soapAction : "+(String)soapActions.get(key));
			    
			pw.println( (String)allOperation.get(key));
		    }
		pw.close();



		/*write .c file*/
		outfilename = uri.substring(0,uri.indexOf('.'))+".c";
		
		fw = new FileWriter(outfilename);
		pw = new PrintWriter(fw);
		System.out.println("\t\t Sample C File\t\t"+outfilename);
		pw.println("#include \"soapH.h\"");
		pw.println("#include \""+"soap"+serviceName+".nsmap\"");
		pw.println("main()");
		pw.println("{\n\tstruct soap soap;");
		pw.println("\tsoap_init(&soap);");
		pw.println("\n");
		keyList = allOperation.keys();
		String description="";
		String functionName="";
		String parameters="";
		int index1=0,index2=0;
		String saction="";
		
		while(keyList.hasMoreElements())
		    {
			key = (String)keyList.nextElement();
			description = (String)allOperation.get(key);

			saction = (String)soapActions.get(key);

			index1 = description.indexOf('(');
			index2= description.lastIndexOf(')');
			functionName = description.substring(0,index1).trim();
			parameters = description.substring(index1+1,index2).trim();

			pw.print("\tif (soap_call_"+functionName+" ( &soap, \""+serviceAddrLocation+"\", \"");
			pw.println(saction+"\",/* "+parameters+"*/))");
			pw.println("\t\tsoap_print_fault(&soap,stderr);\n\n");
			
		    }
		pw.println("}");
		pw.close();
						
	    } 
	// catch the excepetion
	catch (IOException e) {
            System.out.println("Error reading URI: " + e.getMessage());
        } catch (SAXException e) {
            System.out.println("Error in parsing: " + e.getMessage());
        }
    }
    

    /*convert the datatype ie convert : to __*/
    private String dataType(String str)
    {
	if (str==null){System.out.println("str null");System.exit(0);}

	StringBuffer finalString = new StringBuffer();
	
	StringTokenizer colon = new StringTokenizer(str,":",true);
	String temp;


	while(colon.hasMoreElements())
	    {
		temp= (String)colon.nextElement();
		if(temp.equals(":"))
		    finalString.append("__");
		else
		    finalString.append(convertToCpp(temp,false));
	    }


	String uscores="";
	if(keyWords.contains(finalString.toString()))
	    uscores="_";
	return finalString.toString()+uscores;
	
	//return finalString.toString();
    }


    /*convert the string to print format _ , . and __ */

    private String convertToCpp(String str,boolean uscoreflag)
    {
	StringBuffer finalString = new StringBuffer();
	//replace  all '_' with _USCORE_ this should be done first as all
	// other replacements will add'_' to the string

	StringTokenizer uscore = new StringTokenizer(str,"_",true);
	String temp;
	//	finalString.append((String)uscore.nextElement());
	while(uscore.hasMoreElements())
	    {
		temp = (String)uscore.nextElement();
		if(temp.equals("_"))
		    finalString.append("_USCORE_");
		else
		    finalString.append(temp);
	    }

	//replace all '.' with '_DOT_'
	StringTokenizer dot = new StringTokenizer(finalString.toString(),".",true);
	finalString = new StringBuffer();

	//finalString.append((String)dot.nextElement());
	while(dot.hasMoreElements())
	    {
		temp = (String)dot.nextElement();
		if(temp.equals("."))
		    finalString.append("_DOT_");
		else
		    finalString.append(temp);
	    }

	//replace all '-' with '_'
	StringTokenizer hyphen = new StringTokenizer(finalString.toString(),"-",true);
	finalString = new StringBuffer();


	//finalString.append((String)hyphen.nextElement());
	while(hyphen.hasMoreElements())
	    {
		temp = (String)hyphen.nextElement();
		if(temp.equals("_"))
		    finalString.append("_");
		else
		    finalString.append(temp);
	    }

	String uscores="";
	if(keyWords.contains(finalString.toString())&&uscoreflag)
	    uscores="_";
	return finalString.toString()+uscores;
    }
    
    /*get the value of attribute of the given node*/

    public String getAttrValue(Node node, String attribute)
    {
	NamedNodeMap attributes = node.getAttributes();
	for (int i=0; i<attributes.getLength(); i++)
	    {
		Node current = attributes.item(i);
		if(current.getNodeName().compareToIgnoreCase(attribute)==0)
		    {
			return current.getNodeValue();
		    }
		}
	return null;
    }
    

private String serviceName=null;
private String serviceAddrLocation=null;
private String servicePortName=null;
private String servicePortBinding=null;



    public boolean getServiceInfo(Node node)
    {
	Node serviceNode=null;
	Node portNode=null;
	Node soapAddrNode=null;
	/*
	String docNS = (String) namespace.get("docNS");
	String soapNS = (String) namespace.get("soapNS");
	*/
	NodeSearch service = new NodeSearch(node,docNS+"service");

	serviceNode = service.getNextNode();
	if(serviceNode==null)
	    {
		if(defWsdl)
		    service = new NodeSearch(node,"service");
		if(serviceNode==null){
		System.out.println("Service element not found");
		return false;
		}
	    }
	
	serviceName = getAttrValue(serviceNode,"name");
	if(serviceName == null)
	    {
		System.out.println("name attribute of service not found");
		return false;
	    }

	portNode = (new NodeSearch(serviceNode,docNS+"port")).getNextNode();
	if(portNode == null)
	    {
		if(defWsdl)
		    portNode = (new NodeSearch(serviceNode,"port")).getNextNode();
		if(portNode == null)
		    {
		System.out.println("element PORT not found");
		return false;}
	    }

	servicePortName = getAttrValue(portNode,"name");
	if(servicePortName == null)
	    {
		System.out.println("name attribute of PORT not found");
		return false;
	    }

	servicePortBinding = getAttrValue(portNode,"binding");
	if(servicePortBinding == null)
	    {
		System.out.println("Binding attribute of PORT not found");
		return false;
	    }

	soapAddrNode = (new NodeSearch(serviceNode,soapNS+"address")).getNextNode();
	if(soapAddrNode == null)
	    {
		if(defSoap)
		soapAddrNode = (new
		    NodeSearch(serviceNode,"address")).getNextNode();
		if(soapAddrNode == null)
		    {
			System.out.println("element SOAP ADDRRESS not found");
		return false;}
	    }

	serviceAddrLocation = getAttrValue(soapAddrNode,"location");
	if(serviceAddrLocation == null)
	    {
		System.out.println("Location attribute of SOAP ADDRESS not found");
		return false;
	    }
	    return true;
    }

private String operationName=null;
private String inputName=null;
private String outputName=null;

public boolean getBindingInfo(Node node, String bindingName)
    {
	Node bindingNode=null;
	Node operationNode=null;
	Node inputNode=null;
	Node outputNode=null;

	Hashtable attrib = new Hashtable();

	attrib.put("name",bindingName);
	NodeSearch binding = new NodeSearch(node,docNS+"binding",attrib);
	
	bindingNode = binding.getNextNode();
	
	if(bindingNode == null)
	    {
		if(defWsdl)
		    binding = new NodeSearch(node,"binding",attrib);
		if(bindingNode == null){
		System.out.println("element BINDING not found");
		return false;}
	    }

	String bindingType = getAttrValue(bindingNode,"type");

	bindingType = bindingType.substring(bindingType.indexOf(":")+1);
	

	NodeSearch operation = new NodeSearch(bindingNode,docNS+"operation");

	/*	System.out.println("*********************Operation Information*********************************");*/
	operationNode = operation.getNextNode();
	if(operationNode == null)
	    {
		if(defWsdl){
		    operation = new NodeSearch(bindingNode,"operation");
		    operationNode = operation.getNextNode();}
		
		if(operationNode == null){
		System.out.println("element OPERATION not found");
		return false;}
	    }
	
	while(operationNode!=null)
	    {
		operationName = getAttrValue(operationNode,"name");
		if(operationName == null)
		    {
			System.out.println("name attribute of OPERATION not found");
			return false;
		    }

		NodeSearch soapoperation = new NodeSearch(operationNode,soapNS+"operation");
		Node soapOperationNode = soapoperation.getNextNode();

		if((soapOperationNode==null)&&(defSoap))
		    {
			soapoperation = new NodeSearch(operationNode,"operation");
			soapOperationNode = soapoperation.getNextNode();
		    }

		
		String soapact="";
		if(soapOperationNode!=null)
		    {
			soapact = getAttrValue(soapOperationNode,"soapAction");
			//System.out.println("soap action "+getAttrValue(soapOperationNode,"soapAction"));
		    }

		NodeSearch soapbody = new NodeSearch(bindingNode,soapNS+"body");
		Node soapBodyNode = soapbody.getNextNode();

		if((soapBodyNode==null)&&(defSoap))
		    {
			soapbody = new NodeSearch(bindingNode,"body");
			soapBodyNode = soapbody.getNextNode();
		    }

		String tdefns = definitionNS.substring(0,definitionNS.length()-1);
		if(soapBodyNode!=null)
		    {
			String tnsp = getAttrValue(soapBodyNode,"namespace");

			if(tnsp!=null){
			if(bindingsList.containsKey(tnsp))
			    tdefns=(String)bindingsList.get(tnsp);
			else
			    {
				if(!bindingsList.containsValue(tdefns))
				    {
					bindingsList.put(tnsp,tdefns);
				    }
				else
				    {
					int cnt=1;
					while(bindingsList.containsValue(tdefns+cnt))cnt++;
					bindingsList.put(tnsp,tdefns+cnt);
					tdefns+=cnt;
				    }
			    }
			}
		    }
		
		printOperationInfo(node,bindingType,operationName,tdefns+":",soapact);
		
		operationNode = operation.getNextNode();
	    }
	
	/*System.out.println("*****************End Operation Information*********************************");*/
	return true;
    }

    public void printOperationInfo(Node node,String bindingType,String
				   operationName,String defns,String soapact)
    {
	Hashtable bindattrib = new Hashtable();
	Hashtable operattrib = new Hashtable();

	bindattrib.put("name",bindingType);

	operattrib.put("name",operationName);
	
	NodeSearch operations = new NodeSearch(new NodeSearch(node,docNS+"portType",bindattrib).getNextNode(),
					       docNS+"operation",operattrib);

	Node operation = operations.getNextNode();

	if((operation==null)&&(defWsdl))
	    {
		operations = new NodeSearch(new NodeSearch(node,"portType",bindattrib).getNextNode(),
					       "operation",operattrib);

		operation = operations.getNextNode();
	    }
	
	while(operation!=null)
	    {
		if(operation!=null)
		    {
			//System.out.println("operation found "+operationName);
		    }
		else
		    {
			System.out.println("operation NOT found "+operationName);
			return;
		    }

		elementflag = false;
		
		NodeSearch input = new NodeSearch(operation,docNS+"input");
		Node inputNode = input.getNextNode();


		if((inputNode==null)&&(defWsdl))
		    {
			input = new NodeSearch(operation,"input");
			inputNode = input.getNextNode();			
		    }

		
		StringBuffer inputArgs = new StringBuffer();
		StringBuffer outArgs = new StringBuffer();

		
		String messageEquivalent=null;
		for(int i=0;i<input.getTotalMatches();i++)
		    {
			if(inputNode==null)
			    {
				System.out.println("INput node for operation " +
						   convertToCpp(operationName,true)+" Not found");
				return;
			    }
			
			
			String inmessage = getAttrValue(inputNode,"message");
			inmessage = inmessage.substring(inmessage.indexOf(":")+1);

			messageEquivalent = getMessageEquivalent(node,inmessage,false);
			
			//System.out.println("operation "+ operationName+" IN "+ messageEquivalent);
			
			inputArgs.append(messageEquivalent+", ");
			
			inputNode = input.getNextNode(); 
		    }
		
		NodeSearch output = new NodeSearch(operation,docNS+"output");
		Node outputNode = output.getNextNode();
		if((outputNode==null)&&(defWsdl))
		    {
			output = new NodeSearch(operation,"output");
			outputNode = output.getNextNode();
		    }
		
		for(int i=0;i<output.getTotalMatches();i++)
		    {
			
			if(outputNode==null)
			    {
				System.out.println("outputput node for operation " +
						   convertToCpp(operationName,true)+" Not found");
				return;
			    }
			
			String outmessage = getAttrValue(outputNode,"message");
			outmessage = outmessage.substring(outmessage.indexOf(":")+1);
			
			messageEquivalent = getMessageEquivalent(node,outmessage,true);
			//System.out.println(" OUT "+ messageEquivalent);
			
			outArgs.append(messageEquivalent.replace(',',';'));
			
			outputNode = output.getNextNode();
			
		    }

		/*
		String description =  " ( "+inputArgs.substring(0,inputArgs.length()-2)+
		                     " struct "+dataType(definitionNS+operationName)+
		                     "Response {"+outArgs.substring(0,outArgs.length())+" } *out) ;";
		*/

		String description="";
		if(!elementflag){
		    description=  " ( "+inputArgs.substring(0,inputArgs.length()-2)+
		                     " struct "+dataType(defns+operationName)+
		                     "Response {"+outArgs.substring(0,outArgs.length())+" } *out) ;";
		    addOperation(defns,operationName,description,soapact);
		}
		else{
		    description=  " ( "+inputArgs.substring(0,inputArgs.length()-2)+
		                     " struct "+dataType(defns+operationName)+
		                     "_Response {"+outArgs.substring(0,outArgs.length())+" } *out) ;";
		
		addOperation(defns,operationName+"_",description,soapact);
		}
		
		
/*		System.out.println(dataType(definitionNS+operationName)+
				   " ( "+inputArgs.substring(0,inputArgs.length()-2)+
				   " struct "+dataType(definitionNS+operationName)+"Response {"+
				   outArgs.substring(0,outArgs.length())+" } *out) ;");*/

		operation = operations.getNextNode();
	    }
	
	
    }
    

    Hashtable allOperation = new Hashtable();

    void addOperation(String ldefNS,String operationName,String
		      description,String soapaction)
    {
	boolean inserted=false;
	String opString="";
	
	while(!inserted)
	    {
		if(!allOperation.containsKey(opString+operationName))
		    {
			
			allOperation.put(opString+operationName,dataType(ldefNS)+opString+dataType(operationName)+description);
			soapActions.put(opString+operationName,soapaction);
			inserted=true;
		    }
		else
		    {
			int pos;

			
			if(((String)allOperation.get(opString+operationName)).compareToIgnoreCase(dataType(ldefNS)+opString+dataType(operationName)+description)==0)
			    return;

			if((pos=description.indexOf(opString+operationName))!=-1)
			    {
				description = description.substring(0,pos)+"_"+opString+description.substring(pos+opString.length()+operationName.length());
			    }
			opString = opString+"_";
		    }
	    }
		
    }
    
   String getNsEquivalent(Hashtable ht, String name)
    {
	if(ht==null)
	    return null;
	else
	    return (String)ht.get(name);
    }

    boolean isdefWsdl(Hashtable ht)
    {
	String sdefaultNS = (String)ht.get("defaultNS");
	if(sdefaultNS!=null)
	    {
		if(sdefaultNS.endsWith("/wsdl/"))
		    {
			return true;
		    }

	    }
	return false;
    }
    boolean isdefSoap(Hashtable ht)
    {
	String sdefaultNS = (String)ht.get("defaultNS");
	if(sdefaultNS!=null)
	    {
		if(sdefaultNS.endsWith("/soap/"))
		    {
			return true;
		    }

	    }
	return false;
    }

    
    private boolean elementflag;    

String getMessageEquivalent(Node node, String messageName,boolean isreturn)
    {
	Node messageNode=null;

	StringBuffer equivalentString = new StringBuffer();
	
	Hashtable attr = new Hashtable();

	attr.put("name",messageName);
	
	NsNodeSearch nmessage = new NsNodeSearch(node,docNS+"message",attr,new Hashtable());
	NsNode nmessageNode = nmessage.getNextNode();

	if((nmessageNode==null)&&(defWsdl))
	    {
		 nmessage = new NsNodeSearch(node,"message",attr,new Hashtable());
		 nmessageNode = nmessage.getNextNode();
	    }
	elementflag = false;

	String uscore="";
	if(isreturn)
	    uscore="_";

	if(nmessageNode!=null)	
	messageNode= nmessageNode.getNode();
	else{
	    System.out.println("message of message name" +messageName+
				   "not found");
	    System.exit(0);
	}

	if(messageNode==null)
	    {
		System.out.println("message of message name" +messageName+
				   "not found");
	    }
	
	NsNodeSearch part = new NsNodeSearch(messageNode,docNS+"part",nmessageNode.getNameSpace());

	//StringBuffer messageEq = new StringBuffer();
	NsNode npartNode=part.getNextNode();

	if((npartNode==null)&&(defWsdl))
	    {
		part = new NsNodeSearch(messageNode,"part",nmessageNode.getNameSpace());
		npartNode=part.getNextNode();
	    }
	

	
	Node partNode=null;
	if(npartNode!=null)
	    partNode = npartNode.getNode();

	for(int i=0;i<part.getTotalMatches();i++)
	    {
		String partName=null;
		String partType=null;
		String partElement=null;

		partName = getAttrValue(partNode,"name");
		partType = getAttrValue(partNode,"type");
		partElement = getAttrValue(partNode,"element");/*not need in
								* soap encoding*/
		
		if(partName == null)
		    {
			System.out.println("name field for part is NOT found");
		    }
		String xsdNS = getNsEquivalent(npartNode.getNameSpace(),"xsdNS");
		
		if(partType == null)//means it is specidied in othere way (element)
		    {
			if(partElement!=null)
			    {/*
				System.out.println("type specified by element");
				partType = partElement;*/
				elementflag = true;
				String eleEq;
				if(isXsd(partElement,xsdNS))
				    {
					eleEq = dataType(new String("xsd:"+getName(partElement))+uscore+convertToCpp(partName,true)+ ", ");
					
				    }
				else
				    {
					eleEq = (String)allDataType.get(getName(partElement));
					eleEq = eleEq.substring(eleEq.indexOf('{')+1,eleEq.lastIndexOf('}'));
					eleEq = eleEq.replace(';',',');
					eleEq = eleEq.replace('\n',' ');
					//System.out.println(eleEq);

					StringTokenizer st = new StringTokenizer(eleEq," ");
					String token;
					eleEq ="";
					while(st.hasMoreTokens())
					    {
						token= st.nextToken();
						if(token.compareToIgnoreCase("struct")==0)
						    {
							eleEq+= token;
							token = st.nextToken();
							StringTokenizer lst = new StringTokenizer(token,"*");
							token = lst.nextToken();
							eleEq+= " "+token;
							token = lst.nextToken();
							eleEq+= "*_"+token;
							
							/*token = st.nextToken();
							  eleEq+= "+ _"+token;*/
						    }
						else if(token.startsWith("xsd"))
						    {
							eleEq+= token;
							token = st.nextToken();
							eleEq+= " _"+token;
						    }
						else
						    eleEq+=" "+token;
					    }
				    }
				//System.out.println("new " +eleEq);
				equivalentString.append(eleEq);
			    }
			else
			    System.out.println("Type not specified by element or type!!");
		    }
		else
		    {

			if(isXsd(partType,xsdNS))
			    partType = new String("xsd:"+getName(partType));
			else
			    partType = new String("struct "+partType);
			
			equivalentString.append(dataType(partType)+ " " +uscore+convertToCpp(partName,true)+ ", ");
		    }
		 npartNode = part.getNextNode();
		 if(npartNode!=null)
		     partNode = npartNode.getNode();
	    }
	
	//System.out.println("msg eq is "+equivalentString.toString());
	return equivalentString.toString();
    }

    boolean isXsd(String name,String currentXSDNS)
    {
	String typeName = getName(name);
	if(typeName.indexOf('[')!=-1)
	    typeName = typeName.substring(0,typeName.indexOf('['));
	
	if(currentXSDNS!=null)
	    if(getNS(name).compareToIgnoreCase(currentXSDNS)==0)
		{
		    allPrimitiveDataType.put(typeName,new Boolean(true));
		    return true;
		}
	if(getNS(name).compareToIgnoreCase("")==0)
	    {

		if(allPrimitiveDataType.containsKey(typeName))
		    {
			allPrimitiveDataType.put(typeName,new Boolean(true));
			return true;
		    }
	    }

	return false;
    }

    Vector keyWords = new Vector();

    private void intitlizePrimitiveType()
    {
        keyWords.add("auto");
	keyWords.add("bool");
	keyWords.add("break");
	keyWords.add("case");
	keyWords.add("char");
	keyWords.add("class");
	keyWords.add("const");
	keyWords.add("continue");
	keyWords.add("default");
	keyWords.add("do");
	keyWords.add("double");
	keyWords.add("else");
	keyWords.add("enum");
	keyWords.add("extern");
	keyWords.add("false");
	keyWords.add("float");
	keyWords.add("for");
	keyWords.add("goto");
	keyWords.add("if");
	keyWords.add("inline");
	keyWords.add("int");
	keyWords.add("long");
	keyWords.add("LONG64");
	keyWords.add("mustUnderstand");
	keyWords.add("namespace");
	keyWords.add("operator");
	keyWords.add("private");
	keyWords.add("protected");
	keyWords.add("public");
	keyWords.add("register");
	keyWords.add("return");
	keyWords.add("short");
	keyWords.add("signed");
	keyWords.add("sizeof");
	keyWords.add("static");
	keyWords.add("struct");
	keyWords.add("switch");
	keyWords.add("time_t");
	keyWords.add("true");
	keyWords.add("typedef");
	keyWords.add("ULONG64");
	keyWords.add("union");
	keyWords.add("unsigned");
	keyWords.add("using");
	keyWords.add("virtual");
	keyWords.add("void");
	keyWords.add("volatile");
	keyWords.add("wchar_t");
	keyWords.add("while");
	
	allPrimitiveDataType.put("anyURI",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("anyURI","typedef char * xsd__anyURI;\n");
	allPrimitiveDataType.put("base64Binary",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("base64Binary","typedef struct {\nunsigned char *__ptr;\nint __size;\n}xsd__base64Binary;\n");
	allPrimitiveDataType.put("boolean",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("boolean","typedef char * xsd__boolean;\n");
	allPrimitiveDataType.put("byte",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("byte","typedef char xsd__byte;\n");
	allPrimitiveDataType.put("dateTime",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("dateTime","typedef char * xsd__dateTime;\n");
	allPrimitiveDataType.put("date",new Boolean(false));	
	allPrimitiveDataTypeEquivalent.put("date","typedef char * xsd__date;\n");	
	allPrimitiveDataType.put("decimal",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("decimal","typedef char * xsd__decimal;\n");
	allPrimitiveDataType.put("double",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("double","typedef char * xsd__double;\n");
	allPrimitiveDataType.put("duration",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("duration","typedef char * xsd__duration;\n");
	allPrimitiveDataType.put("float",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("float","typedef float xsd__float;\n");
	allPrimitiveDataType.put("hexBinary",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("hexBinary","typedef struct {\nunsigned char *__ptr;\nint __size;\n}xsd__hexBinary;\n");
	allPrimitiveDataType.put("int",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("int","typedef int xsd__int;\n");
	allPrimitiveDataType.put("integer",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("integer","typedef char * xsd_integer;\n");
	allPrimitiveDataType.put("long",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("long","typedef LONG64 xsd__long;\n");
	allPrimitiveDataType.put("negativeInteger",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("negativeInteger","typedef char * xsd__negativeInteger;\n");
	allPrimitiveDataType.put("nonNegativeInteger",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("nonNegativeInteger","typedef char * xsd__nonNegativeInteger;\n");
	allPrimitiveDataType.put("nonPositiveInteger",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("nonPositiveInteger","typedef char * xsd__nonPositiveInteger;\n");
	allPrimitiveDataType.put("normalizedString",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("normalizedString","typedef char *  xsd__normalizedString;\n");
	allPrimitiveDataType.put("positiveInteger",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("positiveInteger","typedef char * xsd__positiveInteger;\n");
	allPrimitiveDataType.put("short",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("short","typedef char * xsd__short;\n");
	allPrimitiveDataType.put("string",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("string","typedef char * xsd__string;\n");
	allPrimitiveDataType.put("time",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("time","typedef char * xsd__time;\n");
	allPrimitiveDataType.put("token",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("token","typedef char * xsd__token;\n");
	allPrimitiveDataType.put("unsignedByte",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("unsignedByte","typedef unsigned char xsd__unsignedByte;\n");
	allPrimitiveDataType.put("unsignedInt",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("unsignedInt","typedef unsigned int xsd__unsignedInt;\n");
	allPrimitiveDataType.put("unsignedLong",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("unsignedLong","typedef unsigned LONG64 xsd__unsignedLong;\n");
	allPrimitiveDataType.put("unsignedShort",new Boolean(false));
	allPrimitiveDataTypeEquivalent.put("unsignedShort","typedef unsigned short xsd__unsignedShort;\n");
	


    }

	
    public void printTypesInformation(Node node)
    {
	NsNodeSearch types = new NsNodeSearch(node,docNS+"types",new Hashtable());

	NsNode ntypesNode = types.getNextNode();


	if(ntypesNode==null)
	    {
		if(defWsdl){
		    types = new NsNodeSearch(node,"types",new Hashtable());
		    ntypesNode = types.getNextNode();
		}
		    
		if(ntypesNode==null){
		System.out.println(" Types node not found");
		return;}
	    }

	Node typesNode = ntypesNode.getNode();
	
	NsNodeSearch schema = new NsNodeSearch(typesNode,"schema",ntypesNode.getNameSpace());
	NsNode schemaNode = schema.getNextPartialMatch();

	/*System.out.println("*********************Type Information**************************************");*/
	for(int i=0; i<schema.getTotalMatches();i++)
	    {

		Node cnode = schemaNode.getNode();
		/*find a element node just below the schema node*/
		String dataName = null;
		NodeList cnodes = cnode.getChildNodes();
		String schemaNS = cnode.getNodeName().substring(0,cnode.getNodeName().indexOf("schema"));

		boolean processed=false;
		
		for (int j=0; j<cnodes.getLength(); j++){
		    //		    System.out.println(cnodes.item(j).getNodeName());
		    if(cnodes.item(j).getNodeName().equalsIgnoreCase(schemaNS+"element"))
			{
			    dataName = getAttrValue(cnodes.item(j),"name");
			    //System.out.println("data Name set to : "+ dataName);
		
			    NsNode celementNode = new NsNode(cnodes.item(j),schemaNode.getNameSpace());
			    processSchema(celementNode,dataName,schemaNS);
			    processed=true;
			    //			    System.out.println("element procesing");
			}
		    else
			{
			    NsNode celementNode = new NsNode(cnodes.item(j),schemaNode.getNameSpace());
			    processSchema(celementNode,null,schemaNS);
			    //		    System.out.println("normall "+cnodes.getLength());
			}
		}
		/*end find a element node just below the schema node*/
		if (!processed){

		    processSchema(schemaNode,null,null);}


		schemaNode = schema.getNextPartialMatch();
	    }
	//System.out.println("processing strucutres");
		processAllStructures();
	/*System.out.println("*****************End Type Information**************************************");*/
    }

    void processSchema(NsNode nnode,String dataName,String schNS)
    {
	String localXSDNS=getNsEquivalent(nnode.getNameSpace(),"xsdNS");

	String tns=getNsEquivalent(nnode.getNameSpace(),"tns");
	String currentNS=getNsEquivalent(nnode.getNameSpace(),"definitionNS");
	String schemaNS =null;
	String localencodingNS=getNsEquivalent(nnode.getNameSpace(),"encodingNS");
	String wsdlNS = getNsEquivalent(nnode.getNameSpace(),"wsdlNS");
	
	Node node=null;
	
	if(nnode!=null)
	    node = nnode.getNode();

	if(schNS==null)
	    {
		String nodeName = node.getNodeName();
		schemaNS = nodeName.substring(0,nodeName.indexOf("schema"));
	    }
	else
	    schemaNS = schNS;

	NsNodeSearch complexType = new NsNodeSearch(node,schemaNS+"complexType",nnode.getNameSpace());

	NsNode ncomplexNode = complexType.getNextNode();
	
	Node complexNode=null;

	
	if(ncomplexNode!=null)
	    complexNode = ncomplexNode.getNode();

	Hashtable referenceNameSpace=null;


	for(int i=0;i<complexType.getTotalMatches();i++)
	    {
		String arrayType,arrayName;
		/*processing for a Array identified by restricition*/

		if((new NodeSearch(complexNode,schemaNS+"restriction").getNextNode())!=null)
		    {
		       /*System.out.println("Structure "+currentNS+ getAttrValue(complexNode,"name")+" { ");*/
			if(dataName==null)
			    arrayName = getAttrValue(complexNode,"name");
			else
			    arrayName = getName(dataName);

				
			Hashtable attributesref = new Hashtable();
			attributesref.put("ref",localencodingNS+"Array");

			
			NsNodeSearch attribute  = new NsNodeSearch(complexNode,schemaNS+"attribute",ncomplexNode.getNameSpace());
			NsNode nattributeNode = attribute.getNextNode();
			Node attributeNode=null;
			if(nattributeNode!=null)
			    {
				attributeNode = nattributeNode.getNode();
				referenceNameSpace = nattributeNode.getNameSpace();
				wsdlNS = getNsEquivalent(nattributeNode.getNameSpace(),"wsdlNS");
			    }
			
			if(attributeNode == null)
			    {
				//this case occurs in some docs where the
				//typeof array is specified using attribute
				// element qualified by a arraytype

				//waring
				
				NsNodeSearch element = new NsNodeSearch(complexNode,schemaNS+"element",nnode.getNameSpace());
				NsNode nelementNode = element.getNextNode();
				Node elementNode =null;
				
				if(nelementNode!=null)
				    {
					elementNode = nelementNode.getNode();
					referenceNameSpace = nelementNode.getNameSpace();
				    }

				if(elementNode==null)
				    {

					System.out.println("even element Node not found");
					continue;
				    }
				
				arrayType = getAttrValue(elementNode,"type");
				arrayType = arrayType+"[]";
				//assumption that the datatype is a one dimension array of the give type 
				
				//System.out.println("array type is "+arrayType);
			    }
			else
			    {
				arrayType =getAttrValue(attributeNode,wsdlNS+"arrayType");
			    }

			if(arrayType==null)
			    {
				System.out.println("No Type attribute found for element "+arrayName);
				ncomplexNode = complexType.getNextNode();
				if(ncomplexNode!=null)
				    complexNode = ncomplexNode.getNode();
				
				continue;
			    }
			
			//System.out.println("calling single for "+arrayType);		    

			String curlocalXSDNS = getNsEquivalent(referenceNameSpace,"xsdNS");


			boolean xsdflag=false;
			/*if(getNS(arrayType).compareToIgnoreCase(curlocalXSDNS)==0)*/
			if(isXsd(arrayType,curlocalXSDNS))
			    {
				xsdflag=true;
				arrayType = new String("xsd:"+getName(arrayType));
			    }
			/*			else
						arrayType = new String("struct arrayType);*/
			
			if(singleLevelArray(arrayType))
			    {
				
				//System.out.println("Structure "+currentNS+

				//getAttrValue(complexNode,"name")+" { ");
				String description;

				/*System.out.println("struct "+dataType(arrayName)+" { ");*/
				int dim = getArrayDimension(arrayType);
				
				String type = arrayType.substring(0,arrayType.indexOf('['));
				
				if(dim==1)
				    {
					/*System.out.println("struct "+dataType(type)
					  +"\t*__ptr;\nint\t__size;\nint\t__offset;\n};");*/
					if(xsdflag)
					    description =dataType(type)+"\t*__ptr;\nint\t__size;\nint\t__offset;\n};";
					else
					    description = "struct "+dataType(type)+"\t*__ptr;\nint\t__size;\nint\t__offset;\n};";
				    }
				else
				    {
					/*System.out.println(dataType(type)
							   + "\t*__ptr;\n 
int\t__size["+dim+"];\nint\t__offset["+dim+"]\n};");*/

					if(xsdflag)
					description = dataType(type)+
					       "\t*__ptr;\nint\t__size["+dim+"];\nint\t__offset["+dim+"]\n};";
					else
					    description = "struct "+dataType(type)
					     + "\t*__ptr;\nint\t__size["+dim+"];\nint\t__offset["+dim+"]\n};";
				    }
				
				addDataType("",arrayName,description);
				processedArrays.put(arrayType,arrayName);
			    }
			else
			    {
				/*add in to  TOBE process que*/
				tobeProcessedArrays.put(arrayType,arrayName+"\n"+curlocalXSDNS);
			    }
		    }
		else
		    {

			if(dataName==null)
			    dataName = new String("");
			
			structureNodes.put(ncomplexNode,dataName+"\n"+schemaNS);

			if(dataName.equals(""))
			    dataName = null;

			
		    }
		ncomplexNode = complexType.getNextNode();
		if(ncomplexNode!=null)
		    complexNode = ncomplexNode.getNode();
	    }

	
	
	/*System.out.println("to be processed que is "+tobeProcessedArrays.size());*/

	processPendingArrays();

    }


    private void processAllStructures()
    {


	Enumeration keyList;

	keyList = structureNodes.keys();
	
	while(keyList.hasMoreElements())
	    {

		NsNode ncomplexNode = (NsNode)keyList.nextElement();
		String datasch = (String)structureNodes.get(ncomplexNode);
		
		String dataName = datasch.substring(0,datasch.indexOf('\n'));


		
		String schemaNS = datasch.substring(datasch.indexOf('\n')+1);
		/*get it from hashtable ... put it along with dataname*/

		
		Node complexNode=ncomplexNode.getNode();
		
		String currentNS=getNsEquivalent(ncomplexNode.getNameSpace(),"definitionNS");
		/*		String schemaNS =null;*/
		
		Hashtable referenceNameSpace=null;
	
		String description = new String("");
		String structName =null;



		String occursString = "__size";


		
		

		
		if(dataName.equals(""))
		    structName= getAttrValue(complexNode,"name");
		else
		    structName = dataName;
		
	        //processing for a strucuture
/*		System.out.println("struct "+dataType(currentNS+ structName)+" { ");*/
		
		NsNodeSearch element = new NsNodeSearch(complexNode,schemaNS+"element",ncomplexNode.getNameSpace());
		
		NsNode nelementNode = element.getNextNode();
		
		Node elementNode =null;
		
		if(nelementNode!=null)
		    {
			elementNode =nelementNode.getNode();
			referenceNameSpace = nelementNode.getNameSpace();
		    }
		
		for(int j=0;j<element.getTotalMatches();j++)
		    {
			String typeStr = getAttrValue(elementNode,"type");
			String nameStr = getAttrValue(elementNode,"name");
			
			//System.out.println("Name space "+getNS(typeStr)+typeStr);
			String curlocalXSDNS = getNsEquivalent(referenceNameSpace,"xsdNS");
		
			String ptr=" ";
			
			boolean maxoccursFlag = false;
			
			String nillable = getAttrValue(elementNode,"nillable");

			
			if((nillable!=null)&&(nillable.compareToIgnoreCase("true")==0))
			    {
				ptr="*";
			    }
			

			if(isArrayType(typeStr))
			    {
				typeStr ="struct "+ getName(typeStr);
			    }
			else
			    {
				if(isXsd(typeStr,curlocalXSDNS))
				    typeStr = new String("xsd:"+getName(typeStr));
				else
				    {
					ptr="*";
					typeStr = new String("struct "+typeStr);
				    }
			    }


			
			
			String maxOccurs = getAttrValue(elementNode,"maxOccurs");
			if((maxOccurs!=null)&&(maxOccurs.equals("unbounded")))
			    {
				ptr = "*";
				occursString  +="_";
				maxoccursFlag = true;
			    }

			/*max occurs is unbound*/
			if(maxoccursFlag)
			    {
				description+="int\t"+occursString+";\n";
				/*System.out.println("int\t"+occursString);*/
			    }

			
			
			description+=dataType(typeStr)+ "\t"+ptr+convertToCpp(nameStr,true)+" ;\n";
		
			/*System.out.println(dataType(typeStr)+ "\t"+ptr+convertToCpp(nameStr)+" ;");*/

			
			
			nelementNode = element.getNextNode();
			
			if(nelementNode!=null)
			    {
				elementNode = nelementNode.getNode();
				referenceNameSpace = nelementNode.getNameSpace();
			    }
		    }
		
		description+=" } ;";
/*		System.out.println(" } ;");*/
		
		
		addDataType(currentNS,structName,description);
	    }
    }



    boolean isArrayType(String name)
    {
	if(name==null)
	{    System.out.println("Error occurred while parsing the file\nWSDL file may be encoded using Doc/Literal\nDoc/Literal is not completely supported in this version of WSDL reader");
	System.exit(0);
	}
	    else
	//System.out.println(name);
	if(processedArrays.containsValue(getName(name)))
	    return true;
       
	return false;

    }
    
    private void processPendingArrays()
    {
	Enumeration keyList;

	boolean someProcessed = false;
	
	keyList = tobeProcessedArrays.keys();
	
	while(keyList.hasMoreElements())
	    {
		String arrType = (String)keyList.nextElement();
		

		String temp= (String)tobeProcessedArrays.get(arrType);

		String arrName=temp.substring(0,temp.indexOf('\n'));
		String curlocalXSDNS = temp.substring(temp.indexOf('\n')+1);
		String baseType = arrType.substring(0,arrType.lastIndexOf("["));


		//System.out.println(arrType+"   "+baseType+"  "+arrName);
		
		if(processedArrays.containsKey(baseType))
		    {
			String description;
			
			someProcessed = true;

			String baseName = (String)processedArrays.get(baseType);
			
			/*System.out.println("struct "+dataType(arrName)+" { ");*/
			int dim = getArrayDimension(arrType);

			//String type = arrType.substring(0,arrType.indexOf('['));

			//System.out.println("test basa Name "+baseName);
			String type = baseName;
			if(dim==1)
			    {
				/*	System.out.println("test"+dataType(baseName)
					+ "\t*__ptr;\nint\t__size;\nint\t__offset;\n};");*/
					if(isXsd(type,curlocalXSDNS))
					    description =dataType(type)+"\t*__ptr;\nint\t__size;\nint\t__offset;\n};";
					else
					    description = "struct "+dataType(type)+"\t*__ptr;\nint\t__size;\nint\t__offset;\n};";
					/*
				description = "struct "+dataType(baseName)
				+ "\t*__ptr;\nint\t__size;\nint\t__offset;\n};";*/
			    }
			else
			    {

				if(isXsd(type,curlocalXSDNS))
				    description = dataType(type)+
					"\t*__ptr;\nint\t__size["+dim+"];\nint\t__offset["+dim+"]\n};";
				else
				    description = "struct "+dataType(type)
					+ "\t*__ptr;\nint\t__size["+dim+"];\nint\t__offset["+dim+"]\n};";
				/*System.out.println(dataType(baseName)
				  + "\t*__ptr;\nint\t__size["+dim+"];\nint\t__offset["+dim+"]\n};");*/
				/*				description = "struct "+dataType(baseName)
								+ "\t*__ptr;\nint\t__size["+dim+"];\nint\t__offset["+dim+"]\n};";*/
			    }
			
			addDataType("",arrName,description);
			
			processedArrays.put(arrType,arrName);
			tobeProcessedArrays.remove(arrType);
		    }
	    }
		
		if(someProcessed)
		    {
			if(tobeProcessedArrays.size()!=0)
			    {
				processPendingArrays();
				System.out.println("recall");
			    }
		    }
		else
		    {
			if(tobeProcessedArrays.size()!=0)
			    System.out.println("Warning : some array element left unprocessed and the size of to be processed array is "+tobeProcessedArrays.size());
		    }  
    }


    Hashtable allDataType = new Hashtable();
    
    void addDataType(String cNS,String arrayName, String description)
    {
	boolean inserted = false;
	while(!inserted)
	    {
		if(!allDataType.containsKey(arrayName))
		    {
			allDataType.put(arrayName,"struct "+dataType(cNS+arrayName)+" {\n"+description);
			inserted = true;
		    }
		else
		    {
			if(((String)allDataType.get(arrayName)).compareToIgnoreCase("struct "+dataType(cNS+arrayName)+" {\n"+description)==0)
			    return;
			arrayName = "_"+arrayName;
		    }
	    }
    }
    

    private int getArrayDimension(String atype)
    {
	int lindex = atype.lastIndexOf("[");
	String last = atype.substring(lindex,atype.length());

	int cnt=0;

	int indx=-1;

	while((indx=last.indexOf(',',indx+1))!=-1)cnt++;

	return cnt+1;
    }

    
    private boolean singleLevelArray(String atype)
    {
	int cnt=0;

	int indx=-1;
	while((indx=atype.indexOf('[',indx+1))!=-1)cnt++;
	
	if(cnt==1)
	    return true;
	else
	    return false;
    }
    
    private String getNS(String name)
    {
	
	if(name.indexOf(':')==-1)
	    return "";
	return name.substring(0,name.indexOf(':')+1);
    }
    
    private String getName(String name)
    {
	if(name.indexOf(':')==-1)
	    return name;
	return name.substring(name.indexOf(':')+1);
    }	
	      

public void parseMyDoc(Node node)
    {

	intitlizePrimitiveType();
	
	if(node.getNodeType()!=Node.DOCUMENT_NODE)
	    {
		System.out.println("Expected a Document node!!!!!");
		System.exit(0);
	    }
	NodeList nodes = node.getChildNodes();
	//	node = nodes.item(0);	/* go to a lever below definitions*/
	/*nodes = node.getChildNodes();*/


	if (nodes != null)
	    {
		
		setNameSpaces(node);

		printTypesInformation(node);
		/*		
		System.out.println("********************Name Space Information ***********************");
		System.out.println("Def NS "+definitionNS);
		//System.out.println("WSDL NS "+ wsdlNS);
		//System.out.println("XSD NS "+ xsdNS);
		System.out.println("Doc NS "+ docNS);
		//System.out.println("Encoding NS "+ encodingNS);
		System.out.println("******************** END Name Space Information ******************\n\n");
		*/
		//getServiceInfo(nodes.item(0));

		getServiceInfo(node);
		/*
		System.out.println("********************Service Information***************************");
		System.out.println("Service of the document found - "+ serviceName);
		System.out.println("Port number - "+ servicePortName);
		System.out.println("Binding Name - "+ servicePortBinding);
		System.out.println("Addess location - "+  serviceAddrLocation);


		System.out.println("********************End of Service Information********************\n\n");
		*/
		getBindingInfo(node,servicePortBinding.substring(servicePortBinding.indexOf(":")+1));
		/*		
		System.out.println("Operation -"+operationName);
		System.out.println("Input -"+inputName);
		System.out.println("Output -"+outputName);
		
		String inputParameters = getMessageEquivalent(node,inputName);
		System.out.println("input parameters - "+inputParameters);
		
		String outputParameters = getMessageEquivalent(node,outputName);
		System.out.println("output parameters - "+outputParameters);
		
		*/
		    
	    }
	
    }





    
    public static void main(String[] args) {
        if (args.length != 1) {
            System.out.println("Usage: java wsdlcpp [XML URI]");
            System.exit(0);
        }
        
        String uri = args[0];
    
        wsdlcpp parserParser = new wsdlcpp();
	parserParser.myParser(uri);
	  
    }
    
}

