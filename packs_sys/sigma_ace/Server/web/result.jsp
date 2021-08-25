<%@ page language="java" import="java.io.*,java.util.*,com.tks.celt.*" %>

<%!

	static int userCount = 0;
	
  public String[] parseString(String wholeString){      
   
   			//System.out.println("whole String" + wholeString);
   			
   			if(wholeString.startsWith("\"")){
   				wholeString = wholeString.substring(1);
   			}
   			
   			if(wholeString.endsWith("\"")){
   				wholeString = wholeString.substring(0, wholeString.length()-1);
   			}   			
   			
   			Vector elements = new Vector();
			int index = 0;
			int previousIndex = 0;
			
			while( (index =wholeString.indexOf("\\n", previousIndex)) != -1){
	
					//System.out.println("String " + wholeString.substring(previousIndex, index));
					String token = wholeString.substring(previousIndex, index);
					token = token.replaceAll(" ","&nbsp;");
					elements.add(token);
					previousIndex = index;
					previousIndex = previousIndex + 2;
				}
				
			if(previousIndex != wholeString.length()){
			
					String token = wholeString.substring(previousIndex, wholeString.length());
					token = token.replaceAll(" ","&nbsp;");
					elements.add(token);
			}
   		
   			String[] stringElements = new String[elements.size()];
   		
   				for(int i=0; i < elements.size(); i++){
   		
   					stringElements[i] = (String) elements.get(i);
   		
   				}
   		
   				return stringElements;      
   		      
  }	

%>


<%

	
	HashMap bindings = new HashMap();
	String query = request.getParameter("query");
	
	if(query == null){
			response.sendRedirect("query.jsp");
			return;		
	
	}
	
			

	String user = (String) session.getAttribute("user");

	if(user == null){
		
		user = "user"+userCount++;
		session.setAttribute("user","user"+userCount++);
	}
	
	
	PrologHandler prologHandler = new PrologHandler();
	
	String result = "";
	
	
	try{
	
		result = prologHandler.sendRequest(query,300,user);
	
	
	}catch(Exception e){
	
			System.out.println(e);
			result = e.getMessage();
	}
	
	
	
	result = result.replaceAll("<->", ":");
	//System.out.println("replaced string " + result);
	StringReader stringReader = new StringReader(result);
	ResponseProcessor responseProcessor = new ResponseProcessor();
	responseProcessor.processResponse(stringReader);
	bindings = responseProcessor.getBindings();
	//responseProcessor.processResponse(result);
	

%>

<html>
<head>
<title>CELT QUERY Results</title>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">

</head>
<body background="images/blue-stripe-vleft.gif">
<ul> 

<table width="600" border="0" align="center">
    <tr> 
      <td colspan="2"><b>Controlled English to Logic Translation <!--Common English Language Translation--><b><br>
        <b>CELT<br>
        </b></td>
    </tr>
    <tr> 
      <TD colspan="2" BGCOLOR="#666666" WIDTH="1" HEIGHT=" "><IMG SRC="images/pixel.gif"  width="1" height="1" border="0"></TD>
    </tr>
    <tr>
      <td width="100" align="right">&nbsp;</td>
      <td>&nbsp;</td>
    </tr>
<!--</table>-->



<!--<table width="600" border="0" align="center">-->

<%

	
	String warning = (String) bindings.get(PrologHandler.WARNING);
	String parsedTree = (String) bindings.get(PrologHandler.PARSE_TR);
	String logicExpr = (String) bindings.get(PrologHandler.LOGIC_EXP);
	String action = (String) bindings.get(PrologHandler.ACTION);
	
	if(warning != null){
		warning = warning.replaceAll(":","<->");
	}
	
	if(parsedTree != null){
		parsedTree = parsedTree.replaceAll(":","<->");
	}
	
	if( logicExpr != null){
		logicExpr = logicExpr.replaceAll(":","<->");
	}
	
	if(action != null){
		action = action.replaceAll(":","<->");
		
		if(action.startsWith("\"")){
			action = action.substring(1);
		}
		
		if(action.endsWith("\\n\"")){
			action = action.substring(0, action.length() - 2);
		}
		
		if(action.startsWith("Speech Act = ")){
		
			action = action.substring(12);
		}
		
		if(action.endsWith("\\")){
		
			action = action.substring(0, action.length()-1);
		}		
		
	}
	
	
	if(warning == null){
%>
	<TR><TD><B>English Sentence : </B></TD></TR>
	<TR><TD><i><%= query %> </i></TD></TR>
	<br>
	<TR><TD> No translation at this stage. </TD></TR>
<%		
	
	}else if(!warning.equalsIgnoreCase("''")){
	
%>
	<TR><TD><B>English Sentence : </B></TD></TR>
	<TR><TD><i><%= query %></i></TD></TR>
	<br>
	<TR><TD> <%= warning %> </TD></TR>
<%		
	
	}else{
%>		
	<TR><TD><B>English Sentence : </B></TD></TR>
	<TR><TD><i><%= query %></i></TD></TR>
	<br>
	<br>
	<br>
	<TR><TD><b>Translation to Logic : </b></TD></TR>

<%	
	
	
	
	String tokens[] = parseString(logicExpr);
	for(int i =0; i < tokens.length; i++){
	
%>
	<TR><TD> <%= tokens[i] %> </TD></TR>

<%
	}
%>

		
		
	<TR><TD><b>Parsed Sentence : </b></TD></TR>	
	<TR><TD> <%= parsedTree %> </TD></TR>
	<TR><TD><b>Speech Act : </b></TD></TR>	
	<TR><TD> <%= action %> </TD></TR>	
<%

	}

%>
</TABLE>

</body>
</HTML>