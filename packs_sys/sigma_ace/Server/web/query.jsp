
<%@ page language="java" import="java.io.*,java.util.*" %>


<%
%>

<html>
<head>
<title>CELT QUERY PAGE</title>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
<script language="javascript">
function validateOnSubmit(){

		if(document.queryForm.query.value == ""){
		
			alert("please enter query before submiting ");
			return false;
		
		}
		
		return true;




}


</script>
</head>


<body background="images/blue-stripe-vleft.gif">
<ul> 
<br>
<br>
<br>


<table width="600" border="0" align="center">
<FORM name="queryForm" method="Post" action="result.jsp" onSubmit="return validateOnSubmit()">
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
    
      


<TR><td width="100" align="left"></td><TD>Please Enter Your Query</TD></TR>
<TR><td width="100" align="left"></td><TD><textarea name="query" cols=50 rows=10></textarea></TD></TR>

    <tr> 
      <td>&nbsp;</td>
      <td> 
        <input type="submit" name="Submit" value="Submit">
        <input type="reset" name="Clear" value="Clear">
      </td>
    </tr>
    <tr> 
      <td>&nbsp;</td>
      <td>&nbsp;</td>
    </tr>
    <tr> 
		<td>&nbsp;</td>
      <td>&nbsp;</td>
    </tr>  
    </form>  
  </table>
 <TAble  width="600" border="0" align="center">
 <tr>
 <td><i>&copy; 2000-2001, Teknowledge Corp.</i></td>
 <td align=right>&nbsp;</td>
 <tr>
 </table>
</ul> 
</body>
</html>
