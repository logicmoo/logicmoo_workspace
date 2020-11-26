<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

        <xsl:template match="TestReport">       
            <H4> Total Time: <xsl:value-of select="@totalTime"/> </H4> 
            <H4> Total Test Cases: <xsl:value-of select="@total"/> </H4>             
            <H4> Total Full Success: <xsl:value-of select="@fullSuccessCnt"/> </H4> 
            <H4> Total Failure: <xsl:value-of select="@failureCnt"/> </H4> 
            <H4> Total Partial Success: <xsl:value-of select="@partialSuccessCnt"/> </H4> 
            <H4> Total Corrupt: <xsl:value-of select="@corruptCnt"/> </H4> 
            <H4> Total Abort: <xsl:value-of select="@abortCnt"/> </H4> 
            <table cellspacing="0" cellpadding="0" width="95%">
                <tr bgColor="#cccccc">
                        <td><B> Test Name </B></td>
                        <td><B> Status    </B></td>
                        <td><B> Answer </B></td>
                        <td><B> Answer In Html </B> </td>
                        <td><B> Time (ms) </B></td>
                </tr>
                <xsl:apply-templates select="TestCase"/>
            </table>        
        </xsl:template>
        
        <xsl:template match="TestCase">
                <tr> 
                        <td><a><xsl:attribute name="href"><xsl:value-of select="@name"/></xsl:attribute><xsl:value-of select="@name" /> </a></td>
                        <td><xsl:value-of select="@status" /></td>
                        <td><a><xsl:attribute name="href"><xsl:value-of select="@answer"/></xsl:attribute><xsl:value-of select="@answer" /></a></td>
                        <td><a><xsl:attribute name="href"><xsl:value-of select="@answer_html"/></xsl:attribute><xsl:value-of select="@answer_html" /></a></td>
                        <td><xsl:value-of select="@time" /></td>
                </tr>
        </xsl:template>

</xsl:stylesheet>
