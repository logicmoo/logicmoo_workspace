<?php
$candc_query = $_REQUEST['candc_query'];
$showui = !isset($_REQUEST['showui']) || $_REQUEST['showui']=="1";
if($showui) {
?>/*<!DOCTYPE html><html lang="en"><head><title><?php 
echo htmlspecialchars(str_replace("\n", ' ', $candc_query));
?></title></head>
<body><form name="form1" action="?" method = "GET">qcandc_query:<br/>
<textarea name = "candc_query" rows="10" cols="100"><?php echo $candc_query;?></textarea><br/>
&nbsp;ccg: <input type = "checkbox" name = "ccg" value="1" <?php if($_REQUEST['ccg']!="0") echo "checked"; ?>/>
&nbsp;pos: <input type = "checkbox" name = "pos" value="1" <?php if($_REQUEST['pos']!="0") echo "checked"; ?>/>
&nbsp;boxer: <input type = "checkbox" name = "boxer" value="1" <?php if($_REQUEST['boxer']!="0") echo "checked"; ?>/>
&nbsp;No UI: <input type = "checkbox" name = "showui" value="0" /> <input type="submit"/></form><pre>*/
<?php
}
try {
    $tmpfname = tempnam(sys_get_temp_dir(), "candc_query_"); 
    $handle = fopen($tmpfname, "w");    
    fwrite($handle, $candc_query);
    fwrite($handle, chr(10));
    fclose($handle);
    system('./logicmoocanc.sh '.$tmpfname.' '.(strcasecmp($_REQUEST['boxer'],'0')).' '.(strcasecmp($_REQUEST['pos'],'0').' '.(strcasecmp($_REQUEST['ccg'],'0'))));
    
    $link = (isset($_SERVER['HTTPS']) && $_SERVER['HTTPS'] === 'on' ? 
                "https" : "http") . "://" . $_SERVER['HTTP_HOST'] .  
                $_SERVER['REQUEST_URI']; 
    unlink($tmpfname);
    unlink($tmpfname.".ccg");
} catch(Exception $e) {
    echo $e->getMessage();
}
echo '<a href="'.$link.'">'.$link.'</a>';
if($showui) echo "</pre></body></html>"; 
?>

