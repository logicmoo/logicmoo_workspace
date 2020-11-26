<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="utf-8">
    <title>Bolt htaccess tester.</title>
    <link rel="stylesheet" src="//normalize-css.googlecode.com/svn/trunk/normalize.css" />
    <!--[if IE]>
        <script src="http://html5shiv.googlecode.com/svn/trunk/html5.js"></script>
    <![endif]-->
</head>

<body id="home">
<?php

#htaccess tester, version 1.0

echo "<h1>Bolt Apache <tt>.htaccess</tt> tester.</h1>";

if (strpos($_SERVER['REQUEST_URI'], 'htaccess_tester.php') === false) {

    echo "<p><tt>mod_rewrite</tt> is working! You used the path <tt>" . $_SERVER['REQUEST_URI'] . "</tt> to request this page.</p>";

} elseif (is_readable(__DIR__.'/.htaccess') ) {

    echo "<p>The file .htaccess exists and is readable to the webserver. These are its contents: </p>\n<textarea style='width: 700px; height: 200px;'>";
    echo file_get_contents(__DIR__.'/.htaccess');
    echo "</textarea>";

} else {

    echo "<p><strong>Error:</strong> The file .htaccess does not exist or it is not readable to the webserver. <br><br>Retieve a new version of the file here, and place it in your webroot. Make sure it is readable to the webserver.</p>";
    die();

}

// echo "<h1>PHPinfo</h1>";

// echo "<p>Below you'll find the specifics of your PHP installation, for debugging purposes.</p>";

// phpinfo();
?>
</body>
</html>