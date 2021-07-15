<?php

/* A comment */
if (true):
    echo 'was true 1';
    echo 'was true 1 2';
endif;

// A line comment
if (true):
    echo 'was true 2';
else:
    echo 'was false 2';
endif;

/**
 * A doc comment
 */
function myFunction() {
    echo 'was here';
    ?>
<html><body>
    <div>My random stuff</div>
</body></html>
    <?php
}


switch (true):
    case true:
        echo 'was true 3';
        echo 'was true 3 2';
        break;
    default:
        echo 'the default';
endswitch;

?>
<html>
    <body>
        <p>
            <?php echo $my; ?>
        </p>
    </body>
</html>
