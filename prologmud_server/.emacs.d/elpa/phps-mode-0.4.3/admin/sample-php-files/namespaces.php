<?php

namespace MyNameSpaceA {
    class MyClassA {
        public function myMethodA()
        {
            echo "Do something A here";
        }
    }
}

namespace MyNameSpaceB {
    class MyClassB {
        public function myMethodB($arg1)
        {
            $class = \MyNameSpaceA\MyClassA();
            $class->myMethodA();
            echo "Do something B here";
        }
    }
}
