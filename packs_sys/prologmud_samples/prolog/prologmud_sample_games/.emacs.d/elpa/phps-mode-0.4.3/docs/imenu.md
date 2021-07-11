# Imenu algorithm

## Description

This file explains with psuedo-code how the imenu generation algorithm works.

## Psuedo-code

```
for token in tokens:

    if token is "{":
        nesting-level + 1;
    endif;
    if token is "}":
        nesting-level - 1;
    endif;
    
    

endfor;
```

[Back to start](../../../)
