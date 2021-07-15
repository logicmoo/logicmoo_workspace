# Indentation algorithm

Document describing indentation algorithm.

## Terminology

* `nesting`: sum of all kinds of open brackets
* `nesting-stack`: contains nesting-levels before indentation was increased
* `nesting-start`: nesting-level at the beginning of the last line
* `nesting-end`: nesting-level at the end of the last line
* `nesting-stack-start`: nesting-start at top of stack
* `nesting-stack-end`: nesting-end at top of stack

## Algorithm

Here follows pseudo-code for a algorithm that calculates indentation for each line in buffer. Tokens are from the official re2c PHP lexer.

```php
foreach token in buffer:
    
    if token = T_START_HEREDOC:
        in-heredoc = true;
        in-heredoc-started-this-line = true;
    elseif token == T_END_HEREDOC:
        in-heredoc = false;
        in-heredoc-ended-this-line = true;
    endif;
    
    calculate nesting-end;
    
    if nesting-stack AND nesting-end <= nesting-stack-start: // #decrease
        pop stack;
        
        if first-token-on-line-is-nesting-decrease:
            indent--;
        else:
            if !temp-post-indent:
                temp-post-indent = indent;
            endif;
            
            temp-post-indent--;
        endif;
        
    endif;
    
    if we reached end of a line:
        
        if this-token or last token is a dot:
            concatentation-level = 1;
        else:
            concatentation-level = 0;
        endif;
        
        indent-start = indent;
        
        if new-token-line-start is more than one line after last-token-line-start AND token is not T_CLOSE_TAG:
            foreach line between last-token-line-start and new-token-line-start:
                save line indent-start
            endforeach;
        endforeach;
        
        if temp-pre-indent: // #temp-pre-indent
            indent-start = temp-pre-indent;
        endif;
        
        if (in-heredoc AND !in-heredoc-started-this-line) OR in-heredoc-ended-this-line:
            indent-start = 0;
        endif;
        
        save line indent-start; // #save
        
        if temp-post-indent: #temp-post-indent
            indent = temp-post-indent;
        endif;
        
        if nesting-end > 0 AND (!nesting-stack OR nesting-end > nesting-stack-end): // #increase
            if !nesting-stack:
                nesting-stack-end = 0;
            endif;
            
            push (nesting-stack-end nesting-end) to stack;
            indent++;
        endif;
        
        indent-end = indent;
        if token-end-line-number > token-start-line-number:
            if (in-heredoc AND !in-heredoc-started-this-line) OR in-heredoc-ended-this-line:
                indent-end = 0;
            endif;
            
            if token = T_DOC_COMMENT:
                tuning-level = 1;
            endif;
            
            foreach line between token-start-line-number to token-end-line-number:
                save line indent-end tuning-level;
            endforeach;
        endif;
        
        in-heredoc-started-this-line = false;
        in-heredoc-ended-this-line = false;
        first-token-on-line = true;
        
    else:
        if token != T_OPEN_TAG AND token != T_OPEN_TAG_WITH_ECHO:
            first-token-on-line = false;
        endif;
    endif;
    
endforeach;
```

## Examples

PHP examples using algorithms defined above, explained each line.

## Basic multi-line if-expression 1

```php			// #save indent: 0
if (function(		// #save indent: 0, #increase push (0 2) indent: 1
    false)		// #save indent: 1
) {				// #decrease pop (0 2) indent: 0, #save indent: 0, #increase push (0 1) indent: 1
    echo true;	// #save indent: 1
}					// #decrease pop (0 1) indent: 0, #save indent: 0
```

## Basic multi-line if-expression 2

```php				// #save indent: 0
if (function(		// #save indent: 0, #increase push (0 2) indent: 1
    false)) {		// #decrease pop (0 2) temp-post-indent: 0, #save indent: 1, #temp-post-indent indent: 0, #increase push (0 1) indent: 1
    echo true;		// #save indent: 1
}					// #decrease pop (0 1) indent: 0, #save indent: 0
```

## Inline control structure for if-else

```php				// #save indent: 0
if (true)			// #save indent: 0
    echo true;		// #temp-pre-indent: 1, #save indent: 1
else				// #save indent: 0
    echo false;	// #temp-pre-indent: 1, #save indent: 1
```

## Alternative control structure for if-else 2

```php					// #save indent: 0
if (true &&			// #save indent: 0, #increase push (0 1) indent: 1
    true				// #save indent: 1
):						// #decrease pop (0 1) indent: 0, #save indent: 0, #increase push (0 1) indent: 1
    echo true;			// #save indent_ 1
elseif (true			// #decrease pop (0 1) indent: 0, #save indent: 0, #increase push (0 1) indent: 1
    || false):			// #decrease pop (0 1) indent: 0, #save indent: 0, #increase push (0 1) indent: 1
    echo 'another';	// #save indent: 1
else:					// #decrease pop (0 1) indent: 0, #save indent: 0, #increase push (0 1) indent: 1
    echo false;		// #save indent: 1
endif;					// #decrease pop (0 1) indent: 0, #save indent: 0
```

## Multi-line assignments 1

```php
<?php				// #save indent: 0
$var = array(		// #save indent: 0, #increase push (0 2) indent: 1
    'def'			// #save indent: 1
);				// #decrease pop (0 2) indent: 0, #save indent: 0
```

## Multi-line assignments 2

```php
<?php				// #save indent: 0
$var = 'abc'		// #save indent: 0, #increase push (0 1) indent: 1
    . 'def'		// #save indent: 1
    . 'ghj';		// #decrease pop (0 1) indent: 0, #save indent: 0 /* ERROR */
```
