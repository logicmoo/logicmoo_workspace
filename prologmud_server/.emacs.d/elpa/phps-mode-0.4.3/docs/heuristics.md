# Heuristics

These should solve the problem of freezing editor for too long when making small changes to large files. Otherwise a full incremental re-parse would be triggered more often than necessary.

## Indenting

When indenting a line, calculate difference in white-space and change indexes of buffer after point correspondingly.

## Incremental

When user has done other changes, determine unchanged previous position R, determine changed maximum position X, determine new buffer length L. Do incremental lex from R to X.

### State preserving changes

If new states at X equals old states at X just move all indexes with delta X, otherwise do incremental lex of rest of buffer.

### State disrupting changes

Otherwise continue with incremental lex of rest of buffer.


[Back to start](../../../)
