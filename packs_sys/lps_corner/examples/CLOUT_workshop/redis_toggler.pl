
:- expects_dialect(lps).

% Minimal example to toggle a Redis binary tag value
% First you need to call (only once) create(RedisHost,Password)

toggle("0","1"). % Note: later versions of the API may convert to ints etc
toggle("1","0").

if get_key("tag:echo.value1",Old) at 1 
then toggle(Old,New), set_key("tag:echo.value1",New) from 1.
/** <examples> 
?- go(Timeline).
*/
